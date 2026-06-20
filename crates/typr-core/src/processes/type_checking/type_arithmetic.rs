//! Type arithmetic (RFC: `arithmétique_de_type.md`).
//!
//! Implements the RFC's domain-of-definition rules and normalization for the
//! type-level operators `+ - * /` (Number-only) and `&` (Record-only). `|` is
//! always well-formed per the RFC and isn't handled here. Operands must
//! already be reduced (callers reduce children before combining, same
//! convention as the rest of `reduce_type_helper`).
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::tnumber::Tnum;
use crate::components::r#type::type_category::TypeCategory;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use std::collections::HashMap;
use std::collections::HashSet;

/// Kinds accepted by the Number-domain operators (`+ - * /`). Deliberately
/// permissive for not-yet-resolved types (generics, `Any`, `Never`, an
/// unreduced operator tree): we only reject kinds we are *certain* are wrong,
/// since e.g. array-index arithmetic combines `IndexGen` (kind `Generic`)
/// with concrete integers and must stay valid until the generic is resolved.
fn accepts_number_kind(t: &Type) -> bool {
    // A kind-sigiled generic (`%`/`@`/`^`/`?`) is never Number-kind by
    // construction — sigils.md §4.3: "Vec[%R, A] ✗ (Record n'est pas Number)".
    if matches!(t, Type::KindedGen(_, _, _)) {
        return false;
    }
    matches!(
        t.to_category(),
        TypeCategory::Number
            | TypeCategory::Integer
            | TypeCategory::Generic
            | TypeCategory::Any
            | TypeCategory::Empty
            | TypeCategory::Template
            | TypeCategory::Operator
            | TypeCategory::Opaque(_)
            | TypeCategory::Alias
    )
}

/// Kinds accepted by the Intersection operator (`&`): `Record`, and
/// `Interface` per the RFC's "et éventuellement Interface". Same permissive
/// treatment of not-yet-resolved types as `accepts_number_kind`.
pub fn accepts_record_kind(t: &Type) -> bool {
    if let Type::KindedGen(k, _, _) = t {
        return matches!(
            k,
            crate::components::r#type::kind::Kind::Record
                | crate::components::r#type::kind::Kind::Interface
        );
    }
    matches!(
        t.to_category(),
        TypeCategory::Record
            | TypeCategory::Interface
            | TypeCategory::Generic
            | TypeCategory::Any
            | TypeCategory::Empty
            | TypeCategory::Template
            | TypeCategory::Operator
            | TypeCategory::Opaque(_)
            | TypeCategory::Alias
    )
}

/// RFC §7.1 + §4.1 — normalize a `+ - * /` operator node. `t1`/`t2` must
/// already be reduced. Reduces literal integer arithmetic, propagates to
/// `Number` once either side is a non-literal Number/Integer, reports
/// division-by-zero and out-of-domain operands as `Type::Failed` (a type
/// error, not `Never`, per RFC §9), and otherwise keeps the operator
/// symbolic (e.g. unresolved generics in array-index arithmetic).
pub fn norm_arithmetic(op: TypeOperator, t1: Type, t2: Type, h: HelpData) -> Type {
    match (&t1, &t2) {
        (Type::Integer(i1, _), Type::Integer(i2, _)) => match (i1.get_value(), i2.get_value()) {
            (Some(a), Some(b)) => match op {
                TypeOperator::Addition => Type::Integer(Tint::Val(a + b), h),
                TypeOperator::Substraction => Type::Integer(Tint::Val(a - b), h),
                TypeOperator::Multiplication => Type::Integer(Tint::Val(a * b), h),
                TypeOperator::Division if b == 0 => {
                    Type::Failed(format!("division by zero: `{} / {}`", a, b), h)
                }
                TypeOperator::Division => Type::Integer(Tint::Val(a / b), h),
                _ => Type::Operator(op, Box::new(t1), Box::new(t2), h),
            },
            // At least one side is a not-yet-known integer (e.g. an `IndexGen`
            // hasn't been substituted yet) — can't reduce further numerically,
            // but the operation is still well-formed.
            _ => Type::Operator(op, Box::new(t1), Box::new(t2), h),
        },
        (Type::Number(_, _) | Type::Integer(_, _), Type::Number(_, _) | Type::Integer(_, _)) => {
            Type::Number(Tnum::Unknown, h)
        }
        _ => {
            if accepts_number_kind(&t1) && accepts_number_kind(&t2) {
                Type::Operator(op, Box::new(t1), Box::new(t2), h)
            } else {
                Type::Failed(
                    format!(
                        "`{}` requires both operands to be of kind Number, got `{}` and `{}`",
                        op,
                        t1.pretty(),
                        t2.pretty()
                    ),
                    h,
                )
            }
        }
    }
}

/// RFC §7.3 + §4.3 — normalize an `&` (intersection) operator node.
/// `t1`/`t2` must already be reduced. `Record & Record` merges fields
/// (shared fields recursively become `T1 & T2`); `Never` absorbs (`X & Never
/// = Never`); `Any` is the identity; an out-of-domain operand or an
/// impossible merged field is reported as `Type::Failed`. Otherwise the
/// operator stays symbolic (e.g. a still-generic operand).
pub fn norm_intersection(t1: Type, t2: Type, h: HelpData) -> Type {
    match (&t1, &t2) {
        (Type::Empty(_), _) | (_, Type::Empty(_)) => Type::Empty(h),
        (Type::Any(_), _) => t2,
        (_, Type::Any(_)) => t1,
        (Type::Record(f1, _), Type::Record(f2, _)) => merge_record_fields(f1, f2, &h),
        _ => {
            if accepts_record_kind(&t1) && accepts_record_kind(&t2) {
                Type::Operator(TypeOperator::Intersection, Box::new(t1), Box::new(t2), h)
            } else {
                Type::Failed(
                    format!(
                        "`&` requires both operands to be of kind Record, got `{}` and `{}`",
                        t1.pretty(),
                        t2.pretty()
                    ),
                    h,
                )
            }
        }
    }
}

fn merge_record_fields(
    f1: &HashSet<ArgumentType>,
    f2: &HashSet<ArgumentType>,
    h: &HelpData,
) -> Type {
    let names1: HashMap<String, Type> = f1
        .iter()
        .map(|a| (a.get_argument_str(), a.get_type()))
        .collect();
    let names2: HashMap<String, Type> = f2
        .iter()
        .map(|a| (a.get_argument_str(), a.get_type()))
        .collect();

    let mut all_names: Vec<&String> = names1.keys().chain(names2.keys()).collect();
    all_names.sort();
    all_names.dedup();

    let mut fields = HashSet::new();
    for name in all_names {
        let merged = match (names1.get(name), names2.get(name)) {
            (Some(t1), Some(t2)) => norm_intersection(t1.clone(), t2.clone(), h.clone()),
            (Some(t), None) | (None, Some(t)) => t.clone(),
            (None, None) => unreachable!(),
        };
        // A merged field that is `Never` makes the whole record `Never`
        // (RFC §7.3.3); a kind violation in a merged field makes the whole
        // record ill-formed.
        if matches!(merged, Type::Empty(_) | Type::Failed(_, _)) {
            return merged;
        }
        fields.insert(ArgumentType::new(name, &merged));
    }
    Type::Record(fields, h.clone())
}

/// Recursively reduce `typ` and collect every `Type::Failed` node produced
/// by `norm_arithmetic`/`norm_intersection` into a `TypeError`, so a
/// kind-domain violation surfaces as a real compile error (RFC §9) instead of
/// silently vanishing. Intended to be called wherever a user-written type
/// expression is declared (aliases, signatures).
pub fn validate_operator_kinds(context: &Context, typ: &Type) -> Vec<TypRError> {
    let reduced = typ.reduce(context);
    let mut acc = Vec::new();
    collect_failed_types(&reduced, &mut acc);
    acc.into_iter()
        .map(|(message, h)| TypRError::Type(TypeError::InvalidTypeOperatorDomain(message, h)))
        .collect()
}

fn collect_failed_types(typ: &Type, acc: &mut Vec<(String, HelpData)>) {
    match typ {
        Type::Failed(message, h) => acc.push((message.clone(), h.clone())),
        Type::Function(args, ret, _) => {
            args.iter()
                .for_each(|a| collect_failed_types(&a.get_type(), acc));
            collect_failed_types(ret, acc);
        }
        Type::Record(fields, _) | Type::Interface(fields, _) => fields
            .iter()
            .for_each(|a| collect_failed_types(&a.get_type(), acc)),
        Type::Vec(_, idx, body, _) => {
            collect_failed_types(idx, acc);
            collect_failed_types(body, acc);
        }
        Type::Tag(_, inner, _) | Type::Multi(inner, _) => collect_failed_types(inner, acc),
        Type::Operator(_, a, b, _) => {
            collect_failed_types(a, acc);
            collect_failed_types(b, acc);
        }
        Type::Params(ts, _) | Type::Tuple(ts, _) => {
            ts.iter().for_each(|t| collect_failed_types(t, acc))
        }
        Type::Alias(_, params, _, _) => params.iter().for_each(|t| collect_failed_types(t, acc)),
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::builder;

    fn int(v: i32) -> Type {
        Type::Integer(Tint::Val(v), HelpData::default())
    }

    #[test]
    fn test_arithmetic_literal_reduction() {
        // `Type::Integer`'s `PartialEq` ignores the literal value (it only
        // compares the *kind*), so assert on the `Tint` payload directly.
        let res = norm_arithmetic(TypeOperator::Addition, int(2), int(3), HelpData::default());
        match res {
            Type::Integer(Tint::Val(v), _) => assert_eq!(v, 5),
            other => panic!("expected Integer(5), got {:?}", other),
        }

        let res = norm_arithmetic(
            TypeOperator::Substraction,
            int(5),
            int(3),
            HelpData::default(),
        );
        match res {
            Type::Integer(Tint::Val(v), _) => assert_eq!(v, 2),
            other => panic!("expected Integer(2), got {:?}", other),
        }

        let res = norm_arithmetic(
            TypeOperator::Multiplication,
            int(4),
            int(3),
            HelpData::default(),
        );
        match res {
            Type::Integer(Tint::Val(v), _) => assert_eq!(v, 12),
            other => panic!("expected Integer(12), got {:?}", other),
        }

        let res = norm_arithmetic(TypeOperator::Division, int(9), int(3), HelpData::default());
        match res {
            Type::Integer(Tint::Val(v), _) => assert_eq!(v, 3),
            other => panic!("expected Integer(3), got {:?}", other),
        }
    }

    #[test]
    fn test_arithmetic_division_by_zero_is_failed() {
        let res = norm_arithmetic(TypeOperator::Division, int(4), int(0), HelpData::default());
        assert!(matches!(res, Type::Failed(_, _)));
    }

    #[test]
    fn test_arithmetic_propagates_to_number() {
        let res = norm_arithmetic(
            TypeOperator::Addition,
            builder::number_type(),
            int(3),
            HelpData::default(),
        );
        assert_eq!(res, builder::number_type());
    }

    #[test]
    fn test_arithmetic_rejects_non_number_kind() {
        let res = norm_arithmetic(
            TypeOperator::Addition,
            builder::character_type_default(),
            builder::boolean_type(),
            HelpData::default(),
        );
        assert!(matches!(res, Type::Failed(_, _)));
    }

    #[test]
    fn test_arithmetic_stays_symbolic_for_generics() {
        let index = Type::IndexGen("N".to_string(), HelpData::default());
        let res = norm_arithmetic(TypeOperator::Addition, index, int(1), HelpData::default());
        assert!(matches!(
            res,
            Type::Operator(TypeOperator::Addition, _, _, _)
        ));
    }

    #[test]
    fn test_arithmetic_rejects_record_kinded_generic() {
        // sigils.md §4.3: a `%R`-kinded generic is Record-kind, never Number.
        let record_gen = Type::KindedGen(
            crate::components::r#type::kind::Kind::Record,
            "R".to_string(),
            HelpData::default(),
        );
        let res = norm_arithmetic(
            TypeOperator::Addition,
            record_gen,
            int(1),
            HelpData::default(),
        );
        assert!(matches!(res, Type::Failed(_, _)));
    }

    #[test]
    fn test_intersection_accepts_record_kinded_generic() {
        let record_gen = Type::KindedGen(
            crate::components::r#type::kind::Kind::Record,
            "R".to_string(),
            HelpData::default(),
        );
        let record = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let res = norm_intersection(record_gen, record, HelpData::default());
        assert!(!matches!(res, Type::Failed(_, _)));
    }

    #[test]
    fn test_intersection_rejects_string_kinded_generic() {
        let string_gen = Type::KindedGen(
            crate::components::r#type::kind::Kind::String,
            "S".to_string(),
            HelpData::default(),
        );
        let res = norm_intersection(
            string_gen,
            builder::character_type_default(),
            HelpData::default(),
        );
        assert!(matches!(res, Type::Failed(_, _)));
    }

    #[test]
    fn test_intersection_merges_disjoint_record_fields() {
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let b = builder::record_type(&[("y".to_string(), builder::character_type_default())]);
        let res = norm_intersection(a, b, HelpData::default());
        let expected = builder::record_type(&[
            ("x".to_string(), builder::integer_type_default()),
            ("y".to_string(), builder::character_type_default()),
        ]);
        assert_eq!(res, expected);
    }

    #[test]
    fn test_intersection_merges_shared_field_recursively() {
        let a = builder::record_type(&[(
            "p".to_string(),
            builder::record_type(&[("x".to_string(), builder::integer_type_default())]),
        )]);
        let b = builder::record_type(&[(
            "p".to_string(),
            builder::record_type(&[("y".to_string(), builder::character_type_default())]),
        )]);
        let res = norm_intersection(a, b, HelpData::default());
        match res {
            Type::Record(fields, _) => {
                let p = fields.iter().find(|a| a.get_argument_str() == "p").unwrap();
                assert!(matches!(p.get_type(), Type::Record(fs, _) if fs.len() == 2));
            }
            other => panic!("expected a Record, got {:?}", other),
        }
    }

    #[test]
    fn test_intersection_never_absorbs() {
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let res = norm_intersection(a, builder::empty_type(), HelpData::default());
        assert!(matches!(res, Type::Empty(_)));
    }

    #[test]
    fn test_intersection_any_is_identity() {
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let res = norm_intersection(a.clone(), builder::any_type(), HelpData::default());
        assert_eq!(res, a);
    }

    #[test]
    fn test_intersection_rejects_non_record_kind() {
        let res = norm_intersection(
            builder::integer_type_default(),
            builder::character_type_default(),
            HelpData::default(),
        );
        assert!(matches!(res, Type::Failed(_, _)));
    }

    #[test]
    fn test_intersection_impossible_shared_field_fails() {
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let b = builder::record_type(&[("x".to_string(), builder::character_type_default())]);
        let res = norm_intersection(a, b, HelpData::default());
        assert!(matches!(res, Type::Failed(_, _)));
    }

    #[test]
    fn test_validate_operator_kinds_reports_invalid_arithmetic() {
        let typ = Type::Operator(
            TypeOperator::Addition,
            Box::new(builder::character_type_default()),
            Box::new(builder::boolean_type()),
            HelpData::default(),
        );
        let errors = validate_operator_kinds(&Context::default(), &typ);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_validate_operator_kinds_accepts_valid_intersection() {
        let typ = builder::intersection_type(&[
            builder::record_type(&[("x".to_string(), builder::integer_type_default())]),
            builder::record_type(&[("y".to_string(), builder::character_type_default())]),
        ]);
        let errors = validate_operator_kinds(&Context::default(), &typ);
        assert!(errors.is_empty());
    }
}
