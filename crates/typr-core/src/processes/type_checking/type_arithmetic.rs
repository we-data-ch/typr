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
use crate::components::r#type::refinement::{Interval, Measure, RefinementSet};
use crate::components::r#type::tint::Tint;
use crate::components::r#type::tnumber::Tnum;
use crate::components::r#type::type_category::GKind;
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
            // IndexGen (`#N`) is now categorized as GenericKinded(Number); it
            // stays valid in number arithmetic exactly as it did when it mapped
            // to the bare `Generic` category.
            | TypeCategory::GenericKinded(GKind::Number)
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
            crate::components::r#type::kind::Kind::Record | crate::components::r#type::kind::Kind::Interface
        );
    }
    matches!(
        t.to_category(),
        TypeCategory::Record
            | TypeCategory::Interface
            | TypeCategory::Generic
            // IndexGen (`#N`) → GenericKinded(Number): keep the same permissive
            // treatment it had via the bare `Generic` category before kinded
            // categories existed.
            | TypeCategory::GenericKinded(GKind::Number)
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
                TypeOperator::Division if b == 0 => Type::Failed(format!("division by zero: `{} / {}`", a, b), h),
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
/// (shared fields recursively become `T1 & T2`); `Interface & Interface`
/// merges methods (same rule: a shared method name must have identical
/// signatures, else `Type::Failed`); `Never` absorbs (`X & Never = Never`);
/// `Any` is the identity; an out-of-domain operand or an impossible merged
/// field/method is reported as `Type::Failed`. A mixed `Record & Interface`
/// stays symbolic (both operands are within the accepted kind, but there is
/// nothing to merge structurally) — callers that need to see through it
/// project the facet they need (see `facets.rs`).
pub fn norm_intersection(t1: Type, t2: Type, h: HelpData) -> Type {
    if matches!(t1, Type::Property(..) | Type::Refined(..)) || matches!(t2, Type::Property(..) | Type::Refined(..)) {
        return norm_refinement(t1, t2, h);
    }
    match (&t1, &t2) {
        (Type::Empty(_), _) | (_, Type::Empty(_)) => Type::Empty(h),
        (Type::Any(_), _) => t2,
        (_, Type::Any(_)) => t1,
        (Type::Record(f1, _), Type::Record(f2, _)) => merge_record_fields(f1, f2, &h),
        (Type::Interface(m1, _), Type::Interface(m2, _)) => merge_interface_methods(m1, m2, &h),
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

/// The `TypeError` for a `Failed` produced by `apply_refinements`, or `None`
/// for any other `Failed` (arithmetic, records, ...).
pub fn refinement_error(t: &Type) -> Option<TypeError> {
    match t {
        Type::Failed(msg, h) if msg.starts_with("invalid refinement") || msg.contains("is a property, not a type") => {
            Some(TypeError::InvalidRefinement(msg.clone(), h.clone()))
        }
        Type::Refined(base, set, h) if matches!(**base, Type::Any(_)) => Some(TypeError::InvalidRefinement(
            format!("`{}` is a property, not a type: refine a base type with `T & {}`", set, set),
            h.clone(),
        )),
        Type::Failed(msg, h) if msg.starts_with("unsatisfiable refinement") => {
            Some(TypeError::UnsatisfiableRefinement(msg.clone(), h.clone()))
        }
        _ => None,
    }
}

/// Split an `&` operand into `(base, refinements)`. A bare `Property` has no
/// base (`int & (> 0)` is parsed as `Intersection(int, Property)`).
fn split_refinement(t: Type) -> (Option<Type>, RefinementSet) {
    match t {
        Type::Property(p, _) => (None, RefinementSet::single(&p)),
        // `Refined(Any, ..)` is the base-less carrier made below for `p & q`.
        Type::Refined(base, set, _) if matches!(*base, Type::Any(_)) => (None, set),
        Type::Refined(base, set, _) => (Some(*base), set),
        t => (Some(t), RefinementSet::empty()),
    }
}

/// The base and the refinements a declared type carries, read off its shape
/// without reducing it: `[#N, T] & length(> 0)` stays an unreduced
/// `Intersection` in a signature, and reducing it once `N` is substituted
/// (`[0, char] & length(> 0)`) would collapse it into an error.
pub(crate) fn declared_refinements(t: &Type) -> Option<(Type, RefinementSet)> {
    fn walk(t: &Type) -> (Option<Type>, RefinementSet) {
        match t {
            Type::Operator(TypeOperator::Intersection, a, b, _) => {
                let ((ba, sa), (bb, sb)) = (walk(a), walk(b));
                (ba.or(bb), sa.meet(&sb))
            }
            t => split_refinement(t.clone()),
        }
    }
    let (base, set) = walk(t);
    base.filter(|_| !set.is_trivial()).map(|b| (b, set))
}

/// `&` where at least one operand is a `Property` or a `Refined` type
/// (refined_types_plan.md, Phase 3): merge the bases like any other
/// intersection, intersect the property sets, then check the result.
fn norm_refinement(t1: Type, t2: Type, h: HelpData) -> Type {
    let ((b1, s1), (b2, s2)) = (split_refinement(t1), split_refinement(t2));
    let set = s1.meet(&s2);
    let base = match (b1, b2) {
        (Some(a), Some(b)) => match norm_intersection(a, b, h.clone()) {
            failed @ Type::Failed(..) => return failed,
            t => t,
        },
        (Some(t), None) | (None, Some(t)) => t,
        // `p & q` with no base yet (`int & ((> 0) & (< 10))`): keep the merged
        // properties until a base shows up. `refinement_error` rejects one
        // that stays alone.
        (None, None) => return Type::Refined(Box::new(Type::Any(h.clone())), set, h),
    };
    apply_refinements(base, set, h)
}

/// Attach `set` to `base`, or explain why that is impossible: the base does
/// not support the measure (invalid refinement), or no value can satisfy the
/// set (unsatisfiable refinement). `[T] & length(n)` folds into the length
/// index of `Vec` (D4); a literal that satisfies the set stays a literal.
pub(crate) fn apply_refinements(base: Type, set: RefinementSet, h: HelpData) -> Type {
    if set.is_trivial() {
        return base;
    }
    let unsatisfiable = |why: &str| {
        Type::Failed(format!("unsatisfiable refinement: `{} & {}` {}", base.pretty(), set, why), h.clone())
    };
    let invalid = |m: Measure| {
        Type::Failed(
            format!("invalid refinement: `{}` cannot be refined by `{}`", base.pretty(), m),
            h.clone(),
        )
    };
    let full = Interval::full();
    match &base {
        Type::Empty(_) => base,
        Type::Vec(kind, index, elem, vh) => {
            if set.get(Measure::Value).is_some() {
                return invalid(Measure::Value);
            }
            if set.is_empty(true) {
                return unsatisfiable("has no value");
            }
            let len = set.get(Measure::Length).and_then(|iv| iv.as_point());
            match (len, &**index) {
                (Some(n), Type::Any(_)) => Type::Vec(
                    kind.clone(),
                    Box::new(Type::Integer(Tint::Val(n as i32), vh.clone())),
                    elem.clone(),
                    vh.clone(),
                ),
                (Some(n), Type::Integer(Tint::Val(m), _)) if n as i32 == *m => base.clone(),
                (Some(n), Type::Integer(Tint::Val(m), _)) => {
                    unsatisfiable(&format!("contradicts its length {} (asked for {})", m, n))
                }
                // A range (`length(> 0)`) against a known length: the literal
                // either satisfies it (the range adds nothing) or contradicts it.
                (None, Type::Integer(Tint::Val(m), _)) => {
                    let range = set.get(Measure::Length).unwrap_or(&full);
                    if Interval::point(*m as f64).implies(range) {
                        base.clone()
                    } else {
                        unsatisfiable(&format!("contradicts its length {}", m))
                    }
                }
                // Symbolic length (`#N`) or unknown length with a range: keep the property.
                _ => Type::Refined(Box::new(base.clone()), set, h),
            }
        }
        Type::Integer(tint, _) => {
            if set.get(Measure::Length).is_some() {
                return invalid(Measure::Length);
            }
            if set.is_empty(true) {
                return unsatisfiable("has no integer value");
            }
            match tint {
                Tint::Val(v) if Interval::point(*v as f64).implies(set.get(Measure::Value).unwrap_or(&full)) => {
                    base.clone()
                }
                Tint::Val(_) => unsatisfiable("excludes this literal"),
                Tint::Unknown => Type::Refined(Box::new(base.clone()), set, h),
            }
        }
        Type::Number(tnum, _) => {
            if set.get(Measure::Length).is_some() {
                return invalid(Measure::Length);
            }
            if set.is_empty(false) {
                return unsatisfiable("has no value");
            }
            match tnum {
                Tnum::Val(v) if Interval::point(*v).implies(set.get(Measure::Value).unwrap_or(&full)) => base.clone(),
                Tnum::Val(_) => unsatisfiable("excludes this literal"),
                Tnum::Unknown => Type::Refined(Box::new(base.clone()), set, h),
            }
        }
        _ => invalid(set.iter().next().map(|(m, _)| *m).unwrap_or(Measure::Value)),
    }
}

fn merge_record_fields(f1: &HashSet<ArgumentType>, f2: &HashSet<ArgumentType>, h: &HelpData) -> Type {
    let names1: HashMap<String, Type> = f1.iter().map(|a| (a.get_argument_str(), a.get_type())).collect();
    let names2: HashMap<String, Type> = f2.iter().map(|a| (a.get_argument_str(), a.get_type())).collect();

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

/// Mirrors `merge_record_fields` for `Interface & Interface`: a method name
/// shared by both interfaces must have exactly the same signature in both
/// (no covariant/contravariant merge attempted — that's future work), else
/// the intersection is ill-formed. Unlike record fields, method types are
/// never merged recursively (`Self`-shaped `Function` types, not records).
fn merge_interface_methods(m1: &HashSet<ArgumentType>, m2: &HashSet<ArgumentType>, h: &HelpData) -> Type {
    let names1: HashMap<String, Type> = m1.iter().map(|a| (a.get_argument_str(), a.get_type())).collect();
    let names2: HashMap<String, Type> = m2.iter().map(|a| (a.get_argument_str(), a.get_type())).collect();

    let mut all_names: Vec<&String> = names1.keys().chain(names2.keys()).collect();
    all_names.sort();
    all_names.dedup();

    let mut methods = HashSet::new();
    for name in all_names {
        let merged = match (names1.get(name), names2.get(name)) {
            (Some(t1), Some(t2)) if t1 == t2 => t1.clone(),
            (Some(t1), Some(t2)) => {
                return Type::Failed(
                    format!(
                        "`&` cannot merge method `{}`: incompatible signatures `{}` and `{}`",
                        name,
                        t1.pretty(),
                        t2.pretty()
                    ),
                    h.clone(),
                );
            }
            (Some(t), None) | (None, Some(t)) => t.clone(),
            (None, None) => unreachable!(),
        };
        methods.insert(ArgumentType::new(name, &merged));
    }
    Type::Interface(methods, h.clone())
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
            args.iter().for_each(|a| collect_failed_types(&a.get_type(), acc));
            collect_failed_types(ret, acc);
        }
        Type::Record(fields, _) | Type::Interface(fields, _) => {
            fields.iter().for_each(|a| collect_failed_types(&a.get_type(), acc))
        }
        Type::Vec(_, idx, body, _) => {
            collect_failed_types(idx, acc);
            collect_failed_types(body, acc);
        }
        Type::Tag(_, inner, _) | Type::Multi(inner, _) => collect_failed_types(inner, acc),
        Type::Operator(_, a, b, _) => {
            collect_failed_types(a, acc);
            collect_failed_types(b, acc);
        }
        Type::Params(ts, _) | Type::Tuple(ts, _) => ts.iter().for_each(|t| collect_failed_types(t, acc)),
        Type::Alias(_, params, _, _) => params.iter().for_each(|t| collect_failed_types(t, acc)),
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::components::r#type::refinement::{Num, Refinement};

    fn prop(r: Refinement) -> Type {
        Type::Property(r, HelpData::default())
    }
    fn gt(c: f64) -> Type {
        prop(Refinement::Gt(Num::new(c)))
    }
    fn lt(c: f64) -> Type {
        prop(Refinement::Lt(Num::new(c)))
    }
    fn inter(a: Type, b: Type) -> Type {
        norm_intersection(a, b, HelpData::default())
    }

    #[test]
    fn test_refine_int_with_value_property() {
        let res = inter(builder::integer_type_default(), gt(0.0));
        assert!(matches!(res, Type::Refined(..)), "{:?}", res);
    }

    #[test]
    fn test_refine_is_commutative_and_associative() {
        let int = builder::integer_type_default;
        let a = inter(inter(int(), gt(0.0)), lt(10.0));
        let c = inter(int(), inter(lt(10.0), gt(0.0)));
        let d = inter(lt(10.0), inter(gt(0.0), int()));
        assert_eq!(a, c);
        assert_eq!(a, d);
    }

    #[test]
    fn test_refine_is_idempotent() {
        let once = inter(builder::integer_type_default(), gt(0.0));
        let twice = inter(once.clone(), gt(0.0));
        assert_eq!(once, twice);
    }

    #[test]
    fn test_refine_contradiction_is_unsatisfiable() {
        let res = inter(inter(builder::integer_type_default(), gt(10.0)), lt(5.0));
        assert!(matches!(refinement_error(&res), Some(TypeError::UnsatisfiableRefinement(..))), "{:?}", res);
    }

    #[test]
    fn test_refine_integers_round_bounds() {
        // (> 0) & (< 1) has real values but no integer.
        let res = inter(inter(builder::integer_type_default(), gt(0.0)), lt(1.0));
        assert!(matches!(refinement_error(&res), Some(TypeError::UnsatisfiableRefinement(..))));
        let num = inter(inter(builder::number_type(), gt(0.0)), lt(1.0));
        assert!(matches!(num, Type::Refined(..)));
    }

    #[test]
    fn test_refine_capability_is_checked() {
        let len = || prop(Refinement::Length(3));
        assert!(matches!(
            refinement_error(&inter(builder::integer_type_default(), len())),
            Some(TypeError::InvalidRefinement(..))
        ));
        assert!(matches!(
            refinement_error(&inter(builder::character_type_default(), gt(0.0))),
            Some(TypeError::InvalidRefinement(..))
        ));
        let vec = builder::array_type(builder::any_type(), builder::integer_type_default());
        assert!(matches!(refinement_error(&inter(vec, gt(0.0))), Some(TypeError::InvalidRefinement(..))));
    }

    #[test]
    fn test_refine_length_folds_into_vec_index() {
        let vec = builder::array_type(builder::any_type(), builder::integer_type_default());
        let res = inter(vec, prop(Refinement::Length(5)));
        assert_eq!(res, builder::array_type2(5, builder::integer_type_default()));
        // Same length again: no-op. Another length: contradiction.
        assert_eq!(inter(res.clone(), prop(Refinement::Length(5))), res);
        let clash = inter(res, prop(Refinement::Length(3)));
        assert!(matches!(refinement_error(&clash), Some(TypeError::UnsatisfiableRefinement(..))));
    }

    #[test]
    fn test_refine_literal_is_proven_or_refuted() {
        assert_eq!(inter(builder::integer_type(3), gt(0.0)), builder::integer_type(3));
        let refuted = inter(builder::integer_type(-3), gt(0.0));
        assert!(matches!(refinement_error(&refuted), Some(TypeError::UnsatisfiableRefinement(..))));
    }

    #[test]
    fn test_bare_properties_are_not_types() {
        let res = inter(gt(0.0), lt(5.0));
        assert!(matches!(refinement_error(&res), Some(TypeError::InvalidRefinement(..))), "{:?}", res);
    }

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

        let res = norm_arithmetic(TypeOperator::Substraction, int(5), int(3), HelpData::default());
        match res {
            Type::Integer(Tint::Val(v), _) => assert_eq!(v, 2),
            other => panic!("expected Integer(2), got {:?}", other),
        }

        let res = norm_arithmetic(TypeOperator::Multiplication, int(4), int(3), HelpData::default());
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
        assert!(matches!(res, Type::Operator(TypeOperator::Addition, _, _, _)));
    }

    #[test]
    fn test_arithmetic_rejects_record_kinded_generic() {
        // sigils.md §4.3: a `%R`-kinded generic is Record-kind, never Number.
        let record_gen = Type::KindedGen(
            crate::components::r#type::kind::Kind::Record,
            "R".to_string(),
            HelpData::default(),
        );
        let res = norm_arithmetic(TypeOperator::Addition, record_gen, int(1), HelpData::default());
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
        let res = norm_intersection(string_gen, builder::character_type_default(), HelpData::default());
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
    fn test_intersection_merges_disjoint_interface_methods() {
        let a = builder::interface_type(&[(
            "view",
            builder::function_type(&[builder::self_generic_type()], builder::character_type_default()),
        )]);
        let b = builder::interface_type(&[(
            "len",
            builder::function_type(&[builder::self_generic_type()], builder::integer_type_default()),
        )]);
        let res = norm_intersection(a, b, HelpData::default());
        match res {
            Type::Interface(methods, _) => assert_eq!(methods.len(), 2),
            other => panic!("expected an Interface, got {:?}", other),
        }
    }

    #[test]
    fn test_intersection_merges_shared_identical_method() {
        let view = builder::function_type(&[builder::self_generic_type()], builder::character_type_default());
        let a = builder::interface_type(&[("view", view.clone())]);
        let b = builder::interface_type(&[("view", view)]);
        let res = norm_intersection(a, b, HelpData::default());
        match res {
            Type::Interface(methods, _) => assert_eq!(methods.len(), 1),
            other => panic!("expected an Interface, got {:?}", other),
        }
    }

    #[test]
    fn test_intersection_incompatible_shared_method_fails() {
        let a = builder::interface_type(&[(
            "view",
            builder::function_type(&[builder::self_generic_type()], builder::character_type_default()),
        )]);
        let b = builder::interface_type(&[(
            "view",
            builder::function_type(&[builder::self_generic_type()], builder::integer_type_default()),
        )]);
        let res = norm_intersection(a, b, HelpData::default());
        assert!(matches!(res, Type::Failed(_, _)));
    }

    #[test]
    fn test_intersection_record_and_interface_stays_symbolic() {
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let b = builder::interface_type(&[(
            "view",
            builder::function_type(&[builder::self_generic_type()], builder::character_type_default()),
        )]);
        let res = norm_intersection(a, b, HelpData::default());
        assert!(matches!(res, Type::Operator(TypeOperator::Intersection, _, _, _)));
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

    #[test]
    fn length_range_on_vectors() {
        use crate::components::r#type::refinement::{Interval, Measure};
        let range = || prop(Refinement::Range(Measure::Length, Interval::greater_than(0.0)));
        let unknown = Type::Vec(
            crate::components::r#type::vector_type::VecType::S3,
            Box::new(Type::Any(HelpData::default())),
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        // unknown length: the range is kept as a property
        assert!(matches!(inter(unknown.clone(), range()), Type::Refined(..)));
        // known length: satisfied range is redundant, violated one is a contradiction
        let sized = |n| Type::Vec(
            crate::components::r#type::vector_type::VecType::S3,
            Box::new(builder::integer_type(n)),
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        assert_eq!(inter(sized(3), range()), sized(3));
        let res = inter(sized(0), range());
        assert!(matches!(refinement_error(&res), Some(TypeError::UnsatisfiableRefinement(..))), "{:?}", res);
    }
}
