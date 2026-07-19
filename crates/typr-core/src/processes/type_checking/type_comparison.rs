use crate::components::context::Context;
use crate::components::language::var::Var;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::type_checking::type_arithmetic::norm_arithmetic;
use crate::processes::type_checking::type_arithmetic::norm_intersection;
use crate::processes::type_checking::unification::type_substitution;
use crate::utils::builder;
use rpds::Vector;

pub fn reduce_param(
    context: &Context,
    param: &ArgumentType,
    memory: Vector<String>, // List of pairs [X, Y1]
) -> ArgumentType {
    // Returns list of pairs [X, Y2]

    // Reduce the type part of each parameter
    let reduced_type = reduce_type_helper(context, &param.get_type(), memory);
    ArgumentType(
        param.get_argument(),
        reduced_type,
        param.2.to_owned(),
        param.3.to_owned(),
        param.4.to_owned(),
    )
}

fn is_in_memory(name: &str, memory: &Vector<String>) -> bool {
    memory.iter().any(|val| val == name)
}

pub fn reduce_type(context: &Context, type_: &Type) -> Type {
    reduce_type_helper(context, type_, Vector::new())
}

pub fn reduce_alias(
    aliased_type: Type,
    generics: &[Type],
    concret_types: &[Type],
    name: &str,
    memory: Vector<String>,
    context: &Context,
) -> Type {
    let substituted = type_substitution(
        &aliased_type,
        &generics
            .iter()
            .zip(concret_types.iter())
            .map(|(r#gen, typ)| (r#gen.clone(), typ.clone()))
            .collect::<Vec<_>>(),
    );
    reduce_type_helper(context, &substituted, memory.push_back(name.to_string()))
}

pub fn reduce_type_helper(context: &Context, type_: &Type, memory: Vector<String>) -> Type {
    match type_ {
        Type::Record(args, h) => Type::Record(
            args.iter()
                .map(|arg| reduce_param(context, arg, memory.clone()))
                .collect(),
            h.clone(),
        ),
        Type::Alias(name, concret_types, is_opaque, h) => match (is_opaque, is_in_memory(name, &memory)) {
            (true, _) | (_, true) => type_.clone(),
            (false, _) => match Var::from_type(type_.clone()) {
                Some(var) => context
                    .get_matching_alias_signature(&var)
                    .map(|(aliased_type, generics)| {
                        reduce_alias(aliased_type, &generics, concret_types, name, memory, context)
                    })
                    .unwrap_or_else(|| match name.as_str() {
                        "Integer" => builder::integer_type_default(),
                        "Character" => builder::character_type_default(),
                        "Boolean" => builder::boolean_type(),
                        "Number" => builder::number_type(),
                        _ => Type::Any(h.clone()),
                    }),
                None => Type::Any(h.clone()),
            },
        },
        Type::Tag(name, inner, h) => Type::Tag(
            name.clone(),
            Box::new(reduce_type_helper(context, inner, memory.clone())),
            h.clone(),
        ),
        Type::If(typ, _conditions, _) => *typ.clone(),
        Type::Function(typs, ret_typ, h) => {
            let typs2: Vec<ArgumentType> = typs
                .iter()
                .map(|arg| {
                    let new_type = reduce_type_helper(context, &arg.get_type(), memory.clone());
                    ArgumentType::new(&arg.get_argument_str(), &new_type)
                })
                .collect();
            let ret_typ2 = reduce_type_helper(context, ret_typ, memory.clone());
            Type::Function(typs2, Box::new(ret_typ2), h.to_owned())
        }
        Type::Vec(vtype, ind, typ, h) => Type::Vec(
            vtype.clone(),
            ind.clone(),
            Box::new(reduce_type_helper(context, typ, memory.clone())),
            h.clone(),
        ),
        Type::Operator(TypeOperator::Union, t1, t2, h) => {
            // G4 (audit_type_checking.md): branches used to be compared
            // un-reduced, and — when neither subsumed the other — returned
            // as the original, still-un-reduced union. That left an alias
            // buried in an unrelated union branch (e.g. `char | Meters`)
            // never resolved by this path. Reduce both branches first, so
            // the subtype comparison (and the union kept when neither side
            // wins) both operate on resolved types.
            let typ1 = reduce_type_helper(context, t1, memory.clone());
            let typ2 = reduce_type_helper(context, t2, memory.clone());
            if typ1.is_subtype(&typ2, context).0 {
                typ1
            } else if typ2.is_subtype(&typ1, context).0 {
                typ2
            } else {
                Type::Operator(TypeOperator::Union, Box::new(typ1), Box::new(typ2), h.clone())
            }
        }
        Type::Operator(
            op @ (TypeOperator::Addition
            | TypeOperator::Substraction
            | TypeOperator::Multiplication
            | TypeOperator::Division),
            t1,
            t2,
            h,
        ) => {
            let r1 = reduce_type_helper(context, t1, memory.clone());
            let r2 = reduce_type_helper(context, t2, memory.clone());
            norm_arithmetic(*op, r1, r2, h.clone())
        }
        Type::Operator(TypeOperator::Intersection, t1, t2, h) => {
            let r1 = reduce_type_helper(context, t1, memory.clone());
            let r2 = reduce_type_helper(context, t2, memory.clone());
            norm_intersection(r1, r2, h.clone())
        }
        Type::Operator(TypeOperator::Access, t1, t2, h) => {
            let t1_inner = (**t1).clone();
            let t2_inner = (**t2).clone();
            let module_name = match &t1_inner {
                Type::Variable(name, _) => Some(name.as_str()),
                Type::Alias(name, _, _, _) => Some(name.as_str()),
                _ => None,
            };
            let alias_name = match &t2_inner {
                Type::Alias(name, _, _, _) => Some(name.as_str()),
                _ => None,
            };
            match (module_name, alias_name) {
                (Some(module_name), Some(alias_name)) => context
                    .get_type_from_variable(&Var::from_name(module_name))
                    .ok()
                    .and_then(|t| t.to_module_type().ok())
                    .and_then(|module_type| module_type.get_type_from_name(alias_name).ok())
                    .unwrap_or_else(|| Type::Any(h.clone())),
                _ => Type::Any(h.clone()),
            }
        }
        t => t.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::error_message::help_data::HelpData;
    use crate::utils::builder;

    // `type_comparison.rs` had zero inline tests despite `reduce_type` being
    // the most-called function of the checker (audit_type_checking.md §7,
    // Phase 6 item 4) — these cover every arm of `reduce_type_helper`.

    #[test]
    fn test_reduce_record_reduces_field_types() {
        let context = Context::default().push_alias("Meters".to_string(), builder::integer_type_default());
        let alias_field = Type::Alias("Meters".to_string(), vec![], false, HelpData::default());
        // Not `builder::record_type`: its `From<(String, Type)>` derives a
        // field's `HelpData` from the field's `Type` via `.into()`, which
        // panics on `Type::Alias` (unimplemented in that conversion) —
        // `ArgumentType::new` sidesteps it with an explicit default HelpData.
        let record = Type::Record(
            std::collections::HashSet::from([ArgumentType::new("x", &alias_field)]),
            HelpData::default(),
        );

        let reduced = reduce_type(&context, &record);

        match reduced {
            Type::Record(fields, _) => {
                let field = fields.iter().find(|f| f.get_argument_str() == "x").unwrap();
                assert_eq!(field.get_type(), builder::integer_type_default());
            }
            other => panic!("expected Record, got {:?}", other),
        }
    }

    #[test]
    fn test_reduce_alias_resolves_through_context() {
        let context = Context::default().push_alias("Meters".to_string(), builder::integer_type_default());
        let alias = Type::Alias("Meters".to_string(), vec![], false, HelpData::default());

        assert_eq!(reduce_type(&context, &alias), builder::integer_type_default());
    }

    #[test]
    fn test_reduce_alias_opaque_is_left_untouched() {
        let context = Context::default().push_alias("Meters".to_string(), builder::integer_type_default());
        let opaque_alias = Type::Alias("Meters".to_string(), vec![], true, HelpData::default());

        assert_eq!(reduce_type(&context, &opaque_alias), opaque_alias);
    }

    #[test]
    fn test_reduce_alias_unresolved_unknown_name_is_any() {
        let context = Context::default();
        let alias = Type::Alias("NotDeclared".to_string(), vec![], false, HelpData::default());

        assert!(matches!(reduce_type(&context, &alias), Type::Any(_)));
    }

    #[test]
    fn test_reduce_alias_unresolved_builtin_name_falls_back_to_builtin() {
        // Not registered via push_alias, but the four builtin names have a
        // hardcoded fallback in reduce_type_helper's unwrap_or_else.
        let context = Context::default();
        let alias = Type::Alias("Integer".to_string(), vec![], false, HelpData::default());

        assert_eq!(reduce_type(&context, &alias), builder::integer_type_default());
    }

    #[test]
    fn test_reduce_alias_self_referential_guarded_by_memory() {
        // reduce_alias pushes `name` onto `memory` before recursing into the
        // aliased type; an alias that (directly or indirectly) refers back to
        // itself must stop instead of looping forever.
        let context = Context::default();
        let alias = Type::Alias("Meters".to_string(), vec![], false, HelpData::default());
        let memory = Vector::new().push_back("Meters".to_string());

        assert_eq!(reduce_type_helper(&context, &alias, memory), alias);
    }

    #[test]
    fn test_reduce_tag_reduces_inner_type() {
        let context = Context::default().push_alias("Meters".to_string(), builder::integer_type_default());
        let alias_field = Type::Alias("Meters".to_string(), vec![], false, HelpData::default());
        let tag = Type::Tag("Some".to_string(), Box::new(alias_field), HelpData::default());

        match reduce_type(&context, &tag) {
            Type::Tag(name, inner, _) => {
                assert_eq!(name, "Some");
                assert_eq!(*inner, builder::integer_type_default());
            }
            other => panic!("expected Tag, got {:?}", other),
        }
    }

    #[test]
    fn test_reduce_if_type_drops_conditions() {
        // G5 (audit_type_checking.md): investigated, not a live bug to fix
        // here. `Type::If`/`Type::Condition` (the `T if <cond>` refinement
        // syntax parsed by `parsing/types.rs::if_type`/`type_condition`) has
        // no consumer anywhere else in the codebase — grepping the crate
        // turns up only construction (parsing), boilerplate derive-adjacent
        // impls (`with_help_data`/`PartialEq`/`Hash` in `type/mod.rs`), and
        // this drop in `reduce_type_helper`. No subtyping rule, unification
        // rule, or arithmetic pass ever inspects the condition list, so
        // there is nothing for `reduce_type` to "evaluate" the conditions
        // against — the feature is parse-only/inert, not a silent-Any-style
        // degradation. Dropping them here just unwraps to the base type,
        // consistent with that inertness. Documented rather than "fixed":
        // giving conditions real meaning is a full feature design (what do
        // they constrain, checked when/where), out of scope for this audit.
        let context = Context::default();
        let if_type = Type::If(Box::new(builder::integer_type_default()), vec![], HelpData::default());

        assert_eq!(reduce_type(&context, &if_type), builder::integer_type_default());
    }

    #[test]
    fn test_reduce_function_reduces_args_and_return() {
        let context = Context::default().push_alias("Meters".to_string(), builder::integer_type_default());
        let alias_field = Type::Alias("Meters".to_string(), vec![], false, HelpData::default());
        let fn_type = builder::function_type(&[alias_field.clone()], alias_field);

        match reduce_type(&context, &fn_type) {
            Type::Function(args, ret, _) => {
                assert_eq!(args[0].get_type(), builder::integer_type_default());
                assert_eq!(*ret, builder::integer_type_default());
            }
            other => panic!("expected Function, got {:?}", other),
        }
    }

    #[test]
    fn test_reduce_vec_reduces_element_type() {
        let context = Context::default().push_alias("Meters".to_string(), builder::integer_type_default());
        let alias_field = Type::Alias("Meters".to_string(), vec![], false, HelpData::default());
        let vec_type = builder::array_type(builder::integer_type(3), alias_field);

        match reduce_type(&context, &vec_type) {
            Type::Vec(_, _, elem, _) => assert_eq!(*elem, builder::integer_type_default()),
            other => panic!("expected Vec, got {:?}", other),
        }
    }

    #[test]
    fn test_reduce_union_collapses_when_one_side_is_subtype() {
        let context = Context::default();
        // Record structural subtyping: a record with a superset of fields is
        // a subtype of one with fewer fields (width subtyping), so the union
        // collapses to the wider (more specific) record.
        let wide = builder::record_type(&[
            ("x".to_string(), builder::integer_type_default()),
            ("y".to_string(), builder::integer_type_default()),
        ]);
        let narrow = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let union = builder::union_type(&[wide.clone(), narrow]);

        assert_eq!(reduce_type(&context, &union), wide);
    }

    #[test]
    fn test_reduce_union_unrelated_branches_are_reduced_in_depth() {
        // G4 (audit_type_checking.md): fixed — when neither branch is a
        // subtype of the other, reduce_type_helper now recurses into both
        // branches before rebuilding the union, so an alias buried inside an
        // unrelated union member does get resolved.
        let context = Context::default().push_alias("Meters".to_string(), builder::integer_type_default());
        let alias_field = Type::Alias("Meters".to_string(), vec![], false, HelpData::default());
        let union = builder::union_type(&[builder::character_type_default(), alias_field]);

        let reduced = reduce_type(&context, &union);
        let expected = builder::union_type(&[builder::character_type_default(), builder::integer_type_default()]);
        assert_eq!(reduced, expected);
    }

    #[test]
    fn test_reduce_arithmetic_operator_normalizes() {
        let context = Context::default();
        let sum = Type::Operator(
            TypeOperator::Addition,
            Box::new(builder::integer_type(2)),
            Box::new(builder::integer_type(3)),
            HelpData::default(),
        );

        match reduce_type(&context, &sum) {
            Type::Integer(crate::components::r#type::tint::Tint::Val(v), _) => assert_eq!(v, 5),
            other => panic!("expected Integer(5), got {:?}", other),
        }
    }

    #[test]
    fn test_reduce_intersection_operator_normalizes() {
        let context = Context::default();
        let point = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let extra = builder::record_type(&[("y".to_string(), builder::integer_type_default())]);
        let intersection = Type::Operator(
            TypeOperator::Intersection,
            Box::new(point),
            Box::new(extra),
            HelpData::default(),
        );

        match reduce_type(&context, &intersection) {
            Type::Record(fields, _) => assert_eq!(fields.len(), 2),
            other => panic!("expected merged Record, got {:?}", other),
        }
    }

    #[test]
    fn test_reduce_access_unresolvable_pieces_is_any() {
        let context = Context::default();
        let access = Type::Operator(
            TypeOperator::Access,
            Box::new(builder::integer_type_default()),
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );

        assert!(matches!(reduce_type(&context, &access), Type::Any(_)));
    }

    #[test]
    fn test_reduce_fallback_leaves_scalar_types_untouched() {
        let context = Context::default();
        assert_eq!(reduce_type(&context, &builder::boolean_type()), builder::boolean_type());
        assert_eq!(
            reduce_type(&context, &builder::character_type_default()),
            builder::character_type_default()
        );
    }

    #[test]
    fn test_reduce_param_reduces_argument_type() {
        let context = Context::default().push_alias("Meters".to_string(), builder::integer_type_default());
        let alias_field = Type::Alias("Meters".to_string(), vec![], false, HelpData::default());
        let param = ArgumentType::new("x", &alias_field);

        let reduced = reduce_param(&context, &param, Vector::new());

        assert_eq!(reduced.get_type(), builder::integer_type_default());
        assert_eq!(reduced.get_argument_str(), "x");
    }
}
