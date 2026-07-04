//! Projects a "facet" (record fields, or interface methods) out of a type
//! that may be a plain `Record`/`Interface`, an `Intersection` combining the
//! two (`List & Interface`), or a rigid generic bound to such a type via an
//! `interface_constraint` (see `function.rs`'s interface-parameter handling).
//!
//! `norm_intersection` (`type_arithmetic.rs`) already merges `Record & Record`
//! and `Interface & Interface` into a single `Record`/`Interface`, so a
//! reduced, irreducible intersection contains *at most one* `Record` member
//! and *at most one* `Interface` member. These two functions are the single
//! place that looks through that shape — callers (`$` field access, method
//! call resolution) project the facet they need instead of pattern-matching
//! `Type::Record`/`Type::Interface` directly, so a mixed `List & Interface`
//! parameter supports both `$` field access and interface method calls.
use crate::components::context::Context;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::intersection_type::IntersectionType;
use crate::components::r#type::Type;
use crate::processes::type_checking::type_comparison::reduce_type;
use std::collections::HashSet;

/// Project the record-field facet of `typ`: its own fields if it's a
/// `Record`, the `Record` member's fields if it's an intersection, or the
/// facet of its bound if it's a rigid generic constrained by
/// `add_interface_constraint` (which may itself be a `Record`, an
/// `Interface`, or an intersection of both).
pub fn record_facet(context: &Context, typ: &Type) -> Option<HashSet<ArgumentType>> {
    match reduce_type(context, typ) {
        Type::Record(fields, _) => Some(fields),
        Type::Operator(
            crate::components::r#type::type_operator::TypeOperator::Intersection,
            ..,
        ) => IntersectionType::try_from(reduce_type(context, typ))
            .ok()?
            .get_types()
            .iter()
            .find_map(|member| match reduce_type(context, member) {
                Type::Record(fields, _) => Some(fields),
                _ => None,
            }),
        Type::Generic(name, _) => {
            let bound = context.get_interface_constraint(&name)?.clone();
            record_facet(context, &bound)
        }
        _ => None,
    }
}

/// Project the interface-method facet of `typ`: its own methods if it's an
/// `Interface`, the `Interface` member's methods if it's an intersection, or
/// the facet of its bound if it's a rigid generic constrained by
/// `add_interface_constraint`.
pub fn interface_facet(context: &Context, typ: &Type) -> Option<HashSet<ArgumentType>> {
    match reduce_type(context, typ) {
        Type::Interface(methods, _) => Some(methods),
        Type::Operator(
            crate::components::r#type::type_operator::TypeOperator::Intersection,
            ..,
        ) => IntersectionType::try_from(reduce_type(context, typ))
            .ok()?
            .get_types()
            .iter()
            .find_map(|member| match reduce_type(context, member) {
                Type::Interface(methods, _) => Some(methods),
                _ => None,
            }),
        Type::Generic(name, _) => {
            let bound = context.get_interface_constraint(&name)?.clone();
            interface_facet(context, &bound)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::builder;

    #[test]
    fn test_record_facet_on_plain_record() {
        let context = Context::default();
        let typ = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let fields = record_facet(&context, &typ).unwrap();
        assert_eq!(fields.len(), 1);
    }

    #[test]
    fn test_interface_facet_on_plain_interface() {
        let context = Context::default();
        let typ = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let methods = interface_facet(&context, &typ).unwrap();
        assert_eq!(methods.len(), 1);
    }

    #[test]
    fn test_record_facet_on_mixed_intersection() {
        let context = Context::default();
        let record = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let iface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let typ = builder::intersection_type(&[record, iface]);
        let fields = record_facet(&context, &typ).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields.iter().next().unwrap().get_argument_str(), "x");
    }

    #[test]
    fn test_interface_facet_on_mixed_intersection() {
        let context = Context::default();
        let record = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let iface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let typ = builder::intersection_type(&[record, iface]);
        let methods = interface_facet(&context, &typ).unwrap();
        assert_eq!(methods.len(), 1);
        assert_eq!(methods.iter().next().unwrap().get_argument_str(), "view");
    }

    #[test]
    fn test_record_facet_on_record_only_intersection_is_none_for_interface() {
        let context = Context::default();
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let b = builder::record_type(&[("y".to_string(), builder::character_type_default())]);
        let typ = builder::intersection_type(&[a, b]);
        // Record & Record is merged by norm_intersection/reduce, so this is
        // already a plain Record with both fields — no Interface facet.
        assert!(interface_facet(&context, &typ).is_none());
        assert_eq!(record_facet(&context, &typ).unwrap().len(), 2);
    }

    #[test]
    fn test_record_facet_on_rigid_generic_bound_to_intersection() {
        let context = Context::default();
        let record = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let iface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let bound = builder::intersection_type(&[record, iface]);
        let context = context.add_interface_constraint("R0".to_string(), bound);
        let rigid = Type::Generic("R0".to_string(), Default::default());
        assert_eq!(record_facet(&context, &rigid).unwrap().len(), 1);
        assert_eq!(interface_facet(&context, &rigid).unwrap().len(), 1);
    }

    #[test]
    fn test_facets_none_on_unrelated_type() {
        let context = Context::default();
        let typ = builder::integer_type_default();
        assert!(record_facet(&context, &typ).is_none());
        assert!(interface_facet(&context, &typ).is_none());
    }
}
