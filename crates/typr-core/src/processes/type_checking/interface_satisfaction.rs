//! Structural interface satisfaction — the core check behind interface
//! constructors (`interface_constructeurs.md`): "does this concrete type
//! provide every method an interface requires, with a compatible signature?"
//!
//! This is deliberately independent from `Type::is_subtype_raw`'s
//! `(_, Interface)` arm, which only accepts an exact `ArgumentType` match
//! (name + type equality) between the candidate's natural interface
//! (`Type::to_interface`) and the required one. The RFC (§8.1) asks for
//! variance-aware compatibility instead: a candidate method may accept a
//! *broader* parameter type than required (contravariance) and return a
//! *narrower* type than required (covariance) and still satisfy the
//! interface. Both `check_interface_satisfaction` (used at alias-declaration
//! time for `Record & Interface` intersections, `type_checking/mod.rs`'s
//! `Lang::Alias` arm) and the `I(x)` interface-constructor call form
//! (`function_application.rs`) share this single primitive.
use crate::components::context::Context;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::intersection_type::IntersectionType;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::type_checking::type_comparison::reduce_type;
use std::collections::HashSet;

/// Why `concrete` fails to satisfy an interface's required methods.
#[derive(Debug, Clone)]
pub enum InterfaceSatisfactionError {
    /// Required method names with no same-named counterpart at all.
    Missing(Vec<String>),
    /// A same-named method exists but its signature isn't compatible —
    /// `(method_name, required_signature, found_signature)`.
    Incompatible(String, Box<Type>, Box<Type>),
}

/// Gathers the union of "natural interface" methods (`Type::to_interface`)
/// that `concrete` provides. For a plain type this is just its own natural
/// interface. For an intersection (`Movable & Point`), `to_interface` must be
/// called on each *raw*, un-reduced member that is itself record-shaped
/// (`Point`, not the merged/anonymous record `facets::record_facet` would
/// build) — free functions are looked up by `Context::get_functions_from_type`
/// via an exact match against the *nominal* type a `let` parameter was
/// declared with (e.g. `Type::Alias("Point", ...)`), so collapsing several
/// record members into one synthetic `Type::Record` would silently break
/// that lookup for every named-alias member.
fn candidate_methods(context: &Context, concrete: &Type) -> HashSet<ArgumentType> {
    match concrete {
        Type::Operator(TypeOperator::Intersection, ..) => {
            match IntersectionType::try_from(concrete.clone()) {
                Ok(flat) => flat
                    .get_types()
                    .iter()
                    .filter(|member| matches!(reduce_type(context, member), Type::Record(_, _)))
                    .flat_map(|member| match member.to_interface(context) {
                        Type::Interface(methods, _) => methods,
                        _ => HashSet::new(),
                    })
                    .collect(),
                Err(_) => HashSet::new(),
            }
        }
        _ => match concrete.to_interface(context) {
            Type::Interface(methods, _) => methods,
            _ => HashSet::new(),
        },
    }
}

/// Checks that `concrete` structurally implements every method in
/// `required_methods` (typically an interface's method set, e.g. from
/// `facets::interface_facet`). `concrete` should be the type as originally
/// written (e.g. the full `Movable & Point` intersection, or a plain
/// `Type::Alias`/`Type::Record`) rather than a facet-merged reconstruction —
/// see `candidate_methods` above. Returns `Ok(())` when satisfied.
pub fn check_interface_satisfaction(
    context: &Context,
    concrete: &Type,
    required_methods: &HashSet<ArgumentType>,
) -> Result<(), InterfaceSatisfactionError> {
    let candidate_methods = candidate_methods(context, concrete);

    let mut missing = Vec::new();
    for required in required_methods {
        let name = required.get_argument_str();
        match candidate_methods
            .iter()
            .find(|m| m.get_argument_str() == name)
        {
            None => missing.push(name),
            Some(found) => {
                if missing.is_empty()
                    && !signatures_compatible(context, &required.get_type(), &found.get_type())
                {
                    return Err(InterfaceSatisfactionError::Incompatible(
                        name,
                        Box::new(required.get_type()),
                        Box::new(found.get_type()),
                    ));
                }
            }
        }
    }

    if !missing.is_empty() {
        missing.sort();
        return Err(InterfaceSatisfactionError::Missing(missing));
    }
    Ok(())
}

/// `found` (the candidate's method) is compatible with `required` (the
/// interface's declared method) when parameters are contravariant (each
/// required parameter must be a subtype of the candidate's corresponding
/// parameter — the candidate is allowed to accept a broader type) and the
/// return type is covariant (the candidate's return must be a subtype of the
/// required return type).
fn signatures_compatible(context: &Context, required: &Type, found: &Type) -> bool {
    match (required, found) {
        (Type::Function(req_args, req_ret, _), Type::Function(found_args, found_ret, _)) => {
            req_args.len() == found_args.len()
                && req_args
                    .iter()
                    .zip(found_args.iter())
                    .all(|(rp, fp)| rp.get_type().is_subtype(&fp.get_type(), context).0)
                && found_ret.is_subtype(req_ret, context).0
        }
        _ => required == found,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::builder;

    fn movable_methods() -> HashSet<ArgumentType> {
        let mut set = HashSet::new();
        set.insert(ArgumentType::new(
            "move",
            &builder::function_type(
                &[
                    builder::self_generic_type(),
                    builder::number_type(),
                    builder::number_type(),
                ],
                builder::self_generic_type(),
            ),
        ));
        set
    }

    #[test]
    fn test_missing_method() {
        let context = Context::default();
        let concrete = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let err = check_interface_satisfaction(&context, &concrete, &movable_methods())
            .expect_err("should be missing `move`");
        match err {
            InterfaceSatisfactionError::Missing(names) => {
                assert_eq!(names, vec!["move".to_string()])
            }
            _ => panic!("expected Missing"),
        }
    }

    #[test]
    fn test_satisfied_via_free_function() {
        let base = Context::default().push_alias("Point".to_string(), builder::record_type(&[]));
        let move_fn_type = builder::function_type(
            &[
                Type::Alias("Point".to_string(), vec![], false, Default::default()),
                builder::number_type(),
                builder::number_type(),
            ],
            Type::Alias("Point".to_string(), vec![], false, Default::default()),
        );
        let concrete = Type::Alias("Point".to_string(), vec![], false, Default::default());
        let move_var =
            crate::components::language::var::Var::from_name("move").set_type(concrete.clone());
        let context = base.clone().push_var_type(move_var, move_fn_type, &base);
        assert!(check_interface_satisfaction(&context, &concrete, &movable_methods()).is_ok());
    }

    #[test]
    fn test_contravariant_param_and_covariant_return_ok() {
        // Interface requires (Self, number, number) -> Self. A candidate that
        // accepts a broader `Any` param and returns a narrower alias whose
        // supertype is Self should still be considered compatible for the
        // plain-equality parts (Self <-> Self here), while genuinely broader
        // param types must be accepted.
        let context = Context::default();
        let required = builder::function_type(
            &[
                builder::integer_type_default(),
                builder::integer_type_default(),
            ],
            builder::integer_type_default(),
        );
        let found = builder::function_type(
            &[builder::any_type(), builder::any_type()],
            builder::integer_type_default(),
        );
        assert!(signatures_compatible(&context, &required, &found));
    }

    #[test]
    fn test_incompatible_param_type() {
        let context = Context::default();
        let required =
            builder::function_type(&[builder::any_type()], builder::integer_type_default());
        let found = builder::function_type(
            &[builder::integer_type_default()],
            builder::integer_type_default(),
        );
        // `found` only accepts int, but the interface promises callers can
        // pass Any — found is narrower than required, so incompatible.
        assert!(!signatures_compatible(&context, &required, &found));
    }
}
