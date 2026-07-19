//! `TypeName:{ x = 1, y = 2 }` (non-`Self` case; see `typing_self_constructor`
//! in `mod.rs` for `Self:{...}`): resolves the target alias, then validates
//! the field list against the alias's record shape — explicit fields,
//! runtime `...spread`s, and the static `..spread`/coverage check.
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::type_checking::flatten_operator_union;
use crate::processes::type_checking::merge_record_fields_override;
use crate::processes::type_checking::resolve_module_member_type;
use crate::processes::type_checking::type_context::TypeContext;
use crate::processes::type_checking::typing;
use crate::utils::builder;

/// `type_name` isn't a declared alias (no `type X <- ...;`) — but it might
/// still name a `Type::Tag` variant of some union alias in scope (e.g.
/// `Circle` from `type Shape <- .Circle(num) | .Square(num);`). Search every
/// union alias in `context` for a matching tag; returns its owning union
/// name if found. Used to turn the unqualified `TagName:{...}` form of
/// audit_type_checking.md U1 into a clear error instead of a silent `Any`
/// that only crashes once transpiled (tag constructors always take a single
/// positional payload, never named record-style fields).
fn find_tag_owner(context: &Context, type_name: &str) -> Option<String> {
    context.aliases().find_map(|(var, target)| {
        flatten_operator_union(target)
            .iter()
            .any(|member| matches!(member, Type::Tag(name, _, _) if name == type_name))
            .then(|| var.get_name().to_string())
    })
}

#[allow(clippy::too_many_arguments)]
pub fn constructor_call(
    context: &Context,
    expr: &Lang,
    module_path: &[String],
    type_name: &str,
    fields: &[ArgumentValue],
    spread: &Option<(Vec<String>, String, HelpData)>,
    spreads: &[Lang],
    h: &HelpData,
) -> TypeContext {
    // `generics` is the declared generic parameter list for this alias (e.g.
    // `[T]` for `type Box<T> <- list{value:T}`), only available for the
    // module-local case (`get_matching_alias_signature` — same accessor G3
    // uses for arity checking). A module-qualified constructor call
    // (`module_path` non-empty) keeps the pre-existing behaviour (0
    // generics inferred) — `resolve_module_member_type` only returns the
    // already-reduced exported type, not the (target, generics) pair.
    let (resolved_alias, generics) = if module_path.is_empty() {
        match context.get_matching_alias_signature(&Var::from_name(type_name)) {
            Some((target, generics)) => (Some(target), generics),
            None => (None, vec![]),
        }
    } else {
        (resolve_module_member_type(context, module_path, type_name), vec![])
    };
    if resolved_alias.is_none() && module_path.is_empty() {
        if let Some(union_name) = find_tag_owner(context, type_name) {
            return TypeContext::new(builder::any_type(), expr.clone(), context.clone()).with_errors(vec![
                TypRError::Type(TypeError::TagFieldConstructorNotSupported(
                    type_name.to_string(),
                    union_name,
                    h.clone(),
                )),
            ]);
        }
    }
    let Some(resolved_alias) = resolved_alias else {
        return TypeContext::new(builder::any_type(), expr.clone(), context.clone());
    };

    let mut errors: Vec<TypRError> = Vec::new();
    // Display-only, arity-less alias reference used in error messages below
    // (`FieldNotFound`/`SpreadTypeMismatch`/`MissingField`) — the concrete
    // generic arguments aren't relevant to those diagnostics.
    let display_type = Type::Alias(type_name.to_string(), vec![], false, h.clone());
    // (field value type, declared field type) pairs, collected below while
    // walking `fields`/the spread coverage loop — used to infer this
    // constructor call's concrete generic arguments (`Box:{value=5}` should
    // type as `Box<int>`, not the bare, 0-arity `Box`) by unifying against
    // `generics`, the alias's still-generic-bearing declared field types
    // (e.g. `value: T`).
    let mut unify_pairs: Vec<(Type, Type)> = Vec::new();
    // Thread the context through field/spread typing so that types
    // registered while typing the values (e.g. the on-the-fly ArrayN
    // alias created by an inline `as! [T]` cast) survive to transpile
    // time — otherwise their `as.<name>` lookup falls back to the
    // meaningless `as.Generic()`.
    let mut new_context = context.clone();

    // Record-shaped target: validate the field list (this also fixes the
    // preexisting bug where `fields` was never checked at all, spread or not).
    if let Type::Record(record_fields, _) = resolved_alias.reduce(context) {
        let mut seen = std::collections::HashSet::new();
        for f in fields {
            if !seen.insert(f.get_argument()) {
                errors.push(TypRError::Type(TypeError::DuplicateField(f.get_argument(), h.clone())));
            }
        }

        for f in fields {
            let fname = f.get_argument();
            match record_fields.iter().find(|rf| rf.get_argument_str() == fname) {
                Some(rf) => {
                    let value_tc = typing(&new_context, &f.get_value());
                    errors.extend(value_tc.errors);
                    // reduce_and_subtype (not is_subtype): `record_fields`
                    // comes from the *reduced* record, so a field declared
                    // as `[Option]` reads `[any, .Some(T) | .None]` here,
                    // while the value keeps its unreduced `[any, Option]`
                    // alias — is_subtype would split the union before ever
                    // reducing the alias and spuriously fail.
                    if !value_tc.value.reduce_and_subtype(&rf.get_type(), context).0 {
                        errors.push(TypRError::Type(TypeError::Param(rf.get_type(), value_tc.value.clone())));
                    }
                    unify_pairs.push((value_tc.value.clone(), rf.get_type()));
                    new_context = value_tc.context;
                }
                None => {
                    errors.push(TypRError::Type(TypeError::FieldNotFound(
                        (fname, h.clone()),
                        display_type.clone(),
                    )));
                }
            }
        }

        // Runtime `...source` spread(s) (spread_operator2.md): structural
        // merge, sequential like in a record literal. Unlike the static `..`
        // spread below, this doesn't require `source` to be exactly
        // `Alias(type_name)` — only that the merged result covers every
        // declared field with a compatible type.
        let mut spread_merged: Vec<ArgumentType> = Vec::new();
        for spread_expr in spreads {
            let tc = typing(&new_context, spread_expr);
            errors.extend(tc.errors);
            new_context = tc.context;
            match tc.value.reduce(context) {
                Type::Record(spread_fields, _) => {
                    spread_merged =
                        merge_record_fields_override(&spread_merged, &spread_fields.into_iter().collect::<Vec<_>>());
                }
                _ => {
                    errors.push(TypRError::Type(TypeError::WrongExpression(spread_expr.get_help_data())));
                }
            }
        }

        match spread {
            Some((spread_path, spread_var, spread_h)) => {
                let spread_type = if spread_path.is_empty() {
                    context.get_type_from_variable(&Var::from_name(spread_var)).ok()
                } else {
                    resolve_module_member_type(context, spread_path, spread_var)
                };
                match spread_type {
                    Some(Type::Alias(name, ..)) if name == *type_name => {}
                    Some(found) => {
                        errors.push(TypRError::Type(TypeError::SpreadTypeMismatch(
                            display_type.clone(),
                            found,
                            spread_h.clone(),
                        )));
                    }
                    None => {
                        errors.push(TypRError::Type(TypeError::UndefinedVariable(Lang::Variable {
                            name: spread_var.clone(),
                            is_opaque: false,
                            related_type: builder::any_type(),
                            help_data: spread_h.clone(),
                        })));
                    }
                }
            }
            None => {
                // No static `..` spread: every record field must be covered
                // either explicitly or by a runtime `...` spread above.
                let provided: std::collections::HashSet<String> = fields.iter().map(|f| f.get_argument()).collect();
                for rf in &record_fields {
                    let rname = rf.get_argument_str();
                    if provided.contains(&rname) {
                        continue;
                    }
                    match spread_merged.iter().find(|mf| mf.get_argument_str() == rname) {
                        Some(mf) => {
                            if !mf.get_type().is_subtype(&rf.get_type(), context).0 {
                                errors.push(TypRError::Type(TypeError::Param(rf.get_type(), mf.get_type())));
                            }
                            unify_pairs.push((mf.get_type(), rf.get_type()));
                        }
                        None => {
                            errors.push(TypRError::Type(TypeError::MissingField(
                                rname,
                                display_type.clone(),
                                h.clone(),
                            )));
                        }
                    }
                }
            }
        }
    }

    // Infer the concrete generic arguments (`Box<int>` rather than the bare,
    // 0-arity `Box`) by unifying every collected (value type, declared
    // generic-bearing field type) pair against `generics` — same mechanism
    // `FunctionType::infer_return_type` uses for a call's return type
    // (`Context::get_unification_map` + `UnificationMap::apply_unification_type`).
    // A generic that can't be pinned down (no field mentions it, or a
    // conflicting unification) is left as-is rather than erroring: it
    // degrades to the pre-fix behaviour for that one argument instead of
    // rejecting an otherwise-valid construction.
    let concret_types: Vec<Type> = if generics.is_empty() {
        vec![]
    } else {
        let (entered, params): (Vec<Type>, Vec<Type>) = unify_pairs.into_iter().unzip();
        match new_context.get_unification_map(&entered, &params) {
            Some(um) => generics
                .iter()
                .map(|g| um.apply_unification_type(&new_context, g).0)
                .collect(),
            None => generics.clone(),
        }
    };
    let target_type = Type::Alias(type_name.to_string(), concret_types, false, h.clone());

    TypeContext::new(target_type, expr.clone(), new_context).with_errors(errors)
}

#[cfg(test)]
mod tests {
    use crate::components::context::Context;
    use crate::components::error_message::type_error::TypeError;
    use crate::components::error_message::typr_error::TypRError;
    use crate::components::r#type::Type;
    use crate::processes::parsing::parse_from_string;
    use crate::processes::type_checking::typing_with_errors;

    #[test]
    fn edge_p8_constructor_call_infers_generic_type_argument() {
        // audit_type_checking.md Phase 8: `Box:{value=5}` used to type as the
        // bare, 0-arity `Box` alias instead of `Box<int>` — the concrete
        // generic argument was never inferred from the field values.
        let ast = parse_from_string(
            "type Box<T> <- list { value: T }; \
             Box:{ value = 5 };",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(!result.has_errors(), "unexpected errors: {:?}", result.get_errors());
        match result.get_type() {
            Type::Alias(name, args, _, _) => {
                assert_eq!(name, "Box");
                assert_eq!(args.len(), 1, "expected exactly one inferred type argument");
                assert!(
                    matches!(args[0], Type::Integer(_, _)),
                    "expected Box<int>, got Box<{:?}>",
                    args[0]
                );
            }
            other => panic!("expected a Box alias type, got {:?}", other),
        }
    }

    #[test]
    fn edge_p8_inferred_generic_argument_is_checked_at_call_sites() {
        // Companion test: the inferred `Box<int>` must actually be checked
        // against callers expecting a different instantiation — before the
        // fix, every `Box:{...}` literal typed as the same generics-less
        // `Box`, so this call wrongly type-checked no matter what `T` the
        // parameter declared.
        let ast = parse_from_string(
            "type Box<T> <- list { value: T }; \
             let get_char <- fn(b: Box<char>): char { b.value }; \
             get_char(Box:{ value = 5 });",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(
            result.has_errors(),
            "Box:{{value=5}} (Box<int>) should not satisfy a Box<char> parameter"
        );
    }

    #[test]
    fn edge_p8_inferred_generic_argument_matches_compatible_call_site() {
        // Positive counterpart: the same construction against a matching
        // `Box<int>` parameter must still type-check cleanly.
        let ast = parse_from_string(
            "type Box<T> <- list { value: T }; \
             let get_value <- fn(b: Box<int>): int { b.value }; \
             get_value(Box:{ value = 5 });",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(!result.has_errors(), "unexpected errors: {:?}", result.get_errors());
    }

    // ==================== U1: TagName:{...} rejected for tag members ====================
    // audit_type_checking.md U1: `TagName:{ field = val }` (unqualified,
    // ConstructorCall) type-checked to `Any` (no error) but always crashed at
    // R runtime — the generated tag constructor is `Variant <- function(x)
    // {...}`, a single positional payload, never named fields. Piste 1
    // (chosen fix): make the mismatched syntax a compile error, for every
    // tag shape (scalar or record payload) — never allow the false case to
    // reach a "successful" compile in the first place.

    #[test]
    fn edge_u1_unqualified_scalar_tag_field_syntax_is_rejected() {
        let ast = parse_from_string(
            "type Shape <- .Circle(num) | .Square(num); \
             Circle:{ r = 3.0 };",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(
            result.has_errors(),
            "Circle:{{ r = 3.0 }} must be rejected: Circle's real constructor takes one \
             positional payload, not a named `r` field"
        );
        assert!(
            result.get_errors().iter().any(|e| matches!(
                e,
                TypRError::Type(TypeError::TagFieldConstructorNotSupported(variant, union, _))
                    if variant == "Circle" && union == "Shape"
            )),
            "expected TagFieldConstructorNotSupported(Circle, Shape, _), got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn edge_u1_unqualified_record_payload_tag_field_syntax_is_rejected() {
        // Even when the tag's payload is itself record-shaped, the generated
        // constructor still only accepts one positional `x` (the whole
        // payload) — `.Circle(:{ r = 3.0 })` is required, not `Circle:{ r =
        // 3.0 }`.
        let ast = parse_from_string(
            "type Shape <- .Circle(list { r: num }) | .Square(num); \
             Circle:{ r = 3.0 };",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(
            result.has_errors(),
            "Circle:{{ r = 3.0 }} must still be rejected even for a record-shaped tag payload"
        );
        assert!(
            result.get_errors().iter().any(|e| matches!(
                e,
                TypRError::Type(TypeError::TagFieldConstructorNotSupported(variant, union, _))
                    if variant == "Circle" && union == "Shape"
            )),
            "expected TagFieldConstructorNotSupported(Circle, Shape, _), got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn edge_u1_qualified_tag_field_syntax_is_rejected() {
        let ast = parse_from_string(
            "type Shape <- .Circle(num) | .Square(num); \
             Shape.Circle:{ r = 3.0 };",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(result.has_errors(), "Shape.Circle:{{ r = 3.0 }} must be rejected");
        assert!(
            result.get_errors().iter().any(|e| matches!(
                e,
                TypRError::Type(TypeError::TagFieldConstructorNotSupported(variant, union, _))
                    if variant == "Circle" && union == "Shape"
            )),
            "expected TagFieldConstructorNotSupported(Circle, Shape, _), got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn edge_u1_qualified_bare_tag_with_payload_is_rejected() {
        // `Shape.Circle` with no `:{...}` at all is also invalid once the
        // tag has a non-empty payload: the transpiled `Circle()` call would
        // still crash (missing the required `x` argument). Only a genuinely
        // zero-payload tag (e.g. `Color.Red`, see `test_union_constructor_typing`)
        // may be referenced this way.
        let ast = parse_from_string(
            "type Shape <- .Circle(num) | .Square(num); \
             Shape.Circle;",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(
            result.has_errors(),
            "bare Shape.Circle (non-empty payload, no braces) must be rejected"
        );
        assert!(
            result.get_errors().iter().any(|e| matches!(
                e,
                TypRError::Type(TypeError::TagFieldConstructorNotSupported(variant, union, _))
                    if variant == "Circle" && union == "Shape"
            )),
            "expected TagFieldConstructorNotSupported(Circle, Shape, _), got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn edge_u1_unknown_union_variant_is_rejected() {
        let ast = parse_from_string(
            "type Shape <- .Circle(num) | .Square(num); \
             Shape.Triangle;",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(
            result.has_errors(),
            "Shape.Triangle must be rejected: Triangle isn't a variant of Shape"
        );
        assert!(
            result.get_errors().iter().any(|e| matches!(
                e,
                TypRError::Type(TypeError::UnknownUnionVariant(variant, union, _))
                    if variant == "Triangle" && union == "Shape"
            )),
            "expected UnknownUnionVariant(Triangle, Shape, _), got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn edge_u1_record_alias_union_member_still_works() {
        // Not a Tag at all: `Rgb` is a genuine top-level record alias used
        // directly as a union member (no `.Rgb(...)` wrapping) — its
        // generated constructor really does take named fields, so this form
        // must keep working (regression guard against U1's fix being too
        // broad).
        let ast = parse_from_string(
            "type Rgb <- list { r: int, g: int, b: int }; \
             type Color <- .Red | .Blue | Rgb; \
             Color.Rgb:{ r = 10, g = 20, b = 30 };",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(!result.has_errors(), "unexpected errors: {:?}", result.get_errors());
    }

    #[test]
    fn edge_u1_zero_payload_tag_bare_reference_still_works() {
        // Regression guard: the one legitimately working shape of this
        // syntax (a bare qualified reference to a zero-arg tag) must be
        // untouched by the fix.
        let ast = parse_from_string(
            "type Color <- .Red | .Blue; \
             Color.Red;",
            "test.ty",
        );
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(!result.has_errors(), "unexpected errors: {:?}", result.get_errors());
    }
}
