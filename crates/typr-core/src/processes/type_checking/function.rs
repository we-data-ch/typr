use crate::components::error_message::syntax_error::SyntaxError;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::var::Var;
use crate::components::r#type::kind::Kind;
use crate::components::r#type::type_system::TypeSystem;
use crate::processes::type_checking::facets;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::processes::type_checking::ArgumentType;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::TypeContext;
use crate::utils::builder;
use crate::Context;
use crate::Lang;
use crate::Type;

/// RFC sigils.md §7 — the kind a generic name's occurrence implies, used
/// only by the intra-signature consistency pass below. Distinguishes "no
/// sigil" (`Any`) from "Number sigil" (`#`, `IndexGen`) from one of the four
/// `Kind`-enum sigils. Kept local: `Kind` itself has no `Number`/`Any`
/// variant by design (Number stays `IndexGen`-based).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ObservedKind {
    Any,
    Number,
    Sigil(Kind),
}

impl ObservedKind {
    fn describe(&self) -> String {
        match self {
            ObservedKind::Any => "Any".to_string(),
            ObservedKind::Number => "Number".to_string(),
            ObservedKind::Sigil(Kind::Record) => "Record".to_string(),
            ObservedKind::Sigil(Kind::Interface) => "Interface".to_string(),
            ObservedKind::Sigil(Kind::String) => "String".to_string(),
            ObservedKind::Sigil(Kind::Boolean) => "Boolean".to_string(),
        }
    }
}

/// Collects `(generic_name, observed_kind, position)` for every occurrence of
/// every bare-name generic reachable inside `typ`. Mirrors the recursion
/// shape of `Type::extract_generics`, but records one entry per occurrence
/// (not deduplicated) together with the kind implied by that occurrence.
fn collect_generic_kind_occurrences(typ: &Type, acc: &mut Vec<(String, ObservedKind, HelpData)>) {
    match typ {
        Type::Generic(name, h) => acc.push((name.clone(), ObservedKind::Any, h.clone())),
        Type::IndexGen(name, h) => acc.push((name.clone(), ObservedKind::Number, h.clone())),
        Type::KindedGen(k, name, h) => acc.push((name.clone(), ObservedKind::Sigil(*k), h.clone())),
        Type::Function(args, ret, _) => {
            args.iter()
                .for_each(|a| collect_generic_kind_occurrences(&a.get_type(), acc));
            collect_generic_kind_occurrences(ret, acc);
        }
        Type::Vec(_, idx, body, _) => {
            collect_generic_kind_occurrences(idx, acc);
            collect_generic_kind_occurrences(body, acc);
        }
        Type::Record(fields, _) | Type::Interface(fields, _) => fields
            .iter()
            .for_each(|a| collect_generic_kind_occurrences(&a.get_type(), acc)),
        Type::Tag(_, inner, _) | Type::Multi(inner, _) => {
            collect_generic_kind_occurrences(inner, acc)
        }
        Type::Operator(_, a, b, _) => {
            collect_generic_kind_occurrences(a, acc);
            collect_generic_kind_occurrences(b, acc);
        }
        Type::Params(ts, _) | Type::Tuple(ts, _) => ts
            .iter()
            .for_each(|t| collect_generic_kind_occurrences(t, acc)),
        Type::Alias(_, params, _, _) => params
            .iter()
            .for_each(|t| collect_generic_kind_occurrences(t, acc)),
        _ => {}
    }
}

/// RFC sigils.md §7 — intra-signature kind-consistency pass. For every
/// generic name occurring across `params` + `ret_ty`, the first non-Any
/// occurrence establishes that name's kind; any later occurrence with a
/// different non-Any kind raises one `TypeError::KindMismatch`. Bare/Any
/// occurrences never conflict with anything. Bounded to a single function
/// signature — not whole-program inference.
fn check_kind_consistency(params: &[ArgumentType], ret_ty: &Type) -> Vec<TypRError> {
    let mut occurrences: Vec<(String, ObservedKind, HelpData)> = Vec::new();
    for p in params {
        collect_generic_kind_occurrences(&p.get_type(), &mut occurrences);
    }
    collect_generic_kind_occurrences(ret_ty, &mut occurrences);

    let mut seen_names: Vec<String> = Vec::new();
    for (name, _, _) in &occurrences {
        if !seen_names.contains(name) {
            seen_names.push(name.clone());
        }
    }

    // Per RFC §7.2: a bare occurrence (`Any`) is itself a kind commitment
    // (the type-checker must accept any concrete type there), so it
    // conflicts with a sigiled occurrence of the same name just as much as
    // two different sigils conflict with each other. So: every occurrence of
    // a given name must share the exact same `ObservedKind`; the first
    // occurrence establishes it, anything that differs is an error.
    let mut errors = Vec::new();
    for name in seen_names {
        let for_name: Vec<&(String, ObservedKind, HelpData)> =
            occurrences.iter().filter(|(n, _, _)| n == &name).collect();
        let Some((_, established, _)) = for_name.first() else {
            continue;
        };
        let established = *established;
        for (_, k, h) in &for_name {
            if *k != established {
                errors.push(TypRError::Type(TypeError::KindMismatch(
                    established.describe(),
                    k.describe(),
                    h.clone(),
                )));
            }
        }
    }
    errors
}

/// Validates default-parameter declarations on a function signature:
/// once a parameter has a default value, every later non-variadic
/// parameter must also have one (`NonTrailingDefaultParam`), and each
/// default expression must type-check against its declared parameter type.
/// Defaults are closed expressions — typed against the function's
/// *enclosing* `context`, not a scope where sibling parameters are bound —
/// so a default referencing another parameter surfaces as the ordinary
/// `UndefinedVariable` error, no special-casing needed.
fn check_default_params(params: &[ArgumentType], context: &Context) -> Vec<TypRError> {
    let mut errors = Vec::new();
    let mut seen_default = false;
    for arg_typ in params {
        if arg_typ.is_variadic() {
            continue;
        }
        match arg_typ.get_default() {
            Some(default_lang) => {
                seen_default = true;
                let declared_type = arg_typ.get_type();
                let default_tc = default_lang.typing(context);
                errors.extend(default_tc.errors.clone());
                let reduced_declared = reduce_type(context, &declared_type);
                let is_bare_generic = matches!(
                    reduced_declared,
                    Type::Generic(_, _)
                        | Type::IndexGen(_, _)
                        | Type::LabelGen(_, _)
                        | Type::KindedGen(_, _, _)
                );
                if !is_bare_generic
                    && !default_tc
                        .value
                        .reduce_and_subtype(&declared_type, context)
                        .0
                {
                    errors.push(TypRError::Type(TypeError::Param(
                        declared_type,
                        default_tc.value.clone(),
                    )));
                }
            }
            None if seen_default => {
                errors.push(TypRError::Type(TypeError::NonTrailingDefaultParam(
                    arg_typ.get_argument_str(),
                    arg_typ.get_type().get_help_data(),
                )));
            }
            None => {}
        }
    }
    errors
}

fn is_opaque_of(body_type: &Type, declared_ret: &Type) -> bool {
    match (body_type, declared_ret) {
        (Type::Alias(name1, _, true, _), Type::Alias(name2, _, false, _)) => {
            name1 == &format!("{}_", name2)
        }
        _ => false,
    }
}

/// Collects every interface-faceted type reachable inside `typ` (already
/// reduced — `reduce_type` recurses through `Vec`/`Record`/`Function`/`Tag`/
/// `Operator` bodies on its own, so a single top-level reduction is enough).
/// A node that itself has an interface facet (a plain `Interface`, or an
/// intersection like `List & Interface`) is recorded and not descended into
/// further; anything else is walked structurally so an interface nested
/// inside an array element, record field, tuple slot, or function
/// arg/return still counts as "anchored" by that parameter. Mirrors the
/// recursion shape of `collect_generic_kind_occurrences` above.
fn collect_interface_leaf_types(context: &Context, typ: &Type, acc: &mut Vec<Type>) {
    if facets::interface_facet(context, typ).is_some() {
        acc.push(typ.clone());
        return;
    }
    match typ {
        Type::Function(args, ret, _) => {
            args.iter()
                .for_each(|a| collect_interface_leaf_types(context, &a.get_type(), acc));
            collect_interface_leaf_types(context, ret, acc);
        }
        Type::Vec(_, idx, body, _) => {
            collect_interface_leaf_types(context, idx, acc);
            collect_interface_leaf_types(context, body, acc);
        }
        Type::Record(fields, _) => fields
            .iter()
            .for_each(|a| collect_interface_leaf_types(context, &a.get_type(), acc)),
        Type::Tag(_, inner, _) | Type::Multi(inner, _) => {
            collect_interface_leaf_types(context, inner, acc)
        }
        Type::Operator(_, a, b, _) => {
            collect_interface_leaf_types(context, a, acc);
            collect_interface_leaf_types(context, b, acc);
        }
        Type::Params(ts, _) | Type::Tuple(ts, _) => ts
            .iter()
            .for_each(|t| collect_interface_leaf_types(context, t, acc)),
        Type::Alias(_, params, _, _) => params
            .iter()
            .for_each(|t| collect_interface_leaf_types(context, t, acc)),
        _ => {}
    }
}

/// Returns true when the return type is an interface (or has an interface
/// facet — e.g. `List & Interface`) that is not "anchored" by any
/// parameter — i.e. it doesn't reappear, at any depth, inside a parameter's
/// type. Interfaces are treated like generics: a function is free to return
/// one as long as *some* parameter carries a value that could concretely
/// supply it (directly, or nested inside an array/record/tuple/etc.), so the
/// runtime representation flows in from an argument. Without such an anchor
/// the position would require an existential type, which TypR does not
/// support — the caller should use an opaque type instead.
fn is_interface_return_only(params: &[ArgumentType], ret_ty: &Type, context: &Context) -> bool {
    let reduced_ret = reduce_type(context, ret_ty);
    if facets::interface_facet(context, &reduced_ret).is_none() {
        return false;
    }
    let mut anchors = Vec::new();
    for p in params {
        let reduced_param = reduce_type(context, &p.get_type());
        collect_interface_leaf_types(context, &reduced_param, &mut anchors);
    }
    !anchors.contains(&reduced_ret)
}

/// Check if a type is a constrained rigid generic that satisfies the declared return type.
/// This handles the case where the body returns a rigid variable `A` and the declared return
/// is the interface `I` that constrains `A`.
fn is_rigid_compatible(body_type: &Type, declared_ret: &Type, context: &Context) -> bool {
    let name = match body_type {
        Type::Generic(name, _) => name,
        _ => return false,
    };
    let Some(interface) = context.get_interface_constraint(name) else {
        return false;
    };
    let reduced_ret = reduce_type(context, declared_ret);
    *interface == reduced_ret || interface.is_subtype_raw(&reduced_ret, context)
}

pub fn function(
    context: &Context,
    expr: &Lang,
    params: &[ArgumentType],
    ret_ty: &Type,
    body: &Lang,
    h: &HelpData,
) -> TypeContext {
    if let Lang::Scope {
        body: scope_body,
        help_data: scope_h,
    } = body
    {
        if scope_body.is_empty() {
            let error = TypRError::Syntax(SyntaxError::EmptyFunctionBody(scope_h.clone()));
            return TypeContext::new(
                Type::Function(params.to_vec(), Box::new(ret_ty.clone()), h.clone()),
                expr.clone(),
                context.clone(),
            )
            .with_errors(vec![error]);
        }
    }

    let list_of_types = params.to_vec();

    // Error early if the return type is an interface not bound to any parameter.
    // Such a position would require an existential type; use an opaque type instead.
    if is_interface_return_only(params, ret_ty, context) {
        return TypeContext::new(
            Type::Function(list_of_types, Box::new(ret_ty.clone()), h.clone()),
            expr.clone(),
            context.clone(),
        )
        .with_errors(vec![TypRError::Type(TypeError::InterfaceReturnOnly(
            ret_ty.clone(),
        ))]);
    }

    // RFC sigils.md §7 — intra-signature kind-consistency pass.
    let kind_consistency_errors = check_kind_consistency(params, ret_ty);
    let default_param_errors = check_default_params(params, context);

    // Build sub-context: interface parameters (including a `List & Interface`
    // intersection that has an interface facet) become rigid generic
    // variables with interface constraints, instead of being decomposed via
    // push_interface. The full reduced type (record facet included) is kept
    // as the constraint, so `facets::record_facet` can still recover the
    // record side of a mixed intersection through the rigid generic.
    let mut sub_context = context.clone();
    for arg_typ in params {
        let param_type = arg_typ.body_type();
        let reduced = reduce_type(&sub_context, &param_type);
        if facets::interface_facet(&sub_context, &reduced).is_some() {
            let (rigid_name, new_ctx) = sub_context.clone().fresh_rigid_name();
            let rigid_type = Type::Generic(rigid_name.clone(), h.clone());
            sub_context = new_ctx
                .add_interface_constraint(rigid_name, reduced)
                .push_var_type(
                    Var::from_name(&arg_typ.get_argument_str()).set_type(rigid_type.clone()),
                    rigid_type,
                    &sub_context,
                );
        } else {
            let var = arg_typ
                .clone()
                .set_type(param_type.clone())
                .to_var(&sub_context);
            sub_context = sub_context
                .clone()
                .push_var_type(var, param_type.clone(), &sub_context);
        }
    }

    // `Self:{ ... }` (generic_constructor.md §4.1): bind `Self` to whatever
    // type the first parameter ended up with in `sub_context` (the declared
    // type, or a fresh rigid generic for an interface param), so it's only
    // valid for the duration of typing this body.
    if let Some(first_param) = params.first() {
        if let Ok(first_type) =
            sub_context.get_type_from_variable(&Var::from_name(&first_param.get_argument_str()))
        {
            sub_context = sub_context.set_self_type(Some(first_type));
        }
    }

    let body_type = body.typing(&sub_context);
    let mut errors = body_type.errors.clone();
    errors.extend(kind_consistency_errors);
    errors.extend(default_param_errors);
    let is_compatible = is_opaque_of(&body_type.value, ret_ty)
        || body_type.value.reduce_and_subtype(ret_ty, &sub_context).0
        || is_rigid_compatible(&body_type.value, ret_ty, &sub_context);
    (!is_compatible)
        .then(|| errors.push(builder::unmatching_return_type(ret_ty, &body_type.value)));
    // Structural types registered on the fly while typing the body (e.g. the
    // `ArrayN` alias created by an inline `expr as! [T]` cast) must survive
    // into the outer context: the transpiler resolves them there to emit the
    // `|> as.ArrayN()` annotation and the matching `types.R` entry. Only
    // alias registrations are hoisted — body variables stay local.
    TypeContext::new(
        Type::Function(list_of_types, Box::new(ret_ty.clone()), h.clone()),
        expr.clone(),
        context.clone().hoist_aliases(&body_type.context),
    )
    .with_errors(errors)
}

#[cfg(test)]
mod tests {
    use crate::components::error_message::help_data::HelpData;
    use crate::components::r#type::argument_type::ArgumentType;
    use crate::utils::builder;
    use crate::utils::fluent_parser::FluentParser;
    use crate::Lang;
    use crate::Type;

    // =====================================================================
    // Tests for function type-checking with Interface parameters
    // =====================================================================
    //
    // When a function parameter has an Interface type, the type-checker must:
    //   1. Assign an opaque alias type to that parameter in the sub-context
    //   2. Decompose the interface's method signatures into individual
    //      function variables in the sub-context, replacing `Self` with
    //      the opaque alias
    //   3. Allow the function body to call those interface methods
    //   4. Return the correct function type (with the original Interface
    //      in the parameter list)
    //
    // This approach avoids requiring explicit generic notation to implement
    // multiple interfaces.
    // =====================================================================

    /// A function with an interface parameter that has a single method
    /// should type-check successfully when the body calls that method.
    #[test]
    fn test_function_with_interface_param_single_method() {
        let res = FluentParser::new()
            .check_typing("fn(x: interface { view: (Self) -> char }): char { view(x) }");
        let interface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let expected = Type::Function(
            vec![ArgumentType::new("x", &interface)],
            Box::new(builder::character_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    /// A function with an interface parameter containing multiple methods.
    /// The body can call any of those methods.
    #[test]
    fn test_function_with_interface_param_multiple_methods() {
        let res = FluentParser::new()
            .check_typing(
                "fn(x: interface { to_char: (Self) -> char, to_int: (Self) -> int }): char { to_char(x) }"
            );
        let interface = builder::interface_type(&[
            (
                "to_char",
                builder::function_type(
                    &[builder::self_generic_type()],
                    builder::character_type_default(),
                ),
            ),
            (
                "to_int",
                builder::function_type(
                    &[builder::self_generic_type()],
                    builder::integer_type_default(),
                ),
            ),
        ]);
        let expected = Type::Function(
            vec![ArgumentType::new("x", &interface)],
            Box::new(builder::character_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    /// A concrete type (int) that satisfies an interface can be passed
    /// as an argument to a function expecting that interface.
    #[test]
    fn test_function_app_with_interface_param_concrete_type() {
        let res = FluentParser::new()
            .push("@view: (int) -> char;")
            .run()
            .push("let f <- fn(x: interface { view: (Self) -> char }): char { view(x) };")
            .parse_type_next()
            .check_typing("f(5)");
        assert_eq!(res, builder::character_type_default());
    }

    /// When a function with interface parameter has a body whose return
    /// type does not match the declared return type, an error should be
    /// reported.
    #[test]
    fn test_function_with_interface_param_return_type_mismatch() {
        let fp = FluentParser::new()
            .push("fn(x: interface { view: (Self) -> char }): int { view(x) }")
            .parse_next()
            .type_next();
        // The body returns char (from view), but declared return is int
        // This should produce an error
        let interface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let expected = Type::Function(
            vec![ArgumentType::new("x", &interface)],
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        assert_eq!(fp.get_last_type(), expected);
    }

    /// A function with an interface parameter alongside a regular parameter.
    #[test]
    fn test_function_with_interface_and_regular_params() {
        let res = FluentParser::new()
            .check_typing("fn(x: interface { view: (Self) -> char }, y: int): char { view(x) }");
        let interface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let expected = Type::Function(
            vec![
                ArgumentType::new("x", &interface),
                ArgumentType::new("y", &builder::integer_type_default()),
            ],
            Box::new(builder::character_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    /// An interface method with multiple parameters (Self + another type).
    /// The `replace_function_types` should only replace `Self` occurrences,
    /// preserving other parameter types.
    #[test]
    fn test_function_with_interface_multi_param_method() {
        let res = FluentParser::new()
            .check_typing("fn(x: interface { add: (Self, int) -> Self }): int { 5 }");
        let interface = builder::interface_type(&[(
            "add",
            builder::function_type(
                &[
                    builder::self_generic_type(),
                    builder::integer_type_default(),
                ],
                builder::self_generic_type(),
            ),
        )]);
        let expected = Type::Function(
            vec![ArgumentType::new("x", &interface)],
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    /// Basic function without interface parameters should still work.
    /// Regression test to ensure interface handling doesn't break normal functions.
    #[test]
    fn test_function_without_interface_param() {
        let res = FluentParser::new().check_typing("fn(x: int): int { x }");
        let expected = Type::Function(
            vec![ArgumentType::new("x", &builder::integer_type_default())],
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    /// A function with an interface parameter that uses `Self` as a return type
    /// in one of its methods. The body calls the method and returns the result.
    #[test]
    fn test_function_with_interface_self_return() {
        let res = FluentParser::new()
            .check_typing("fn(x: interface { identity: (Self) -> Self }): int { 5 }");
        let interface = builder::interface_type(&[(
            "identity",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::self_generic_type(),
            ),
        )]);
        let expected = Type::Function(
            vec![ArgumentType::new("x", &interface)],
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    /// Test that an inline interface with an operator method (`+`) can be
    /// used in the function body via the operator syntax (a + a).
    #[test]
    fn test_function_with_inline_interface_operator_in_body() {
        let fp = FluentParser::new()
            .push("fn(a: interface { `+`: (Self, Self) -> Self }): int { a + a }")
            .parse_next()
            .type_next();
        let interface = builder::interface_type(&[(
            "`+`",
            builder::function_type(
                &[builder::self_generic_type(), builder::self_generic_type()],
                builder::self_generic_type(),
            ),
        )]);
        let expected = builder::function_type(&[interface], builder::integer_type_default());
        assert_eq!(fp.get_last_type(), expected);
    }

    /// Reproduction of the user bug: a named interface type alias with an
    /// operator method, used as both parameter and return type.
    /// When using a named alias like `type Addable <- interface {...}`,
    /// the function type preserves the alias name in its signature.
    /// The body `a + a` should type-check without errors.
    #[test]
    fn test_function_with_named_interface_operator() {
        let fp = FluentParser::new()
            .push("type Addable <- interface { `+`: (Self, Self) -> Self };")
            .run()
            .push("fn(a: Addable): Addable { a + a }")
            .parse_next()
            .type_next();
        // With a named alias, the function type uses Alias("Addable") not the expanded Interface
        let addable = Type::Alias("Addable".to_string(), vec![], false, HelpData::default());
        let expected = builder::function_type(&[addable.clone()], addable);
        assert_eq!(fp.get_last_type(), expected);
    }

    /// The named interface operator test should produce no type errors.
    /// This verifies the actual user-reported bug is fixed (the body
    /// `a + a` should not produce "Found: any" error).
    #[test]
    fn test_function_with_named_interface_operator_no_errors() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        // First, parse and type-check the type alias
        let code1 =
            parse2("type Addable <- interface { `+`: (Self, Self) -> Self };".into()).unwrap();
        let tc = TypeChecker::new(Context::empty()).typing_no_panic(&code1);

        // Then parse and type-check the function
        let code2 = parse2("let double <- fn(a: Addable): Addable { a + a };".into()).unwrap();
        let tc2 = tc.typing_no_panic(&code2);

        assert!(
            !tc2.has_errors(),
            "Expected no type errors, but got: {:?}",
            tc2.get_errors()
        );
    }

    /// Test: function parameter called inside match expression branch.
    /// Regression test for: function 'f' not defined in this scope when
    /// calling f(x) inside a .Some(x) => pattern match branch.
    /// The fix ensures that:
    /// 1. Match type is not reduced before pattern matching (preserves Alias types)
    /// 2. Function parameters are preserved in the returned context
    #[test]
    fn test_function_param_called_in_match_branch() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2(
            "fn(o: Option<int>, f: (int) -> int): int { match o { .Some(x) => f(x), .None => 0 } }"
                .into(),
        )
        .unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }

    /// A parameter typed `list { ... } & interface { ... }` should support
    /// both record field access (`$`) and interface method calls in the same
    /// function body — the user-reported gap where combining a record and an
    /// interface via `&` used to lose both capabilities.
    #[test]
    fn test_function_with_record_and_interface_intersection_param_field_access() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2(
            "fn(v: list { x: int } & interface { view: (Self) -> char }): int { v$x }".into(),
        )
        .unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }

    #[test]
    fn test_function_with_record_and_interface_intersection_param_method_call() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2(
            "fn(v: list { x: int } & interface { view: (Self) -> char }): char { view(v) }".into(),
        )
        .unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }

    #[test]
    fn test_function_with_record_and_interface_intersection_param_both_in_one_body() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2(
            "fn(v: list { x: int } & interface { view: (Self) -> char }): char { let n <- v$x; view(v) }"
                .into(),
        )
        .unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }

    #[test]
    fn test_empty_function_body_produces_error() {
        use crate::components::context::Context;
        use crate::components::error_message::syntax_error::SyntaxError;
        use crate::components::error_message::typr_error::TypRError;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2("fn(): Any {}".into()).unwrap();
        let tc = TypeChecker::new(Context::empty()).typing_no_panic(&code);
        assert!(
            tc.has_errors(),
            "Expected at least one error for empty function body"
        );
        let errors = tc.get_errors();
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, TypRError::Syntax(SyntaxError::EmptyFunctionBody(_)))),
            "Expected EmptyFunctionBody error, got: {:?}",
            errors
        );
    }

    #[test]
    fn test_dotdotdot_placeholder_no_error() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2("fn(): Any { ... }".into()).unwrap();
        let tc = TypeChecker::new(Context::empty()).typing_no_panic(&code);
        assert!(
            !tc.has_errors(),
            "fn(): Any {{ ... }} should not produce errors, got: {:?}",
            tc.get_errors()
        );
    }

    // ── sigils.md §7 : intra-signature kind-consistency pass ────────────────

    /// RFC §9.1 — a Number-sigil-only signature, the pre-existing `#N`
    /// (IndexGen) case, must keep working unchanged.
    #[test]
    fn test_rfc_example_zeros_number_sigil() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2("fn(n: #N): [#N, int] { [1] }".into()).unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(!tc.has_errors(), "errors: {:?}", tc.get_errors());
    }

    /// RFC §9.2 — Record + String sigils in the same signature.
    #[test]
    fn test_rfc_example_select_record_string_sigils() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2("fn(r: %R, field: ^S): ^S { field }".into()).unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(!tc.has_errors(), "errors: {:?}", tc.get_errors());
    }

    /// RFC §9.3 — Interface sigil over a vector element type.
    #[test]
    fn test_rfc_example_sort_interface_sigil() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2("fn(items: [#N, @T]): [#N, @T] { items }".into()).unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(!tc.has_errors(), "errors: {:?}", tc.get_errors());
    }

    /// RFC §7.2 — the same generic name used bare (`A`, Any-kind) and with a
    /// Number sigil (`#A`) in the same signature is a kind conflict.
    #[test]
    fn test_intra_signature_kind_conflict_bare_then_number_sigil() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2("fn(x: A): [#A, int] { [1] }".into()).unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(tc.has_errors(), "Expected a kind-conflict error");
        assert!(
            tc.get_errors().iter().any(|e| matches!(
                e,
                crate::components::error_message::typr_error::TypRError::Type(
                    crate::components::error_message::type_error::TypeError::KindMismatch(_, _, _)
                )
            )),
            "Expected KindMismatch error, got: {:?}",
            tc.get_errors()
        );
    }

    /// Same generic name `R` used once as `%R` (Record) and once as `@R`
    /// (Interface) in the same signature — a direct sigil clash.
    #[test]
    fn test_intra_signature_kind_conflict_record_vs_interface_sigil() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2("fn(a: %R, b: @R): %R { a }".into()).unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            tc.has_errors(),
            "Expected a kind-conflict error for %R vs @R"
        );
    }

    #[test]
    fn test_variadic_function_type() {
        // Inside the body the variadic param is a vector, so it can be
        // returned as `[#N, num]`.
        let res = FluentParser::new().check_typing("fn(...xs: num): [#N, num] { xs }");
        let expected = Type::Function(
            vec![ArgumentType(
                Type::Char("xs".to_string().into(), HelpData::default()),
                builder::number_type(),
                false,
                true,
                None,
            )],
            Box::new(builder::array_type(
                Type::IndexGen("N".to_string(), HelpData::default()),
                builder::number_type(),
            )),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    #[test]
    fn test_variadic_function_call_multi_args() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 = parse2("let f <- fn(...xs: num): num { sum(xs) };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::default()).typing_no_panic(&code1);
        assert!(
            !tc1.has_errors(),
            "variadic decl errors: {:?}",
            tc1.get_errors()
        );

        let code2 = parse2("f(1.0, 2.0, 3.0)".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);
        assert!(
            !tc2.has_errors(),
            "variadic call errors: {:?}",
            tc2.get_errors()
        );
    }

    #[test]
    fn test_variadic_function_call_zero_args() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 = parse2("let f <- fn(...xs: num): num { sum(xs) };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::default()).typing_no_panic(&code1);

        let code2 = parse2("f()".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);
        assert!(
            !tc2.has_errors(),
            "zero-arg variadic call errors: {:?}",
            tc2.get_errors()
        );
    }

    #[test]
    fn test_variadic_function_call_type_error() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 = parse2("let f <- fn(...xs: num): num { sum(xs) };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::default()).typing_no_panic(&code1);

        let code2 = parse2("f(\"hello\", \"world\")".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);
        assert!(tc2.has_errors(), "expected type error for wrong arg type");
    }

    #[test]
    fn test_variadic_with_fixed_params() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 =
            parse2("let concat <- fn(sep: char, ...args: char): [#N, char] { args };".into())
                .unwrap();
        let tc1 = TypeChecker::new(Context::default()).typing_no_panic(&code1);
        assert!(
            !tc1.has_errors(),
            "variadic with fixed param errors: {:?}",
            tc1.get_errors()
        );

        let code2 = parse2("concat(\"-\", \"a\", \"b\", \"c\")".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);
        assert!(
            !tc2.has_errors(),
            "call with fixed+variadic errors: {:?}",
            tc2.get_errors()
        );
    }

    #[test]
    fn test_default_param_parses_with_default_attached() {
        let res = FluentParser::new().check_typing("fn(a: int, b: int = 10): int { a + b }");
        let expected = Type::Function(
            vec![
                ArgumentType(
                    Type::Char("a".to_string().into(), HelpData::default()),
                    builder::integer_type_default(),
                    false,
                    false,
                    None,
                ),
                ArgumentType(
                    Type::Char("b".to_string().into(), HelpData::default()),
                    builder::integer_type_default(),
                    false,
                    false,
                    Some(Box::new(Lang::Integer {
                        value: 10,
                        help_data: HelpData::default(),
                    })),
                ),
            ],
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    #[test]
    fn test_default_param_call_with_and_without_optional_arg() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 = parse2("let f <- fn(a: int, b: int = 10): int { a + b };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::default()).typing_no_panic(&code1);
        assert!(!tc1.has_errors(), "decl errors: {:?}", tc1.get_errors());

        let code2 = parse2("f(1)".into()).unwrap();
        let tc2 = tc1.clone().typing_no_panic(&code2);
        assert!(
            !tc2.has_errors(),
            "call with omitted default arg errors: {:?}",
            tc2.get_errors()
        );

        let code3 = parse2("f(1, 2)".into()).unwrap();
        let tc3 = tc1.typing_no_panic(&code3);
        assert!(
            !tc3.has_errors(),
            "call with all args errors: {:?}",
            tc3.get_errors()
        );
    }

    #[test]
    fn test_default_param_call_with_zero_args_when_all_defaulted() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 = parse2("let f <- fn(a: int = 1, b: int = 2): int { a + b };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::default()).typing_no_panic(&code1);
        assert!(!tc1.has_errors(), "decl errors: {:?}", tc1.get_errors());

        let code2 = parse2("f()".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);
        assert!(
            !tc2.has_errors(),
            "zero-arg call errors: {:?}",
            tc2.get_errors()
        );
    }

    #[test]
    fn test_missing_required_arg_without_default_is_an_error() {
        // Regression test: a call with too few arguments must be rejected
        // when the missing trailing parameter has no default — without
        // `FunctionType::infer_return_type_direct`'s arity guard, this used
        // to silently succeed (the unification zip ignored the unmatched
        // trailing parameter), producing R that crashes at runtime with
        // "argument b is missing, with no default".
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 = parse2("let f <- fn(a: int, b: int): int { a + b };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::default()).typing_no_panic(&code1);
        assert!(!tc1.has_errors(), "decl errors: {:?}", tc1.get_errors());

        let code2 = parse2("f(1)".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);
        assert!(
            tc2.has_errors(),
            "expected FunctionNotFound for too-few-args call with no default"
        );
    }

    #[test]
    fn test_non_trailing_default_param_is_an_error() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code = parse2("fn(a: int = 1, b: int): int { a + b }".into()).unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            tc.has_errors(),
            "expected NonTrailingDefaultParam for a required param after a defaulted one"
        );
    }

    #[test]
    fn test_default_value_type_mismatch_is_an_error() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code = parse2("fn(a: int = \"x\"): int { a }".into()).unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            tc.has_errors(),
            "expected a Param type mismatch for a default value of the wrong type"
        );
    }

    #[test]
    fn test_default_referencing_sibling_param_is_an_error() {
        // Defaults are closed expressions, type-checked against the
        // function's enclosing context — not a scope where sibling
        // parameters are bound — so referencing another parameter surfaces
        // as the ordinary UndefinedVariable error.
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code = parse2("fn(a: int, b: int = a): int { a + b }".into()).unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            tc.has_errors(),
            "expected UndefinedVariable for a default referencing a sibling parameter"
        );
    }

    #[test]
    fn test_record_constructor_matches_type_alias() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 =
            parse2("type Character <- list { name: char, attack: int, health: int };".into())
                .unwrap();
        let tc1 = TypeChecker::new(Context::default()).typing_no_panic(&code1);

        let code2 = parse2(
            "let new_character <- fn(name: char, attack: int, health: int): Character { list(name=name, attack=attack, health=health) };".into()
        ).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);
        assert!(
            !tc2.has_errors(),
            "Expected no errors, got: {:?}",
            tc2.get_errors()
        );
    }

    // =====================================================================
    // Tests for interface-return-only error (existential position)
    // =====================================================================

    /// fn(): Incrementable — interface only in return, no params at all.
    /// Must produce an InterfaceReturnOnly error.
    #[test]
    fn test_interface_return_only_no_params() {
        use crate::components::context::Context;
        use crate::components::error_message::type_error::TypeError;
        use crate::components::error_message::typr_error::TypRError;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 =
            parse2("type Incrementable <- interface { incr: (Self) -> Self };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::empty()).typing_no_panic(&code1);

        let code2 = parse2("fn(): Incrementable { ... }".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);

        assert!(tc2.has_errors(), "Expected InterfaceReturnOnly error");
        assert!(
            tc2.get_errors()
                .iter()
                .any(|e| matches!(e, TypRError::Type(TypeError::InterfaceReturnOnly(_)))),
            "Expected InterfaceReturnOnly, got: {:?}",
            tc2.get_errors()
        );
    }

    /// fn(x: int): Incrementable — interface only in return, param is concrete.
    /// Must produce an InterfaceReturnOnly error.
    #[test]
    fn test_interface_return_only_concrete_param() {
        use crate::components::context::Context;
        use crate::components::error_message::type_error::TypeError;
        use crate::components::error_message::typr_error::TypRError;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 =
            parse2("type Incrementable <- interface { incr: (Self) -> Self };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::empty()).typing_no_panic(&code1);

        let code2 = parse2("fn(x: int): Incrementable { ... }".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);

        assert!(tc2.has_errors(), "Expected InterfaceReturnOnly error");
        assert!(
            tc2.get_errors()
                .iter()
                .any(|e| matches!(e, TypRError::Type(TypeError::InterfaceReturnOnly(_)))),
            "Expected InterfaceReturnOnly, got: {:?}",
            tc2.get_errors()
        );
    }

    /// fn(i: Incrementable): Incrementable — interface in both param and return.
    /// Must NOT produce an InterfaceReturnOnly error.
    #[test]
    fn test_interface_param_and_return_no_error() {
        use crate::components::context::Context;
        use crate::components::error_message::type_error::TypeError;
        use crate::components::error_message::typr_error::TypRError;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 =
            parse2("type Incrementable <- interface { incr: (Self) -> Self };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::empty()).typing_no_panic(&code1);

        let code2 = parse2("fn(i: Incrementable): Incrementable { i.incr() }".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);

        assert!(
            !tc2.get_errors()
                .iter()
                .any(|e| matches!(e, TypRError::Type(TypeError::InterfaceReturnOnly(_)))),
            "Should not have InterfaceReturnOnly error, got: {:?}",
            tc2.get_errors()
        );
    }

    /// fn(items: [Incrementable]): Incrementable — interface nested inside an
    /// array parameter, returned directly. Interfaces are treated like
    /// generics: the array anchors the interface just as a bare parameter
    /// would, so this must NOT produce an InterfaceReturnOnly error.
    #[test]
    fn test_interface_anchored_inside_array_param_no_error() {
        use crate::components::context::Context;
        use crate::components::error_message::type_error::TypeError;
        use crate::components::error_message::typr_error::TypRError;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        let code1 =
            parse2("type Incrementable <- interface { incr: (Self) -> Self };".into()).unwrap();
        let tc1 = TypeChecker::new(Context::empty()).typing_no_panic(&code1);

        let code2 =
            parse2("fn(items: [Incrementable]): Incrementable { items[0] }".into()).unwrap();
        let tc2 = tc1.typing_no_panic(&code2);

        assert!(
            !tc2.get_errors()
                .iter()
                .any(|e| matches!(e, TypRError::Type(TypeError::InterfaceReturnOnly(_)))),
            "Should not have InterfaceReturnOnly error, got: {:?}",
            tc2.get_errors()
        );
    }
}
