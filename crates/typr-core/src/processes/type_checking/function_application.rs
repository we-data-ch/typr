#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::set_related_type_if_variable;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::type_system::TypeSystem;
use crate::processes::type_checking::facets;
use crate::processes::type_checking::interface_satisfaction;
use crate::processes::type_checking::match_types_to_generic;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::processes::type_checking::typing;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::Type;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::TypeError;
use crate::processes::type_checking::Var;
use crate::processes::type_checking::VecType;
use crate::utils::builder;

/// Try to resolve a method call on a constrained rigid generic variable.
/// §5 elimination rule: when the receiver has type `A` with constraint `A: I`,
/// and `m: (Self, …) -> T` is a method of `I`, then `e.m(x₁, …, xⱼ) : T[Self ↦ A]`.
fn try_constrained_variable_match(var: &Var, types: &[Type], context: &Context) -> Option<FunctionType> {
    let first_type = types.first()?;
    if !matches!(first_type, Type::Generic(_, _)) {
        return None;
    }
    // `interface_facet` looks through the rigid generic to its
    // `interface_constraint` bound, which may be a plain interface or a
    // `List & Interface` intersection — either way this recovers the method
    // set, so a mixed-intersection parameter can still dispatch method calls.
    let methods = facets::interface_facet(context, first_type)?;
    let method = methods.iter().find(|m| m.get_argument_str() == var.get_name())?;
    let method_type = method.get_type(); // e.g. Function([Self], Self) or Function([Self, Self], Self)

    // Substitute Self → receiver type in the method signature
    let substituted = method_type
        .clone()
        .replace_function_types(builder::self_generic_type(), first_type.clone());

    // Convert to FunctionType and infer return type
    let mut fun_typ = FunctionType::try_from(substituted).ok()?;
    let param_types = fun_typ.get_param_types();
    if param_types.len() != types.len() {
        return None;
    }

    // Verify all arguments match their parameters
    let all_match = param_types.iter().zip(types.iter()).all(|(param, arg)| {
        if let Type::Generic(_, _) = param {
            // Substituted Self or other generic — accept matching types
            arg.is_subtype_raw(param, context)
        } else {
            arg.is_subtype_raw(param, context)
        }
    });
    if !all_match {
        return None;
    }

    let return_type = fun_typ.get_return_type();
    fun_typ = fun_typ.set_infered_return_type(return_type);
    Some(fun_typ)
}

fn build_success(
    var: &Var,
    fun_typ: &FunctionType,
    expanded_parameters: Vec<Lang>,
    arg_types: &[Type],
    param_errors: Vec<TypRError>,
    context: &Context,
    h: &HelpData,
) -> TypeContext {
    let updated_parameters: Vec<Lang> = expanded_parameters
        .iter()
        .zip(arg_types.iter())
        .map(set_related_type_if_variable)
        .collect();
    let new_expr = build_function_lang(h, updated_parameters, fun_typ, var.clone().to_language());
    let return_type = fun_typ.get_infered_return_type().set_help_data(h.clone());
    TypeContext::new(return_type, new_expr, context.clone()).with_errors(param_errors)
}

fn filter_by_first_param(signatures: &[FunctionType], first_arg_type: &Type, context: &Context) -> Vec<FunctionType> {
    let reduced_arg = reduce_type(context, first_arg_type);
    // Reduce each candidate's first param exactly once — this used to be
    // recomputed a second time for every signature that fell through to the
    // `unifiable` filter below.
    let candidates: Vec<(&FunctionType, Type, Type)> = signatures
        .iter()
        .filter_map(|sig| {
            sig.get_first_param().map(|p| {
                let reduced_p = reduce_type(context, &p);
                (sig, p, reduced_p)
            })
        })
        .collect();
    let (exact, rest): (Vec<_>, Vec<_>) = candidates.into_iter().partition(|(_, _, reduced_p)| {
        *reduced_p == reduced_arg
            || matches!(
                reduced_p,
                Type::Generic(_, _) | Type::IndexGen(_, _) | Type::LabelGen(_, _) | Type::KindedGen(_, _, _)
            )
    });
    // Composite generic params (e.g. `[#N, T]` against an argument `[#N, num]`)
    // don't pass the equality test above but can still unify. They are appended
    // after the exact matches so they don't steal overload resolution.
    // Also include signatures where the arg is a primitive subtype of the param
    // (e.g. Char(Val("A")) <: Char(Unknown)), excluding Any so that Any-param
    // functions still fall through to FILTERING 3 vectorization as before.
    let unifiable = rest.into_iter().filter(|(_, p, reduced_p)| {
        // Exclude Any (belongs to FILTERING 3) and anything with an
        // interface facet — plain `Interface` or a `Record &
        // Interface` intersection (belongs to FILTERING 2.5, which
        // uses the *unreduced* arg type so `to_interface`'s exact
        // first-param-type match still sees the alias name) — from
        // the is_subtype_raw fast path here, which only has the
        // already-reduced (alias-stripped) `reduced_arg` to work with.
        (!matches!(reduced_p, Type::Any(_))
            && facets::interface_facet(context, reduced_p).is_none()
            && reduced_arg.is_subtype_raw(reduced_p, context))
            || match_types_to_generic(context, &reduced_arg, p)
                .map(|bindings| !bindings.is_empty())
                .unwrap_or(false)
    });
    exact
        .into_iter()
        .chain(unifiable)
        .map(|(sig, _, _)| sig.clone())
        .collect()
}

fn try_direct_match(candidates: &[FunctionType], types: &[Type], context: &Context) -> Option<FunctionType> {
    candidates
        .iter()
        .flat_map(|x| x.clone().infer_return_type_direct(types, context))
        .next()
}

fn try_vectorized_match(
    candidates: &[FunctionType],
    types: &[Type],
    _var: &Var,
    context: &Context,
) -> Option<FunctionType> {
    candidates
        .iter()
        .flat_map(|x| x.clone().infer_return_type_vectorized(types, context))
        .next()
}

fn try_variadic_match(
    all_signatures: &[FunctionType],
    types: &[Type],
    _var: &Var,
    context: &Context,
) -> Option<FunctionType> {
    all_signatures
        .iter()
        .filter(|sig| sig.is_variadic())
        .flat_map(|x| x.clone().infer_return_type(types, context))
        .next()
}

/// Matches a call supplying fewer arguments than declared, when the missing
/// trailing parameters all carry a default value.
fn try_default_match(
    all_signatures: &[FunctionType],
    types: &[Type],
    _var: &Var,
    context: &Context,
) -> Option<FunctionType> {
    all_signatures
        .iter()
        .filter(|sig| sig.has_defaults() && !sig.is_variadic())
        .flat_map(|x| x.clone().infer_return_type_partial(types, context))
        .next()
}

// ── Tuple-spread unification (FILTERING 5) ──────────────────────────────────
//
// The SafeHashMap used by the normal generic-inference path treats every
// Type::Generic as equal (by design), so two distinct generic variables T and U
// cannot both be bound at the same time.  Spread signatures like
//   @append: (tuple{T...}, U) -> tuple{T..., U};
// need TWO distinct bindings (T→sequence, U→scalar).  We bypass SafeHashMap
// entirely and use a String-keyed map instead.

enum SpreadSub {
    Scalar(Type),
    Seq(Vec<Type>),
}

fn has_tuple_spread(param_types: &[Type]) -> bool {
    param_types.iter().any(|p| {
        matches!(p, Type::Tuple(elems, _) if elems.iter().any(|e| {
            matches!(e, Type::Multi(inner, _) if matches!(inner.as_ref(), Type::Generic(_, _)))
        }))
    })
}

fn collect_tuple_spread(
    subs: &mut std::collections::HashMap<String, SpreadSub>,
    concrete: &[Type],
    params: &[Type],
) -> Option<()> {
    let spread_pos = params
        .iter()
        .position(|p| matches!(p, Type::Multi(inner, _) if matches!(inner.as_ref(), Type::Generic(_, _))));

    match spread_pos {
        None => {
            if concrete.len() != params.len() {
                return None;
            }
            for (c, p) in concrete.iter().zip(params.iter()) {
                if let Type::Generic(name, _) = p {
                    subs.insert(name.clone(), SpreadSub::Scalar(c.clone()));
                }
            }
            Some(())
        }
        Some(pos) => {
            let suffix_len = params.len() - pos - 1;
            if concrete.len() < pos + suffix_len {
                return None;
            }
            let spread_end = concrete.len() - suffix_len;
            for (c, p) in concrete[..pos].iter().zip(params[..pos].iter()) {
                if let Type::Generic(name, _) = p {
                    subs.insert(name.clone(), SpreadSub::Scalar(c.clone()));
                }
            }
            if let Type::Multi(inner, _) = &params[pos] {
                if let Type::Generic(name, _) = inner.as_ref() {
                    subs.insert(name.clone(), SpreadSub::Seq(concrete[pos..spread_end].to_vec()));
                }
            }
            for (c, p) in concrete[spread_end..].iter().zip(params[pos + 1..].iter()) {
                if let Type::Generic(name, _) = p {
                    subs.insert(name.clone(), SpreadSub::Scalar(c.clone()));
                }
            }
            Some(())
        }
    }
}

fn collect_spread_subs(
    arg_types: &[Type],
    param_types: &[Type],
) -> Option<std::collections::HashMap<String, SpreadSub>> {
    let mut subs = std::collections::HashMap::new();
    for (arg, param) in arg_types.iter().zip(param_types.iter()) {
        match (arg, param) {
            (Type::Tuple(concrete, _), Type::Tuple(params, _)) => {
                collect_tuple_spread(&mut subs, concrete, params)?;
            }
            (arg, Type::Generic(name, _)) => {
                subs.insert(name.clone(), SpreadSub::Scalar(arg.clone()));
            }
            _ => {}
        }
    }
    Some(subs)
}

fn apply_spread_subs(ret: &Type, subs: &std::collections::HashMap<String, SpreadSub>) -> Type {
    match ret {
        Type::Tuple(elems, h) => {
            let mut new_elems: Vec<Type> = vec![];
            for elem in elems {
                match elem {
                    Type::Multi(inner, _) => {
                        if let Type::Generic(name, _) = inner.as_ref() {
                            match subs.get(name) {
                                Some(SpreadSub::Seq(seq)) => {
                                    new_elems.extend(seq.clone());
                                    continue;
                                }
                                Some(SpreadSub::Scalar(t)) => {
                                    new_elems.push(t.clone());
                                    continue;
                                }
                                None => {}
                            }
                        }
                        new_elems.push(apply_spread_subs(elem, subs));
                    }
                    Type::Generic(name, _) => match subs.get(name) {
                        Some(SpreadSub::Scalar(t)) => new_elems.push(t.clone()),
                        Some(SpreadSub::Seq(seq)) => new_elems.extend(seq.clone()),
                        None => new_elems.push(elem.clone()),
                    },
                    _ => new_elems.push(apply_spread_subs(elem, subs)),
                }
            }
            Type::Tuple(new_elems, h.clone())
        }
        Type::Generic(name, _) => match subs.get(name) {
            Some(SpreadSub::Scalar(t)) => t.clone(),
            _ => ret.clone(),
        },
        _ => ret.clone(),
    }
}

fn try_spread_tuple_match(
    all_signatures: &[FunctionType],
    types: &[Type],
    _var: &Var,
    context: &Context,
) -> Option<FunctionType> {
    for sig in all_signatures {
        let param_types = sig.get_param_types();
        if param_types.len() != types.len() {
            continue;
        }
        if !has_tuple_spread(&param_types) {
            continue;
        }
        if let Some(subs) = collect_spread_subs(types, &param_types) {
            let new_return = apply_spread_subs(&sig.get_return_type(), &subs);
            return Some(sig.clone().set_infered_return_type(new_return));
        }
    }
    None
}

// ── Named multi-generic unification (FILTERING 5.5) ─────────────────────────
//
// The standard unification path (`SafeHashMap` / `merge_substitutions`) treats
// every `Type::Generic` as identical regardless of its name.  That is fine for
// single-variable signatures but breaks signatures like
//   @fold: ([#N, T], U, (U, T) -> U) -> U
// where T and U are two *distinct* generic variables — the binding for U gets
// overwritten by the binding for T.
//
// This alternative path builds a `String`-keyed map so that T and U are kept
// separate.  It is tried *after* the standard paths fail.

fn collect_named_generics(concrete: &Type, param: &Type, subs: &mut std::collections::HashMap<String, Type>) {
    // Any is the error sentinel (lambda body failed to type with abstract params).
    // Don't let it bind generics — it would produce spurious [any, any] returns.
    if matches!(concrete, Type::Any(_)) {
        return;
    }
    match param {
        Type::Generic(name, _) => {
            if let Some(existing) = subs.get(name).cloned() {
                // Param generic already bound. If arg is a fresh generic, propagate.
                if let Type::Generic(arg_name, _) = concrete {
                    if !matches!(existing, Type::Generic(_, _)) {
                        subs.entry(arg_name.clone()).or_insert(existing);
                    }
                }
            } else {
                subs.insert(name.clone(), concrete.clone());
            }
        }
        Type::IndexGen(name, _) => {
            subs.entry(name.clone()).or_insert_with(|| concrete.clone());
        }
        Type::Vec(_, size_param, elem_param, _) => {
            if let Type::Vec(_, size_concrete, elem_concrete, _) = concrete {
                collect_named_generics(size_concrete, size_param, subs);
                collect_named_generics(elem_concrete, elem_param, subs);
            }
        }
        Type::Function(params_p, ret_p, _) => {
            if let Type::Function(params_c, ret_c, _) = concrete {
                for (p_c, p_p) in params_c.iter().zip(params_p.iter()) {
                    collect_named_generics(&p_c.get_type(), &p_p.get_type(), subs);
                }
                collect_named_generics(ret_c, ret_p, subs);
            }
        }
        _ => {}
    }
}

fn verify_named_generics(
    concrete: &Type,
    param: &Type,
    subs: &std::collections::HashMap<String, Type>,
    context: &Context,
) -> bool {
    // Resolve lambda fresh-generic params (T0, T1, …) that were bound during
    // collection, so that e.g. `T0 vs U(=StoryBoard)` becomes
    // `StoryBoard vs U(=StoryBoard)` and passes the subtype check.
    // resolve_named_generic_chain handles cycles via its `seen` set.
    let resolved_storage;
    let concrete = if let Type::Generic(cname, _) = concrete {
        let mut seen = std::collections::HashSet::new();
        resolved_storage = resolve_named_generic_chain(cname, subs, &mut seen);
        &resolved_storage
    } else {
        concrete
    };
    // Any is the error sentinel (lambda body failed to type with abstract generics).
    // Treat it as compatible so the named-generic match can proceed; the lambda
    // will be re-typed with concrete types during specialize_lambdas.
    if matches!(concrete, Type::Any(_)) {
        return true;
    }
    match param {
        Type::Generic(name, _) => match subs.get(name) {
            Some(expected) => concrete.is_subtype_raw(expected, context),
            None => true,
        },
        Type::IndexGen(_, _) => true,
        Type::Vec(_, _, elem_param, _) => {
            if let Type::Vec(_, _, elem_concrete, _) = concrete {
                verify_named_generics(elem_concrete, elem_param, subs, context)
            } else {
                false
            }
        }
        Type::Function(params_p, ret_p, _) => {
            if let Type::Function(params_c, ret_c, _) = concrete {
                params_c
                    .iter()
                    .zip(params_p.iter())
                    .all(|(pc, pp)| verify_named_generics(&pc.get_type(), &pp.get_type(), subs, context))
                    && verify_named_generics(ret_c, ret_p, subs, context)
            } else {
                false
            }
        }
        concrete_param => concrete.is_subtype_raw(concrete_param, context),
    }
}

fn resolve_named_generic_chain(
    name: &str,
    subs: &std::collections::HashMap<String, Type>,
    seen: &mut std::collections::HashSet<String>,
) -> Type {
    if !seen.insert(name.to_string()) {
        return Type::Generic(name.to_string(), HelpData::default());
    }
    match subs.get(name) {
        Some(Type::Generic(next_name, _)) => resolve_named_generic_chain(next_name, subs, seen),
        Some(other) => other.clone(),
        None => Type::Generic(name.to_string(), HelpData::default()),
    }
}

fn apply_named_generics(ty: &Type, subs: &std::collections::HashMap<String, Type>) -> Type {
    match ty {
        Type::Generic(name, _) => {
            let mut seen = std::collections::HashSet::new();
            resolve_named_generic_chain(name, subs, &mut seen)
        }
        Type::IndexGen(name, _) => subs.get(name).cloned().unwrap_or_else(|| ty.clone()),
        Type::Vec(vt, size, elem, h) => Type::Vec(
            vt.clone(),
            Box::new(apply_named_generics(size, subs)),
            Box::new(apply_named_generics(elem, subs)),
            h.clone(),
        ),
        Type::Function(params, ret, h) => Type::Function(
            params
                .iter()
                .map(|p| ArgumentType::new(&p.get_argument_str(), &apply_named_generics(&p.get_type(), subs)))
                .collect(),
            Box::new(apply_named_generics(ret, subs)),
            h.clone(),
        ),
        Type::Alias(name, params, opacity, h) => Type::Alias(
            name.clone(),
            params.iter().map(|p| apply_named_generics(p, subs)).collect(),
            *opacity,
            h.clone(),
        ),
        _ => ty.clone(),
    }
}

fn try_named_generic_match(all_signatures: &[FunctionType], types: &[Type], context: &Context) -> Option<FunctionType> {
    for sig in all_signatures {
        let param_types = sig.get_param_types();
        if param_types.len() != types.len() {
            continue;
        }
        let mut subs = std::collections::HashMap::new();
        for (arg, param) in types.iter().zip(param_types.iter()) {
            collect_named_generics(arg, param, &mut subs);
        }
        if subs.is_empty() {
            continue;
        }
        // Verify all args are consistent with the collected bindings.
        let valid = types
            .iter()
            .zip(param_types.iter())
            .all(|(arg, param)| verify_named_generics(arg, param, &subs, context));
        if !valid {
            continue;
        }
        let new_return = apply_named_generics(&sig.get_return_type(), &subs);
        if !matches!(new_return, Type::Generic(_, _)) {
            // Also apply subs to argument types so that specialize_lambdas can
            // see the concrete expected type for each lambda parameter (e.g. T
            // resolved to `int` makes `(T) -> U` become `(int) -> U`, letting
            // specialize_lambda substitute the lambda's fresh generic T0 = int
            // and re-type the body with the correct concrete type).
            let new_args: Vec<Type> = sig
                .get_param_types()
                .iter()
                .map(|arg| apply_named_generics(arg, &subs))
                .collect();
            return Some(sig.clone().set_params(new_args).set_infered_return_type(new_return));
        }
    }
    None
}

/// Interface subtyping match with universal generic return-type inference.
///
/// For each candidate, checks that every argument satisfies its corresponding
/// parameter (using is_subtype_raw for interface params, subtype check otherwise).
/// When the return type reduces to the same interface as one of the matched
/// parameters, it is replaced by the concrete argument type for that parameter.
/// This implements the desugaring: fn(i: I): I  =>  forall A: I. A -> A.
fn try_interface_subtype_match(
    candidates: &[FunctionType],
    arg_types: &[Type],
    context: &Context,
) -> Option<FunctionType> {
    for sig in candidates {
        let param_types = sig.get_param_types();
        if param_types.len() != arg_types.len() {
            continue;
        }

        // Collect interface-param → concrete-arg mappings while verifying all params match.
        let mut interface_to_concrete: Vec<(Type, Type)> = Vec::new();
        let all_match = param_types.iter().zip(arg_types.iter()).all(|(param, arg)| {
            let reduced_param = reduce_type(context, param);
            if facets::interface_facet(context, &reduced_param).is_some() {
                if arg.is_subtype_raw(&reduced_param, context) {
                    if !interface_to_concrete.iter().any(|(k, _)| k == &reduced_param) {
                        interface_to_concrete.push((reduced_param, arg.clone()));
                    }
                    true
                } else {
                    false
                }
            } else {
                arg.is_subtype_raw(&reduced_param, context)
            }
        });

        if !all_match {
            continue;
        }

        // If the return type reduces to one of the matched interfaces, substitute it
        // with the corresponding concrete argument type.
        let ret_type = sig.get_return_type();
        let reduced_ret = reduce_type(context, &ret_type);
        let inferred_ret = interface_to_concrete
            .iter()
            .find(|(iface, _)| *iface == reduced_ret)
            .map(|(_, concrete)| concrete.clone())
            .unwrap_or(ret_type);

        return Some(sig.clone().set_infered_return_type(inferred_ret));
    }
    None
}

fn get_generic_params_from_type(typ: &Type) -> Vec<(String, Type)> {
    match typ {
        Type::Function(params, ret, _) => {
            let mut result = Vec::new();
            for (i, param) in params.iter().enumerate() {
                if let Type::Generic(name, _) = &param.get_type() {
                    result.push((name.clone(), param.get_type()));
                }
            }
            if let Type::Generic(name, _) = ret.as_ref() {
                result.push((name.clone(), ret.as_ref().clone()));
            }
            result
        }
        _ => Vec::new(),
    }
}

fn build_substitution_map(lambda_types: &[(Type, Type)]) -> std::collections::HashMap<String, Type> {
    let mut subs = std::collections::HashMap::new();
    for (fresh_type, concrete_type) in lambda_types {
        if let Type::Generic(name, _) = fresh_type {
            if !matches!(concrete_type, Type::Generic(_, _)) {
                subs.insert(name.clone(), concrete_type.clone());
            }
        }
    }
    subs
}

fn substitute_type(typ: &Type, subs: &std::collections::HashMap<String, Type>) -> Type {
    match typ {
        Type::Generic(name, h) => subs.get(name).cloned().unwrap_or_else(|| typ.clone()),
        Type::Function(params, ret, h) => Type::Function(
            params
                .iter()
                .map(|arg| {
                    let new_type = substitute_type(&arg.get_type(), subs);
                    ArgumentType::new(&arg.get_argument_str(), &new_type)
                })
                .collect(),
            Box::new(substitute_type(ret, subs)),
            h.clone(),
        ),
        _ => typ.clone(),
    }
}

fn substitute_type_in_lang(lang: &Lang, subs: &std::collections::HashMap<String, Type>) -> Lang {
    match lang {
        Lang::Variable {
            name,
            is_opaque: mutable_,
            help_data: h,
            ..
        } => {
            if let Some(related_type) = subs.get(name) {
                Lang::Variable {
                    name: name.clone(),
                    is_opaque: *mutable_,
                    related_type: related_type.clone(),
                    help_data: h.clone(),
                }
            } else {
                lang.clone()
            }
        }
        Lang::Lambda {
            parameters: params,
            body,
            help_data: h,
        } => {
            let new_params: Vec<Lang> = params
                .iter()
                .map(|p: &Lang| {
                    if let Lang::Variable {
                        name,
                        is_opaque: mutable_,
                        help_data: h2,
                        ..
                    } = p
                    {
                        if let Some(related_type) = subs.get(name.as_str()) {
                            Lang::Variable {
                                name: name.clone(),
                                is_opaque: *mutable_,
                                related_type: related_type.clone(),
                                help_data: h2.clone(),
                            }
                        } else {
                            p.clone()
                        }
                    } else {
                        p.clone()
                    }
                })
                .collect();
            let new_body = substitute_type_in_lang(body, subs);
            Lang::Lambda {
                parameters: new_params,
                body: Box::new(new_body),
                help_data: h.clone(),
            }
        }
        Lang::Function {
            parameters: args,
            return_type: ret,
            body,
            help_data: h,
        } => {
            let new_ret = substitute_type(ret, subs);
            let new_body = substitute_type_in_lang(body, subs);
            Lang::Function {
                parameters: args.clone(),
                return_type: new_ret,
                body: Box::new(new_body),
                help_data: h.clone(),
            }
        }
        Lang::FunctionApp {
            identifier: func,
            arguments: args,
            help_data: h,
        } => Lang::FunctionApp {
            identifier: Box::new(substitute_type_in_lang(func, subs)),
            arguments: args.iter().map(|a| substitute_type_in_lang(a, subs)).collect(),
            help_data: h.clone(),
        },
        Lang::VecFunctionApp {
            vector_type: vec_type,
            identifier: func,
            arguments: args,
            help_data: h,
        } => Lang::VecFunctionApp {
            vector_type: vec_type.clone(),
            identifier: Box::new(substitute_type_in_lang(func, subs)),
            arguments: args.iter().map(|a| substitute_type_in_lang(a, subs)).collect(),
            help_data: h.clone(),
        },
        Lang::Operator {
            operator: op,
            rhs: e1,
            lhs: e2,
            help_data: h,
        } => Lang::Operator {
            operator: op.clone(),
            rhs: Box::new(substitute_type_in_lang(e1, subs)),
            lhs: Box::new(substitute_type_in_lang(e2, subs)),
            help_data: h.clone(),
        },
        Lang::Return {
            value: exp,
            help_data: h,
        } => Lang::Return {
            value: Box::new(substitute_type_in_lang(exp, subs)),
            help_data: h.clone(),
        },
        Lang::Let {
            variable: name,
            r#type: typ,
            expression: val,
            is_public,
            is_testable,
            is_export,
            help_data: h,
        } => Lang::Let {
            variable: name.clone(),
            r#type: substitute_type(typ, subs),
            expression: Box::new(substitute_type_in_lang(val, subs)),
            is_public: *is_public,
            is_testable: *is_testable,
            is_export: *is_export,
            help_data: h.clone(),
        },
        Lang::If {
            condition: cond,
            if_block: then,
            else_block: else_,
            help_data: h,
        } => Lang::If {
            condition: Box::new(substitute_type_in_lang(cond, subs)),
            if_block: Box::new(substitute_type_in_lang(then, subs)),
            else_block: Box::new(substitute_type_in_lang(else_, subs)),
            help_data: h.clone(),
        },
        Lang::Not {
            value: exp,
            help_data: h,
        } => Lang::Not {
            value: Box::new(substitute_type_in_lang(exp, subs)),
            help_data: h.clone(),
        },
        Lang::Scope {
            body: exprs,
            help_data: h,
        } => Lang::Scope {
            body: exprs.iter().map(|e| substitute_type_in_lang(e, subs)).collect(),
            help_data: h.clone(),
        },
        Lang::Lines {
            value: exprs,
            help_data: h,
        } => Lang::Lines {
            value: exprs.iter().map(|e| substitute_type_in_lang(e, subs)).collect(),
            help_data: h.clone(),
        },
        Lang::Assign {
            identifier: target,
            expression: value,
            help_data: h,
        } => Lang::Assign {
            identifier: Box::new(substitute_type_in_lang(target, subs)),
            expression: Box::new(substitute_type_in_lang(value, subs)),
            help_data: h.clone(),
        },
        Lang::ForLoop {
            identifier: var,
            expression: iter,
            body,
            help_data: h,
        } => Lang::ForLoop {
            identifier: var.clone(),
            expression: Box::new(substitute_type_in_lang(iter, subs)),
            body: Box::new(substitute_type_in_lang(body, subs)),
            help_data: h.clone(),
        },
        Lang::WhileLoop {
            condition: cond,
            body,
            help_data: h,
        } => Lang::WhileLoop {
            condition: Box::new(substitute_type_in_lang(cond, subs)),
            body: Box::new(substitute_type_in_lang(body, subs)),
            help_data: h.clone(),
        },
        Lang::Loop { body, help_data: h } => Lang::Loop {
            body: Box::new(substitute_type_in_lang(body, subs)),
            help_data: h.clone(),
        },
        Lang::Match {
            target: exp,
            branches,
            help_data: h,
        } => Lang::Match {
            target: Box::new(substitute_type_in_lang(exp, subs)),
            branches: branches
                .iter()
                .map(|(pat, exp): &(Lang, Box<Lang>)| (pat.clone(), Box::new(substitute_type_in_lang(exp, subs))))
                .collect(),
            help_data: h.clone(),
        },
        Lang::Array {
            value: elems,
            help_data: h,
        } => Lang::Array {
            value: elems.iter().map(|e| substitute_type_in_lang(e, subs)).collect(),
            help_data: h.clone(),
        },
        Lang::Vector {
            value: elems,
            help_data: h,
        } => Lang::Vector {
            value: elems.iter().map(|e| substitute_type_in_lang(e, subs)).collect(),
            help_data: h.clone(),
        },
        Lang::Tuple {
            value: elems,
            help_data: h,
        } => Lang::Tuple {
            value: elems.iter().map(|e| substitute_type_in_lang(e, subs)).collect(),
            help_data: h.clone(),
        },
        Lang::ArrayIndexing {
            identifier: arr,
            indexing: idx,
            help_data: h,
        } => Lang::ArrayIndexing {
            identifier: Box::new(substitute_type_in_lang(arr, subs)),
            indexing: Box::new(substitute_type_in_lang(idx, subs)),
            help_data: h.clone(),
        },
        Lang::Tag {
            name,
            value: inner,
            help_data: h,
        } => Lang::Tag {
            name: name.clone(),
            value: Box::new(substitute_type_in_lang(inner, subs)),
            help_data: h.clone(),
        },
        Lang::List {
            value: fields,
            spreads,
            help_data: h,
        } => Lang::List {
            value: fields.clone(),
            spreads: spreads.iter().map(|e| substitute_type_in_lang(e, subs)).collect(),
            help_data: h.clone(),
        },
        Lang::DataFrame {
            value: fields,
            help_data: h,
        } => Lang::DataFrame {
            value: fields.clone(),
            help_data: h.clone(),
        },
        Lang::Module {
            name,
            body: exprs,
            module_position: pos,
            config,
            help_data: h,
        } => Lang::Module {
            name: name.clone(),
            body: exprs.iter().map(|e| substitute_type_in_lang(e, subs)).collect(),
            module_position: pos.clone(),
            config: config.clone(),
            help_data: h.clone(),
        },
        Lang::Union(left, right, h) => Lang::Union(
            Box::new(substitute_type_in_lang(left, subs)),
            Box::new(substitute_type_in_lang(right, subs)),
            h.clone(),
        ),
        Lang::JSBlock(body, id, h) => Lang::JSBlock(Box::new(substitute_type_in_lang(body, subs)), *id, h.clone()),
        Lang::VecBlock { .. } => lang.clone(),
        Lang::RFunction { help_data: h, .. } => lang.clone(),
        Lang::Signature { help_data: h, .. } => lang.clone(),
        Lang::TypePattern { help_data: h, .. } => lang.clone(),
        Lang::KeyValue { help_data: h, .. } => lang.clone(),
        Lang::Sequence { .. } => lang.clone(),
        Lang::Use { .. } => lang.clone(),
        _ => lang.clone(),
    }
}

fn specialize_lambda(lambda_lang: &Lang, lambda_type: &Type, expected_type: &Type, context: &Context) -> (Lang, Type) {
    if let (Type::Function(expected_params, expected_ret, _), Type::Function(_lambda_params, lambda_ret, _)) =
        (expected_type, lambda_type)
    {
        // Map the lambda's parameter *names* (e.g. "x") to the concrete
        // expected types. Keying by name is what both substitute_type_in_lang
        // (which matches Lang::Variable by name) and the body re-typing below
        // need — the fresh generic names (T0, T1, …) assigned during the
        // initial abstract typing never appear as variable names in the Lang.
        let mut substitutions: std::collections::HashMap<String, Type> = std::collections::HashMap::new();

        let (parameters, body, help_data) = match lambda_lang {
            Lang::Lambda {
                parameters,
                body,
                help_data,
            } => (parameters, body, help_data),
            _ => return (lambda_lang.clone(), lambda_type.clone()),
        };

        for (param_lang, expected_param) in parameters.iter().zip(expected_params.iter()) {
            if let Lang::Variable { name, .. } = param_lang {
                let expected_param_type = expected_param.get_type();
                if !matches!(expected_param_type, Type::Generic(_, _)) {
                    substitutions.insert(name.clone(), expected_param_type);
                }
            }
        }

        if substitutions.is_empty() {
            return (lambda_lang.clone(), lambda_type.clone());
        }

        // Annotate the lambda's Lang (params + body variables get their
        // related_type set) so transpilation sees the concrete types.
        let specialized_lang = substitute_type_in_lang(lambda_lang, &substitutions);

        // Re-type the *body* directly (not the whole Lambda: the Lang::Lambda
        // typing arm would just re-bind the params to fresh abstract generics)
        // with each parameter name bound to its concrete type. Mirrors
        // `function.rs`'s param-binding loop: a param type carrying an
        // interface facet (a pure interface, or a `List & Interface`
        // intersection) becomes a rigid generic with an interface constraint
        // instead of the raw type, so interface-method calls in the body
        // (`try_constrained_variable_match`, FILTERING 0) resolve — without
        // this, calling an interface method on a lambda param whose only
        // implementer is a different concrete type fails with
        // `FunctionNotFound` inside `map`-like calls.
        // Rigid names introduced below need mapping back to their original
        // concrete param type once the body has been re-typed: an interface
        // method call resolves `Self` to the rigid variable itself, so a
        // body that just returns the receiver unchanged (`fn(a: Self) -> Self`)
        // types to the bare rigid generic, not the concrete type.
        let mut rigid_to_concrete: std::collections::HashMap<String, Type> = std::collections::HashMap::new();
        let specialized_context = substitutions.iter().fold(context.clone(), |ctx, (name, typ)| {
            let reduced = reduce_type(&ctx, typ);
            if facets::interface_facet(&ctx, &reduced).is_some() {
                let (rigid_name, new_ctx) = ctx.clone().fresh_rigid_name();
                rigid_to_concrete.insert(rigid_name.clone(), typ.clone());
                let rigid_type = Type::Generic(rigid_name.clone(), HelpData::default());
                new_ctx.add_interface_constraint(rigid_name, reduced).push_var_type(
                    Var::from_name(name),
                    rigid_type,
                    &ctx,
                )
            } else {
                ctx.clone().push_var_type(Var::from_name(name), typ.clone(), &ctx)
            }
        });
        let body_tc = typing(&specialized_context, body);

        // Keep the concrete body type when re-typing succeeded; otherwise fall
        // back to the abstract lambda return (Any sentinel or generic).
        let specialized_body_type = if matches!(body_tc.value, Type::Empty(_)) {
            lambda_ret.as_ref().clone()
        } else if rigid_to_concrete.is_empty() {
            body_tc.value.clone()
        } else {
            apply_named_generics(&body_tc.value, &rigid_to_concrete)
        };

        let new_lang = match &specialized_lang {
            Lang::Lambda { parameters, .. } => Lang::Lambda {
                parameters: parameters.clone(),
                body: Box::new(body_tc.lang),
                help_data: help_data.clone(),
            },
            _ => specialized_lang.clone(),
        };

        let new_type = Type::Function(
            expected_params.clone(),
            Box::new(specialized_body_type),
            lambda_type.get_help_data().clone(),
        );

        return (new_lang, new_type);
    }
    (lambda_lang.clone(), lambda_type.clone())
}

pub fn apply_from_variable(var: Var, context: &Context, parameters: &[Lang], h: &HelpData) -> TypeContext {
    thread_local! {
        static DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
    }
    let prev = DEPTH.with(|d| {
        let v = d.get();
        d.set(v + 1);
        v
    });
    if prev > 200 {
        DEPTH.with(|d| d.set(d.get() - 1));
        return TypeContext::new(builder::any_type(), Lang::Empty(h.clone()), context.clone())
            .with_errors(vec![TypRError::Type(TypeError::FunctionNotFound(var.clone()))]);
    }
    let result = apply_from_variable_inner(var, context, parameters, h);
    DEPTH.with(|d| d.set(d.get() - 1));
    result
}

// ── Filter chain for function-application dispatch ─────────────────────────
//
// Each FILTERING step is a standalone function; the chain iterates in
// priority order and the first match wins.  Cross-cutting concerns
// (lambda specialization, success building) run once after the match,
// not in every step.

/// Signature of a single filter: `(all_signatures, arg_types, fn_var, context) -> Option<matched_signature>`
type FilterFn = for<'a> fn(&'a [FunctionType], &'a [Type], &'a Var, &'a Context) -> Option<FunctionType>;

/// Refinement function: re-run after lambda specialization with concrete arg types.
type RefineFn = fn(&[FunctionType], &[Type], &Context) -> Option<FunctionType>;

/// A step in the filter chain, optionally with a refinement pass that
/// re-runs after lambda specialization to improve the inferred return type.
struct FilterStep {
    filter: FilterFn,
    refine: Option<RefineFn>,
}

/// FILTERING 0: constrained rigid variable method resolution (§5 elimination)
fn filter_constrained_variable(
    _sigs: &[FunctionType],
    types: &[Type],
    var: &Var,
    ctx: &Context,
) -> Option<FunctionType> {
    try_constrained_variable_match(var, types, ctx)
}

/// FILTERING 0.5: zero-argument call to a plain (non-variadic) function
fn filter_zero_arg(sigs: &[FunctionType], types: &[Type], _var: &Var, ctx: &Context) -> Option<FunctionType> {
    if !types.is_empty() {
        return None;
    }
    sigs.iter()
        .find(|sig| !sig.is_variadic() && sig.get_param_types().is_empty())
        .cloned()
        .and_then(|sig| sig.infer_return_type_direct(types, ctx))
        .or_else(|| {
            sigs.iter()
                .find(|sig| !sig.is_variadic() && sig.min_arity() == 0 && sig.has_defaults())
                .cloned()
                .and_then(|sig| sig.infer_return_type_partial(types, ctx))
        })
}

/// FILTERING 1: first-param equality, then full unification
fn filter_first_param_match(sigs: &[FunctionType], types: &[Type], _var: &Var, ctx: &Context) -> Option<FunctionType> {
    let first_arg_type = types.first()?;
    let candidates = filter_by_first_param(sigs, first_arg_type, ctx);
    if candidates.is_empty() {
        return None;
    }
    try_direct_match(&candidates, types, ctx)
}

/// FILTERING 2: super-type chain (walk ancestors of the first argument)
fn filter_supertype_match(sigs: &[FunctionType], types: &[Type], _var: &Var, ctx: &Context) -> Option<FunctionType> {
    let first_arg_type = types.first()?;
    let super_types = ctx.subtypes.get_ordered_supertypes(first_arg_type, ctx);
    for super_type in &super_types {
        if facets::interface_facet(ctx, &reduce_type(ctx, super_type)).is_some() {
            continue;
        }
        let candidates = filter_by_first_param(sigs, super_type, ctx);
        if candidates.is_empty() {
            continue;
        }
        if let Some(fun_typ) = try_direct_match(&candidates, types, ctx) {
            return Some(fun_typ);
        }
    }
    None
}

/// Like `facets::interface_facet`, but also looks through one level of array
/// wrapping: `[Object]` has an interface facet whenever `Object` does, so an
/// `objects: [Object]` parameter is recognised as an (array-covariant)
/// interface parameter, not just a bare `o: Object` one. `facets::interface_facet`
/// itself stays array-unaware since its other callers (`$`/method-call
/// resolution) must NOT reach through an array to the element's methods.
fn has_interface_facet(ctx: &Context, typ: &Type) -> bool {
    match typ {
        Type::Vec(_, _, elem, _) => facets::interface_facet(ctx, elem).is_some(),
        _ => facets::interface_facet(ctx, typ).is_some(),
    }
}

/// FILTERING 2.5: interface structural subtyping
fn filter_interface_match(sigs: &[FunctionType], types: &[Type], _var: &Var, ctx: &Context) -> Option<FunctionType> {
    let first_arg_type = types.first()?;
    let interface_candidates: Vec<FunctionType> = sigs
        .iter()
        .filter(|sig| {
            sig.get_first_param()
                .map(|p| {
                    let reduced_param = reduce_type(ctx, &p);
                    has_interface_facet(ctx, &reduced_param) && first_arg_type.is_subtype_raw(&reduced_param, ctx)
                })
                .unwrap_or(false)
        })
        .cloned()
        .collect();
    if interface_candidates.is_empty() {
        return None;
    }
    try_interface_subtype_match(&interface_candidates, types, ctx)
}

/// Wrapper so `try_named_generic_match` can serve as a `FilterFn`.
fn filter_named_generic(sigs: &[FunctionType], types: &[Type], _var: &Var, ctx: &Context) -> Option<FunctionType> {
    try_named_generic_match(sigs, types, ctx)
}

fn apply_from_variable_inner(var: Var, context: &Context, parameters: &[Lang], h: &HelpData) -> TypeContext {
    let (expanded_parameters, types, param_errors, arg_context) =
        get_expanded_parameters_with_their_types(context, parameters);
    let context = &arg_context;
    let all_signatures = var.get_functions_from_name(context);

    let filters: &[FilterStep] = &[
        FilterStep {
            filter: filter_constrained_variable,
            refine: None,
        },
        FilterStep {
            filter: filter_zero_arg,
            refine: None,
        },
        FilterStep {
            filter: filter_named_generic,
            refine: Some(try_named_generic_match),
        },
        FilterStep {
            filter: filter_first_param_match,
            refine: None,
        },
        FilterStep {
            filter: filter_supertype_match,
            refine: None,
        },
        FilterStep {
            filter: filter_interface_match,
            refine: None,
        },
        FilterStep {
            filter: try_vectorized_match,
            refine: None,
        },
        FilterStep {
            filter: try_default_match,
            refine: None,
        },
        FilterStep {
            filter: try_variadic_match,
            refine: None,
        },
        FilterStep {
            filter: try_spread_tuple_match,
            refine: None,
        },
    ];

    for step in filters {
        if let Some(fun_typ) = (step.filter)(&all_signatures, &types, &var, context) {
            let (final_params, final_types) = specialize_lambdas(context, &expanded_parameters, &types, &fun_typ);
            let final_fun_typ = match step.refine {
                Some(refine) => refine(&all_signatures, &final_types, context).unwrap_or(fun_typ),
                None => fun_typ,
            };
            return build_success(
                &var,
                &final_fun_typ,
                final_params,
                &final_types,
                param_errors,
                context,
                h,
            );
        }
    }

    // === ERROR : no filtering worked ===
    let mut errors = param_errors;
    // No signature at all under this name (`get_functions_from_name` came back
    // empty) may mean it's declared in an in-scope module but never `use`d —
    // same idea as `VariableNotImported` for a bare variable reference, just
    // reached through function-call resolution instead of `Lang::Variable`.
    let not_imported = all_signatures
        .is_empty()
        .then(|| context.find_variable_source_module(&var.get_name()))
        .flatten();
    match not_imported {
        Some((module_name, is_public)) => {
            errors.push(TypRError::Type(TypeError::VariableNotImported(
                var.get_name(),
                module_name,
                is_public,
                h.clone(),
            )));
        }
        None if !all_signatures.is_empty() => {
            // The name IS bound to function signature(s) — the call just
            // doesn't match any of them (wrong arity or argument types).
            // Reporting `FunctionNotFound` here reads as "the variable
            // doesn't exist", which is wrong and misleading (e.g. `f()` on
            // a 1-parameter lambda looked like lambda-lets were unsupported).
            let signatures = all_signatures
                .iter()
                .map(|sig| {
                    format!(
                        "({}) -> {}",
                        sig.get_param_types()
                            .iter()
                            .map(|t| t.pretty())
                            .collect::<Vec<_>>()
                            .join(", "),
                        sig.get_return_type().pretty()
                    )
                })
                .collect::<Vec<_>>();
            errors.push(TypRError::Type(TypeError::NoMatchingSignature(
                var.get_name(),
                types.clone(),
                signatures,
                h.clone(),
            )));
        }
        None => {
            errors.push(TypRError::Type(TypeError::FunctionNotFound(
                var.clone().set_type_from_params(parameters, context),
            )));
        }
    }
    TypeContext::new(builder::any_type(), Lang::Empty(h.clone()), context.clone()).with_errors(errors)
}

fn specialize_lambdas(
    context: &Context,
    params: &[Lang],
    types: &[Type],
    fun_typ: &FunctionType,
) -> (Vec<Lang>, Vec<Type>) {
    let mut new_params: Vec<Lang> = params.to_vec();
    let mut new_types: Vec<Type> = types.to_vec();
    let fun_params = fun_typ.get_param_types();

    for (i, (param, param_type)) in params.iter().zip(types.iter()).enumerate() {
        if let Lang::Lambda { .. } = param {
            if let Some(expected_type) = fun_params.get(i) {
                let (specialized_lang, specialized_type) = specialize_lambda(param, param_type, expected_type, context);
                new_params[i] = specialized_lang;
                new_types[i] = specialized_type;
            }
        }
    }

    (new_params, new_types)
}

fn get_expanded_parameters_with_their_types(
    context: &Context,
    values: &[Lang],
) -> (Vec<Lang>, Vec<Type>, Vec<TypRError>, Context) {
    // Arguments are typed sequentially, hoisting each argument's type-alias
    // registrations (e.g. the `ArrayN` alias created on the fly by an inline
    // `expr as! [T]` cast) into the context used for the next argument and
    // returned to the caller — the transpiler resolves them there to emit
    // the `|> as.ArrayN()` annotation. Argument-local variables never leak.
    let mut new_context = context.clone();
    let mut new_values = Vec::with_capacity(values.len());
    let mut types = Vec::with_capacity(values.len());
    let mut errors: Vec<TypRError> = Vec::new();
    for value in values {
        let tc = typing(&new_context, value);
        new_context = new_context.hoist_aliases(&tc.context);
        errors.extend(tc.errors);
        types.push(tc.value);
        new_values.push(tc.lang);
    }
    (new_values, types, errors, new_context)
}

fn build_function_lang(h: &HelpData, new_values: Vec<Lang>, fun_typ: &FunctionType, lang: Lang) -> Lang {
    if fun_typ.is_vectorized() {
        Lang::VecFunctionApp {
            vector_type: fun_typ.get_vec_type(),
            identifier: Box::new(lang),
            arguments: new_values.clone(),
            help_data: h.clone(),
        }
    } else {
        Lang::FunctionApp {
            identifier: Box::new(lang),
            arguments: new_values.clone(),
            help_data: h.clone(),
        }
    }
}

pub fn apply_from_expression(context: &Context, fn_var_name: &Lang, values: &[Lang], h: &HelpData) -> TypeContext {
    // Collect errors from parameters but return Any type
    let (_expanded_parameters, _types, param_errors, _) = get_expanded_parameters_with_their_types(context, values);
    let mut errors = param_errors;
    errors.push(TypRError::Type(TypeError::WrongExpression(h.clone())));
    TypeContext::new(builder::any_type(), Lang::Empty(h.clone()), context.clone()).with_errors(errors)
}

pub fn function_application(context: &Context, fn_var_name: &Lang, values: &[Lang], h: &HelpData) -> TypeContext {
    // Interface constructor call (`I(x)`, interface_constructeurs.md §3/§4):
    // when the callee name resolves to an alias with an interface facet, this
    // isn't an ordinary function call — aliases live in a separate namespace
    // from callable variables/functions (`Context::aliases()`), so `I` could
    // never resolve through the normal path below anyway. It's a compile-time
    // structural validator: the argument's type must already satisfy every
    // method the interface requires (§8.1 variance rules), and the call
    // desugars to nothing at transpile time — the argument itself is returned
    // unchanged, exactly like `Self:{...}`/`PartialApp` desugaring elsewhere.
    if values.len() == 1 {
        if let Ok(var) = Var::try_from(fn_var_name.clone()) {
            if let Some(alias_type) = context.get_type_from_aliases(&Var::from_name(&var.get_name())) {
                if let Some(required_methods) = facets::interface_facet(context, &alias_type) {
                    return interface_constructor_call(context, &var.get_name(), &required_methods, &values[0], h);
                }
            }
        }
    }
    match Var::try_from(fn_var_name.clone()) {
        Ok(var) => apply_from_variable(var, context, values, h),
        _ => apply_from_expression(context, fn_var_name, values, h),
    }
}

/// `I(x)`: validates that `argument`'s type structurally satisfies
/// `required_methods` and, on success, desugars to `argument` itself — no
/// instance is created, no field is transformed (RFC §3.2). On failure the
/// error is collected but the argument's own type/lang are still returned so
/// type-checking can continue.
fn interface_constructor_call(
    context: &Context,
    interface_name: &str,
    required_methods: &std::collections::HashSet<ArgumentType>,
    argument: &Lang,
    h: &HelpData,
) -> TypeContext {
    let arg_tc = typing(context, argument);
    let mut errors = arg_tc.errors;
    if let Err(err) =
        interface_satisfaction::check_interface_satisfaction(&arg_tc.context, &arg_tc.value, required_methods)
    {
        let interface_typ = Type::Alias(interface_name.to_string(), vec![], false, h.clone());
        errors.push(TypRError::Type(match err {
            interface_satisfaction::InterfaceSatisfactionError::Missing(names) => {
                TypeError::InterfaceNotSatisfied(arg_tc.value.clone(), interface_typ, names, h.clone())
            }
            interface_satisfaction::InterfaceSatisfactionError::Incompatible(name, expected, found) => {
                TypeError::IncompatibleInterfaceMethod(name, *expected, *found, h.clone())
            }
        }));
    }
    TypeContext::new(arg_tc.value, arg_tc.lang, arg_tc.context).with_errors(errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::processes::parsing::parse_from_string;
    use crate::processes::type_checking::typing_with_errors;
    use crate::utils::builder;
    use crate::utils::fluent_parser::FluentParser;

    // --- variadic stdlib signatures ---

    #[test]
    fn test_cat_variadic_mixed_args_types_to_empty() {
        // `@cat: (...values: Any) -> Empty;` (std_R.ty) — mixed argument
        // types all match the Any variadic, and the Empty return satisfies
        // an enclosing `-> Empty` annotation (the shape `cat("x:", v, "\n")`
        // inside a printing function).
        // FluentParser::new() starts from Context::empty() (no stdlib);
        // the preloaded typed signatures live in Context::default().
        let fp = FluentParser::new()
            .set_context(Context::default())
            .push(r#"cat("Todo:", "a", true, "\n");"#)
            .parse_type_next();
        assert_eq!(fp.get_last_type(), builder::empty_type());
        assert_eq!(fp.get_last_log(), "The logs are empty");
    }

    // --- interface constructors (interface_constructeurs.md) ---

    #[test]
    fn test_interface_constructor_call_satisfied_no_type_errors() {
        let src = "type Point <- list { x: int, y: int };\n\
                   let mv <- fn(p: Point, dx: int, dy: int): Point { p };\n\
                   type Movable <- interface { mv: (Self, int, int) -> Self };\n\
                   let p <- Point:{ x = 1, y = 2 };\n\
                   Movable(p);";
        let ast = parse_from_string(src, "test.ty");
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(!result.has_errors(), "expected no errors, got: {:?}", result.errors);
    }

    #[test]
    fn test_interface_constructor_call_satisfied_passes_through() {
        let fp = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .run()
            .push("let mv <- fn(p: Point, dx: int, dy: int): Point { p };")
            .run()
            .push("type Movable <- interface { mv: (Self, int, int) -> Self };")
            .run()
            .push("let p <- Point:{ x = 1, y = 2 };")
            .run()
            .push("Movable(p)")
            .run();
        assert_eq!(fp.get_last_log(), "The logs are empty");
        let r_code = fp.get_r_code().iter().cloned().collect::<Vec<_>>().join("\n");
        // `I(x)` desugars to `x` itself: no wrapper call in the generated R.
        assert!(
            !r_code.contains("Movable("),
            "expected no Movable(...) wrapper call in transpiled R, got:\n{r_code}"
        );
        assert!(
            r_code.trim_end().ends_with('p'),
            "expected the last transpiled statement to be a bare pass-through of `p`, got:\n{r_code}"
        );
    }

    #[test]
    fn test_interface_constructor_call_missing_method() {
        let src = "type Point <- list { x: int, y: int };\n\
                   type Movable <- interface { mv: (Self, int, int) -> Self };\n\
                   let p <- Point:{ x = 1, y = 2 };\n\
                   Movable(p);";
        let ast = parse_from_string(src, "test.ty");
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(result.has_errors(), "expected a missing-method error");
        assert!(result
            .errors
            .iter()
            .any(|e| matches!(e, TypRError::Type(TypeError::InterfaceNotSatisfied(_, _, _, _)))));
    }

    #[test]
    fn test_interface_constructor_call_incompatible_signature() {
        let src = "type Point <- list { x: int, y: int };\n\
                   let mv <- fn(p: Point, dx: int): Point { p };\n\
                   type Movable <- interface { mv: (Self, int, int) -> Self };\n\
                   let p <- Point:{ x = 1, y = 2 };\n\
                   Movable(p);";
        let ast = parse_from_string(src, "test.ty");
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(result.has_errors(), "expected an incompatible-signature error");
        assert!(result.errors.iter().any(|e| matches!(
            e,
            TypRError::Type(TypeError::InterfaceNotSatisfied(_, _, _, _))
                | TypRError::Type(TypeError::IncompatibleInterfaceMethod(_, _, _, _))
        )));
    }

    #[test]
    fn test_interface_constructor_call_chains_two_interfaces() {
        // `DrawableMovable <- Movable & Drawable` merges into one
        // `Type::Interface` (Interface & Interface norm_intersection), so
        // calling `DrawableMovable(p)` validates both `mv` and `draw` in one
        // shot without any dedicated "chain" codegen.
        let src = "type Point <- list { x: int, y: int };\n\
                   let mv <- fn(p: Point, dx: int, dy: int): Point { p };\n\
                   let draw <- fn(p: Point): char { \"o\" };\n\
                   type Movable <- interface { mv: (Self, int, int) -> Self };\n\
                   type Drawable <- interface { draw: (Self) -> char };\n\
                   type DrawableMovable <- Movable & Drawable;\n\
                   let p <- Point:{ x = 1, y = 2 };\n\
                   DrawableMovable(p);";
        let ast = parse_from_string(src, "test.ty");
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(!result.has_errors(), "expected no errors, got: {:?}", result.errors);
    }

    #[test]
    fn test_interface_constructor_call_chains_two_interfaces_missing_one() {
        let src = "type Point <- list { x: int, y: int };\n\
                   let mv <- fn(p: Point, dx: int, dy: int): Point { p };\n\
                   type Movable <- interface { mv: (Self, int, int) -> Self };\n\
                   type Drawable <- interface { draw: (Self) -> char };\n\
                   type DrawableMovable <- Movable & Drawable;\n\
                   let p <- Point:{ x = 1, y = 2 };\n\
                   DrawableMovable(p);";
        let ast = parse_from_string(src, "test.ty");
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(result.has_errors(), "expected a missing draw method error");
        assert!(result.errors.iter().any(|e| matches!(
            e,
            TypRError::Type(TypeError::InterfaceNotSatisfied(_, _, names, _)) if names.contains(&"draw".to_string())
        )));
    }

    #[test]
    fn test_vectorization0() {
        let res = FluentParser::new()
            .push("@f1: (int) -> int;")
            .run()
            .check_typing("f1([1, 2])");
        let fun_typ = builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, fun_typ);
    }

    #[test]
    fn test_litteral_char_type1() {
        let res = FluentParser::new()
            .push("@f3: (\"hello\") -> bool;")
            .run()
            .check_typing("f3(\"hello\")");
        assert_eq!(res, builder::boolean_type());
    }

    #[test]
    fn test_litteral_char_type2() {
        let res = FluentParser::new()
            .push("@f3: (char) -> bool;")
            .run()
            .check_typing("f3(\"hello\")");
        assert_eq!(res, builder::boolean_type());
    }

    #[test]
    fn test_union_litteral1() {
        let res = FluentParser::new()
            .push("@f3: (\"html\" | \"h1\") -> bool;")
            .run()
            .check_typing("f3(\"h1\")");
        assert_eq!(res, builder::boolean_type());
    }

    #[test]
    fn test_vectorization_binary_function() {
        // Calling a binary function (int, int) -> int with an array and a scalar
        // should lift the function and produce [4, int]
        let res = FluentParser::new()
            .push("@f2: (int, int) -> int;")
            .run()
            .check_typing("f2([1, 2, 3, 4], 1)");
        let expected = builder::array_type(builder::integer_type(4), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    #[test]
    fn test_union_litteral2() {
        let res = FluentParser::new()
            .push("let f3 <- fn(a: \"html\" | \"h1\"): char { \"hello\" };")
            .run()
            .check_transpiling("f3(\"h1\")");
        assert_eq!(
            res.iter().cloned().collect::<Vec<_>>(),
            vec![
                "`f3.Union0` <- (function(a) {\n\"hello\" |> as.Character()\n} |> as.Character()) |> as.Function0()\n"
                    .to_string(),
                "f3(\"h1\" |> as.Character())".to_string(),
            ]
        );
    }

    // =====================================================================
    // Vectorization / lifting tests for apply_from_variable
    // =====================================================================
    //
    // These tests verify that apply_from_variable correctly lifts
    // (vectorizes) functions when called with array arguments.
    // For example, a function with signature (int, int) -> int should
    // work with:
    //   - scalar, scalar  -> int           (direct match, no lifting)
    //   - [N, int], scalar -> [N, int]     (left array, lift)
    //   - scalar, [N, int] -> [N, int]     (right array, lift)
    //   - [N, int], [N, int] -> [N, int]   (both arrays, lift)
    //
    // We use named functions (e.g. `add`) instead of operators (e.g. `+`)
    // because operator expressions with array literals on the left trigger
    // a miette span bug. The vectorization logic in apply_from_variable
    // is the same regardless of how the function is resolved.
    // =====================================================================

    // --- Binary function: scalar + scalar (no lifting) ---
    #[test]
    fn test_lift_add_scalar_scalar() {
        let res = FluentParser::new()
            .push("@add: (int, int) -> int;")
            .run()
            .check_typing("add(1, 1)");
        assert_eq!(res, builder::integer_type_default());
    }

    // --- Binary function: [2, int] + scalar (left lifting) ---
    #[test]
    fn test_lift_add_array_left_scalar_right() {
        let res = FluentParser::new()
            .push("@add: (int, int) -> int;")
            .run()
            .check_typing("add([1, 2], 1)");
        let expected = builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Binary function: scalar + [2, int] (right lifting) ---
    #[test]
    fn test_lift_add_scalar_left_array_right() {
        let res = FluentParser::new()
            .push("@add: (int, int) -> int;")
            .run()
            .check_typing("add(1, [1, 2])");
        let expected = builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Binary function: [2, int] + [2, int] (both arrays) ---
    #[test]
    fn test_lift_add_array_both() {
        let res = FluentParser::new()
            .push("@add: (int, int) -> int;")
            .run()
            .check_typing("add([1, 2], [3, 4])");
        let expected = builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Binary function: larger arrays [5, int] + scalar ---
    #[test]
    fn test_lift_add_array_larger() {
        let res = FluentParser::new()
            .push("@add: (int, int) -> int;")
            .run()
            .check_typing("add([1, 2, 3, 4, 5], 1)");
        let expected = builder::array_type(builder::integer_type(5), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Array alias argument: `type Numbers <- [int]` lifts like the array itself ---
    #[test]
    fn test_lift_array_alias_argument() {
        let res = FluentParser::new()
            .push("type Numbers <- [int];")
            .run()
            .push("@f1: (int) -> int;")
            .run()
            .check_typing("f1(Numbers:[1, 2, 3, 4])");
        // The alias carries no static size, so the lifted result is an
        // unknown-size (index 0) array.
        let expected = builder::array_type(builder::integer_type(0), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Opaque array alias argument stays scalar: no lifting through opacity ---
    #[test]
    fn test_no_lift_through_opaque_array_alias() {
        let res = FluentParser::new()
            .set_context(Context::default())
            .push("opaque Numbers <- [int];")
            .run()
            .push("@f1: (int) -> int;")
            .run()
            .check_typing("f1(Numbers:[1, 2, 3, 4])");
        assert!(
            !matches!(res, Type::Vec(_, _, _, _)),
            "opaque alias got lifted: {:?}",
            res
        );
    }

    // --- Generic-size array argument ([#N, int]) lifts too ---
    #[test]
    fn test_lift_generic_size_array_argument() {
        let res = FluentParser::new()
            .push("@f1: (int) -> int;")
            .run()
            .push("let g <- fn(v: [#N, int]): [#N, int] { f1(v) };")
            .run()
            .check_typing("g([1, 2, 3])");
        let expected = builder::array_type(builder::integer_type(3), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Single-element array [1, int] lifts (used to be rejected by the size > 1 gate) ---
    #[test]
    fn test_lift_single_element_array() {
        let res = FluentParser::new()
            .push("@f1: (int) -> int;")
            .run()
            .check_typing("f1([1])");
        let expected = builder::array_type(builder::integer_type(1), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Subtraction: scalar - scalar (no lifting) ---
    #[test]
    fn test_lift_sub_scalar_scalar() {
        let res = FluentParser::new()
            .push("@sub: (int, int) -> int;")
            .run()
            .check_typing("sub(1, 1)");
        assert_eq!(res, builder::integer_type_default());
    }

    // --- Subtraction: [3, int] - scalar (left lifting) ---
    #[test]
    fn test_lift_sub_array_left_scalar_right() {
        let res = FluentParser::new()
            .push("@sub: (int, int) -> int;")
            .run()
            .check_typing("sub([1, 2, 3], 1)");
        let expected = builder::array_type(builder::integer_type(3), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Multiplication: scalar * [3, int] (right lifting) ---
    #[test]
    fn test_lift_mul_scalar_left_array_right() {
        let res = FluentParser::new()
            .push("@mul: (int, int) -> int;")
            .run()
            .check_typing("mul(2, [1, 2, 3])");
        let expected = builder::array_type(builder::integer_type(3), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Division: [2, int] / [2, int] (both arrays) ---
    #[test]
    fn test_lift_div_array_both() {
        let res = FluentParser::new()
            .push("@div: (int, int) -> int;")
            .run()
            .check_typing("div([10, 20], [2, 4])");
        let expected = builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Unary function: (int) -> bool lifted with [3, int] ---
    #[test]
    fn test_lift_unary_custom() {
        let res = FluentParser::new()
            .push("@is_positive: (int) -> bool;")
            .run()
            .check_typing("is_positive([1, 2, 3])");
        let expected = builder::array_type(builder::integer_type(3), builder::boolean_type());
        assert_eq!(res, expected);
    }

    // --- Binary function: (int, int) -> bool, both arrays ---
    #[test]
    fn test_lift_binary_custom_both_arrays() {
        let res = FluentParser::new()
            .push("@compare: (int, int) -> bool;")
            .run()
            .check_typing("compare([1, 2], [3, 4])");
        let expected = builder::array_type(builder::integer_type(2), builder::boolean_type());
        assert_eq!(res, expected);
    }

    // --- Binary function: (int, int) -> bool, scalar + array ---
    #[test]
    fn test_lift_binary_custom_scalar_array() {
        let res = FluentParser::new()
            .push("@compare: (int, int) -> bool;")
            .run()
            .check_typing("compare(1, [3, 4, 5])");
        let expected = builder::array_type(builder::integer_type(3), builder::boolean_type());
        assert_eq!(res, expected);
    }

    // --- No lifting needed: scalar call matches directly ---
    #[test]
    fn test_no_lift_direct_match() {
        let res = FluentParser::new()
            .push("@process: (int, int) -> char;")
            .run()
            .check_typing("process(1, 2)");
        assert_eq!(res, builder::character_type_default());
    }

    // --- Lifting with num type: scalar + scalar (direct match) ---
    #[test]
    fn test_lift_num_scalar_scalar() {
        let res = FluentParser::new()
            .push("@fadd: (num, num) -> num;")
            .run()
            .check_typing("fadd(1.5, 2.3)");
        assert_eq!(res, builder::number_type());
    }

    // --- Lifting with num type: [2, num] + scalar ---
    #[test]
    fn test_lift_num_array_scalar() {
        let res = FluentParser::new()
            .push("@fadd: (num, num) -> num;")
            .run()
            .check_typing("fadd([1.5, 2.3], 1.0)");
        let expected = builder::array_type(builder::integer_type(2), builder::number_type());
        assert_eq!(res, expected);
    }

    // --- Operator syntax: scalar + scalar (via `+`) ---
    #[test]
    fn test_operator_add_scalar_scalar() {
        let res = FluentParser::new()
            .push("@`+`: (int, int) -> int;")
            .run()
            .check_typing("1 + 1");
        assert_eq!(res, builder::integer_type_default());
    }

    // --- Operator syntax: scalar + [2, int] (via `+`, right lifting) ---
    #[test]
    fn test_operator_add_scalar_left_array_right() {
        let res = FluentParser::new()
            .push("@`+`: (int, int) -> int;")
            .run()
            .check_typing("1 + [1, 2]");
        let expected = builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Operator syntax: scalar * [3, int] (via `*`, right lifting) ---
    #[test]
    fn test_operator_mul_scalar_left_array_right() {
        let res = FluentParser::new()
            .push("@`*`: (int, int) -> int;")
            .run()
            .check_typing("2 * [1, 2, 3]");
        let expected = builder::array_type(builder::integer_type(3), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Operator syntax: num + num (via `+`) ---
    #[test]
    fn test_operator_add_num_scalar_scalar() {
        let res = FluentParser::new()
            .push("@`+`: (num, num) -> num;")
            .run()
            .check_typing("1.5 + 2.3");
        assert_eq!(res, builder::number_type());
    }

    // --- Operator syntax: scalar - scalar (via `-`) ---
    #[test]
    fn test_operator_sub_scalar_scalar() {
        let res = FluentParser::new()
            .push("@`-`: (int, int) -> int;")
            .run()
            .check_typing("1 - 1");
        assert_eq!(res, builder::integer_type_default());
    }

    // =====================================================================
    // Transpilation tests for vectorization
    // =====================================================================

    // --- Transpilation: scalar * array should produce vec_apply ---
    #[test]
    fn test_transpile_vec_apply_mul_scalar_array() {
        let fp = FluentParser::new()
            .push("@`*`: (int, int) -> int;")
            .run()
            .push("2 * [1, 2, 3]")
            .parse_next()
            .type_next()
            .transpile_next();
        let r_code = fp.get_r_code();
        let code = r_code.iter().last().unwrap().clone();
        // Step ③ (unification_arrays.md): `[1, 2, 3]` is a bare atomic
        // vector and `*` is natively element-wise in R, so the lifted call
        // is emitted directly (no vec_apply/vapply wrapper).
        assert!(
            code.contains("`*`(") && !code.contains("vec_apply") && !code.contains("vapply"),
            "Expected a direct `*` call on the atomic vector, got: {}",
            code
        );
    }

    // --- Transpilation: let a1 <- [1,2,3]; 2*a1+3 should use vec_apply ---
    #[test]
    fn test_transpile_vec_apply_let_then_operator() {
        let fp = FluentParser::new()
            .push("@`*`: (int, int) -> int;")
            .run()
            .push("@`+`: (int, int) -> int;")
            .run()
            .push("let a1 <- [1, 2, 3];")
            .parse_type_transpile_next()
            .push("2*a1+3")
            .parse_type_transpile_next();
        let r_codes: Vec<_> = fp.get_r_code().iter().cloned().collect();
        let last_code = r_codes.last().unwrap();
        assert!(
            last_code.contains("vec_apply"),
            "Expected vec_apply in transpiled code for 2*a1+3, got: {}",
            last_code
        );
    }

    // =====================================================================
    // Generic function variable dispatch tests
    // =====================================================================

    /// When a function variable (e.g. `incr`) is passed as argument to a
    /// generic higher-order function (e.g. `apply`), the transpiled R code
    /// must resolve the function name to the concrete type dispatch
    /// (e.g. `incr.int`) instead of `incr.Generic`.
    #[test]
    fn test_generic_function_variable_transpiles_without_generic() {
        let fp = FluentParser::new()
            .push("let apply <- fn(a: T, f: (T) -> U): U { f(a) };")
            .run()
            .push("let incr <- fn(i: int): int { i + 1 };")
            .run()
            .push("apply(4, incr)")
            .parse_type_transpile_next();
        let r_codes: Vec<_> = fp.get_r_code().iter().cloned().collect();
        let last_code = r_codes.last().unwrap();
        assert!(
            !last_code.contains("Generic"),
            "Expected no 'Generic' in transpiled code, got: {}",
            last_code
        );
    }

    /// Test that anonymous lambdas can be passed as arguments to
    /// higher-order functions. The lambda `\(n) n + 1` should have type
    /// `(T) -> U` where T is inferred from the first argument and U from the body.
    #[test]
    fn test_lambda_as_higher_order_function_argument() {
        let fp = FluentParser::new()
            .push("@`+`: (int, int) -> int;")
            .run()
            .push("let single_map <- fn(a: T, f: (T) -> U): U { f(a) };")
            .run()
            .push("single_map(3, \\(n) n + 1)")
            .parse_type_transpile_next();
        let r_codes: Vec<_> = fp.get_r_code().iter().cloned().collect();
        let last_code = r_codes.last().unwrap();
        assert!(
            !last_code.contains("Generic"),
            "Expected no 'Generic' in transpiled code, got: {}",
            last_code
        );
    }

    /// Test that a lambda returning a different type than its input works correctly.
    #[test]
    fn test_lambda_with_different_input_output_types() {
        let fp = FluentParser::new()
            .push("@to_string: (int) -> char;")
            .run()
            .push("let transform <- fn(a: T, f: (T) -> U): U { f(a) };")
            .run()
            .push("transform(42, \\(x) to_string(x))")
            .parse_type_transpile_next();
        let r_codes: Vec<_> = fp.get_r_code().iter().cloned().collect();
        let last_code = r_codes.last().unwrap();
        assert!(
            !last_code.contains("Generic"),
            "Expected no 'Generic' in transpiled code, got: {}",
            last_code
        );
    }

    // =====================================================================
    // Type alias resolution in function application
    // =====================================================================

    /// A function whose parameter is a type alias (e.g. `Int` aliasing `int`)
    /// should accept a value of the underlying concrete type (`int`).
    #[test]
    fn test_type_alias_param_accepts_concrete_type() {
        let res = FluentParser::new()
            .push("type Int <- int;")
            .run()
            .push("let id <- fn(o: Int): Int { o };")
            .run()
            .check_typing("id(10)");
        // Int reduces to int, so the return type should be int
        assert_eq!(res, builder::integer_type_default());
    }

    /// A non-generic function returning a named record alias (e.g. `Scene`)
    /// must keep that alias as the call's inferred type, not the expanded
    /// structural record. Regression test for case 0006: chaining a second
    /// UFCS call (`sc.add(c1).add(c1)`) on the result used to lose the alias
    /// in `UnificationMap::apply_unification_type`, which unconditionally fell
    /// back to `ret_ty.reduce(context)` whenever no generic substitution
    /// applied (true for any non-generic function, not just generics).
    #[test]
    fn test_chained_call_preserves_record_alias_return_type() {
        let res = FluentParser::new()
            .push("type Scene <- list { width: int };")
            .run()
            .push("let add <- fn(self: Scene, n: int): Scene { self };")
            .run()
            .push("let sc <- Scene:{ width = 1 };")
            .run()
            .check_typing("sc.add(1).add(2)");
        assert_eq!(
            res,
            Type::Alias("Scene".to_string(), vec![], false, HelpData::default())
        );
    }

    /// A function with a concrete `int` parameter should accept a value
    /// whose type is an alias that reduces to `int`.
    #[test]
    fn test_concrete_param_accepts_alias_type_arg() {
        let res = FluentParser::new()
            .push("type Int <- int;")
            .run()
            .push("@incr: (int) -> int;")
            .run()
            .check_typing("incr(10)");
        assert_eq!(res, builder::integer_type_default());
    }

    /// Both sides are aliases: function param is alias A, argument is alias B,
    /// both reduce to the same concrete type.
    #[test]
    fn test_alias_param_with_alias_arg() {
        let res = FluentParser::new()
            .push("type MyInt <- int;")
            .run()
            .push("type AlsoInt <- int;")
            .run()
            .push("let id <- fn(o: MyInt): MyInt { o };")
            .run()
            .push("let a: AlsoInt <- 42;")
            .parse_type_next()
            .push("id(a)")
            .parse_next();
        // Both MyInt and AlsoInt reduce to int, so id(a) should type-check
        // The return type is MyInt (the alias from the function signature)
        let last_type = res.get_last_type();
        assert!(
            last_type.is_alias() || last_type == builder::integer_type_default(),
            "Expected alias or int type, got: {:?}",
            last_type
        );
    }

    // =====================================================================
    // Generic type parameter inference
    // =====================================================================

    /// When calling a generic function with a concrete type argument,
    /// the return type should be properly resolved (not left as a generic).
    #[test]
    fn test_generic_function_return_type_inference() {
        let fp = FluentParser::new()
            .push("let apply <- fn(a: T, f: (T) -> U): U { f(a) };")
            .run()
            .push("apply(3, \\(n) n)")
            .parse_type_next();
        let last_type = fp.get_last_type();
        assert_eq!(
            last_type,
            builder::integer_type_default(),
            "Expected int, got: {:?}",
            last_type
        );
    }

    /// Multiple levels of generic inference should chain correctly.
    #[test]
    fn test_chained_generic_inference() {
        let res = FluentParser::new()
            .push("let extract <- fn(c: T): T { c };")
            .run()
            .push("extract(42)")
            .parse_type_next();
        assert_eq!(res.get_last_type(), builder::integer_type_default());
    }

    /// Test that Option<T> return type is properly inferred.
    #[test]
    fn test_option_generic_return_type() {
        let fp = FluentParser::new()
            .push("type Option<T> <- .Some(T) | .None;")
            .run()
            .push("let map <- fn(o: T, f: (T) -> U): U { f(o) };")
            .run()
            .push("map(3, \\(n) n)")
            .parse_type_next();
        assert_eq!(
            fp.get_last_type(),
            builder::integer_type_default(),
            "Expected int, got: {:?}",
            fp.get_last_type()
        );
    }

    /// Test generic function with concrete return type.
    #[test]
    fn test_generic_function_concrete_return() {
        let fp = FluentParser::new()
            .push("let first <- fn(a: T, b: U): T { a };")
            .run()
            .push("first(3, \"hello\")")
            .parse_type_next();
        assert_eq!(
            fp.get_last_type(),
            builder::integer_type_default(),
            "Expected int, got: {:?}",
            fp.get_last_type()
        );
    }

    /// Test that a concrete type satisfying a named interface alias
    /// can be passed to a function expecting that interface via dot notation.
    #[test]
    fn test_interface_alias_structural_subtyping_dot_call() {
        let fp = FluentParser::new()
            .push("type Incrementable <- interface { incr: (Self) -> Self };")
            .run()
            .push("let double <- fn(i: Incrementable): Incrementable { i.incr().incr() };")
            .run()
            .push("let incr <- fn(i: int): int { i + 1 };")
            .run()
            .push("(3).double()")
            .parse_type_next();
        let last_log = fp.get_last_log();
        assert!(
            !last_log.contains("not defined in this scope"),
            "Expected double to resolve, got: {}",
            last_log
        );
    }

    /// When calling a function with an interface parameter and the return type is the same
    /// interface, the return type should be the concrete argument type (universal generic
    /// semantics: fn(i: I): I  desugars to  forall A: I. A -> A).
    #[test]
    fn test_interface_param_infers_concrete_return_type() {
        let fp = FluentParser::new()
            .push("type Incrementable <- interface { incr: (Self) -> Self };")
            .run()
            .push("let double <- fn(i: Incrementable): Incrementable { i.incr().incr() };")
            .run()
            .push("let incr <- fn(i: int): int { i + 1 };")
            .run()
            .push("double(3)")
            .parse_type_next();
        assert_eq!(
            fp.get_last_type(),
            builder::integer_type_default(),
            "Expected int (concrete return type), got: {:?}",
            fp.get_last_type()
        );
    }

    /// Chaining interface method calls: double(double(3)) should return int.
    #[test]
    fn test_interface_chained_application_returns_concrete_type() {
        let fp = FluentParser::new()
            .push("type Incrementable <- interface { incr: (Self) -> Self };")
            .run()
            .push("let double <- fn(i: Incrementable): Incrementable { i.incr().incr() };")
            .run()
            .push("let incr <- fn(i: int): int { i + 1 };")
            .run()
            .push("double(double(3))")
            .parse_type_next();
        assert_eq!(
            fp.get_last_type(),
            builder::integer_type_default(),
            "Expected int, got: {:?}",
            fp.get_last_type()
        );
    }

    #[test]
    fn test_tuple_spread_append_basic() {
        // @append: (tuple{T...}, U) -> tuple{T..., U};
        // list("apple", "banana") has type tuple{apple, banana}
        // append(fruits, "kiwi") should return tuple{apple, banana, kiwi}
        let fp = FluentParser::new()
            .push("@append: (tuple{T...}, U) -> tuple{T..., U};")
            .run()
            .push("let fruits <- list(\"apple\", \"banana\");")
            .run()
            .push("fruits.append(\"kiwi\")")
            .parse_type_next();

        let result = fp.get_last_type();
        // Result should be tuple{apple, banana, kiwi}
        assert!(
            matches!(&result, Type::Tuple(elems, _) if elems.len() == 3),
            "Expected tuple with 3 elements, got: {result:?}"
        );
    }

    #[test]
    fn test_tuple_spread_append_ufc_syntax() {
        let fp = FluentParser::new()
            .push("@append: (tuple{T...}, U) -> tuple{T..., U};")
            .run()
            .push("append(list(\"a\", \"b\", \"c\"), \"d\")")
            .parse_type_next();

        let result = fp.get_last_type();
        assert!(
            matches!(&result, Type::Tuple(elems, _) if elems.len() == 4),
            "Expected tuple with 4 elements, got: {result:?}"
        );
    }

    // --- State<T>: stdlib overload resolution (see configs/std/state.ty) ---

    /// Reproduces the `map` failure when the array element type is a concrete
    /// record (`[any, list{id: char}]`) and the mapping function is typed with
    /// an alias for that record.
    #[test]
    fn test_map_over_any_dim_record_array() {
        // Wrap in a function so `arr` enters context with type [Any, Object].
        let fp = FluentParser::new()
            .push("type Object <- list { id: char };")
            .run()
            .push("let animate <- fn(obj: Object, action: char): Object { obj };")
            .run()
            .push("let use_map <- fn(arr: [Any, Object]): [Any, Object] { arr |> map(\\animate(action = \"run\")) };")
            .parse_type_next();
        let log = fp.get_last_log();
        assert!(
            !log.contains("not defined in this scope"),
            "map over [any, Object] (alias) failed:\n{}",
            log
        );

        // Same but with the concrete record type as element — this is the case
        // that triggers the user's error: [any, list{id:char}] vs animate<Object>.
        let fp2 = FluentParser::new()
            .push("type Object <- list { id: char };")
            .run()
            .push("let animate <- fn(obj: Object, action: char): Object { obj };")
            .run()
            .push("let use_map <- fn(arr: [Any, list{id: char}]): [Any, list{id: char}] { arr |> map(\\animate<Object>(action = \"run\")) };")
            .parse_type_next();
        let log2 = fp2.get_last_log();
        assert!(
            !log2.contains("not defined in this scope"),
            "map over [any, list{{id:char}}] with animate<Object> failed:\n{}",
            log2
        );
    }

    /// fold with an untyped lambda whose body can't be typed with abstract params
    /// (T0, T1) should still resolve the return type from the accumulator arg.
    /// Regression for: "Expected: StoryBoard, Found: U"
    #[test]
    fn test_fold_return_type_resolved_from_accumulator() {
        let fp = FluentParser::new()
            .set_context(Context::default())
            .push("type Frame <- list { id: int };")
            .run()
            .push("type Clip <- list { frames: [Any, Frame] };")
            .run()
            .push("let append_frame <- fn(clip: Clip, f: Frame): Clip { clip };")
            .run()
            .push("let frames: [Any, Frame] <- [Frame:{ id = 1 }];")
            .run()
            .push("let base: Clip <- Clip:{ frames = frames };")
            .run()
            .push("frames |> fold(base, \\(acc, x) append_frame(acc, x))")
            .parse_type_next();
        let typ = fp.get_last_type();
        let log = fp.get_last_log();
        assert!(
            matches!(&typ, Type::Alias(name, _, _, _) if name == "Clip"),
            "fold should return Clip (resolved from accumulator), got: {typ:?}\nlog: {log}"
        );
    }

    #[test]
    fn test_state_get_resolves_against_real_stdlib() {
        let typ = FluentParser::new()
            .set_context(Context::default())
            .push("let s <- state(10);")
            .run()
            .push("get(s)")
            .parse_type_next()
            .get_last_type();
        assert!(
            matches!(&typ, Type::Integer(..)),
            "get(state(10)) should resolve to an int, got: {typ:?}"
        );
    }

    #[test]
    fn test_state_get_overload_does_not_break_legacy_two_arg_get() {
        // Regression guard: adding `@get: (s: State<T>) -> T;` must not break
        // the pre-existing `@get: (a: Any, b: char) -> T;` overload. That
        // overload's `T` only appears in the return position (never in a
        // param), so it stays an unresolved Generic — that's the existing,
        // pre-State behavior; the point here is just that resolution still
        // picks this 2-arg overload instead of erroring or misfiring on the
        // new 1-arg State overload.
        let typ = FluentParser::new()
            .set_context(Context::default())
            .push("type Point <- list { x: int, y: int };")
            .run()
            .push("let p <- Point:{ x = 1, y = 2 };")
            .run()
            .push("get(p, \"x\")")
            .parse_type_next()
            .get_last_type();
        assert!(
            matches!(&typ, Type::Generic(name, _) if name == "T"),
            "get(p, \"x\") should still resolve via the legacy 2-arg overload, got: {typ:?}"
        );
    }

    #[test]
    fn test_map_any_int_with_untyped_lambda_no_type_error() {
        use crate::processes::parsing::parse;
        use crate::processes::type_checking::type_checker::TypeChecker;
        // Use the same parse path as the CLI (parse → Lang::Lines).
        let src = "let f <- fn(a: [Any, int]): [Any, int] {\n    a\n        |> map(\\(x) x + 1)\n};";
        let lang = parse(nom_locate::LocatedSpan::new_extra(src, "test.ty".to_string())).ast;
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&lang);
        let errors: Vec<String> = tc.get_errors().iter().map(|e| format!("{e:?}")).collect();
        assert!(
            tc.get_errors().is_empty(),
            "map over [Any, int] with untyped lambda produced unexpected type errors:\n{}",
            errors.join("\n")
        );
    }

    #[test]
    fn test_lambda_let_call_with_matching_arity_type_checks() {
        let src = "let ident <- \\(x) x;\nident(3);";
        let ast = parse_from_string(src, "test.ty");
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(!result.has_errors(), "expected no errors, got: {:?}", result.errors);
    }

    #[test]
    fn test_arity_mismatch_reports_no_matching_signature_not_function_not_found() {
        // `ident()` on a 1-parameter lambda used to report `FunctionNotFound`
        // ("not defined in this scope"), which reads as lambda-lets being
        // unsupported. The name is bound — the call just matches no signature.
        let src = "let ident <- \\(x) x;\nident();";
        let ast = parse_from_string(src, "test.ty");
        let result = typing_with_errors(&Context::default(), &ast);
        assert!(
            result.errors.iter().any(|e| matches!(
                e,
                crate::components::error_message::typr_error::TypRError::Type(TypeError::NoMatchingSignature(
                    name,
                    args,
                    _,
                    _
                )) if name == "ident" && args.is_empty()
            )),
            "expected NoMatchingSignature for zero-arg call on a 1-param lambda, got: {:?}",
            result.errors
        );
    }
}
