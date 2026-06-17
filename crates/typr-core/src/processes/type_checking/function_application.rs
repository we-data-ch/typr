#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::set_related_type_if_variable;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::type_system::TypeSystem;
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
fn try_constrained_variable_match(
    var: &Var,
    types: &[Type],
    context: &Context,
) -> Option<FunctionType> {
    let first_type = types.first()?;
    let rigid_name = match first_type {
        Type::Generic(name, _) => name,
        _ => return None,
    };
    let interface = context.get_interface_constraint(rigid_name)?;
    let methods = match interface {
        Type::Interface(methods, _) => methods,
        _ => return None,
    };
    let method = methods
        .iter()
        .find(|m| m.get_argument_str() == var.get_name())?;
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
    TypeContext::new(fun_typ.get_infered_return_type(), new_expr, context.clone())
        .with_errors(param_errors)
}

fn filter_by_first_param(
    signatures: &[FunctionType],
    first_arg_type: &Type,
    context: &Context,
) -> Vec<FunctionType> {
    let reduced_arg = reduce_type(context, first_arg_type);
    let (exact, rest): (Vec<&FunctionType>, Vec<&FunctionType>) =
        signatures.iter().partition(|sig| {
            sig.get_first_param()
                .map(|p| {
                    let reduced_p = reduce_type(context, &p);
                    reduced_p == reduced_arg
                        || matches!(
                            reduced_p,
                            Type::Generic(_, _) | Type::IndexGen(_, _) | Type::LabelGen(_, _)
                        )
                })
                .unwrap_or(false)
        });
    // Composite generic params (e.g. `[#N, T]` against an argument `[#N, num]`)
    // don't pass the equality test above but can still unify. They are appended
    // after the exact matches so they don't steal overload resolution.
    // Also include signatures where the arg is a primitive subtype of the param
    // (e.g. Char(Val("A")) <: Char(Unknown)), excluding Any so that Any-param
    // functions still fall through to FILTERING 3 vectorization as before.
    let unifiable = rest.into_iter().filter(|sig| {
        sig.get_first_param()
            .map(|p| {
                let reduced_p = reduce_type(context, &p);
                // Exclude Any (belongs to FILTERING 3) and Interface (belongs to
                // FILTERING 2.5) from the is_subtype_raw fast path.
                (!matches!(&reduced_p, Type::Any(_) | Type::Interface(_, _))
                    && reduced_arg.is_subtype_raw(&reduced_p, context))
                    || match_types_to_generic(context, &reduced_arg, &p)
                        .map(|bindings| !bindings.is_empty())
                        .unwrap_or(false)
            })
            .unwrap_or(false)
    });
    exact.into_iter().chain(unifiable).cloned().collect()
}

fn try_direct_match(
    candidates: &[FunctionType],
    types: &[Type],
    context: &Context,
) -> Option<FunctionType> {
    candidates
        .iter()
        .flat_map(|x| x.clone().infer_return_type_direct(types, context))
        .next()
}

fn try_vectorized_match(
    candidates: &[FunctionType],
    types: &[Type],
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
    context: &Context,
) -> Option<FunctionType> {
    all_signatures
        .iter()
        .filter(|sig| sig.is_variadic())
        .flat_map(|x| x.clone().infer_return_type(types, context))
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
    let spread_pos = params.iter().position(
        |p| matches!(p, Type::Multi(inner, _) if matches!(inner.as_ref(), Type::Generic(_, _))),
    );

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
                    subs.insert(
                        name.clone(),
                        SpreadSub::Seq(concrete[pos..spread_end].to_vec()),
                    );
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
        let all_match = param_types
            .iter()
            .zip(arg_types.iter())
            .all(|(param, arg)| {
                let reduced_param = reduce_type(context, param);
                if matches!(&reduced_param, Type::Interface(_, _)) {
                    if arg.is_subtype_raw(&reduced_param, context) {
                        if !interface_to_concrete
                            .iter()
                            .any(|(k, _)| k == &reduced_param)
                        {
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

fn build_substitution_map(
    lambda_types: &[(Type, Type)],
) -> std::collections::HashMap<String, Type> {
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
                                is_opaque: mutable_.clone(),
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
            arguments: args
                .iter()
                .map(|a| substitute_type_in_lang(a, subs))
                .collect(),
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
            arguments: args
                .iter()
                .map(|a| substitute_type_in_lang(a, subs))
                .collect(),
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
            body: exprs
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            help_data: h.clone(),
        },
        Lang::Lines {
            value: exprs,
            help_data: h,
        } => Lang::Lines {
            value: exprs
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
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
                .map(|(pat, exp): &(Lang, Box<Lang>)| {
                    (pat.clone(), Box::new(substitute_type_in_lang(exp, subs)))
                })
                .collect(),
            help_data: h.clone(),
        },
        Lang::Array {
            value: elems,
            help_data: h,
        } => Lang::Array {
            value: elems
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            help_data: h.clone(),
        },
        Lang::Vector {
            value: elems,
            help_data: h,
        } => Lang::Vector {
            value: elems
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            help_data: h.clone(),
        },
        Lang::Tuple {
            value: elems,
            help_data: h,
        } => Lang::Tuple {
            value: elems
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
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
            spreads: spreads
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
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
            body: exprs
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            module_position: pos.clone(),
            config: config.clone(),
            help_data: h.clone(),
        },
        Lang::Union(left, right, h) => Lang::Union(
            Box::new(substitute_type_in_lang(left, subs)),
            Box::new(substitute_type_in_lang(right, subs)),
            h.clone(),
        ),
        Lang::JSBlock(body, id, h) => Lang::JSBlock(
            Box::new(substitute_type_in_lang(body, subs)),
            *id,
            h.clone(),
        ),
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

fn specialize_lambda(
    lambda_lang: &Lang,
    lambda_type: &Type,
    expected_type: &Type,
    context: &Context,
) -> (Lang, Type) {
    if let (
        Type::Function(expected_params, expected_ret, _),
        Type::Function(lambda_params, lambda_ret, _),
    ) = (expected_type, lambda_type)
    {
        let mut substitutions: std::collections::HashMap<String, Type> =
            std::collections::HashMap::new();

        for (lambda_param, expected_param) in lambda_params.iter().zip(expected_params.iter()) {
            if let Type::Generic(name, _) = &lambda_param.get_type() {
                if !matches!(expected_param.get_type(), Type::Generic(_, _)) {
                    substitutions.insert(name.clone(), expected_param.get_type());
                }
            }
        }

        if let Type::Generic(name, _) = lambda_ret.as_ref() {
            if !matches!(expected_ret.as_ref(), Type::Generic(_, _)) {
                substitutions.insert(name.clone(), expected_ret.as_ref().clone());
            }
        }

        if substitutions.is_empty() {
            return (lambda_lang.clone(), lambda_type.clone());
        }

        let specialized_lang = substitute_type_in_lang(lambda_lang, &substitutions);

        let specialized_context = substitutions
            .iter()
            .fold(context.clone(), |ctx, (name, typ)| {
                let new_ctx = ctx.clone();
                new_ctx.push_var_type(Var::from_name(name), typ.clone(), &ctx)
            });

        let specialized_tc = typing(&specialized_context, &specialized_lang);
        let specialized_body_type = if let Type::Function(_, ret, _) = &specialized_tc.value {
            *ret.clone()
        } else {
            expected_ret.as_ref().clone()
        };

        let new_type = Type::Function(
            expected_params.clone(),
            Box::new(specialized_body_type),
            lambda_type.get_help_data().clone(),
        );

        return (specialized_tc.lang, new_type);
    }
    (lambda_lang.clone(), lambda_type.clone())
}

pub fn apply_from_variable(
    var: Var,
    context: &Context,
    parameters: &[Lang],
    h: &HelpData,
) -> TypeContext {
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
            .with_errors(vec![TypRError::Type(TypeError::FunctionNotFound(
                var.clone(),
            ))]);
    }
    let result = apply_from_variable_inner(var, context, parameters, h);
    DEPTH.with(|d| d.set(d.get() - 1));
    result
}

fn apply_from_variable_inner(
    var: Var,
    context: &Context,
    parameters: &[Lang],
    h: &HelpData,
) -> TypeContext {
    let (expanded_parameters, types, param_errors) =
        get_expanded_parameters_with_their_types(context, parameters);

    let all_signatures = var.get_functions_from_name(context);

    // === FILTERING 0 : Constrained rigid variable method resolution (§5 elimination) ===
    // When the receiver (first argument) is a constrained generic rigid variable,
    // resolve the method directly from the interface constraint.
    if let Some(fun_typ) = try_constrained_variable_match(&var, &types, context) {
        return build_success(
            &var,
            &fun_typ,
            expanded_parameters,
            &types,
            param_errors,
            context,
            h,
        );
    }

    // === FILTERING 0.5 : Zero-argument call to a plain (non-variadic) function ===
    // FILTERING 1/2/2.5 all key off `types.first()`, so a call like `f()` to a
    // plain `fn(): T { ... }` (no parameters, not variadic) never reaches a
    // matching filter and falls through to FunctionNotFound. Handle the
    // arity-0 case directly here.
    if types.is_empty() {
        if let Some(fun_typ) = all_signatures
            .iter()
            .find(|sig| !sig.is_variadic() && sig.get_param_types().is_empty())
            .cloned()
            .and_then(|sig| sig.infer_return_type_direct(&types, context))
        {
            return build_success(
                &var,
                &fun_typ,
                expanded_parameters,
                &types,
                param_errors,
                context,
                h,
            );
        }
    }

    // === FILTERING 1 : First equality of the first param, then complet match (unification) ===
    if let Some(first_arg_type) = types.first() {
        let candidates = filter_by_first_param(&all_signatures, first_arg_type, context);
        if !candidates.is_empty() {
            if let Some(fun_typ) = try_direct_match(&candidates, &types, context) {
                let (final_params, final_types) =
                    specialize_lambdas(context, &expanded_parameters, &types, &fun_typ);
                return build_success(
                    &var,
                    &fun_typ,
                    final_params,
                    &final_types,
                    param_errors,
                    context,
                    h,
                );
            }
        }
    }

    // === FILTERING 2 : Super-type of the first arg, one by one from the closest one ===
    if let Some(first_arg_type) = types.first() {
        let super_types = context
            .subtypes
            .get_ordered_supertypes(first_arg_type, context);
        for super_type in &super_types {
            // Interface super-types are handled by FILTERING 2.5 with proper
            // return-type inference (universal generic semantics). Skip them here.
            if matches!(reduce_type(context, super_type), Type::Interface(_, _)) {
                continue;
            }
            let candidates = filter_by_first_param(&all_signatures, super_type, context);
            if !candidates.is_empty() {
                if let Some(fun_typ) = try_direct_match(&candidates, &types, context) {
                    let (final_params, final_types) =
                        specialize_lambdas(context, &expanded_parameters, &types, &fun_typ);
                    return build_success(
                        &var,
                        &fun_typ,
                        final_params,
                        &final_types,
                        param_errors,
                        context,
                        h,
                    );
                }
            }
        }
    }

    // === FILTERING 2.5 : Interface structural subtyping ===
    // Handles the case where parameters are interface types and arguments satisfy them
    // structurally. When the return type is the same interface as a matched parameter,
    // it is substituted with the concrete argument type (universal generic semantics:
    // fn(i: I): I desugars to forall A: I. A -> A).
    if let Some(first_arg_type) = types.first() {
        let interface_candidates: Vec<FunctionType> = all_signatures
            .iter()
            .filter(|sig| {
                sig.get_first_param()
                    .map(|p| {
                        let reduced_param = reduce_type(context, &p);
                        matches!(&reduced_param, Type::Interface(_, _))
                            && first_arg_type.is_subtype_raw(&reduced_param, context)
                    })
                    .unwrap_or(false)
            })
            .cloned()
            .collect();
        if !interface_candidates.is_empty() {
            if let Some(fun_typ) =
                try_interface_subtype_match(&interface_candidates, &types, context)
            {
                let (final_params, final_types) =
                    specialize_lambdas(context, &expanded_parameters, &types, &fun_typ);
                return build_success(
                    &var,
                    &fun_typ,
                    final_params,
                    &final_types,
                    param_errors,
                    context,
                    h,
                );
            }
        }
    }

    // === FILTERING 3 : Vectorization ===
    if let Some(fun_typ) = try_vectorized_match(&all_signatures, &types, context) {
        let (final_params, final_types) =
            specialize_lambdas(context, &expanded_parameters, &types, &fun_typ);
        return build_success(
            &var,
            &fun_typ,
            final_params,
            &final_types,
            param_errors,
            context,
            h,
        );
    }

    // === FILTERING 4 : Variadic function (handles 0-arg calls and arity mismatch) ===
    if let Some(fun_typ) = try_variadic_match(&all_signatures, &types, context) {
        let (final_params, final_types) =
            specialize_lambdas(context, &expanded_parameters, &types, &fun_typ);
        return build_success(
            &var,
            &fun_typ,
            final_params,
            &final_types,
            param_errors,
            context,
            h,
        );
    }

    // === FILTERING 5 : Tuple spread (Ts...) ===
    if let Some(fun_typ) = try_spread_tuple_match(&all_signatures, &types, context) {
        let (final_params, final_types) =
            specialize_lambdas(context, &expanded_parameters, &types, &fun_typ);
        return build_success(
            &var,
            &fun_typ,
            final_params,
            &final_types,
            param_errors,
            context,
            h,
        );
    }

    // === ERROR : no filtering worked ===
    let mut errors = param_errors;
    errors.push(TypRError::Type(TypeError::FunctionNotFound(
        var.clone().set_type_from_params(parameters, context),
    )));
    TypeContext::new(builder::any_type(), Lang::Empty(h.clone()), context.clone())
        .with_errors(errors)
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
                let (specialized_lang, specialized_type) =
                    specialize_lambda(param, param_type, expected_type, context);
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
) -> (Vec<Lang>, Vec<Type>, Vec<TypRError>) {
    let typing_contexts: Vec<_> = values.iter().map(|x| typing(context, x)).collect();
    let errors: Vec<TypRError> = typing_contexts
        .iter()
        .flat_map(|tc| tc.errors.clone())
        .collect();
    let types = typing_contexts
        .iter()
        .cloned()
        .map(|x| x.value)
        .collect::<Vec<_>>();
    let new_values = typing_contexts
        .iter()
        .cloned()
        .map(|x| x.lang)
        .collect::<Vec<_>>();
    (new_values, types, errors)
}

fn build_function_lang(
    h: &HelpData,
    new_values: Vec<Lang>,
    fun_typ: &FunctionType,
    lang: Lang,
) -> Lang {
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

pub fn apply_from_expression(
    context: &Context,
    fn_var_name: &Lang,
    values: &[Lang],
    h: &HelpData,
) -> TypeContext {
    // Collect errors from parameters but return Any type
    let (_expanded_parameters, _types, param_errors) =
        get_expanded_parameters_with_their_types(context, values);
    let mut errors = param_errors;
    errors.push(TypRError::Type(TypeError::WrongExpression(h.clone())));
    TypeContext::new(builder::any_type(), Lang::Empty(h.clone()), context.clone())
        .with_errors(errors)
}

pub fn function_application(
    context: &Context,
    fn_var_name: &Lang,
    values: &[Lang],
    h: &HelpData,
) -> TypeContext {
    match Var::try_from(fn_var_name.clone()) {
        Ok(var) => apply_from_variable(var, context, values, h),
        _ => apply_from_expression(context, fn_var_name, values, h),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::builder;
    use crate::utils::fluent_parser::FluentParser;

    #[test]
    fn test_vectorization0() {
        let res = FluentParser::new()
            .push("@f1: (int) -> int;")
            .run()
            .check_typing("f1([1, 2])");
        let fun_typ =
            builder::array_type(builder::integer_type(2), builder::integer_type_default());
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
        let expected =
            builder::array_type(builder::integer_type(4), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    #[test]
    fn test_union_litteral2() {
        let res = FluentParser::new()
            .push("let f3 <- fn(a: \"html\" | \"h1\"): char { \"hello\" };")
            .run()
            .check_transpiling("f3(\"h1\")");
        assert!(true);
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
        let expected =
            builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Binary function: scalar + [2, int] (right lifting) ---
    #[test]
    fn test_lift_add_scalar_left_array_right() {
        let res = FluentParser::new()
            .push("@add: (int, int) -> int;")
            .run()
            .check_typing("add(1, [1, 2])");
        let expected =
            builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Binary function: [2, int] + [2, int] (both arrays) ---
    #[test]
    fn test_lift_add_array_both() {
        let res = FluentParser::new()
            .push("@add: (int, int) -> int;")
            .run()
            .check_typing("add([1, 2], [3, 4])");
        let expected =
            builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Binary function: larger arrays [5, int] + scalar ---
    #[test]
    fn test_lift_add_array_larger() {
        let res = FluentParser::new()
            .push("@add: (int, int) -> int;")
            .run()
            .check_typing("add([1, 2, 3, 4, 5], 1)");
        let expected =
            builder::array_type(builder::integer_type(5), builder::integer_type_default());
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
        let expected =
            builder::array_type(builder::integer_type(3), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Multiplication: scalar * [3, int] (right lifting) ---
    #[test]
    fn test_lift_mul_scalar_left_array_right() {
        let res = FluentParser::new()
            .push("@mul: (int, int) -> int;")
            .run()
            .check_typing("mul(2, [1, 2, 3])");
        let expected =
            builder::array_type(builder::integer_type(3), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Division: [2, int] / [2, int] (both arrays) ---
    #[test]
    fn test_lift_div_array_both() {
        let res = FluentParser::new()
            .push("@div: (int, int) -> int;")
            .run()
            .check_typing("div([10, 20], [2, 4])");
        let expected =
            builder::array_type(builder::integer_type(2), builder::integer_type_default());
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
        let expected =
            builder::array_type(builder::integer_type(2), builder::integer_type_default());
        assert_eq!(res, expected);
    }

    // --- Operator syntax: scalar * [3, int] (via `*`, right lifting) ---
    #[test]
    fn test_operator_mul_scalar_left_array_right() {
        let res = FluentParser::new()
            .push("@`*`: (int, int) -> int;")
            .run()
            .check_typing("2 * [1, 2, 3]");
        let expected =
            builder::array_type(builder::integer_type(3), builder::integer_type_default());
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
        assert!(
            code.contains("vec_apply"),
            "Expected vec_apply in transpiled code, got: {}",
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
}
