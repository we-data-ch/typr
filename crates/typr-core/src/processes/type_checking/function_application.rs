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
use crate::components::r#type::function_type::FunctionType;
use crate::processes::type_checking::typing;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::Type;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::TypeError;
use crate::processes::type_checking::Var;
use crate::utils::builder;

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

fn filter_by_first_param(signatures: &[FunctionType], first_arg_type: &Type) -> Vec<FunctionType> {
    signatures
        .iter()
        .filter(|sig| sig.get_first_param().as_ref() == Some(first_arg_type))
        .cloned()
        .collect()
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

fn get_generic_params_from_type(typ: &Type) -> Vec<(String, Type)> {
    match typ {
        Type::Function(params, ret, _) => {
            let mut result = Vec::new();
            for (i, param) in params.iter().enumerate() {
                if let Type::Generic(name, _) = param {
                    result.push((name.clone(), param.clone()));
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
            params.iter().map(|p| substitute_type(p, subs)).collect(),
            Box::new(substitute_type(ret, subs)),
            h.clone(),
        ),
        _ => typ.clone(),
    }
}

fn substitute_type_in_lang(lang: &Lang, subs: &std::collections::HashMap<String, Type>) -> Lang {
    match lang {
        Lang::Variable(name, mutable_, _, h) => {
            if let Some(new_type) = subs.get(name) {
                Lang::Variable(name.clone(), *mutable_, new_type.clone(), h.clone())
            } else {
                lang.clone()
            }
        }
        Lang::Lambda(params, body, h) => {
            let new_params: Vec<Lang> = params
                .iter()
                .map(|p| {
                    if let Lang::Variable(name, mutable_, _, h2) = p {
                        if let Some(new_type) = subs.get(name) {
                            Lang::Variable(name.clone(), *mutable_, new_type.clone(), h2.clone())
                        } else {
                            p.clone()
                        }
                    } else {
                        p.clone()
                    }
                })
                .collect();
            let new_body = substitute_type_in_lang(body, subs);
            Lang::Lambda(new_params, Box::new(new_body), h.clone())
        }
        Lang::Function(args, ret, body, h) => {
            let new_ret = substitute_type(ret, subs);
            let new_body = substitute_type_in_lang(body, subs);
            Lang::Function(args.clone(), new_ret, Box::new(new_body), h.clone())
        }
        Lang::FunctionApp(func, args, h) => Lang::FunctionApp(
            Box::new(substitute_type_in_lang(func, subs)),
            args.iter()
                .map(|a| substitute_type_in_lang(a, subs))
                .collect(),
            h.clone(),
        ),
        Lang::VecFunctionApp(func, args, h) => Lang::VecFunctionApp(
            Box::new(substitute_type_in_lang(func, subs)),
            args.iter()
                .map(|a| substitute_type_in_lang(a, subs))
                .collect(),
            h.clone(),
        ),
        Lang::Operator(op, e1, e2, h) => Lang::Operator(
            op.clone(),
            Box::new(substitute_type_in_lang(e1, subs)),
            Box::new(substitute_type_in_lang(e2, subs)),
            h.clone(),
        ),
        Lang::Return(exp, h) => {
            Lang::Return(Box::new(substitute_type_in_lang(exp, subs)), h.clone())
        }
        Lang::Let(name, typ, val, h) => Lang::Let(
            name.clone(),
            substitute_type(typ, subs),
            Box::new(substitute_type_in_lang(val, subs)),
            h.clone(),
        ),
        Lang::If(cond, then, else_, h) => Lang::If(
            Box::new(substitute_type_in_lang(cond, subs)),
            Box::new(substitute_type_in_lang(then, subs)),
            Box::new(substitute_type_in_lang(else_, subs)),
            h.clone(),
        ),
        Lang::Not(exp, h) => Lang::Not(Box::new(substitute_type_in_lang(exp, subs)), h.clone()),
        Lang::Scope(exprs, h) => Lang::Scope(
            exprs
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            h.clone(),
        ),
        Lang::Lines(exprs, h) => Lang::Lines(
            exprs
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            h.clone(),
        ),
        Lang::Assign(target, value, h) => Lang::Assign(
            Box::new(substitute_type_in_lang(target, subs)),
            Box::new(substitute_type_in_lang(value, subs)),
            h.clone(),
        ),
        Lang::ForLoop(var, iter, body, h) => Lang::ForLoop(
            var.clone(),
            Box::new(substitute_type_in_lang(iter, subs)),
            Box::new(substitute_type_in_lang(body, subs)),
            h.clone(),
        ),
        Lang::WhileLoop(cond, body, h) => Lang::WhileLoop(
            Box::new(substitute_type_in_lang(cond, subs)),
            Box::new(substitute_type_in_lang(body, subs)),
            h.clone(),
        ),
        Lang::Match(exp, branches, h) => Lang::Match(
            Box::new(substitute_type_in_lang(exp, subs)),
            branches
                .iter()
                .map(|(pat, exp)| (pat.clone(), Box::new(substitute_type_in_lang(exp, subs))))
                .collect(),
            h.clone(),
        ),
        Lang::Array(elems, h) => Lang::Array(
            elems
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            h.clone(),
        ),
        Lang::Vector(elems, h) => Lang::Vector(
            elems
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            h.clone(),
        ),
        Lang::Tuple(elems, h) => Lang::Tuple(
            elems
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            h.clone(),
        ),
        Lang::ArrayIndexing(arr, idx, h) => Lang::ArrayIndexing(
            Box::new(substitute_type_in_lang(arr, subs)),
            Box::new(substitute_type_in_lang(idx, subs)),
            h.clone(),
        ),
        Lang::Tag(name, inner, h) => Lang::Tag(
            name.clone(),
            Box::new(substitute_type_in_lang(inner, subs)),
            h.clone(),
        ),
        Lang::List(fields, h) => Lang::List(fields.clone(), h.clone()),
        Lang::Module(name, exprs, pos, config, h) => Lang::Module(
            name.clone(),
            exprs
                .iter()
                .map(|e| substitute_type_in_lang(e, subs))
                .collect(),
            pos.clone(),
            config.clone(),
            h.clone(),
        ),
        Lang::ModuleDecl(name, h) => lang.clone(),
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
        Lang::VecBlock(_, h) => lang.clone(),
        Lang::RFunction(_, _, h) => lang.clone(),
        Lang::Alias(_, _, _, h) => lang.clone(),
        Lang::Signature(_, _, h) => lang.clone(),
        Lang::Library(_, h) => lang.clone(),
        Lang::TypePattern(_, _, h) => lang.clone(),
        Lang::TestBlock(_, h) => lang.clone(),
        Lang::Exp(_, h) => lang.clone(),
        Lang::Break(h) => Lang::Break(h.clone()),
        Lang::Empty(h) => Lang::Empty(h.clone()),
        Lang::Number(_, h) => lang.clone(),
        Lang::Integer(_, h) => lang.clone(),
        Lang::Bool(_, h) => lang.clone(),
        Lang::Char(_, h) => lang.clone(),
        Lang::Null(h) => lang.clone(),
        Lang::NA(h) => lang.clone(),
        Lang::SyntaxErr(_, _) => lang.clone(),
        Lang::Comment(_, h) => lang.clone(),
        Lang::ModuleImport(_, h) => lang.clone(),
        Lang::Import(_, h) => lang.clone(),
        Lang::GenFunc(_, _, h) => lang.clone(),
        Lang::Test(_, h) => lang.clone(),
        Lang::KeyValue(_, _, h) => lang.clone(),
        Lang::Sequence(_, h) => lang.clone(),
        Lang::MethodCall(_, _, _, h) => lang.clone(),
        Lang::Use(_, _, h) => lang.clone(),
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
            if let Type::Generic(name, _) = lambda_param {
                if !matches!(expected_param, Type::Generic(_, _)) {
                    substitutions.insert(name.clone(), expected_param.clone());
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
        let new_type = Type::Function(
            expected_params.clone(),
            expected_ret.clone(),
            lambda_type.get_help_data().clone(),
        );

        let specialized_tc = typing(context, &specialized_lang);
        return (specialized_tc.lang, specialized_tc.value);
    }
    (lambda_lang.clone(), lambda_type.clone())
}

pub fn apply_from_variable(
    var: Var,
    context: &Context,
    parameters: &Vec<Lang>,
    h: &HelpData,
) -> TypeContext {
    let (expanded_parameters, types, param_errors) =
        get_expanded_parameters_with_their_types(context, parameters);

    let all_signatures = var.get_functions_from_name(context);

    // === FILTRAGE 1 : Égalité stricte du 1er param, puis match complet (unification) ===
    if let Some(first_arg_type) = types.first() {
        let candidates = filter_by_first_param(&all_signatures, first_arg_type);
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

    // === FILTRAGE 2 : Super-types du 1er arg, un par un du plus proche ===
    if let Some(first_arg_type) = types.first() {
        let super_types = context
            .subtypes
            .get_ordered_supertypes(first_arg_type, context);
        for super_type in &super_types {
            let candidates = filter_by_first_param(&all_signatures, super_type);
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

    // === FILTRAGE 3 : Vectorisation ===
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

    // === ERREUR : aucun filtrage n'a marché ===
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
        if let Lang::Lambda(_, _, _) = param {
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
    values: &Vec<Lang>,
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
    let new_expr = if fun_typ.is_vectorized() {
        Lang::VecFunctionApp(Box::new(lang), new_values.clone(), h.clone())
    } else {
        Lang::FunctionApp(Box::new(lang), new_values.clone(), h.clone())
    };
    new_expr
}

pub fn apply_from_expression(
    context: &Context,
    fn_var_name: &Box<Lang>,
    values: &Vec<Lang>,
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
    fn_var_name: &Box<Lang>,
    values: &Vec<Lang>,
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
}
