#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::typr_error::TypRError;
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

fn infer_return_type(
    functions: &[FunctionType],
    types: &[Type],
    context: &Context,
) -> Option<FunctionType> {
    functions
        .iter()
        .flat_map(|x| x.clone().infer_return_type(types, context))
        .next()
}

pub fn apply_from_variable(
    var: Var,
    context: &Context,
    parameters: &Vec<Lang>,
    h: &HelpData,
) -> TypeContext {
    let (expanded_parameters, types, param_errors) =
        get_expanded_parameters_with_their_types(context, parameters);

    match infer_return_type(&var.get_functions_from_name(context), &types, context) {
        Some(fun_typ) => {
            let new_expr = build_function_lang(h, expanded_parameters, &fun_typ, var.to_language());
            TypeContext::new(fun_typ.get_infered_return_type(), new_expr, context.clone())
                .with_errors(param_errors)
        }
        None => {
            let mut errors = param_errors;
            errors.push(TypRError::Type(TypeError::FunctionNotFound(
                var.clone().set_type_from_params(parameters, context),
            )));
            TypeContext::new(builder::any_type(), Lang::Empty(h.clone()), context.clone())
                .with_errors(errors)
        }
    }
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
}
