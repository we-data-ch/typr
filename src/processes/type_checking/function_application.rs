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
            errors.push(TypRError::Type(TypeError::FunctionNotFound(var.clone())));
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
    fn test_vectorization1() {
        let res = FluentParser::new()
            .push("@f2: (int, int) -> int;")
            .run()
            .check_typing("f2(3, [1, 2])");
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
    fn test_union_litteral2() {
        let res = FluentParser::new()
            .push("let f3 <- fn(a: \"html\" | \"h1\"): char { \"hello\" };")
            .run()
            .check_transpiling("f3(\"h1\")");
        assert!(true);
    }
}
