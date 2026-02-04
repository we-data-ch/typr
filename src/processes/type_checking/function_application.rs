#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::r#type::function_type::FunctionType;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::typing;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::Type;
use crate::processes::type_checking::Var;

fn infer_return_type(functions: &[FunctionType], types: &[Type], context: &Context) 
    -> Option<FunctionType> {
        functions.iter()
            .flat_map(|x| x.clone().infer_return_type(types, context))
            .next()
}

pub fn apply_from_variable(var: Var, context: &Context, parameters: &Vec<Lang>, h: &HelpData) -> TypeContext {
    let (expanded_parameters, types) = 
        get_expanded_parameters_with_their_types(context, parameters);
    let fun_typ = Some(var.get_functions_from_name(context))
        .and_then(|functions| infer_return_type(&functions, &types, context))
        .unwrap(); // Todo: write an error message here
    let new_expr = build_function_lang(h, expanded_parameters, &fun_typ, var.to_language());
    (fun_typ.get_infered_return_type(), new_expr, context.clone()).into()
}

fn get_expanded_parameters_with_their_types(context: &Context, values: &Vec<Lang>) -> (Vec<Lang>, Vec<Type>) {
    let typing_contexts = values.iter().map(|x| typing(context, x)).collect::<Vec<_>>();
    let types = typing_contexts.iter().cloned().map(|x| x.value).collect::<Vec<_>>();
    let new_values = typing_contexts.iter().cloned().map(|x| x.lang).collect::<Vec<_>>();
    (new_values, types)
}

fn build_function_lang(h: &HelpData, new_values: Vec<Lang>, fun_typ: &FunctionType, lang: Lang) -> Lang {
    let new_expr = if fun_typ.is_vectorized() {
       Lang::VecFunctionApp(Box::new(lang), new_values.clone(), h.clone()) 
    } else { 
       Lang::FunctionApp(Box::new(lang), new_values.clone(), h.clone())
    };
    new_expr
}

pub fn apply_from_expression(context: &Context, fn_var_name: &Box<Lang>, values: &Vec<Lang>, h: &HelpData) -> TypeContext {
    todo!();
}

pub fn function_application(context: &Context, fn_var_name: &Box<Lang>, values: &Vec<Lang>, h: &HelpData) -> TypeContext {
    match Var::try_from(fn_var_name.clone()) {
        Ok(var) => apply_from_variable(var, context, values, h),
        _ => apply_from_expression(context, fn_var_name, values, h)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::fluent_parser::FluentParser;
    use crate::utils::builder;

    #[test]
    fn test_vectorization0(){
       let res = FluentParser::new()
           .push("@f1: (int) -> int;").run()
           .check_typing("f1([1, 2])");
       let fun_typ = builder::array_type(
           builder::integer_type(2),
           builder::integer_type_default());
       assert_eq!(res, fun_typ);
    }

}
