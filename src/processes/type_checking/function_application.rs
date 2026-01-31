#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::processes::type_checking::Context;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::Type;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::Var;
use crate::processes::type_checking::typing;

//get function signature 
//get parameters signature
// do unification and get return type

pub fn apply_from_variable(var: Var, context: &Context, values: &Vec<Lang>, h: &HelpData) -> TypeContext {
    let new_values = values.iter().map(|x| typing(context, x).lang).collect::<Vec<_>>();
    let ret_typ = var.get_function_signatures(values, context)
        .and_then(|x| x.infer_return_type(values, context, &var.get_name()))
        .unwrap();
    //let new_expr = if typ.is_vector_of(&fun, context) {
       //Lang::VecFunctionApp(fn_var_name.clone(), new_values.clone(), t.clone(), h.clone()) 
    //} else { 
       //Lang::FunctionApp(fn_var_name.clone(), new_values.clone(), t.clone(), h.clone())
    //};
//
    //let (typ, new_context) = typ
        //.tuple(&context.clone());
    //(typ, new_expr, new_context).into()
    todo!()
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
