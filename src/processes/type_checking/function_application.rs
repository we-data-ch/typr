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

pub fn apply_from_variable(var: Var, context: &Context, fn_var_name: &Box<Lang>, values: &Vec<Lang>, t: &Type, h: &HelpData) -> TypeContext {
    let name = var.get_name();
    let new_values = values.iter().map(|x| typing(context, x).lang).collect::<Vec<_>>();
    let funs = var.get_function_signatures(values, context);
    let (id, typ) = funs.iter()
        .enumerate()
        .find_map(|(id, x)| 
                  match x.clone().infer_return_type(values, context, &name) {
                      Some(res) => Some((id, res)),
                      _ => None})
        .unwrap();
    let old_ret_typ = funs[id].get_return_type();
    let new_expr = if typ.is_vector_of(&old_ret_typ, context) {
       Lang::VecFunctionApp(fn_var_name.clone(), new_values.clone(), t.clone(), h.clone()) 
    } else { 
       Lang::FunctionApp(fn_var_name.clone(), new_values.clone(), t.clone(), h.clone())
    };

    let (typ, new_context) = typ
        .tuple(&context.clone());
    (typ, new_expr, new_context).into()
}

pub fn apply_from_expression(context: &Context, fn_var_name: &Box<Lang>, values: &Vec<Lang>, t: &Type, h: &HelpData) -> TypeContext {
    todo!();
}

pub fn function_application(context: &Context, fn_var_name: &Box<Lang>, values: &Vec<Lang>, t: &Type, h: &HelpData) -> TypeContext {
    match Var::try_from(fn_var_name.clone()) {
        Ok(var) => apply_from_variable(var, context, fn_var_name, values, t, h),
        _ => apply_from_expression(context, fn_var_name, values, t, h)
    }
}
