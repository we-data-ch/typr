use crate::processes::type_checking::FunctionType;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::r#Type;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::Var;
use crate::utils::builder;



pub fn signature_expression(context: &Context, expr: &Lang, var: &Var, typ: &Type) -> TypeContext {
    if var.is_variable(){
        let new_var = FunctionType::try_from(typ.clone())
                    .map(|ft| var.clone().set_type(ft.get_first_param().unwrap_or(builder::unknown_function_type())))
                    .unwrap_or(var.clone());
        (builder::unknown_function_type(), expr.clone(),
        context.clone().replace_or_push_var_type(new_var, typ.to_owned(), context)).into()
    } else { // is alias
        (builder::unknown_function_type(), expr.clone(),
                context.clone()
                    .replace_or_push_var_type(var.to_owned(), typ.to_owned(), context)).into()
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::fluent_parser::FluentParser;
    use crate::processes::type_checking::HelpData;
    use crate::components::language::Lang;
    use super::*;

    #[test]
    fn test_signature_expression0(){
       let res = FluentParser::new() 
           .push("@a: int;").run()
           .push("a").run()
           .get_last_type();
       assert_eq!(res, builder::integer_type_default());
    }

    #[test]
    fn test_signature_expression1(){
       let res = FluentParser::new() 
           .push("@a: int;").parse_next()
           .get_code();

       let res = res.first().unwrap();

       let integer = builder::integer_type_default();
       let var = Var::from_name("a").set_type(integer.clone());

       //assert_eq!(res, 
                  //&Lang::Signature(var, integer, HelpData::default()));
    }

}
