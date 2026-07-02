use crate::processes::type_checking::r#Type;
use crate::processes::type_checking::type_arithmetic::validate_operator_kinds;
use crate::processes::type_checking::validate_named_constructors;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::FunctionType;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::Var;
use crate::utils::builder;

pub fn signature_expression(
    context: &Context,
    expr: &Lang,
    var: &Var,
    typ: &Type,
    is_extern: bool,
    extern_r_name: Option<String>,
) -> TypeContext {
    let mut constructor_errors = validate_named_constructors(context, typ);
    constructor_errors.extend(validate_operator_kinds(context, typ));
    let tc: TypeContext = if var.is_variable() {
        let new_var = FunctionType::try_from(typ.clone())
            .map(|ft| {
                var.clone().set_type(
                    ft.get_first_param()
                        .unwrap_or(builder::unknown_function_type()),
                )
            })
            .unwrap_or(var.clone());
        let mut new_context =
            context
                .clone()
                .replace_or_push_var_type(new_var, typ.to_owned(), context);
        if is_extern {
            new_context.extern_fns.push((var.get_name(), extern_r_name));
        }
        (builder::unknown_function_type(), expr.clone(), new_context).into()
    } else {
        // is alias
        let mut new_context =
            context
                .clone()
                .replace_or_push_var_type(var.to_owned(), typ.to_owned(), context);
        if is_extern {
            new_context.extern_fns.push((var.get_name(), extern_r_name));
        }
        (builder::unknown_function_type(), expr.clone(), new_context).into()
    };
    tc.with_errors(constructor_errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::language::Lang;
    use crate::processes::type_checking::HelpData;
    use crate::utils::fluent_parser::FluentParser;

    #[test]
    fn test_signature_expression0() {
        let res = FluentParser::new()
            .push("@a: int;")
            .run()
            .push("a")
            .run()
            .get_last_type();
        assert_eq!(res, builder::integer_type_default());
    }

    #[test]
    fn test_signature_expression_is_parsing() {
        let res = FluentParser::new().check_parsing("@a: int;");
        let res = res.first().unwrap();
        let integer = builder::integer_type_default();
        let var = Var::from_name("a").set_type(integer.clone());
        assert_eq!(
            res,
            &Lang::Signature {
                identifier: var,
                target_type: integer,
                help_data: HelpData::default(),
                is_extern: false,
                extern_r_name: None,
            }
        );
    }

    #[test]
    fn test_signature_expression_is_transpiling() {
        let res = FluentParser::new().check_transpiling("@a: int;");
        assert_eq!(res.first().unwrap(), "");
    }

    #[test]
    fn test_signature_expression_saved() {
        let res = FluentParser::new().push("@a: int;").run().check_typing("a");
        assert_eq!(res, builder::integer_type_default())
    }

    #[test]
    fn test_signature_expression() {
        let res = FluentParser::new().push("@a: int;").run().check_typing("a");
        assert_eq!(res, builder::integer_type_default())
    }

    #[test]
    fn test_signature_function1() {
        let res = FluentParser::new()
            .push("@f: (int) -> int;")
            .run()
            .check_typing("f");
        let integer = builder::integer_type_default();
        assert_eq!(res, builder::function_type(&[integer.clone()], integer))
    }

    #[test]
    fn test_signature_function2() {
        let res = FluentParser::new()
            .push("@f: (int) -> bool;")
            .run()
            .check_typing("f");
        let integer = builder::integer_type_default();
        let boolean = builder::boolean_type();
        assert_eq!(res, builder::function_type(&[integer.clone()], boolean))
    }

    #[test]
    fn test_signature_function_application1() {
        let res = FluentParser::new()
            .push("@f: (int) -> bool;")
            .run()
            .check_typing("f(3)");
        let boolean = builder::boolean_type();
        assert_eq!(res, boolean)
    }
}
