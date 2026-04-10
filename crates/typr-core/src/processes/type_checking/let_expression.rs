use crate::processes::type_checking::r#Type;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::Var;

#[allow(clippy::borrowed_box)]
pub fn let_expression(
    context: &Context,
    name: &Box<Lang>,
    ty: &Type,
    exp: &Box<Lang>,
    h: &HelpData,
) -> TypeContext {
    let new_context = context
        .clone()
        .push_types(&exp.extract_types_from_expression(context));

    let res = exp
        .typing(&new_context)
        .get_covariant_type(ty)
        .add_to_context(Var::try_from(name).unwrap());

    let new_expr = Lang::Let {
        variable: name.clone(),
        r#type: ty.clone(),
        expression: Box::new(res.get_expr()),
        help_data: h.clone(),
    };

    res.with_lang(&new_expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::builder;
    use crate::utils::fluent_parser::FluentParser;

    #[test]
    fn test_let_expression0() {
        let res = FluentParser::new()
            .push("let a <- 5;")
            .run()
            .push("a")
            .type_next()
            .get_last_type();
        assert_eq!(res, builder::integer_type_default());
    }
}
