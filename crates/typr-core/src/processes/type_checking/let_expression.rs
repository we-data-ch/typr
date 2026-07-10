use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::r#type::type_operator::TypeOperator;
use crate::processes::type_checking::r#Type;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::Var;

pub fn collect_undefined_aliases(context: &Context, ty: &Type) -> Vec<TypRError> {
    match ty {
        Type::Alias(name, params, is_opaque, h) => {
            let is_builtin = matches!(
                name.as_str(),
                "Integer" | "Character" | "Boolean" | "Number"
            );
            let is_defined = Var::from_type(ty.clone())
                .and_then(|var| context.get_matching_alias_signature(&var))
                .is_some();

            let mut errors = Vec::new();
            if !is_opaque && !is_builtin && !is_defined {
                errors.push(match context.find_alias_source_module(name) {
                    Some(module_name) => TypRError::type_error(TypeError::AliasNotImported(
                        name.clone(),
                        module_name,
                        h.clone(),
                    )),
                    None => TypRError::type_error(TypeError::AliasNotFound(ty.clone())),
                });
            }
            for param in params {
                errors.extend(collect_undefined_aliases(context, param));
            }
            errors
        }
        Type::Operator(TypeOperator::Access, t1, t2, _) => {
            let module_name = match t1.as_ref() {
                Type::Variable(name, _) | Type::Alias(name, _, _, _) => Some(name.as_str()),
                _ => None,
            };
            let alias_name = match t2.as_ref() {
                Type::Alias(name, _, _, _) => Some(name.as_str()),
                _ => None,
            };
            match (module_name, alias_name) {
                (Some(module_name), Some(alias_name)) => {
                    let module_type = context
                        .get_type_from_variable(&Var::from_name(module_name))
                        .ok()
                        .and_then(|t| t.to_module_type().ok());
                    match module_type {
                        None => vec![TypRError::type_error(TypeError::AliasNotFound(ty.clone()))],
                        Some(module_type) => {
                            if module_type.get_type_from_name(alias_name).is_err() {
                                vec![TypRError::type_error(TypeError::AliasNotFound(ty.clone()))]
                            } else {
                                vec![]
                            }
                        }
                    }
                }
                _ => vec![],
            }
        }
        Type::Function(args, ret, _) => {
            let mut errors: Vec<TypRError> = args
                .iter()
                .flat_map(|arg| collect_undefined_aliases(context, &arg.get_type()))
                .collect();
            errors.extend(collect_undefined_aliases(context, ret));
            errors
        }
        Type::Vec(_, ind, inner, _) => {
            let mut errors = collect_undefined_aliases(context, ind);
            errors.extend(collect_undefined_aliases(context, inner));
            errors
        }
        Type::Record(fields, _) => fields
            .iter()
            .flat_map(|arg| collect_undefined_aliases(context, &arg.get_type()))
            .collect(),
        Type::Tag(_, inner, _) => collect_undefined_aliases(context, inner),
        _ => Vec::new(),
    }
}

#[allow(clippy::borrowed_box)]
pub fn let_expression(
    context: &Context,
    name: &Box<Lang>,
    ty: &Type,
    exp: &Box<Lang>,
    is_public: bool,
    is_testable: bool,
    is_export: bool,
    h: &HelpData,
) -> TypeContext {
    let new_context = context
        .clone()
        .push_types(&exp.extract_types_from_expression(context));

    // Pre-inject the function's own type so the body can call it recursively.
    let new_context = if let Lang::Function {
        parameters,
        return_type,
        help_data,
        ..
    } = exp.as_ref()
    {
        if !matches!(return_type, Type::Empty(_)) {
            if let Ok(var) = Var::try_from(name) {
                let fn_type = Type::Function(
                    parameters.to_vec(),
                    Box::new(return_type.clone()),
                    help_data.clone(),
                );
                new_context
                    .clone()
                    .push_var_type(var.set_type(fn_type.clone()), fn_type, context)
            } else {
                new_context
            }
        } else {
            new_context
        }
    } else {
        new_context
    };

    let mut alias_errors = collect_undefined_aliases(context, ty);

    // E-EMBED-003 (named type embedding, `embedding.rs`): an explicit function
    // can only collide with an inherited embedded method by being declared after
    // the embedding type alias (a type must exist before anything can reference
    // it, so the reverse order can't happen — see type_embedding.md §12.4).
    if let Lang::Function { parameters, .. } = exp.as_ref() {
        if let (Some(first_param), Ok(fn_var)) = (parameters.first(), Var::try_from(name)) {
            if let Type::Alias(type_name, _, _, _) = first_param.get_type() {
                let fn_name = fn_var.get_name();
                if let Some(field_name) = context.get_embedded_method(&type_name, &fn_name) {
                    alias_errors.push(TypRError::type_error(TypeError::EmbedConflict(
                        fn_name,
                        type_name,
                        field_name,
                        h.clone(),
                    )));
                }
            }
        }
    }

    let res = exp
        .typing(&new_context)
        .get_covariant_type(ty)
        .add_to_context(Var::try_from(name).unwrap());

    let new_expr = Lang::Let {
        variable: name.clone(),
        r#type: ty.clone(),
        expression: Box::new(res.get_expr()),
        is_public,
        is_testable,
        is_export,
        help_data: h.clone(),
    };

    res.with_lang(&new_expr).with_errors(alias_errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::error_message::typr_error::TypRError;
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

    #[test]
    fn test_let_undefined_type_annotation_produces_error() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::typing_with_errors;

        let expr = parse2("let v: Hey <- false;".into()).unwrap();
        let result = typing_with_errors(&Context::default(), &expr);

        assert!(
            result.has_errors(),
            "Expected an AliasNotFound error for undefined type 'Hey'"
        );
        assert!(
            result.get_errors().iter().any(|e| matches!(
                e,
                TypRError::Type(
                    crate::components::error_message::type_error::TypeError::AliasNotFound(_)
                )
            )),
            "Expected AliasNotFound error but got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn test_let_unimported_module_alias_produces_alias_not_imported_error() {
        use crate::processes::parsing::parse_from_string;
        use crate::processes::type_checking::typing_with_errors;

        let fp = FluentParser::new()
            .push("module geo { @pub type Meters <- int; };")
            .parse_type_next();
        let context = fp.context.clone();

        let expr = parse_from_string("let y: Meters <- 5;", "test");
        let result = typing_with_errors(&context, &expr);

        assert!(
            result.has_errors(),
            "Expected an AliasNotImported error for unimported alias 'Meters'"
        );
        assert!(
            result.get_errors().iter().any(|e| matches!(
                e,
                TypRError::Type(
                    crate::components::error_message::type_error::TypeError::AliasNotImported(
                        name, module, _
                    )
                ) if name == "Meters" && module == "geo"
            )),
            "Expected AliasNotImported error but got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn test_function_signature_unimported_module_alias_produces_alias_not_imported_error() {
        use crate::processes::parsing::parse_from_string;
        use crate::processes::type_checking::typing_with_errors;

        let fp = FluentParser::new()
            .push("module geo { @pub type Meters <- int; };")
            .parse_type_next();
        let context = fp.context.clone();

        let expr = parse_from_string("let f <- fn(x: Meters): int { x };", "test");
        let result = typing_with_errors(&context, &expr);

        assert!(
            result.get_errors().iter().any(|e| matches!(
                e,
                TypRError::Type(
                    crate::components::error_message::type_error::TypeError::AliasNotImported(
                        name, module, _
                    )
                ) if name == "Meters" && module == "geo"
            )),
            "Expected AliasNotImported error for unimported param type 'Meters', got: {:?}",
            result.get_errors()
        );
    }

    #[test]
    fn test_recursive_function_no_error() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::typing_with_errors;

        let expr = parse2(
            "let factorial <- fn(n: int): int { if (n <= 1) { 1 } else { n * factorial(n - 1) } };"
                .into(),
        )
        .unwrap();
        let result = typing_with_errors(&Context::default(), &expr);
        assert!(
            !result.has_errors(),
            "Recursive function should type-check without errors, got: {:?}",
            result.get_errors()
        );
    }
}
