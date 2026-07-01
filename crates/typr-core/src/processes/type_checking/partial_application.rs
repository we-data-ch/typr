use crate::components::language::argument_value::ArgumentValue;
use crate::components::r#type::type_system::TypeSystem;
use crate::processes::type_checking::typing;
use crate::processes::type_checking::ArgumentType;
use crate::processes::type_checking::Context;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::Lang;
use crate::processes::type_checking::TypRError;
use crate::processes::type_checking::Type;
use crate::processes::type_checking::TypeContext;
use crate::processes::type_checking::TypeError;
use crate::processes::type_checking::Var;
use crate::utils::builder;

/// `\f(arg1 = val1, ...)` (RFC partial_application.md). `arguments` holds
/// only the fixed `Lang::KeyValue` entries; every parameter of `f` not named
/// there becomes a hole, in `f`'s original parameter order. Desugars into a
/// `Lang::Function { parameters: holes, body: f(holes_and_fixed_in_order) }`
/// and re-enters `typing()` on it, so the rest of the pipeline (defaults,
/// variadics, transpilation) is handled by the ordinary `Lang::Function` path
/// — `PartialApp` never survives past this point.
pub fn partial_application(
    context: &Context,
    expr: &Lang,
    function: &Lang,
    arguments: &[Lang],
    h: &HelpData,
) -> TypeContext {
    let Some(var) = Var::from_language(function.clone()) else {
        return TypeContext::new(builder::any_type(), expr.clone(), context.clone())
            .with_errors(vec![TypRError::Type(TypeError::WrongExpression(h.clone()))]);
    };

    let fixed: Vec<(String, Lang)> = arguments
        .iter()
        .filter_map(|a| match a {
            // `key_value`'s parser captures trailing whitespace in the key span
            // (`recognize(variable)`, where `variable` consumes trailing
            // `multispace0` as part of its own token) — trim before comparing
            // against parameter names.
            Lang::KeyValue { key, value, .. } => Some((key.trim().to_string(), (**value).clone())),
            _ => None,
        })
        .collect();

    // `\f<Type>(arg = val)` — when a type annotation is present, restrict to
    // overloads whose first parameter type matches the annotation. Mirrors how
    // `f<Type>(value)` forces a specific S3 implementation at call sites.
    let forced_type = var.get_type();
    let is_forced = !matches!(forced_type, Type::Empty(_));

    let candidate = context
        .get_types_from_name(&var.get_name())
        .into_iter()
        .find_map(|t| match t {
            Type::Function(params, ret, _) => {
                if is_forced {
                    match params.first() {
                        Some(p) if p.get_type() != forced_type => return None,
                        None => return None,
                        _ => {}
                    }
                }
                let names: Vec<String> = params.iter().map(|p| p.get_argument_str()).collect();
                fixed
                    .iter()
                    .all(|(k, _)| names.contains(k))
                    .then_some((params, *ret))
            }
            _ => None,
        });

    let Some((params, ret_ty)) = candidate else {
        return record_constructor_partial_application(context, expr, &var, &fixed, h)
            .unwrap_or_else(|| {
                TypeContext::new(builder::any_type(), expr.clone(), context.clone())
                    .with_errors(vec![TypRError::Type(TypeError::FunctionNotFound(var))])
            });
    };

    let mut hole_params: Vec<ArgumentType> = Vec::new();
    let mut call_args: Vec<Lang> = Vec::new();
    for param in &params {
        let name = param.get_argument_str();
        match fixed.iter().find(|(k, _)| k == &name) {
            Some((_, value)) => call_args.push(value.clone()),
            None => {
                // `related_type` is left `Empty`, like an ordinarily-parsed
                // bare variable reference (the parser never knows a
                // variable's type up front) — its real type is resolved via
                // context lookup during typing regardless. Pre-filling it
                // with a concrete type here would instead make the R
                // transpiler's `Var::display_type` append a `.classname`
                // suffix to this reference (e.g. `c` -> `c.integer`) that
                // the closure's own parameter declaration doesn't carry,
                // producing a "object not found" error at R runtime.
                call_args.push(Lang::Variable {
                    name: name.clone(),
                    is_opaque: false,
                    related_type: Type::Empty(h.clone()),
                    help_data: h.clone(),
                });
                hole_params.push(param.clone());
            }
        }
    }

    let body = Lang::FunctionApp {
        identifier: Box::new(function.clone()),
        arguments: call_args,
        help_data: h.clone(),
    };

    let desugared = Lang::Function {
        parameters: hole_params,
        return_type: ret_ty,
        body: Box::new(body),
        help_data: h.clone(),
    };

    typing(context, &desugared)
}

/// Fallback for `\TypeName:{ field = val, ... }` (records-only — see the
/// scoping note in `partial_application` above's RFC). `var`'s name must
/// resolve, via aliasing, to a `Type::Record` rather than a `Type::Function`
/// — record constructors aren't registered as callable `Type::Function`s in
/// the typing context (they're generated directly in the R output), so this
/// is a separate lookup rather than another arm of the function-candidate
/// `find_map` above. Mirrors the function-call path: holes are every
/// declared field not named in `fixed`, in the record's declared order;
/// `desugared`'s body is a `Lang::ConstructorCall` instead of a
/// `Lang::FunctionApp`, then re-enters `typing()` so the rest of the
/// constructor pipeline (field validation, casts) runs unchanged.
fn record_constructor_partial_application(
    context: &Context,
    expr: &Lang,
    var: &Var,
    fixed: &[(String, Lang)],
    h: &HelpData,
) -> Option<TypeContext> {
    let record_fields = match context.get_type_from_aliases(var)?.reduce(context) {
        Type::Record(fields, _) => fields.into_iter().collect::<Vec<_>>(),
        _ => return None,
    };

    let names: Vec<String> = record_fields.iter().map(|f| f.get_argument_str()).collect();
    if !fixed.iter().all(|(k, _)| names.contains(k)) {
        return None;
    }

    let mut hole_params: Vec<ArgumentType> = Vec::new();
    let mut call_fields: Vec<ArgumentValue> = Vec::new();
    for field in &record_fields {
        let name = field.get_argument_str();
        match fixed.iter().find(|(k, _)| k == &name) {
            Some((_, value)) => call_fields.push(ArgumentValue(name, value.clone())),
            None => {
                // Same `Empty` rationale as the function-call path above.
                call_fields.push(ArgumentValue(
                    name.clone(),
                    Lang::Variable {
                        name: name.clone(),
                        is_opaque: false,
                        related_type: Type::Empty(h.clone()),
                        help_data: h.clone(),
                    },
                ));
                hole_params.push(field.clone());
            }
        }
    }

    let body = Lang::ConstructorCall {
        module_path: vec![],
        type_name: var.get_name(),
        fields: call_fields,
        spread: None,
        spreads: vec![],
        help_data: h.clone(),
    };

    let desugared = Lang::Function {
        parameters: hole_params,
        return_type: Type::Alias(var.get_name(), vec![], false, h.clone()),
        body: Box::new(body),
        help_data: h.clone(),
    };

    Some(typing(context, &desugared))
}

#[cfg(test)]
mod tests {
    use crate::components::error_message::help_data::HelpData;
    use crate::components::r#type::argument_type::ArgumentType;
    use crate::utils::builder;
    use crate::utils::fluent_parser::FluentParser;
    use crate::Type;

    /// Regression guard: `partial_application` (parser) must not shadow the
    /// pre-existing `\(params) body` lambda syntax — both start with `\`.
    #[test]
    fn test_existing_lambda_syntax_still_parses() {
        use crate::Lang;
        let r: Lang = "\\(x, y) x + y".parse().unwrap();
        match r {
            Lang::Lambda { .. } => {}
            other => panic!("expected Lambda, got {:?}", other),
        }
    }

    #[test]
    fn test_partial_application_basic_type() {
        let res = FluentParser::new()
            .push("let add <- fn(x: int, y: int): int { x + y };")
            .run()
            .check_typing("\\add(x = 2)");
        let expected = Type::Function(
            vec![ArgumentType::new("y", &builder::integer_type_default())],
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    #[test]
    fn test_partial_application_multiple_holes() {
        let res = FluentParser::new()
            .push("let move <- fn(obj: char, to: int, duration: int): char { obj };")
            .run()
            .check_typing("\\move(to = 5)");
        let expected = Type::Function(
            vec![
                ArgumentType::new("obj", &builder::character_type_default()),
                ArgumentType::new("duration", &builder::integer_type_default()),
            ],
            Box::new(builder::character_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    #[test]
    fn test_partial_application_transpiles_to_function_call() {
        let r_code = FluentParser::new()
            .push("let add <- fn(x: int, y: int): int { x + y };")
            .run()
            .check_transpiling("\\add(x = 2)");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("function(y) add(") && r_str.contains("y"),
            "expected a generated single-hole function calling add(...), got: {}",
            r_str
        );
    }

    #[test]
    fn test_partial_application_no_holes_left_zero_arity() {
        let res = FluentParser::new()
            .push("let add <- fn(x: int, y: int): int { x + y };")
            .run()
            .check_typing("\\add(x = 2, y = 3)");
        let expected = Type::Function(
            vec![],
            Box::new(builder::integer_type_default()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    #[test]
    fn test_partial_constructor_application_basic_type() {
        let res = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .run()
            .check_typing("\\Point:{ x = 0 }");
        let expected = Type::Function(
            vec![ArgumentType::new("y", &builder::integer_type_default())],
            Box::new(Type::Alias(
                "Point".to_string(),
                vec![],
                false,
                HelpData::default(),
            )),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    #[test]
    fn test_partial_constructor_application_transpiles_to_constructor_call() {
        let r_code = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .run()
            .check_transpiling("\\Point:{ x = 0 }");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("function(y)") && r_str.contains("Point(") && r_str.contains("x = 0"),
            "expected a generated single-hole function calling Point(...), got: {}",
            r_str
        );
    }

    #[test]
    fn test_partial_application_with_type_target_selects_right_overload() {
        // Two overloads of `compute`: one for int, one for num. `\compute<num>(x = 1.0)`
        // must pick the num overload and leave `y: num` as the hole.
        // Uses `let` (not `@`) so parameter names are preserved in the context.
        let res = FluentParser::new()
            .push("let compute <- fn(x: int, y: int): int { x + y };")
            .run()
            .push("let compute <- fn(x: num, y: num): num { x + y };")
            .run()
            .check_typing("\\compute<num>(x = 1.0)");
        let expected = Type::Function(
            vec![ArgumentType::new("y", &builder::number_type())],
            Box::new(builder::number_type()),
            HelpData::default(),
        );
        assert_eq!(res, expected);
    }

    #[test]
    fn test_partial_application_with_type_target_transpiles_correctly() {
        let r_code = FluentParser::new()
            .push("let compute <- fn(x: int, y: int): int { x + y };")
            .run()
            .push("let compute <- fn(x: num, y: num): num { x + y };")
            .run()
            .check_transpiling("\\compute<num>(x = 1.0)");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("function(y)") && r_str.contains("compute"),
            "expected a single-hole closure calling compute<num>(...), got: {}",
            r_str
        );
    }
}
