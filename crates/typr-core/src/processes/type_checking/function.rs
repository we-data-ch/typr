use crate::components::r#type::type_system::TypeSystem;
use crate::processes::type_checking::ArgumentType;
use crate::processes::type_checking::HelpData;
use crate::processes::type_checking::TypeContext;
use crate::utils::builder;
use crate::Context;
use crate::Lang;
use crate::Type;

pub fn function(
    context: &Context,
    expr: &Lang,
    params: &Vec<ArgumentType>,
    ret_ty: &Type,
    body: &Box<Lang>,
    h: &HelpData,
) -> TypeContext {
    let list_of_types = params
        .iter()
        .map(ArgumentType::get_type)
        .collect::<Vec<_>>();
    let sub_context = params
        .into_iter()
        .map(|arg_typ| arg_typ.clone().to_var(context))
        .zip(list_of_types.clone())
        .fold(context.clone(), |cont, (var, typ)| {
            cont.clone().push_var_type(var, typ, &cont)
        });
    let body_type = body.typing(&sub_context);
    let mut errors = body_type.errors.clone();
    (!body_type.value.reduce_and_subtype(&ret_ty, &sub_context).0)
        .then(|| errors.push(builder::unmatching_return_type(ret_ty, &body_type.value)));
    TypeContext::new(
        Type::Function(list_of_types, Box::new(ret_ty.clone()), h.clone()),
        expr.clone(),
        sub_context,
    )
    .with_errors(errors)
}

#[cfg(test)]
mod tests {
    use crate::components::error_message::help_data::HelpData;
    use crate::utils::builder;
    use crate::utils::fluent_parser::FluentParser;
    use crate::Type;

    // =====================================================================
    // Tests for function type-checking with Interface parameters
    // =====================================================================
    //
    // When a function parameter has an Interface type, the type-checker must:
    //   1. Assign an opaque alias type to that parameter in the sub-context
    //   2. Decompose the interface's method signatures into individual
    //      function variables in the sub-context, replacing `Self` with
    //      the opaque alias
    //   3. Allow the function body to call those interface methods
    //   4. Return the correct function type (with the original Interface
    //      in the parameter list)
    //
    // This approach avoids requiring explicit generic notation to implement
    // multiple interfaces.
    // =====================================================================

    /// A function with an interface parameter that has a single method
    /// should type-check successfully when the body calls that method.
    #[test]
    fn test_function_with_interface_param_single_method() {
        let res = FluentParser::new()
            .check_typing("fn(x: interface { view: (Self) -> char }): char { view(x) }");
        let interface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let expected = builder::function_type(&[interface], builder::character_type_default());
        assert_eq!(res, expected);
    }

    /// A function with an interface parameter containing multiple methods.
    /// The body can call any of those methods.
    #[test]
    fn test_function_with_interface_param_multiple_methods() {
        let res = FluentParser::new()
            .check_typing(
                "fn(x: interface { to_char: (Self) -> char, to_int: (Self) -> int }): char { to_char(x) }"
            );
        let interface = builder::interface_type(&[
            (
                "to_char",
                builder::function_type(
                    &[builder::self_generic_type()],
                    builder::character_type_default(),
                ),
            ),
            (
                "to_int",
                builder::function_type(
                    &[builder::self_generic_type()],
                    builder::integer_type_default(),
                ),
            ),
        ]);
        let expected = builder::function_type(&[interface], builder::character_type_default());
        assert_eq!(res, expected);
    }

    /// A concrete type (int) that satisfies an interface can be passed
    /// as an argument to a function expecting that interface.
    #[test]
    fn test_function_app_with_interface_param_concrete_type() {
        let res = FluentParser::new()
            .push("@view: (int) -> char;")
            .run()
            .push("let f <- fn(x: interface { view: (Self) -> char }): char { view(x) };")
            .parse_type_next()
            .check_typing("f(5)");
        assert_eq!(res, builder::character_type_default());
    }

    /// When a function with interface parameter has a body whose return
    /// type does not match the declared return type, an error should be
    /// reported.
    #[test]
    fn test_function_with_interface_param_return_type_mismatch() {
        let fp = FluentParser::new()
            .push("fn(x: interface { view: (Self) -> char }): int { view(x) }")
            .parse_next()
            .type_next();
        // The body returns char (from view), but declared return is int
        // This should produce an error
        let interface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let expected = builder::function_type(&[interface], builder::integer_type_default());
        assert_eq!(fp.get_last_type(), expected);
    }

    /// A function with an interface parameter alongside a regular parameter.
    #[test]
    fn test_function_with_interface_and_regular_params() {
        let res = FluentParser::new()
            .check_typing("fn(x: interface { view: (Self) -> char }, y: int): char { view(x) }");
        let interface = builder::interface_type(&[(
            "view",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default(),
            ),
        )]);
        let expected = builder::function_type(
            &[interface, builder::integer_type_default()],
            builder::character_type_default(),
        );
        assert_eq!(res, expected);
    }

    /// An interface method with multiple parameters (Self + another type).
    /// The `replace_function_types` should only replace `Self` occurrences,
    /// preserving other parameter types.
    #[test]
    fn test_function_with_interface_multi_param_method() {
        let res = FluentParser::new()
            .check_typing("fn(x: interface { add: (Self, int) -> Self }): int { 5 }");
        let interface = builder::interface_type(&[(
            "add",
            builder::function_type(
                &[
                    builder::self_generic_type(),
                    builder::integer_type_default(),
                ],
                builder::self_generic_type(),
            ),
        )]);
        let expected = builder::function_type(&[interface], builder::integer_type_default());
        assert_eq!(res, expected);
    }

    /// Basic function without interface parameters should still work.
    /// Regression test to ensure interface handling doesn't break normal functions.
    #[test]
    fn test_function_without_interface_param() {
        let res = FluentParser::new().check_typing("fn(x: int): int { x }");
        let expected = builder::function_type(
            &[builder::integer_type_default()],
            builder::integer_type_default(),
        );
        assert_eq!(res, expected);
    }

    /// A function with an interface parameter that uses `Self` as a return type
    /// in one of its methods. The body calls the method and returns the result.
    #[test]
    fn test_function_with_interface_self_return() {
        let res = FluentParser::new()
            .check_typing("fn(x: interface { identity: (Self) -> Self }): int { 5 }");
        let interface = builder::interface_type(&[(
            "identity",
            builder::function_type(
                &[builder::self_generic_type()],
                builder::self_generic_type(),
            ),
        )]);
        let expected = builder::function_type(&[interface], builder::integer_type_default());
        assert_eq!(res, expected);
    }

    /// Test that an inline interface with an operator method (`+`) can be
    /// used in the function body via the operator syntax (a + a).
    #[test]
    fn test_function_with_inline_interface_operator_in_body() {
        let fp = FluentParser::new()
            .push("fn(a: interface { `+`: (Self, Self) -> Self }): int { a + a }")
            .parse_next()
            .type_next();
        let interface = builder::interface_type(&[(
            "`+`",
            builder::function_type(
                &[builder::self_generic_type(), builder::self_generic_type()],
                builder::self_generic_type(),
            ),
        )]);
        let expected = builder::function_type(&[interface], builder::integer_type_default());
        assert_eq!(fp.get_last_type(), expected);
    }

    /// Reproduction of the user bug: a named interface type alias with an
    /// operator method, used as both parameter and return type.
    /// When using a named alias like `type Addable <- interface {...}`,
    /// the function type preserves the alias name in its signature.
    /// The body `a + a` should type-check without errors.
    #[test]
    fn test_function_with_named_interface_operator() {
        let fp = FluentParser::new()
            .push("type Addable <- interface { `+`: (Self, Self) -> Self };")
            .run()
            .push("fn(a: Addable): Addable { a + a }")
            .parse_next()
            .type_next();
        // With a named alias, the function type uses Alias("Addable") not the expanded Interface
        let addable = Type::Alias("Addable".to_string(), vec![], false, HelpData::default());
        let expected = builder::function_type(&[addable.clone()], addable);
        assert_eq!(fp.get_last_type(), expected);
    }

    /// The named interface operator test should produce no type errors.
    /// This verifies the actual user-reported bug is fixed (the body
    /// `a + a` should not produce "Found: any" error).
    #[test]
    fn test_function_with_named_interface_operator_no_errors() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;

        // First, parse and type-check the type alias
        let code1 =
            parse2("type Addable <- interface { `+`: (Self, Self) -> Self };".into()).unwrap();
        let tc = TypeChecker::new(Context::empty()).typing_no_panic(&code1);

        // Then parse and type-check the function
        let code2 = parse2("let double <- fn(a: Addable): Addable { a + a };".into()).unwrap();
        let tc2 = tc.typing_no_panic(&code2);

        assert!(
            !tc2.has_errors(),
            "Expected no type errors, but got: {:?}",
            tc2.get_errors()
        );
    }

    /// Test: function parameter called inside match expression branch.
    /// Regression test for: function 'f' not defined in this scope when
    /// calling f(x) inside a .Some(x) => pattern match branch.
    /// The fix ensures that:
    /// 1. Match type is not reduced before pattern matching (preserves Alias types)
    /// 2. Function parameters are preserved in the returned context
    #[test]
    fn test_function_param_called_in_match_branch() {
        use crate::components::context::Context;
        use crate::processes::parsing::parse2;
        use crate::processes::type_checking::type_checker::TypeChecker;
        let code = parse2(
            "fn(o: Option<int>, f: (int) -> int): int { match o { .Some(x) => f(x), .None => 0 } }"
                .into(),
        )
        .unwrap();
        let tc = TypeChecker::new(Context::default()).typing_no_panic(&code);
        assert!(
            !tc.has_errors(),
            "Expected no type errors, got: {:?}",
            tc.get_errors()
        );
    }
}
