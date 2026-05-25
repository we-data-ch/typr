//! Snapshot tests for TypR transpilation output.
//!
//! Run `cargo insta review` to review/accept new snapshots.
//! Run `cargo test` to run all tests.

use typr_core::utils::fluent_parser::FluentParser;

fn c_types(lines: &[&str]) -> String {
    let fp = lines.iter().fold(FluentParser::new(), |acc, line| {
        acc.push(line).run()
    });
    fp.get_context().get_type_anotations()
}

fn b_generic_functions(lines: &[&str]) -> String {
    let fp = lines.iter().fold(FluentParser::new(), |acc, line| {
        acc.push(line).run()
    });
    fp.get_context()
        .get_all_generic_functions()
        .iter()
        .map(|(var, _)| var.get_name())
        .filter(|x| !x.contains("<-"))
        .map(|fn_name| {
            format!(
                "{} <- function(x, ...) UseMethod('{}', x)",
                fn_name,
                fn_name.replace('`', "")
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn transpile(code: &str) -> String {
    FluentParser::new()
        .push(code)
        .run()
        .get_r_code()
        .iter()
        .cloned()
        .collect::<Vec<_>>()
        .join("\n")
}

fn transpile_all(lines: &[&str]) -> String {
    let fp = lines.iter().fold(FluentParser::new(), |acc, line| {
        acc.push(line).run()
    });
    fp.get_r_code()
        .iter()
        .cloned()
        .collect::<Vec<_>>()
        .join("\n")
}

mod transpilation {
    use super::*;

    #[test]
    fn integer_literal() {
        let r = transpile("8");
        insta::assert_snapshot!(r);
    }

    #[test]
    fn number_literal() {
        let r = transpile("3.14");
        insta::assert_snapshot!(r);
    }

    #[test]
    fn boolean_literal() {
        let r = transpile("true");
        insta::assert_snapshot!(r);
    }

    #[test]
    fn string_literal() {
        let r = transpile(r#""hello""#);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn simple_let() {
        let r = transpile("let x <- 5;");
        insta::assert_snapshot!(r);
    }

    #[test]
    fn typed_let() {
        let r = transpile("let x: Integer <- 5;");
        insta::assert_snapshot!(r);
    }

    #[test]
    fn simple_function() {
        let r = transpile("let add <- fn(a: int, b: int): int { a + b };");
        insta::assert_snapshot!(r);
    }

    #[test]
    fn type_alias_record() {
        let r = transpile_all(&[
            "type Point <- list { x: int, y: int };",
            "Point:{ x = 1, y = 2 }",
        ]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn pipe_operator() {
        let r = transpile_all(&[
            "type Point <- list { x: int, y: int };",
            "let incr <- fn(p: Point): Point { Point:{x: (p$x+1), y: (p$y+1)} };",
            "Point:{x: 5, y: 12} |> incr()",
        ]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn module_and_use() {
        let r = transpile_all(&[
            "module Math { @pub let pi <- 3.14159; };",
            "use Math::pi;",
        ]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn opaque_alias() {
        let r = transpile("opaque Meters <- int;");
        insta::assert_snapshot!(r);
    }
}

mod generated_files {
    use super::*;

    /// Integer must include 'Incrementable' in its R class hierarchy once a
    /// function typed with Incrementable is defined (which adds the interface
    /// to the subtype graph). Regression test for source-order bug where
    /// a_std.R overwrote c_types.R, silently dropping 'Incrementable'.
    #[test]
    fn integer_class_includes_implemented_interface() {
        let result = c_types(&[
            "type Incrementable <- interface { incr: (Self) -> Self };",
            "let double <- fn(i: Incrementable): Incrementable { i.incr().incr() };",
            "let incr <- fn(s: int): int { s + 1 };",
        ]);
        let integer_line = result
            .lines()
            .find(|l| l.starts_with("Integer <-"))
            .expect("Integer constructor not found in c_types output");
        assert!(
            integer_line.contains("'Incrementable'"),
            "Expected 'Incrementable' in Integer class hierarchy, got: {}",
            integer_line
        );
    }

    #[test]
    fn interface_functions_appear_in_b_generics() {
        let result = b_generic_functions(&[
            "type Incrementable <- interface { incr: (Self) -> Self };",
            "let double <- fn(i: Incrementable): Incrementable { i.incr().incr() };",
            "let incr <- fn(s: int): int { s + 1 };",
        ]);
        assert!(result.contains("double <- function"), "double generic missing");
        assert!(result.contains("incr <- function"), "incr generic missing");
    }
}

mod typing {
    use super::*;
    use typr_core::utils::builder;

    #[test]
    fn integer_type() {
        let ty = FluentParser::new()
            .push("42")
            .parse_type_next()
            .get_last_type();
        insta::assert_debug_snapshot!(ty);
    }

    #[test]
    fn let_introduces_variable() {
        let fp = FluentParser::new()
            .push("let x: Integer <- 5;")
            .run()
            .push("x")
            .parse_type_next();
        let ty = fp.get_last_type();
        insta::assert_debug_snapshot!(ty);
    }

    #[test]
    fn function_type_inference() {
        let ty = FluentParser::new()
            .push("let add <- fn(a: int, b: int): int { a + b };")
            .parse_type_next()
            .get_last_type();
        insta::assert_debug_snapshot!(ty);
    }

    #[test]
    fn record_type() {
        let ty = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .parse_type_next()
            .push("Point:{ x = 1, y = 2 }")
            .parse_type_next()
            .get_last_type();
        insta::assert_debug_snapshot!(ty);
    }

    #[test]
    fn union_alias() {
        let ty = FluentParser::new()
            .push("type Shape <- .Circle(num) | .Square(num);")
            .parse_type_next()
            .push(".Circle(3.14)")
            .parse_type_next()
            .get_last_type();
        insta::assert_debug_snapshot!(ty);
    }
}
