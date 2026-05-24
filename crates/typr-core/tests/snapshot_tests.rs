//! Snapshot tests for TypR transpilation output.
//!
//! Run `cargo insta review` to review/accept new snapshots.
//! Run `cargo test` to run all tests.

use typr_core::utils::fluent_parser::FluentParser;

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
