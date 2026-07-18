//! Snapshot tests for TypR transpilation output.
//!
//! Run `cargo insta review` to review/accept new snapshots.
//! Run `cargo test` to run all tests.

use typr_core::processes::spg::build_spg_from_items;
use typr_core::utils::fluent_parser::FluentParser;

fn c_types(lines: &[&str]) -> String {
    let fp = lines
        .iter()
        .fold(FluentParser::new(), |acc, line| acc.push(line).run());
    fp.get_context().get_type_anotations()
}

fn b_generic_functions(lines: &[&str]) -> String {
    let fp = lines
        .iter()
        .fold(FluentParser::new(), |acc, line| acc.push(line).run());
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
    let fp = lines
        .iter()
        .fold(FluentParser::new(), |acc, line| acc.push(line).run());
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
    fn string_literal_with_double_quotes() {
        let r = transpile(r#""say \"hi\"""#);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn string_literal_single_quoted_source() {
        // A single-quoted TypR literal containing a double quote must still
        // produce a valid, properly escaped R double-quoted string.
        let r = transpile(r#"'a "b" c'"#);
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
    fn type_alias_dataframe() {
        let r = transpile_all(&["type Df <- dataframe[#N]{ id: int, name: char };"]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn type_alias_dataframe_sized() {
        let r = transpile_all(&["type Df3 <- df[3]{ id: int, score: num };"]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn type_alias_vector() {
        let r = transpile_all(&["type IntVector <- Vec[#N, int];"]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn type_alias_vector_sized() {
        let r = transpile_all(&["type V3 <- Vec[3, char];"]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn vector_alias_array_constructor_call() {
        let r = transpile_all(&[
            "type Binaire <- Vec[Any, bool];",
            "Binaire:[true, true, false, true]",
        ]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn constructor_call_spread() {
        // RFC-TR-033: `..source` expands to a `source$field` access for every
        // field not given explicitly; explicit fields keep their own value.
        let r = transpile_all(&[
            "type Person <- list { name: char, age: int };",
            "let bob <- Person:{ name = \"Bob\", age = 12 };",
            "Person:{ name = \"Alice\", ..bob }",
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
        let r = transpile_all(&["module Math { @pub let pi <- 3.14159; };", "use Math::pi;"]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn opaque_alias() {
        let r = transpile("opaque Meters <- int;");
        insta::assert_snapshot!(r);
    }

    #[test]
    fn opaque_state_alias() {
        let r = transpile("opaque State<T> <- Any;");
        insta::assert_snapshot!(r);
    }

    #[test]
    fn tuple_literal_carries_alias_class_for_dispatch() {
        // A function with a tuple-typed parameter is emitted as an S3 method
        // `f.TupleN`, so the tuple literal must be annotated with `as.TupleN()`
        // (not just the bare 'Tuple' class) for UseMethod dispatch to find it.
        let r = transpile_all(&[
            "let f <- fn(a: list{int, bool}): bool { a[2] };",
            "let a <- :{3, false};",
            "f(a)",
        ]);
        insta::assert_snapshot!(r);
    }

    #[test]
    fn state_aliasing_is_plain_assignment() {
        // State<T> gets its shared-mutation semantics entirely from R
        // environments being reference types, so aliasing (`let b <- a;`)
        // must transpile to a plain assignment with no copy/clone wrapper.
        let r = transpile_all(&[
            "opaque State<T> <- Any;",
            "@state: (value: T) -> State<T>;",
            "let a <- state(1);",
            "let b <- a;",
        ]);
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
            .find(|l| l.starts_with("as.Integer <-"))
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
        assert!(
            result.contains("double <- function"),
            "double generic missing"
        );
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

/// Snapshots of the exact wording of diagnostics added by the
/// audit_type_checking.md Phases 1-5 fixes (Phase 6 item 2: freeze the
/// messages so a future refactor can't silently reword/regress them —
/// `cargo test` alone only checks `has_errors()`, not what the error *says*).
mod diagnostics {
    use typr_core::components::error_message::help_data::HelpData;
    use typr_core::components::error_message::type_error::TypeError;
    use typr_core::components::error_message::typr_error::TypRError;
    use typr_core::parsing::parse_from_string;
    use typr_core::{typing_with_errors, Context};

    /// Type-checks a whole (possibly multi-statement) `.ty` source through
    /// the real `parse()`/`typing_with_errors()` pipeline (not `parse2`,
    /// which only accepts a single top-level statement) and renders every
    /// collected error's `simple_message()` — the plain, file-independent
    /// wording (no ANSI/miette framing) also used by the LSP.
    fn diagnose(source: &str) -> String {
        let ast = parse_from_string(source, "test.ty");
        let result = typing_with_errors(&Context::default(), &ast);
        result
            .errors
            .iter()
            .map(|e| e.simple_message())
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn s1_undefined_variable() {
        insta::assert_snapshot!(diagnose("totally_bogus_never_declared_name;"));
    }

    #[test]
    fn m1_non_exhaustive_match() {
        insta::assert_snapshot!(diagnose(
            "type Color <- .Red | .Green | .Blue;\n\
             fn(c: Color): int { match c { .Red => 1, .Green => 2 } };"
        ));
    }

    #[test]
    fn m4_pattern_type_mismatch_unknown_variant() {
        insta::assert_snapshot!(diagnose(
            "type Color <- .Red | .Green;\n\
             fn(c: Color): int { match c { .Red => 1, .Green => 2, .Blue => 3 } };"
        ));
    }

    #[test]
    fn c1_unmatching_return_type() {
        insta::assert_snapshot!(diagnose(
            "fn(x: int): int { if (x > 0) { return \"a\"; } else { 1 } };"
        ));
    }

    #[test]
    fn c2_loop_control_outside_loop() {
        insta::assert_snapshot!(diagnose("break;"));
    }

    #[test]
    fn d1_dataframe_column_not_vector() {
        insta::assert_snapshot!(diagnose("data.frame(a = [1, 2, 3], b = 5);"));
    }

    #[test]
    fn d1_dataframe_column_length_mismatch() {
        insta::assert_snapshot!(diagnose("data.frame(a = [1, 2, 3], b = [4, 5]);"));
    }

    #[test]
    fn g3_alias_arity_mismatch() {
        insta::assert_snapshot!(diagnose("let f <- fn(o: Option): int { 0 };"));
    }

    /// M3 (`UnsupportedPattern`): no clean textual repro exists — the tag
    /// pattern grammar only accepts a plain identifier inside `.Tag(...)`
    /// (see `tag_pattern_with_var` in `parsing/elements.rs`), so a nested
    /// pattern can't be typed at all today (same reason `.Some(.Some(x))`
    /// doesn't parse, per audit_type_checking.md M3). Constructing the error
    /// value directly still freezes its wording.
    #[test]
    fn m3_unsupported_pattern() {
        let err = TypRError::Type(TypeError::UnsupportedPattern(
            "nested pattern inside tuple pattern".to_string(),
            HelpData::default(),
        ));
        insta::assert_snapshot!(err.simple_message());
    }
}

mod spg {
    use super::*;

    #[test]
    fn function_and_record_alias() {
        let fp = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .parse_type_next()
            .push("@pub let distance <- fn(p: Point): num { 0.0 };")
            .parse_type_next();
        let items: Vec<_> = fp.get_new_code().iter().cloned().collect();
        let spg = build_spg_from_items(&items, "mypkg", "0.1.0");
        let json = serde_json::to_string_pretty(&spg).unwrap();
        insta::assert_snapshot!(json);
    }

    #[test]
    fn union_alias() {
        let fp = FluentParser::new()
            .push("type Color <- .Red | .Green | .Blue;")
            .parse_type_next();
        let items: Vec<_> = fp.get_new_code().iter().cloned().collect();
        let spg = build_spg_from_items(&items, "mypkg", "0.1.0");
        let json = serde_json::to_string_pretty(&spg).unwrap();
        insta::assert_snapshot!(json);
    }
}
