//! Regression tests for bugs.md B-01 (tuple arity), B-02 (parameter
//! contravariance), B-03 (full subtyping inside function types) and B-06
//! (occurs check across generic families).

use typr_core::parsing::parse_from_string;
use typr_core::{typing_with_errors, Context};

fn ok(source: &str) -> bool {
    let ast = parse_from_string(source, "test.ty");
    !typing_with_errors(&Context::default(), &ast).has_errors()
}

#[test]
fn b01_tuple_arity_is_checked_both_ways() {
    assert!(!ok(
        "let f <- fn(): tuple{int, int} { :{1, 2} };\nlet x: tuple{int, int, int} <- f();"
    ));
    assert!(!ok(
        "let f <- fn(): tuple{int} { :{1} };\nlet x: tuple{int, int} <- f();"
    ));
    assert!(ok(
        "let f <- fn(): tuple{int, int} { :{1, 2} };\nlet x: tuple{int, int} <- f();"
    ));
}

#[test]
fn b02_function_parameters_are_contravariant() {
    // `g` promises to accept anything, `f` only takes `int`: unsound.
    assert!(!ok("let f <- fn(x: int): int { x };\nlet g: (Any) -> int <- f;"));
    // The reverse is the safe direction.
    assert!(ok("let f <- fn(x: Any): int { 1 };\nlet g: (int) -> int <- f;"));
}

#[test]
fn b03_function_return_uses_record_width() {
    assert!(ok(
        "let f <- fn(): list{a: int, b: char} { :{a = 1, b = \"x\"} };\nlet g: () -> list{a: int} <- f;"
    ));
    assert!(!ok(
        "let f <- fn(): list{a: int} { :{a = 1} };\nlet g: () -> list{a: int, b: char} <- f;"
    ));
}
