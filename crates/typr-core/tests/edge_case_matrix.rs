//! Phase 6 item 1 (audit_type_checking.md): a generative construction ×
//! context matrix.
//!
//! The historical bugs this audit catalogued (`ConstructorCall` context drop,
//! alias hoisting, `Lines` single-statement context drop, and the duplicate-
//! typing bug fixed alongside this file — see the Phase 6 note in
//! audit_type_checking.md) all share a shape: a construct that types fine in
//! isolation breaks once it's nested inside a *different* surrounding
//! construct. This file automates that cross-product check instead of
//! relying on someone happening to write the one test that nests X inside Y.
//!
//! For every (construction, context) pair we assert the checker never
//! silently degrades to `Any` — it must either infer the construction's real
//! type (proven by round-tripping it through a generic identity function, so
//! a silent-`Any` degradation is directly visible as `Type::Any` coming back
//! out) or raise an explicit compile error.

use typr_core::components::r#type::Type;
use typr_core::parsing::parse_from_string;
use typr_core::{typing_with_errors, Context};

/// Type-checks a whole (possibly multi-statement) `.ty` source and returns
/// the last statement's inferred type together with whether any error fired.
fn type_of(source: &str) -> (Type, bool) {
    let ast = parse_from_string(source, "test.ty");
    let result = typing_with_errors(&Context::default(), &ast);
    (result.get_type().clone(), result.has_errors())
}

/// Never-silently-`Any` assertion shared by every matrix cell: a construct
/// nested in a context must come out either with a genuine, non-`Any` type
/// or with an explicit error — anything else means the nesting broke down
/// into the audit's #1 systemic failure mode (silent degradation to `Any`).
fn assert_never_silent_any(construction: &str, context: &str, source: &str) {
    let (ty, has_errors) = type_of(source);
    if !has_errors {
        assert!(
            !matches!(ty, Type::Any(_)),
            "construction `{construction}` inside context `{context}` silently \
             degraded to `Any` with no error — source:\n{source}\ninferred type: {ty:?}"
        );
    }
}

/// One risky construction, expressed as a TypR expression snippet.
struct Construction {
    name: &'static str,
    expr: &'static str,
}

const CONSTRUCTIONS: &[Construction] = &[
    Construction {
        name: "tag_literal",
        expr: ".Some(1)",
    },
    Construction {
        name: "match_expression",
        expr: "match 1 { _ => 42 }",
    },
    Construction {
        name: "lambda_literal",
        expr: "\\(x) x + 1",
    },
    Construction {
        name: "validating_cast",
        expr: "5 as! int",
    },
];

/// Every context wraps `{c}` (the construction's `expr`) in a different
/// surrounding position, then routes the result through `id(...)` — a
/// generic identity function — so the final program type is exactly the
/// construction's own inferred type, not the wrapper's.
fn context_source(context_template: &str, construction: &str) -> String {
    context_template.replace("{c}", construction)
}

const ID_PRELUDE: &str = "let id <- fn(x: T): T { x };\n";

#[test]
fn matrix_construction_as_direct_argument() {
    for c in CONSTRUCTIONS {
        let source = format!("{ID_PRELUDE}id({})", context_source("{c}", c.expr));
        assert_never_silent_any(c.name, "direct_argument", &source);
    }
}

#[test]
fn matrix_construction_through_let_binding() {
    for c in CONSTRUCTIONS {
        let source = format!("{ID_PRELUDE}let v <- {};\nid(v)", context_source("{c}", c.expr));
        assert_never_silent_any(c.name, "let_binding", &source);
    }
}

#[test]
fn matrix_construction_as_sole_match_branch() {
    for c in CONSTRUCTIONS {
        // The construction sits as the (only) branch body of an unrelated
        // outer match — a position distinct from the construction being a
        // match itself (covered by `match_expression` in CONSTRUCTIONS).
        let source = format!("{ID_PRELUDE}id(match 1 {{ _ => {} }})", context_source("{c}", c.expr));
        assert_never_silent_any(c.name, "sole_match_branch", &source);
    }
}

#[test]
fn matrix_construction_as_record_field_value() {
    for c in CONSTRUCTIONS {
        let source = format!(
            "{ID_PRELUDE}let r <- :{{ v = {} }};\nid(r.v)",
            context_source("{c}", c.expr)
        );
        assert_never_silent_any(c.name, "record_field_value", &source);
    }
}

#[test]
fn matrix_construction_as_public_module_member() {
    for c in CONSTRUCTIONS {
        let source = format!(
            "module M {{ @pub let out <- {}; }};\nuse M::out;\n{ID_PRELUDE}id(out)",
            context_source("{c}", c.expr)
        );
        assert_never_silent_any(c.name, "public_module_member", &source);
    }
}

/// The function-body context can't reuse the generic `id(...)` trick: a
/// function's call-site type comes from its *declared* return type, not a
/// fresh inference of its body each call, so wrapping with a generic
/// identity would just measure the annotation, not the construction. Each
/// construction gets its own correctly-typed declaration instead — still
/// covering the position the other five cases can't reach.
#[test]
fn matrix_construction_as_function_body() {
    let cases: &[(&str, &str)] = &[
        ("tag_literal", "let f <- fn(): .Some(int) { .Some(1) };\nf()"),
        ("match_expression", "let f <- fn(): int { match 1 { _ => 42 } };\nf()"),
        ("lambda_literal", "let f <- fn(): (int) -> int { \\(x) x + 1 };\nf()"),
        ("validating_cast", "let f <- fn(): int { 5 as! int };\nf()"),
    ];
    for (name, source) in cases {
        assert_never_silent_any(name, "function_body", source);
    }
}
