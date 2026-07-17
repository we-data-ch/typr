# single-equals-comparison-warning

Regression net for P7 of the syntax-safety plan (`project_syntax_safety_plan` memory,
2026-07-17). A bare `=` used where `==` was meant (`if (a = b) { ... }`) used to be silently
dropped from the tokenizer entirely (see P1: `op()` no longer recognizes a bare `=` as an
operator), leaving the rest of the statement unparseable and causing it to fall through to the
generic "unparsable trailing text" trap (`SyntaxError::UnknownElement`, code silently dropped).

## Ce qui devrait se passer

`elements.rs`'s expression-continuation tokenizer (`operator_like_token`) has a dedicated
`single_equals_recovery_token` fallback, tried only after the real operator tokenizer
(`element_operator_token`, backed by `op()`) has already failed at this position — so it never
fires on a genuine `==`/`!=`/`<=`/`>=` (those are matched first as two-character tags). It
recovers a bare `=` as `Op::Eq` (i.e. `a = b` behaves like `a == b`) while pushing a
**recoverable** `SyntaxError::SingleEqualsComparison` warning: "`=` is not a comparison
operator". Recoverable means `typr check` still reports success — this is deliberately a
warning, not a build-breaking error, same severity class as P1's `WrongCommentSyntax`.

Deliberately NOT added to the shared `op()` primitive in `operators.rs`: `op()` is also called
by the *type* grammar (`types.rs::index_operator`/`compute_operators`, for `type Combined <- A +
B;`), which sits directly in front of a default parameter's `= value` separator (`greeting: char
= "Hello"`) — recovering `=` there panics `compute_operators` on the unhandled `Op::Eq`
combination. This is why the recovery lives in `elements.rs` instead, guarded by
`not(char('>'))` so it never swallows a match arm's `=>` separator either.

## Anomalies (pre-fix)

Before the fix, `if (a = b) { 1 } else { 0 };` failed to parse as an `if` expression at all: the
condition parsing stopped at `a`, the `=` was left unconsumed, `if_exp`'s expected `)` failed to
match, and the whole construct fell through to a different top-level alternative that consumed
only `if (a = b)` before leaving `{ 1 } else { 0 };` as unparseable trailing text — reported as a
confusing `SyntaxError::UnknownElement` with no indication the real problem was a stray `=`.
