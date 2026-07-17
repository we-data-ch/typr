# double-slash-comment-warning

Regression net for P1 of the syntax-safety plan (`project_syntax_safety_plan` memory,
2026-07-15/16). TypR comments use `#`, never `//` — a stray `//` used to be tokenized as a dead
operator (`Op::Div2`) with no type/transpile arm, surfacing as a confusing, badly localized type
error instead of a clear syntax diagnostic.

## Ce qui devrait se passer

`//` is recognized by a dedicated `wrong_comment` parser (`processes/parsing/mod.rs`) that treats
it exactly like a real `#`-comment (consumes to end of line) while pushing a **recoverable**
`SyntaxError::WrongCommentSyntax` warning: "TypR comments use `#`, not `//`". Recoverable means
`typr check` still reports success — this is deliberately a warning, not a build-breaking error
(see the severity-policy note in the P1 memory: only `SyntaxError::UnknownElement` is fatal).

## Anomalies (pre-fix)

Before the fix, `//` had no dedicated handling and fell into the generic "unparsable trailing
text" trap, or tokenized as `Op::Div2`, which has no `type_checking`/`transpiling` match arm —
either way, no clear message pointed at the `//` itself.
