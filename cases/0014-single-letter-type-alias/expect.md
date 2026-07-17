# single-letter-type-alias

Regression net for P2 of the syntax-safety plan (`project_syntax_safety_plan` memory,
2026-07-15/16), see also [[bug_single_letter_alias_name_unparseable]].

## Ce qui devrait se passer

`type T <- int;` (single uppercase letter alias name) must be rejected with a fatal
`SyntaxError::SingleLetterTypeName` — single uppercase letters are reserved for generic type
variables (`T`, `U`, ...), so allowing them as alias names would collide with the generic-param
convention used everywhere else in TypR signatures.

## Anomalies (pre-fix)

Before the fix, `type_alias` required `pascal_case_no_space` (`one_of(A-Z)` + `alphanumeric1`,
i.e. **at least one** letter after the first), so a single-letter name had nothing left to match,
`type_alias` failed outright, and the whole `type X <- ...;` statement silently fell through to a
far more permissive later `alt()` branch that only consumed the bare `type` keyword as a variable
reference — dropping the alias name and its target type from the AST with zero error, exit 0.
