# keyword-positional-record-literal

Regression net for P3 of the syntax-safety plan (`project_syntax_safety_plan` memory,
2026-07-16).

## Ce qui devrait se passer

`list{1, 2, 3}` (and `record{...}`/`object{...}`) with **positional** elements inside the
keyword-prefixed brace form must be rejected with a fatal
`SyntaxError::KeywordRecordPositionalElements`, suggesting `:{1, 2, 3}` (the existing neutral
tuple syntax) instead. Named-field `list{ a = 1, b = 2 }` is unaffected (still `Lang::List`), and
paren-based positional tuples `list(1, 2, 3)` are unaffected too — only the keyword+brace+
positional combination is in scope.

## Anomalies (pre-fix)

Before the fix, `list{1, 2, 3}` silently became `Lang::Tuple` with no error at all — a keyword
that reads as "named record" (`list{...}`) was silently reinterpreted as a positional tuple,
which is confusing and error-prone (a typo'd `=` turns a record into a tuple with no warning).
