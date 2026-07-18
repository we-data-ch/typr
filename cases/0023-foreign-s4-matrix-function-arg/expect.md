## Should happen

Passing a `Foreign<T>`-typed S4 value (`MatrixS4`, a real `Matrix::Matrix`)
as the argument of an ordinary TypR function must reach that function's
body: `get_nrow(mat)` should print `2`.

## Localisation (#@case)

Same dispatch-fallback gap as `0019-foreign-lm-function-arg`, exercised
against a real S4 object instead of an S3 one: `get_nrow.Foreign0` is the
only method `generic_functions.R`'s stub could ever have dispatched to
before the fix, and a real S4 `Matrix` object's runtime class (`"dgeMatrix"`
or similar, never `"Foreign0"`) never matches it. Fixed the same way, by
also emitting `get_nrow.default` when the dispatch parameter resolves to a
`Foreign<T>`-family alias (`processes/transpiling/mod.rs`).
