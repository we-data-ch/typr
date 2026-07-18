## Should happen

Passing a `Foreign<T>`-typed base value (a real ordered `factor`) as the
argument of an ordinary TypR function must reach that function's body:
`count_levels(f)` should print `3`.

## Localisation (#@case)

Same dispatch-fallback gap as `0019-foreign-lm-function-arg` and
`0023-foreign-s4-matrix-function-arg`: `count_levels.Foreign0` could never
be reached since a real `factor`'s runtime class (`c("ordered", "factor")`)
never matches the synthetic `"Foreign0"` suffix. Fixed the same way
(`processes/transpiling/mod.rs`).
