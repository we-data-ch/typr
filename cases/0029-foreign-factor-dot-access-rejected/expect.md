## Should happen

`f.raw_levels` on a `Foreign<T>`-typed base value must be rejected at
compile time, same rationale as `0021-foreign-lm-dot-access-rejected` and
`0025-foreign-s4-matrix-dot-access-rejected`. Reaching the `levels`
attribute requires an explicit `@extern base::levels` accessor instead.
(Named `raw_levels`, not `levels`: TypR's own stdlib already declares
`@levels: (f: Factor<L>) -> L;` in `factor.ty`, so `f.levels` takes the
UFCS dot-pipe path — `e1.e2` desugaring to `e2(e1)` when `e2` is a known
function name — and fails with a different, unrelated parse-level error
instead of the "undefined field" one this case is about.)

## Localisation (#@case)

Cell recorded as a known hole in `interop_matrix.md`, `status = "wontfix"`
— same rationale as the other two dot-access cells in this priority set.
