## Should happen

Passing a `Foreign<T>`-typed value (here `LmModel`, a real `stats::lm` fit)
as the argument of an ordinary TypR function must reach that function's
body — `count_coefficients(m)` should print `2` (the model has an
intercept and one slope coefficient).

## Localisation (#@case)

Found while implementing Phase D of `soundness_transpilation.md`. Same two
upstream prerequisite fixes as `0018-foreign-lm-let-annotation` (`Foreign<T>`
alias arity + `@extern` names shadowed by `generic_functions.R`), plus a
third, specific to this column: a plain top-level `let f <- fn(x: T): R
{...}` is emitted as an S3 method dispatching on `T`'s class — for
`LmModel` that means `count_coefficients.Foreign0 <- function(m) ...`. But a
real foreign value's runtime class is whatever R gave it (`"lm"` here, never
the TypR-side alias name), so that method could never be reached — every
call would fail with `no applicable method for 'count_coefficients' applied
to an object of class "lm"`.

Fixed in `processes/transpiling/mod.rs`'s `Lang::Let` arm by extending the
existing "interface-pure first param also emits a `.default` fallback"
mechanism (originally built for structurally-satisfied interfaces with no
runtime class of their own) to also cover a first parameter that resolves
to a `Foreign<T>`-family alias: `count_coefficients.default` is now emitted
alongside `count_coefficients.Foreign0`, and `UseMethod` falls through to it
since no class ever matches `"Foreign0"`.
