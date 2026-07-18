## Should happen

`m.coefficients` on a `Foreign<T>`-typed value must be rejected at compile
time with a clear error, not silently produce `NULL`/a wrong value. This is
a deliberate design gap, not a bug: `Foreign<T>` reduces to `Any`, and `Any`
carries no structural field information for the dot-access typing rule to
resolve against — TypR simply has no way to know a real `lm` object has a
`$coefficients` slot without an explicit `@extern` accessor (see
`0019-foreign-lm-function-arg`'s `stats::coef`).

## Localisation (#@case)

Cell recorded as a known hole in `interop_matrix.md` (Phase D,
`soundness_transpilation.md`), `status = "wontfix"`: closing this gap would
mean either (a) letting users declare partial structural shapes for foreign
values (a real feature, out of scope here), or (b) special-casing `$`/`.`
access to fall back to a generic `@extern`-style runtime accessor with no
compile-time field checking at all (defeats the point of static typing).
Kept as a case so the exact rejection message is pinned and visible if
either direction is ever pursued.
