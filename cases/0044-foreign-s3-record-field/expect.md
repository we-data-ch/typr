## Should happen

A `Foreign<T>`-family value stored as a record field (`Wrapper:{ model = m,
label = "test" }`) must not be touched by the constructor's per-field
handling. `w.model` (field access on the *known* structural field `model`
of `Wrapper` — not a `.`/`$` access on the `Foreign<T>`-typed value itself,
which is the documented column-f hole) should dispatch `print()` through
`m`'s real `lm` class and show the usual `Call:`/`Coefficients:` output.

## Localisation (#@case)

Part of Phase D.4 of `soundness_plan.md`. Confirmed by inspecting the
generated R: `Wrapper(model = m, label = ...)` passes `m` straight through
with no per-field cast, and `validate_Wrapper`'s per-field type check
(`inherits(x[["field"]], "class")`) is only emitted for the concrete
`label: char` field, not for the `model: LmModel` (`Foreign<T>`-family)
field — consistent with the `identity()` skip already established for
`let`/return-type annotations (bug #3 of the four original Phase D fixes).
