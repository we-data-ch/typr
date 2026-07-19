## Should happen

An RC (`setRefClass`) instance read back through `@extern base::readRDS` must
flow into a TypR function parameter typed `Foreign<Any>`-family untouched.
`isS4(x)` (`base::isS4`) must stay `TRUE`.

## Localisation (#@case)

Part of Phase D.1 of `soundness_plan.md`. Uses `isS4()` rather than `print()`
as the oracle: a bare `readRDS()`'d RC instance has no defining generator
registered in the fresh R session that deserializes it (only the instance
data was serialized, not the `refObjectGenerator`), so its reference-class
print dispatch falls back to R's generic S4 object print regardless of
TypR — confirmed independently with a plain `Rscript -e 'print(readRDS(...))'`
outside any TypR-generated code. That's a pure-R session/serialization quirk
orthogonal to the soundness property under test, so `isS4()` is used
instead: it survives round-tripping intact, and is exactly what a
corrupting `as.X()`/`struct()` cast would flip to `FALSE` (confirmed
directly: `class(rc) <- unique(c(class(rc), "Counter", "Foreign0", "Any"))`
on the fixture prints R's own warning `the result will no longer be an S4
object` and `isS4()` becomes `FALSE` — the same corruption mechanism
`0022-foreign-s4-matrix-let-annotation` found and fixed for
`Matrix::Matrix`). RC is a genuinely S4 object (`isS4()` is `TRUE`) unlike
R6, wrapping a mutable environment in an S4 shell — a structurally distinct
case from the slotted `Matrix::Matrix` object even though both are S4.
