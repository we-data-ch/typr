## Should happen

`let counter: Counter <- readRDS(...)` where `Counter <- Foreign<Any>` must
use `identity()` for the annotation cast, not `as.Counter()`. This is the
exact bug shape `0022-foreign-s4-matrix-let-annotation` found and fixed for
`Matrix::Matrix`: `as.X()` calls `struct()`, which appends TypR class names
onto the object's real class vector via `class(x) <- unique(c(class(x),
new_class))` — for an S4 object this silently strips S4-ness (`isS4()`
becomes `FALSE` after a bare `class<-` assignment with more than one class
string), corrupting the value. RC (`setRefClass`) is also genuinely S4
(`isS4()` is `TRUE`), so this case exercises the same fix
(`Context::resolves_to_foreign_alias` / `VarType::get_type_anotation` in
`components/context/vartype.rs`) on a second, structurally different S4
object (reference/environment-backed rather than slotted numeric data).
`isS4(counter)` is used as the oracle instead of `print()` — see `0031`'s
expect.md for why.

## Localisation (#@case)

Part of Phase D.1 of `soundness_plan.md`. Confirmed by inspecting the
generated `R/main.R`: the annotation compiles to `identity() |>
as.Generic()`, not `as.Counter()`, so this case is a regression guard
rather than expected to find a new bug.
