## Should happen

`let c: Counter7 <- readRDS(...)` where `Counter7 <- Foreign<Any>` must use
`identity()` for the annotation cast, not `as.Counter7()` — the same
`Foreign<T>`-family skip (`VarType::get_type_anotation`) that fixed bug #3
of the four original Phase D fixes for `lm`/`Matrix::Matrix`. Since S7
objects are not S4, appending a TypR class via `struct()`/`class<-` would
be non-destructive the same way it is for `lm`/`factor` even if the skip
somehow didn't apply — so this case is primarily a regression guard
confirming the `identity()` skip already covers the S7 shape, not expected
to find a new bug.

## Localisation (#@case)

Part of Phase D.5 of `soundness_plan.md`. Also exercises `--checked`
(`checked = true`): `checked_descriptor` must return `None` (no assertion)
for the same `Foreign<T>`-resolution reason, confirmed by the
`must_not_contain = "[typr --checked]"` rule.
