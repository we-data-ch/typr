## Should happen

`let counter: Counter6 <- readRDS(...)` where `Counter6 <- Foreign<Any>`
must use `identity()` for the annotation cast, not `as.Counter6()`. Since an
R6 object is class `c("Counter6", "R6")` (S3-shaped, not S4), appending a
TypR class via `struct()`/`class<-` is non-destructive the same way it is
for `lm` (`0018-foreign-lm-let-annotation`) — this case is primarily a
regression guard confirming the `Foreign<T>`-family `identity()` skip
(`VarType::get_type_anotation`) already covers the R6 shape, not expected
to find a new bug. `inherits(counter, "Counter6")` is used as the oracle
instead of `print()` — see `0035`'s expect.md for why.

## Localisation (#@case)

Part of Phase D.1 of `soundness_plan.md`.
