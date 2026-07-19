## Should happen

An RC (`setRefClass`) instance returned from a TypR function typed to
return `Foreign<Any>`-family must not be touched by the return-type
conversion — no `as.X()`/`struct()` cast should be applied. `isS4(counter)`
must stay `TRUE`.

## Localisation (#@case)

Part of Phase D.1 of `soundness_plan.md`. See `0031`'s expect.md for why
`isS4()` is used as the oracle instead of `print()`.
