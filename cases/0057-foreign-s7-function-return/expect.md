## Should happen

An S7 instance returned from a TypR function typed to return
`Foreign<Any>`-family must not be touched by the return-type conversion —
no `as.X()`/`struct()` cast should be applied. `print(c)` after the call
should show the usual S7 print output.

## Localisation (#@case)

Part of Phase D.5 of `soundness_plan.md`. See `0056`'s expect.md for the
S7-specific background (not S4, not reference-typed, `print()` works as
oracle unlike RC/R6).
