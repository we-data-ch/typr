## Should happen

An R6 instance returned from a TypR function typed to return
`Foreign<Any>`-family must not be touched by the return-type conversion.
`inherits(counter, "Counter6")` must stay `TRUE`.

## Localisation (#@case)

Part of Phase D.1 of `soundness_plan.md`. See `0035`'s expect.md for why
`inherits()` is used as the oracle instead of `print()`.
