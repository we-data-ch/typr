## Should happen

A TypR function declared to return a `Foreign<T>`-family base-with-attributes
type (`get_factor(): FactorExt`) must hand the value back unchanged:
`nlevels(get_factor())` should print `3`.

## Localisation (#@case)

Return-boundary counterpart of `0026-foreign-factor-let-annotation`, same
shared `get_type_anotation` fix (`components/context/vartype.rs`). Kept
separate for the same reason as `0020`/`0024`: it is a structurally
different transpile arm from the `let`-annotation boundary even though one
helper change fixed both.
