# underscore-name-not-backticked-at-use

TypR identifiers may start with `_` (`starting_char`), which R only accepts backtick-quoted.
Definitions were always quoted (`format_backtick` in the `Let` arm), but the `Variable` and
`FunctionApp` arms of `transpiling/mod.rs` emitted the bare name, so `_x` produced R that does
not even parse.

## Ce qui devrait se passer

Use sites go through `transpiling/mod.rs::r_symbol`, which backticks a name starting with `_`:
`` `_inc`(`_x`) ``, printing `4`. The generic stub in `generic_functions.R` is quoted too
(`Var::add_backticks_if_needed`, formerly `add_backticks_if_percent`).

## Anomalies (pre-fix)

`print(_inc(_x))` → R parse error `unexpected input`.
