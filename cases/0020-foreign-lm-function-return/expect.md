## Should happen

A TypR function declared to *return* a `Foreign<T>`-family type
(`get_model(): LmModel`) must hand the value back unchanged: `print(m)`
after `let m <- get_model();` should show the same `lm` `print.lm` output
as the direct-`readRDS` case (`0018-foreign-lm-let-annotation`).

## Localisation (#@case)

Found while implementing Phase D of `soundness_transpilation.md`. Same
upstream prerequisites as `0018`/`0019`. The function-return boundary
shares `VarType::get_type_anotation` with the `let`-annotation boundary
(`processes/transpiling/mod.rs` call sites at both the `Lang::Let` arm and
the `Lang::Function` return-type wrap use the same helper), so the
class-append fix landed in one place (`vartype.rs`) covers both — this case
exists to pin the return-boundary specifically as its own regression guard,
since it is a structurally different transpile arm even though it happened
to be fixed by the same helper change.
