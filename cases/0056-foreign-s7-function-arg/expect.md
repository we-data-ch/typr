## Should happen

An S7 (`S7::new_class`) instance read back through `@extern base::readRDS`
must flow into a TypR function parameter typed `Foreign<Any>`-family
untouched. `print(x)` inside `describe` should dispatch through S7's real
print method and show the usual `<Counter7>\n @ value: num 5` output.

## Localisation (#@case)

Part of Phase D.5 of `soundness_plan.md`. S7 (R-core's new official OOP
system, installed via `install.packages("S7")` for this pass — not present
before) is neither S4 (`isS4()` is `FALSE`, unlike RC/`Matrix::Matrix`) nor
a reference/environment type like RC/R6 (`typeof()` is the new `"object"`
type; properties are set by value, no mutation-through-alias semantics).
Unlike RC/R6, `print()` works standalone on a bare `readRDS()`'d S7
fixture with no `library(S7)` call anywhere in the session — confirmed
directly with a plain `Rscript -e 'print(readRDS(...))'` — so this row
uses `print()` as the oracle exactly like rows 1/2/7, no
`isS4()`/`inherits()` workaround needed the way D.1's RC/R6 cases required.
