## Should happen

An R6 instance read back through `@extern base::readRDS` must flow into a
TypR function parameter typed `Foreign<Any>`-family untouched.
`inherits(x, "Counter6")` (`base::inherits`) must stay `TRUE`.

## Localisation (#@case)

Part of Phase D.1 of `soundness_plan.md`. Uses `inherits()` rather than
`print()` as the oracle: R6's real print formatting (`print.R6`) requires
the R6 package to be attached for S3 method dispatch to find it — a bare
`readRDS()`'d fixture not routed through an `@extern pkg::fn` call to a
real R6-exporting package never attaches it, so `print()` falls back to
R's generic environment print regardless of TypR — confirmed independently
with a plain `Rscript -e 'print(readRDS(...))'` with no `library(R6)`
call. That's a pure-R package-loading quirk orthogonal to the soundness
property under test, so `inherits()` is used instead: it's a plain
`class` attribute check needing no package, and unlike RC's `isS4()`,
R6's class-append (even if a corrupting `as.X()`/`struct()` cast were
reintroduced) wouldn't flip this to `FALSE` since `struct()` appends new
class entries after the existing ones (`class(x) <- unique(c(class(x),
new_class))`) — R6 isn't S4, so there's nothing analogous to `isS4()` to
lose. Unlike RC, an R6 instance is NOT S4 (`isS4()` is `FALSE`) — it's a
plain `environment` with class `c("Counter6", "R6")`, closer in shape to
the base-with-attributes row (`factor`,
`0027-foreign-factor-function-arg`) than to the S4 rows, despite also
being a reference/mutable object like RC.
