# hidden-dot-names

A leading `.` makes a *hidden* name, as in R (`.cache`, `.helper`): private by name — it can't be
`@pub`/`@export` (S020) — and emitted unchanged in the R output (so `ls()` and R's default
`exportPattern("^[^\\.]")` hide it too).

`__name` is the ASCII spelling of `.name`, consistent with the general `__` → `.` rule
(`as__character` → `as.character`). The parser normalizes a leading `__` to `.`
(`parsing/elements.rs::variable_exp`), so `.cache` and `__cache` are the **same** binding for the
type checker. That spelling is the one to use right after the UFCS/field `.` (`p.__norm()`,
`p.__secret`): `p..norm()` is rejected (S019) because it reads as the `..` spread token.
`p |> .norm()` is the other way to call it.

## Ce qui devrait se passer

`p.__norm()` and `p |> .norm()` both print `42`; `.twice(__cache)` prints `40`; the `Seg`
overload of `.norm` prints `7`. No `__` survives in the generated R.

A hidden typed function is an S3 method like any other (`.norm.Pt`, `.norm.Seg`, generic `.norm`
in `generic_functions.R`): `Var::display_type` and `not_in_blacklist` used to treat *any* dot
as "already an R/S3 name", which a leading dot no longer means. Its generic carries no
`#' @export` (`typr-cli/src/project.rs`).

## Anomalies (pre-fix)

`.x` did not parse at all (`Unknown element`). With `__norm` defined and `.norm` used (or the
reverse) the two names were distinct bindings. In Project mode the roxygen tag of a typed hidden
function was `#' @method  norm Pt` (the generic name split on every dot), and it got no S3
suffix nor generic, so a second overload silently replaced the first.
