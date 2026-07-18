## Should happen

`type MatrixS4 <- Foreign<Any>;` with a real `Matrix::Matrix` S4 object read
back through `@extern base::readRDS`, bound with an explicit `let mat:
MatrixS4 <- ...` annotation, must stay a genuine S4 object: `nrow(mat)`
(via `@extern base::nrow`) should print `2`.

## Localisation (#@case)

**The most severe finding of Phase D.** Same upstream prerequisites as
`0018-foreign-lm-let-annotation` (`Foreign<T>` alias arity +
`@extern`-name shadowing), but this is the row where the annotated-`let`
class-append bug (also described in 0018) is actively destructive rather
than harmless: `let mat: MatrixS4 <- readRDS(...)` used to transpile to
`readRDS(...) |> as.MatrixS4()`, i.e. `x |> struct(c('MatrixS4',
'Foreign0', 'Foreign0', 'Any'))` — and R's own `class<-` semantics silently
strip S4-ness the instant a *second* class string is assigned onto an S4
object's class attribute (with a `Message d'avis` warning that the object
"will no longer be an S4 object" — locale-dependent text, not asserted on
directly in `expect.toml`). Every subsequent S4 slot access failed with "no
applicable method for `@` applied to an object of class...". This is not
`nrow`-specific: **any** S4 value that ever crosses an annotated `let` (or a
typed function return, sharing the same `get_type_anotation` helper) was
silently converted into a broken non-S4 object.

Fixed in `VarType::get_type_anotation` (`components/context/vartype.rs`):
a type that resolves through the alias chain to `Foreign<T>` (`opaque
Foreign<T> <- Any;`, see `resolves_to_foreign`) now gets `identity()`
instead of `as.X()` — the value is never touched. The same skip was
propagated to `checked_assertions.rs`'s `checked_descriptor` (this case
runs under `--checked`, see `case.toml`): asserting a Foreign value's class
against a TypR-side synthetic name would otherwise be a guaranteed false
positive for every legitimately-opaque foreign value, defeating the point
of the escape hatch.
