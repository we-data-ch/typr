# untyped-r-function-callable

Source: `rfcs/0028-calling-untyped-r-functions.md`, accepted 2026-09-12
(we-data-ch/typr#28, tracking we-data-ch/typr#29).

## Ce qui devrait se passer

`function(a, b) { ... }` parses to `Lang::RFunction` and used to type as the placeholder
`Type::UnknownFunction`, which carries no parameter list — so any call with at least one
argument failed with `No signature of function 'my_addition' matches this call ... () ->
UnknownFunction`, even though the emitted R was already correct. Per the RFC, `Lang::RFunction`
with `n` parameters now types as `(Any, ..., Any) -> Any`: arity is checked (exactly `n`
arguments), argument types are not, and the result must be brought back into the type system
explicitly with `as!` — the same shape as the rest of TypR's opaque values.

This is the exact `docs/philosophy/intro.md` example ("weak on safety, strong on freedom"),
which the RFC's Motivation section shows failing to compile on 0.5.10.

## Vérification

Implemented alongside this case: `Lang::RFunction`'s typing rule
(`processes/type_checking/mod.rs`) now builds a real `Type::Function` instead of
`Type::UnknownFunction`. The emitted R is unchanged — `my_addition(num1, num2)` — since only the
type checker ever rejected this call.

## Statut

Kept as a regression net for the RFC's core guarantee: an untyped R function must stay callable.
