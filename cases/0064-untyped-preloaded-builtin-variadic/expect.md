# untyped-preloaded-builtin-variadic

Source: `rfcs/0028-calling-untyped-r-functions.md`, accepted 2026-09-12
(we-data-ch/typr#28, tracking we-data-ch/typr#29), Motivation and point 4 of the Reference-level
explanation ("Point 4 is the load-bearing half").

## Ce qui devrait se passer

The untyped names from `functions_R.txt` (`Position`, `t`, `Reduce`, …) are preloaded as
`(Any, UnknownFunction)` so that referring to them is not an "undefined variable" error.
Before the RFC, `Type::UnknownFunction` was effectively 0-ary (`FunctionType::new(VecType::Empty,
vec![], ...)`), so `Position()` type-checked but `Position(1, 2)` failed the same way a
user-written `function(...)` did. Fixing only `Lang::RFunction` (case
`untyped-r-function-callable`) without also fixing `UnknownFunction` would leave the same bug
reachable through every untyped base-R name — this is the "confusing version of the bug rather
than a fix" the RFC warns about. `UnknownFunction` must be variadic: any number of arguments,
result `Any`.

## Vérification

Implemented alongside case `untyped-r-function-callable`: `Type::UnknownFunction`'s conversion to
`FunctionType` (`components/type/mod.rs`'s `to_function_type`, and the matching
`TryFrom<Type> for FunctionType` arm in `components/type/function_type.rs`) now produces a single
variadic `Any` parameter returning `Any`, instead of zero parameters.

## Statut

Kept as a regression net: every untyped base-R builtin must stay callable with arguments, not
just referenceable by name.
