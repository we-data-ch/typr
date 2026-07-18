# cross-module-interface-arg-map-generic-unresolved

## Ce qui DEVRAIT se passer

`Object <- Id & Animable` (record & interface intersection), `Circle` satisfies `Object`
structurally, and `circle.ty` (a *different* module) declares a free function
`animate: (self: Circle, animation: Animation) -> Circle` implementing `Animable`'s
`animate: (Self, Animation) -> Self` method for `Circle`.

`storyboard.ty` imports `animate` via `use circle::animate;` and calls it through a generic
higher-order function on an array of the *interface* type itself:

```typr
let objs = self$objects |> map(\(x) animate(x, animation));   // self$objects: [Object]
```

Since every element of `[Object]` genuinely satisfies `Animable`'s `animate` method (by
construction — `Object` *is* `Id & Animable`), `animate(x, animation)` should type-check as
`Object -> Object` here (dispatching through the interface's own declared signature, `Self` =
`Object`), and `map`'s generic return should resolve to `[Object]`, matching the `objects:
[Object]` field it's later assigned into (`Snapshot:{ ..., objects: objs }`).

## Anomalie observée

Both `objs`'s inferred type and the two record field validations that consume it come back as
`[#N, U]` — a **fully unresolved generic**, not `[Object]`. `map`'s own signature is `(a: [#N,
T], f: (T) -> U) -> [#N, U]`; `U` should have been unified with `animate`'s resolved return type
(`Object`) while type-checking the lambda `\(x) animate(x, animation)`, but stays a bare
generic — meaning `animate(x, animation)`'s call itself isn't resolving to a concrete `Object ->
Object` signature at all when `x`'s static type is the *interface-bearing alias* `Object`
directly (not a further-known concrete subtype), and `animate` was found via a cross-module
`use`.

**Confirmed NOT reproducible** with the same code in a single file (no module boundary) — see
`crates/typr-core` tests / the investigation below. The module boundary is load-bearing for the
bug: `use circle::animate;` bringing in a function typed for a *specific* record (`Circle`) whose
resolution needs to go through `Object`'s *interface* facet (`Animable`), rather than the record
facet, seems to be exactly where the existing structural-interface-satisfaction call path (already
patched several times for `cases/0009`/`0010`'s original bug, `interface_facet`/
`has_interface_facet` in `function_application.rs`) doesn't cover a value whose *own* static type
already *is* the interface (as opposed to a concrete record known to satisfy it).

## Localisation

Reduced from `cases/0010-array-covariance-record-subtype/repro`'s `storyboard.ty`, which hits
this while type-checking `snapshot`'s body. Confirmed present at HEAD (2026-07-18) and **absent**
at commit `c133bd1` (the last commit before `efd627f "finished adding new edge cases"`, which
first shows the failure) — so this is a real regression introduced somewhere in that commit's
large diff (`type_checking/mod.rs` +835, `constructor_call.rs` +345 lines), not a pre-existing
gap that was merely uncovered by better validation. `constructor_call.rs`'s new field-type
validation (added in the same commit) is what surfaces it as a hard error now — plausible that
`objs` already resolved to `[#N, U]` before that commit too, just without anything checking a
constructor's field types strictly enough to notice. Not yet root-caused to an exact line/commit
hunk; `function_application.rs` (where the interface-facet dispatch logic lives) was **not**
touched by `efd627f`, so the break is more likely in how `mod.rs`'s generic/lambda-application
typing (`Lang::FunctionApp` inside `map`'s higher-order argument) resolves `U`, not in the
interface-facet matching itself.

## Not attempted here

Fixing this requires tracing exactly how `Lang::FunctionApp`'s generic instantiation for `map`'s
lambda parameter resolves `f`'s return type when `f`'s first param's *declared* type (`Circle`)
differs from the *call-site* argument's static type (`Object`, a supertype via interface
satisfaction) — likely in `function_application.rs`'s generic-unification path
(`try_named_generic_match`/`collect_named_generics` or the interface-facet filters), reconciling
it with the cross-module function lookup. Left open rather than rushed, given how many prior
rounds of careful, scoped interface-dispatch fixes this codebase already needed (see
`project_interface_dispatch_hybrid`/`project_interface_return_position` in memory) — a hasty fix
here risks the same "fixed X, broke Y" pattern that produced `efd627f`'s own regression.
