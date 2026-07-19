## Should happen

`let id <- fn(x: T): T { x };` — a plain top-level generic function, exactly
the canonical example in CLAUDE.md's own "TypR Language Quick Reference" —
must build successfully in project mode (`typr build`), including the
`devtools::document()` step (which sources every generated `R/*.R` file via
`pkgload::load_all()`).

## Localisation (#@case)

Found while attempting Phase D.3 of `soundness_plan.md` (column j: generic
instantiation, `id(foreign_val)`) — **not** `Foreign<T>`-specific, this
reproduces for a plain `int` argument (never even called: the broken
reference is emitted purely from *declaring* `id`, no call site needed).

Root cause: `Lang::Function`'s own transpilation
(`processes/transpiling/mod.rs`, ~line 839) unconditionally wraps the
function value in its own self-cast, `cont.get_type_anotation(&fn_type.into())`
— for `id`, `fn_type` is `(T) -> T`, a type still containing an unresolved
generic. During type-checking, this exact structural type gets hoisted and
auto-registered under a numbered alias (`FunctionN`, e.g. `Function15`,
the same `RecordN`/`ArrayN`-style numbering used elsewhere — see
`project_alias_hoisting` in the codebase's own conventions). But
`Context::get_type_anotations()` (`components/context/mod.rs`, the function
that actually *writes* `as.<Name> <- ...` definitions into `types.R`)
deliberately filters out any alias whose underlying type
`.has_generic()` (`.filter(|(_, typ)| !typ.has_generic())`) — a generic type
has no fixed runtime shape to validate against, so no definition is ever
emitted for `Function15`. The singular per-call-site helper
`VarType::get_type_anotation()` (`components/context/vartype.rs`) had no
matching skip: it just looks up any alias whose stored `Type` structurally
matches, finds `Function15`, and emits `as.Function15()` regardless —
producing a call to a function that was never defined. Confirmed by a
targeted unit test
(`components/type/mod.rs::tests::test_concrete_function_type_has_no_generic`)
documenting the *sibling*, already-fixed bug (`bug_function_type_has_generic_false_positive`
in project memory): that fix made `has_generic()` correctly `true` for
genuinely generic function types like `id`'s, which is exactly what now
triggers this new gap (the skip works correctly in the emitter, but the
call site wasn't updated to skip in step).

**Fixed** by adding the identical `has_generic()` skip to
`VarType::get_type_anotation()`, falling back to the always-defined
`as.Generic()` passthrough — mirroring the `Foreign<T>` → `identity()`
precedent in the very same function.

See `0043-generic-function-dispatch-no-default` for a second, deeper,
**still-open** bug found immediately after fixing this one: even once `id`
builds, *calling* it on any concrete value fails, because its S3 dispatch
method is only ever registered under the literal suffix `.Generic`, with no
`.default` fallback.
