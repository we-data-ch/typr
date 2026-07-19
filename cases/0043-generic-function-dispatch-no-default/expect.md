## Should happen

`id(m)` where `id <- fn(x: T): T { x }` and `m` is a plain `int` must
return `m` unchanged — this is the canonical generic-identity example from
CLAUDE.md's own "TypR Language Quick Reference".

## Localisation (#@case)

Found while attempting Phase D.3 of `soundness_plan.md` (column j: generic
instantiation, `id(foreign_val)`) — **not** `Foreign<T>`-specific. This is a
second, deeper bug than `0042-generic-function-own-type-cast-undefined`
(fixed): even once `id` *builds* cleanly, *calling* it on any concrete
value fails at runtime.

TypR's dispatch model is single-definition S3: a typed function is emitted
once, as a method suffixed by its dispatch parameter's class
(`Context::get_class`/`get_class_unquoted`), with `UseMethod` doing runtime
dispatch — there is no per-call-site monomorphization into separate R
functions. For a dispatch parameter whose type is a genuinely bare,
unresolved `Generic` (not wrapped in a record/array/alias that would give
it a concrete monomorphized class), the only class TypR can derive at
transpile time is the literal string `"Generic"` — so `id` is emitted as
`id.Generic` alone. A real value's runtime class is never `"Generic"`
(`m` here is `c('integer', 'Integer', 'Any')`), so `UseMethod("id")` can
never find a matching method.

The existing `.default` fallback mechanism (used for pure-interface and
`Foreign<T>`-family dispatch parameters — see `soundness_transpilation.md`
Phase D bug #4, and its module-boundary counterpart in D.2) does **not**
cover this case: its trigger condition
(`is_foreign_dispatch || (interface_facet && !record_facet)` in
`processes/transpiling/mod.rs`'s `Lang::Let` arm) is specific to those two
shapes and deliberately does not fire for a bare `Type::Generic`.

Confirmed this has never been caught by the existing test suite: every
existing test exercising `let id <- fn(x: T): T { x };`
(`crates/typr-core/tests/edge_case_matrix.rs`'s `ID_PRELUDE`,
`processes/type_checking/function_application.rs`'s `id`-based tests) only
checks *type-checking* (`typing_with_errors`/`FluentParser::parse_type_next`),
never executes the transpiled R — so the runtime dispatch gap was invisible
to the whole suite.

**Not fixed here** — deliberately scoped out of Phase D.3 (`soundness_plan.md`):
fixing this properly means deciding a dispatch strategy for bare-generic
parameters (extend the existing `.default` fallback to cover them too, at
the cost of `id.default` silently catching *any* class including ones a
more specific overload should have matched instead; or introduce real
per-call-site monomorphization, a much larger change). Left open,
catalogued precisely so a future fix has a concrete regression target.
