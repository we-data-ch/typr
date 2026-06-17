# Cross-module record subtype missing from generated S3 class vector

## Ce qui devrait se passer

`Circle` (circle.ty) has all the fields of `Position` (position.ty) plus more, so
`Circle <: Position` structurally. `move` is declared for `Position` only
(`@pub let move <- fn(self: Position, ...)`). Calling `.move(...)` on a `Circle`
value via UFCS type-checks fine, because the type-checker resolves the call
against the final, whole-program context where both aliases are visible.

For the R output to actually dispatch at runtime, `as.Circle`'s annotator must
tag the value with `"Position"` in its S3 class vector:
`class(x) <- c("Circle", "Position", "list")` (plus whatever other structural
supertypes apply), exactly like it already does when both `type` aliases live
in the *same* file (see `test_record_subtype_includes_supertype_in_s3_class`).

## Anomalies (→ règles expect.toml)

- Before the fix, `R/circle.R`'s `as.Circle` only emitted
  `class(x) <- c("Circle", "Record0", "list")` — no `"Position"` — because the
  structural-supertype scan in `processes/transpiling/mod.rs`
  (`supertype_entries`) only looked at `cont.aliases()`, which is scoped to the
  *current module's* local typing pass. `circle.ty` never imports anything
  from `position.ty`, so `Position` was invisible to that scan even though the
  type-checker (using the fully-merged global context) had already accepted
  the call.
- At the R level this means `UseMethod("move")` had no `move.Circle` nor
  `move.Position` to dispatch to, since the class vector lacked `"Position"`:
  `Error: no applicable method for 'move' applied to an object of class
  "c('Circle', 'Record0', 'list')"`.

## Root cause & fix

Each `Lang::Module` (one per `mod x;` file) is re-typed in its own scope and,
for encapsulation, only its *public* members bubble up into the surrounding
context (`processes/type_checking/mod.rs`, `Lang::Module` arm) — internal
`type` aliases are otherwise dropped entirely at the module boundary. That's
correct for term-level visibility, but R's S3 class system has no such
privacy: a method dispatch needs the *full* structural picture regardless of
which file declared which alias.

Fix: added a flat, whole-program registry `Context::record_aliases` that
every `type X <- list { ... }` alias is pushed into
(`Lang::Alias` arm), and that is explicitly carried across the module
boundary (`Context::merge_record_aliases`, called in the `Lang::Module` arm)
even though the rest of the inner typing context is discarded there. The
transpiler's supertype scan (`processes/transpiling/mod.rs`) now consults this
registry in addition to the locally-scoped `cont.aliases()`.

## Two unrelated, secondary issues found while debugging (not fixed here)

- `.move(10, 11)` (passing 2 scalar ints where `direction: Vec[2, int]` expects
  one vector) fails overload resolution, but the error message
  (`Function move<list{...}> not defined in this scope`) only shows the
  receiver's type, not the arity mismatch — easy to misread as a subtyping
  failure. Use `.move(c(10, 11))` instead.
- A comment line between two chained `.method()` calls (e.g. between
  `new_circle(4)` and `.move(...)`) silently breaks the dot-chain parse: the
  rest of the chain is dropped from the AST with no error at all. Reproduced
  outside this case; not investigated further.

## Localisation (#@case)

- `TypR/circle.ty:10` — : Was declared as a public function
- `TypR/main.ty:5` — : `move` was imported in the scope
- `TypR/main.ty:8` — : `Circle` is a subtype of `Position`
- `TypR/main.ty:10` — : `move` should works on type `Circle`
- `TypR/position.ty:1` — : `Position` type publicly defined here
- `TypR/position.ty:7` — : `move` function publicly defined here
