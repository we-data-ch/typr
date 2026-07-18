# interop_matrix.md — S4 / R6 / RC / S7 / base conformity grid

Phase D of `soundness_transpilation.md`. Each cell is either a `cases/NNNN-...`
case (a real foreign R value crossing a real TypR boundary, `cmd = "run"`,
`layer = "r-run"`, oracle on `@run` = the captured stdout+stderr of a real
`Rscript` execution) or a documented hole (`—` = not attempted yet, `n/a` =
doesn't apply). The value of this grid is as much in its visible holes as in
its green cells — see `soundness_transpilation.md` Phase D for the rationale.

## Mechanism

Every case uses the already-implemented `Foreign<T>` / `@extern` interop
layer (`crates/typr-cli/configs/std/foreign.ty`, `configs/src/std.R`
"Interop" sections):

```typr
type LmModel <- Foreign<Any>;
@extern base::readRDS: (path: char) -> LmModel;

let m: LmModel <- readRDS("fixtures/lm.rds");
```

Foreign values are pre-baked into small `.rds` fixtures (`tools/
gen_interop_fixtures.R`, run by hand, result committed into each case's own
`repro/fixtures/`, same convention as `tools/gen_r_name_db.R`) rather than
constructed by a hand-written companion `.R` file sourced at run time: TypR's
project loader (`load_module.R`) sources every `R/*.R` file into its own
*isolated* environment and only shares bindings across files via `@include`
tags that TypR itself generates — a hand-placed R file would need explicit
plumbing to be reachable from `main.ty`'s environment at all, which
`readRDS` sidesteps entirely while still exercising a completely real,
externally-produced R value.

## Four real bugs found and fixed while wiring this up

Building the very first cell (`type LmModel <- Foreign<Any>;` — the exact
usage pattern documented in `foreign.ty`'s own comment) immediately failed,
in four different ways, once each was fixed in turn:

1. **`Foreign<T>` unusable at all** — any `type X <- Foreign<Any>;` raised a
   bogus `AliasArityMismatch` ("Foreign expects 0 type argument, found 1").
   `collect_undefined_aliases` (`processes/type_checking/let_expression.rs`)
   skipped arity checking only when the *reference itself* was written
   `opaque X` (practically never at a use site), not when the *resolved
   target* was declared opaque elsewhere — which is `Foreign<T>`'s actual
   shape. Fixed by checking the target alias's own opacity.
2. **`@extern` names shadowed by their own dispatch stub** — a bare
   `@extern base::readRDS: ...;` got a `readRDS <- function(x, ...)
   UseMethod('readRDS', x)` stub emitted into `generic_functions.R` (and
   flagged by the Phase C lint), which would shadow the real `base::readRDS`
   the call site relies on — the `nlevels` bug shape, but for `@extern`
   names. Fixed by excluding `Context.extern_fns`/`import_from_fns` names
   from `Context::get_all_generic_functions`.
3. **Annotated `let`/typed return silently corrupts foreign values** — `let
   x: T <- expr` (and a typed function return, same helper) piped the value
   through `as.T()`, which appends TypR-internal classes onto the object's
   *real* class vector via `struct()`/`class(x) <-`. Harmless for S3 objects
   (dispatch still finds the original class first) but **actively breaks S4
   objects**: R silently drops S4-ness the instant a second class string is
   assigned, so any subsequent `@`-slot access failed. Fixed by making
   `Foreign<T>`-family types get `identity()` instead of `as.X()`
   (`VarType::get_type_anotation`), propagated to `--checked`'s
   `checked_descriptor` too (a synthetic-class assertion on a foreign value
   would otherwise be a guaranteed false positive).
4. **Plain functions taking a `Foreign<T>` argument are unreachable** — a
   top-level `let f <- fn(x: T): R {...}` is emitted as an S3 method keyed
   on `T`'s (possibly monomorphized) alias name (e.g. `f.Foreign0`), but a
   real foreign value's runtime class is whatever R gave it — never that
   synthetic name — so the method could never be dispatched to. Fixed by
   extending the existing "interface-pure first param also emits a
   `.default` fallback" mechanism to also cover `Foreign<T>`-family
   dispatch parameters.

Full write-up and code pointers: `expect.md` in each `cases/001[89]-...`/
`cases/002[0-9]-...` case, and the four commits touching
`let_expression.rs` / `context/mod.rs` / `vartype.rs` /
`checked_assertions.rs` / `transpiling/mod.rs`.

## The grid

Rows = provenance of the value, columns = TypR boundary crossed. v1
priority per `soundness_transpilation.md`: rows **1, 2, 7** × columns
**a, b, c, f** (the 12 cells below) — the rest are recorded as `—` for a
future pass, `n/a` where the combination doesn't apply.

| # | Row (value) | a: fn argument | b: fn return | c: `let x: T <-` | d: record field | e: `match` subject | f: `.`/`$` access | g: pipe | h: `for` iterable | i: module boundary | j: generic instantiation |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 1 | S3 external (`stats::lm`) | [0019](cases/0019-foreign-lm-function-arg) | [0020](cases/0020-foreign-lm-function-return) | [0018](cases/0018-foreign-lm-let-annotation) | — | — | [0021](cases/0021-foreign-lm-dot-access-rejected) (wontfix, documented) | — | — | — | — |
| 2 | S4 (`Matrix::Matrix`) | [0023](cases/0023-foreign-s4-matrix-function-arg) | [0024](cases/0024-foreign-s4-matrix-function-return) | [0022](cases/0022-foreign-s4-matrix-let-annotation) | — | — | [0025](cases/0025-foreign-s4-matrix-dot-access-rejected) (wontfix, documented) | — | — | — | — |
| 3 | RC (`setRefClass`) | — | — | — | — | — | — | — | — | — | — |
| 4 | R6 (`R6::R6Class`) | — | — | — | — | — | — | — | — | — | — |
| 5 | S7 (`S7::new_class`) | n/a | n/a | n/a | n/a | n/a | n/a | n/a | n/a | n/a | n/a |
| 6 | Closure / pure environment | — | — | — | — | — | — | — | — | — | — |
| 7 | Base vector w/ attributes (ordered `factor`) | [0027](cases/0027-foreign-factor-function-arg) | [0028](cases/0028-foreign-factor-function-return) | [0026](cases/0026-foreign-factor-let-annotation) | — | — | [0029](cases/0029-foreign-factor-dot-access-rejected) (wontfix, documented) | — | — | — | — |

Row 5 (S7) is marked `n/a` project-wide, not per-cell `—`: the `S7` package
is not installed in the dev/CI environment this matrix was built in
(`Rscript -e 'installed.packages()'` confirms `R6`/`Matrix`/`methods`
present, `S7` absent) — recorded as a known gap rather than silently
skipped. `R6`/`Matrix` **are** installed; rows 3/4 (RC/R6) and the
remaining columns (d/e/g/h/i/j) across all rows are left `—` for a future
pass — the plan doc's own v1 scope is exactly rows {1,2,7} × columns
{a,b,c,f} (12 cells), which is what's filled in here.

## Column f: a documented hole, not a bug

All three `f` (dot/dollar access) cells are `status = "wontfix"` cases, not
open bugs: `Foreign<T>` reduces to `Any`, which carries no structural field
information, so `val.field` can never type-check — TypR has no way to know
a real `lm`/S4/`factor` object has a given field/slot without an explicit
`@extern` accessor (`stats::coef`, `methods::slot`, `base::levels`, …, as
demonstrated indirectly by the `a`/`b`/`c` cells in the same row). Closing
this gap would mean either letting users declare partial structural shapes
for foreign values, or dropping compile-time field checking for `.`/`$`
entirely — both real design decisions, out of scope here. Each case pins
the exact rejection message so it stays visible if either direction is
pursued later.

## `--checked` interaction

Per the plan doc: "chaque case de la matrice se lance aussi sous
`--checked`" — the three `c`-column cases (`0018`, `0022`, `0026`) set
`checked = true` in `case.toml` (a small addition to `typr case`:
`CaseMeta.checked` appends `--checked` to the replayed `cmd` when set, for
`build`/`run` cases). The policy question the plan doc raises — "que doit
faire `typr_assert_type` face à un objet R6/S4 annoté d'un type précis ?" —
is answered by bug fix #3 above: **no assertion at all**. A `Foreign<T>`
value's real class is never controlled by TypR, so asserting it against a
TypR-side synthetic class name would be a guaranteed false positive on
every legitimate use of the escape hatch, not a real soundness check —
`checked_descriptor` (`processes/transpiling/checked_assertions.rs`) now
returns `None` (pass-through, no assertion emitted) for any type that
resolves to `Foreign<T>`, symmetric with the `as.X` skip. All three
`checked = true` cases pass with zero `[typr --checked]` output, confirming
this in the harness itself rather than as a one-off manual check.

## Backlog (open cells)

Every `—` cell above is this project's interop backlog, prioritized
loosely in the order a real package integration would hit them:

- **Rows 3/4 (RC, R6)**: both packages are installed; the next natural
  cases, since reference-semantics objects (mutation through a shared
  environment) may interact differently with `to_native`/`from_native`
  than the immutable S3/S4/attributed-vector rows already covered.
- **Column i (module boundary)**: does a `Foreign<T>` value survive being
  passed to a `@pub fn` inside a `module M { ... }`? The dispatch-suffix
  bug (#4 above) was found and fixed for plain top-level functions; module
  member functions go through a related but distinct naming path
  (`Lang::to_arg_value`) not yet checked against `Foreign<T>`.
- **Column j (generic instantiation)**: `id(foreign_val)` — does a generic
  function's monomorphization ever try to derive/assert a concrete class
  for a `Foreign<T>` argument the way it does for ordinary generics?
- **Columns d/e/g/h (record field, `match` subject, pipe, `for` iterable)**:
  not yet attempted for any row.
- **Row 5 (S7)**: install the package and repeat the a/b/c/f quartet.
- **Row 6 (closure/pure environment)**: a plain R closure captured by a
  factory function — likely the simplest remaining row (no class vector at
  all to corrupt), but not yet verified.

Each of these, once attempted, becomes either a new `cases/NNNN-...` entry
(bug or confirmed-working) or an `n/a` with a one-line reason — the same
discipline used for row 5 above.
