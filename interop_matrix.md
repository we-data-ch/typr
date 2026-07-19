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
| 1 | S3 external (`stats::lm`) | [0019](cases/0019-foreign-lm-function-arg) | [0020](cases/0020-foreign-lm-function-return) | [0018](cases/0018-foreign-lm-let-annotation) | [0044](cases/0044-foreign-s3-record-field) | [0050](cases/0050-foreign-s3-match) (wontfix, documented) | [0021](cases/0021-foreign-lm-dot-access-rejected) (wontfix, documented) | [0045](cases/0045-foreign-s3-pipe) | — | [0039](cases/0039-foreign-s3-module-boundary) | — |
| 2 | S4 (`Matrix::Matrix`) | [0023](cases/0023-foreign-s4-matrix-function-arg) | [0024](cases/0024-foreign-s4-matrix-function-return) | [0022](cases/0022-foreign-s4-matrix-let-annotation) | [0046](cases/0046-foreign-s4-record-field) | [0051](cases/0051-foreign-s4-match) (wontfix, documented) | [0025](cases/0025-foreign-s4-matrix-dot-access-rejected) (wontfix, documented) | [0047](cases/0047-foreign-s4-pipe) | — | [0040](cases/0040-foreign-s4-module-boundary) | — |
| 3 | RC (`setRefClass`) | [0031](cases/0031-foreign-rc-function-arg) | [0032](cases/0032-foreign-rc-function-return) | [0033](cases/0033-foreign-rc-let-annotation) | — | — | [0034](cases/0034-foreign-rc-dot-access-rejected) (wontfix, documented) | — | — | — | — |
| 4 | R6 (`R6::R6Class`) | [0035](cases/0035-foreign-r6-function-arg) | [0036](cases/0036-foreign-r6-function-return) | [0037](cases/0037-foreign-r6-let-annotation) | — | — | [0038](cases/0038-foreign-r6-dot-access-rejected) (wontfix, documented) | — | — | — | — |
| 5 | S7 (`S7::new_class`) | [0056](cases/0056-foreign-s7-function-arg) | [0057](cases/0057-foreign-s7-function-return) | [0058](cases/0058-foreign-s7-let-annotation) | — | — | [0059](cases/0059-foreign-s7-dot-access-rejected) (wontfix, documented) | — | — | — | — |
| 6 | Closure / pure environment | — | — | — | — | — | — | — | — | — | — |
| 7 | Base vector w/ attributes (ordered `factor`) | [0027](cases/0027-foreign-factor-function-arg) | [0028](cases/0028-foreign-factor-function-return) | [0026](cases/0026-foreign-factor-let-annotation) | [0048](cases/0048-foreign-factor-record-field) | [0052](cases/0052-foreign-factor-match) (wontfix, silent failure) | [0029](cases/0029-foreign-factor-dot-access-rejected) (wontfix, documented) | [0049](cases/0049-foreign-factor-pipe) | — | [0041](cases/0041-foreign-factor-module-boundary) | — |

Rows 3/4 (RC/R6) × columns {a,b,c,f} were filled in as `soundness_plan.md`
Phase D.1 (2026-07-18) — see "RC/R6 (Phase D.1)" below. Row 5 (S7) ×
columns {a,b,c,f} were filled in as Phase D.5 (2026-07-18, `S7` installed
via `install.packages("S7")` for this pass — absent before) — see "Row 5:
S7 (Phase D.5)" below. The remaining columns (d/e/g/h/i/j) across all
rows, plus row 6 (closure), are left `—` for a future pass.

## RC/R6 (Phase D.1): reference-semantics objects, zero new bugs

Unlike rows 1/2/7, filling rows 3/4 found no new interop bug — the four
fixes above (in particular #3, the `Foreign<T>`-family `identity()` skip)
already generalize correctly to both RC and R6:

- **RC (`setRefClass`) is genuinely S4** (`isS4()` is `TRUE`), structurally
  distinct from `Matrix::Matrix` (slotted numeric data) since RC wraps a
  mutable environment in an S4 shell. Confirmed directly: hand-applying the
  *old*, pre-fix `struct()` cast to the RC fixture
  (`class(rc) <- unique(c(class(rc), "Counter", "Foreign0", "Any"))`)
  reproduces R's own warning ("the result will no longer be an S4 object")
  and flips `isS4()` to `FALSE` — exactly the corruption mechanism `0022`
  found and fixed, confirmed to also threaten RC if the fix ever regressed.
  `0031`/`0032`/`0033` assert `isS4(x)` stays `TRUE` across all three
  boundaries.
- **R6 is NOT S4** (`isS4()` is `FALSE`) — a plain `environment` with class
  `c("Counter6", "R6")`, closer in shape to the base-with-attributes row
  (`factor`) than to the S4 rows, despite also having reference semantics
  like RC. `0035`/`0036`/`0037` assert `inherits(x, "Counter6")` stays
  `TRUE`.
- **Neither row's `a`/`b`/`c` cases use `print()` as the oracle**, unlike
  rows 1/2/7: a bare `readRDS()`'d RC/R6 fixture has no defining
  generator/package registered in the fresh sandbox R session that
  deserializes it (only the instance data was serialized), so RC's
  reference-class print dispatch and R6's `print.R6` method are both
  unavailable regardless of TypR — confirmed independently with a plain
  `Rscript -e 'print(readRDS(...))'` outside any TypR-generated code. This
  is a pure-R session/package-loading quirk orthogonal to the soundness
  property under test, so `isS4()`/`inherits()` are used as session-
  independent invariants instead. See each case's `expect.md` for the full
  reasoning.
- One self-inflicted false start, not a TypR bug: the first draft of
  `0031`/`0035` named the helper function `show`, colliding with
  `methods::show` (a real S4 generic) — correctly flagged by the Phase C
  r-name lint as a warning, and the resulting `UseMethod` stub shadowed S4
  dispatch, producing R's generic `<S4 Type Object>` fallback print.
  Renamed to `describe`; see `0031`'s `observed.txt`.

## Row 5: S7 (Phase D.5), zero new bugs

`cases/0056`-`0059` fill the a/b/c/f quartet for S7 (R-core's newer
official OOP system, installed for this pass via
`install.packages("S7")` — absent before) — zero new bugs, the existing
`Foreign<T>` machinery covers it without further changes. Two things make
this row structurally distinct from every other row already covered:

- **Not S4** (`isS4()` is `FALSE`, unlike RC/`Matrix::Matrix`) **and not a
  reference type** either (unlike RC/R6): `typeof()` is a new dedicated
  `"object"` type, but properties are set by value at construction time —
  no mutation-through-alias semantics to worry about.
- **`print()` works standalone**, unlike RC/R6: a bare `readRDS()`'d S7
  fixture prints correctly with no `library(S7)` call anywhere in the
  session (confirmed directly with a plain
  `Rscript -e 'print(readRDS(...))'`) — so this row's cases use `print()`
  as the oracle exactly like rows 1/2/7, without the `isS4()`/`inherits()`
  workaround D.1 needed for RC/R6.

## Columns d/g (D.4 Phase 1): clean, zero new bugs

`cases/0044`-`0049` cover record field (`Wrapper:{ model = m, label =
"test" }`) and pipe (`m |> print()`) for rows 1/2/7 — both work correctly
on the first attempt, no fix needed. Confirmed by inspecting the generated
R: the record constructor (`Wrapper(model = m, label = ...)`) passes the
`Foreign<T>`-family field straight through with no per-field cast, and
`validate_Wrapper`'s per-field type check is only emitted for the concrete
`label: char` field — consistent with the `identity()` skip already
established for `let`/return-type annotations (bug #3 above). The pipe
operator desugars to a plain function call before any type-directed
casting logic runs, so it was never expected to differ from the
already-covered plain function-argument case (`0019` etc.) — confirmed
rather than assumed.

## Column e (D.4 Phase 2): a documented hole, with one row markedly worse

All three `e` (`match` subject) cells are `status = "wontfix"` — the plan
doc's own risk analysis predicted this shape correctly before any case was
built: declaring `@extern`'s return type *directly* as a union
(`@extern base::readRDS: (path: char) -> Wrapped;` where `Wrapped <-
.Model(LmModel) | .Empty`) bypasses `Foreign<T>` entirely. TypR believes
the value is a properly tagged union, but the real foreign object never
carries TypR's synthetic union/tag class vector. `match` desugars to
`if (match_val__[[1]] == 'Model') { ... } else if (match_val__[[1]] ==
'Empty') { ... }`, and what `[[1]]` means for a real foreign value is
unrelated to TypR's tag convention:

- **Row 1 (S3 `lm`)**: `[[1]]` returns the real `coefficients` list
  element (length 2) — comparing a length-2 vector in an `if` condition is
  itself an R error (`la condition est de longueur > 1`). Loud crash.
- **Row 2 (S4 `Matrix`)**: S4 objects don't support `[[` at all
  (`this S4 class is not subsettable`). Loud crash, even earlier.
- **Row 7 (`factor`)**: `[[1]]` returns the factor's first element as a
  length-1 factor (`"lo"`), and factor's `==` operator returns plain
  `FALSE` (not `NA`/error) when compared against a string outside its
  levels. **Neither `if` branch fires, there's no `else`, and execution
  silently continues past the whole `match` block — no error, no output,
  exit code 0.** Confirmed by appending a trailing `print("reached end")`:
  it's the only thing printed. This silent-failure shape is strictly worse
  than the other two rows' loud crashes and is the strongest argument in
  this grid for rejecting `@extern ... -> UnionType` (when the union isn't
  itself `Foreign<T>`-backed) at compile time in a future pass, rather than
  leaving this purely as a documented hole — see `0052`'s `expect.md`.

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

## Column i (module boundary): a real bug found and fixed (D.2)

Unlike column f, column i was a genuine bug, not a documented gap. A `@pub
fn` declared inside `module M { ... }` whose dispatch parameter is a
`Foreign<T>`-family alias (or a pure interface — same mechanism) compiles to
a method suffixed by the TypR-side alias name (e.g. `describe.Foreign0`),
plus a `describe.default <- describe.Foreign0` fallback binding (fix #4 of
the four original bugs above) — since a real foreign value's runtime class
never matches the synthetic `.Foreign0` suffix, `.default` is the *only*
method that can ever catch it.

The module post-processing block in `transpiling/mod.rs` already re-exported
`describe.Foreign0` itself out of the module's `local({...})` scope (both
into the module env, `M$describe.Foreign0 <- describe.Foreign0`, and back
out to a top-level binding, `describe.Foreign0 <- M$describe.Foreign0`, so
`UseMethod` dispatch from any importing file can find it) — but never did
the same for `describe.default`. The fallback binding existed, but only
*inside* `local({...})`, invisible everywhere else. Confirmed directly: with
the fix reverted and the incremental-build cache cleared (`--no-incremental`
— otherwise the cache silently reuses already-fixed output), calling
`describe(lm_value)` from outside the module fails with `Erreur dans
UseMethod("describe") : pas de méthode pour 'describe' applicable pour un
objet de classe "lm"`.

Fixed by adding the identical export + re-expose treatment for
`raw_name.default` right next to the existing `typed_name` treatment, gated
on the same `is_foreign_dispatch || (pure interface facet)` condition
already used by the `Lang::Let` arm that originally emits the `.default`
binding (`crates/typr-core/src/processes/transpiling/mod.rs`, module
post-processing loop). Covered by a dedicated unit test
(`transpiling::tests::test_module_foreign_dispatch_default_exported_outside_local`)
plus `cases/0039`-`0041` (rows 1, 2, 7 — RC/R6/S7/closure rows not yet
attempted for this column).

## Column j (generic instantiation): blocked by a general, non-`Foreign<T>` bug (D.3)

Attempting `id(foreign_val)` (Phase D.3, `soundness_plan.md`) immediately
hit two bugs, **neither of them `Foreign<T>`-specific** — both reproduce
identically for a plain `int` argument, no foreign value or `@extern`
involved at all:

1. **Fixed**: a top-level generic function's own definition
   (`let id <- fn(x: T): T { x };`) failed to even *build* in project mode
   — its self-cast referenced an auto-numbered `as.FunctionN` that
   `Context::get_type_anotations()` correctly never emits for a
   generic-containing type, but the single-site `get_type_anotation()`
   helper had no matching skip. Fixed in
   `components/context/vartype.rs`, covered by
   `cases/0042-generic-function-own-type-cast-undefined`.
2. **Open, deliberately not fixed here**: once `id` builds, *calling* it on
   any concrete value still fails — TypR's single-definition S3 dispatch
   model gives a bare, unresolved `Generic` dispatch parameter only the
   literal method suffix `.Generic`, which no real runtime class ever
   matches, and the existing `.default` fallback mechanism (used for
   `Foreign<T>` and pure-interface params) deliberately doesn't cover this
   shape. Catalogued in `cases/0043-generic-function-dispatch-no-default`
   with the full reasoning for why a fix is out of scope for Phase D.

Since bug 2 blocks calling *any* plain generic function at all, there is no
way to observe `Foreign<T>`-specific behavior at this column independent of
the general gap — column j stays `—` (blocked) for every row until
`cases/0043` has an owner.

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

- **Column i, rows 3/4/5/6 (RC/R6/S7/closure × module boundary)**: the
  structural fix (D.2 above) is row-agnostic — it patches the export path,
  not anything row-specific — but only rows 1/2/7 have been exercised
  against it so far.
- **Column j (generic instantiation)**: blocked project-wide by
  `cases/0043-generic-function-dispatch-no-default` (see write-up above) —
  not a next step for this grid until that general bug has an owner.
- **Columns d/e/g done for rows 1/2/7** (D.4 Phase 1+2, see write-ups
  above) — d/g clean, e a documented (one-row-silent) hole. Not yet
  attempted for rows 3/4/5/6.
- **Column h (`for` iterable)**: not yet attempted for any row — the plan
  doc's own priority ranking put this last ("rare avec étranger").
- **Row 5 (S7)**: a/b/c/f quartet done (D.5, see write-up above); columns
  d/e/g/h/i/j not yet attempted for this row.
- **Row 6 (closure/pure environment)**: a plain R closure captured by a
  factory function — likely the simplest remaining row (no class vector at
  all to corrupt), but not yet verified.

Each of these, once attempted, becomes either a new `cases/NNNN-...` entry
(bug or confirmed-working) or an `n/a` with a one-line reason — the same
discipline used for row 5 above.
