---
name: case
description: Import a TypR bug into cases/ and drive it from repro to fix. Use when the user hits a bug in a TypR project and wants it turned into a reproducible case (from a `typr case snapshot` bundle, or directly from a live project directory plus a description of what's wrong) and then fixed here. Applies to the typr repo root (cases/ lives there).
---

# /case — from a broken project to a fixed, cataloged case

Shortcuts the transition described in `cases/README.md` and CLAUDE.md's "Reproducible bug
catalog" section: today, going from "I hit a bug in my TypR project" to "Claude has a
reproducible case and is fixing it" needs several manual steps (mark the code, `typr case
snapshot`, move the bundle, hand-write `expect.toml`). This skill does all of that in one
invocation, and — the reason it exists — accepts a **live project directory plus your own prose
description of the problem** as an alternative to pre-marking the code with `#@case` comments.

## Reading `args`

`args` is free text, not a fixed flag syntax. It contains, in whatever order the user typed it:

- **A filesystem path**, to one of two things:
  - a `<slug>.case/` bundle already produced by `typr case snapshot` (has `case.toml` +
    `repro/` at its top level) — **bundle mode**;
  - a live TypR project directory (has `TypR/main.ty`, no `case.toml`) that has *not* been
    snapshotted — **project mode**.
- **A prose description** of the bug — what's wrong, what should happen instead. In project
  mode this is the *only* source of the problem description (no `#@case` marker required). In
  bundle mode, treat it as additional context layered on top of whatever `snapshot` already
  captured in `.case.md`/`expect.md`.
- Optionally, hints about where the bug lives (a runtime crash vs. wrong generated R vs. a type
  error) — use these to pick `cmd`/`layer`, see step 2/3 below.

If `args` contains no resolvable path, stop and ask the user for one via AskUserQuestion —
don't guess a path. A description alone, with nothing to build a `repro/` from, isn't enough.

## Steps

1. **Resolve the path and mode.** Check for `case.toml` at the path's top level → bundle mode.
   Check for `TypR/main.ty` with no `case.toml` → project mode. If the path exists but matches
   neither shape, say so and ask what it is rather than guessing.

2. **Bundle mode.**
   - Count existing dirs under `cases/` in *this* repo, zero-pad `+1` to 4 digits → `NNNN`.
   - `mv <bundle-path> cases/<NNNN>-<slug>` (slug = the bundle's dirname minus the trailing
     `.case`). Don't re-copy or re-curate `repro/` — `snapshot` already curated it
     (`TypR/` + `DESCRIPTION`/`NAMESPACE`, no `renv/`/`.git/`).
   - If the user's prose in `args` adds anything not already in the bundle's `expect.md`,
     append it there under a `## Précisions (session)` heading instead of discarding it.

3. **Project mode.**
   - From the prose, infer:
     - `cmd`/`layer`: mentions of running the program, wrong printed/returned values, a crash
       "when I run it", wrong dispatch at runtime → `cmd=run`, `layer=r-run`. Mentions of wrong
       *generated R* (visible without running it) → `cmd=build`, `layer=transpile`. A pure type
       error caught before transpilation → `cmd=check`, `layer=type`. Default, if unclear:
       `cmd=build`, `layer=transpile`.
     - `slug`: a short kebab-case handle (2-5 words) summarizing the bug.
   - From the typr repo root: `cargo build --workspace` (get a current binary), then:
     ```
     target/debug/typr case add <slug> --from <project-dir> --cmd <cmd> --layer <layer>
     ```
     This curates the copy, scaffolds `case.toml`, and captures a real `observed.txt` (a build
     or run of the case as it stands right now).
   - Rewrite `cases/<NNNN>-<slug>/expect.md`: a `# <slug>` heading, a "## Ce qui devrait se
     passer" section paraphrasing the user's prose, and an "## Anomalies" section that connects
     that prose to what `observed.txt` actually shows going wrong (don't just paste the user's
     text verbatim — ground it in the captured output).

4. **Write `expect.toml` rules** (both modes). Read `observed.txt` and `expect.md`. Translate
   each anomaly into `must_contain`/`must_not_contain` rules — substrings of real captured
   output, never hand-written expected R (see `cases/README.md`, "L'oracle : on ne décrit jamais
   le R attendu en entier"). For `layer = "r-run"` cases, target `file = "@run"` — this greps
   the captured stdout+stderr of an actual `Rscript` execution rather than a generated file
   (e.g. `must_not_contain = "no applicable method"`). Skim 2-3 existing `cases/*/expect.toml`
   first to match the house style before writing new ones.

5. **Confirm it reproduces**: `target/debug/typr case run <slug>` from the repo root. Expect
   `OPEN`. If it comes back `READY`, or fails in a way that doesn't match the described bug, the
   rules are wrong (too loose, or targeting the wrong file) — fix them before touching
   typr-core. Fixing code against a rule that isn't actually exercising the bug proves nothing.

6. **Fix it.** Normal typr-core debugging: identify the failing pipeline stage (parsing / type-
   checking / transpiling — CLAUDE.md's "Debugging Workflow" section has the playbook), fix it,
   `cargo build --workspace`.

7. **Confirm the fix**: `target/debug/typr case run <slug>` → expect `READY`. Loop 6-7 until it is.

8. **Freeze**: `target/debug/typr case freeze <slug>` (captures golden `R/*.R`, flips `status`
   to `fixed`). Then `target/debug/typr case run --status fixed` as a regression guard — must
   print `PASS` and exit 0.

9. **Report**: the case id, the root cause, what changed and where, and — if this bug class is
   generic enough to deserve one — the `cargo test -p typr-core <module>::` command for a
   proper unit test beyond the case harness.

## Notes

- Run every `typr case …` command from the typr repo root: `cases/` and the sandboxing in
  `cases.rs` are resolved relative to cwd.
- `cases/README.md` and CLAUDE.md's "Reproducible bug catalog" section are the canonical docs
  for how the harness works; re-read them if anything here seems out of date with the code.
- The oracle is always rules grepped against real captured output (generated R, or — for
  `r-run` cases — real `Rscript` stdout+stderr via `file = "@run"`). Never author the expected R
  by hand, and never golden-diff `@run` (its capture includes CLI progress timings that vary
  every run — grep only).
- `case.toml` has a `checked = true` field (default `false`): appends `--checked` to the
  replayed `cmd` (`build`/`run` only). Set it when the bug is specifically about
  `typr_assert_type` runtime boundary checks (soundness_transpilation.md Phase A) — e.g. a
  false-positive/false-negative assertion, not just "the program crashes".

### `@extern`/`Foreign<T>` bugs (interop with real R values)

If the bug involves a value from outside TypR's own type system — an S3/S4/RC/R6 object, a
third-party package, anything reached via `@extern` — this is the `interop_matrix.md` (repo
root) family, soundness_transpilation.md Phase D. Before scaffolding a new case:

- **Check the matrix first.** The cell (provenance row × TypR-boundary column) may already be
  covered by an existing `cases/00NN-...` — read `interop_matrix.md`'s grid and its "Backlog"
  section before assuming this is new.
- **`typr case add --from <dir>` won't carry extra files.** It curates only `TypR/` +
  `DESCRIPTION`/`NAMESPACE` (`copy_repro_curated` in `cases.rs`) — any `repro/fixtures/*.rds` the
  repro needs must be added by hand into the scaffolded `repro/` afterward, same as the existing
  `cases/0018`-`0029` do.
- **Never construct the foreign value from TypR source or a hand-written companion `.R` file
  sourced at run.** TypR's project loader (`load_module.R`) sources every `R/*.R` into its own
  *isolated* environment, only sharing bindings via `@include` tags TypR itself generates — a
  hand-placed R file isn't reachable from `main.ty` without that plumbing. Instead: generate the
  value once with a plain R script (extend `tools/gen_interop_fixtures.R` if it's a new fixture,
  same "run by hand, `.rds` committed" convention as `tools/gen_r_name_db.R`) and read it back
  via `@extern base::readRDS: (path: char) -> Foreign<Any>;`.
- **After fixing**, update the matching cell in `interop_matrix.md` to link the new case (or add
  a row/column if this opens a genuinely new one), not just `cases/README.md`.
