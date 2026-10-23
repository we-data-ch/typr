<!--
Thank you for contributing to TypR!

Target branch: `develop` or `tester` (NOT `main` since it is the release branch and `nu publish.nu release` refuses to publish from elsewhere).
Read CONTRIBUTING.md before opening a PR if you haven't yet.
Delete sections that don't apply rather than leaving unchecked boxes.
-->

Fixes #NNNN

## Summary

<!-- 1–3 sentences: what changed, and why. -->

## RFC boundary

<!-- CONTRIBUTING.md: if the answer to "What does TypR here?" changes (so a big change in its direction), it's an RFC. If the compiler is only catching up to an already-given answer, it's an issue. -->

- [ ] This PR does **not** change what TypR does. It implements/fixes already-decided behavior (link the issue).
- [ ] This PR **does** change language behavior (new syntax, typing rule, or generated R form). Linked RFC exists in `rfcs/`: #

*(If neither box can honestly be checked, the idea probably belongs in Discussions → Ideas first.)*

## Type of change

<!-- Keep what applies, delete the rest. -->

- [ ] Compiler core (`crates/typr-core`): parsing, type checking, transpilation
- [ ] CLI (`crates/typr-cli`)
- [ ] LSP (`crates/typr-lsp`)
- [ ] MCP server (`crates/typr-mcp`), including `tutor` lesson data
- [ ] Editor integrations (`editors/` VS Code/Positron, RStudio addins, Vim/Neovim)
- [ ] Cases catalog (`cases/`)
- [ ] Packaging / docker / install / release tooling

## Verification checklist (all PRs)

These mirror the CI jobs and they block merge anyway. Check them before so you don't wait on CI:

- [ ] `cargo test --workspace` passes locally.
      *(Note: `export RUST_MIN_STACK=8388608` first, type checking recursion overflows the default 2 MB stack.)*
- [ ] `cargo fmt --all` applied.
- [ ] `typr case run` shows no 'REGRESS'.
- [ ] No binaries committed; versions stay aligned (CI `coherence`).
- [ ] Release build compiles (`cargo build --release`, CI `build-check`).
- [ ] If touching `editors/`: VS Code extension compiles and the R package is analyzable (CI `editors`).

## If this PR touches the parser or type system

Per CONTRIBUTING.md: a new case should be added to `cases/` (`typr case add`) for the corrected behavior, **or** a reason for not adding one is given below.

<!-- Briefly explain why a case is not necessary if applicable. -->

**Reason for not adding a case:**

<!-- Delete this section if a case was added. Otherwise, explain here why a case is not needed. -->

## If this PR is an RFC

The file is `rfcs/0000-<slug>.md` (the number is assigned at merge time, which is this PR's number) and follows `rfcs/0000-template.md`.

> An RFC discussion does not work like code: comment threads should be posed on specific sentences. Avoid talking-over-an-existing-review in progress. See [`rfcs/README.md`](./rfcs/README.md).

## Compiler core details

- [ ] Existing R code still behaves identically (TypR is a complement of R, not a fork of its semantics).
- [ ] Generated R stays idiomatic, readers should not see artifacts of this change.
- [ ] Behavior justified: either an issue (already-decided behavior) or an accepted RFC is linked above.

## Editor grammars

Hand-written grammars have historically drifted: highlighting Rust keywords (`impl`, `trait`, `mut`, …) that don't exist in TypR while missing real TypR keywords (`opaque`, `module`, `record`, `object`, `interface`, `typeconstructor`, `recursive`, `embed`, `@export`, kind sigils).

- [ ] If this PR adds/changes TypR keywords or tokens: every editor grammar in `editors/` was updated to match.

## MCP server

The MCP server exposes the compiler to AI agents (Claude Code, Claude Desktop, …), tools run in-process, no filesystem, no `.typr_cache`.

- [ ] Tool behavior and output shape are unchanged, or any change is intentional and documented (agents depend on this contract).
- [ ] If `tutor` lessons were touched: `typr tutor check` still passes for affected lessons.

## Documentation

User-facing documentation lives in the separate `we-data-ch/typr.github.io` repo.

- [ ] User-facing changes have a corresponding PR in `we-data-ch/typr.github.io`.
      *(Warning: `onBrokenLinks` is set to `throw` there, a dead link fails that repo's CI before merge, not after.)*
- [ ] Link to the docs PR: #

## Test plan

<!-- How you verified this beyond CI: commands run, cases exercised, editor behavior manually confirmed if applicable. -->