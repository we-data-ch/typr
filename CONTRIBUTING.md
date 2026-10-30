# Contributing to TypR

## Branch model

- `main` holds what is publishable. Release tags are cut from it, and `nu publish.nu release` refuses to publish from anything else.
- `develop` is where integration happens.
- All contributions land through a PR targeting `develop`.

`main` should be protected on GitHub, with mandatory PRs and green CI. Without that, the guard in `publish.nu` only protects whoever happens to use it.

## What the CI checks

| Job | Checks |
|---|---|
| `test` | `cargo test --workspace` (868 tests, blocking) |
| `build-check` | `--release` compilation |
| `cases` | replays the `cases/` catalog and fails on any `REGRESS` |
| `coherence` | aligned versions, no committed binaries |
| `editors` | the VS Code extension compiles, the R package is analyzable |

`RUST_MIN_STACK=8388608` matters more than it looks. Type checking is recursive, and it overflows the default 2 MB stack on at least one test. CI sets it for you. Locally, export it yourself if `cargo test` ever dies with a `stack overflow`.

## Proposing a language change (RFC)

A bug fix is discussed in an issue. A **language change**, meaning new syntax, a typing rule, or a different form of generated R, goes through an RFC first. That means a written proposal, reviewed as a PR, merged into `rfcs/` if accepted. The point is not ceremony. The point is making sure the reasoning survives: six months later, when someone asks why TypR behaves this way, there is a written answer.

Here is the dividing line. If the answer to "what does TypR do here?" changes, it is an RFC. If the compiler is just catching up to an answer that was already given, it is an issue. An idea that is still fuzzy belongs in the [Ideas](https://github.com/we-data-ch/typr/discussions/categories/ideas) category of Discussions. It often gets resolved right there, and when it doesn't, it comes out sharper anyway.

The full process, the labels (`rfc-draft`, `rfc-accepted`, `rfc-rejected`), and the template live in [rfcs/README.md](rfcs/README.md). The workspace design notes (`spécifications/`, `ai_context/*.md`) are its raw material. An RFC is their public face.

## Working on the parser or the type system

The `cases/` catalog is a set of curated reproductions, each with its expected R output. It serves two purposes at once: documenting a behavior and testing that it does not regress.

```bash
typr case list              # catalog status
typr case run               # replays everything, exits 1 on REGRESS
typr case add --from <proj> # creates a case from a real project
typr case freeze <nnnn>     # freezes the current output as golden
```

A bug fix without an associated case has nothing stopping it from coming back.

## Working on syntax highlighting

Editor grammars are **generated**. The single source of truth is the syntax manifest at `crates/typr-core/src/components/syntax/mod.rs`: an ordered list of rules, one entry per token the parser actually recognizes.

```bash
typr syntax --json          # the manifest
typr syntax --target tmlanguage
typr syntax --write         # regenerates the grammars under editors/
typr syntax --check         # fails if a committed grammar has drifted
```

Never edit `editors/vscode/syntaxes/typr.tmLanguage.json` by hand. The file carries a header saying exactly that, and `cargo test -p typr-cli` compares it against what the manifest produces.

Two tests guard the invariant in both directions (`components::syntax::tests`):

- add a `tag("…")` to the parser without adding it to the manifest, and `every_parser_tag_is_in_the_manifest` fails;
- put a word in the manifest the parser does not know, and `manifest_claims_no_word_the_parser_does_not_know` fails.

The second one was missing, and it showed. The hand-written grammars happily highlighted `impl`, `trait`, `struct`, `enum`, `where`, `mut`, `Option`, and `Result`, all copied from a Rust grammar and none of them existing in TypR. Meanwhile `opaque`, `module`, `record`, `object`, `interface`, `typeconstructor`, `recursive`, `embed`, `@export`, and the kind sigils got no highlighting at all.

## Working on the documentation

The documentation lives in a separate repository: `we-data-ch/typr.github.io`. Every PR there triggers a full build of the site. `onBrokenLinks` is set to `throw`, so a dead link breaks the CI before merging rather than after.

Every ` ```typr ` block on the site is run through the real compiler in CI (`npm run check:examples`). If an example stops compiling, the build stops too. The oracle is the **latest release**, meaning the version readers actually have installed. A second workflow replays the same checks every night against `develop`, as warnings only. Think of it as the heads-up that the next release will break a given example. When it fires, the compiler PR and the doc PR should ship together.

## Versions

Nobody bumps a version by hand. `Cargo.toml [workspace.package] version` is the single source of truth. `nu publish.nu sync` propagates it to the VS Code extension and the RStudio runner, and the `coherence` job fails on any mismatch. See [RELEASING.md](RELEASING.md).

## Binaries

Never commit a binary. The RStudio runner's binaries are assembled by CI when building the release tarball, and the `coherence` job fails if one sneaks back into the index.
