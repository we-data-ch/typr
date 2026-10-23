# Publishing a TypR Release

A single gesture triggers the whole distribution: **push a `vX.Y.Z` tag**.
Everything else is done by `.github/workflows/release.yml`.

## Source of truth

`[workspace.package] version` in `Cargo.toml`. Nothing else carries a reference
version: the VS Code extension and the RStudio runner are derived from it by
`nu publish.nu sync`, and the playground reads the one the CI writes next to the WASM.

The CI **refuses** to publish if the tag, `Cargo.toml`, `editors/vscode/package.json`
and `editors/rstudio/DESCRIPTION` do not all say the same thing (job `verify`),
and the same check runs on every PR (job `coherence` in `ci.yml`).

## Procedure

```bash
git switch develop
nu publish.nu ship patch      # or minor / major
```

That is all. `ship` chains the version bump, the pull request, waiting for
the CI, the merge, the tag, and resyncing `develop`.

It interrupts **only once**, when the checks are green and just before
merging:

```
Ready to merge PR #22 and set v0.5.10.
The tag triggers publication on crates.io, where a number can no longer be reused.
Continue? [o/N]
```

This is the last reversible moment. Answering anything other than `o` leaves the PR
open and publishes nothing; `nu publish.nu ship --resume` picks it up later.

Same thing if the CI fails: nothing is merged or tagged, the PR stays open,
you fix it on `develop` and rerun with `--resume`.

Adding `--yes` removes the confirmation, for non-interactive use. Otherwise avoid
it: it is the only barrier before the irreversible.

### What ship does, in order

1. checks that `gh` is authenticated, that we are on `develop`, that the tree is
   clean, and that `develop` is up to date;
2. merges `main` into `develop` (without this the PR would carry a regression);
3. `bump`, which propagates the version to the VS Code extension and the RStudio runner;
4. `cargo check --workspace`, which updates `Cargo.lock`. **On failure the
   version is restored**: a number that does not compile does not stay in the
   working tree;
5. commit, push, open the PR;
6. wait for the required checks;
7. *confirmation*;
8. merge, then `release`, which repeats its four guards, deliberately
   twice: it is the last barrier;
9. `develop` brought back level with `main`.

### By hand

The sequence remains decomposable, which is useful for resuming a partially
completed release:

```bash
git switch develop && git pull
nu publish.nu bump patch
cargo check --workspace
git commit -am "release v0.5.10" && git push origin develop
gh pr create --base main --head develop --fill
gh pr merge --merge --delete-branch=false

git switch main && git pull
nu publish.nu release --dry-run
nu publish.nu release

git switch develop && git merge main && git push origin develop
```

`release` pushes **only the tag**: `main` is protected, its content only arrives there
through a pull request. If it detects that the local `main` diverges from `origin/main`,
it stops rather than tagging into the void.

An already published version number is not reused. `release` refuses an existing tag
both locally and remotely, and `cargo publish` would refuse anyway
to overwrite a version present on crates.io.

`nu publish.nu check` then compares what is actually published on each channel.

## What the CI does from the tag

```
tag vX.Y.Z
   │
   ├─ verify ......... refuses if tag ≠ Cargo.toml ≠ editors
   │
   ├─ build .......... 8 targets (Linux musl + Linux GNU + Windows + macOS,
     │                   × x86_64/aarch64)
   │   └─ release .... GitHub Release + SHA-256 checksums
   │        ├─ docker ..... image from the Linux binary, tags X.Y.Z + latest
   │        ├─ rstudio .... R tarball with the fresh binaries embedded
   │        └─ docs ....... repository_dispatch → site rebuild
   │
    ├─ crates-io ...... typr-core → typr-lsp → typr-cli → typr
    ├─ wasm ........... WASM + version.json → playground repo
    ├─ vscode ......... .vsix → Marketplace + Open VSX + attached to the release
    ├─ vim ............ plugin tarball, attached to the release
    ├─ brew ........... generated formula → tap `we-data-ch/homebrew-typr`
    ├─ winget ......... generated manifests → PR `microsoft/winget-pkgs`
    └─ scoop .......... generated manifest → bucket `we-data-ch/scoop-bucket`
```

The branches are independent: if the Marketplace fails, the binaries and
crates.io are still published.

### Replaying a branch that failed

```bash
gh workflow run release.yml -f tag=v0.5.9
```

The workflow accepts a tag as input, which avoids having to re-tag.

**With a limitation you need to know: it reloads the repository *at the tag*.** A
fix pushed afterwards is therefore not taken into account: the replay recompiles
exactly the same tree.

The replay is for **environment** failures: expired token, registry
unavailable, network. For a **code** failure, you need a fresh tag, hence a
new version.

That is what happened at v0.5.8: the Docker job failed on the
`Dockerfile` itself, and a v0.5.9 had to be shipped to deliver the fix.
Replaying v0.5.8 would have hit the same file again.

## The crates.io order is not negotiable

`typr-cli` declares `typr-lsp = { path = ..., version = "0.5" }`. A `path`
dependency **carrying a version number** requires that the crate exist on the registry
at the time of `cargo publish`. `typr-lsp` must therefore be published before `typr-cli`,
itself before `typr`.

This is precisely what had blocked the chain: `typr-lsp` never having been
published, `typr-cli` and `typr` stayed at 0.5.5 while `typr-core` moved
to 0.5.7 and the GitHub releases announced 0.5.7.

## Required secrets

| Secret | Used by | Where to get it |
|---|---|---|
| `CARGO_REGISTRY_TOKEN` | `crates-io` | crates.io → Account Settings → API Tokens |
| `DOCKERHUB_USERNAME` | `docker` | Docker Hub account name |
| `DOCKERHUB_TOKEN` | `docker` | Docker Hub → Account Settings → Personal access tokens |
| `VSCE_PAT` | `vscode` | Azure DevOps → PAT, scope *Marketplace: Manage* |
| `PLAYGROUND_DEPLOY_TOKEN` | `wasm` | GitHub PAT, scope `repo` on the playground repo |
| `DOCS_DISPATCH_TOKEN` | `docs` | GitHub PAT, scope `repo` on `typr.github.io` |
| `HOMEBREW_TAP_TOKEN` | `brew` | GitHub PAT, scope `repo` on `homebrew-typr` |
| `WINGET_CREATE_GITHUB_TOKEN` | `winget` | GitHub PAT, scope `public_repo` (read by `wingetcreate` in the environment) |
| `SCOOP_BUCKET_TOKEN` | `scoop` | GitHub PAT, write on `scoop-bucket` |

## The Homebrew tap

`we-data-ch/homebrew-typr` contains only `Formula/typr.rb`, `README.md` and
`LICENSE`: no binaries. The `brew` job generates the formula from the
`checksums.txt` of the release, has it validated by `brew style` and
`brew audit --strict`, then commits it. The formula is therefore never written by
hand: see `packaging/homebrew/README.md`.

The repository was created on 30 September 2026 and already contains the formula for
v0.5.12; the CI then keeps it up to date at each tag. If it had to be redone, the gesture
is:

```bash
gh repo create we-data-ch/homebrew-typr --public --description "Homebrew tap for TypR"
```

Two ways to maintain it, to be decided once and for all:

- **free `main`**: the CI pushes directly, as `vscode` and `wasm` do
  for their repositories. The tag is authoritative, and the tap can never be behind.
- **protected `main`, admission via the `we-data-ch/homebrew-typr-autoupdate` repository**:
  the CI no longer writes to the tap but to the admission repository, which opens a
  PR that we review. `homebrew-typr/typr` remains installable in the meantime, since what
  `brew install` reads is the default branch. The tap then becomes impossible to
  modify without human review, at the cost of a `brew update-autoupdate` to
  install on the maintainers' machines.

The choice for batch 3 is the first one: it is consistent with the rest of the chain, and
a formula generated by the CI from `checksums.txt` does not need to be reviewed: what
would hold it back is its uniqueness, not its wording.

## The WinGet and Scoop channels

No binaries here either: `winget-pkgs` publishes only three YAML files, and the
Scoop bucket only URLs and their SHA-256. Both channels read the **same**
`checksums.txt` as the Homebrew formula, and are rendered by the same script,
`packaging/render.sh`: see `packaging/README.md`.

What the two jobs do beyond the tap is **actually install** before publishing:

- `winget` downloads the two Windows archives and compares their fingerprints to
  those of the rendered manifests, then passes the folder to `wingetcreate submit`,
  which validates it with the official validator before opening the PR;
- `scoop` runs `scoop install` on the local manifest, waits for the `typr` shim, and
  requires that `typr --version` reports the version of the release.

A channel that refuses to install therefore publishes nothing: better a red job
than a manifest nobody can install.

**The `we-data-ch/scoop-bucket` repository must exist before the first tag.** The
`clone` job fails loudly if it is absent, because a `git push` to a nonexistent URL
would also fail, but three minutes later:

```bash
gh repo create we-data-ch/scoop-bucket --public --description "Scoop bucket for TypR"
```

Missing secrets do not block the release: rendering and local installations
take place, only the publication is skipped with a warning. This is
intentional, so that a tag serves something other than producing a red job.

The bucket keeps ten versions, purged in order of addition in the history and not
in alphabetical order. `typr update` is for this: without purging, a user who
had not run `scoop update` for a year would see an ever longer list of manifests.

## No binaries in the repository

The RStudio runner binaries are assembled by the CI at the time of building
the tarball, never committed. The former `typr_runner` repository weighed 140 MB of `.git`
for 100 KB of source, because of 33 "[auto] updated binaries" commits.
The `coherence` job in `ci.yml` fails if a binary reappears in the index.

## Local development

```bash
nu deploy.nu               # debug binary → ~/sh/typr
./build-wasm.sh            # WASM → neighboring playground, with version.json
nu docker/deploy.nu        # local image from the release binary
```
