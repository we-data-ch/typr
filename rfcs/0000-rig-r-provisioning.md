- **Status:** draft
- **RFC PR:** we-data-ch/typr#0000
- **Tracking issue:** —
- **Implemented in:** not yet
- **Start date:** 2026-10-25

# Provisioning R with rig, and splitting `r_env.rs` into a toolchain module

## Summary

`typr` already delegates **packages** to [uvr](https://github.com/nbafrank/uvr): `uvr add`,
`uvr remove`, `uvr sync`, a `uvr.toml` manifest and a `uvr.lock` lockfile. It delegates nothing for
the **R interpreter** itself. If R is missing, every command that needs it fails with a raw
"program not found" from the operating system.

This RFC adds [rig](https://rig.r-lib.org/) (r-lib, the R Installation Manager) as the one tool
`typr` uses to **find, install and select a version of R**. The division of labour becomes:

| Concern | Tool |
|---|---|
| Which R runs, and installing it when absent | **rig** |
| Which packages, at which versions, in a project | **uvr** (unchanged) |

The change is deliberately narrow. rig does not replace uvr: it has no `add`/`remove` for project
packages and no lockfile, and its `rig proj` command is experimental. As with uvr today, `typr`
never installs anything silently. It detects, explains, and installs only on an explicit command.

The RFC also settles a code-organisation question that this feature forces: `crates/typr-cli/src/r_env.rs`
mixes three unrelated jobs and its name no longer describes it. It is replaced by a `toolchain/`
module (§Reference-level explanation, "Module layout"), and `r_env.rs` is deleted.

## Motivation

A person who installs `typr` from a release binary, without R on the machine, gets this:

```
$ typr new demo
...
Error: No such file or directory (os error 2)
```

or, from a command that does reach `Command::new("Rscript")` and reports it, a message that says
nothing about what to do. TypR's audience is R developers (see the project's target audience), who
mostly have R. But "mostly" leaves out three real cases:

1. **First contact.** Someone tries TypR from a fresh container, a CI runner, a new laptop, or a
   machine where R lives under a name or path `typr` does not see. The first thing they meet is an
   error about a file.
2. **Version mismatch.** A project needs R ≥ 4.3 (for example for a package it depends on). The
   machine has 4.1. Nothing in `typr` can say so, let alone fix it.
3. **No admin rights.** The standard ways to get R (`apt`, the macOS installer, the Windows
   installer) need administrator rights. rig 0.10 adds a **user mode** that installs everything
   under the user's home directory and never asks for `sudo`, which removes the main reason people
   give up.

uvr is not the answer to any of these. It manages packages for a project and presumes an R it can
find; the existing doc comment in `r_env.rs` already says so ("uvr has no `run -e`… we launch
`R`/`Rscript` directly"). Something else has to put R on the machine.

### Why rig and not something else

- It does exactly this and nothing more: install several R versions side by side, choose the
  default, resolve `release` / `oldrel` / `devel` / exact versions, on Linux, macOS and Windows.
- It is maintained by the r-lib organisation (Posit), which R users already trust.
- It is a single native binary with no R dependency, so it can bootstrap R from nothing. Tools
  written in R (`renv`, `pak`) cannot.
- On Linux it ships both distribution-specific builds and portable builds for glibc and musl.

## Guide-level explanation

### What a user sees

`typr` checks for R before running anything that needs it. When R is missing, the message says what
to do:

```
$ typr build
R is not installed (or not on the PATH).

Install it with rig:
  typr setup-r              # installs the latest release of R, in your home directory

or install rig yourself first: https://rig.r-lib.org/
```

`typr setup-r` is the explicit command. It never runs by itself.

```
$ typr setup-r
rig is not installed. TypR can install it for you:
  https://github.com/r-lib/rig/releases/...   (checksum verified)
Install rig into ~/.local/bin and R 4.5.1 into ~/.local/share/rig? [y/N] y
...
R 4.5.1 installed. `typr` will use it.
```

```
$ typr setup-r --version 4.4        # a specific version
$ typr setup-r --yes                # non-interactive (CI)
$ typr setup-r --check              # report only; exit 1 if R is missing or too old
```

### Pinning R for a project

A project can say which R it wants. `uvr.toml` already carries an R version (uvr resolves its own
`r_version`); `typr` reads it and does **not** introduce a second field.

```toml
# uvr.toml  (owned by uvr, read by typr)
[project]
r_version = ">=4.3"
```

When the R found is outside that constraint, `typr check`, `typr build` and `typr run` stop before
spawning anything:

```
error: this project needs R >=4.3, found R 4.1.2 at /usr/bin/R.
  fix: typr setup-r --version 4.3
```

### How to teach it

"`uvr` installs your **packages**. `rig` installs **R**. TypR calls each of them for you, and it
asks before it installs either." For someone who knows `pyenv` + `uv`, or `rustup` + `cargo`, it is
the same pair.

## Reference-level explanation

### Module layout

`r_env.rs` (269 lines) currently holds three things under one name:

1. the `Cmd` value type (program, argv, env) and its `Command` conversion,
2. `REnv` (system vs. a uvr project) and how a subprocess is pointed at the project library,
3. the uvr command builders (`add_packages`, `remove_packages`, `sync`, `init`) and the
   availability checks (`uvr_available`, `require_uvr`).

Adding rig as a fourth concern to that file would make it worse, and the name `r_env` already
undersells (3). The file is split and removed:

```
crates/typr-cli/src/toolchain/
  mod.rs      re-exports; `Cmd`; `require_r()`
  cmd.rs      Cmd (moved from r_env.rs, unchanged)
  env.rs      REnv, project_env, current_env, with_libs        (was r_env.rs, part 2)
  uvr.rs      add_packages, remove_packages, sync, init,
              uvr_available, require_uvr, UVR_* constants       (was r_env.rs, part 3)
  rig.rs      NEW: rig_available, list_installed, add, default,
              require_r, r_missing_message, version_satisfies
```

`r_env.rs` is deleted. `mod r_env;` in `main.rs` and `pub mod r_env;` in `lib.rs` become
`mod toolchain;` / `pub mod toolchain;`. The 40-odd call sites (`crate::r_env::current_env()` in
`project.rs`, `r_deps.rs`, `io.rs`) are rewritten mechanically to `crate::toolchain::env::…` or
re-exported names; there is **no behaviour change in that step**, and it lands as its own commit so
the diff is reviewable as a pure move. The existing unit tests move with their code and keep their
names.

### The rig builders

Same discipline as uvr: pure functions that return a `Cmd`, so tests inspect argv without rig or R
on the machine.

```rust
pub fn list_installed() -> Cmd            // rig list --user --json
pub fn add(version: &str) -> Cmd          // rig add <version> --user --json
pub fn default(version: &str) -> Cmd      // rig default <version> --user
```

and the checks:

```rust
pub fn rig_available() -> bool            // `rig --version` succeeds
pub fn r_available() -> bool              // `R --version` succeeds, after rig's PATH shims
pub fn r_version() -> Option<Version>     // parsed from `R --version`
pub fn require_r(min: Option<&VersionReq>) -> Result<(), String>
```

`require_r` is called at the same points `require_uvr` is today, and **before** it: uvr needs R, so
a missing R is reported first. It never falls back to anything.

### Where R is looked for

In this order, first hit wins:

1. `R` / `Rscript` on the `PATH`;
2. the quick links rig creates (`R-4.5`, …) and rig's own installation directory, so that a
   user-mode install works in the very same shell without editing the profile;
3. nothing: `require_r` returns the message above.

`typr` does not mutate the user's shell profile, `PATH`, or R's default version unless the user ran
`typr setup-r`. Even then it edits nothing itself: rig, on its first user-mode install, adds
`. "$HOME/.local/bin/rigenv"` to `~/.profile` and makes the first R the default (Step 0 findings),
and `setup-r` says so in its prompt.

### `typr setup-r`

A new subcommand in `cli.rs`, in the same family as `typr new`/`typr check`. It never runs
implicitly.

1. If R satisfies the project's constraint (or there is none) and `--version` is not given: print
   where R is and exit 0.
2. If rig is missing: print what will be downloaded (URL, version, destination) and ask. With
   `--yes`, skip the question. The binary is fetched from the rig GitHub release for the current
   OS/arch, and its **checksum is verified** before it is made executable. No `curl | sh`.
3. Run `rig add <version> --user --json` so no administrator rights are needed.
4. Never change an existing user's default silently: `rig add` already leaves it alone when one is
   set and only makes the first R the default, so `rig default` is run only for an explicit
   `--version` on an R that is already installed.
5. Print the R that will be used.

Exit codes: 0 R available; 1 failed or declined; 2 usage.

### Error messages

R missing, no project constraint:

```
error: R is not installed (or not on the PATH).
  fix: typr setup-r
  or:  install R yourself, then re-run this command
```

R too old for the project:

```
error: this project needs R >=4.3, found R 4.1.2 at /usr/bin/R.
  fix: typr setup-r --version 4.3
```

rig missing during `setup-r --yes` on an unsupported platform:

```
error: there is no rig build for linux/riscv64.
  Install R by your distribution's package manager, then re-run.
```

### What does not change

- `REnv`, `R_LIBS` handling and the `.uvr/library` convention (moved to `toolchain/env.rs`).
- `typr add`, `typr use`, `typr sync` and everything that goes through uvr.
- Subprocesses that deliberately interrogate the *system* R (`gen-types`, registry validation,
  `typr fuzz`'s availability probe) still use plain `Rscript`.
- The WASM build and `typr-core`: all of this lives in `typr-cli`. The playground uses WebR, not a
  local R.
- No generated R changes.

### Windows and macOS

rig supports both and handles Rtools on Windows. `typr setup-r` on Windows uses the rig installer
or `winget`; the exact command per platform is an implementation detail verified in Step 0 (below),
not specified here.

### Step 0 findings (rig 0.11.0, Linux x86_64, 2026-10-25)

Verified by downloading the release binary into a scratch directory, with a throw-away `$HOME`, and
installing a real R in user mode. **Not verified**: macOS, Windows, other Linux arches, and whether
`uvr` picks up a rig-installed R (uvr was not on the sandboxed `PATH`; check in Step 2).

- **Version.** The latest rig is 0.11.0 (2026-10-07), not 0.10 as assumed above; user mode is
  present and documented. Behaviour below is for 0.11.0.
- **Mode is `admin` by default.** User mode must be requested on *every* call: `--user`, or
  `RIG_MODE=user`, or `rig config`. Every builder in `toolchain/rig.rs` therefore passes `--user`
  explicitly, so `typr` never depends on the user's rig config. `RIG_MODE=user` also works.
- **Layout in user mode.** R versions go to `~/.local/share/rig/r/<ver>`, quick links to
  `~/.local/bin` (`R`, `Rscript`, `R-4.4.3`, `rigenv`), config at `~/.local/share/rig/config.json`.
  Paths come from `rig system dirs --user`. `~/.local/bin` is **not necessarily on the `PATH`**, so
  the lookup order in "Where R is looked for" (step 2) is needed, not optional.
- **rig edits the shell profile.** On the first user-mode install it created `~/.profile` containing
  `. "$HOME/.local/bin/rigenv"`. That is rig's doing, not `typr`'s, but the "does not mutate the
  user's shell profile" sentence must say so, and `typr setup-r`'s prompt must announce it.
- **Default version.** The first `rig add` makes that version the default (`"default": true`); a
  second `rig add 4.3` left the default untouched. So Step 4's "run `rig default` only if there was
  no R before" needs **no code**: `rig add` already behaves that way. `rig default` is only used
  for an explicit `--version` request on an already-installed R.
- **Machine-readable output is stable enough.** `rig list --json` returns
  `[{name, default, version, aliases, path, binary}]` (`[]` when nothing is installed, exit 0);
  `rig add <v> --json` returns the same object plus `new-install`. With `--json` stdout is pure
  JSON; rig's `[INFO]` log lines go to stderr. `rig default` with no default set exits 1.
  `rig resolve <spec> --json` maps `release` / `4.4` to an exact version and URL without installing.
  Decision: parse `rig list --json` (it gives the `binary` path directly); `R --version` stays the
  fallback for a system R that rig does not manage.
- **Version specs.** `release`, `devel`, `next`, `oldrel[/n]`, `x.y` (latest patch, e.g. `4.4` →
  4.4.3), `x.y.z`, a URL, or an `renv.lock` path. `typr setup-r --version 4.3` maps straight on.
- **Linux builds.** In user mode rig always installs a *portable* Posit build chosen by libc (glibc
  or musl); an install took ~27 s. It also downloads fallback fonts (skip with `--without-fonts`,
  which `typr` should pass in `--yes`/CI mode). No `sudo`, no distro package.
- **Checksum source.** There is **no `.sha256` sidecar** (404). GitHub's release API exposes a
  `digest` (`sha256:…`) per asset, e.g. `rig-linux-x86_64-0.11.0.tar.gz` →
  `27e063df…fcf1`; the download matched it. `typr setup-r` reads that field. Windows ships a `.zip`
  and `.exe`, macOS a `.tar.gz` and a `.pkg`.
- **`R_LIBS`.** User-mode R honours it: with `R_LIBS=<dir>` set, `.libPaths()` lists `<dir>` first,
  then the user library, then the R library. The `.uvr/library` convention is unaffected.
- **Self-management.** `rig self update` / `rig self uninstall` exist; `typr` does not call them.

## Gradual typing

Not applicable: this RFC concerns the toolchain, not the language. No syntax, typing rule or
emitted R changes. It is an RFC because it changes a published contract (the CLI gains a command,
and commands that used to fail with an OS error now fail with a specified message and exit code).

## Drawbacks

- **A second external tool.** The user may now need rig *and* uvr. Mitigated by `typr setup-r`
  being the only thing they have to learn, and by neither being required when R is already
  installed: a user with R and uvr never sees rig.
- **rig's user mode is new** (0.10, still beta at the time of writing). Its flags and install paths
  may change before release. The RFC pins behaviour (`typr setup-r` works without admin rights),
  not rig's flags, and Step 0 re-verifies them against the version we target.
- **Downloading a binary.** `typr` fetching and running an executable is a trust decision. It is
  gated by an explicit prompt, a checksum, and a refusal in non-interactive mode without `--yes`.
- **Support surface.** More platforms, more ways for an install to fail, and the failure is now
  partly `typr`'s to explain. The fallback is always the same sentence: install R yourself.
- **Churn.** Splitting `r_env.rs` touches the most-used module in the CLI. Mitigated by doing the
  move first, alone, with no behaviour change, behind the 868-test suite.

## Rationale and alternatives

**Do nothing, only improve the error.** The cost is small: print "R is not installed, see
https://cran.r-project.org". This is the baseline, and it is a real option; `require_r` with that
message is useful on its own and is Step 1. What it does not give is a path for the no-admin case
and the version-mismatch case. The RFC proposes going further, and the first step is shippable
alone if the rest is rejected.

**Replace uvr with rig.** Rejected. rig has no `add`/`remove`/`sync` for project packages and no
lockfile; its `rig proj` is explicitly experimental. It would lose the requirement that drove the
uvr migration (pin, add, remove and change the version of packages in a project).

**Let uvr install R.** uvr is a package manager and presumes an R. If a future uvr gains that, this
RFC's `toolchain/rig.rs` is the only file that would change. The user-facing command
(`typr setup-r`) is deliberately tool-neutral for that reason.

**`rig` as a hard dependency, auto-installed on first use.** Rejected: it contradicts the rule the
module already states ("never a silent `install.packages`"). Installing an interpreter is a larger
act than installing a package.

**Bundle R with `typr`.** Rejected: size, licensing (GPL), and per-platform builds are all out of
proportion to the problem.

**Use the platform's package manager (`apt install r-base`, `brew install r`, `winget`).** Needs
administrator rights on most systems, gives no control over the version, and differs on every
platform. rig is the one interface over all of them.

**Name the command `typr install-r`, `typr r install`, or fold it into `typr doctor`.** `setup-r`
reads as an action on the environment, parallels `typr new`, and leaves `doctor` free for a broader
health check later.

## Prior art

- **rustup + cargo.** The closest model: one tool installs the toolchain, another builds with it,
  and `rust-toolchain.toml` pins the version per project. `uvr.toml`'s `r_version` plays that role
  here.
- **pyenv/uv.** `uv python install` handles interpreters itself, which is the direction uvr could
  take; the split in this RFC is the one the R ecosystem has today.
- **R-hub / GitHub Actions `r-lib/actions/setup-r`.** Already use rig under the hood to provision R
  in CI, so the behaviour is exercised daily on all three platforms.
- **renv.** Records the R version in its lockfile but cannot install it, and warns on mismatch. Our
  error message plays the same role and adds a fix.

## Unresolved questions

Settle before merge:

- The name and flags of `typr setup-r` (`--version`, `--yes`, `--check`).
- Whether the minimum R version is read from `uvr.toml` only, or also from `DESCRIPTION`'s `Depends:
  R (>= x)`. Today `typr use` writes both; they should agree, and `typr check` should say when they
  do not.

Settled by Step 0 (see "Step 0 findings"): the user-mode invocation (`--user`, Linux), `rig list
--json` is parsed, quick links are in `~/.local/bin` and not necessarily on the `PATH`, the checksum
comes from GitHub's release `digest`, and user-mode R honours `R_LIBS`.

Still open (Step 0 covered Linux x86_64 only):

- Windows entirely (rig's `.zip`/`.exe`, Rtools), and macOS in practice. The user-mode invocation and install paths there (the `.pkg`/`.exe` vs the
  `.tar.gz`/`.zip` assets, and Rtools on Windows).
- Whether `uvr` finds a rig-installed R that is not on the `PATH`, or `typr` must pass it.

## Future possibilities

Out of scope here:

- `typr doctor`: one command that reports R, rig, uvr and the project's environment.
- Letting `typr` install uvr the same way (`typr setup-r` becoming `typr setup`), replacing the
  `curl | sh` hint in `UVR_INSTALL_HINT` with a checksummed download.
- Per-project R versions through rig's project support once `rig proj` stabilises.
- Running the test suite against several R versions (`typr test --r 4.3,4.4,release`).

## Implementation plan

Each step is a commit that leaves the 868 tests green.

- **Step 0: verify rig.** *Done 2026-10-25 (Linux x86_64, rig 0.11.0).* Findings recorded in
  "Step 0 findings"; macOS/Windows remain to verify when `setup-r` is written for them (Step 4).
- **Step 1: pure move.** *Done 2026-10-25 (not committed).* Split `r_env.rs` into
  `toolchain/{mod,cmd,env,uvr}.rs`, deleted `r_env.rs`, rewrote call sites. No behaviour change; the
  9 unit tests moved with their code under the same names (`toolchain::env::tests`,
  `toolchain::uvr::tests`). `mod.rs` re-exports `Cmd`, `REnv`, `project_env`, `current_env` and the
  uvr builders, so call sites read `crate::toolchain::current_env()`. `UVR_MANIFEST`/`UVR_LIBRARY`
  live in `uvr.rs`, which `env.rs` imports. `Cmd::new`/`arg` became `pub(super)`.
  `cargo test --workspace` green (with `RUST_MIN_STACK=8388608`).
- **Step 2: `require_r`.** *Done 2026-10-26 (not committed).* New `toolchain/rig.rs` with `Version`,
  `VersionReq` (`>=`, `>`, `<=`, `<`, `=`, bare; a bare/`=` `x.y` matches every patch, like rig's
  spec), `parse_r_version`, `find_r`, `r_available`, `r_missing_message`, `r_too_old_message`,
  `require_r(Option<&VersionReq>)`; no new dependency. `find_r` tries `R` on the `PATH`, then rig's
  user-mode layout (`~/.local/bin/R`, then `~/.local/share/rig/r/<newest>/bin/R`); an R found only
  there has its directory prepended to **this process's** `PATH` so later plain `Rscript` spawns find
  it (the user's shell is untouched). Wired before `require_uvr` in `uvr_init` (`typr new`, still a
  warning), `require_uvr_project` (`typr use`/`add`) and `r_deps` (`typr init`). 8 unit tests
  (`toolchain::rig::tests`); `cargo test --workspace` green.
  *Deviations:* (1) the messages say "install R from cran.r-project.org, or with rig" instead of
  `typr setup-r`, which does not exist until Step 4: flip `R_INSTALL_HINT` then. (2) `build`/`check`/
  `run` are **not** wired yet: they do not need R today, and the constraint check that would justify
  it is Step 5. `VersionReq::parse` and `r_available` are `#[allow(dead_code)]` until then.
  Not covered: no test spawns a real/fake `R` (the `cases/` item in the checklist, via a fake `PATH`).
- **Step 3: `toolchain/rig.rs`.** *Done 2026-10-26 (not committed).* Added to the Step 2 `rig.rs`:
  `list_installed()` (`rig list --user --json`), `add(version, without_fonts)`
  (`rig add <v> --user --json [--without-fonts]`), `default(version)` (`rig default <v> --user`) and
  `rig_available()` (`rig --version` succeeds). Pure `Cmd` builders, tested on argv only
  (`rig_builders_always_ask_for_user_mode`: every command carries `--user`, no env). `mod.rs`
  re-exports them as `list_installed`, `rig_add`, `rig_default`, `rig_available`; all are
  `#[allow(dead_code)]` until Step 4 calls them. `cargo test --workspace` green.
  *Deviation:* `add` takes a second parameter `without_fonts`, not the `add(version)` of the sketch
  above, so `setup-r --yes` can pass `--without-fonts` (Step 0 finding). Not done: parsing the
  `rig list --json` output (`[{name, default, version, aliases, path, binary}]`); it has no caller
  before Step 4, so it lands there.
- **Step 4: `typr setup-r`.** *Done 2026-10-26 (not committed).* New `toolchain/setup.rs` and a
  `SetupR { --version, --yes/-y, --check }` subcommand (exempt from the R-deps warning). Flow: use the
  R that is there if it matches; else find rig (`PATH`, then `~/.local/bin/rig`); if absent, read the
  latest release from the GitHub API, show URL + destination, ask (`--yes` skips; no TTY without
  `--yes` refuses), download the `rig-<os>-<arch>-<v>.tar.gz`, **verify its SHA-256 against the
  asset's `digest`** (no digest or a mismatch: nothing is unpacked), unpack into `~/.local`; then
  `rig list --user --json` (now parsed: `parse_rig_list`, `installed_matching`) and either
  `rig default` (requested version already installed but not default) or `rig add --user --json`
  (`--without-fonts` with `--yes`). The prompt announces rig's `~/.profile` edit when there was no R.
  `R_INSTALL_HINT` now points at `typr setup-r`. 5 pure unit tests (asset names, release JSON,
  digest, SHA-256 of a file, `rig list` + spec matching); `cargo test --workspace` green.
  Verified for real on Linux x86_64 with a throw-away `$HOME` and a `PATH` without R: `--check` (exit
  1), no TTY (refused), `--yes --version 4.4` (rig 0.11.0 + R 4.4.3, `.profile` written), repeat
  (no-op), `--version 4.3` (installed, default stays 4.4 with a note), repeat (switches the default).
  *Deviations / limits:* (1) **no HTTP crate**: download and unpack shell out to `curl` and `tar`
  (the same assumption as `UVR_INSTALL_HINT`). (2) **Windows is not handled**: `rig_asset` returns
  `None` and the user gets the "no rig build" message pointing at their package manager; macOS uses
  the `.tar.gz` and is untested. (3) The prompt names the spec (`release`), not the resolved version,
  since rig is not there yet to resolve it. (4) Musl Linux gets the glibc archive.
  (5) `registry_validate::…formals_diff_flags_a_missing_export_and_an_arity_mismatch` failed once in
  the parallel run and passes alone, with or without this change: it spawns R.
  Not covered: no `cases/` entry yet (needs a fake `PATH`), and `setup-r` itself is not tested end to
  end in CI (it needs the network).
- **Step 5: version constraint.** *Done 2026-10-26 (not committed).* In `toolchain/rig.rs`:
  `manifest_r_requirement` (reads `[project] r_version` with the `toml` crate already in use; absent or
  unparseable gives `None`, since uvr owns the manifest's validity), `project_r_requirement(dir)` (finds
  the project through `project_env`) and `require_project_r(dir)`. `project.rs::enforce_r_constraint()`
  calls it first thing in `check_project` and `build_project_impl`, which covers `build`, `run` and
  `test`; on failure it prints `error: …` and exits 1. Real run in a scratch project: `r_version =
  ">=99"` gives "this project needs R >=99, found R 4.6.1 at R" and exit 1; `">=4"` passes. The
  `#[allow(dead_code)]` on `VersionReq` are gone. 3 new unit tests; `cargo test -p typr-cli` green
  apart from the known flaky `registry_validate::…formals_diff…` (passes alone).
  *Deviations / limits:* (1) **no pin means no R check at all**: `check`/`build`/`run` still work on a
  machine without R, as before. (2) `check_file`/`build_file`/`run_file` (single files, no project) are
  not covered. (3) The "found R … at R" message shows the bare `R` when it comes from the `PATH`.
  (4) The `DESCRIPTION` `Depends: R (>= x)` agreement check (Unresolved questions) is not done.
- **Step 6: docs.** *Done 2026-10-26 (not committed).* `typr.github.io`: `docs/reference/installation.md`
  §3 rewritten (`typr setup-r`, options, where R lands, the `~/.profile` edit made by rig, the
  `uvr.toml` `r_version` pin) and a new "Install R, then uvr" section in
  `docs/howto/manage-r-dependencies.md`; `README.md` gets a paragraph; `uvr.md` notes the
  `r_env.rs` → `toolchain/` split. The docs say Windows is unsupported by `setup-r`, matching Step 4.
  `npm run build` on the site passes (`onBrokenLinks: throw`, so the new links and the `#3-install-r`
  anchor resolve); no new ` ```typr ` block was added, so `check:examples` was not run.
  *Not done:* there is no `ai_context/CLAUDE.md` in the workspace (the root `CLAUDE.md`
  refers to one that does not exist), so no module map was updated.

## Implementation checklist

- [x] `cases/0139-r-missing-for-pinned-project` and `cases/0140-r-too-old-for-pinned-project` (fixed, frozen 2026-10-26) — via a new `fake_r` field in `case.toml` (`cases.rs`: sandbox `PATH`/`HOME` replaced, `"none"` or a stub `R` reporting a version). Both pin `r_version = ">=4.3"`: with no pin `check` does not need R (Step 5), so "R missing" is only testable on a pinned project
- [ ] `typr syntax --write` — not needed, no lexeme changes
- [ ] `syntaxe.md` — not needed
- [x] Documentation on `we-data-ch/typr.github.io` written (Step 6, not committed; PR still to open, same release)
- [x] `r_env.rs` deleted, no remaining reference (`grep -rn r_env crates/`) — Step 1
- [ ] `Implemented in:` filled in above
