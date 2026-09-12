- **Status:** draft
- **RFC PR:** we-data-ch/typr#0000
- **Tracking issue:** —
- **Implemented in:** not yet
- **Start date:** 2026-09-12

# External type definitions

<!--
  Copy this file to `rfcs/0000-my-feature.md` and open a PR against `develop`.
  Keep the `0000` — the number is the PR's own number, assigned at merge.
  Delete the guidance comments as you fill each section; delete a whole section
  only if it genuinely does not apply, and say why in one line.
  Read `rfcs/README.md` first.
-->

## Summary

A TypR project can declare a `.ty` file written by someone else as the type
description of an R package it imports, pin it to an exact commit and content
digest in a new `typr.lock`, and load it into the type-checking context at a
configurable trust threshold (`T1`/`T2`/`T3`). An entry below that threshold is
never rejected and never errors: it is loaded as a variadic function returning
`Any`, so an untrustworthy or wrong external definition can only make TypR
check *less*, never make a correct program stop compiling. `typr gen-types`
(already shipped) becomes the bootstrap path for this: its output is a valid
`T3` external definition, consumable the same way as a hand-written one.

This is the second milestone (J2) of the type-registry design in
`typR/registry.md`, whose §5 (definition format), §7 (resolution and locking)
and §8.3 (conflict order) this RFC turns into a committed contract. It
deliberately excludes the registry service itself (a hosted index of
definitions, `typr search`, `typr add` auto-discovery) — that is J3 and later,
and needs no language or CLI contract change beyond what is specified here:
until it exists, `typr types add <repo>` is the only way to point at a
definition, which is sufficient to use everything below.

## Motivation

TypR programs already import untyped R packages today, and the escape hatches
to describe them exist and work: `@extern`, `@importFrom` + a hand-written
`@name: (T) -> R;`, `Foreign<Any>` for opaque values, `Option<T>` for
`from_nullable()`. What is missing is not expressive power — it is a *place to
put* those declarations that isn't the consuming project's own source, and a
way to load them safely.

Concretely, two things are true at once and neither has an answer today:

1. **Nobody will hand-type `shiny`.** It exports on the order of 400 functions.
   `typR/typr/crates/typr-cli/src/gen_types.rs` (shipped, J1) already produces
   a `.ty` file for any installed package via `Rscript`-based introspection —
   but that file has nowhere to live except inside the consuming project, and
   nothing loads it automatically. Two projects using `shiny` today duplicate
   the same generated file, or don't bother and leave every `shiny::*` call
   untyped.

2. **A hand-written or generated definition can be wrong**, and R's ABI makes
   "wrong" dangerous, not just imprecise. `typR/ai_context/external_packages_incompatibilities.md`
   documents that a TypR value does not cross into a base-R function for free:
   `int` is `structure(x, class = c("Integer", "integer", "Any", "Generic"))`,
   `[N, T]` is an S3 list (`typed_vec`), not an atomic vector. A definition that
   only states `(int) -> int` for a function that actually strips those
   attributes on return produces code that type-checks and returns the wrong
   runtime type — silently. There is currently no mechanism that limits the
   blast radius of that mistake to "less checking" instead of "a corrupted
   type", because there is no mechanism for loading a third-party `.ty` at all.

Today, describing `dplyr::filter` requires either editing the consuming
project's own source with hand-rolled `@importFrom`/`@extern` declarations (not
shareable, not versioned against the package, redone by every project), or
nothing, leaving the call fully untyped with no hover, no signature help, no
argument names. This RFC is what turns "someone wrote this once" into
"everyone using `dplyr` in TypR benefits from it, without trusting it more than
the project asks to."

## Guide-level explanation

Say you are writing a Shiny app in TypR and want typed access to
`fluidPage`/`titlePanel`, and someone has already published a definition for
it.

**Pointing at a definition.** `typr.toml` gets a new `[types]` table. It is
*not* a dependency list — `DESCRIPTION`'s `Imports:` remains the one place R
dependencies are declared, exactly as today (`typr add shiny` still runs
`usethis::use_package('shiny')` under the hood). `[types]` only says which type
description to use for a package already imported, and how much to trust
descriptions you did not pin explicitly:

```toml
# typr.toml
[types]
trust = "T2"                          # T1 = paranoid, T2 = default, T3 = trust generated code too
shiny = "github:alice/typr-shiny"     # explicit pin — wins over everything else
```

```bash
typr types add github:alice/typr-shiny
```

resolves `alice/typr-shiny` on GitHub, fetches its `HEAD`, checks the manifest
(below), and records exactly what was fetched in a new `typr.lock`:

```toml
# typr.lock — generated, commit this file
[[definition]]
package    = "shiny"
repository = "github:alice/typr-shiny"
version    = "0.3.0"
rev        = "a1b2c3d4e5f6…"
digest     = "sha256:…"
tier       = "T2"
r_version_seen = "1.11.1"
```

From then on, `typr check`/`build`/`run` load `alice/typr-shiny`'s `.ty` files
the same way they load `std.ty` today, and:

```typr
@importFrom shiny fluidPage;
fluidPage("hello")     # hover shows the real signature and doc, not "Any"
```

type-checks against the real declared signature, with completion and hover in
the LSP and MCP showing where the definition came from and at what tier.

**No definition, or one you don't trust enough.** Nothing breaks. If `shiny`
had no pinned definition at all, `fluidPage` is exactly as untyped as any other
R name today — callable, arity-checked when known, returning `Any`
(`rfcs/0028-calling-untyped-r-functions.md`, already shipped). If a definition
*is* pinned but its declared tier is below the project's `trust`, the effect is
identical: every one of its entries loads as `(Any, …) -> Any` instead of its
declared signature. The project never has to remove or fight a bad definition
to keep building — lowering `trust`, or unpinning it, is enough, and doing
neither is also safe.

**Bootstrapping your own.** If nobody has published one yet:

```bash
typr gen-types shiny --out ./ty/
```

produces a `T3` definition from the package installed locally (arity and
argument names from `formals()`, everything else `Any`) — this already ships.
This RFC adds `typr types vendor`, which copies whatever is currently resolved
(generated or fetched) into the project's own tree, so the build stops
depending on the network or on the upstream repository's continued existence.

**How you'd explain it to an R user who knows TypR's basics but nothing about
this feature:** "The `#! tier` you've seen on standard-library entries isn't
just internal bookkeeping — you can attach the same trust levels to
descriptions of *any* R package, written by anyone, and TypR will never let a
description it doesn't fully trust turn your correct code into a type error."

## Reference-level explanation

### Definition repository layout and manifest

A definition repository (`typr-shiny/` in the example above) has this shape:

```text
typr-shiny/
├── README.md
├── typr-def.toml          # manifest
├── ty/
│   ├── core.ty
│   └── ui.ty
├── R/                     # optional — see Capabilities and R shims below
│   └── shims.R
└── tests/
    └── smoke.ty           # a program that must compile against this definition
```

`typr-def.toml`, not `typr.toml`, on purpose: the two files answer different
questions (the definition's own metadata vs. a consuming project's
configuration) and living in different repositories does not stop someone from
copy-pasting one into the other by habit if the name is shared.

```toml
format_version = 1                  # REQUIRED — an unknown format_version is refused, not guessed at

[package]
name    = "shiny"
since   = "1.11.0"                  # a floor, not a closed range — see Version compatibility below
# until = "2.0.0"                   # only when a break is *known*, never speculative

[definition]
version = "0.3.0"                   # semver of the definition itself, independent of the package's
tier    = "T2"                      # default tier for entries with no `#! tier:` of their own

[provider]
type       = "community"            # official | community | generated | local
repository = "github:alice/typr-shiny"

[capabilities]
r_shims    = false                  # ships executable R alongside the declarations?
extern_raw = false                  # uses `extern: (...) -> T r#"...R..."#` verbatim blocks?
```

`format_version` is what keeps this survivable across N repositories the
project does not control: when the definition format changes, the compiler
reads old manifests it recognizes or refuses the ones it doesn't — it never
silently misparses one.

### `.ty` files and the two new annotations

Definitions are ordinary `.ty` files, parsed and type-checked the same way as
`std.ty`, with the same `#!` annotation block already implemented in
`crates/typr-core/src/processes/spg/stdlib_meta.rs` (`pkg`, `tier`, `param`,
`ret`, `coercion`/`note`, `example`, `seealso`). That parser currently drops
any key it does not recognize (`_ => {} // ignore silently`), which is exactly
how it stays forward-compatible — this RFC uses that door to add two new keys
rather than changing the parser's shape:

```typr
#! pkg: shiny
#! tier: T2
#! since: 1.11.0
#! ret: UI object, opaque on the TypR side
#! example: fluidPage(titlePanel("hello"))
@importFrom shiny fluidPage;
@fluidPage: (Any) -> Foreign<Any>;

#! pkg: dplyr
#! tier: T3
#! since: 1.1.0
#! until: 2.0.0
#! param .data: input table
@importFrom dplyr filter;
@filter: (Any, Any) -> Any;
```

`FunctionMeta` gains `since: Option<String>` and `until: Option<String>`,
parsed identically to `ret`/`coercion` (a single value, `strip_leading_colon`).
Nothing about `@extern`/`@importFrom`/`Foreign<T>` changes: choosing between
them *is* the declaration of how the boundary is crossed
(`typR/ai_context/tuto_external_packages.md`), and that choice is exactly what
these files make.

### Loading external `.ty` into the context

`crates/typr-cli/src/standard_library.rs` builds the checking context from
`R_T1_SOURCES` (plus `R_DOC_ONLY_SOURCES`) — a fixed, embedded list of
`(filename, source)` pairs. This RFC adds a second list built at project-load
time from `typr.lock`: for each resolved definition, read its `.ty` files from
the on-disk cache (below), parse `#! tier` per entry (falling back to the
manifest's `[definition] tier` when absent), and merge into the same table
`build_typed_vartype` already builds from `R_T1_SOURCES` — with one difference
from today's uniform T1 preload: **each entry's tier is compared against the
project's configured `trust`.**

- entry tier ≥ project `trust`: loaded with its declared signature, exactly as
  `R_T1_SOURCES` entries are today.
- entry tier < project `trust`: loaded as `Type::UnknownFunction` (variadic,
  returns `Any`) instead of its declared signature — the same representation
  `rfcs/0028-calling-untyped-r-functions.md` already gives every untyped R
  name. No error, no warning bubbled up to a build failure; the LSP and MCP
  still surface the declared signature and doc (`§11` below), only the
  type-checker itself degrades.
- package with no resolved definition at all: unchanged from today — every
  name from it is `UnknownFunction` unless the project's own source declares
  it.

This is the mechanism that makes D2 (`typR/registry.md` §0) real: nothing
downstream of this merge step needs to know *why* an entry became `Any` — a
missing definition, a low tier, and an out-of-range version (next section) all
collapse to the same representation.

### Version compatibility: a floor, not a range

R has no npm/cargo-style version resolution — CRAN publishes one current
version, and a user has "whatever is installed". Declaring `supports =
["1.11.x"]` would make compatibility depend on the resolving machine (breaking
`typr.lock`'s point), produce false "incompatible" verdicts the moment a
package ships a patch release (R rarely breaks its own API), and require
upkeep nobody will do.

Instead: `since` is a floor. At load time, the introspected installed version
(already recovered by `introspect_pkg.R`'s `P` line, used today by
`r_name_cache`) is compared against it:

- installed ≥ `since` (and ≤ `until`, when present): used without warning.
- installed < `since`, or > `until` when declared: TypR **warns and degrades to
  `Any`** for that package's entries — never refuses to build. The version
  actually observed is written into `typr.lock` as `r_version_seen`, which is
  what lets any future CI on a definition repository notice drift between what
  it declares and what real installs report.

### Resolution, `typr.lock`, cache, vendoring

```text
typr add shiny
     → DESCRIPTION: Imports += shiny        (usethis — unchanged)
     → typr.toml [types]: explicit pin for `shiny`?
           yes → use it, done
           no  → no definition resolved (until J3's registry exists to search)
     → fetch resolved repository at HEAD, or the pinned rev
     → verify content digest
     → ~/.cache/typr/types/<pkg>/<digest>/
     → typr.lock updated
     → available to the compiler, the LSP and the MCP
```

New CLI surface (`crates/typr-cli/src/cli.rs`, alongside the existing `Add`,
`Check`, `Cache` subcommands):

| Command | Role |
|---|---|
| `typr add <pkg>` | unchanged (`usethis::use_package`) — this RFC adds nothing here until J3 lets it also look up a definition automatically |
| `typr types add <repo>` | pin a definition (`github:owner/repo[@rev]`) — fetch, verify, write into `typr.toml`/`typr.lock` |
| `typr types update [pkg]` | re-fetch and re-pin `typr.lock` for one or all resolved definitions |
| `typr types list` | what's resolved, with tier and provenance |
| `typr types vendor` | copy resolved `.ty` files into the project tree, so the build no longer depends on the network or on the upstream repository still existing |

`typr.toml`'s `[types]` and `typr.lock` are new files with no analog in the
project today; `DESCRIPTION` is untouched and remains authoritative for R
dependencies (`§7.1` — one source of truth per question).

### Capabilities and R shims

A `.ty` file is inert; `extern: (…) -> T r#"…R…"#` blocks and any `R/` shim
directory are not — they execute in the consuming project's process. The
threat model is `npm postinstall`'s, not `@types/*`'s package.json's. This RFC
adopts the manifest's `[capabilities]` gate as a hard rule, not a convention:

- a definition whose manifest leaves `r_shims`/`extern_raw` at their default
  (`false`) but ships either anyway is **rejected at fetch time**, before a
  single byte of it is loaded — not just flagged in CI on the definition's own
  repository.
- a definition that sets either to `true` shows an explicit warning on the
  first `typr types add`/`typr add` that resolves it, and requires
  confirmation (or `--allow-r`).
- nothing is executed during discovery or resolution: fetching a definition is
  a source download, full stop; only actually building/running the consuming
  project can execute a shim, exactly as it would execute any other R in the
  project.

### Failure modes are never hard errors

Every one of the following degrades to the *some entries are `Any`* case
above; none of them fails a build:

- the pinned repository is unreachable (network, deleted, rewritten history —
  digest mismatch on the cached copy).
- the resolved manifest has no `format_version`, or one this compiler build
  doesn't recognize (the *definition* is refused; the package's names fall
  back to fully untyped, same as having none).
- `since`/`until` exclude the installed R package version.
- the project's `trust` excludes the definition's declared tier.

## Gradual typing

This proposal is gradual typing's boundary case taken to its logical end: a
package with **zero** resolved definitions behaves exactly as today (every
name `UnknownFunction`, callable, arity-checked when knowable, `Any`-typed
result — `rfcs/0028`). Resolving a `T3` (generated) definition changes *only*
argument names and documentation surfaced to a human or to the LSP/MCP; the
type-checker's view is unchanged, because `T3` entries load exactly like
`UnknownFunction` today. Type constraints appear at `T2`/`T1`, and only for the
entries that actually declare them, only when the project's own `trust`
accepts that tier. There is no annotation density this proposal forces:
a project that never touches `typr.toml [types]` sees no behavior change at
all.

## Drawbacks

- **Two new project files** (`typr.toml [types]`, `typr.lock`) for a project
  type (R packages) that already has one dependency manifest (`DESCRIPTION`).
  The mitigation is the explicit division of labor in §7.1 of
  `typR/registry.md`, but it is still a second file to explain to newcomers.
- **The trust/degradation model is invisible by default.** A project that sets
  `trust = "T2"` and pins a `T3` definition gets *no* type checking for that
  package and no error telling it so — by design (D2), but it means a mistake
  here reads as "TypR isn't catching this" rather than as a loud failure.
  `typr types list` showing tier per package is the mitigation; it needs to be
  something people actually run.
- **Fetching arbitrary GitHub repositories as part of a build-adjacent command**
  (`typr types add`, `typr types update`) is new network/trust surface for a
  compiler CLI that has not had it before, even with the capability gate in
  place. The digest-pinning in `typr.lock` bounds this to "you get what you
  first approved," not "you get whatever is at HEAD right now" — but it is a
  new class of thing this CLI does.
- **`since`/`until` add two more `#!` keys to a metadata format that already
  has seven**; each addition is small, but the format is not designed against
  a fixed budget, and every key is a piece every generator and consumer has to
  handle for good, per `format_version`'s forward-compatibility promise.

## Rationale and alternatives

The central choice is D2 (`typR/registry.md` §0): **degrade to `Any`, never
error.** The alternative — treat a definition below trust, or a version
mismatch, as a hard type error — was considered and rejected, because a
third-party definition is exactly the kind of input a project does not
control, and "a package you don't maintain published something" becoming
"your correct code stops compiling" is the single failure mode a community
type registry cannot survive. This is only possible because
`rfcs/0028-calling-untyped-r-functions.md` already made `UnknownFunction`
variadic and callable — before that RFC, "degrade to `Any`" had nowhere safe to
land.

The second choice is pinning by content digest in a lockfile rather than a
semver range (`registry.md` §7.2, §D4/§D5): R has no ecosystem-wide version
resolver, so a range like `supports = ["1.11.x"]` would depend on the
resolving machine and rot without anyone noticing. A digest is the only thing
that is reproducible without inventing R version-range semantics R itself
doesn't have.

The cost of doing nothing is not small: it is the status quo described in
Motivation — every project hand-rolling or duplicating its own `@importFrom`
declarations for the same handful of popular packages, with no sharing
mechanism at all. `typr gen-types` (J1, already shipped) is only half-useful
without this RFC, because its output currently has nowhere to be shared or
loaded except by hand-copying files between projects.

## Prior art

- **R itself**: no package in CRAN or Bioconductor ships a machine-readable
  type description of its own API; `Rd`/roxygen comments are prose. There is
  no existing convention to be compatible with, which is why this proposal
  invents one rather than adapting one.
- **DefinitelyTyped** (`@types/*` for TypeScript) is the closest analog:
  community-maintained type descriptions decoupled from the packages they
  describe, in a monorepo. Its main lesson, already reflected in
  `registry.md` §8.2 (not this RFC's scope, since J2 has no registry yet): a
  single monorepo makes format migrations tractable across a long tail of
  small packages, at the cost of requiring PRs into a repo the community
  doesn't fully own. Its second lesson, which *is* this RFC's concern:
  `@types` packages can and do drift from the real library's behavior, and
  TypeScript has no equivalent of D2 — a wrong `@types` package produces
  compile errors on correct code, which is the exact failure this RFC's
  degradation rule is designed to avoid.
- **Rust's `cargo vendor`** is the direct model for `typr types vendor`:
  making a build reproducible and independent of an upstream repository's
  continued existence, distinct from the cache used for day-to-day iteration.
- **npm's `postinstall` scripts** are the threat model for the capabilities
  gate in §"Capabilities and R shims" — the industry's cautionary tale for
  "a dependency description can execute arbitrary code," which is why this
  RFC treats `r_shims`/`extern_raw` as a hard gate rather than a documented
  convention.

## Unresolved questions

Carried over from `registry.md` §14, to the extent they bear on J2 rather than
the registry service itself:

- **Q1** — is `typr-def.toml` the right filename, or should it be folded into
  something reused elsewhere? This RFC takes the name as settled for the
  purpose of shipping J2; revisiting it later is a rename, not a redesign.
- **Q2** — do third-party definitions get to ship R at all (shims,
  `extern r#"…"#`)? This RFC allows it behind the `[capabilities]` gate, but
  whether entire categories of package (those needing S3/S4 shims) are
  reachable *without* shims is open, and may push some of that need into the
  standard library instead of third-party definitions.
- **Q4** — a definition's own `type X <- Foreign<Any>;` declarations leak into
  the consuming project's namespace; this RFC does not specify a namespacing
  rule for that, and two definitions declaring the same type name is
  unresolved.
- **Q5** — can a definition be type-checked (in the definition repository's
  own CI, or when a consuming project resolves it) when its package is not
  installed locally? This determines whether definition repositories, and
  projects that pin them, need R + the package installed just to run `typr
  check`. Left open for J2; matters more once J4 (registry CI) exists.

## Future possibilities

Explicitly out of scope here, deferred to `registry.md`'s later milestones:

- **J3 — the registry itself**: a hosted index (`typr-lang/registry`) that
  lets `typr add`/`typr types update` *discover* a definition instead of
  requiring an explicit `typr types add <repo>`, plus `typr search`. Nothing
  in this RFC needs to change for that to land — J3 only adds a lookup step
  before the resolution flow specified here, and the conflict order in
  `registry.md` §8.3 (explicit pin, then official, then community by tier,
  then locally generated, then nothing) already anticipates it.
- **J4 — registry CI**: mechanical validation (exports exist, arity matches
  `formals()`, no unconstrained `...` at `T1`) and drift re-detection over
  time, run centrally instead of per-consumer.
- **J6 — the Store**: a web UI over the registry. Explicitly optional per
  `registry.md` D6, and needs nothing from this RFC beyond the manifest and
  lockfile shapes already specified.
- Typing S4/R6/S7 by anything more precise than `Foreign<T>` is its own RFC,
  out of scope here as it is in `registry.md` §15.

## Implementation checklist

<!-- Filled in as the work lands, after acceptance. -->

- [ ] `typr-def.toml` manifest parsing + `format_version` gate
- [ ] `since`/`until` added to `FunctionMeta` (`stdlib_meta.rs`) and to the
      manifest
- [ ] external `.ty` loading merged into `standard_library.rs`'s context build,
      keyed by resolved `typr.lock` entries
- [ ] `trust` threshold + degrade-to-`UnknownFunction` at merge time
- [ ] `typr types add|update|list|vendor`, `typr.lock` read/write, digest
      verification, `~/.cache/typr/types/<pkg>/<digest>/`
- [ ] `cases/`: missing definition, tier below `trust`, version below `since`,
      version above `until`, unreachable repository, digest mismatch — each
      asserting *no hard error*, only degradation
- [ ] `[capabilities]` gate enforced at fetch time (reject undeclared R;
      confirm-or-`--allow-r` for declared)
- [ ] `syntaxe.md` — no lexeme changes expected, but confirm before merge
- [ ] Documentation PR on `we-data-ch/typr.github.io` (a How-to page for
      consuming an external definition; a Reference page for the manifest and
      `#!` keys), landing in the same release
- [ ] `Implemented in:` filled in above
