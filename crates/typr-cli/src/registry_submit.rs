//! `typr types submit <pkg> [github:owner/repo[/subdir][@rev]]` — open a pull
//! request against `we-data-ch/registry` adding or updating
//! `packages/<pkg>.json`. This is the "« Add to Registry » → PR automatique"
//! item of `typR/registry.md` §13 J6 (second half) — the one item that was
//! still unchecked once J0-J5 and J6's static Store page were done.
//!
//! Deliberately **not** what registry.md §12/D6 describes and defers as its
//! own project: a Store web form that generates a PR "on behalf of a user"
//! needs a GitHub App, stored tokens, a backend, and anti-spam moderation.
//! This is the CLI shape instead — it shells out to the caller's own,
//! already-authenticated `gh` (the GitHub CLI), the same "shell out rather
//! than add an in-process client" choice `type_registry.rs` already made for
//! `git` and `gen_types.rs` made for `Rscript`. There is no backend and no
//! app: the PR is opened as the actual signed-in `gh` user, from their own
//! fork, exactly as if they had typed the `gh repo fork`/`git push`/`gh pr
//! create` sequence by hand.
//!
//! Never opens a PR for a definition that hasn't been mechanically checked:
//! `submit` runs the same checks `typr types validate` runs
//! (`registry_validate::validate`) first and refuses to proceed if any of
//! them fails. It also refuses to open a no-op PR when the target
//! `packages/<pkg>.json` already contains an identical entry for the same
//! repository.

use crate::type_definition::ProviderType;
use crate::type_registry::{self, FetchedDefinition, LockedDefinition, Lockfile, RepoSpec, LOCKFILE_NAME};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// The registry this build submits to by default — same one
/// `type_registry.rs`'s `REGISTRY_REPO_URL` points at, but as `owner/repo`
/// rather than a clone URL, since `gh repo fork`/`gh pr create` want it in
/// that form.
pub const DEFAULT_REGISTRY_REPO: &str = "we-data-ch/registry";

// ---------------------------------------------------------------------
// packages/<pkg>.json — full read/write shape
// ---------------------------------------------------------------------
//
// `type_registry.rs`'s own `RegistryDefinitionEntry`/`RegistryPackageFile`
// are intentionally partial — "only the fields the selection logic needs"
// (its own doc comment). Submitting has to read and write the *whole* file
// (every field `schema/package.schema.json` requires) and must not lose an
// existing sibling entry for another repository, so this module keeps its
// own full round-trippable shape instead of widening that one.

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
struct CapabilitiesJson {
    r_shims: bool,
    extern_raw: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
struct DefinitionEntryJson {
    /// `owner/repo` or `owner/repo/subdir...` — no `github:` scheme, matching
    /// `schema/package.schema.json`'s `repository` pattern.
    repository: String,
    version: String,
    rev: String,
    since: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    until: Option<String>,
    /// `official` | `community` | `generated` | `local`.
    source: String,
    /// `T1` | `T2` | `T3`.
    tier: String,
    capabilities: CapabilitiesJson,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct PackageFileJson {
    name: String,
    #[serde(default)]
    definitions: Vec<DefinitionEntryJson>,
}

fn provider_type_str(kind: ProviderType) -> &'static str {
    match kind {
        ProviderType::Official => "official",
        ProviderType::Community => "community",
        ProviderType::Generated => "generated",
        ProviderType::Local => "local",
    }
}

/// `spec.owner/spec.repo[/spec.subdir]` — the `repository` field's shape in
/// the registry (registry.md §8.1/§8.2), derived from the same `RepoSpec` a
/// user would otherwise write into `typr.toml [types]` or pass to `typr
/// types add`.
fn repository_field(spec: &RepoSpec) -> String {
    match &spec.subdir {
        Some(sub) => format!("{}/{}/{}", spec.owner, spec.repo, sub),
        None => format!("{}/{}", spec.owner, spec.repo),
    }
}

fn build_registry_entry(spec: &RepoSpec, fetched: &FetchedDefinition) -> DefinitionEntryJson {
    DefinitionEntryJson {
        repository: repository_field(spec),
        version: fetched.manifest.definition.version.clone(),
        rev: fetched.rev.clone(),
        since: fetched.manifest.package.since.clone(),
        until: fetched.manifest.package.until.clone(),
        source: provider_type_str(fetched.manifest.provider.kind).to_string(),
        tier: fetched.manifest.definition.tier.clone(),
        capabilities: CapabilitiesJson {
            r_shims: fetched.manifest.capabilities.r_shims,
            extern_raw: fetched.manifest.capabilities.extern_raw,
        },
    }
}

/// Insert or replace `entry` in `<work_dir>/packages/<package>.json`, keyed
/// by `repository` (a package can list several definitions, registry.md
/// §8.3 — this only ever touches the one matching this submission's
/// repository, every sibling entry is preserved byte-for-byte apart from
/// JSON re-formatting). Returns `false` without touching the file when an
/// identical entry is already present, so the caller can skip opening a
/// no-op PR.
fn upsert_package_entry(work_dir: &Path, package: &str, entry: DefinitionEntryJson) -> Result<bool, String> {
    let path = work_dir.join("packages").join(format!("{package}.json"));
    let mut file = if path.is_file() {
        let source = fs::read_to_string(&path).map_err(|e| format!("could not read {}: {e}", path.display()))?;
        serde_json::from_str::<PackageFileJson>(&source).map_err(|e| {
            format!(
                "{} does not match the registry's own schema — refusing to overwrite it: {e}",
                path.display()
            )
        })?
    } else {
        PackageFileJson {
            name: package.to_string(),
            definitions: Vec::new(),
        }
    };

    let changed = match file.definitions.iter_mut().find(|d| d.repository == entry.repository) {
        Some(existing) if *existing == entry => false,
        Some(existing) => {
            *existing = entry;
            true
        }
        None => {
            file.definitions.push(entry);
            true
        }
    };
    if !changed {
        return Ok(false);
    }

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| format!("could not create {}: {e}", parent.display()))?;
    }
    let rendered =
        serde_json::to_string_pretty(&file).map_err(|e| format!("could not serialize {}: {e}", path.display()))?;
    fs::write(&path, format!("{rendered}\n")).map_err(|e| format!("could not write {}: {e}", path.display()))?;
    Ok(true)
}

// ---------------------------------------------------------------------
// gh / git plumbing
// ---------------------------------------------------------------------

fn gh_available() -> bool {
    Command::new("gh")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn gh_authenticated() -> bool {
    Command::new("gh")
        .args(["auth", "status"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn now_millis() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis())
        .unwrap_or(0)
}

/// Fork `registry_repo` into the signed-in `gh` user's account (a no-op if
/// they already have one) and clone that fork into a fresh temp directory,
/// with `origin` pointing at the fork and `upstream` at `registry_repo` —
/// exactly the layout `gh repo fork --clone --remote` sets up by hand, and
/// exactly what `gh pr create` below needs to infer the PR's head without
/// being told explicitly.
fn fork_and_clone(registry_repo: &str) -> Result<PathBuf, String> {
    let repo_name = registry_repo.rsplit('/').next().unwrap_or(registry_repo);
    let parent = std::env::temp_dir().join(format!("typr_types_submit_{}_{}", std::process::id(), now_millis()));
    fs::create_dir_all(&parent).map_err(|e| format!("could not create temp dir: {e}"))?;

    let fork = Command::new("gh")
        .args(["repo", "fork", registry_repo, "--clone", "--remote"])
        .current_dir(&parent)
        .output()
        .map_err(|e| format!("could not run `gh repo fork`: {e}"))?;
    if !fork.status.success() {
        let _ = fs::remove_dir_all(&parent);
        return Err(format!(
            "`gh repo fork {registry_repo}` failed: {}",
            String::from_utf8_lossy(&fork.stderr).trim()
        ));
    }

    let work_dir = parent.join(repo_name);
    if !work_dir.is_dir() {
        let _ = fs::remove_dir_all(&parent);
        return Err(format!(
            "`gh repo fork {registry_repo}` reported success but {} was not created",
            work_dir.display()
        ));
    }
    Ok(work_dir)
}

fn run_git(work_dir: &Path, args: &[&str], what: &str) -> Result<(), String> {
    let out = Command::new("git")
        .arg("-C")
        .arg(work_dir)
        .args(args)
        .output()
        .map_err(|e| format!("could not run `git {}`: {e}", args.join(" ")))?;
    if !out.status.success() {
        return Err(format!(
            "{what} failed: {}",
            String::from_utf8_lossy(&out.stderr).trim()
        ));
    }
    Ok(())
}

fn commit_and_push(work_dir: &Path, package: &str, branch: &str) -> Result<(), String> {
    run_git(work_dir, &["checkout", "-b", branch], "`git checkout -b`")?;
    run_git(work_dir, &["add", &format!("packages/{package}.json")], "`git add`")?;
    let message = format!("Add/update packages/{package}.json via `typr types submit`");
    run_git(work_dir, &["commit", "--quiet", "-m", &message], "`git commit`")?;
    run_git(work_dir, &["push", "--quiet", "-u", "origin", branch], "`git push`")
}

/// `gh pr create`, run from inside the fork clone so `gh` infers the head
/// branch/owner from the checked-out branch and its `origin` remote — the
/// same thing a person would get running it by hand right after `git push`.
fn open_pr(work_dir: &Path, registry_repo: &str, package: &str, entry: &DefinitionEntryJson) -> Result<String, String> {
    let title = format!("Add/update {package}: {} (tier {})", entry.repository, entry.tier);
    let body = format!(
        "Adds or updates `packages/{package}.json` for `{}` at rev `{}` (tier `{}`, source `{}`).\n\n\
         Opened by `typr types submit` (`typR/registry.md` §13 J6). Passed `typr types \
         validate {package} github:{}@{}` locally before this PR was opened — see that command's \
         own output for exactly what was and was not mechanically checked (`registry.md` §9: \
         this PR is not a claim that the definition is correct beyond what was mechanically \
         verifiable).\n",
        entry.repository, entry.rev, entry.tier, entry.source, entry.repository, entry.rev,
    );
    let pr = Command::new("gh")
        .current_dir(work_dir)
        .args([
            "pr",
            "create",
            "--repo",
            registry_repo,
            "--title",
            &title,
            "--body",
            &body,
        ])
        .output()
        .map_err(|e| format!("could not run `gh pr create`: {e}"))?;
    if !pr.status.success() {
        return Err(format!(
            "`gh pr create` failed: {}",
            String::from_utf8_lossy(&pr.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&pr.stdout).trim().to_string())
}

// ---------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubmitOutcome {
    /// `packages/<pkg>.json` already had this exact entry — no PR opened.
    AlreadyUpToDate,
    Opened(String),
}

/// A repository spec to submit for `package`: the one given explicitly, or —
/// mirroring `type_registry::update`'s own fallback — the one already
/// resolved in `typr.lock` (pinned rev included, so what gets submitted is
/// exactly what this project already trusts, not a possibly-moved `HEAD`).
fn resolve_spec_to_submit(project_root: &Path, package: &str, spec_override: Option<&str>) -> Result<String, String> {
    if let Some(spec) = spec_override {
        return Ok(spec.to_string());
    }
    let lockfile = Lockfile::read(&project_root.join(LOCKFILE_NAME));
    let locked: &LockedDefinition = lockfile.find(package).ok_or_else(|| {
        format!(
            "no repository given and `{package}` is not resolved in typr.lock — run `typr types \
             add {package} github:owner/repo` first, or pass one explicitly: `typr types submit \
             {package} github:owner/repo[@rev]`"
        )
    })?;
    Ok(format!("{}@{}", locked.repository, locked.rev))
}

/// `typr types submit <package> [repo]` — resolve a spec, validate it
/// mechanically, and open (or update) a pull request against `registry_repo`
/// indexing it. Every temp directory this creates (the fetch's clone, the
/// fork's clone) is cleaned up before returning, success or failure.
pub fn submit(
    project_root: &Path,
    package: &str,
    spec_override: Option<&str>,
    registry_repo: &str,
) -> Result<SubmitOutcome, String> {
    if !gh_available() {
        return Err(
            "`gh` (the GitHub CLI) is not on PATH — install it from https://cli.github.com, run \
             `gh auth login`, then retry; `typr types submit` opens the PR as you, from your own \
             fork, it never touches your GitHub credentials directly"
                .to_string(),
        );
    }
    if !gh_authenticated() {
        return Err("`gh` is not authenticated — run `gh auth login` first".to_string());
    }

    let spec_str = resolve_spec_to_submit(project_root, package, spec_override)?;
    let repo_spec = RepoSpec::parse(&spec_str)?;

    let report = crate::registry_validate::validate(package, &spec_str);
    if !report.ok() {
        return Err(format!(
            "`{package}` at {spec_str} fails mechanical validation — fix it before submitting \
             (rerun `typr types validate {package} {spec_str}` for details):\n{}",
            report.render()
        ));
    }

    let (fetched, _warnings) = type_registry::fetch(&repo_spec)?;
    let entry = build_registry_entry(&repo_spec, &fetched);
    let _ = fs::remove_dir_all(&fetched.root);

    let work_dir = fork_and_clone(registry_repo)?;
    let result = (|| {
        if !upsert_package_entry(&work_dir, package, entry.clone())? {
            return Ok(SubmitOutcome::AlreadyUpToDate);
        }
        let branch = format!("typr-types-submit-{package}-{}", &entry.rev[..entry.rev.len().min(12)]);
        commit_and_push(&work_dir, package, &branch)?;
        let pr_url = open_pr(&work_dir, registry_repo, package, &entry)?;
        Ok(SubmitOutcome::Opened(pr_url))
    })();
    let _ = fs::remove_dir_all(work_dir.parent().unwrap_or(&work_dir));
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_entry(repository: &str) -> DefinitionEntryJson {
        DefinitionEntryJson {
            repository: repository.to_string(),
            version: "0.1.0".to_string(),
            rev: "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2".to_string(),
            since: "1.0.0".to_string(),
            until: None,
            source: "community".to_string(),
            tier: "T2".to_string(),
            capabilities: CapabilitiesJson {
                r_shims: false,
                extern_raw: false,
            },
        }
    }

    #[test]
    fn repository_field_includes_subdir_when_present() {
        let spec = RepoSpec::parse("github:we-data-ch/registry/definitions/dplyr").unwrap();
        assert_eq!(repository_field(&spec), "we-data-ch/registry/definitions/dplyr");

        let spec = RepoSpec::parse("github:alice/typr-shiny").unwrap();
        assert_eq!(repository_field(&spec), "alice/typr-shiny");
    }

    #[test]
    fn upsert_creates_a_new_file_when_none_exists() {
        let dir = std::env::temp_dir().join(format!("typr_submit_new_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let changed = upsert_package_entry(&dir, "shiny", sample_entry("alice/typr-shiny")).unwrap();
        assert!(changed);

        let written = fs::read_to_string(dir.join("packages").join("shiny.json")).unwrap();
        let parsed: PackageFileJson = serde_json::from_str(&written).unwrap();
        assert_eq!(parsed.name, "shiny");
        assert_eq!(parsed.definitions.len(), 1);
        assert_eq!(parsed.definitions[0].repository, "alice/typr-shiny");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn upsert_preserves_sibling_entries_for_other_repositories() {
        let dir = std::env::temp_dir().join(format!("typr_submit_sibling_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("packages")).unwrap();
        let existing = PackageFileJson {
            name: "shiny".to_string(),
            definitions: vec![sample_entry("bob/other-typr-shiny")],
        };
        fs::write(
            dir.join("packages").join("shiny.json"),
            serde_json::to_string_pretty(&existing).unwrap(),
        )
        .unwrap();

        let changed = upsert_package_entry(&dir, "shiny", sample_entry("alice/typr-shiny")).unwrap();
        assert!(changed);

        let written = fs::read_to_string(dir.join("packages").join("shiny.json")).unwrap();
        let parsed: PackageFileJson = serde_json::from_str(&written).unwrap();
        assert_eq!(parsed.definitions.len(), 2);
        assert!(parsed
            .definitions
            .iter()
            .any(|d| d.repository == "bob/other-typr-shiny"));
        assert!(parsed.definitions.iter().any(|d| d.repository == "alice/typr-shiny"));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn upsert_replaces_the_matching_repository_entry_in_place() {
        let dir = std::env::temp_dir().join(format!("typr_submit_replace_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("packages")).unwrap();
        let mut first = sample_entry("alice/typr-shiny");
        first.tier = "T3".to_string();
        let existing = PackageFileJson {
            name: "shiny".to_string(),
            definitions: vec![first],
        };
        fs::write(
            dir.join("packages").join("shiny.json"),
            serde_json::to_string_pretty(&existing).unwrap(),
        )
        .unwrap();

        let mut updated = sample_entry("alice/typr-shiny");
        updated.tier = "T1".to_string();
        let changed = upsert_package_entry(&dir, "shiny", updated).unwrap();
        assert!(changed);

        let written = fs::read_to_string(dir.join("packages").join("shiny.json")).unwrap();
        let parsed: PackageFileJson = serde_json::from_str(&written).unwrap();
        assert_eq!(parsed.definitions.len(), 1);
        assert_eq!(parsed.definitions[0].tier, "T1");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn upsert_is_a_no_op_when_the_entry_is_already_identical() {
        let dir = std::env::temp_dir().join(format!("typr_submit_noop_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("packages")).unwrap();
        let entry = sample_entry("alice/typr-shiny");
        let existing = PackageFileJson {
            name: "shiny".to_string(),
            definitions: vec![entry.clone()],
        };
        fs::write(
            dir.join("packages").join("shiny.json"),
            serde_json::to_string_pretty(&existing).unwrap(),
        )
        .unwrap();

        let changed = upsert_package_entry(&dir, "shiny", entry).unwrap();
        assert!(!changed);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn resolve_spec_prefers_explicit_override_over_typr_lock() {
        let dir = std::env::temp_dir().join(format!("typr_submit_resolve_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let spec = resolve_spec_to_submit(&dir, "shiny", Some("github:alice/typr-shiny@abc123")).unwrap();
        assert_eq!(spec, "github:alice/typr-shiny@abc123");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn resolve_spec_falls_back_to_typr_lock_pinned_rev() {
        let dir = std::env::temp_dir().join(format!("typr_submit_resolve_lock_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let mut lockfile = Lockfile::default();
        lockfile.upsert(LockedDefinition {
            package: "shiny".to_string(),
            repository: "github:alice/typr-shiny".to_string(),
            version: "0.3.0".to_string(),
            rev: "a1b2c3d4e5f6".to_string(),
            digest: "sha256:deadbeef".to_string(),
            tier: "T2".to_string(),
            r_version_seen: None,
        });
        lockfile.write(&dir.join(LOCKFILE_NAME)).unwrap();

        let spec = resolve_spec_to_submit(&dir, "shiny", None).unwrap();
        assert_eq!(spec, "github:alice/typr-shiny@a1b2c3d4e5f6");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn resolve_spec_errors_when_nothing_is_locked_and_nothing_is_given() {
        let dir = std::env::temp_dir().join(format!("typr_submit_resolve_missing_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let err = resolve_spec_to_submit(&dir, "shiny", None).unwrap_err();
        assert!(err.contains("typr types add"), "unexpected error: {err}");

        let _ = fs::remove_dir_all(&dir);
    }
}
