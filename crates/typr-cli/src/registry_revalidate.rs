//! `typr types revalidate` — periodic revalidation of every Type Definition
//! indexed by `we-data-ch/registry`, and detection of drift since the last
//! run. `typR/registry.md` §13 J4, "revalidation périodique des définitions
//! déjà indexées (détection de dérive)" — the item `registry_validate.rs`'s
//! own doc comment named as still open when it landed the single-repository
//! checks of §9.
//!
//! `registry_validate::validate` answers "is this one definition healthy
//! *today*". This module answers two questions a single on-demand run
//! cannot: "is *everything* the registry lists healthy today" (by walking
//! `type_registry::list_registry_targets` instead of one repository), and
//! "did anything that was fine *stop* being fine" (by diffing against a
//! persisted snapshot of the previous run) — the actual failure mode this
//! guards against is a CRAN release silently breaking a definition nobody
//! touched (registry.md §9: "le mode de mort n° 1 d'un registre
//! communautaire").
//!
//! This is meant to run centrally — a scheduled CI job in `we-data-ch/registry`
//! itself, not a command a consuming project would ever call — which is why
//! `revalidate` takes an optional `--dir` pointing at a *registry* checkout
//! (a CI job that has already checked one out) rather than a project root;
//! omitted, it falls back to the same synced mirror `typr search` already
//! uses (`type_registry::sync_registry_index`).

use crate::registry_validate::{self, CheckStatus};
use crate::type_registry;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

// ---------------------------------------------------------------------
// Persisted snapshot shape
// ---------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SnapshotCheck {
    pub name: String,
    /// "ok" | "warning" | "FAILED" | "not checked" — `CheckStatus`'s own
    /// display words (`ValidationReport::render`), kept as text rather than
    /// re-deriving the enum so an older snapshot file with a status word this
    /// build no longer emits still deserializes instead of breaking the diff.
    pub status: String,
    pub detail: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Snapshot {
    pub package: String,
    /// `github:owner/repo[@rev]` — doubles as the identity key alongside
    /// `package` for diffing against the previous run (registry.md §8.3: one
    /// package can resolve several definitions, so `package` alone isn't a
    /// key).
    pub repository: String,
    pub definition_version: String,
    /// `YYYY-MM-DD`, UTC — the "last verified" date registry.md §9's example
    /// report shows next to each check.
    pub checked_at: String,
    pub ok: bool,
    pub checks: Vec<SnapshotCheck>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct SnapshotFile {
    #[serde(default)]
    definitions: Vec<Snapshot>,
}

// ---------------------------------------------------------------------
// Drift
// ---------------------------------------------------------------------

/// What changed since the previous persisted run, keyed by `(package,
/// repository)`. Informational, not a build gate on its own (D2/D5) — a
/// caller (the CLI, a CI job) decides what to do with it; `typr types
/// revalidate` exits non-zero only when `newly_failing` is non-empty, so a
/// long-standing, already-known failure doesn't keep paging someone forever.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Drift {
    /// `ok` last run (or never checked before) — `Fail` now.
    pub newly_failing: Vec<(String, String)>,
    /// `Fail` last run — `ok` now.
    pub recovered: Vec<(String, String)>,
    /// Checked last run, no longer listed by the registry at all (the
    /// `packages/<pkg>.json` entry, or the whole file, was removed).
    pub removed: Vec<(String, String)>,
}

impl Drift {
    pub fn is_empty(&self) -> bool {
        self.newly_failing.is_empty() && self.recovered.is_empty() && self.removed.is_empty()
    }
}

/// Compare `previous` against `current`, both keyed by `(package,
/// repository)`. An entry with no prior record counts as "newly failing" when
/// it fails now — first sight of a problem is still news — but never as
/// "recovered" or "removed", which need an actual prior record to compare
/// against.
fn diff(previous: &[Snapshot], current: &[Snapshot]) -> Drift {
    let prev_by_key: HashMap<(&str, &str), &Snapshot> = previous
        .iter()
        .map(|s| ((s.package.as_str(), s.repository.as_str()), s))
        .collect();
    let curr_keys: HashSet<(&str, &str)> = current
        .iter()
        .map(|s| (s.package.as_str(), s.repository.as_str()))
        .collect();

    let mut drift = Drift::default();
    for snap in current {
        let key = (snap.package.as_str(), snap.repository.as_str());
        match (prev_by_key.get(&key).map(|p| p.ok), snap.ok) {
            (Some(true), false) | (None, false) => drift
                .newly_failing
                .push((snap.package.clone(), snap.repository.clone())),
            (Some(false), true) => drift.recovered.push((snap.package.clone(), snap.repository.clone())),
            _ => {}
        }
    }
    for prev in previous {
        let key = (prev.package.as_str(), prev.repository.as_str());
        if !curr_keys.contains(&key) {
            drift.removed.push((prev.package.clone(), prev.repository.clone()));
        }
    }
    drift.newly_failing.sort();
    drift.recovered.sort();
    drift.removed.sort();
    drift
}

// ---------------------------------------------------------------------
// Running the checks
// ---------------------------------------------------------------------

fn status_word(status: CheckStatus) -> &'static str {
    match status {
        CheckStatus::Pass => "ok",
        CheckStatus::Warn => "warning",
        CheckStatus::Fail => "FAILED",
        CheckStatus::Skipped => "not checked",
    }
}

/// Run `registry_validate::validate` against every target `type_registry::
/// list_registry_targets(registry_dir)` lists, stamping each result with
/// today's date. Real work per target (a `git clone`, a type-check, an
/// `Rscript` introspection) — this is the periodic/CI path, not an
/// interactive one, so it is expected to take as long as the registry is big.
fn run(registry_dir: &Path) -> Vec<Snapshot> {
    let checked_at = today();
    type_registry::list_registry_targets(registry_dir)
        .into_iter()
        .map(|target| {
            let report = registry_validate::validate(&target.package, &target.spec);
            Snapshot {
                package: target.package,
                repository: target.spec,
                definition_version: report.definition_version.clone(),
                checked_at: checked_at.clone(),
                ok: report.ok(),
                checks: report
                    .checks
                    .iter()
                    .map(|c| SnapshotCheck {
                        name: c.name.to_string(),
                        status: status_word(c.status).to_string(),
                        detail: c.detail.clone(),
                    })
                    .collect(),
            }
        })
        .collect()
}

/// A missing or unparsable snapshot file is simply "no previous run" — never
/// a hard error (D2), same contract as `type_registry::Lockfile::read`.
fn load_previous(path: &Path) -> Vec<Snapshot> {
    fs::read_to_string(path)
        .ok()
        .and_then(|s| serde_json::from_str::<SnapshotFile>(&s).ok())
        .map(|f| f.definitions)
        .unwrap_or_default()
}

fn save(path: &Path, snapshots: &[Snapshot]) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(|e| format!("could not create {}: {e}", parent.display()))?;
        }
    }
    let rendered = serde_json::to_string_pretty(&SnapshotFile {
        definitions: snapshots.to_vec(),
    })
    .map_err(|e| format!("could not serialize {}: {e}", path.display()))?;
    fs::write(path, rendered).map_err(|e| format!("could not write {}: {e}", path.display()))
}

// ---------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------

/// `typr types revalidate [--dir PATH] [--out FILE]`.
///
/// `registry_dir`: a local checkout of `we-data-ch/registry` to validate as-is
/// (e.g. a CI job's own working tree, so a PR can be checked before it merges)
/// — `None` syncs the same local mirror `typr search`/`typr types add` use.
///
/// `out`: where the previous run's snapshot is read from and the new one is
/// written to. Defaults to `<registry_dir>/status/validation.json`, so a CI
/// job that commits its checkout's working tree back naturally persists
/// history across runs without needing to know the path in advance.
///
/// Fails only when the registry truly cannot be consulted at all (no `git`,
/// clone/pull failure, or a snapshot that cannot be written) — matches
/// `type_registry::search`'s Err semantics. A registry with nothing indexed
/// yet, or one where every check is `Skipped`/`Warn`, resolves fine (D2): an
/// absent or uncertain signal is never a reason to fail the job that
/// discovers it.
pub fn revalidate(registry_dir: Option<&Path>, out: Option<&Path>) -> Result<(Vec<Snapshot>, Drift, PathBuf), String> {
    let dir = match registry_dir {
        Some(d) => d.to_path_buf(),
        None => {
            if !type_registry::git_available() {
                return Err("`git` is not installed or not on PATH — cannot reach the registry".to_string());
            }
            type_registry::sync_registry_index()?
        }
    };
    let out_path = out
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| dir.join("status").join("validation.json"));

    let previous = load_previous(&out_path);
    let current = run(&dir);
    let drift = diff(&previous, &current);
    save(&out_path, &current)?;

    Ok((current, drift, out_path))
}

/// The nominative report of registry.md §9 — what was verified, named, with
/// the date it was last checked, never a single green badge — plus a "drift
/// since last run" section when `drift` has anything to say.
pub fn render(snapshots: &[Snapshot], drift: &Drift) -> String {
    let mut out = String::new();
    if snapshots.is_empty() {
        out.push_str("no definitions indexed in the registry — nothing to revalidate.\n");
        return out;
    }
    for s in snapshots {
        out.push_str(&format!(
            "{} — {} (definition v{}) — last verified {}\n",
            s.package, s.repository, s.definition_version, s.checked_at
        ));
        for c in &s.checks {
            out.push_str(&format!("  {:<26} {:<12} {}\n", c.name, c.status, c.detail));
        }
    }
    if !drift.is_empty() {
        out.push_str("\ndrift since last run:\n");
        for (pkg, repo) in &drift.newly_failing {
            out.push_str(&format!("  NEW FAILURE   {pkg} — {repo}\n"));
        }
        for (pkg, repo) in &drift.recovered {
            out.push_str(&format!("  recovered     {pkg} — {repo}\n"));
        }
        for (pkg, repo) in &drift.removed {
            out.push_str(&format!("  removed       {pkg} — {repo}\n"));
        }
    }
    out
}

// ---------------------------------------------------------------------
// Dates — no `chrono` in this workspace (registry.md's own instinct
// elsewhere: shell out or hand-roll rather than add an HTTP/date dependency
// for one call site). Howard Hinnant's `civil_from_days` (public domain) is a
// small, well-known, dependency-free days-since-epoch → (y, m, d) conversion.
// ---------------------------------------------------------------------

fn today() -> String {
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0);
    let (y, m, d) = civil_from_days((secs / 86_400) as i64);
    format!("{y:04}-{m:02}-{d:02}")
}

fn civil_from_days(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = (z - era * 146_097) as u64;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}

#[cfg(test)]
mod tests {
    use super::*;

    // -- civil_from_days --------------------------------------------------

    #[test]
    fn civil_from_days_epoch_is_1970_01_01() {
        assert_eq!(civil_from_days(0), (1970, 1, 1));
    }

    #[test]
    fn civil_from_days_matches_a_known_date() {
        // 2024-03-01 is 19783 days after the epoch (external reference: date -d @1709251200 -u).
        assert_eq!(civil_from_days(19_783), (2024, 3, 1));
    }

    #[test]
    fn civil_from_days_handles_a_leap_day() {
        // 2024-02-29 is one day before the above.
        assert_eq!(civil_from_days(19_782), (2024, 2, 29));
    }

    // -- diff ---------------------------------------------------------------

    fn snap(package: &str, repository: &str, ok: bool) -> Snapshot {
        Snapshot {
            package: package.to_string(),
            repository: repository.to_string(),
            definition_version: "0.1.0".to_string(),
            checked_at: "2026-01-01".to_string(),
            ok,
            checks: Vec::new(),
        }
    }

    #[test]
    fn ok_to_fail_is_newly_failing() {
        let previous = vec![snap("shiny", "github:alice/typr-shiny", true)];
        let current = vec![snap("shiny", "github:alice/typr-shiny", false)];
        let drift = diff(&previous, &current);
        assert_eq!(
            drift.newly_failing,
            vec![("shiny".to_string(), "github:alice/typr-shiny".to_string())]
        );
        assert!(drift.recovered.is_empty());
        assert!(drift.removed.is_empty());
    }

    #[test]
    fn fail_to_ok_is_recovered() {
        let previous = vec![snap("shiny", "github:alice/typr-shiny", false)];
        let current = vec![snap("shiny", "github:alice/typr-shiny", true)];
        let drift = diff(&previous, &current);
        assert_eq!(
            drift.recovered,
            vec![("shiny".to_string(), "github:alice/typr-shiny".to_string())]
        );
        assert!(drift.newly_failing.is_empty());
    }

    #[test]
    fn never_seen_before_and_failing_counts_as_newly_failing() {
        let previous: Vec<Snapshot> = Vec::new();
        let current = vec![snap("shiny", "github:alice/typr-shiny", false)];
        let drift = diff(&previous, &current);
        assert_eq!(
            drift.newly_failing,
            vec![("shiny".to_string(), "github:alice/typr-shiny".to_string())]
        );
    }

    #[test]
    fn never_seen_before_and_ok_is_not_drift() {
        let previous: Vec<Snapshot> = Vec::new();
        let current = vec![snap("shiny", "github:alice/typr-shiny", true)];
        assert!(diff(&previous, &current).is_empty());
    }

    #[test]
    fn still_failing_both_runs_is_not_drift() {
        let previous = vec![snap("shiny", "github:alice/typr-shiny", false)];
        let current = vec![snap("shiny", "github:alice/typr-shiny", false)];
        assert!(diff(&previous, &current).is_empty());
    }

    #[test]
    fn dropped_from_the_registry_is_removed() {
        let previous = vec![snap("shiny", "github:alice/typr-shiny", true)];
        let current: Vec<Snapshot> = Vec::new();
        let drift = diff(&previous, &current);
        assert_eq!(
            drift.removed,
            vec![("shiny".to_string(), "github:alice/typr-shiny".to_string())]
        );
        assert!(drift.newly_failing.is_empty());
    }

    #[test]
    fn same_package_two_repositories_are_independent_keys() {
        let previous = vec![
            snap("shiny", "github:alice/typr-shiny", true),
            snap("shiny", "github:bob/typr-shiny", true),
        ];
        let current = vec![
            snap("shiny", "github:alice/typr-shiny", false),
            snap("shiny", "github:bob/typr-shiny", true),
        ];
        let drift = diff(&previous, &current);
        assert_eq!(
            drift.newly_failing,
            vec![("shiny".to_string(), "github:alice/typr-shiny".to_string())]
        );
    }

    // -- load_previous / save round-trip ------------------------------------

    #[test]
    fn load_previous_missing_file_is_empty_not_an_error() {
        let path = std::env::temp_dir().join(format!("typr_revalidate_missing_{}.json", std::process::id()));
        let _ = fs::remove_file(&path);
        assert!(load_previous(&path).is_empty());
    }

    #[test]
    fn save_then_load_previous_round_trips() {
        let path = std::env::temp_dir().join(format!("typr_revalidate_roundtrip_{}.json", std::process::id()));
        let _ = fs::remove_file(&path);
        let snapshots = vec![snap("shiny", "github:alice/typr-shiny", true)];

        save(&path, &snapshots).unwrap();
        let loaded = load_previous(&path);

        let _ = fs::remove_file(&path);
        assert_eq!(loaded, snapshots);
    }

    // -- render ---------------------------------------------------------------

    #[test]
    fn render_empty_registry_says_so() {
        assert_eq!(
            render(&[], &Drift::default()),
            "no definitions indexed in the registry — nothing to revalidate.\n"
        );
    }

    #[test]
    fn render_includes_drift_section_only_when_non_empty() {
        let clean = render(&[snap("shiny", "github:alice/typr-shiny", true)], &Drift::default());
        assert!(!clean.contains("drift since last run"));

        let mut drift = Drift::default();
        drift
            .newly_failing
            .push(("shiny".to_string(), "github:alice/typr-shiny".to_string()));
        let dirty = render(&[snap("shiny", "github:alice/typr-shiny", false)], &drift);
        assert!(dirty.contains("drift since last run"));
        assert!(dirty.contains("NEW FAILURE   shiny — github:alice/typr-shiny"));
    }
}
