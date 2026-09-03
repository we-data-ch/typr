//! Startup check for the R packages TypR needs (`devtools`, `testthat`).
//!
//! Every `typr` invocation (except `lsp`, `init` and `std`) runs
//! [`warn_if_missing`], which prints a one-line hint pointing at `typr init`
//! when a required package is absent. Since this sits on the hot path of
//! *every* command, it must never cost an R subprocess in the common case:
//!
//! - the check is a pure `stat` of `<lib>/<pkg>/DESCRIPTION` over a list of
//!   candidate library directories (project `renv/library/**`, `R_LIBS_USER`
//!   / `R_LIBS` / `R_LIBS_SITE`, plus the paths cached from a previous run);
//! - `Rscript -e '.libPaths()'` is spawned **only** when that stat scan comes
//!   up short *and* the cached library list is missing or older than
//!   [`CACHE_TTL_SECS`] — i.e. essentially once per machine, then again at
//!   most once a day while a package really is missing;
//! - the result is cached in `<cache_home>/typr/r_deps.json`, which is
//!   self-invalidating: an uninstalled package stops satisfying the stat, and
//!   a newly installed one starts satisfying it immediately (no TTL wait),
//!   because it lands in one of the very directories being scanned.
//!
//! Every read path is fail-open: an unreadable/unparsable/incompatible cache
//! just means "no cache", and an environment where R cannot be probed at all
//! stays silent rather than nagging with a guess.

use serde::Deserialize;
use serde::Serialize;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::time::SystemTime;
use std::time::UNIX_EPOCH;

const YELLOW: &str = "\x1b[33m";
const GREEN: &str = "\x1b[32m";
const RED: &str = "\x1b[31m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

/// R packages TypR's own toolchain calls into (`devtools::document`,
/// `devtools::test` → `testthat`).
pub const REQUIRED_PACKAGES: [&str; 2] = ["devtools", "testthat"];

/// Bump when the cache layout changes.
pub const CACHE_FORMAT_VERSION: u32 = 1;

/// How long a probed `.libPaths()` list (or a "Rscript is not reachable"
/// verdict) is trusted before another subprocess is allowed.
pub const CACHE_TTL_SECS: u64 = 24 * 60 * 60;

/// Cached result of the last real probe. Only the *library paths* need the
/// TTL — package presence itself is re-derived by `stat` on every run.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DepsCache {
    pub format: u32,
    pub typr_version: String,
    /// Unix timestamp of the last `Rscript` probe.
    pub probed_at: u64,
    /// `.libPaths()` as reported by R at that probe.
    pub lib_paths: Vec<String>,
    /// `false` when `Rscript` could not be executed at all.
    pub r_available: bool,
}

impl DepsCache {
    fn is_compatible(&self) -> bool {
        self.format == CACHE_FORMAT_VERSION && self.typr_version == env!("CARGO_PKG_VERSION")
    }

    fn is_fresh(&self, now: u64) -> bool {
        now.saturating_sub(self.probed_at) < CACHE_TTL_SECS
    }
}

/// What the check concluded, without any I/O of its own.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DepsStatus {
    /// Required packages not found in any candidate library directory.
    pub missing: Vec<String>,
    /// `false` when R itself could not be reached (nothing can be concluded
    /// about the packages, so callers stay quiet about them).
    pub r_available: bool,
}

fn now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

fn cache_home() -> Option<PathBuf> {
    for var in ["XDG_CACHE_HOME", "LOCALAPPDATA"] {
        if let Ok(dir) = std::env::var(var) {
            if !dir.is_empty() {
                return Some(PathBuf::from(dir));
            }
        }
    }
    for var in ["HOME", "USERPROFILE"] {
        if let Ok(dir) = std::env::var(var) {
            if !dir.is_empty() {
                return Some(PathBuf::from(dir).join(".cache"));
            }
        }
    }
    None
}

pub fn cache_path() -> Option<PathBuf> {
    cache_home().map(|dir| dir.join("typr").join("r_deps.json"))
}

fn read_cache() -> Option<DepsCache> {
    let path = cache_path()?;
    let content = fs::read_to_string(path).ok()?;
    let cache: DepsCache = serde_json::from_str(&content).ok()?;
    cache.is_compatible().then_some(cache)
}

fn write_cache(cache: &DepsCache) {
    let Some(path) = cache_path() else { return };
    if let Some(parent) = path.parent() {
        if fs::create_dir_all(parent).is_err() {
            return;
        }
    }
    if let Ok(content) = serde_json::to_string_pretty(cache) {
        let _ = fs::write(path, content);
    }
}

/// Drop the cache so the next check re-probes R from scratch. Called after
/// `typr init` installed something.
pub fn invalidate_cache() {
    if let Some(path) = cache_path() {
        let _ = fs::remove_file(path);
    }
}

/// Library directories to `stat`, cheapest/most specific first: the project's
/// own renv library, then the R library env vars, then whatever R reported at
/// the last probe.
fn candidate_lib_dirs(cached: Option<&DepsCache>) -> Vec<PathBuf> {
    let mut dirs: Vec<PathBuf> = Vec::new();

    if let Ok(cwd) = std::env::current_dir() {
        dirs.extend(renv_lib_dirs(&cwd));
    }

    for var in ["R_LIBS_USER", "R_LIBS", "R_LIBS_SITE"] {
        if let Ok(value) = std::env::var(var) {
            for entry in value.split([':', ';']) {
                let entry = entry.trim();
                // `%V`-style placeholders are only expanded by R itself.
                if !entry.is_empty() && !entry.contains('%') {
                    dirs.push(PathBuf::from(entry));
                }
            }
        }
    }

    if let Some(cache) = cached {
        dirs.extend(cache.lib_paths.iter().map(PathBuf::from));
    }

    dirs.sort();
    dirs.dedup();
    dirs
}

/// renv nests its library under a platform/R-version pair whose exact order
/// changed across renv releases, so walk `renv/library` a few levels deep
/// instead of hardcoding a layout.
fn renv_lib_dirs(project: &Path) -> Vec<PathBuf> {
    let root = project.join("renv").join("library");
    if !root.is_dir() {
        return Vec::new();
    }
    let mut found = vec![root.clone()];
    let mut frontier = vec![root];
    for _ in 0..3 {
        let mut next = Vec::new();
        for dir in &frontier {
            let Ok(entries) = fs::read_dir(dir) else { continue };
            for entry in entries.flatten() {
                if entry.file_type().map(|t| t.is_dir()).unwrap_or(false) {
                    next.push(entry.path());
                }
            }
        }
        found.extend(next.iter().cloned());
        frontier = next;
    }
    found
}

/// An installed R package is a directory holding a `DESCRIPTION` file — the
/// same thing `.libPaths()` lookups key on.
pub fn is_installed_in(lib_dirs: &[PathBuf], package: &str) -> bool {
    lib_dirs
        .iter()
        .any(|dir| dir.join(package).join("DESCRIPTION").is_file())
}

fn scan(lib_dirs: &[PathBuf]) -> Vec<String> {
    REQUIRED_PACKAGES
        .iter()
        .filter(|pkg| !is_installed_in(lib_dirs, pkg))
        .map(|pkg| pkg.to_string())
        .collect()
}

/// Ask R for its real `.libPaths()`. The one expensive path, guarded by the
/// TTL above. `None` means R could not be reached.
fn probe_lib_paths() -> Option<Vec<String>> {
    let output = Command::new("Rscript")
        .arg("-e")
        .arg("cat(.libPaths(), sep = '\\n')")
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    Some(
        String::from_utf8_lossy(&output.stdout)
            .lines()
            .map(|line| line.trim().to_string())
            .filter(|line| !line.is_empty())
            .collect(),
    )
}

/// Full check: stat-only when possible, one `Rscript` probe at most.
pub fn check() -> DepsStatus {
    let cached = read_cache();
    let dirs = candidate_lib_dirs(cached.as_ref());
    let missing = scan(&dirs);

    // Fast path: everything found by `stat`, no subprocess, no cache write.
    if missing.is_empty() {
        return DepsStatus {
            missing,
            r_available: true,
        };
    }

    // Something looks missing. Trust a fresh cache rather than re-probing on
    // every invocation; only a stale (or absent) one earns a subprocess.
    let now = now_secs();
    if let Some(cache) = cached.as_ref() {
        if cache.is_fresh(now) {
            return DepsStatus {
                missing,
                r_available: cache.r_available,
            };
        }
    }

    match probe_lib_paths() {
        Some(lib_paths) => {
            let mut dirs = candidate_lib_dirs(cached.as_ref());
            dirs.extend(lib_paths.iter().map(PathBuf::from));
            dirs.sort();
            dirs.dedup();
            let missing = scan(&dirs);
            write_cache(&DepsCache {
                format: CACHE_FORMAT_VERSION,
                typr_version: env!("CARGO_PKG_VERSION").to_string(),
                probed_at: now,
                lib_paths,
                r_available: true,
            });
            DepsStatus {
                missing,
                r_available: true,
            }
        }
        None => {
            write_cache(&DepsCache {
                format: CACHE_FORMAT_VERSION,
                typr_version: env!("CARGO_PKG_VERSION").to_string(),
                probed_at: now,
                lib_paths: cached.map(|c| c.lib_paths).unwrap_or_default(),
                r_available: false,
            });
            DepsStatus {
                missing,
                r_available: false,
            }
        }
    }
}

/// Set to any non-empty value (other than `0`) to silence the startup hint.
/// The internal harnesses (`typr case`, `typr fuzz`) set it on the `typr`
/// subprocesses they capture, so the hint never lands in an `observed.txt` or
/// in an `@run` oracle's captured output.
pub const SKIP_ENV_VAR: &str = "TYPR_SKIP_R_DEPS_CHECK";

pub fn check_is_disabled() -> bool {
    matches!(std::env::var(SKIP_ENV_VAR), Ok(value) if !value.is_empty() && value != "0")
}

/// Startup hint. Prints to **stderr** so it never pollutes generated output,
/// and stays silent when everything is in place.
pub fn warn_if_missing() {
    if check_is_disabled() {
        return;
    }
    let status = check();
    if !status.r_available {
        eprintln!(
            "{YELLOW}warning{RESET}: R was not found on this machine (`Rscript` is not on PATH).\n         \
             Install R first, then run {BOLD}typr init{RESET} to add the R packages TypR needs."
        );
        return;
    }
    if status.missing.is_empty() {
        return;
    }
    eprintln!(
        "{YELLOW}warning{RESET}: missing R package(s): {BOLD}{}{RESET}\n         \
         Run {BOLD}typr init{RESET} to install them.",
        status.missing.join(", ")
    );
}

/// `typr init` — install the required R packages (renv-aware, like
/// `typr pkg install`), then refresh the cache.
pub fn init() {
    let status = check();

    if !status.r_available {
        eprintln!("{RED}error{RESET}: `Rscript` is not on PATH — install R before running `typr init`.");
        std::process::exit(1);
    }

    if status.missing.is_empty() {
        println!(
            "{GREEN}✓{RESET} All required R packages are already installed ({}).",
            REQUIRED_PACKAGES.join(", ")
        );
        return;
    }

    let renv = std::env::current_dir()
        .map(|d| d.join("renv.lock").exists())
        .unwrap_or(false);

    let pkgs = status
        .missing
        .iter()
        .map(|p| format!("'{}'", p))
        .collect::<Vec<_>>()
        .join(", ");
    let r_command = if renv {
        println!(
            "Installing missing R package(s) via renv: {}",
            status.missing.join(", ")
        );
        format!("renv::install(c({}))", pkgs)
    } else {
        println!(
            "Installing missing R package(s) from CRAN: {}",
            status.missing.join(", ")
        );
        format!("install.packages(c({}), repos = 'https://cloud.r-project.org')", pkgs)
    };
    println!("Executing: R -e \"{}\"", r_command);

    match Command::new("R").arg("-e").arg(&r_command).status() {
        Ok(exit) if exit.success() => {
            invalidate_cache();
            let after = check();
            if after.missing.is_empty() {
                println!("{GREEN}✓{RESET} R dependencies installed.");
            } else {
                eprintln!(
                    "{RED}error{RESET}: still missing after installation: {}",
                    after.missing.join(", ")
                );
                std::process::exit(1);
            }
        }
        Ok(_) => {
            invalidate_cache();
            eprintln!("{RED}error{RESET}: R package installation failed.");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("{RED}error{RESET}: could not execute R: {}", e);
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp_dir(tag: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let dir = std::env::temp_dir().join(format!("typr-rdeps-{}-{}-{}", tag, std::process::id(), nanos));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn fake_package(lib: &Path, name: &str) {
        let pkg = lib.join(name);
        fs::create_dir_all(&pkg).unwrap();
        fs::write(pkg.join("DESCRIPTION"), format!("Package: {}\n", name)).unwrap();
    }

    #[test]
    fn detects_an_installed_package_by_stat() {
        let lib = tmp_dir("installed");
        fake_package(&lib, "devtools");

        assert!(is_installed_in(std::slice::from_ref(&lib), "devtools"));
        assert!(!is_installed_in(std::slice::from_ref(&lib), "testthat"));

        fs::remove_dir_all(&lib).unwrap();
    }

    #[test]
    fn a_directory_without_description_is_not_a_package() {
        let lib = tmp_dir("no-description");
        fs::create_dir_all(lib.join("testthat")).unwrap();

        assert!(!is_installed_in(std::slice::from_ref(&lib), "testthat"));

        fs::remove_dir_all(&lib).unwrap();
    }

    #[test]
    fn scan_reports_only_the_absent_packages() {
        let lib = tmp_dir("scan");
        fake_package(&lib, "devtools");

        assert_eq!(scan(std::slice::from_ref(&lib)), vec!["testthat".to_string()]);

        fake_package(&lib, "testthat");
        assert!(scan(std::slice::from_ref(&lib)).is_empty());

        fs::remove_dir_all(&lib).unwrap();
    }

    #[test]
    fn renv_lib_dirs_walks_the_nested_layout() {
        let project = tmp_dir("renv");
        let lib = project.join("renv/library/R-4.3/x86_64-pc-linux-gnu");
        fs::create_dir_all(&lib).unwrap();
        fake_package(&lib, "testthat");

        let dirs = renv_lib_dirs(&project);
        assert!(dirs.contains(&lib), "expected {:?} in {:?}", lib, dirs);
        assert!(is_installed_in(&dirs, "testthat"));

        fs::remove_dir_all(&project).unwrap();
    }

    #[test]
    fn renv_lib_dirs_is_empty_without_a_renv_library() {
        let project = tmp_dir("no-renv");
        assert!(renv_lib_dirs(&project).is_empty());
        fs::remove_dir_all(&project).unwrap();
    }

    #[test]
    fn a_cache_from_another_typr_version_is_ignored() {
        let stale = DepsCache {
            format: CACHE_FORMAT_VERSION,
            typr_version: "0.0.0-not-this-one".to_string(),
            probed_at: now_secs(),
            lib_paths: vec!["/nowhere".to_string()],
            r_available: true,
        };
        assert!(!stale.is_compatible());

        let wrong_format = DepsCache {
            format: CACHE_FORMAT_VERSION + 1,
            typr_version: env!("CARGO_PKG_VERSION").to_string(),
            ..stale.clone()
        };
        assert!(!wrong_format.is_compatible());
    }

    #[test]
    fn cache_freshness_follows_the_ttl() {
        let now = now_secs();
        let cache = DepsCache {
            format: CACHE_FORMAT_VERSION,
            typr_version: env!("CARGO_PKG_VERSION").to_string(),
            probed_at: now,
            lib_paths: vec![],
            r_available: true,
        };
        assert!(cache.is_fresh(now));
        assert!(cache.is_fresh(now + CACHE_TTL_SECS - 1));
        assert!(!cache.is_fresh(now + CACHE_TTL_SECS));
    }
}
