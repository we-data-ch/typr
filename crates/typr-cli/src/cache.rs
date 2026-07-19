//! Incremental-build cache for `typr build` (project mode).
//!
//! A `BuildManifest` is persisted as `.typr_cache/manifest.json` at the
//! project root after every successful build. On the next build it is used
//! to:
//!
//! - short-circuit the whole pipeline when no source or output file changed
//!   ("Project up to date");
//! - skip the `devtools::document` R subprocess when its inputs
//!   (`R/*.R` + `DESCRIPTION`) are identical to the last successful run.
//!
//! Every read path is fail-open: a missing, unreadable, or unparsable
//! manifest simply means "no cache" and the full build runs.

use crate::metaprogramming::ExpansionInfo;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use typr_core::processes::type_checking::module_cache::ModuleCacheEntry;
use typr_core::processes::type_checking::module_cache::ModuleCacheStore;

/// Bump when the manifest layout or the meaning of a hash changes.
pub const CACHE_FORMAT_VERSION: u32 = 1;
pub const CACHE_DIR: &str = ".typr_cache";

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BuildManifest {
    pub cache_format: u32,
    pub typr_version: String,
    pub test_mode: bool,
    /// Mirrors `test_mode`: a manifest written with a different `--checked`
    /// state must never be reused (checked and non-checked R differ).
    #[serde(default)]
    pub checked_mode: bool,
    /// Relative path → content hash of every `.ty` file reached by module
    /// expansion (entry file included).
    pub source_hashes: BTreeMap<String, u64>,
    /// Relative path → content hash of every generated file
    /// (`R/*.R`, `load_module.R`, `man/*.Rd`).
    pub output_hashes: BTreeMap<String, u64>,
    /// Hash over (sorted `R/*.R` + `DESCRIPTION`) at the last successful
    /// `devtools::document` run. `None` until devtools has succeeded once.
    pub devtools_input_hash: Option<u64>,
}

impl BuildManifest {
    pub fn new(test_mode: bool, checked_mode: bool) -> Self {
        BuildManifest {
            cache_format: CACHE_FORMAT_VERSION,
            typr_version: env!("CARGO_PKG_VERSION").to_string(),
            test_mode,
            checked_mode,
            ..Default::default()
        }
    }

    /// A manifest written by another typr version, cache format, or build
    /// mode must be ignored entirely.
    pub fn is_compatible(&self, test_mode: bool, checked_mode: bool) -> bool {
        self.cache_format == CACHE_FORMAT_VERSION
            && self.typr_version == env!("CARGO_PKG_VERSION")
            && self.test_mode == test_mode
            && self.checked_mode == checked_mode
    }

    /// True when every recorded source and output file still exists on disk
    /// with the same content hash. Empty hash sets are never "up to date"
    /// (defensive: a half-written manifest must not skip a build).
    pub fn is_up_to_date(&self, root: &Path) -> bool {
        if self.source_hashes.is_empty() || self.output_hashes.is_empty() {
            return false;
        }
        self.source_hashes
            .iter()
            .chain(self.output_hashes.iter())
            .all(|(path, expected)| hash_file(&root.join(path)) == Some(*expected))
    }
}

pub fn hash_str(content: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}

/// Content hash of a file; `None` if unreadable.
pub fn hash_file(path: &Path) -> Option<u64> {
    fs::read_to_string(path).ok().map(|c| hash_str(&c))
}

/// Write `content` to `path` only if it differs from what is already on
/// disk, keeping mtimes stable for unchanged files. Returns whether the
/// file was (re)written.
pub fn write_if_changed(path: &Path, content: &str) -> io::Result<bool> {
    if let Ok(existing) = fs::read_to_string(path) {
        if existing == content {
            return Ok(false);
        }
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, content)?;
    Ok(true)
}

pub fn manifest_path(root: &Path) -> PathBuf {
    root.join(CACHE_DIR).join("manifest.json")
}

/// Fail-open: any read/parse error yields `None` (treated as a cold cache).
pub fn load_manifest(root: &Path) -> Option<BuildManifest> {
    let content = fs::read_to_string(manifest_path(root)).ok()?;
    serde_json::from_str(&content).ok()
}

/// Best-effort: a failed write only costs the next build its cache.
pub fn save_manifest(root: &Path, manifest: &BuildManifest) {
    let path = manifest_path(root);
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    if let Ok(json) = serde_json::to_string_pretty(manifest) {
        let _ = fs::write(path, json);
    }
}

/// Hash over the inputs that determine `devtools::document`'s
/// NAMESPACE/Collate output: every `R/*.R` (sorted by name) + `DESCRIPTION`.
/// `None` when the R directory is unreadable.
pub fn devtools_input_hash(root: &Path) -> Option<u64> {
    let mut entries: Vec<(String, String)> = Vec::new();
    for dir_entry in fs::read_dir(root.join("R")).ok()?.flatten() {
        let path = dir_entry.path();
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        if !name.ends_with(".R") {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        entries.push((name.to_string(), content));
    }
    entries.sort();
    let description = fs::read_to_string(root.join("DESCRIPTION")).unwrap_or_default();
    let combined = entries
        .iter()
        .map(|(name, content)| format!("{}\u{0}{}\u{0}", name, content))
        .collect::<String>()
        + &description;
    Some(hash_str(&combined))
}

/// Hash every generated file currently on disk: `R/*.R`, `load_module.R`,
/// `man/*.Rd`. Called after a successful build to fill
/// `BuildManifest::output_hashes`.
pub fn collect_output_hashes(root: &Path) -> BTreeMap<String, u64> {
    let mut hashes = BTreeMap::new();
    for (dir, extension) in [("R", ".R"), ("man", ".Rd")] {
        let Ok(read_dir) = fs::read_dir(root.join(dir)) else {
            continue;
        };
        for dir_entry in read_dir.flatten() {
            let path = dir_entry.path();
            let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
                continue;
            };
            if !name.ends_with(extension) {
                continue;
            }
            if let Some(hash) = hash_file(&path) {
                hashes.insert(format!("{}/{}", dir, name), hash);
            }
        }
    }
    if let Some(hash) = hash_file(&root.join("load_module.R")) {
        hashes.insert("load_module.R".to_string(), hash);
    }
    hashes
}

/// Salt folded into every per-module cache key: a different typr version,
/// cache format or build mode must never share entries.
pub fn module_cache_salt(test_mode: bool, checked_mode: bool) -> u64 {
    hash_str(&format!(
        "module-cache|{}|{}|{}|{}",
        CACHE_FORMAT_VERSION,
        env!("CARGO_PKG_VERSION"),
        test_mode,
        checked_mode
    ))
}

/// Combined source hash per module: its own `.ty` content hash plus the
/// hashes of every module it (transitively) imports, so a dependency edit
/// invalidates the whole import chain even when the importer's own file is
/// untouched.
pub fn combined_module_hashes(info: &ExpansionInfo) -> HashMap<String, u64> {
    fn closure(name: &str, deps: &BTreeMap<String, Vec<String>>, acc: &mut BTreeSet<String>) {
        if !acc.insert(name.to_string()) {
            return;
        }
        for dep in deps.get(name).map(|v| v.as_slice()).unwrap_or(&[]) {
            closure(dep, deps, acc);
        }
    }
    info.module_hashes
        .keys()
        .map(|name| {
            let mut reached = BTreeSet::new();
            closure(name, &info.deps, &mut reached);
            let combined = reached
                .iter()
                .filter_map(|m| info.module_hashes.get(m).map(|h| format!("{}:{}", m, h)))
                .collect::<Vec<_>>()
                .join("|");
            (name.clone(), hash_str(&combined))
        })
        .collect()
}

/// Filesystem-backed [`ModuleCacheStore`] under `.typr_cache/modules/`.
/// Fail-open on every path: unreadable or undecodable entries are misses.
/// Keys touched during the build (both hits and fresh puts) are tracked so
/// [`FsModuleStore::gc`] can prune entries the build no longer reaches.
#[derive(Clone)]
pub struct FsModuleStore {
    dir: PathBuf,
    used: Rc<RefCell<HashSet<u64>>>,
}

impl FsModuleStore {
    pub fn new(dir: PathBuf) -> Self {
        FsModuleStore {
            dir,
            used: Rc::new(RefCell::new(HashSet::new())),
        }
    }

    fn entry_path(&self, key: u64) -> PathBuf {
        self.dir.join(format!("{:016x}.bin", key))
    }

    /// Remove every stored entry whose key was neither read nor written by
    /// the build that just finished.
    pub fn gc(&self) {
        let used = self.used.borrow();
        let Ok(read_dir) = fs::read_dir(&self.dir) else {
            return;
        };
        for dir_entry in read_dir.flatten() {
            let path = dir_entry.path();
            let stale = path
                .file_stem()
                .and_then(|s| s.to_str())
                .and_then(|s| u64::from_str_radix(s, 16).ok())
                .is_none_or(|key| !used.contains(&key));
            if stale {
                let _ = fs::remove_file(&path);
            }
        }
    }
}

impl ModuleCacheStore for FsModuleStore {
    fn get(&self, key: u64) -> Option<ModuleCacheEntry> {
        let bytes = fs::read(self.entry_path(key)).ok()?;
        let entry = bincode::deserialize(&bytes).ok()?;
        self.used.borrow_mut().insert(key);
        Some(entry)
    }

    fn put(&self, key: u64, entry: &ModuleCacheEntry) {
        let _ = fs::create_dir_all(&self.dir);
        if let Ok(bytes) = bincode::serialize(entry) {
            let _ = fs::write(self.entry_path(key), bytes);
            self.used.borrow_mut().insert(key);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_if_changed_skips_identical_content() {
        let dir = std::env::temp_dir().join(format!("typr_wic_test_{}", std::process::id()));
        let _ = fs::create_dir_all(&dir);
        let path = dir.join("out.R");

        assert!(write_if_changed(&path, "x <- 1\n").unwrap());
        assert!(!write_if_changed(&path, "x <- 1\n").unwrap());
        assert!(write_if_changed(&path, "x <- 2\n").unwrap());
        assert_eq!(fs::read_to_string(&path).unwrap(), "x <- 2\n");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn manifest_round_trips_and_checks_compatibility() {
        let dir = std::env::temp_dir().join(format!("typr_manifest_test_{}", std::process::id()));
        let _ = fs::create_dir_all(&dir);

        let mut manifest = BuildManifest::new(false, false);
        manifest.source_hashes.insert("TypR/main.ty".into(), 42);
        save_manifest(&dir, &manifest);

        let loaded = load_manifest(&dir).expect("manifest should load");
        assert!(loaded.is_compatible(false, false));
        assert!(!loaded.is_compatible(true, false)); // test_mode mismatch
        assert!(!loaded.is_compatible(false, true)); // checked_mode mismatch
        assert_eq!(loaded.source_hashes.get("TypR/main.ty"), Some(&42));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn up_to_date_requires_matching_hashes() {
        let dir = std::env::temp_dir().join(format!("typr_uptodate_test_{}", std::process::id()));
        let _ = fs::create_dir_all(dir.join("TypR"));
        let _ = fs::create_dir_all(dir.join("R"));
        fs::write(dir.join("TypR/main.ty"), "let x <- 1;\n").unwrap();
        fs::write(dir.join("R/main.R"), "x <- 1\n").unwrap();

        let mut manifest = BuildManifest::new(false, false);
        assert!(!manifest.is_up_to_date(&dir)); // empty hash sets

        manifest
            .source_hashes
            .insert("TypR/main.ty".into(), hash_file(&dir.join("TypR/main.ty")).unwrap());
        manifest
            .output_hashes
            .insert("R/main.R".into(), hash_file(&dir.join("R/main.R")).unwrap());
        assert!(manifest.is_up_to_date(&dir));

        fs::write(dir.join("TypR/main.ty"), "let x <- 2;\n").unwrap();
        assert!(!manifest.is_up_to_date(&dir));

        let _ = fs::remove_dir_all(&dir);
    }
}
