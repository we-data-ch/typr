//! Coherence between `DESCRIPTION` and `uvr.toml` (uvr migration, step 4).
//!
//! `typr use <pkg>` writes a dependency in both files; either can be edited by hand afterwards.
//! [`diagnose`] is pure (file contents in, drifts out, no R, no uvr) so it is unit-tested;
//! [`check_project_dir`] is the thin I/O wrapper `typr check` calls. Drifts are warnings.

use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

/// Packages shipped with R itself: never declared anywhere.
const BASE_PACKAGES: &[&str] = &[
    "R",
    "base",
    "compiler",
    "datasets",
    "grDevices",
    "graphics",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "utils",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Drift {
    /// In `DESCRIPTION`, absent from `uvr.toml`.
    MissingInUvr { package: String, dev: bool },
    /// In `uvr.toml`, absent from `DESCRIPTION`.
    MissingInDescription { package: String, dev: bool },
    /// `library(x)` in the TypR sources, declared nowhere.
    Undeclared { package: String },
}

impl Drift {
    pub fn message(&self) -> String {
        match self {
            Drift::MissingInUvr { package, dev } => format!(
                "`{package}` is in DESCRIPTION but not in uvr.toml [{}]: run `typr use {package}`",
                section(*dev)
            ),
            Drift::MissingInDescription { package, dev } => format!(
                "`{package}` is in uvr.toml [{}] but not in DESCRIPTION: add it to DESCRIPTION or run `uvr remove {package}`",
                section(*dev)
            ),
            Drift::Undeclared { package } => {
                format!("package `{package}` is used with library() but not declared: run `typr use {package}`")
            }
        }
    }
}

fn section(dev: bool) -> &'static str {
    if dev {
        "dev-dependencies"
    } else {
        "dependencies"
    }
}

fn is_base(name: &str) -> bool {
    BASE_PACKAGES.contains(&name)
}

/// `(runtime, suggested)` package names of a DESCRIPTION: `Imports`, `Depends`, `LinkingTo`
/// versus `Suggests`. Version constraints are dropped, base packages ignored.
pub fn description_deps(description: &str) -> (BTreeSet<String>, BTreeSet<String>) {
    let (mut runtime, mut suggested) = (BTreeSet::new(), BTreeSet::new());
    let mut field: Option<String> = None;
    for line in description.lines() {
        if line.starts_with(char::is_whitespace) {
            if let Some(f) = &field {
                add_field_value(f, line, &mut runtime, &mut suggested);
            }
        } else if let Some((name, value)) = line.split_once(':') {
            field = Some(name.trim().to_string());
            add_field_value(name.trim(), value, &mut runtime, &mut suggested);
        } else {
            field = None;
        }
    }
    (runtime, suggested)
}

fn add_field_value(field: &str, value: &str, runtime: &mut BTreeSet<String>, suggested: &mut BTreeSet<String>) {
    let target = match field {
        "Imports" | "Depends" | "LinkingTo" => runtime,
        "Suggests" => suggested,
        _ => return,
    };
    for item in value.split(',') {
        let name = item.split('(').next().unwrap_or("").trim();
        if !name.is_empty() && !is_base(name) {
            target.insert(name.to_string());
        }
    }
}

/// `(dependencies, dev-dependencies)` keys of a `uvr.toml`; `None` if it does not parse.
pub fn uvr_deps(manifest: &str) -> Option<(BTreeSet<String>, BTreeSet<String>)> {
    let value: toml::Value = manifest.parse().ok()?;
    let keys = |section: &str| -> BTreeSet<String> {
        value
            .get(section)
            .and_then(|t| t.as_table())
            .map(|t| t.keys().filter(|k| !is_base(k)).cloned().collect())
            .unwrap_or_default()
    };
    Some((keys("dependencies"), keys("dev-dependencies")))
}

/// Package names used with `library(x)` / `require(x)` in TypR source (comments stripped).
pub fn library_uses(source: &str) -> BTreeSet<String> {
    let mut found = BTreeSet::new();
    for line in source.lines() {
        let code = line.split('#').next().unwrap_or("");
        for call in ["library(", "require("] {
            let mut rest = code;
            while let Some(i) = rest.find(call) {
                let preceded_by_ident = rest[..i]
                    .chars()
                    .next_back()
                    .is_some_and(|c| c.is_alphanumeric() || c == '_' || c == '.');
                rest = &rest[i + call.len()..];
                if preceded_by_ident {
                    continue;
                }
                let name: String = rest
                    .trim_start_matches(['"', '\'', ' '])
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '.' || *c == '_')
                    .collect();
                if !name.is_empty() && !is_base(&name) {
                    found.insert(name);
                }
            }
        }
    }
    found
}

pub fn diagnose(description: &str, manifest: &str, sources: &[String]) -> Vec<Drift> {
    let Some((uvr_run, uvr_dev)) = uvr_deps(manifest) else {
        return Vec::new();
    };
    let (desc_run, desc_sug) = description_deps(description);
    let mut drifts = Vec::new();
    for (desc, uvr, dev) in [(&desc_run, &uvr_run, false), (&desc_sug, &uvr_dev, true)] {
        for p in desc.difference(uvr) {
            // A package already declared in the other uvr section is not a drift worth shouting about.
            if uvr_run.contains(p) || uvr_dev.contains(p) {
                continue;
            }
            drifts.push(Drift::MissingInUvr {
                package: p.clone(),
                dev,
            });
        }
        // [dev-dependencies] is tooling (devtools, usethis…), not necessarily `Suggests`.
        for p in uvr.difference(desc).filter(|_| !dev) {
            if desc_run.contains(p) || desc_sug.contains(p) {
                continue;
            }
            drifts.push(Drift::MissingInDescription {
                package: p.clone(),
                dev,
            });
        }
    }
    let declared: BTreeSet<&String> = desc_run
        .iter()
        .chain(&desc_sug)
        .chain(&uvr_run)
        .chain(&uvr_dev)
        .collect();
    let used: BTreeSet<String> = sources.iter().flat_map(|s| library_uses(s)).collect();
    for p in used {
        if !declared.contains(&p) {
            drifts.push(Drift::Undeclared { package: p });
        }
    }
    drifts
}

fn collect_ty(dir: &Path, out: &mut Vec<String>) {
    let Ok(entries) = fs::read_dir(dir) else { return };
    for e in entries.flatten() {
        let p = e.path();
        if p.is_dir() {
            collect_ty(&p, out);
        } else if p.extension().is_some_and(|x| x == "ty") {
            if let Ok(s) = fs::read_to_string(&p) {
                out.push(s);
            }
        }
    }
}

/// Warnings for the project rooted at `root`; empty when it has no `uvr.toml`.
pub fn check_project_dir(root: &Path) -> Vec<Drift> {
    let Ok(manifest) = fs::read_to_string(root.join("uvr.toml")) else {
        return Vec::new();
    };
    let description = fs::read_to_string(root.join("DESCRIPTION")).unwrap_or_default();
    let mut sources = Vec::new();
    collect_ty(&root.join("TypR"), &mut sources);
    diagnose(&description, &manifest, &sources)
}

/// Print the drifts as warnings on stderr.
pub fn warn(root: &Path) {
    for d in check_project_dir(root) {
        eprintln!("warning: {}", d.message());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DESC: &str =
        "Package: demo\nImports:\n    cli (>= 3.0),\n    stats,\n    dplyr\nSuggests:\n    testthat (>= 3.0.0)\n";

    #[test]
    fn parses_description_fields_across_lines() {
        let (run, sug) = description_deps(DESC);
        assert_eq!(run.into_iter().collect::<Vec<_>>(), ["cli", "dplyr"]);
        assert_eq!(sug.into_iter().collect::<Vec<_>>(), ["testthat"]);
    }

    #[test]
    fn dev_tooling_only_in_uvr_is_not_a_drift() {
        let uvr =
            "[dependencies]\ncli = \"*\"\ndplyr = \"*\"\n[dev-dependencies]\ntestthat = \"*\"\ndevtools = \"*\"\n";
        let d = diagnose(DESC, uvr, &[]);
        assert!(
            d.is_empty(),
            "dev tooling absent from DESCRIPTION is not a drift: {d:?}"
        );
    }

    #[test]
    fn both_directions_are_reported() {
        let uvr = "[dependencies]\ncli = \"*\"\njsonlite = \"*\"\n[dev-dependencies]\ntestthat = \"*\"\n";
        let d = diagnose(DESC, uvr, &[]);
        assert!(d.contains(&Drift::MissingInUvr {
            package: "dplyr".into(),
            dev: false
        }));
        assert!(d.contains(&Drift::MissingInDescription {
            package: "jsonlite".into(),
            dev: false
        }));
    }

    #[test]
    fn library_without_declaration_is_reported() {
        let uvr = "[dependencies]\ncli = \"*\"\ndplyr = \"*\"\n";
        let src = "library(cli)\nlibrary(\"ggplot2\") # library(commented)\nx <- 1;".to_string();
        let d = diagnose(DESC, uvr, &[src]);
        assert!(d.contains(&Drift::Undeclared {
            package: "ggplot2".into()
        }));
        assert!(!d
            .iter()
            .any(|x| matches!(x, Drift::Undeclared { package } if package == "cli" || package == "commented")));
    }

    #[test]
    fn unparsable_manifest_is_ignored() {
        assert!(diagnose(DESC, "not = [toml", &[]).is_empty());
    }

    #[test]
    fn no_uvr_toml_means_no_check() {
        let dir = std::env::temp_dir().join(format!("typr_manifest_{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        assert!(check_project_dir(&dir).is_empty());
        let _ = fs::remove_dir_all(&dir);
    }
}
