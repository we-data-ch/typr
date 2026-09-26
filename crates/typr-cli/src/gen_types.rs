//! `typr gen-types <pkg>` — J1 of the type registry proposal (see
//! `typR/registry.md` §6, "Génération automatique — le chemin principal").
//!
//! Nobody will hand-write signatures for a 400-function package like `shiny`.
//! This command introspects an *installed* R package (arity, argument names,
//! presence of `...`, version) and emits a `.ty` file where every entry is
//! `#! tier: T3`: every parameter and the return type are `Any`, so the
//! generated definition cannot make the type-checker reject correct code
//! (registry.md D2) — it is immediately useful to the LSP and MCP (argument
//! names, doc scaffolding) while being a no-op for soundness. Promotion to
//! T2/T1 is a manual, per-entry follow-up (registry.md §6, "Promotion").
//!
//! Reuses `r_name_cache`'s `introspect_pkg.R`: that script's `F`/`P` lines
//! (formals, package version) exist for this command; its `N`/`V`/`C`/`E`
//! lines are what the R-name cache itself consumes.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::r_name_cache::INTROSPECT_R;

/// One exported function, as recovered from `formals()`.
pub struct GeneratedFn {
    pub name: String,
    pub has_dots: bool,
    pub params: Vec<String>,
}

pub struct Introspection {
    pub r_version: String,
    pub pkg_version: Option<String>,
    pub functions: Vec<GeneratedFn>,
    /// Human-readable notes about packages that failed to load.
    pub errors: Vec<String>,
}

/// Whether a name can appear in `@importFrom pkg <name>;` and as a plain
/// (optionally backtick-quoted) `@name: ...;` signature — i.e. it is not an
/// operator or other non-identifier export (`%>%`, `[.foo`, `+.money`, ...),
/// which `gen-types` has no safe syntax to emit and simply skips.
fn is_generatable_name(name: &str) -> bool {
    !name.is_empty()
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '.' || c == '_')
        && name.chars().any(|c| c.is_ascii_alphabetic())
}

pub fn rscript_available() -> bool {
    Command::new("Rscript")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Spawn `Rscript introspect_pkg.R <pkg>` and return its stdout, exactly like
/// `r_name_cache::run_introspection` but for a single package at a time.
fn run_introspection(pkg: &str) -> Result<String, String> {
    let script = std::env::temp_dir().join(format!("typr_gen_types_{}.R", std::process::id()));
    fs::write(&script, INTROSPECT_R).map_err(|e| format!("cannot write introspection script: {e}"))?;
    let result = Command::new("Rscript").arg(&script).arg(pkg).output();
    let _ = fs::remove_file(&script);
    let output = result.map_err(|e| format!("Rscript could not be run ({e})"))?;
    if !output.status.success() {
        return Err(format!(
            "Rscript exited with {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

/// Introspect `pkg` (must be installed locally) and collect its exported
/// functions' arity and parameter names.
pub fn introspect(pkg: &str) -> Result<Introspection, String> {
    let raw = run_introspection(pkg)?;

    let mut r_version = String::new();
    let mut pkg_version = None;
    let mut functions = Vec::new();
    let mut errors = Vec::new();

    for line in raw.lines() {
        let mut fields = line.split('\t');
        match fields.next() {
            Some("V") => r_version = fields.next().unwrap_or_default().to_string(),
            Some("P") => {
                let _pkg = fields.next();
                pkg_version = fields.next().map(str::to_string);
            }
            Some("F") => {
                let (Some(name), Some(has_dots)) = (fields.next(), fields.next()) else {
                    continue;
                };
                if !is_generatable_name(name) {
                    continue;
                }
                functions.push(GeneratedFn {
                    name: name.to_string(),
                    has_dots: has_dots == "1",
                    params: fields.map(str::to_string).collect(),
                });
            }
            Some("E") => {
                if let Some(pkg) = fields.next() {
                    let reason = fields.next().unwrap_or("unknown reason");
                    errors.push(format!("{pkg}: {reason}"));
                }
            }
            _ => {}
        }
    }

    Ok(Introspection {
        r_version,
        pkg_version,
        functions,
        errors,
    })
}

/// Render an `@name: (...) -> Any;` signature name, backtick-quoting it when
/// it contains a `.` — the convention already used throughout
/// `configs/std/*.ty` for names like `` `is.numeric` `` (a bare `.` inside a
/// TypR identifier would otherwise parse as field access).
fn signature_name(name: &str) -> String {
    if name.contains('.') {
        format!("`{name}`")
    } else {
        name.to_string()
    }
}

/// Emit the `.ty` source for every generatable function of `pkg`, all at
/// `#! tier: T3` (registry.md §5.4 / §6).
pub fn emit_ty(pkg: &str, info: &Introspection) -> String {
    let mut out = String::new();
    let version = info.pkg_version.as_deref().unwrap_or("unknown");
    let generated_from = format!("{pkg} {version}, R {}", info.r_version);

    if info.functions.is_empty() {
        return out;
    }

    let names: Vec<&str> = info.functions.iter().map(|f| f.name.as_str()).collect();
    out.push_str(&format!("@importFrom {pkg} {};\n\n", names.join(" ")));

    for f in &info.functions {
        out.push_str("#! pkg: ");
        out.push_str(pkg);
        out.push('\n');
        out.push_str("#! tier: T3\n");
        out.push_str(&format!("#! generated-from: {generated_from}\n"));
        for p in &f.params {
            out.push_str(&format!("#! param {p}:\n"));
        }
        if f.has_dots {
            out.push_str("#! param ...:\n");
        }

        let mut arg_types: Vec<&str> = vec!["Any"; f.params.len()];
        if f.has_dots {
            arg_types.push("...Any");
        }
        out.push_str(&format!(
            "@{}: ({}) -> Any;\n\n",
            signature_name(&f.name),
            arg_types.join(", ")
        ));
    }

    out
}

/// `typr gen-types <pkg> [--out DIR]` — see registry.md §6.
pub fn run(pkg: &str, out_dir: Option<PathBuf>) {
    if !rscript_available() {
        eprintln!("error: `Rscript` is not on PATH — `typr gen-types` needs R to introspect `{pkg}`.");
        std::process::exit(1);
    }

    let info = match introspect(pkg) {
        Ok(info) => info,
        Err(e) => {
            eprintln!("error: could not introspect `{pkg}`: {e}");
            std::process::exit(1);
        }
    };

    for note in &info.errors {
        eprintln!("warning: {note}");
    }
    if info.functions.is_empty() {
        eprintln!(
            "error: `{pkg}` introspected but exports nothing gen-types can generate a signature for \
             (not installed, or every export is an operator/non-identifier form)."
        );
        std::process::exit(1);
    }

    let content = emit_ty(pkg, &info);

    let dir: PathBuf = out_dir.unwrap_or_else(|| Path::new("ty").to_path_buf());
    if let Err(e) = fs::create_dir_all(&dir) {
        eprintln!("error: could not create `{}`: {e}", dir.display());
        std::process::exit(1);
    }
    let path = dir.join(format!("{pkg}.generated.ty"));
    if let Err(e) = fs::write(&path, &content) {
        eprintln!("error: could not write `{}`: {e}", path.display());
        std::process::exit(1);
    }

    println!(
        "{} — {} function(s), all T3 (generated, unverified) → {}",
        pkg,
        info.functions.len(),
        path.display()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample() -> Introspection {
        Introspection {
            r_version: "4.5.2".to_string(),
            pkg_version: Some("1.1.4".to_string()),
            functions: vec![
                GeneratedFn {
                    name: "filter".to_string(),
                    has_dots: true,
                    params: vec![".data".to_string()],
                },
                GeneratedFn {
                    name: "is.numeric".to_string(),
                    has_dots: false,
                    params: vec!["x".to_string()],
                },
            ],
            errors: vec![],
        }
    }

    #[test]
    fn skips_operator_and_non_identifier_exports() {
        assert!(is_generatable_name("filter"));
        assert!(is_generatable_name("is.numeric"));
        assert!(!is_generatable_name("%>%"));
        assert!(!is_generatable_name("[.foo"));
        assert!(!is_generatable_name("+.money"));
        assert!(!is_generatable_name(""));
        assert!(!is_generatable_name("..."));
    }

    #[test]
    fn quotes_dotted_signature_names_only() {
        assert_eq!(signature_name("filter"), "filter");
        assert_eq!(signature_name("is.numeric"), "`is.numeric`");
    }

    #[test]
    fn emits_import_from_and_variadic_signature() {
        let ty = emit_ty("dplyr", &sample());

        assert!(ty.starts_with("@importFrom dplyr filter is.numeric;\n"));
        assert!(ty.contains("#! tier: T3\n"));
        assert!(ty.contains("#! generated-from: dplyr 1.1.4, R 4.5.2\n"));
        assert!(ty.contains("#! param .data:\n"));
        assert!(ty.contains("#! param ...:\n"));
        assert!(ty.contains("@filter: (Any, ...Any) -> Any;\n"));
        assert!(ty.contains("@`is.numeric`: (Any) -> Any;\n"));
    }

    #[test]
    fn empty_introspection_emits_nothing() {
        let info = Introspection {
            r_version: "4.5.2".to_string(),
            pkg_version: None,
            functions: vec![],
            errors: vec![],
        };
        assert!(emit_ty("empty", &info).is_empty());
    }

    /// Real end-to-end check against packages actually installed on this
    /// machine (registry.md J1: "tests sur 3 packages contrastés"). Fails
    /// open — like every other Rscript-dependent test in this crate — since
    /// CI's `test` job runs with no R and none of these packages installed.
    #[test]
    fn generated_ty_type_checks_for_contrasting_packages() {
        if !rscript_available() {
            eprintln!("skipping: Rscript not on PATH");
            return;
        }
        for pkg in ["jsonlite", "httr2", "dplyr"] {
            let info = match introspect(pkg) {
                Ok(info) if !info.functions.is_empty() => info,
                _ => {
                    eprintln!("skipping {pkg}: not installed on this machine");
                    continue;
                }
            };
            let ty = emit_ty(pkg, &info);
            assert!(!ty.is_empty(), "{pkg}: generated nothing");

            let dir = std::env::temp_dir().join(format!("typr_gen_types_test_{pkg}_{}", std::process::id()));
            let _ = fs::remove_dir_all(&dir);
            fs::create_dir_all(&dir).unwrap();
            crate::engine::write_std_for_type_checking(&dir);
            let file = dir.join(format!("{pkg}.generated.ty"));
            fs::write(&file, &ty).unwrap();

            let context = typr_core::components::context::Context::default()
                .set_environment(typr_core::components::context::config::Environment::Project);
            let (lang, syntax_errors) = crate::engine::parse_code(&file, context.get_environment());
            assert!(syntax_errors.is_empty(), "{pkg}: syntax errors: {syntax_errors:?}");
            let type_checker =
                typr_core::processes::type_checking::type_checker::TypeChecker::new(context).typing_no_panic(&lang);
            assert!(!type_checker.has_errors(), "{pkg}: generated .ty failed to type-check");

            let _ = fs::remove_dir_all(&dir);
        }
    }
}
