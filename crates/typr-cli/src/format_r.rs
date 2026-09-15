//! Formats generated R code with the `styler` package, so `R/*.R` files stay
//! readable when a user opens them to debug a transpilation.
//!
//! Formatting is best-effort: a missing `Rscript`/`styler` or a styling
//! failure must never fail the build, so callers always get code back
//! (formatted if possible, the original otherwise).
//!
//! Only content that genuinely benefits from re-indentation goes through
//! `styler` at all: `main.R` (the transpiled user program, with real nested
//! control flow) does, but `types.R`/`generic_functions.R` are one
//! definition per line by construction (simple `format!` templates in
//! `project.rs`) and never need it — callers should skip this module for
//! those. `styler` also spawns and loads a fresh R process per call, which
//! dominates build time on a project with several generated files; Project
//! builds should go through [`format_r_code_cached`] so unchanged content
//! (by hash, regardless of which file it ends up in) is never re-styled.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;

/// Formats `code` with `styler::style_text()` when available; returns `code`
/// unchanged otherwise (missing Rscript, missing `styler`, or a styling
/// error).
pub fn format_r_code(code: &str) -> String {
    if !styler_available() {
        return code.to_string();
    }
    match run_styler(code) {
        Ok(formatted) => formatted,
        Err(e) => {
            eprintln!("Warning: could not format R output with styler ({e}); writing unformatted code");
            code.to_string()
        }
    }
}

/// Same as [`format_r_code`], but content-addressed under
/// `<cache_dir>/format/<hash>.R`: a `typr build` that regenerates the exact
/// same source for a file (the common case — most files in a project don't
/// change between builds) reuses the previously styled output instead of
/// spawning another `Rscript`. Keyed on the raw pre-format content, so it's
/// shared across files and across builds.
pub fn format_r_code_cached(code: &str, cache_dir: &Path) -> String {
    let entry = format_cache_entry_path(cache_dir, code);
    if let Ok(cached) = std::fs::read_to_string(&entry) {
        return cached;
    }
    let formatted = format_r_code(code);
    if let Some(parent) = entry.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let _ = std::fs::write(&entry, &formatted);
    formatted
}

fn format_cache_entry_path(cache_dir: &Path, code: &str) -> PathBuf {
    cache_dir.join("format").join(format!("{:016x}.R", crate::cache::hash_str(code)))
}

/// `Rscript`/`styler` availability rarely changes within a single run, and
/// probing it spawns a process — check once per process instead of once per
/// file.
fn styler_available() -> bool {
    static AVAILABLE: OnceLock<bool> = OnceLock::new();
    *AVAILABLE.get_or_init(|| {
        Command::new("Rscript")
            .arg("-e")
            .arg("quit(status = if (requireNamespace('styler', quietly = TRUE)) 0L else 1L)")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map(|status| status.success())
            .unwrap_or(false)
    })
}

fn run_styler(code: &str) -> Result<String, String> {
    let script = "con <- file('stdin'); \
                  code <- readLines(con, warn = FALSE); \
                  close(con); \
                  out <- styler::style_text(code); \
                  cat(paste(as.character(out), collapse = '\n'), '\n', sep = '')";

    let mut child = Command::new("Rscript")
        .arg("-e")
        .arg(script)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("failed to spawn Rscript: {e}"))?;

    child
        .stdin
        .take()
        .expect("piped stdin")
        .write_all(code.as_bytes())
        .map_err(|e| format!("failed to write code to Rscript stdin: {e}"))?;

    let output = child
        .wait_with_output()
        .map_err(|e| format!("failed to wait on Rscript: {e}"))?;

    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).trim().to_string());
    }

    let formatted = String::from_utf8(output.stdout).map_err(|e| format!("styler produced non-UTF-8 output: {e}"))?;
    if formatted.trim().is_empty() && !code.trim().is_empty() {
        return Err("styler produced empty output".to_string());
    }
    Ok(formatted)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn leaves_code_unchanged_when_styler_is_unavailable() {
        if styler_available() {
            return; // exercised by `styles_messy_code_when_available` instead
        }
        let messy = "f<-function(x){x+1}";
        assert_eq!(format_r_code(messy), messy);
    }

    #[test]
    fn styles_messy_code_when_available() {
        if !styler_available() {
            return; // no Rscript/styler on this machine — nothing to check
        }
        let messy = "f<-function(x){\nx+1\n}";
        let formatted = format_r_code(messy);
        assert_ne!(formatted, messy);
        assert!(formatted.contains("f <- function(x) {"));
    }

    #[test]
    fn cached_format_reuses_disk_entry_without_reformatting() {
        let dir = std::env::temp_dir().join(format!("typr_format_cache_test_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        let _ = std::fs::create_dir_all(&dir);

        let code = "f<-function(x){x+1}";
        let first = format_r_code_cached(code, &dir);

        // Pre-seed a distinguishable value directly at the cache entry, so a
        // cache hit (vs. a fresh, indistinguishable-from-`first` styler run)
        // is unambiguous.
        let entry = format_cache_entry_path(&dir, code);
        std::fs::write(&entry, "# from cache\n").unwrap();
        assert_eq!(format_r_code_cached(code, &dir), "# from cache\n");

        let _ = std::fs::remove_dir_all(&dir);
        let _ = first; // first run succeeded without panicking, cache dir was created
    }
}
