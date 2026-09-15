//! Formats generated R code with the `styler` package, so `R/*.R` files stay
//! readable when a user opens them to debug a transpilation.
//!
//! Formatting is best-effort: a missing `Rscript`/`styler` or a styling
//! failure must never fail the build, so callers always get code back
//! (formatted if possible, the original otherwise).

use std::io::Write;
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
}
