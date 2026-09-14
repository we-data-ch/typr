//! `typr types validate <package> [--repo SPEC]` — the mechanically-verifiable
//! checks of `typR/registry.md` §9, run against a single Type Definition
//! repository (registry.md §13 J4, "contrôles mécaniques de §9, dont le diff
//! `formals()` contre le package installé").
//!
//! One check named in §9 is deliberately out of scope here and stays open in
//! the J4 checklist: "schéma JSON du registre valide" is a property of
//! `we-data-ch/registry`'s own `packages/*.json` files, not of one definition
//! repository (it already gets a mechanical check for free every time
//! `type_registry::lookup_in_registry_dir`/`search_in_registry_dir` parses one
//! into `RegistryPackageFile` — a malformed file simply resolves to no
//! candidates, per D2 — but there is no *dedicated* validator yet that flags
//! *which* file is malformed). "revalidation périodique" (drift re-detection
//! over time) — a place to run this validator centrally, on every registry
//! entry, on a schedule — is [`crate::registry_revalidate`], the next J4 item.
//!
//! Every other §9 line is a [`CheckResult`] here:
//!
//! - `format_version` known, repository accessible, rev pinned, digest —
//!   mostly free: `type_registry::fetch` already enforces/computes these
//!   before this module sees anything.
//! - capabilities coherent with the real content — same, via `fetch`'s
//!   returned warnings (`type_registry::check_capabilities`).
//! - the `.ty` files parse and type-check, and `tests/smoke.ty` compiles —
//!   reuses `standard_library::load_external_ty_definitions`, the exact loop
//!   a consuming project runs.
//! - every declared name is really exported by the installed package, and its
//!   arity/`...`-ness matches `formals()` — the new piece: diffs
//!   `parse_declared_entries`'s view of the `.ty` sources against
//!   `gen_types::introspect`'s view of the installed R package.
//! - no `T1` entry with an unconstrained `...` — the RFC-STDLIB-0001 §7
//!   promotion gate (registry.md §6, "Promotion T3 → T2 → T1"), applied here
//!   for the first time to *external* definitions rather than the bundled
//!   stdlib.
//! - the package exists on CRAN — shelled out to `Rscript`'s own
//!   `available.packages()` against the public CRAN mirror, the same
//!   shell-out-rather-than-add-an-HTTP-client choice `gen_types.rs`/
//!   `type_registry.rs` already made for R/`git`.
//!
//! Every introspection-dependent check (exports, arity, CRAN) fails *open*
//! when `Rscript` is unavailable or the package isn't installed locally —
//! `Skipped`, never `Fail` — the same contract every other Rscript-dependent
//! path in this crate already follows (`gen_types.rs`, `r_name_cache.rs`).
//! Only a check that could actually run and found something wrong reports
//! `Fail`; `typr types validate`'s exit code is 1 only in that case.

use crate::gen_types;
use crate::standard_library;
use crate::type_registry::{self, FetchedDefinition, RepoSpec};
use std::collections::HashMap;
use std::fs;
use std::process::Command;
use typr_core::components::context::Context;
use typr_core::processes::spg::stdlib_meta::parse_meta_from_source;

// ---------------------------------------------------------------------
// Report shape
// ---------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckStatus {
    Pass,
    Warn,
    Fail,
    /// Could not run (missing `Rscript`/`git`, package not installed
    /// locally, network unreachable, no `tests/smoke.ty` present, …) — never
    /// a reason to fail the build (registry.md D2).
    Skipped,
}

#[derive(Debug, Clone)]
pub struct CheckResult {
    pub name: &'static str,
    pub status: CheckStatus,
    pub detail: String,
}

impl CheckResult {
    fn pass(name: &'static str, detail: impl Into<String>) -> Self {
        CheckResult {
            name,
            status: CheckStatus::Pass,
            detail: detail.into(),
        }
    }
    fn warn(name: &'static str, detail: impl Into<String>) -> Self {
        CheckResult {
            name,
            status: CheckStatus::Warn,
            detail: detail.into(),
        }
    }
    fn fail(name: &'static str, detail: impl Into<String>) -> Self {
        CheckResult {
            name,
            status: CheckStatus::Fail,
            detail: detail.into(),
        }
    }
    fn skipped(name: &'static str, detail: impl Into<String>) -> Self {
        CheckResult {
            name,
            status: CheckStatus::Skipped,
            detail: detail.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValidationReport {
    pub package: String,
    pub repository: String,
    pub definition_version: String,
    pub checks: Vec<CheckResult>,
}

impl ValidationReport {
    /// `false` when any check is `Fail` — everything else (`Warn`,
    /// `Skipped`) is, per D5, information to display, never a build-blocking
    /// signal on its own.
    pub fn ok(&self) -> bool {
        !self.checks.iter().any(|c| c.status == CheckStatus::Fail)
    }

    /// The nominative report of registry.md §9: what was verified, named,
    /// never a single green badge.
    pub fn render(&self) -> String {
        let mut out = format!(
            "{} — {} (definition v{})\n",
            self.package, self.repository, self.definition_version
        );
        for c in &self.checks {
            let word = match c.status {
                CheckStatus::Pass => "ok",
                CheckStatus::Warn => "warning",
                CheckStatus::Fail => "FAILED",
                CheckStatus::Skipped => "not checked",
            };
            out.push_str(&format!("  {:<26} {:<12} {}\n", c.name, word, c.detail));
        }
        out
    }
}

// ---------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------

/// Fetch `spec_str` (`github:owner/repo[@rev]`) and run every check below
/// against it. Always cleans up the temporary clone, success or failure.
pub fn validate(package: &str, spec_str: &str) -> ValidationReport {
    let spec = match RepoSpec::parse(spec_str) {
        Ok(s) => s,
        Err(e) => {
            return ValidationReport {
                package: package.to_string(),
                repository: spec_str.to_string(),
                definition_version: "unknown".to_string(),
                checks: vec![CheckResult::fail("repository spec", e)],
            }
        }
    };

    let (fetched, warnings) = match type_registry::fetch(&spec) {
        Ok(ok) => ok,
        Err(e) => {
            return ValidationReport {
                package: package.to_string(),
                repository: spec.display(),
                definition_version: "unknown".to_string(),
                checks: vec![CheckResult::fail("fetch", e)],
            }
        }
    };

    let report = validate_fetched(package, &spec.display(), spec.rev.is_some(), &fetched, &warnings);
    let _ = fs::remove_dir_all(&fetched.dir);
    report
}

// ---------------------------------------------------------------------
// The checks themselves — pure given an already-fetched definition, so
// this is directly testable against a hand-built temp directory with no
// `git`/network involved (see `tests` below).
// ---------------------------------------------------------------------

fn validate_fetched(
    package: &str,
    repository: &str,
    rev_pinned: bool,
    fetched: &FetchedDefinition,
    capability_warnings: &[String],
) -> ValidationReport {
    let mut checks = Vec::new();

    checks.push(CheckResult::pass(
        "format_version",
        fetched.manifest.format_version.to_string(),
    ));

    checks.push(CheckResult::pass(
        "repository accessible",
        format!("cloned @ {}", short_rev(&fetched.rev)),
    ));
    checks.push(if rev_pinned {
        CheckResult::pass("rev pinned", &fetched.rev)
    } else {
        CheckResult::warn(
            "rev pinned",
            "resolved from the default branch's HEAD — pass @<rev> to pin reproducibly (registry.md D4)",
        )
    });
    checks.push(CheckResult::pass("digest", fetched.digest.clone()));

    if capability_warnings.is_empty() {
        checks.push(CheckResult::pass(
            "capabilities",
            "no undeclared R; r_shims/extern_raw not declared",
        ));
    } else {
        checks.push(CheckResult::warn("capabilities", capability_warnings.join(" ")));
    }

    let all_files = type_registry::tracked_files(&fetched.dir).unwrap_or_default();
    // Declarations first, `tests/` last — a plain lexicographic walk would
    // interleave them by accident ("tests/…" sorts before "ty/…") and
    // type-check a smoke test against a context that doesn't have its own
    // definitions loaded yet.
    let mut declaration_files: Vec<&std::path::PathBuf> = Vec::new();
    let mut test_files: Vec<&std::path::PathBuf> = Vec::new();
    for rel in &all_files {
        if rel.extension().and_then(|e| e.to_str()) != Some("ty") {
            continue;
        }
        if rel.starts_with("tests") {
            test_files.push(rel);
        } else {
            declaration_files.push(rel);
        }
    }
    let mut ty_sources: Vec<(String, String)> = Vec::new();
    for rel in declaration_files.into_iter().chain(test_files) {
        let content = fs::read_to_string(fetched.dir.join(rel)).unwrap_or_default();
        ty_sources.push((rel.to_string_lossy().replace('\\', "/"), content));
    }

    let sources_ref: Vec<(&str, &str)> = ty_sources.iter().map(|(f, s)| (f.as_str(), s.as_str())).collect();
    // trust = "T1" here has no bearing on this check: `load_external_ty_definitions`
    // only *degrades* entries below trust after they've already
    // parsed/type-checked, and `skipped` records failures from before that
    // point — this just avoids implying a trust decision that isn't ours to
    // make in a validator.
    let (_context, skipped) = standard_library::load_external_ty_definitions(
        Context::default(),
        &sources_ref,
        &fetched.manifest.definition.tier,
        "T1",
    );

    if ty_sources.is_empty() {
        checks.push(CheckResult::warn(
            ".ty parse/type-check",
            "no .ty file found in this repository",
        ));
    } else if skipped.is_empty() {
        checks.push(CheckResult::pass(
            ".ty parse/type-check",
            format!("{} file(s) OK", ty_sources.len()),
        ));
    } else {
        let names: Vec<&str> = skipped.iter().map(|(f, _)| f.as_str()).collect();
        checks.push(CheckResult::fail(
            ".ty parse/type-check",
            format!(
                "{}/{} file(s) failed: {}",
                skipped.len(),
                ty_sources.len(),
                names.join(", ")
            ),
        ));
    }

    const SMOKE_PATH: &str = "tests/smoke.ty";
    match ty_sources.iter().any(|(f, _)| f == SMOKE_PATH) {
        true => match skipped.iter().find(|(f, _)| f == SMOKE_PATH) {
            Some((_, message)) => checks.push(CheckResult::fail("tests/smoke.ty", message.clone())),
            None => checks.push(CheckResult::pass("tests/smoke.ty", "compiles")),
        },
        false => checks.push(CheckResult::skipped("tests/smoke.ty", "not present in this repository")),
    }

    // Declared names/arity/tier — deliberately excludes `tests/`, which
    // exercises the API rather than declaring it (registry.md §5.1).
    let declared_sources: Vec<(String, String)> = ty_sources
        .iter()
        .filter(|(f, _)| !f.starts_with("tests/"))
        .cloned()
        .collect();
    let declared = parse_declared_entries(&declared_sources, &fetched.manifest.definition.tier);

    let t1_entries: Vec<&DeclaredEntry> = declared.iter().filter(|e| e.tier.as_deref() == Some("T1")).collect();
    let t1_violations: Vec<&str> = t1_entries
        .iter()
        .filter(|e| e.params.iter().any(|p| p.is_variadic && p.type_text.trim() == "Any"))
        .map(|e| e.name.as_str())
        .collect();
    if t1_violations.is_empty() {
        checks.push(CheckResult::pass(
            "T1 promotion gate",
            format!("{} T1 entrie(s), none with an unconstrained `...`", t1_entries.len()),
        ));
    } else {
        checks.push(CheckResult::fail(
            "T1 promotion gate",
            format!("unconstrained `...` at T1: {}", t1_violations.join(", ")),
        ));
    }

    if !gen_types::rscript_available() {
        checks.push(CheckResult::skipped("exports vs formals()", "Rscript not on PATH"));
        checks.push(CheckResult::skipped("arity vs formals()", "Rscript not on PATH"));
        checks.push(CheckResult::skipped("package on CRAN", "Rscript not on PATH"));
    } else {
        match gen_types::introspect(package) {
            Ok(info) if !info.functions.is_empty() => {
                push_formals_diff(&mut checks, &declared, &info.functions);
            }
            _ => {
                checks.push(CheckResult::skipped(
                    "exports vs formals()",
                    format!("`{package}` not installed locally"),
                ));
                checks.push(CheckResult::skipped(
                    "arity vs formals()",
                    format!("`{package}` not installed locally"),
                ));
            }
        }

        match check_cran_availability(package) {
            Some(true) => checks.push(CheckResult::pass("package on CRAN", "found on cloud.r-project.org")),
            Some(false) => checks.push(CheckResult::warn(
                "package on CRAN",
                "not found on CRAN — may be R-universe/Bioconductor/GitHub-only",
            )),
            None => checks.push(CheckResult::skipped(
                "package on CRAN",
                "could not reach the CRAN mirror",
            )),
        }
    }

    ValidationReport {
        package: package.to_string(),
        repository: repository.to_string(),
        definition_version: fetched.manifest.definition.version.clone(),
        checks,
    }
}

fn short_rev(rev: &str) -> &str {
    &rev[..rev.len().min(12)]
}

/// Diff `declared` (what the `.ty` sources say) against `installed` (what
/// `formals()` says on the real, locally installed package) — the "diff
/// `formals()` contre le package installé" registry.md §13 J4 names
/// explicitly. Pushes both the "exports vs formals()" and "arity vs
/// formals()" checks.
fn push_formals_diff(
    checks: &mut Vec<CheckResult>,
    declared: &[DeclaredEntry],
    installed_fns: &[gen_types::GeneratedFn],
) {
    let installed: HashMap<&str, &gen_types::GeneratedFn> =
        installed_fns.iter().map(|f| (f.name.as_str(), f)).collect();

    let mut missing = Vec::new();
    let mut mismatches = Vec::new();
    for entry in declared {
        match installed.get(entry.name.as_str()) {
            None => missing.push(entry.name.clone()),
            Some(f) => {
                let declared_fixed = entry.params.iter().filter(|p| !p.is_variadic).count();
                let declared_variadic = entry.params.iter().any(|p| p.is_variadic);
                if declared_fixed != f.params.len() || declared_variadic != f.has_dots {
                    mismatches.push(format!(
                        "{} (declared {} arg(s){}, formals() has {} arg(s){})",
                        entry.name,
                        declared_fixed,
                        if declared_variadic { "+..." } else { "" },
                        f.params.len(),
                        if f.has_dots { "+..." } else { "" }
                    ));
                }
            }
        }
    }

    let total = declared.len();
    let found = total - missing.len();
    if missing.is_empty() {
        checks.push(CheckResult::pass(
            "exports vs formals()",
            format!("{found}/{total} found"),
        ));
    } else {
        checks.push(CheckResult::fail(
            "exports vs formals()",
            format!("{found}/{total} found — missing: {}", missing.join(", ")),
        ));
    }
    if mismatches.is_empty() {
        checks.push(CheckResult::pass(
            "arity vs formals()",
            format!("{total}/{total} match"),
        ));
    } else {
        checks.push(CheckResult::fail(
            "arity vs formals()",
            format!("{} mismatch(es): {}", mismatches.len(), mismatches.join("; ")),
        ));
    }
}

/// Best-effort: is `pkg` on CRAN, via `Rscript`'s own
/// `available.packages()` against the public mirror — the same
/// shell-out-to-`Rscript` choice `gen_types.rs` already made, rather than
/// adding an HTTP client dependency for one query. `None` (never a `Fail`)
/// when `Rscript` errors, the mirror is unreachable, or `pkg` isn't a plain
/// package-name-shaped string (never interpolated into R source otherwise —
/// same guard as `gen_types::is_generatable_name`).
fn check_cran_availability(pkg: &str) -> Option<bool> {
    if pkg.is_empty() || !pkg.chars().all(|c| c.is_ascii_alphanumeric() || c == '.' || c == '_') {
        return None;
    }
    let expr = format!(
        "ap <- tryCatch(available.packages(repos = \"https://cloud.r-project.org\"), error = function(e) NULL); \
         if (is.null(ap)) cat(\"NETERR\") else if (\"{pkg}\" %in% rownames(ap)) cat(\"YES\") else cat(\"NO\")"
    );
    let output = Command::new("Rscript").arg("-e").arg(&expr).output().ok()?;
    if !output.status.success() {
        return None;
    }
    match String::from_utf8_lossy(&output.stdout).trim() {
        "YES" => Some(true),
        "NO" => Some(false),
        _ => None,
    }
}

// ---------------------------------------------------------------------
// Declared signature parsing — arity, parameter names and tier straight
// from the `.ty` source text, independent of the type-checker's own
// preprocessing (which strips parameter names before parsing, since the
// type grammar doesn't accept them — `standard_library::preprocess_ty_source`).
// ---------------------------------------------------------------------

struct DeclaredParam {
    is_variadic: bool,
    type_text: String,
}

struct DeclaredEntry {
    /// Display name, backticks stripped.
    name: String,
    /// Effective tier: the entry's own `#! tier:`, or the manifest's
    /// `[definition] tier` when absent.
    tier: Option<String>,
    params: Vec<DeclaredParam>,
}

fn parse_declared_entries(ty_sources: &[(String, String)], default_tier: &str) -> Vec<DeclaredEntry> {
    let mut out = Vec::new();
    for (_file, source) in ty_sources {
        let meta_map = parse_meta_from_source(source);
        for line in source.lines() {
            let trimmed = line.trim();
            if !trimmed.starts_with('@') {
                continue;
            }
            let Some(sig) = parse_signature_line(trimmed) else {
                continue;
            };
            let tier = meta_map
                .get(&sig.raw_name)
                .and_then(|m| m.tier.clone())
                .or_else(|| Some(default_tier.to_string()));
            out.push(DeclaredEntry {
                name: unwrap_backtick(&sig.raw_name),
                tier,
                params: sig.params,
            });
        }
    }
    out
}

struct ParsedSignature {
    /// Exactly what `#! tier:`-map lookups need to match against — keeps
    /// surrounding backticks, since `parse_meta_from_source` keys its map the
    /// same way.
    raw_name: String,
    params: Vec<DeclaredParam>,
}

fn unwrap_backtick(name: &str) -> String {
    name.trim_matches('`').to_string()
}

/// Parse one `@name: (...) -> Ret;` (or `@extern pkg::name: ...;`) line into
/// its raw name and parameter list. `None` for a non-function declaration
/// (`type X <- Foreign<Any>;`, a bare `@x: int;` constant, …) — those don't
/// carry an arity to diff against `formals()`.
fn parse_signature_line(line: &str) -> Option<ParsedSignature> {
    let rest = line.strip_prefix('@')?;
    let sep = find_unqualified_colon(rest)?;
    let head = &rest[..sep];
    let raw_name = head
        .strip_prefix("extern ")
        .map(|n| n.rsplit("::").next().unwrap_or(n))
        .unwrap_or(head)
        .trim();
    if raw_name.is_empty() {
        return None;
    }
    let sig = rest[sep + 1..].trim();
    let sig = sig.strip_suffix(';').unwrap_or(sig).trim();
    if !sig.starts_with('(') {
        return None;
    }
    let params = parse_param_list(sig)?;
    Some(ParsedSignature {
        raw_name: raw_name.to_string(),
        params,
    })
}

/// The first `:` in `text` that is not part of a `::` (which appears in
/// `@extern pkg::name`) — same rule stdlib_meta.rs's private
/// `extract_signature_name` uses, reimplemented here since that one isn't
/// exported across the crate boundary.
fn find_unqualified_colon(text: &str) -> Option<usize> {
    let bytes = text.as_bytes();
    for (i, &b) in bytes.iter().enumerate() {
        if b == b':' {
            let prev = if i > 0 { bytes[i - 1] } else { 0 };
            let next = if i + 1 < bytes.len() { bytes[i + 1] } else { 0 };
            if prev != b':' && next != b':' {
                return Some(i);
            }
        }
    }
    None
}

/// `sig` starts with `(` — find its matching `)` (bracket-depth aware, so a
/// nested function-type or generic parameter doesn't confuse the split) and
/// parse the top-level comma-separated parameter list inside.
fn parse_param_list(sig: &str) -> Option<Vec<DeclaredParam>> {
    let chars: Vec<char> = sig.chars().collect();
    let mut depth = 0i32;
    let mut close_idx = None;
    for (i, &c) in chars.iter().enumerate() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => {
                depth -= 1;
                if depth == 0 && c == ')' {
                    close_idx = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }
    let close_idx = close_idx?;
    let inner: String = chars[1..close_idx].iter().collect();
    Some(
        split_top_level(&inner)
            .into_iter()
            .map(|p| parse_one_param(&p))
            .collect(),
    )
}

fn split_top_level(inner: &str) -> Vec<String> {
    let trimmed = inner.trim();
    if trimmed.is_empty() {
        return Vec::new();
    }
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut current = String::new();
    for c in trimmed.chars() {
        match c {
            '(' | '[' | '{' => {
                depth += 1;
                current.push(c);
            }
            ')' | ']' | '}' => {
                depth -= 1;
                current.push(c);
            }
            ',' if depth == 0 => {
                parts.push(current.trim().to_string());
                current.clear();
            }
            _ => current.push(c),
        }
    }
    if !current.trim().is_empty() {
        parts.push(current.trim().to_string());
    }
    parts
}

fn parse_one_param(text: &str) -> DeclaredParam {
    let (is_variadic, rest) = match text.strip_prefix("...") {
        Some(r) => (true, r.trim()),
        None => (false, text.trim()),
    };
    if rest.is_empty() {
        return DeclaredParam {
            is_variadic,
            type_text: "Any".to_string(),
        };
    }
    match find_unqualified_colon(rest) {
        Some(colon_idx) => DeclaredParam {
            is_variadic,
            type_text: rest[colon_idx + 1..].trim().to_string(),
        },
        None => DeclaredParam {
            is_variadic,
            type_text: rest.to_string(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::type_definition::{parse_manifest, DefinitionManifest};
    use std::path::{Path, PathBuf};

    // -- Signature parsing --------------------------------------------

    #[test]
    fn parses_unnamed_generated_style_signature() {
        let sig = parse_signature_line("@filter: (Any, ...Any) -> Any;").unwrap();
        assert_eq!(sig.raw_name, "filter");
        assert_eq!(sig.params.len(), 2);
        assert!(!sig.params[0].is_variadic);
        assert_eq!(sig.params[0].type_text, "Any");
        assert!(sig.params[1].is_variadic);
        assert_eq!(sig.params[1].type_text, "Any");
    }

    #[test]
    fn parses_named_hand_authored_signature() {
        let sig = parse_signature_line("@cat: (...values: Any) -> Empty;").unwrap();
        assert_eq!(sig.raw_name, "cat");
        assert_eq!(sig.params.len(), 1);
        assert!(sig.params[0].is_variadic);
        assert_eq!(sig.params[0].type_text, "Any");
    }

    #[test]
    fn parses_extern_pkg_double_colon_signature() {
        let sig = parse_signature_line("@extern jsonlite::toJSON: (Any) -> char;").unwrap();
        assert_eq!(sig.raw_name, "toJSON");
        assert_eq!(sig.params.len(), 1);
    }

    #[test]
    fn parses_backtick_quoted_name() {
        let sig = parse_signature_line("@`is.numeric`: (Any) -> bool;").unwrap();
        assert_eq!(sig.raw_name, "`is.numeric`");
    }

    #[test]
    fn nested_higher_order_param_does_not_confuse_arity() {
        let sig = parse_signature_line("@apply_fn: (f: (int) -> int, x: int) -> int;").unwrap();
        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.params[0].type_text, "(int) -> int");
        assert_eq!(sig.params[1].type_text, "int");
    }

    #[test]
    fn non_function_declaration_is_not_a_signature() {
        assert!(parse_signature_line("type DataFrame <- Foreign<Any>;").is_none());
    }

    // -- Declared entries + tier fallback -------------------------------

    #[test]
    fn own_tier_annotation_wins_over_manifest_default() {
        let src = "#! tier: T1\n@a: (Any) -> Any;\n\n@b: (Any) -> Any;\n";
        let declared = parse_declared_entries(&[("core.ty".to_string(), src.to_string())], "T3");
        let a = declared.iter().find(|e| e.name == "a").unwrap();
        let b = declared.iter().find(|e| e.name == "b").unwrap();
        assert_eq!(a.tier.as_deref(), Some("T1"));
        assert_eq!(b.tier.as_deref(), Some("T3"));
    }

    // -- T1 promotion gate + formals() diff, via `validate_fetched` -----

    fn manifest(tier: &str) -> DefinitionManifest {
        parse_manifest(&format!(
            "format_version = 1\n\
             [package]\nname = \"shiny\"\nsince = \"1.11.0\"\n\
             [definition]\nversion = \"0.3.0\"\ntier = \"{tier}\"\n\
             [provider]\ntype = \"community\"\nrepository = \"github:alice/typr-shiny\"\n"
        ))
        .unwrap()
    }

    fn write_fetched(subdir: &str, files: &[(&str, &str)]) -> FetchedDefinition {
        let dir = std::env::temp_dir().join(format!("typr_registry_validate_{subdir}_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        for (rel, content) in files {
            let path = dir.join(rel);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(&path, content).unwrap();
        }
        FetchedDefinition {
            manifest: manifest("T2"),
            rev: "deadbeefcafef00d".to_string(),
            digest: "sha256:test".to_string(),
            dir,
        }
    }

    fn cleanup(fetched: &FetchedDefinition) {
        let _ = fs::remove_dir_all(&fetched.dir);
    }

    #[test]
    fn clean_definition_passes_the_non_introspection_checks() {
        let fetched = write_fetched(
            "clean",
            &[(
                "ty/core.ty",
                "#! tier: T2\n@importFrom shiny fluidPage;\n@fluidPage: (Any) -> Any;\n",
            )],
        );
        let report = validate_fetched("shiny", "github:alice/typr-shiny", true, &fetched, &[]);
        cleanup(&fetched);

        let by_name = |n: &str| report.checks.iter().find(|c| c.name == n).unwrap();
        assert_eq!(by_name("format_version").status, CheckStatus::Pass);
        assert_eq!(by_name("rev pinned").status, CheckStatus::Pass);
        assert_eq!(by_name("capabilities").status, CheckStatus::Pass);
        assert_eq!(by_name(".ty parse/type-check").status, CheckStatus::Pass);
        assert_eq!(by_name("tests/smoke.ty").status, CheckStatus::Skipped);
        assert_eq!(by_name("T1 promotion gate").status, CheckStatus::Pass);
    }

    #[test]
    fn unpinned_rev_warns() {
        let fetched = write_fetched("unpinned", &[("ty/core.ty", "@f: (Any) -> Any;\n")]);
        let report = validate_fetched("shiny", "github:alice/typr-shiny", false, &fetched, &[]);
        cleanup(&fetched);
        let rev_check = report.checks.iter().find(|c| c.name == "rev pinned").unwrap();
        assert_eq!(rev_check.status, CheckStatus::Warn);
    }

    /// A package name no real registry/CRAN package will ever have —
    /// guarantees `gen_types::introspect` finds nothing installed, so tests
    /// that assert on `report.ok()` as a whole aren't at the mercy of what
    /// happens to be installed on the machine running the suite (unlike
    /// `formals_diff_flags_a_missing_export_and_an_arity_mismatch`, below,
    /// which deliberately wants a real installed package).
    const FAKE_PACKAGE: &str = "typr_registry_validate_fixture_zzz";

    #[test]
    fn declared_capability_warning_is_surfaced_not_failed() {
        let fetched = write_fetched("capwarn", &[("ty/core.ty", "@f: (Any) -> Any;\n")]);
        let report = validate_fetched(
            FAKE_PACKAGE,
            "github:alice/typr-shiny",
            true,
            &fetched,
            &["ships R shims".to_string()],
        );
        cleanup(&fetched);
        let cap = report.checks.iter().find(|c| c.name == "capabilities").unwrap();
        assert_eq!(cap.status, CheckStatus::Warn);
        assert!(report.ok(), "a declared capability must not fail validation");
    }

    #[test]
    fn broken_ty_source_fails_the_parse_check() {
        // `fn(x)` with no parameter type hits the dedicated
        // `SyntaxError::FunctionWithoutType` panic in `parsing/elements.rs`
        // (see `standard_library.rs::tests::broken_ty_source_is_reported_as_skipped_with_a_real_message`).
        let fetched = write_fetched("broken", &[("ty/core.ty", "let f <- fn(x) { x };\n")]);
        let report = validate_fetched(FAKE_PACKAGE, "github:alice/typr-shiny", true, &fetched, &[]);
        cleanup(&fetched);
        let parse_check = report.checks.iter().find(|c| c.name == ".ty parse/type-check").unwrap();
        assert_eq!(parse_check.status, CheckStatus::Fail);
        assert!(!report.ok());
    }

    #[test]
    fn present_smoke_test_is_checked_and_passes_when_it_typechecks() {
        let fetched = write_fetched(
            "smoke",
            &[
                (
                    "ty/core.ty",
                    "@importFrom shiny fluidPage;\n@fluidPage: (Any) -> Any;\n",
                ),
                ("tests/smoke.ty", "let x <- fluidPage(1);\n"),
            ],
        );
        let report = validate_fetched("shiny", "github:alice/typr-shiny", true, &fetched, &[]);
        cleanup(&fetched);
        let smoke = report.checks.iter().find(|c| c.name == "tests/smoke.ty").unwrap();
        assert_eq!(smoke.status, CheckStatus::Pass);
    }

    #[test]
    fn t1_entry_with_unconstrained_variadic_fails_the_promotion_gate() {
        let dir = std::env::temp_dir().join(format!("typr_registry_validate_t1gate_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        let rel = Path::new("ty/core.ty");
        let path: PathBuf = dir.join(rel);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, "#! tier: T1\n@risky: (...values: Any) -> Any;\n").unwrap();
        let fetched = FetchedDefinition {
            manifest: manifest("T2"),
            rev: "abc123".to_string(),
            digest: "sha256:test".to_string(),
            dir,
        };

        let report = validate_fetched("shiny", "github:alice/typr-shiny", true, &fetched, &[]);
        cleanup(&fetched);

        let gate = report.checks.iter().find(|c| c.name == "T1 promotion gate").unwrap();
        assert_eq!(gate.status, CheckStatus::Fail);
        assert!(gate.detail.contains("risky"), "unexpected detail: {}", gate.detail);
        assert!(!report.ok());
    }

    #[test]
    fn t1_entry_with_constrained_variadic_passes_the_gate() {
        let fetched = write_fetched(
            "t1ok",
            &[("ty/core.ty", "#! tier: T1\n@safe: (...values: int) -> Any;\n")],
        );
        let report = validate_fetched("shiny", "github:alice/typr-shiny", true, &fetched, &[]);
        cleanup(&fetched);
        let gate = report.checks.iter().find(|c| c.name == "T1 promotion gate").unwrap();
        assert_eq!(gate.status, CheckStatus::Pass);
    }

    // -- formals() diff, against a real installed package (fail-open like
    // every other Rscript-dependent test in this crate: skipped, not
    // failed, when R/the package isn't available on this machine — see
    // `gen_types.rs::generated_ty_type_checks_for_contrasting_packages`) --

    #[test]
    fn formals_diff_flags_a_missing_export_and_an_arity_mismatch() {
        if !gen_types::rscript_available() {
            eprintln!("skipping: Rscript not on PATH");
            return;
        }
        let info = match gen_types::introspect("jsonlite") {
            Ok(info) if !info.functions.is_empty() => info,
            _ => {
                eprintln!("skipping: jsonlite not installed on this machine");
                return;
            }
        };
        let real = info.functions.iter().find(|f| f.name == "toJSON");
        let Some(real) = real else {
            eprintln!("skipping: jsonlite::toJSON not found by introspection");
            return;
        };
        // `toJSON` declared with one extra fixed argument beyond its real
        // `formals()` — a real, existing export with the wrong arity — plus
        // a name that plain doesn't exist in the package at all.
        let wrong_arity_args = std::iter::repeat("Any")
            .take(real.params.len() + 1)
            .collect::<Vec<_>>()
            .join(", ");
        let src = format!(
            "@extern jsonlite::toJSON: ({wrong_arity_args}) -> char;\n\
             @extern jsonlite::doesNotExist: (Any) -> char;\n"
        );
        let fetched = write_fetched("formals", &[("ty/core.ty", &src)]);
        let report = validate_fetched("jsonlite", "github:alice/typr-jsonlite", true, &fetched, &[]);
        cleanup(&fetched);

        let exports = report.checks.iter().find(|c| c.name == "exports vs formals()").unwrap();
        assert_eq!(exports.status, CheckStatus::Fail);
        assert!(
            exports.detail.contains("doesNotExist"),
            "unexpected detail: {}",
            exports.detail
        );

        let arity = report.checks.iter().find(|c| c.name == "arity vs formals()").unwrap();
        assert_eq!(arity.status, CheckStatus::Fail);
        assert!(arity.detail.contains("toJSON"), "unexpected detail: {}", arity.detail);
    }
}
