//! Integration tests for the Phase C static lint (soundness_transpilation.md
//! § Phase C): `typr build` intersects every top-level name it's about to
//! emit against a database of base-R/S4 names, catching the "collision with
//! the rest of R's object systems" bug family (the `nlevels` bug is the
//! historical example, see CLAUDE.md) without ever running R.
//!
//! Each test scaffolds a minimal TypR package in a temp directory and drives
//! the real `typr` binary as a subprocess, mirroring
//! `tests/incremental_builds.rs`.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn scaffold_project(name: &str, main_ty: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("typr_lint_{}_{}", name, std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(dir.join("TypR")).unwrap();
    fs::create_dir_all(dir.join("R")).unwrap();
    fs::create_dir_all(dir.join("man")).unwrap();
    fs::write(
        dir.join("DESCRIPTION"),
        "Package: linttest\nVersion: 0.1.0\nTitle: R Name Lint Test\n\
         Description: Fixture for r_name_lint tests.\nAuthor: test\n\
         Maintainer: test <test@test.test>\nLicense: MIT\nEncoding: UTF-8\n",
    )
    .unwrap();
    fs::write(dir.join("NAMESPACE"), "# empty\n").unwrap();
    fs::write(dir.join("TypR/main.ty"), main_ty).unwrap();
    dir
}

fn typr(project: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_typr"))
        .args(args)
        .current_dir(project)
        .output()
        .expect("failed to run typr binary")
}

fn stderr(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

/// Regression guard for the historical `nlevels` bug and the batch of
/// identical collisions Phase C found in typr's own stdlib (`nchar`,
/// `toupper`, `dir__create`'s siblings, etc.): a fresh project with no user
/// code at all must still build cleanly, since `get_all_generic_functions`
/// walks the *entire* preloaded stdlib context, not just user-declared names.
#[test]
fn fresh_project_builds_with_no_lint_errors() {
    let project = scaffold_project("fresh", "# Main TypR code goes here\nprint(\"hello\");\n");
    let result = typr(&project, &["build"]);
    assert!(
        result.status.success(),
        "a fresh project must not trip the r-name-lint: {}",
        stderr(&result)
    );
    assert!(
        !stderr(&result).contains("r-name-lint"),
        "unexpected lint finding on a fresh project: {}",
        stderr(&result)
    );
}

/// Row 1 of the Phase C table: a name that shadows a plain (non-S3-generic)
/// base-R function with no `.default` anywhere — the exact shape of the
/// historical `nlevels` bug. It used to fail the build and demand a
/// hand-written fallback in `std.R`; it is now fixed by codegen, which emits
/// `crossprod.default` forwarding to `base::crossprod` next to the stub so the
/// shadowed implementation stays reachable.
#[test]
fn plain_base_collision_gets_a_generated_default() {
    let project = scaffold_project("collision", "let crossprod <- fn(x: int): int { x };\n");
    let result = typr(&project, &["build"]);
    assert!(
        result.status.success(),
        "a plain base-R collision must now be fixed by codegen, not reported: {}",
        stderr(&result)
    );
    let generic_functions = fs::read_to_string(project.join("R/generic_functions.R")).unwrap();
    assert!(
        generic_functions.contains("crossprod.default <- function(...) base::crossprod(...)"),
        "expected a generated fallback in generic_functions.R:\n{generic_functions}"
    );
    // The stub itself is still emitted — the fallback complements it.
    assert!(
        generic_functions.contains("crossprod <- function(x, ...) UseMethod('crossprod', x)"),
        "{generic_functions}"
    );
}

/// A name typr's own `std.R` already gives a `.default` to must not get a
/// second, generated one: the hand-written body is authoritative (several
/// deliberately differ from base R, e.g. `strsplit.default` returning `[[1]]`).
#[test]
fn hand_written_std_r_default_is_not_duplicated() {
    let project = scaffold_project("nodup", "let x <- nlevels(factor(c(\"a\", \"b\")));\n");
    let result = typr(&project, &["build"]);
    assert!(result.status.success(), "{}", stderr(&result));
    let generic_functions = fs::read_to_string(project.join("R/generic_functions.R")).unwrap();
    assert!(
        !generic_functions.contains("nlevels.default"),
        "std.R already defines nlevels.default; it must not be generated again:\n{generic_functions}"
    );
}

/// A signature-only declaration (`@name: T;`) whose name exists in no known
/// package is the one case codegen cannot fix: there is nothing to forward to.
/// It warns — naming the two ways out — without failing the build.
///
/// The first parameter is typed `int` rather than `Any` on purpose:
/// `get_all_generic_functions` filters names whose first parameter is `Any`,
/// so an `(Any) -> Any` signature receives no stub at all and there is
/// nothing to warn about.
#[test]
fn unknown_signature_only_name_warns_without_failing() {
    let project = scaffold_project("unknown_sig", "@zz_not_a_real_r_function: (int) -> int;\nlet y <- 1;\n");
    let result = typr(&project, &["build"]);
    assert!(
        result.status.success(),
        "an unresolvable signature must warn, not fail: {}",
        stderr(&result)
    );
    let warn = stderr(&result);
    assert!(warn.contains("warning[r-name-lint]"), "{warn}");
    assert!(warn.contains("zz_not_a_real_r_function"), "{warn}");
}

/// Row 2 of the Phase C table: shadowing a known S4 generic (`show`, package
/// `methods`) is a warning, not a build failure, unless `--strict` is passed.
#[test]
fn s4_generic_collision_is_a_warning_unless_strict() {
    let project = scaffold_project("s4", "let show <- fn(x: int): int { x };\n");

    let plain = typr(&project, &["build"]);
    assert!(
        plain.status.success(),
        "an S4 generic collision alone must not fail a plain build: {}",
        stderr(&plain)
    );
    let warn = stderr(&plain);
    assert!(warn.contains("warning[r-name-lint]"), "{warn}");
    assert!(warn.contains("show"), "{warn}");

    let _ = fs::remove_dir_all(project.join(".typr_cache"));
    let strict = typr(&project, &["build", "--strict"]);
    assert!(
        !strict.status.success(),
        "--strict must escalate the S4 generic warning to a build failure"
    );
    assert!(stderr(&strict).contains("error[r-name-lint]"), "{}", stderr(&strict));
}

/// Row 4 of the Phase C table: a record constructor whose name collides
/// with a known built-in S4 class (`Date`) is a warning, never a build
/// failure (record constructors are a distinct, lower-severity risk than
/// clobbering a dispatch-capable name).
#[test]
fn record_constructor_colliding_with_s4_class_is_a_warning() {
    let project = scaffold_project(
        "ctor",
        "type Date <- list { year: int };\nlet d <- Date:{ year = 2024 };\n",
    );
    let result = typr(&project, &["build"]);
    assert!(
        result.status.success(),
        "a record-constructor/S4-class name collision must stay a warning: {}",
        stderr(&result)
    );
    let warn = stderr(&result);
    assert!(warn.contains("warning[r-name-lint]"), "{warn}");
    assert!(warn.contains("Date"), "{warn}");
}
