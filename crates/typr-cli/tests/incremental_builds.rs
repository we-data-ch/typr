//! Integration tests for the incremental build of `typr build` (project mode).
//!
//! Each test scaffolds a minimal multi-module TypR package in a temp
//! directory and drives the real `typr` binary as a subprocess
//! (`CARGO_BIN_EXE_typr`), mirroring how `typr case run` replays projects.
//!
//! The whole-project short-circuit ("Project up to date") only engages once
//! `devtools::document` has succeeded, so assertions that depend on it are
//! gated on devtools being installed — environments without R/devtools still
//! exercise the write-if-changed and cache-invalidation paths.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::SystemTime;

const MAIN_TY: &str = "mod helper;\nuse helper::*;\n\nlet x <- add_one(1);\n";
const HELPER_TY: &str = "@pub let add_one <- fn(a: int): int { a + 1 };\n";

fn scaffold_project(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("typr_incr_{}_{}", name, std::process::id()));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(dir.join("TypR")).unwrap();
    fs::create_dir_all(dir.join("R")).unwrap();
    fs::create_dir_all(dir.join("man")).unwrap();
    fs::write(
        dir.join("DESCRIPTION"),
        "Package: incrtest\nVersion: 0.1.0\nTitle: Incremental Build Test\n\
         Description: Fixture for incremental build tests.\nAuthor: test\n\
         Maintainer: test <test@test.test>\nLicense: MIT\nEncoding: UTF-8\n",
    )
    .unwrap();
    fs::write(dir.join("NAMESPACE"), "# empty\n").unwrap();
    fs::write(dir.join("TypR/main.ty"), MAIN_TY).unwrap();
    fs::write(dir.join("TypR/helper.ty"), HELPER_TY).unwrap();
    dir
}

fn typr(project: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_typr"))
        .args(args)
        .current_dir(project)
        .output()
        .expect("failed to run typr binary")
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn devtools_available() -> bool {
    Command::new("R")
        .args([
            "-e",
            "quit(status = !requireNamespace('devtools', quietly = TRUE))",
        ])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn mtime(path: &Path) -> SystemTime {
    fs::metadata(path).unwrap().modified().unwrap()
}

#[test]
fn second_build_is_incremental() {
    let project = scaffold_project("second_build");

    let first = typr(&project, &["build"]);
    assert!(first.status.success(), "first build failed: {:?}", first);
    assert!(project.join("R/main.R").exists());
    assert!(project.join("R/helper.R").exists());
    assert!(
        project.join(".typr_cache/manifest.json").exists(),
        "manifest should be written after a successful build"
    );

    let main_r_mtime = mtime(&project.join("R/main.R"));
    let second = typr(&project, &["build"]);
    assert!(second.status.success(), "second build failed: {:?}", second);

    // Unchanged outputs must keep their mtime (write-if-changed), whether
    // the build short-circuited or ran the full pipeline.
    assert_eq!(
        main_r_mtime,
        mtime(&project.join("R/main.R")),
        "R/main.R was rewritten although nothing changed"
    );

    if devtools_available() {
        assert!(
            stdout(&second).contains("Project up to date"),
            "second build should short-circuit, got stdout: {}",
            stdout(&second)
        );
    }

    let _ = fs::remove_dir_all(&project);
}

#[test]
fn no_incremental_forces_full_rebuild() {
    let project = scaffold_project("no_incremental");

    let first = typr(&project, &["build"]);
    assert!(first.status.success(), "first build failed: {:?}", first);

    let forced = typr(&project, &["build", "--no-incremental"]);
    assert!(forced.status.success(), "forced build failed: {:?}", forced);
    assert!(
        !stdout(&forced).contains("Project up to date"),
        "--no-incremental must never short-circuit"
    );
    // The full pipeline ran: its completion message is printed.
    assert!(
        stdout(&forced).contains("R code successfully generated"),
        "expected full pipeline output, got: {}",
        stdout(&forced)
    );

    let _ = fs::remove_dir_all(&project);
}

#[test]
fn source_change_invalidates_cache() {
    let project = scaffold_project("source_change");

    let first = typr(&project, &["build"]);
    assert!(first.status.success(), "first build failed: {:?}", first);

    // Touching a module file must invalidate the whole-project skip and
    // propagate to the generated R.
    fs::write(
        project.join("TypR/helper.ty"),
        "@pub let add_one <- fn(a: int): int { a + 2 };\n",
    )
    .unwrap();

    let second = typr(&project, &["build"]);
    assert!(second.status.success(), "rebuild failed: {:?}", second);
    assert!(
        !stdout(&second).contains("Project up to date"),
        "a changed module must trigger a rebuild"
    );
    let helper_r = fs::read_to_string(project.join("R/helper.R")).unwrap();
    assert!(
        helper_r.contains("+ 2"),
        "rebuilt R/helper.R should reflect the new source, got: {}",
        helper_r
    );

    let _ = fs::remove_dir_all(&project);
}

#[test]
fn tampered_output_invalidates_cache() {
    let project = scaffold_project("output_tamper");

    let first = typr(&project, &["build"]);
    assert!(first.status.success(), "first build failed: {:?}", first);
    let original = fs::read_to_string(project.join("R/helper.R")).unwrap();

    fs::write(project.join("R/helper.R"), "# tampered\n").unwrap();

    let second = typr(&project, &["build"]);
    assert!(second.status.success(), "rebuild failed: {:?}", second);
    assert!(
        !stdout(&second).contains("Project up to date"),
        "a modified output must trigger a rebuild"
    );
    assert_eq!(
        fs::read_to_string(project.join("R/helper.R")).unwrap(),
        original,
        "the rebuild should restore the generated file"
    );

    let _ = fs::remove_dir_all(&project);
}

#[test]
fn clean_removes_cache_dir() {
    let project = scaffold_project("clean_cache");

    let first = typr(&project, &["build"]);
    assert!(first.status.success(), "first build failed: {:?}", first);
    assert!(project.join(".typr_cache").exists());

    let cleaned = typr(&project, &["clean"]);
    assert!(cleaned.status.success(), "clean failed: {:?}", cleaned);
    assert!(
        !project.join(".typr_cache").exists(),
        "typr clean must remove the incremental cache"
    );

    let _ = fs::remove_dir_all(&project);
}
