//! `typr new --no-uvr` scaffolds a package whose ignore files know about uvr and not renv.

use std::fs;
use std::process::Command;

#[test]
fn new_project_template_targets_uvr_not_renv() {
    let root = std::env::temp_dir().join(format!("typr_new_{}", std::process::id()));
    let _ = fs::remove_dir_all(&root);
    fs::create_dir_all(&root).unwrap();

    let out = Command::new(env!("CARGO_BIN_EXE_typr"))
        .args(["new", "demo", "--no-uvr"])
        .current_dir(&root)
        .output()
        .unwrap();
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));

    let proj = root.join("demo");
    let rbuildignore = fs::read_to_string(proj.join(".Rbuildignore")).unwrap();
    for pat in [r"^uvr\.toml$", r"^uvr\.lock$", r"^\.uvr$", r"^\.Rprofile$"] {
        assert!(rbuildignore.contains(pat), "missing {pat} in .Rbuildignore");
    }
    let gitignore = fs::read_to_string(proj.join(".gitignore")).unwrap();
    assert!(gitignore.lines().any(|l| l == ".uvr/"));
    for f in [".Rbuildignore", ".gitignore", "README.md"] {
        let text = fs::read_to_string(proj.join(f)).unwrap();
        assert!(!text.to_lowercase().contains("renv"), "renv mentioned in {f}");
    }
    assert!(!proj.join("renv.lock").exists() && !proj.join("renv").exists());

    let _ = fs::remove_dir_all(&root);
}
