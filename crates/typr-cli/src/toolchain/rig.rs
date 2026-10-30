//! Finding R, and saying so when it is not there. R itself is provisioned by
//! [rig](https://rig.r-lib.org/) (see `rfcs/0000-rig-r-provisioning.md`); this
//! file holds the detection half. Nothing here installs anything.

use super::cmd::Cmd;
use std::fmt;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Where to send a user whose machine has no R.
pub const R_INSTALL_HINT: &str =
    "run `typr setup-r` (installs R with rig, no administrator rights), or install R yourself: https://cran.r-project.org";

/// `rig list --user --json`: the R versions rig manages, as
/// `[{name, default, version, aliases, path, binary}]`. `[]` when none.
pub fn list_installed() -> Cmd {
    Cmd::new("rig").arg("list").arg("--user").arg("--json")
}

/// `rig add <version> --user --json`: install an R version without administrator
/// rights. `--user` is passed on every call so `typr` never depends on the
/// user's rig config (rig's default mode is `admin`). `without_fonts` skips the
/// fallback-font download, for `--yes`/CI runs. `version` is any rig spec:
/// `release`, `oldrel`, `4.4`, `4.4.3`, …
pub fn add(version: &str, without_fonts: bool) -> Cmd {
    let cmd = Cmd::new("rig").arg("add").arg(version).arg("--user").arg("--json");
    if without_fonts {
        cmd.arg("--without-fonts")
    } else {
        cmd
    }
}

/// `rig default <version> --user`. Only for an explicit `--version` on an R that
/// is already installed: `rig add` already makes the *first* R the default and
/// leaves an existing default alone.
pub fn default(version: &str) -> Cmd {
    Cmd::new("rig").arg("default").arg(version).arg("--user")
}

/// `rig --version` succeeds, i.e. rig is installed and on the `PATH`.
pub fn rig_available() -> bool {
    Command::new("rig")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// An R version, `major.minor.patch` (a missing patch reads as 0).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version(pub u32, pub u32, pub u32);

impl Version {
    pub fn parse(s: &str) -> Option<Version> {
        let mut parts = s.trim().split('.');
        let major = parts.next()?.parse().ok()?;
        let minor = parts.next().map_or(Some(0), |p| p.parse().ok())?;
        let patch = parts.next().map_or(Some(0), |p| p.parse().ok())?;
        if parts.next().is_some() {
            return None;
        }
        Some(Version(major, minor, patch))
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.0, self.1, self.2)
    }
}

/// A constraint on the R version, as written in `uvr.toml`'s `r_version`:
/// `>=4.3`, `>4.2.1`, `<=4.4`, `<5`, `=4.3.1`, or a bare `4.3` (meaning `=`).
/// A bare or `=` version with fewer than three parts matches every patch of it:
/// `4.3` accepts 4.3.0 … 4.3.9, like rig's own `x.y` spec.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VersionReq {
    op: Op,
    version: Version,
    parts: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Ge,
    Gt,
    Le,
    Lt,
    Eq,
}

impl VersionReq {
    pub fn parse(s: &str) -> Option<VersionReq> {
        let s = s.trim();
        let (op, rest) = [
            (">=", Op::Ge),
            ("<=", Op::Le),
            (">", Op::Gt),
            ("<", Op::Lt),
            ("=", Op::Eq),
        ]
        .into_iter()
        .find_map(|(sym, op)| s.strip_prefix(sym).map(|r| (op, r)))
        .unwrap_or((Op::Eq, s));
        let rest = rest.trim();
        Some(VersionReq {
            op,
            version: Version::parse(rest)?,
            parts: rest.split('.').count(),
        })
    }

    pub fn matches(&self, found: Version) -> bool {
        match self.op {
            Op::Ge => found >= self.version,
            Op::Gt => found > self.version,
            Op::Le => found <= self.version,
            Op::Lt => found < self.version,
            Op::Eq => match self.parts {
                1 => found.0 == self.version.0,
                2 => (found.0, found.1) == (self.version.0, self.version.1),
                _ => found == self.version,
            },
        }
    }
}

impl fmt::Display for VersionReq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self.op {
            Op::Ge => ">=",
            Op::Gt => ">",
            Op::Le => "<=",
            Op::Lt => "<",
            Op::Eq => "=",
        };
        let v = self.version;
        match self.parts {
            1 => write!(f, "{op}{}", v.0),
            2 => write!(f, "{op}{}.{}", v.0, v.1),
            _ => write!(f, "{op}{v}"),
        }
    }
}

/// The version out of `R --version`'s first line: `R version 4.4.3 (2025-02-28) -- "…"`.
pub fn parse_r_version(output: &str) -> Option<Version> {
    let rest = output.lines().find_map(|l| l.trim().strip_prefix("R version "))?;
    Version::parse(rest.split_whitespace().next()?)
}

/// An R found on this machine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FoundR {
    pub path: PathBuf,
    pub version: Version,
    /// `false` when it was found by rig's install layout rather than the `PATH`.
    pub on_path: bool,
}

fn probe(program: &Path) -> Option<Version> {
    let out = Command::new(program).arg("--version").output().ok()?;
    if !out.status.success() {
        return None;
    }
    // R prints its banner on stdout; some builds use stderr.
    parse_r_version(&String::from_utf8_lossy(&out.stdout))
        .or_else(|| parse_r_version(&String::from_utf8_lossy(&out.stderr)))
}

/// `R` binaries rig leaves in user mode, outside the `PATH` until the profile
/// is re-read: the quick link first, then each installed version, newest last.
fn rig_candidates(home: &Path) -> Vec<PathBuf> {
    let mut out = vec![home.join(".local/bin/R")];
    if let Ok(entries) = std::fs::read_dir(home.join(".local/share/rig/r")) {
        let mut dirs: Vec<PathBuf> = entries.flatten().map(|e| e.path()).collect();
        dirs.sort();
        out.extend(dirs.into_iter().rev().map(|d| d.join("bin/R")));
    }
    out
}

/// R on the `PATH`, else rig's user-mode layout. `None` when neither has one.
pub fn find_r() -> Option<FoundR> {
    if let Some(version) = probe(Path::new("R")) {
        return Some(FoundR {
            path: PathBuf::from("R"),
            version,
            on_path: true,
        });
    }
    let home = std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE"))?;
    rig_candidates(Path::new(&home)).into_iter().find_map(|path| {
        let version = probe(&path)?;
        Some(FoundR {
            path,
            version,
            on_path: false,
        })
    })
}

#[allow(dead_code)]
pub fn r_available() -> bool {
    find_r().is_some()
}

/// Message for a command that needs R when none is found.
pub fn r_missing_message() -> String {
    format!("R is not installed (or not on the PATH).\n  fix: {R_INSTALL_HINT}")
}

/// Message for a project whose `r_version` the R found does not satisfy.
pub fn r_too_old_message(req: &VersionReq, found: &FoundR) -> String {
    format!(
        "this project needs R {req}, found R {} at {}.\n  fix: {R_INSTALL_HINT}",
        found.version,
        found.path.display()
    )
}

/// `Ok` when an R satisfying `req` (if any) can be run. Never falls back to
/// anything and never installs. An R found only through rig's layout has its
/// directory prepended to this process's `PATH`, so the `Rscript` subprocesses
/// spawned later by plain name find it too; the user's shell is untouched.
pub fn require_r(req: Option<&VersionReq>) -> Result<(), String> {
    let found = find_r().ok_or_else(r_missing_message)?;
    if let Some(req) = req {
        if !req.matches(found.version) {
            return Err(r_too_old_message(req, &found));
        }
    }
    if !found.on_path {
        if let Some(dir) = found.path.parent() {
            let mut dirs = vec![dir.to_path_buf()];
            dirs.extend(std::env::var_os("PATH").iter().flat_map(std::env::split_paths));
            if let Ok(joined) = std::env::join_paths(dirs) {
                std::env::set_var("PATH", joined);
            }
        }
    }
    Ok(())
}

/// `[project] r_version` of a `uvr.toml`; `None` when absent or not a constraint
/// we understand (uvr owns the manifest's validity, so we stay silent).
pub fn manifest_r_requirement(manifest: &str) -> Option<VersionReq> {
    let value: toml::Value = manifest.parse().ok()?;
    VersionReq::parse(value.get("project")?.get("r_version")?.as_str()?)
}

/// The R constraint of the project containing `dir`, if its `uvr.toml` sets one.
pub fn project_r_requirement(dir: &Path) -> Option<VersionReq> {
    let root = super::env::project_env(dir).root()?.to_path_buf();
    let manifest = std::fs::read_to_string(root.join(super::uvr::UVR_MANIFEST)).ok()?;
    manifest_r_requirement(&manifest)
}

/// For commands that do not need R today (`check`, `build`, `run`): a no-op
/// unless the project pins `r_version`, in which case R must exist and satisfy it.
pub fn require_project_r(dir: &Path) -> Result<(), String> {
    match project_r_requirement(dir) {
        Some(req) => require_r(Some(&req)),
        None => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn req(s: &str) -> VersionReq {
        VersionReq::parse(s).unwrap()
    }

    fn strs(cmd: &Cmd) -> Vec<String> {
        cmd.args.iter().map(|a| a.to_string_lossy().into_owned()).collect()
    }

    #[test]
    fn rig_builders_always_ask_for_user_mode() {
        let list = list_installed();
        assert_eq!(list.program, "rig");
        assert_eq!(strs(&list), ["list", "--user", "--json"]);
        assert_eq!(strs(&add("4.4", false)), ["add", "4.4", "--user", "--json"]);
        assert_eq!(
            strs(&add("release", true)),
            ["add", "release", "--user", "--json", "--without-fonts"]
        );
        assert_eq!(strs(&default("4.3.3")), ["default", "4.3.3", "--user"]);
        for cmd in [list_installed(), add("4.4", false), default("4.4")] {
            assert!(strs(&cmd).contains(&"--user".to_string()), "{cmd:?}");
            assert!(cmd.envs.is_empty());
        }
    }

    #[test]
    fn parses_versions() {
        assert_eq!(Version::parse("4.4.3"), Some(Version(4, 4, 3)));
        assert_eq!(Version::parse("4.3"), Some(Version(4, 3, 0)));
        assert_eq!(Version::parse("4"), Some(Version(4, 0, 0)));
        assert_eq!(Version::parse("4.x"), None);
        assert_eq!(Version::parse("4.3.1.2"), None);
        assert_eq!(Version::parse(""), None);
    }

    #[test]
    fn parses_the_r_banner() {
        let banner = "R version 4.4.3 (2025-02-28) -- \"Trophy Case\"\nCopyright (C) 2025\n";
        assert_eq!(parse_r_version(banner), Some(Version(4, 4, 3)));
        assert_eq!(parse_r_version("R scripting front-end version 4.4.3"), None);
        assert_eq!(parse_r_version("bash: R: command not found"), None);
    }

    #[test]
    fn comparison_constraints() {
        assert!(req(">=4.3").matches(Version(4, 3, 0)));
        assert!(req(">=4.3").matches(Version(4, 5, 1)));
        assert!(!req(">=4.3").matches(Version(4, 1, 2)));
        assert!(!req(">4.3.1").matches(Version(4, 3, 1)));
        assert!(req(">4.3.1").matches(Version(4, 3, 2)));
        assert!(req("<5").matches(Version(4, 9, 9)));
        assert!(!req("<5").matches(Version(5, 0, 0)));
        assert!(req("<=4.4").matches(Version(4, 4, 0)));
        assert!(!req("<=4.4").matches(Version(4, 4, 1)));
    }

    #[test]
    fn bare_and_equal_versions_match_every_patch() {
        assert!(req("4.3").matches(Version(4, 3, 9)));
        assert!(!req("4.3").matches(Version(4, 4, 0)));
        assert!(req("=4.3").matches(Version(4, 3, 0)));
        assert!(req("4").matches(Version(4, 0, 1)));
        assert!(req("4.3.1").matches(Version(4, 3, 1)));
        assert!(!req("4.3.1").matches(Version(4, 3, 2)));
    }

    #[test]
    fn reads_r_version_from_the_manifest() {
        let m = "[project]\nname = \"demo\"\nr_version = \">=4.3\"\n";
        assert_eq!(manifest_r_requirement(m), Some(req(">=4.3")));
        assert_eq!(manifest_r_requirement("[project]\nname = \"demo\"\n"), None);
        assert_eq!(manifest_r_requirement("[project]\nr_version = \"latest\"\n"), None);
        assert_eq!(manifest_r_requirement("not toml ["), None);
    }

    #[test]
    fn a_project_without_a_pin_never_needs_r() {
        let root = std::env::temp_dir().join(format!("typr-rpin-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();
        std::fs::write(root.join("uvr.toml"), "[project]\nname = \"x\"\n").unwrap();
        assert_eq!(project_r_requirement(&root), None);
        assert!(require_project_r(&root).is_ok());
        std::fs::write(root.join("uvr.toml"), "[project]\nr_version = \"<3\"\n").unwrap();
        assert_eq!(project_r_requirement(&root), Some(req("<3")));
        let _ = std::fs::remove_dir_all(&root);
    }

    #[test]
    fn rejects_malformed_constraints() {
        assert_eq!(VersionReq::parse(">="), None);
        assert_eq!(VersionReq::parse("~4.3"), None);
        assert_eq!(VersionReq::parse("latest"), None);
    }

    #[test]
    fn constraint_displays_as_written() {
        assert_eq!(req(">=4.3").to_string(), ">=4.3");
        assert_eq!(req("4.3.1").to_string(), "=4.3.1");
        assert_eq!(req("<5").to_string(), "<5");
    }

    #[test]
    fn messages_say_what_to_do() {
        let missing = r_missing_message();
        assert!(missing.starts_with("R is not installed"));
        assert!(missing.contains("typr setup-r"));

        let found = FoundR {
            path: PathBuf::from("/usr/bin/R"),
            version: Version(4, 1, 2),
            on_path: true,
        };
        let old = r_too_old_message(&req(">=4.3"), &found);
        assert!(old.contains("needs R >=4.3, found R 4.1.2 at /usr/bin/R"), "{old}");
    }

    #[test]
    fn rig_layout_lists_quick_link_then_newest_version() {
        let home = std::env::temp_dir().join(format!("typr-rig-layout-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&home);
        for v in ["4.3.3", "4.4.3"] {
            std::fs::create_dir_all(home.join(".local/share/rig/r").join(v)).unwrap();
        }
        let c = rig_candidates(&home);
        assert_eq!(c[0], home.join(".local/bin/R"));
        assert_eq!(c[1], home.join(".local/share/rig/r/4.4.3/bin/R"));
        assert_eq!(c[2], home.join(".local/share/rig/r/4.3.3/bin/R"));
        let _ = std::fs::remove_dir_all(&home);
    }
}
