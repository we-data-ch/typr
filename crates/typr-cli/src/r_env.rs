//! Which R library a `typr` subprocess runs against.
//!
//! A TypR project can carry a [uvr](https://github.com/nbafrank/uvr) environment
//! (`uvr.toml` + `.uvr/library/`). Every R subprocess spawned on behalf of a
//! project must see that library, and every package installation must go through
//! uvr — never a silent `install.packages`.
//!
//! uvr has no `run -e`, so we do not go through `uvr run` to *execute* R: we
//! launch `R`/`Rscript` directly with `R_LIBS=<root>/.uvr/library` in the
//! subprocess environment. uvr is only needed to *install* (`uvr add`, `uvr sync`).
//!
//! The builders below are pure: they return a [`Cmd`] (program, argv, env) that
//! tests can inspect without R or uvr on the machine. [`Cmd::command`] turns one
//! into a [`std::process::Command`].
//!
//! Subprocesses that deliberately interrogate the *system* R (`gen-types`, the
//! R name cache, registry validation, `typr fuzz`'s availability probe) stay on
//! plain `Command::new("Rscript")`: they describe the installed CRAN world, not a
//! project.

use std::ffi::OsString;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

pub const UVR_MANIFEST: &str = "uvr.toml";
pub const UVR_LIBRARY: &str = ".uvr/library";
pub const UVR_INSTALL_HINT: &str =
    "curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | sh";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum REnv {
    /// No project environment: whatever R finds on its own.
    System,
    /// A uvr project rooted at `root` (absolute).
    Uvr { root: PathBuf },
}

/// A subprocess to spawn: program, argv, extra environment variables.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Cmd {
    pub program: String,
    pub args: Vec<OsString>,
    pub envs: Vec<(OsString, OsString)>,
}

impl Cmd {
    fn new(program: &str) -> Self {
        Cmd { program: program.to_string(), args: Vec::new(), envs: Vec::new() }
    }

    fn arg(mut self, arg: impl Into<OsString>) -> Self {
        self.args.push(arg.into());
        self
    }

    pub fn command(&self) -> Command {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);
        cmd.envs(self.envs.iter().map(|(k, v)| (k, v)));
        cmd
    }
}

/// The environment of the project containing `dir`: `Uvr` when a `uvr.toml`
/// sits in `dir` or one of its ancestors, `System` otherwise.
pub fn project_env(dir: &Path) -> REnv {
    let abs = dir
        .canonicalize()
        .or_else(|_| std::env::current_dir().map(|cwd| cwd.join(dir)))
        .unwrap_or_else(|_| dir.to_path_buf());
    abs.ancestors()
        .find(|d| d.join(UVR_MANIFEST).is_file())
        .map(|root| REnv::Uvr { root: root.to_path_buf() })
        .unwrap_or(REnv::System)
}

/// [`project_env`] of the current working directory.
pub fn current_env() -> REnv {
    project_env(Path::new("."))
}

impl REnv {
    pub fn root(&self) -> Option<&Path> {
        match self {
            REnv::Uvr { root } => Some(root),
            REnv::System => None,
        }
    }

    /// Directories holding the project's installed packages.
    pub fn lib_dirs(&self) -> Vec<PathBuf> {
        match self {
            REnv::Uvr { root } => vec![root.join(UVR_LIBRARY)],
            REnv::System => Vec::new(),
        }
    }

    fn with_libs(&self, cmd: Cmd) -> Cmd {
        self.with_libs_over(cmd, std::env::var_os("R_LIBS"))
    }

    /// `R_LIBS` = project library, then whatever the user already had.
    fn with_libs_over(&self, mut cmd: Cmd, existing: Option<OsString>) -> Cmd {
        let mut dirs = self.lib_dirs();
        if dirs.is_empty() {
            return cmd;
        }
        dirs.extend(existing.iter().flat_map(std::env::split_paths));
        if let Ok(joined) = std::env::join_paths(dirs) {
            cmd.envs.push(("R_LIBS".into(), joined));
        }
        cmd
    }

    /// `R -e <expr>` against this environment.
    pub fn r_eval(&self, expr: &str) -> Cmd {
        self.with_libs(Cmd::new("R").arg("-e").arg(expr))
    }

    /// `Rscript -e <expr>` against this environment.
    pub fn rscript_eval(&self, expr: &str) -> Cmd {
        self.with_libs(Cmd::new("Rscript").arg("-e").arg(expr))
    }

    /// `Rscript <script>` against this environment.
    pub fn rscript_file(&self, script: impl Into<OsString>) -> Cmd {
        self.with_libs(Cmd::new("Rscript").arg(script))
    }
}

/// `uvr add [--dev] --no-companion <pkgs…>`. The companion R package is never
/// wanted from typr (see uvr.md, step 0.7).
pub fn add_packages(pkgs: &[String], dev: bool) -> Cmd {
    let mut cmd = Cmd::new("uvr").arg("add");
    if dev {
        cmd = cmd.arg("--dev");
    }
    cmd = cmd.arg("--no-companion");
    pkgs.iter().fold(cmd, |c, p| c.arg(p))
}

/// `uvr remove --no-companion <pkgs…>`.
#[allow(dead_code)] // no `typr` command removes a dependency yet
pub fn remove_packages(pkgs: &[String]) -> Cmd {
    pkgs.iter().fold(Cmd::new("uvr").arg("remove").arg("--no-companion"), |c, p| c.arg(p))
}

/// `uvr sync`, with `--frozen` for CI.
pub fn sync(frozen: bool) -> Cmd {
    let cmd = Cmd::new("uvr").arg("sync");
    if frozen { cmd.arg("--frozen") } else { cmd }
}

/// `uvr init --here <name>` in an existing directory, without touching the
/// working tree beyond what the project needs when `unattended` is false.
pub fn init(name: &str, unattended: bool) -> Cmd {
    let cmd = Cmd::new("uvr").arg("init").arg("--here");
    let cmd = if unattended { cmd.arg("--unattended") } else { cmd };
    cmd.arg(name)
}

pub fn uvr_available() -> bool {
    Command::new("uvr")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Message for a command that needs uvr when it is not on the `PATH`.
pub fn uvr_missing_message() -> String {
    format!("uvr is not installed. Install it with:\n  {UVR_INSTALL_HINT}")
}

/// `Ok` when uvr can be run, the install hint otherwise. Never falls back to
/// `install.packages`.
pub fn require_uvr() -> Result<(), String> {
    if uvr_available() { Ok(()) } else { Err(uvr_missing_message()) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn tmp_dir(tag: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("typr_r_env_{}_{}", tag, std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir.canonicalize().unwrap()
    }

    fn strs(args: &[OsString]) -> Vec<String> {
        args.iter().map(|a| a.to_string_lossy().into_owned()).collect()
    }

    fn uvr_env() -> REnv {
        REnv::Uvr { root: PathBuf::from("/p") }
    }

    #[test]
    fn project_env_detects_uvr_toml_in_an_ancestor() {
        let root = tmp_dir("detect");
        fs::write(root.join(UVR_MANIFEST), "").unwrap();
        fs::create_dir_all(root.join("TypR/sub")).unwrap();
        assert_eq!(project_env(&root), REnv::Uvr { root: root.clone() });
        assert_eq!(project_env(&root.join("TypR/sub")), REnv::Uvr { root });
    }

    #[test]
    fn project_env_is_system_without_a_manifest() {
        assert_eq!(project_env(&tmp_dir("none")), REnv::System);
    }

    #[test]
    fn system_env_leaves_the_subprocess_alone() {
        let cmd = REnv::System.r_eval("1 + 1");
        assert_eq!(cmd.program, "R");
        assert_eq!(strs(&cmd.args), ["-e", "1 + 1"]);
        assert!(cmd.envs.is_empty());
    }

    #[test]
    fn uvr_env_points_r_libs_at_the_project_library() {
        let cmd = uvr_env().rscript_eval("x");
        assert_eq!(cmd.program, "Rscript");
        assert_eq!(strs(&cmd.args), ["-e", "x"]);
        assert_eq!(cmd.envs, vec![(OsString::from("R_LIBS"), OsString::from("/p/.uvr/library"))]);
    }

    #[test]
    fn existing_r_libs_stays_behind_the_project_library() {
        let env = uvr_env();
        let cmd = env.with_libs_over(Cmd::new("R"), Some(OsString::from("/home/u/lib")));
        let value = cmd.envs[0].1.clone();
        let dirs: Vec<PathBuf> = std::env::split_paths(&value).collect();
        assert_eq!(dirs, [PathBuf::from("/p/.uvr/library"), PathBuf::from("/home/u/lib")]);
    }

    #[test]
    fn rscript_file_passes_the_script_path() {
        let cmd = uvr_env().rscript_file("a.R");
        assert_eq!(strs(&cmd.args), ["a.R"]);
    }

    #[test]
    fn add_packages_never_installs_the_companion() {
        let pkgs = vec!["dplyr".to_string(), "ggplot2".to_string()];
        assert_eq!(strs(&add_packages(&pkgs, false).args), ["add", "--no-companion", "dplyr", "ggplot2"]);
        assert_eq!(strs(&add_packages(&pkgs[..1], true).args), ["add", "--dev", "--no-companion", "dplyr"]);
    }

    #[test]
    fn remove_sync_and_init_argv() {
        assert_eq!(strs(&remove_packages(&["x".to_string()]).args), ["remove", "--no-companion", "x"]);
        assert_eq!(strs(&sync(false).args), ["sync"]);
        assert_eq!(strs(&sync(true).args), ["sync", "--frozen"]);
        assert_eq!(strs(&init("demo", false).args), ["init", "--here", "demo"]);
        assert_eq!(strs(&init("demo", true).args), ["init", "--here", "--unattended", "demo"]);
    }

    #[test]
    fn lib_dirs_is_empty_outside_uvr() {
        assert!(REnv::System.lib_dirs().is_empty());
        assert_eq!(uvr_env().lib_dirs(), [PathBuf::from("/p/.uvr/library")]);
    }
}
