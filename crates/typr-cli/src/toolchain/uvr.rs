//! Which R library a `typr` subprocess runs against.
//! Package installation through [uvr](https://github.com/nbafrank/uvr). Every
//! installation goes through uvr — never a silent `install.packages`.

use std::process::Command;

use super::cmd::Cmd;

pub const UVR_MANIFEST: &str = "uvr.toml";
pub const UVR_LIBRARY: &str = ".uvr/library";
pub const UVR_INSTALL_HINT: &str =
    "curl -fsSL https://raw.githubusercontent.com/nbafrank/uvr/main/install.sh | sh";


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
    use std::ffi::OsString;

    fn strs(args: &[OsString]) -> Vec<String> {
        args.iter().map(|a| a.to_string_lossy().into_owned()).collect()
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

}
