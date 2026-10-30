//! `typr setup-r`: the one place `typr` installs anything on the machine, and
//! only when the user typed the command (see `rfcs/0000-rig-r-provisioning.md`).
//!
//! It fetches the [rig](https://rig.r-lib.org/) binary when absent (checksum
//! verified against the `digest` GitHub publishes for each release asset), then
//! asks rig to install R in user mode, so no administrator rights are needed.
//! Downloading and unpacking shell out to `curl` and `tar`, as `uvr`'s install
//! hint already does; the pure parts (asset naming, release/`rig list` parsing,
//! digest check) are tested without a network.

use super::rig::{self, Version, VersionReq};
use super::Cmd;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::io::{BufRead, IsTerminal, Write};
use std::path::{Path, PathBuf};
use std::process::Stdio;

const RELEASE_API: &str = "https://api.github.com/repos/r-lib/rig/releases/latest";

/// Flags of `typr setup-r`.
pub struct Options {
    /// A rig version spec (`release`, `oldrel`, `4.4`, `4.4.3`); `None` = whatever R is there, else `release`.
    pub version: Option<String>,
    /// Skip the confirmation prompt (CI).
    pub yes: bool,
    /// Report only; install nothing.
    pub check: bool,
}

/// The rig release asset for a platform, as `std::env::consts` names it.
/// `None` when rig ships no archive we can unpack (Windows is handled by the
/// installer, not here).
pub fn rig_asset(os: &str, arch: &str, version: &str) -> Option<String> {
    let (platform, arch) = match (os, arch) {
        ("linux", "x86_64") => ("linux", "x86_64"),
        ("linux", "aarch64") => ("linux", "aarch64"),
        ("macos", "x86_64") => ("macos", "x86_64"),
        ("macos", "aarch64") => ("macos", "arm64"),
        _ => return None,
    };
    Some(format!("rig-{platform}-{arch}-{version}.tar.gz"))
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct Asset {
    pub name: String,
    #[serde(rename = "browser_download_url")]
    pub url: String,
    pub digest: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Release {
    tag_name: String,
    assets: Vec<Asset>,
}

/// `(version, asset)` for `asset_name_of(version)` out of the GitHub release JSON.
fn pick_asset(
    json: &str,
    asset_name_of: impl Fn(&str) -> Option<String>,
) -> Result<(String, Asset), String> {
    let release: Release =
        serde_json::from_str(json).map_err(|e| format!("unreadable rig release data: {e}"))?;
    let version = release.tag_name.trim_start_matches('v').to_string();
    let name = asset_name_of(&version).ok_or_else(|| {
        format!(
            "there is no rig build for {}/{}.\n  Install R by your distribution's package manager, then re-run.",
            std::env::consts::OS,
            std::env::consts::ARCH
        )
    })?;
    let asset = release
        .assets
        .into_iter()
        .find(|a| a.name == name)
        .ok_or_else(|| format!("rig {version} has no asset named {name}"))?;
    Ok((version, asset))
}

/// Does the file's SHA-256 equal GitHub's `sha256:<hex>` digest?
pub fn digest_matches(file_hex: &str, digest: &str) -> bool {
    digest.strip_prefix("sha256:").is_some_and(|d| d.eq_ignore_ascii_case(file_hex))
}

fn sha256_file(path: &Path) -> std::io::Result<String> {
    let mut hasher = Sha256::new();
    std::io::copy(&mut std::fs::File::open(path)?, &mut hasher)?;
    Ok(hasher.finalize().iter().map(|b| format!("{b:02x}")).collect())
}

/// One entry of `rig list --json`.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct InstalledR {
    pub version: String,
    #[serde(default)]
    pub default: bool,
    #[serde(default)]
    pub binary: Option<String>,
}

pub fn parse_rig_list(json: &str) -> Result<Vec<InstalledR>, String> {
    serde_json::from_str(json).map_err(|e| format!("unreadable `rig list` output: {e}"))
}

/// The installed R a `--version` spec names, if any: `4.4` and `4.4.3` match by
/// [`VersionReq`] (so `4.4` accepts every patch), the newest first. Aliases
/// such as `release` cannot be matched offline and return `None`.
pub fn installed_matching<'a>(list: &'a [InstalledR], spec: &str) -> Option<&'a InstalledR> {
    let req = VersionReq::parse(spec)?;
    list.iter()
        .filter_map(|r| Some((Version::parse(&r.version)?, r)))
        .filter(|(v, _)| req.matches(*v))
        .max_by_key(|(v, _)| *v)
        .map(|(_, r)| r)
}

fn home() -> Option<PathBuf> {
    std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE")).map(PathBuf::from)
}

/// rig on the `PATH`, else the `~/.local/bin/rig` this command installs.
fn rig_program() -> Option<String> {
    if rig::rig_available() {
        return Some("rig".to_string());
    }
    let path = home()?.join(".local/bin/rig");
    let ok = std::process::Command::new(&path)
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    ok.then(|| path.to_string_lossy().into_owned())
}

fn with_program(mut cmd: Cmd, program: &str) -> Cmd {
    cmd.program = program.to_string();
    cmd
}

fn confirm(question: &str, yes: bool) -> Result<bool, String> {
    if yes {
        return Ok(true);
    }
    if !std::io::stdin().is_terminal() {
        return Err("refusing to install without asking: run in a terminal, or pass --yes.".to_string());
    }
    print!("{question} [y/N] ");
    std::io::stdout().flush().ok();
    let mut line = String::new();
    std::io::stdin().lock().read_line(&mut line).map_err(|e| e.to_string())?;
    Ok(matches!(line.trim().to_ascii_lowercase().as_str(), "y" | "yes"))
}

fn curl(args: &[&str]) -> Result<std::process::Output, String> {
    std::process::Command::new("curl")
        .args(["--fail", "--silent", "--show-error", "--location"])
        .args(args)
        .output()
        .map_err(|e| format!("could not run curl ({e}); install curl, or install rig yourself: https://rig.r-lib.org/"))
        .and_then(|o| {
            if o.status.success() {
                Ok(o)
            } else {
                Err(format!("download failed: {}", String::from_utf8_lossy(&o.stderr).trim()))
            }
        })
}

/// Download rig into `~/.local` (binary in `bin/`), verified against GitHub's digest.
fn install_rig(asset: &Asset, prefix: &Path) -> Result<(), String> {
    let digest = asset
        .digest
        .as_deref()
        .ok_or_else(|| format!("GitHub publishes no checksum for {}; not installing it.", asset.name))?;
    let dir = std::env::temp_dir().join(format!("typr-setup-r-{}", std::process::id()));
    std::fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    let result = (|| {
        let archive = dir.join(&asset.name);
        curl(&["--output", &archive.to_string_lossy(), &asset.url])?;
        let got = sha256_file(&archive).map_err(|e| e.to_string())?;
        if !digest_matches(&got, digest) {
            return Err(format!("checksum mismatch for {} (expected {digest}, got sha256:{got}); not installing it.", asset.name));
        }
        std::fs::create_dir_all(prefix).map_err(|e| e.to_string())?;
        let status = std::process::Command::new("tar")
            .arg("-xzf")
            .arg(&archive)
            .arg("-C")
            .arg(prefix)
            .status()
            .map_err(|e| format!("could not run tar: {e}"))?;
        status.success().then_some(()).ok_or_else(|| "could not unpack the rig archive".to_string())
    })();
    let _ = std::fs::remove_dir_all(&dir);
    result
}

fn print_found(found: &rig::FoundR) {
    println!("R {} at {}", found.version, found.path.display());
}

/// Run `typr setup-r`. Returns the process exit code: 0 R available, 1 failed or declined.
pub fn setup_r(opts: &Options) -> i32 {
    match run(opts) {
        Ok(()) => 0,
        Err(msg) => {
            eprintln!("error: {msg}");
            1
        }
    }
}

fn run(opts: &Options) -> Result<(), String> {
    let req = opts.version.as_deref().and_then(VersionReq::parse);
    let found = rig::find_r();

    // 1. Nothing to do when the R that is there is what was asked for.
    if let Some(f) = &found {
        if req.as_ref().map_or(opts.version.is_none(), |r| r.matches(f.version)) {
            print_found(f);
            return Ok(());
        }
    }
    if opts.check {
        return Err(match (&found, &opts.version) {
            (Some(f), Some(spec)) => format!("R {} at {} does not match {spec}.", f.version, f.path.display()),
            _ => rig::r_missing_message(),
        });
    }

    let spec = opts.version.as_deref().unwrap_or("release");
    let had_r = found.is_some();

    // 2. rig, installed only after asking.
    let rig = match rig_program() {
        Some(p) => p,
        None => {
            let target = home().ok_or("cannot find your home directory")?.join(".local");
            let json = String::from_utf8_lossy(&curl(&["--header", "Accept: application/vnd.github+json", RELEASE_API])?.stdout).into_owned();
            let (version, asset) =
                pick_asset(&json, |v| rig_asset(std::env::consts::OS, std::env::consts::ARCH, v))?;
            println!("rig is not installed. TypR can install it for you:");
            println!("  {}   (sha256 checked against GitHub's published digest)", asset.url);
            if !confirm(
                &format!("Install rig {version} into {} and R ({spec}) in your home directory, no administrator rights?", target.join("bin").display()),
                opts.yes,
            )? {
                return Err("declined; nothing was installed.".to_string());
            }
            if !had_r {
                println!("note: rig adds `. \"$HOME/.local/bin/rigenv\"` to ~/.profile on its first install and makes that R the default.");
            }
            install_rig(&asset, &target)?;
            target.join("bin/rig").to_string_lossy().into_owned()
        }
    };

    // 3. R, through rig, in user mode.
    let list_out = with_program(rig::list_installed(), &rig)
        .command()
        .stderr(Stdio::inherit())
        .output()
        .map_err(|e| format!("could not run rig: {e}"))?;
    let installed = parse_rig_list(&String::from_utf8_lossy(&list_out.stdout)).unwrap_or_default();

    if let Some(r) = installed_matching(&installed, spec) {
        // Already installed: only switch the default, and only because a version was asked for.
        if !r.default {
            println!("R {} is installed but not the default; making it the default.", r.version);
            let status = with_program(rig::default(&r.version), &rig)
                .command()
                .status()
                .map_err(|e| format!("could not run rig: {e}"))?;
            if !status.success() {
                return Err(format!("`rig default {}` failed", r.version));
            }
        }
    } else {
        let status = with_program(rig::add(spec, opts.yes), &rig)
            .command()
            .stdout(Stdio::null()) // the JSON describing the install; the user sees rig's log on stderr
            .status()
            .map_err(|e| format!("could not run rig: {e}"))?;
        if !status.success() {
            return Err(format!("`rig add {spec}` failed"));
        }
    }

    // 4. Say which R will be used.
    let now = rig::find_r().ok_or("R was installed but cannot be found; open a new shell and retry.")?;
    print!("R is ready. `typr` will use: ");
    print_found(&now);
    if let (Some(r), Some(spec)) = (&req, &opts.version) {
        if !r.matches(now.version) {
            println!("note: R {spec} is installed, but R {} stays the default. Run `typr setup-r --version {spec}` again to switch.", now.version);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn asset_names_follow_rigs_releases() {
        assert_eq!(rig_asset("linux", "x86_64", "0.11.0").unwrap(), "rig-linux-x86_64-0.11.0.tar.gz");
        assert_eq!(rig_asset("linux", "aarch64", "0.11.0").unwrap(), "rig-linux-aarch64-0.11.0.tar.gz");
        assert_eq!(rig_asset("macos", "aarch64", "0.11.0").unwrap(), "rig-macos-arm64-0.11.0.tar.gz");
        assert_eq!(rig_asset("macos", "x86_64", "0.11.0").unwrap(), "rig-macos-x86_64-0.11.0.tar.gz");
        assert_eq!(rig_asset("linux", "riscv64", "0.11.0"), None);
        assert_eq!(rig_asset("windows", "x86_64", "0.11.0"), None);
    }

    const RELEASE: &str = r#"{"tag_name":"v0.11.0","assets":[
        {"name":"rig-linux-x86_64-0.11.0.tar.gz","browser_download_url":"https://example/rig.tgz","digest":"sha256:ABCD"},
        {"name":"rig-linux-aarch64-0.11.0.tar.gz","browser_download_url":"https://example/arm.tgz","digest":null}]}"#;

    #[test]
    fn picks_the_asset_and_strips_the_tag_prefix() {
        let (v, a) = pick_asset(RELEASE, |v| rig_asset("linux", "x86_64", v)).unwrap();
        assert_eq!(v, "0.11.0");
        assert_eq!(a.url, "https://example/rig.tgz");
        assert_eq!(a.digest.as_deref(), Some("sha256:ABCD"));
        assert!(pick_asset(RELEASE, |_| None).unwrap_err().contains("no rig build"));
        assert!(pick_asset(RELEASE, |v| rig_asset("macos", "x86_64", v)).unwrap_err().contains("no asset named"));
    }

    #[test]
    fn digest_comparison_ignores_case_and_requires_sha256() {
        assert!(digest_matches("abcd", "sha256:ABCD"));
        assert!(!digest_matches("abcd", "sha256:abce"));
        assert!(!digest_matches("abcd", "md5:abcd"));
        assert!(!digest_matches("abcd", "abcd"));
    }

    #[test]
    fn sha256_of_a_file() {
        let p = std::env::temp_dir().join(format!("typr-sha-{}", std::process::id()));
        std::fs::write(&p, "abc").unwrap();
        let got = sha256_file(&p).unwrap();
        let _ = std::fs::remove_file(&p);
        assert_eq!(got, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    }

    const LIST: &str = r#"[
        {"name":"4.3","default":false,"version":"4.3.3","aliases":[],"path":"/r/4.3.3","binary":"/r/4.3.3/bin/R"},
        {"name":"4.4","default":true,"version":"4.4.3","aliases":["release"],"path":"/r/4.4.3","binary":"/r/4.4.3/bin/R"}]"#;

    #[test]
    fn parses_rig_list_and_matches_a_spec() {
        let list = parse_rig_list(LIST).unwrap();
        assert_eq!(list.len(), 2);
        assert!(list[1].default);
        assert_eq!(installed_matching(&list, "4.3").unwrap().version, "4.3.3");
        assert_eq!(installed_matching(&list, "4.4.3").unwrap().version, "4.4.3");
        assert!(installed_matching(&list, "4.4.1").is_none());
        assert!(installed_matching(&list, "release").is_none());
        assert_eq!(parse_rig_list("[]").unwrap(), vec![]);
        assert!(parse_rig_list("not json").is_err());
    }
}
