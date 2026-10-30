//! The external tools `typr` drives: which R library a subprocess runs against
//! ([`env`]), the package manager that installs into it ([`uvr`]), and finding
//! R itself ([`rig`]).
//!
//! Every builder here is pure: it returns a [`Cmd`] (program, argv, env) that
//! tests can inspect without R or uvr on the machine. [`Cmd::command`] turns one
//! into a [`std::process::Command`].
//!
//! Subprocesses that deliberately interrogate the *system* R (`gen-types`, the
//! R name cache, registry validation, `typr fuzz`'s availability probe) stay on
//! plain `Command::new("Rscript")`: they describe the installed CRAN world, not a
//! project.

pub mod cmd;
pub mod env;
pub mod rig;
pub mod setup;
pub mod uvr;

pub use cmd::Cmd;
#[allow(unused_imports)]
pub use env::{current_env, project_env, REnv};
#[allow(unused_imports)] // API for later steps
pub use rig::{
    add as rig_add, default as rig_default, find_r, list_installed, r_available, r_missing_message, require_project_r,
    require_r, rig_available, FoundR, Version, VersionReq,
};
#[allow(unused_imports)] // `remove_packages` and the constants are API, not all called yet
pub use uvr::{
    add_packages, init, remove_packages, require_uvr, sync, uvr_available, uvr_missing_message, UVR_INSTALL_HINT,
    UVR_LIBRARY, UVR_MANIFEST,
};
