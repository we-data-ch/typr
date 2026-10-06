//! TypR main executable
//!
//! This is a thin wrapper around typr-cli.
//! All CLI functionality is provided by the typr-cli crate.

fn main() {
    // `start` start the cli with `typr ...`
    typr_cli::start()
}
