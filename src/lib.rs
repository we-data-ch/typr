//! # TypR
//!
//! A typed superset of R with static type checking and transpilation to R.
//!
//! This crate re-exports everything from `typr-cli` which provides
//! the CLI interface, REPL, and LSP server.
//!
//! For core types and compilation API, see `typr-core`.

// Re-export everything from typr-cli
pub use typr_cli::*;
