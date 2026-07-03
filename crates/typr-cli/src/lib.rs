//! # TypR CLI
//!
//! Command-line interface, REPL, and LSP server for TypR - a typed superset of R.
//!
//! This crate provides the CLI layer for TypR, depending on `typr-core` for
//! the core logic. It includes:
//!
//! - Command-line interface with project management commands
//! - Interactive REPL with syntax highlighting
//! - Language Server Protocol (LSP) server for IDE integration
//! - Filesystem-based source and output handlers
//!
//! ## Usage
//!
//! ```bash
//! # Create a new project
//! typr new myproject
//!
//! # Check types
//! typr check
//!
//! # Build to R
//! typr build
//!
//! # Run
//! typr run
//!
//! # Start REPL
//! typr repl
//!
//! # Start LSP server
//! typr lsp
//! ```

// Re-export typr-core for users who want access to core types
pub use typr_core;

// CLI modules
pub mod cache;
pub mod cases;
pub mod cli;
pub mod engine;
pub mod io;
pub mod lsp;
pub mod lsp_parser;
pub mod metaprogramming;
pub mod progress;
pub mod project;
pub mod rd_renderer;
pub mod repl;
pub mod standard_library;
pub mod vignette_renderer;

// Re-export commonly used items
pub use cli::start;

// Re-export typr-core abstractions
pub use typr_core::{Compiler, InMemorySourceProvider, SourceProvider, TranspileResult};
