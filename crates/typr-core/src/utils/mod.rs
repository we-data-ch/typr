//! Pure utility modules for TypR core
//!
//! These utilities have no system dependencies and are WASM-compatible.

pub mod builder;
pub mod fluent_parser;
#[cfg(feature = "fuzz-gen")]
pub mod program_gen;
pub mod standard_library;

// Re-export commonly used items
pub use builder::*;
