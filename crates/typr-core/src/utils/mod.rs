//! Pure utility modules for TypR core
//!
//! These utilities have no system dependencies and are WASM-compatible.

pub mod builder;
pub mod fluent_parser;
pub mod path;
pub mod standard_library;

// Re-export commonly used items
pub use builder::*;
