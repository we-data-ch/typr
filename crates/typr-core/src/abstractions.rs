//! Abstraction traits for platform-independent operations
//!
//! These traits allow typr-core to work both natively (with filesystem access)
//! and in WebAssembly (with in-memory sources).

use std::collections::HashMap;

/// Provides source code content for compilation.
///
/// This trait abstracts away file system access, allowing the compiler
/// to work with in-memory sources (useful for WASM and testing).
pub trait SourceProvider {
    /// Get the source code for a given file path
    fn get_source(&self, path: &str) -> Option<String>;

    /// Check if a source file exists
    fn exists(&self, path: &str) -> bool {
        self.get_source(path).is_some()
    }

    /// List available source files (for module resolution)
    fn list_sources(&self) -> Vec<String> {
        vec![]
    }
}

/// In-memory source provider for WASM and testing
#[derive(Debug, Clone, Default)]
pub struct InMemorySourceProvider {
    sources: HashMap<String, String>,
}

impl InMemorySourceProvider {
    /// Create a new empty source provider
    pub fn new() -> Self {
        Self {
            sources: HashMap::new(),
        }
    }

    /// Add a source file
    pub fn add_source(&mut self, path: &str, content: &str) {
        self.sources.insert(path.to_string(), content.to_string());
    }

    /// Add a source file (builder pattern)
    pub fn with_source(mut self, path: &str, content: &str) -> Self {
        self.add_source(path, content);
        self
    }
}

impl SourceProvider for InMemorySourceProvider {
    fn get_source(&self, path: &str) -> Option<String> {
        self.sources.get(path).cloned()
    }

    fn exists(&self, path: &str) -> bool {
        self.sources.contains_key(path)
    }

    fn list_sources(&self) -> Vec<String> {
        self.sources.keys().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_in_memory_source_provider() {
        let mut provider = InMemorySourceProvider::new();
        provider.add_source("test.ty", "let x: Number = 42;");

        assert!(provider.exists("test.ty"));
        assert!(!provider.exists("nonexistent.ty"));
        assert_eq!(
            provider.get_source("test.ty"),
            Some("let x: Number = 42;".to_string())
        );
    }

    #[test]
    fn test_builder_pattern() {
        let provider = InMemorySourceProvider::new()
            .with_source("a.ty", "let a = 1;")
            .with_source("b.ty", "let b = 2;");

        assert!(provider.exists("a.ty"));
        assert!(provider.exists("b.ty"));
    }
}
