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

    /// Remove a source file
    pub fn remove_source(&mut self, path: &str) {
        self.sources.remove(path);
    }

    /// Clear all sources
    pub fn clear(&mut self) {
        self.sources.clear();
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

/// Handles output from transpilation.
///
/// This trait allows customizing where transpiled R code goes -
/// to files, memory buffers, or anywhere else.
pub trait OutputHandler {
    /// Write transpiled R code
    fn write_r_code(&mut self, filename: &str, content: &str) -> Result<(), OutputError>;

    /// Write type annotations
    fn write_type_annotations(&mut self, filename: &str, content: &str) -> Result<(), OutputError>;

    /// Write generic function declarations
    fn write_generic_functions(&mut self, filename: &str, content: &str)
        -> Result<(), OutputError>;
}

/// In-memory output handler for WASM and testing
#[derive(Debug, Clone, Default)]
pub struct InMemoryOutputHandler {
    pub outputs: HashMap<String, String>,
}

impl InMemoryOutputHandler {
    pub fn new() -> Self {
        Self {
            outputs: HashMap::new(),
        }
    }

    pub fn get_output(&self, filename: &str) -> Option<&String> {
        self.outputs.get(filename)
    }
}

impl OutputHandler for InMemoryOutputHandler {
    fn write_r_code(&mut self, filename: &str, content: &str) -> Result<(), OutputError> {
        self.outputs
            .insert(filename.to_string(), content.to_string());
        Ok(())
    }

    fn write_type_annotations(&mut self, filename: &str, content: &str) -> Result<(), OutputError> {
        self.outputs
            .insert(format!("{}_types", filename), content.to_string());
        Ok(())
    }

    fn write_generic_functions(
        &mut self,
        filename: &str,
        content: &str,
    ) -> Result<(), OutputError> {
        self.outputs
            .insert(format!("{}_generics", filename), content.to_string());
        Ok(())
    }
}

/// Output operation error
#[derive(Debug, Clone)]
pub struct OutputError {
    pub message: String,
}

impl std::fmt::Display for OutputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Output error: {}", self.message)
    }
}

impl std::error::Error for OutputError {}

/// Checks and optionally installs R packages.
///
/// In native mode, this can execute R commands.
/// In WASM mode, this can be a no-op or return cached information.
pub trait PackageChecker {
    /// Check if a package is available
    fn is_package_available(&self, name: &str) -> bool;

    /// Try to install a package (may be a no-op in WASM)
    fn install_package(&mut self, name: &str) -> Result<(), PackageError>;

    /// Get package type information (cached)
    fn get_package_types(&self, name: &str) -> Option<String>;
}

/// Stub package checker that does nothing (for WASM)
#[derive(Debug, Clone, Default)]
pub struct StubPackageChecker {
    available_packages: HashMap<String, String>,
}

impl StubPackageChecker {
    pub fn new() -> Self {
        Self {
            available_packages: HashMap::new(),
        }
    }

    /// Pre-register a package with its type information
    pub fn register_package(&mut self, name: &str, types: &str) {
        self.available_packages
            .insert(name.to_string(), types.to_string());
    }
}

impl PackageChecker for StubPackageChecker {
    fn is_package_available(&self, name: &str) -> bool {
        self.available_packages.contains_key(name)
    }

    fn install_package(&mut self, _name: &str) -> Result<(), PackageError> {
        // No-op in WASM - packages must be pre-registered
        Ok(())
    }

    fn get_package_types(&self, name: &str) -> Option<String> {
        self.available_packages.get(name).cloned()
    }
}

/// Package operation error
#[derive(Debug, Clone)]
pub struct PackageError {
    pub message: String,
}

impl std::fmt::Display for PackageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Package error: {}", self.message)
    }
}

impl std::error::Error for PackageError {}

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
