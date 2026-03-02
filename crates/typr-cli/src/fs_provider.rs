//! FileSystem-based implementations of typr-core traits
//!
//! These implementations allow the CLI app to use the filesystem
//! for reading sources and writing outputs.

use std::fs;
use std::path::PathBuf;
use typr_core::{OutputError, OutputHandler, PackageChecker, PackageError, SourceProvider};

/// Filesystem-based source provider for native compilation.
///
/// Reads TypR source files from the filesystem.
#[derive(Debug, Clone)]
pub struct FileSystemSourceProvider {
    base_path: PathBuf,
}

impl FileSystemSourceProvider {
    /// Create a new provider with a base path for resolving relative paths
    pub fn new(base_path: PathBuf) -> Self {
        Self { base_path }
    }

    /// Create a provider for the current directory
    pub fn current_dir() -> std::io::Result<Self> {
        Ok(Self {
            base_path: std::env::current_dir()?,
        })
    }

    /// Get the full path for a source file
    fn resolve_path(&self, path: &str) -> PathBuf {
        if PathBuf::from(path).is_absolute() {
            PathBuf::from(path)
        } else {
            self.base_path.join(path)
        }
    }
}

impl SourceProvider for FileSystemSourceProvider {
    fn get_source(&self, path: &str) -> Option<String> {
        let full_path = self.resolve_path(path);
        fs::read_to_string(&full_path).ok()
    }

    fn exists(&self, path: &str) -> bool {
        let full_path = self.resolve_path(path);
        full_path.exists() && full_path.is_file()
    }

    fn list_sources(&self) -> Vec<String> {
        self.list_sources_recursive(&self.base_path)
    }
}

impl FileSystemSourceProvider {
    /// Recursively list all .ty files under a directory
    fn list_sources_recursive(&self, dir: &PathBuf) -> Vec<String> {
        let mut sources = Vec::new();

        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    sources.extend(self.list_sources_recursive(&path));
                } else if path.extension().map_or(false, |ext| ext == "ty") {
                    if let Ok(relative) = path.strip_prefix(&self.base_path) {
                        sources.push(relative.to_string_lossy().to_string());
                    }
                }
            }
        }

        sources
    }
}

/// Filesystem-based output handler for native compilation.
///
/// Writes transpiled R code and related files to the filesystem.
#[derive(Debug, Clone)]
pub struct FileSystemOutputHandler {
    output_dir: PathBuf,
}

impl FileSystemOutputHandler {
    /// Create a new handler that writes to the specified directory
    pub fn new(output_dir: PathBuf) -> Self {
        Self { output_dir }
    }

    /// Create a handler for the current directory
    pub fn current_dir() -> std::io::Result<Self> {
        Ok(Self {
            output_dir: std::env::current_dir()?,
        })
    }

    /// Ensure the output directory exists
    fn ensure_dir(&self) -> Result<(), OutputError> {
        fs::create_dir_all(&self.output_dir).map_err(|e| OutputError {
            message: format!("Failed to create output directory: {}", e),
        })
    }

    /// Write content to a file in the output directory
    fn write_file(&self, filename: &str, content: &str) -> Result<(), OutputError> {
        self.ensure_dir()?;
        let path = self.output_dir.join(filename);
        fs::write(&path, content).map_err(|e| OutputError {
            message: format!("Failed to write {}: {}", path.display(), e),
        })
    }
}

impl OutputHandler for FileSystemOutputHandler {
    fn write_r_code(&mut self, filename: &str, content: &str) -> Result<(), OutputError> {
        self.write_file(filename, content)
    }

    fn write_type_annotations(&mut self, filename: &str, content: &str) -> Result<(), OutputError> {
        self.write_file(
            &format!("{}_types.R", filename.trim_end_matches(".R")),
            content,
        )
    }

    fn write_generic_functions(
        &mut self,
        filename: &str,
        content: &str,
    ) -> Result<(), OutputError> {
        self.write_file(
            &format!("{}_generics.R", filename.trim_end_matches(".R")),
            content,
        )
    }
}

/// Native R package checker that can query R for package availability.
///
/// Uses Rscript to check if packages are installed.
#[derive(Debug, Clone)]
pub struct NativePackageChecker {
    /// Cache of known package types
    package_types: std::collections::HashMap<String, String>,
}

impl NativePackageChecker {
    pub fn new() -> Self {
        Self {
            package_types: std::collections::HashMap::new(),
        }
    }
}

impl Default for NativePackageChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl PackageChecker for NativePackageChecker {
    fn is_package_available(&self, name: &str) -> bool {
        use std::process::Command;

        // Use Rscript to check if package is available
        let result = Command::new("Rscript")
            .arg("-e")
            .arg(format!("cat(requireNamespace('{}', quietly = TRUE))", name))
            .output();

        match result {
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                stdout.trim() == "TRUE"
            }
            Err(_) => false,
        }
    }

    fn install_package(&mut self, name: &str) -> Result<(), PackageError> {
        use std::process::Command;

        let result = Command::new("Rscript")
            .arg("-e")
            .arg(format!(
                "install.packages('{}', repos='https://cloud.r-project.org')",
                name
            ))
            .output();

        match result {
            Ok(output) => {
                if output.status.success() {
                    Ok(())
                } else {
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    Err(PackageError {
                        message: format!("Failed to install package '{}': {}", name, stderr),
                    })
                }
            }
            Err(e) => Err(PackageError {
                message: format!("Failed to run Rscript: {}", e),
            }),
        }
    }

    fn get_package_types(&self, name: &str) -> Option<String> {
        self.package_types.get(name).cloned()
    }
}
