//! # TypR Core
//!
//! Pure logic for TypR - a typed superset of R.
//!
//! This crate contains the core functionality that can be compiled to WebAssembly:
//! - Parsing TypR source code
//! - Type checking
//! - Transpilation to R
//!
//! ## Architecture
//!
//! The crate is designed to be platform-agnostic by using trait abstractions
//! for all I/O operations:
//!
//! - [`SourceProvider`]: Provides source code content (replaces `std::fs::read_to_string`)
//! - [`OutputHandler`]: Handles transpilation output (replaces file writes)
//! - [`PackageChecker`]: Checks/installs R packages (replaces `Command` execution)
//!
//! ## Usage
//!
//! ```rust,ignore
//! use typr_core::{Compiler, InMemorySourceProvider};
//!
//! let mut sources = InMemorySourceProvider::new();
//! sources.add_source("main.ty", "let x: Number = 42;");
//!
//! let compiler = Compiler::new(sources);
//! let result = compiler.compile("main.ty")?;
//! println!("R code: {}", result.r_code);
//! ```

pub mod abstractions;
pub mod components;
pub mod processes;
pub mod utils;

// Re-export main types
pub use abstractions::*;
pub use components::context::config::Environment;
pub use components::context::Context;
pub use components::error_message::typr_error::TypRError;
pub use components::language::Lang;
pub use components::r#type::Type;

// Re-export main functions
pub use processes::parsing;
pub use processes::transpiling;
pub use processes::type_checking::{typing, typing_with_errors, TypingResult};

/// Main compiler interface for TypR
pub struct Compiler<S: SourceProvider> {
    source_provider: S,
    context: Context,
}

impl<S: SourceProvider> Compiler<S> {
    /// Create a new compiler with the given source provider
    pub fn new(source_provider: S) -> Self {
        Self {
            source_provider,
            context: Context::default(),
        }
    }

    /// Create a compiler configured for WASM environment
    ///
    /// In WASM mode:
    /// - All external modules are inlined
    /// - No source() calls are generated
    /// - Generated files are collected and can be retrieved
    pub fn new_wasm(source_provider: S) -> Self {
        use components::context::config::Config;

        let config = Config::default().set_environment(Environment::Wasm);
        Self {
            source_provider,
            context: config.to_context(),
        }
    }

    /// Create a compiler with a custom context
    pub fn with_context(source_provider: S, context: Context) -> Self {
        Self {
            source_provider,
            context,
        }
    }

    /// Get the current context
    pub fn get_context(&self) -> Context {
        self.context.clone()
    }

    /// Parse source code and return the AST
    pub fn parse(&self, filename: &str) -> Result<Lang, CompileError> {
        let source = self
            .source_provider
            .get_source(filename)
            .ok_or_else(|| CompileError::FileNotFound(filename.to_string()))?;

        Ok(parsing::parse_from_string(&source, filename))
    }

    /// Type check the given AST
    pub fn type_check(&self, ast: &Lang) -> TypingResult {
        typing_with_errors(&self.context, ast)
    }

    /// Transpile AST to R code
    pub fn transpile(&self, ast: &Lang) -> TranspileResult {
        use processes::type_checking::type_checker::TypeChecker;

        let type_checker = TypeChecker::new(self.context.clone()).typing(ast);
        let r_code = type_checker.clone().transpile();
        let context = type_checker.get_context();

        TranspileResult {
            r_code,
            type_annotations: context.get_type_anotations(),
            generic_functions: context
                .get_all_generic_functions()
                .iter()
                .map(|(var, _)| var.get_name())
                .filter(|x| !x.contains("<-"))
                .collect(),
        }
    }

    /// Full compilation: parse, type check, and transpile
    pub fn compile(&self, filename: &str) -> Result<CompileOutput, CompileError> {
        let ast = self.parse(filename)?;
        let typing_result = self.type_check(&ast);

        if typing_result.has_errors() {
            return Err(CompileError::TypeErrors(typing_result.get_errors().clone()));
        }

        let transpile_result = self.transpile(&ast);

        Ok(CompileOutput {
            ast,
            r_code: transpile_result.r_code,
            type_annotations: transpile_result.type_annotations,
            generic_functions: transpile_result.generic_functions,
        })
    }

    /// Get completions at a position (for LSP support)
    pub fn get_completions(&self, _source: &str, _position: usize) -> Vec<String> {
        // TODO: Implement completion logic
        vec![]
    }

    /// Get hover information at a position (for LSP support)
    pub fn get_hover(&self, _source: &str, _position: usize) -> Option<String> {
        // TODO: Implement hover logic
        None
    }
}

/// Result of transpilation
#[derive(Debug, Clone)]
pub struct TranspileResult {
    pub r_code: String,
    pub type_annotations: String,
    pub generic_functions: Vec<String>,
}

/// Full compilation output
#[derive(Debug, Clone)]
pub struct CompileOutput {
    pub ast: Lang,
    pub r_code: String,
    pub type_annotations: String,
    pub generic_functions: Vec<String>,
}

/// Compilation errors
#[derive(Debug, Clone)]
pub enum CompileError {
    FileNotFound(String),
    ParseError(String),
    TypeErrors(Vec<TypRError>),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::FileNotFound(name) => write!(f, "File not found: {}", name),
            CompileError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            CompileError::TypeErrors(errors) => {
                write!(f, "Type errors:\n")?;
                for err in errors {
                    write!(f, "  - {:?}\n", err)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for CompileError {}
