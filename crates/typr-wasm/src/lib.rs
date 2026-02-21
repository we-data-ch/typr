//! WebAssembly bindings for TypR
//!
//! This crate provides JavaScript/TypeScript bindings for the TypR compiler,
//! allowing TypR code to be compiled and type-checked directly in the browser.
//!
//! ## Usage in JavaScript
//!
//! ```javascript
//! import init, { compile, typeCheck } from 'typr-wasm';
//!
//! async function main() {
//!     await init();
//!     
//!     const result = compile("let x: int <- 42\nx");
//!     console.log(result.r_code);
//! }
//! ```

use typr_core::processes::transpiling::clear_generated_files;
use typr_core::processes::type_checking::type_checker::TypeChecker;
use typr_core::{Compiler, InMemorySourceProvider};
use wasm_bindgen::prelude::*;

/// Standard library R code (embedded at compile time)
const STD_R: &str = include_str!("../../typr-core/configs/src/std.R");

/// Compile TypR source code to R
///
/// This is the main entry point for compilation.
/// Returns a CompileResult with the generated R code, including all
/// support code (std library, type annotations, generic functions) inlined.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<CompileResult, JsValue> {
    // Clear any previously generated files
    clear_generated_files();

    let mut sources = InMemorySourceProvider::new();
    sources.add_source("main.ty", source);

    // Use WASM-mode compiler which inlines everything
    let compiler = Compiler::new_wasm(sources);

    // Parse
    let ast = compiler
        .parse("main.ty")
        .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

    // Type check and get context
    let type_checker = TypeChecker::new(compiler.get_context()).typing(&ast);
    let context = type_checker.get_context();

    // Get the transpiled main code
    let main_code = type_checker.transpile();

    // Get type annotations (defines Character, Integer, Number, Boolean, etc.)
    let type_annotations = context.get_type_anotations();

    // Get generic function declarations
    let generic_functions: String = context
        .get_all_generic_functions()
        .iter()
        .map(|(var, _)| var.get_name())
        .filter(|x| !x.contains("<-"))
        .map(|fn_name| {
            format!(
                "{} <- function(x, ...) UseMethod('{}', x)",
                fn_name,
                fn_name.replace("`", "")
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    // Build the final R code by concatenating in order:
    // 1. Standard library (std.R)
    // 2. Generic function declarations
    // 3. Type annotations (defines Character, Integer, etc.)
    // 4. The main transpiled code
    let mut final_code = String::new();

    // 1. Standard library
    final_code.push_str("# === TypR Standard Library ===\n");
    final_code.push_str(STD_R);
    final_code.push_str("\n\n");

    // 2. Generic functions
    if !generic_functions.trim().is_empty() {
        final_code.push_str("# === Generic Functions ===\n");
        final_code.push_str(&generic_functions);
        final_code.push_str("\n\n");
    }

    // 3. Type annotations
    if !type_annotations.trim().is_empty() {
        final_code.push_str("# === Type Annotations ===\n");
        final_code.push_str(&type_annotations);
        final_code.push_str("\n\n");
    }

    // 4. Main code
    final_code.push_str("# === Main Code ===\n");
    final_code.push_str(&main_code);

    Ok(CompileResult {
        r_code: final_code,
        type_annotations,
        generic_functions,
    })
}

/// Type check TypR source code without compiling
///
/// Returns a TypeCheckResult indicating if there are errors.
#[wasm_bindgen(js_name = typeCheck)]
pub fn type_check(source: &str) -> Result<TypeCheckResult, JsValue> {
    let mut sources = InMemorySourceProvider::new();
    sources.add_source("main.ty", source);

    let compiler = Compiler::new_wasm(sources);
    let ast = match compiler.parse("main.ty") {
        Ok(ast) => ast,
        Err(e) => {
            return Ok(TypeCheckResult {
                has_errors: true,
                errors: format!("Parse error: {}", e),
            });
        }
    };

    let result = compiler.type_check(&ast);

    Ok(TypeCheckResult {
        has_errors: result.has_errors(),
        errors: result
            .get_errors()
            .iter()
            .map(|e| format!("{:?}", e))
            .collect::<Vec<_>>()
            .join("\n---\n"),
    })
}

/// Parse TypR source code and return the AST as JSON
#[wasm_bindgen]
pub fn parse(source: &str) -> Result<String, JsValue> {
    let mut sources = InMemorySourceProvider::new();
    sources.add_source("main.ty", source);

    let compiler = Compiler::new_wasm(sources);
    match compiler.parse("main.ty") {
        Ok(ast) => serde_json::to_string(&format!("{:?}", ast))
            .map_err(|e| JsValue::from_str(&e.to_string())),
        Err(e) => Err(JsValue::from_str(&e.to_string())),
    }
}

/// Transpile TypR source code to R without type checking
#[wasm_bindgen]
pub fn transpile(source: &str) -> Result<String, JsValue> {
    let mut sources = InMemorySourceProvider::new();
    sources.add_source("main.ty", source);

    let compiler = Compiler::new_wasm(sources);
    let ast = compiler
        .parse("main.ty")
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    let result = compiler.transpile(&ast);
    Ok(result.r_code)
}

/// Compile multiple TypR files
///
/// Takes a JSON object mapping filenames to source code.
/// The main file should be named "main.ty".
#[wasm_bindgen(js_name = compileMultiple)]
pub fn compile_multiple(files_json: &str) -> Result<CompileResult, JsValue> {
    clear_generated_files();

    let files: std::collections::HashMap<String, String> = serde_json::from_str(files_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid JSON: {}", e)))?;

    let mut sources = InMemorySourceProvider::new();
    for (filename, content) in files {
        sources.add_source(&filename, &content);
    }

    let compiler = Compiler::new_wasm(sources);

    let ast = compiler
        .parse("main.ty")
        .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

    let type_checker = TypeChecker::new(compiler.get_context()).typing(&ast);
    let context = type_checker.get_context();
    let main_code = type_checker.transpile();
    let type_annotations = context.get_type_anotations();

    let generic_functions: String = context
        .get_all_generic_functions()
        .iter()
        .map(|(var, _)| var.get_name())
        .filter(|x| !x.contains("<-"))
        .map(|fn_name| {
            format!(
                "{} <- function(x, ...) UseMethod('{}', x)",
                fn_name,
                fn_name.replace("`", "")
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let mut final_code = String::new();
    final_code.push_str(STD_R);
    final_code.push_str("\n\n");
    if !generic_functions.trim().is_empty() {
        final_code.push_str(&generic_functions);
        final_code.push_str("\n\n");
    }
    if !type_annotations.trim().is_empty() {
        final_code.push_str(&type_annotations);
        final_code.push_str("\n\n");
    }
    final_code.push_str(&main_code);

    Ok(CompileResult {
        r_code: final_code,
        type_annotations,
        generic_functions,
    })
}

/// Result of compilation
#[wasm_bindgen]
pub struct CompileResult {
    #[wasm_bindgen(getter_with_clone)]
    pub r_code: String,
    #[wasm_bindgen(getter_with_clone)]
    pub type_annotations: String,
    #[wasm_bindgen(getter_with_clone)]
    pub generic_functions: String,
}

/// Result of type checking
#[wasm_bindgen]
pub struct TypeCheckResult {
    #[wasm_bindgen(getter_with_clone)]
    pub has_errors: bool,
    #[wasm_bindgen(getter_with_clone)]
    pub errors: String,
}

/// Initialize the WASM module (called automatically by wasm-bindgen)
#[wasm_bindgen(start)]
pub fn init() {
    // Module initialization
}

// Keep the TypRCompiler struct for backwards compatibility
/// TypR compiler instance (for backwards compatibility)
///
/// Prefer using the standalone functions `compile()` and `typeCheck()` instead.
#[wasm_bindgen]
pub struct TypRCompiler;

#[wasm_bindgen]
impl TypRCompiler {
    /// Create a new TypR compiler instance
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self
    }

    /// Compile source code
    pub fn compile(&self, source: &str) -> Result<CompileResult, JsValue> {
        compile(source)
    }

    /// Type check source code
    #[wasm_bindgen(js_name = typeCheck)]
    pub fn type_check(&self, source: &str) -> Result<TypeCheckResult, JsValue> {
        type_check(source)
    }

    /// Parse source code
    pub fn parse(&self, source: &str) -> Result<String, JsValue> {
        parse(source)
    }

    /// Transpile source code
    pub fn transpile(&self, source: &str) -> Result<String, JsValue> {
        transpile(source)
    }
}

impl Default for TypRCompiler {
    fn default() -> Self {
        Self::new()
    }
}
