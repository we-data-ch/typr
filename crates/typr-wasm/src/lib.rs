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
use typr_core::processes::type_checking::type_recorder::with_recording;
use typr_core::{Compiler, InMemorySourceProvider};
use wasm_bindgen::prelude::*;

/// Standard library R code (embedded at compile time)
const STD_R: &str = include_str!("../../typr-cli/configs/src/std.R");

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

    // Type check without panicking on errors
    let type_checker = TypeChecker::new(compiler.get_context()).typing_no_panic(&ast);

    // Collect type errors
    let has_errors = type_checker.has_errors();
    let errors = if has_errors {
        type_checker
            .get_errors()
            .iter()
            .map(|e| e.clone().display())
            .collect::<Vec<_>>()
            .join("\n\n")
    } else {
        String::new()
    };

    let context = type_checker.get_context();

    // Get the transpiled main code (proceeds even with type errors)
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
        has_errors,
        errors,
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
            .map(|e| e.clone().display())
            .collect::<Vec<_>>()
            .join("\n\n"),
    })
}

/// Parse TypR source code and return the AST as JSON
#[wasm_bindgen]
pub fn parse(source: &str) -> Result<String, JsValue> {
    let mut sources = InMemorySourceProvider::new();
    sources.add_source("main.ty", source);

    let compiler = Compiler::new_wasm(sources);
    match compiler.parse("main.ty") {
        Ok(ast) => serde_json::to_string(&format!("{:?}", ast)).map_err(|e| JsValue::from_str(&e.to_string())),
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

    let files: std::collections::HashMap<String, String> =
        serde_json::from_str(files_json).map_err(|e| JsValue::from_str(&format!("Invalid JSON: {}", e)))?;

    let mut sources = InMemorySourceProvider::new();
    for (filename, content) in files {
        sources.add_source(&filename, &content);
    }

    let compiler = Compiler::new_wasm(sources);

    let ast = compiler
        .parse("main.ty")
        .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

    // Type check without panicking on errors
    let type_checker = TypeChecker::new(compiler.get_context()).typing_no_panic(&ast);

    // Collect type errors
    let has_errors = type_checker.has_errors();
    let errors = if has_errors {
        type_checker
            .get_errors()
            .iter()
            .map(|e| e.clone().display())
            .collect::<Vec<_>>()
            .join("\n\n")
    } else {
        String::new()
    };

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
        has_errors,
        errors,
    })
}

/// Build the block-graph view of TypR source code (`visualization_graph_v2.md`)
///
/// The graph is only built for source that type-checks: totality of the builder (no panic) is
/// only guaranteed for a program that passes `typr check` (spec §12 étape 1). When there are
/// type errors, `has_errors` is set and `graph_json` is left empty, matching `typeCheck()`.
#[wasm_bindgen(js_name = semanticGraph)]
pub fn semantic_graph(source: &str) -> Result<GraphResult, JsValue> {
    let mut sources = InMemorySourceProvider::new();
    sources.add_source("main.ty", source);

    let compiler = Compiler::new_wasm(sources);
    let ast = compiler
        .parse("main.ty")
        .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

    let (result, table) = with_recording(|| compiler.type_check(&ast));

    if result.has_errors() {
        return Ok(GraphResult {
            graph_json: String::new(),
            has_errors: true,
            errors: result
                .get_errors()
                .iter()
                .map(|e| e.clone().display())
                .collect::<Vec<_>>()
                .join("\n\n"),
        });
    }

    let graph = typr_graph::build(&ast, &result.type_context.context, &table);
    let graph_json =
        typr_graph::export::json::to_string_pretty(&graph).map_err(|e| JsValue::from_str(&e.to_string()))?;

    Ok(GraphResult {
        graph_json,
        has_errors: false,
        errors: String::new(),
    })
}

/// Build the block-graph views of two versions of TypR source and diff them
/// (`visualization_graph_v2.md` §12 étape 6), for the playground's diff view.
///
/// Each side is built independently through the same pipeline as `semanticGraph`: if either
/// side has type errors, the graph on that side is not built and the diff is not computed
/// (`has_errors`/`errors` describe whichever side(s) failed — both sides are still type-checked
/// so a caller sees every error, not just the first). Both `old_graph_json` and `new_graph_json`
/// are included alongside `diff_json` because the diff itself only carries keys, not full block
/// data — the playground renders the diff overlaid on the *new* graph's layout (matching `typr
/// graph diff`'s framing of "how did the new version change relative to the old one"), but still
/// needs the *old* graph to show what a removed block actually was (kind, name) since a removed
/// key has no entry in the new graph to look it up in.
#[wasm_bindgen(js_name = semanticGraphDiff)]
pub fn semantic_graph_diff(old_source: &str, new_source: &str) -> Result<GraphDiffResult, JsValue> {
    let build_one = |source: &str| -> Result<Result<typr_graph::BlockGraph, String>, JsValue> {
        let mut sources = InMemorySourceProvider::new();
        sources.add_source("main.ty", source);
        let compiler = Compiler::new_wasm(sources);
        let ast = compiler
            .parse("main.ty")
            .map_err(|e| JsValue::from_str(&format!("{}", e)))?;

        let (result, table) = with_recording(|| compiler.type_check(&ast));
        if result.has_errors() {
            let errors = result
                .get_errors()
                .iter()
                .map(|e| e.clone().display())
                .collect::<Vec<_>>()
                .join("\n\n");
            return Ok(Err(errors));
        }

        Ok(Ok(typr_graph::build(&ast, &result.type_context.context, &table)))
    };

    let old_result = build_one(old_source)?;
    let new_result = build_one(new_source)?;

    let mut errors = Vec::new();
    if let Err(e) = &old_result {
        errors.push(format!("(old) {}", e));
    }
    if let Err(e) = &new_result {
        errors.push(format!("(new) {}", e));
    }
    if !errors.is_empty() {
        return Ok(GraphDiffResult {
            diff_json: String::new(),
            old_graph_json: String::new(),
            new_graph_json: String::new(),
            has_errors: true,
            errors: errors.join("\n\n"),
        });
    }

    let old_graph = old_result.expect("checked Ok above");
    let new_graph = new_result.expect("checked Ok above");
    let diff = typr_graph::diff::diff(&old_graph, &new_graph);

    let diff_json = serde_json::to_string_pretty(&diff).map_err(|e| JsValue::from_str(&e.to_string()))?;
    let old_graph_json =
        typr_graph::export::json::to_string_pretty(&old_graph).map_err(|e| JsValue::from_str(&e.to_string()))?;
    let new_graph_json =
        typr_graph::export::json::to_string_pretty(&new_graph).map_err(|e| JsValue::from_str(&e.to_string()))?;

    Ok(GraphDiffResult {
        diff_json,
        old_graph_json,
        new_graph_json,
        has_errors: false,
        errors: String::new(),
    })
}

/// Result of building and diffing the block-graph views of two source versions
#[wasm_bindgen]
pub struct GraphDiffResult {
    /// Pretty-printed JSON of the `GraphDiff` (added/removed/modified block keys), empty when
    /// `has_errors` is set
    #[wasm_bindgen(getter_with_clone)]
    pub diff_json: String,
    /// Pretty-printed JSON of the *old* version's `BlockGraph` — only its `blocks` map is
    /// actually needed (to look up a removed key's kind/name), not its layout
    #[wasm_bindgen(getter_with_clone)]
    pub old_graph_json: String,
    /// Pretty-printed JSON of the *new* version's `BlockGraph`, so the playground can render the
    /// diff overlaid on a real layout without a second round-trip
    #[wasm_bindgen(getter_with_clone)]
    pub new_graph_json: String,
    /// Whether type errors on either side prevented building the diff
    pub has_errors: bool,
    /// Formatted type error messages, prefixed with `(old)`/`(new)` per side (empty if none)
    #[wasm_bindgen(getter_with_clone)]
    pub errors: String,
}

/// Result of building the block-graph view
#[wasm_bindgen]
pub struct GraphResult {
    /// Pretty-printed JSON of the `BlockGraph` (spec §9), empty when `has_errors` is set
    #[wasm_bindgen(getter_with_clone)]
    pub graph_json: String,
    /// Whether type errors prevented building the graph
    pub has_errors: bool,
    /// Formatted type error messages (empty string if no errors)
    #[wasm_bindgen(getter_with_clone)]
    pub errors: String,
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
    /// Whether type errors were found during compilation
    pub has_errors: bool,
    /// Formatted error messages (empty string if no errors)
    #[wasm_bindgen(getter_with_clone)]
    pub errors: String,
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
