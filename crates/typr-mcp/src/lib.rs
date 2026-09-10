//! MCP server exposing the TypR compiler to AI agents.
//!
//! Runs over stdio (wired as the `typr mcp` subcommand — never an
//! `npx`/`sh -c` one-liner, see the crate-level design notes) and offers
//! `check` (type-check only) and `build` (transpile to R) tools, both
//! in-process: no filesystem, no project directory, no `.typr_cache`. They
//! reuse the same inlined-context recipe as `typr-wasm`'s `compile`/
//! `typeCheck` (the browser playground), which is the proof this works
//! without I/O.
//!
//! Type checking is recursive, so a long-lived server must drive it from a
//! large-stack thread — see [`run_stdio`].
//!
//! ## Client configuration
//!
//! Point an MCP client at the `typr` binary with the `mcp` argument. After
//! `cargo install typr`, that's just:
//!
//! ```json
//! {
//!   "mcpServers": {
//!     "typr": {
//!       "command": "typr",
//!       "args": ["mcp"]
//!     }
//!   }
//! }
//! ```
//!
//! Building from a local checkout instead? Point `command` at
//! `target/debug/typr` (or `target/release/typr`).

use rmcp::handler::server::wrapper::{Json, Parameters};
use rmcp::model::{ServerCapabilities, ServerInfo};
use rmcp::transport::stdio;
use rmcp::{ServerHandler, ServiceExt, tool, tool_handler, tool_router};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use typr_core::components::context::config::{Config, Environment};
use typr_core::components::error_message::typr_error::TypRError;
use typr_core::parsing::parse_from_string_with_errors;
use typr_core::processes::type_checking::type_checker::TypeChecker;

/// Standard library R code (embedded at compile time), same source `typr-wasm`
/// inlines into its `compile()` output.
const STD_R: &str = include_str!("../../typr-cli/configs/src/std.R");

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CheckParams {
    /// TypR source code to type-check.
    pub source: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct Diagnostic {
    /// Stable error code (`T0xx` for type errors, `S0xx` for syntax errors) —
    /// see `TypeError::code`/`SyntaxError::code` in typr-core.
    pub code: String,
    pub message: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct CheckResult {
    pub ok: bool,
    pub diagnostics: Vec<Diagnostic>,
}

fn check_source(source: &str) -> CheckResult {
    let parsed = parse_from_string_with_errors(source, "main.ty");
    if parsed.has_errors() {
        let diagnostics = parsed
            .errors
            .into_iter()
            .map(|e| {
                let err = TypRError::syntax_error(e);
                Diagnostic {
                    code: err.code().to_string(),
                    message: err.simple_message(),
                }
            })
            .collect();
        return CheckResult { ok: false, diagnostics };
    }

    // Same Environment::Wasm context typr-wasm's type_check() builds: all
    // modules inlined, no source() calls, nothing read from disk.
    let context = Config::default().set_environment(Environment::Wasm).to_context();
    let type_checker = TypeChecker::new(context).typing_no_panic(&parsed.ast);

    let diagnostics = type_checker
        .get_errors()
        .iter()
        .map(|e| Diagnostic {
            code: e.code().to_string(),
            message: e.simple_message(),
        })
        .collect();

    CheckResult {
        ok: !type_checker.has_errors(),
        diagnostics,
    }
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct BuildResult {
    pub ok: bool,
    /// Generated R code, with the standard library, type annotations, and
    /// generic function dispatchers inlined — produced even when
    /// `diagnostics` is non-empty (best-effort transpilation, same as
    /// `typr-wasm`'s `compile()`).
    pub r_code: String,
    pub diagnostics: Vec<Diagnostic>,
}

fn build_source(source: &str) -> BuildResult {
    let parsed = parse_from_string_with_errors(source, "main.ty");
    if parsed.has_errors() {
        let diagnostics = parsed
            .errors
            .into_iter()
            .map(|e| {
                let err = TypRError::syntax_error(e);
                Diagnostic {
                    code: err.code().to_string(),
                    message: err.simple_message(),
                }
            })
            .collect();
        return BuildResult { ok: false, r_code: String::new(), diagnostics };
    }

    // Same Environment::Wasm context check_source() builds.
    let context = Config::default().set_environment(Environment::Wasm).to_context();
    let type_checker = TypeChecker::new(context).typing_no_panic(&parsed.ast);

    let diagnostics: Vec<Diagnostic> = type_checker
        .get_errors()
        .iter()
        .map(|e| Diagnostic {
            code: e.code().to_string(),
            message: e.simple_message(),
        })
        .collect();
    let ok = !type_checker.has_errors();

    // Transpile proceeds even with type errors (best-effort output), mirroring
    // typr-wasm's compile().
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

    let mut r_code = String::new();
    r_code.push_str("# === TypR Standard Library ===\n");
    r_code.push_str(STD_R);
    r_code.push_str("\n\n");
    if !generic_functions.trim().is_empty() {
        r_code.push_str("# === Generic Functions ===\n");
        r_code.push_str(&generic_functions);
        r_code.push_str("\n\n");
    }
    if !type_annotations.trim().is_empty() {
        r_code.push_str("# === Type Annotations ===\n");
        r_code.push_str(&type_annotations);
        r_code.push_str("\n\n");
    }
    r_code.push_str("# === Main Code ===\n");
    r_code.push_str(&main_code);

    BuildResult { ok, r_code, diagnostics }
}

#[derive(Clone, Default)]
pub struct TyprMcpServer;

#[tool_router]
impl TyprMcpServer {
    pub fn new() -> Self {
        Self
    }

    #[tool(
        description = "Type-check TypR source code in-process (no filesystem, no project \
        directory) and report diagnostics with stable error codes (T0xx for type errors, \
        S0xx for syntax errors)."
    )]
    async fn check(&self, Parameters(CheckParams { source }): Parameters<CheckParams>) -> Json<CheckResult> {
        Json(check_source(&source))
    }

    #[tool(
        description = "Transpile TypR source code to R in-process (no filesystem, no project \
        directory), returning the generated R code — with the standard library, type \
        annotations, and generic function dispatchers inlined — plus any diagnostics found \
        while type-checking (stable T0xx/S0xx codes, same as `check`). `r_code` is produced \
        even when type errors are present, so callers should check `ok`/`diagnostics` before \
        trusting the output."
    )]
    async fn build(&self, Parameters(CheckParams { source }): Parameters<CheckParams>) -> Json<BuildResult> {
        Json(build_source(&source))
    }
}

#[tool_handler]
impl ServerHandler for TyprMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build()).with_instructions(
            "TypR compiler tools. `check` type-checks TypR source and returns diagnostics \
             tagged with stable error codes. `build` does the same but also returns the \
             transpiled R code.",
        )
    }
}

#[cfg(test)]
mod tests {
    use super::{build_source, check_source};

    #[test]
    fn valid_source_has_no_diagnostics() {
        let result = check_source("let x: int <- 42;");
        assert!(result.ok, "{:?}", result.diagnostics);
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn type_mismatch_is_reported_with_a_t_code() {
        let result = check_source("let x: int <- \"oops\";");
        assert!(!result.ok);
        assert_eq!(result.diagnostics.len(), 1);
        assert!(result.diagnostics[0].code.starts_with('T'), "{:?}", result.diagnostics[0]);
    }

    #[test]
    fn forgotten_semicolon_is_reported_with_an_s_code() {
        let result = check_source("let a <- 5");
        assert!(!result.ok);
        assert_eq!(result.diagnostics.len(), 1);
        assert!(result.diagnostics[0].code.starts_with('S'), "{:?}", result.diagnostics[0]);
    }

    #[test]
    fn valid_source_transpiles_to_r_with_std_lib_inlined() {
        let result = build_source("let x: int <- 42;");
        assert!(result.ok, "{:?}", result.diagnostics);
        assert!(result.diagnostics.is_empty());
        assert!(result.r_code.contains("42L"), "{}", result.r_code);
        assert!(result.r_code.contains("# === TypR Standard Library ==="));
    }

    #[test]
    fn build_still_returns_r_code_on_type_error() {
        let result = build_source("let x: int <- \"oops\";");
        assert!(!result.ok);
        assert_eq!(result.diagnostics.len(), 1);
        assert!(!result.r_code.is_empty());
    }

    #[test]
    fn build_returns_empty_r_code_on_syntax_error() {
        let result = build_source("let a <- 5");
        assert!(!result.ok);
        assert_eq!(result.diagnostics.len(), 1);
        assert!(result.r_code.is_empty());
    }
}

/// Run the MCP server over stdio until the client disconnects.
///
/// Type checking recurses over the AST, so callers should drive this future
/// from a thread with a large stack (the `typr lsp` subcommand sets up an
/// 8MB-stack tokio runtime for the same reason).
pub async fn run_stdio() -> anyhow::Result<()> {
    let service = TyprMcpServer::new().serve(stdio()).await?;
    service.waiting().await?;
    Ok(())
}
