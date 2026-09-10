//! MCP server exposing the TypR compiler to AI agents.
//!
//! Runs over stdio (wired as the `typr mcp` subcommand — never an
//! `npx`/`sh -c` one-liner, see the crate-level design notes) and offers a
//! `check` tool that type-checks TypR source in-process: no filesystem, no
//! project directory, no `.typr_cache`. It reuses the same inlined-context
//! recipe as `typr-wasm`'s `compile`/`typeCheck` (the browser playground),
//! which is the proof this works without I/O.
//!
//! Type checking is recursive, so a long-lived server must drive it from a
//! large-stack thread — see [`run_stdio`].

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
}

#[tool_handler]
impl ServerHandler for TyprMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build()).with_instructions(
            "TypR compiler tools. `check` type-checks TypR source and returns diagnostics \
             tagged with stable error codes.",
        )
    }
}

#[cfg(test)]
mod tests {
    use super::check_source;

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
