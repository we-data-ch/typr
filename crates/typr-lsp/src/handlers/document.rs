//! Token resolution and Markdown-highlighted type display for LSP hover.
//!
//! Given the full source text and a cursor position (line, character),
//! this module:
//!   1. Identifies the word (identifier / literal) under the cursor.
//!   2. Parses and type-checks the whole document using the project's
//!      pipeline (`parse` → `typing`) to build a fully-populated `Context`.
//!   3. Looks up the identifier in that context and returns its type.
//!   4. Renders the type string with Markdown syntax highlighting.

use crate::metaprogramming::metaprogrammation;
use nom_locate::LocatedSpan;
use tower_lsp_server::ls_types::Range;
use typr_core::components::context::Context;
use typr_core::components::language::Lang;
use typr_core::processes::parsing::parse;
use typr_core::typing;

type Span<'a> = LocatedSpan<&'a str, String>;

use super::*;

/// A resolved hover result: the Markdown-highlighted type and the LSP range.
#[derive(Debug, Clone)]
pub struct HoverInfo {
    /// Markdown string ready to be sent as hover contents.
    pub type_display: String,
    /// The source range of the token that was resolved.
    pub range: Range,
}

/// The result of parsing + metaprogramming + type-checking a whole document
/// once. Hover, go-to-definition and signature-help all only need the final
/// `Context` — they previously each ran this same parse→metaprogram→typing
/// pipeline from scratch. `Backend` caches this per-URI keyed by a content
/// hash (see `lsp.rs`) so repeated requests against unchanged text skip
/// straight to the cheap position-specific lookup below.
#[derive(Debug, Clone)]
pub struct DocumentAnalysis {
    pub context: Context,
    /// The parsed (post-metaprogrammation) AST. Go-to-definition needs this
    /// alongside `context`: module field exports and function parameters
    /// either lose their precise `HelpData` or disappear entirely by the time
    /// the final `Context` is built (see `resolve_definition`'s module-field
    /// and parameter branches for why each needs the AST specifically).
    pub ast: Lang,
}

/// Parse, resolve `mod`/`use` imports, and type-check `content` once. Returns
/// `None` if parsing or type-checking panics (e.g. incomplete/invalid code
/// mid-edit) — callers treat that the same way the old per-feature pipelines
/// did (silently produce no result rather than surfacing an error).
pub fn analyze_document(content: &str, file_path: &str) -> Option<DocumentAnalysis> {
    let span: Span = LocatedSpan::new_extra(content, file_path.to_string());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    let ast = parse_result.ok()?.ast;

    let environment = detect_environment(file_path);
    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        metaprogrammation(ast, environment)
    }))
    .ok()?;

    let context = Context::default().set_environment(environment);
    let type_context =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)));
    let context = type_context.ok()?.context;

    Some(DocumentAnalysis { context, ast })
}

/// A resolved definition result: the location where a symbol is defined.
#[derive(Debug, Clone)]
pub struct DefinitionInfo {
    /// The source range where the symbol is defined.
    pub range: Range,
    /// The file path where the symbol is defined (None if same file or unknown).
    pub file_path: Option<String>,
}

