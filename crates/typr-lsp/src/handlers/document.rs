//! Whole-document analysis: parse + metaprogram + type-check once, plus doc-comment lookup.

use super::goto_definition::detect_environment;
use super::utils::Span;
use crate::metaprogramming::metaprogrammation;
use nom_locate::LocatedSpan;
use std::collections::HashMap;
use typr_core::components::context::Context;
use typr_core::components::language::Lang;
use typr_core::processes::parsing::parse;
use typr_core::typing;

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
    /// Declared-name → doc-comment text, built once from `ast` (see
    /// `build_name_doc_map`) so hover/completion don't re-walk the AST on
    /// every request against unchanged text.
    pub doc_map: HashMap<String, String>,
}

impl DocumentAnalysis {
    /// Build a `DocumentAnalysis` from an already parsed+typed `context`/`ast`
    /// pair, deriving `doc_map` from `ast` so every construction site stays
    /// in sync (see `check_code_and_extract_errors` in `lsp.rs` for the other
    /// caller, which type-checks separately for diagnostics purposes).
    pub fn new(context: Context, ast: Lang) -> Self {
        let doc_map = build_name_doc_map(&ast);
        Self {
            context,
            ast,
            doc_map,
        }
    }
}

/// Parse, resolve `mod`/`use` imports, and type-check `content` once. Returns
/// `None` if parsing or type-checking panics (e.g. incomplete/invalid code
/// mid-edit) — callers treat that the same way the old per-feature pipelines
/// did (silently produce no result rather than surfacing an error).
#[tracing::instrument(skip_all)]
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

    Some(DocumentAnalysis::new(context, ast))
}

// ── doc-comment lookup ──────────────────────────────────────────────────────

/// Walk `ast`, collecting doc-comment blocks (consecutive `Lang::Comment`
/// lines immediately preceding a `let`/`type`/`module` declaration, no
/// intervening non-comment nodes) into a map from the declared name to the
/// joined comment text. Mirrors `typr_core::processes::spg::doc_attach`'s
/// `build_doc_map`, but keys by name instead of byte offset: hover and
/// completion look a symbol up by identifier, not by AST position, and the
/// SPG's offset (the `let`/`type`/`module` keyword's own position) doesn't
/// line up with the `Var`'s offset the rest of this module resolves against.
pub fn build_name_doc_map(ast: &Lang) -> HashMap<String, String> {
    let mut map = HashMap::new();
    collect_name_docs(ast, &mut map);
    map
}

fn collect_name_docs(lang: &Lang, map: &mut HashMap<String, String>) {
    match lang {
        Lang::Lines { value, .. } => {
            attach_name_docs(value, map);
            for item in value {
                collect_name_docs(item, map);
            }
        }
        Lang::Module { body, .. } => {
            attach_name_docs(body, map);
            for item in body {
                collect_name_docs(item, map);
            }
        }
        _ => {}
    }
}

/// Scan a flat slice of `Lang` items, flushing any accumulated comment block
/// onto the next declaration's name when one is found.
fn attach_name_docs(items: &[Lang], map: &mut HashMap<String, String>) {
    let mut pending: Vec<String> = Vec::new();

    for item in items {
        if let Lang::Comment { value, .. } = item {
            pending.push(value.trim().to_string());
            continue;
        }
        if !pending.is_empty() {
            if let Some(name) = declared_name(item) {
                map.insert(name, pending.join("\n"));
            }
            pending.clear();
        }
    }
}

/// The name a top-level declaration binds, for doc-comment attachment
/// purposes. `None` for anything that isn't a documentable declaration.
fn declared_name(lang: &Lang) -> Option<String> {
    match lang {
        Lang::Let { variable, .. } => doc_target_name(variable),
        Lang::Alias { identifier, .. } => doc_target_name(identifier),
        Lang::Module { name, .. } => Some(name.clone()),
        _ => None,
    }
}

pub fn doc_target_name(lang: &Lang) -> Option<String> {
    match lang {
        Lang::Variable { name, .. } => Some(name.clone()),
        _ => None,
    }
}

#[cfg(test)]
mod doc_comment_tests {
    use super::*;
    use crate::handlers::completions::{get_completions_at, resolve_completion_item};
    use crate::handlers::hover::resolve_hover;
    use tower_lsp_server::ls_types::Documentation;

    /// A doc-comment block directly preceding a `let` must surface in hover,
    /// appended after the type.
    #[test]
    fn hover_includes_preceding_doc_comment() {
        let content = "# Adds two integers together.\n\
                        # Returns their sum.\n\
                        let add <- fn(a: int, b: int): int { a + b };\n\
                        let result <- add(1, 2);\n";
        let analysis =
            analyze_document(content, "test.ty").expect("analysis should succeed for valid code");

        let name_col = content.rfind("add(1, 2)").unwrap() as u32;
        let line = content[..name_col as usize].matches('\n').count() as u32;
        let col_on_line = content[..name_col as usize]
            .rfind('\n')
            .map_or(name_col, |nl| name_col - nl as u32 - 1);

        let info = resolve_hover(&analysis, content, line, col_on_line + 1)
            .expect("hover should resolve `add`");
        assert!(
            info.type_display.contains("Adds two integers together."),
            "expected the doc-comment in hover output, got: {}",
            info.type_display
        );
        assert!(info.type_display.contains("Returns their sum."));
    }

    /// A doc-comment on a top-level `let` must surface as the `documentation`
    /// field of its completion item.
    #[test]
    fn completion_includes_preceding_doc_comment() {
        let content = "# Adds two integers together.\n\
                        let add <- fn(a: int, b: int): int { a + b };\n\
                        let result <- ad";
        let last_line = content.lines().count() as u32 - 1;
        let last_col = content.lines().last().unwrap().len() as u32;

        let (items, resolve_ctx) = get_completions_at(content, last_line, last_col, "test.ty");
        let add_item = items
            .iter()
            .find(|i| i.label == "add")
            .expect("expected a completion item for `add`");

        let mut resolved = add_item.clone();
        resolve_completion_item(
            resolve_ctx
                .as_ref()
                .expect("completions should have a resolve context"),
            &mut resolved,
        );

        match &resolved.documentation {
            Some(Documentation::MarkupContent(markup)) => {
                assert!(markup.value.contains("Adds two integers together."));
            }
            other => panic!("expected doc-comment documentation, got: {:?}", other),
        }
    }
}
