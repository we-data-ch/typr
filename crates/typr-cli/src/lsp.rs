//! LSP server for TypR.
//!
//! Currently exposes:
//!   - **Hover** provider: shows inferred types with Markdown syntax highlighting
//!   - **Completion** provider: context-aware autocompletion for variables, functions, and type aliases
//!     - Trigger characters: `.`, `$`, `>` (for `|>`), `:` (for type annotations)
//!   - **Diagnostics** (push model): real-time error checking via `textDocument/publishDiagnostics`
//!     - Diagnostics are published on `didOpen` and `didChange` events
//!   - **Go to Definition** provider: jump to symbol definitions (variables, functions, type aliases)
//!   - **Workspace Symbol** provider: search for symbols across all open documents
//!   - **Document Symbol** provider: outline/breadcrumbs for the current document
//!   - **Signature Help** provider: parameter hints while inside a function call's `(...)`
//!   - **Rename** / **References** providers: occurrence search scoped to a single document
//!     (textual, word-boundary based — not a full scope-aware resolution)
//!
//! Performance: `did_change` debounces diagnostics (`DIAGNOSTICS_DEBOUNCE`) so a
//! burst of keystrokes triggers one parse + type-check instead of one per
//! keystroke, and a per-URI `analysis_cache` lets hover/goto-definition/
//! signatureHelp reuse the `Context` already built for unchanged text instead
//! of re-running the whole pipeline on every request.
//!
//! Launch with `typr lsp`.  The server communicates over stdin/stdout using
//! the standard LSP JSON-RPC protocol.

use crate::lsp_parser;
use nom_locate::LocatedSpan;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer, LspService, Server};
use typr_core::components::context::Context;
use typr_core::components::error_message::help_data::HelpData;
use typr_core::components::error_message::syntax_error::SyntaxError;
use typr_core::components::error_message::type_error::TypeError;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::processes::parsing::parse;

type Span<'a> = LocatedSpan<&'a str, String>;

/// Per-URI cache entry: a content hash paired with the `DocumentAnalysis`
/// computed from that exact content.
type AnalysisCache = HashMap<Uri, (u64, Arc<lsp_parser::DocumentAnalysis>)>;

/// Shared state: one copy of each open document's full text.
#[derive(Clone)]
struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<Uri, String>>>,
    /// Per-URI cache of the last successful parse→metaprogram→typing pass,
    /// keyed by a hash of the content it was computed from. Populated by
    /// `compute_diagnostics`; consulted by hover/goto-definition/signatureHelp
    /// so they can skip straight to a position-specific lookup when the
    /// document hasn't changed since diagnostics last ran.
    analysis_cache: Arc<RwLock<AnalysisCache>>,
    /// Per-URI monotonic counter bumped on every `did_change`, used to
    /// debounce diagnostics: a delayed diagnostics task only runs if its
    /// generation is still the latest when it wakes up, so a burst of
    /// keystrokes results in one type-check, not one per keystroke.
    diagnostics_generation: Arc<RwLock<HashMap<Uri, u64>>>,
}

/// How long to wait after the last `did_change` before actually computing
/// and publishing diagnostics. Keeps fast typing from triggering a full
/// parse + type-check on every keystroke.
const DIAGNOSTICS_DEBOUNCE: std::time::Duration = std::time::Duration::from_millis(250);

impl LanguageServer for Backend {
    // ── initialisation ──────────────────────────────────────────────────────
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".into(), "$".into(), ">".into(), ":".into()]),
                    resolve_provider: None,
                    ..Default::default()
                }),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".into(), ",".into()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                references_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "TypR LSP server initialized.")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    // ── document sync ───────────────────────────────────────────────────────
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let content = params.text_document.text.clone();

        let mut docs = self.documents.write().await;
        docs.insert(uri.clone(), content.clone());
        drop(docs); // Release the lock before computing diagnostics

        // Compute and publish diagnostics
        let diagnostics = self.compute_diagnostics(&content, &uri).await;
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let mut docs = self.documents.write().await;

        // Full-sync mode: each change event contains the complete text.
        if let Some(change) = params.content_changes.into_iter().next() {
            let content = change.text.clone();
            docs.insert(uri.clone(), content.clone());
            drop(docs); // Release the lock before computing diagnostics

            // Debounce: bump this URI's generation now, then only let the
            // delayed task that wakes up still holding the latest generation
            // actually compute + publish. A burst of keystrokes collapses
            // into a single type-check, ~DIAGNOSTICS_DEBOUNCE after typing
            // settles, instead of one per keystroke.
            let generation = {
                let mut gens = self.diagnostics_generation.write().await;
                let next = gens.get(&uri).copied().unwrap_or(0).wrapping_add(1);
                gens.insert(uri.clone(), next);
                next
            };

            let backend = self.clone();
            tokio::spawn(async move {
                tokio::time::sleep(DIAGNOSTICS_DEBOUNCE).await;

                let is_latest = {
                    let gens = backend.diagnostics_generation.read().await;
                    gens.get(&uri).copied() == Some(generation)
                };
                if !is_latest {
                    return; // superseded by a later edit; let that one publish
                }

                let diagnostics = backend.compute_diagnostics(&content, &uri).await;
                backend
                    .client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            });
        }
    }

    // ── hover ───────────────────────────────────────────────────────────────
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c.clone(),
            None => return Ok(None),
        };
        drop(docs);

        // Extract the file path from the URI so imported `mod`/`use` modules
        // can be resolved (mirrors `goto_definition`).
        let file_path = uri
            .to_file_path()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();

        let analysis = match self.analysis_for(&uri, &content, &file_path).await {
            Some(a) => a,
            None => return Ok(None),
        };

        // Offload the position-specific lookup to a blocking thread so we
        // don't stall the LSP event loop.
        let content_owned = content.clone();
        let info = tokio::task::spawn_blocking(move || {
            lsp_parser::resolve_hover(&analysis, &content_owned, position.line, position.character)
        })
        .await
        .ok() // if the blocking task panicked, treat as None
        .flatten();

        match info {
            Some(hover_info) => Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_info.type_display,
                }),
                range: Some(hover_info.range),
            })),
            None => Ok(None),
        }
    }

    // ── completion ──────────────────────────────────────────────────────────
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c,
            None => return Ok(None),
        };

        // Extract the file path from the URI so `mod` completions can scan
        // sibling `.ty` files (same approach as `goto_definition`).
        let file_path = uri
            .to_file_path()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();

        // Offload parsing + typing to a blocking thread (same strategy as hover).
        let content_owned = content.clone();
        let items = tokio::task::spawn_blocking(move || {
            lsp_parser::get_completions_at(
                &content_owned,
                position.line,
                position.character,
                &file_path,
            )
        })
        .await
        .ok()
        .unwrap_or_default();

        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    // ── workspace/symbol ─────────────────────────────────────────────────────
    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<WorkspaceSymbolResponse>> {
        let query = params.query.to_lowercase();
        let docs = self.documents.read().await;

        let mut all_symbols = Vec::new();

        for (uri, content) in docs.iter() {
            let content_owned = content.clone();
            let uri_owned = uri.clone();

            // Offload parsing to a blocking thread
            let symbols = tokio::task::spawn_blocking(move || {
                get_workspace_symbols(&content_owned, &uri_owned)
            })
            .await
            .ok()
            .unwrap_or_default();

            all_symbols.extend(symbols);
        }

        // Filter symbols by query (case-insensitive substring match)
        if !query.is_empty() {
            all_symbols.retain(|sym| sym.name.to_lowercase().contains(&query));
        }

        if all_symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(all_symbols.into()))
        }
    }

    // ── textDocument/documentSymbol ─────────────────────────────────────────
    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c.clone(),
            None => return Ok(None),
        };
        drop(docs);

        let symbols = tokio::task::spawn_blocking(move || get_document_symbols(&content))
            .await
            .ok()
            .unwrap_or_default();

        if symbols.is_empty() {
            Ok(None)
        } else {
            Ok(Some(DocumentSymbolResponse::Nested(symbols)))
        }
    }

    // ── textDocument/definition ───────────────────────────────────────────────
    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c.clone(),
            None => return Ok(None),
        };
        drop(docs);

        // Extract the file path from the URI for cross-file definition lookup.
        let file_path = uri
            .to_file_path()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();

        let analysis = match self.analysis_for(&uri, &content, &file_path).await {
            Some(a) => a,
            None => return Ok(None),
        };

        // Offload the position-specific lookup to a blocking thread so we
        // don't stall the LSP event loop.
        let content_owned = content.clone();
        let uri_owned = uri.clone();
        let file_path_owned = file_path.clone();
        let info = tokio::task::spawn_blocking(move || {
            lsp_parser::resolve_definition(
                &analysis,
                &content_owned,
                position.line,
                position.character,
                &file_path_owned,
            )
        })
        .await
        .ok()
        .flatten();

        match info {
            Some(def_info) => {
                // Use the definition's file path if available, otherwise use current file
                let target_uri = match &def_info.file_path {
                    Some(path) => Uri::from_file_path(path).unwrap_or(uri_owned),
                    None => uri_owned,
                };
                Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: target_uri,
                    range: def_info.range,
                })))
            }
            None => Ok(None),
        }
    }

    // ── textDocument/signatureHelp ───────────────────────────────────────────
    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c.clone(),
            None => return Ok(None),
        };
        drop(docs);

        let file_path = uri
            .to_file_path()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();

        let analysis = match self.analysis_for(&uri, &content, &file_path).await {
            Some(a) => a,
            None => return Ok(None),
        };

        let help = tokio::task::spawn_blocking(move || {
            lsp_parser::resolve_signature_help(
                &analysis,
                &content,
                position.line,
                position.character,
            )
        })
        .await
        .ok()
        .flatten();

        Ok(help)
    }

    // ── textDocument/prepareRename ───────────────────────────────────────────
    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;

        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c.clone(),
            None => return Ok(None),
        };
        drop(docs);

        let range = tokio::task::spawn_blocking(move || {
            lsp_parser::find_renameable_range_at(&content, position.line, position.character)
        })
        .await
        .ok()
        .flatten();

        Ok(range.map(PrepareRenameResponse::Range))
    }

    // ── textDocument/rename ───────────────────────────────────────────────────
    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c.clone(),
            None => return Ok(None),
        };
        drop(docs);

        let ranges = tokio::task::spawn_blocking(move || {
            lsp_parser::find_word_occurrences_at(&content, position.line, position.character)
        })
        .await
        .ok()
        .flatten();

        let ranges = match ranges {
            Some(ranges) if !ranges.is_empty() => ranges,
            _ => return Ok(None),
        };

        let edits: Vec<TextEdit> = ranges
            .into_iter()
            .map(|range| TextEdit {
                range,
                new_text: new_name.clone(),
            })
            .collect();

        let mut changes = HashMap::new();
        changes.insert(uri, edits);

        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }))
    }

    // ── textDocument/references ──────────────────────────────────────────────
    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c.clone(),
            None => return Ok(None),
        };
        drop(docs);

        let ranges = tokio::task::spawn_blocking(move || {
            lsp_parser::find_word_occurrences_at(&content, position.line, position.character)
        })
        .await
        .ok()
        .flatten();

        match ranges {
            Some(ranges) if !ranges.is_empty() => {
                let locations = ranges
                    .into_iter()
                    .map(|range| Location {
                        uri: uri.clone(),
                        range,
                    })
                    .collect();
                Ok(Some(locations))
            }
            _ => Ok(None),
        }
    }
}

// ══════════════════════════════════════════════════════════════════════════
// ── DIAGNOSTICS ───────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

impl Backend {
    /// Compute diagnostics for a document by parsing and type-checking, and
    /// cache the resulting `Context` keyed by this content's hash so that a
    /// hover/goto-definition/signatureHelp request against the *same*
    /// unedited text (the common case: those fire right after diagnostics,
    /// with no keystroke in between) can skip straight to a cheap lookup
    /// instead of re-running parse→metaprogram→typing from scratch.
    async fn compute_diagnostics(&self, content: &str, uri: &Uri) -> Vec<Diagnostic> {
        let content_owned = content.to_string();
        let file_name = uri.to_string();

        let (diagnostics, context, ast) = tokio::task::spawn_blocking(move || {
            check_code_and_extract_errors(&content_owned, &file_name)
        })
        .await
        .unwrap_or_else(|_| {
            // If the blocking task panicked, return a generic diagnostic
            (
                vec![Diagnostic {
                    range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: "Internal error while checking code".to_string(),
                    source: Some("typr".to_string()),
                    ..Default::default()
                }],
                None,
                None,
            )
        });

        if let (Some(context), Some(ast)) = (context, ast) {
            self.store_analysis(uri, content, lsp_parser::DocumentAnalysis { context, ast })
                .await;
        }

        diagnostics
    }

    /// Look up a cached `DocumentAnalysis` for `uri`, but only if it was
    /// computed from exactly this `content` (compared via a content hash,
    /// not the full string, to keep the cache cheap to consult).
    async fn get_cached_analysis(
        &self,
        uri: &Uri,
        content: &str,
    ) -> Option<Arc<lsp_parser::DocumentAnalysis>> {
        let hash = hash_content(content);
        let cache = self.analysis_cache.read().await;
        cache
            .get(uri)
            .filter(|(cached_hash, _)| *cached_hash == hash)
            .map(|(_, analysis)| analysis.clone())
    }

    /// Store a freshly computed `DocumentAnalysis` for `uri`, keyed by a hash
    /// of the `content` it was computed from.
    async fn store_analysis(
        &self,
        uri: &Uri,
        content: &str,
        analysis: lsp_parser::DocumentAnalysis,
    ) {
        let hash = hash_content(content);
        let mut cache = self.analysis_cache.write().await;
        cache.insert(uri.clone(), (hash, Arc::new(analysis)));
    }

    /// Get a `DocumentAnalysis` for `uri`/`content`, reusing the cached one
    /// from the last `compute_diagnostics` pass when it's still fresh, or
    /// running the parse→metaprogram→typing pipeline (and caching the
    /// result) otherwise. Used by hover/goto-definition/signatureHelp, which
    /// all need the same `Context` and previously each rebuilt it from
    /// scratch on every request.
    async fn analysis_for(
        &self,
        uri: &Uri,
        content: &str,
        file_path: &str,
    ) -> Option<Arc<lsp_parser::DocumentAnalysis>> {
        if let Some(analysis) = self.get_cached_analysis(uri, content).await {
            return Some(analysis);
        }

        let content_owned = content.to_string();
        let file_path_owned = file_path.to_string();
        let analysis = tokio::task::spawn_blocking(move || {
            lsp_parser::analyze_document(&content_owned, &file_path_owned)
        })
        .await
        .ok()
        .flatten()?;

        self.store_analysis(uri, content, analysis.clone()).await;
        Some(Arc::new(analysis))
    }
}

/// A cheap, non-cryptographic hash of document content, used only to detect
/// whether a cached `DocumentAnalysis` is still valid for the current text.
fn hash_content(content: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}

/// Check the code and extract errors from parsing and type-checking.
/// Also returns the final `Context` when type-checking ran to completion
/// (even if it collected errors along the way), so the caller can populate
/// the per-URI analysis cache shared with hover/goto-definition/signatureHelp.
fn check_code_and_extract_errors(
    content: &str,
    file_name: &str,
) -> (Vec<Diagnostic>, Option<Context>, Option<Lang>) {
    let mut diagnostics = Vec::new();

    // Convert URI to file path if needed
    let path = if file_name.starts_with("file://") {
        file_name.strip_prefix("file://").unwrap_or(file_name)
    } else {
        file_name
    };

    // 1. Attempt parsing
    let span: Span = LocatedSpan::new_extra(content, path.to_string());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));

    let mut ast = match parse_result {
        Ok(result) => {
            // Collect syntax errors from the parsed AST
            for syntax_error in &result.errors {
                let msg = syntax_error.simple_message();
                let range = if let Some(help_data) = syntax_error.get_help_data() {
                    let offset = help_data.get_offset();
                    let pos = offset_to_position(offset, content);
                    let end_col = find_token_end(content, offset, pos);
                    Range::new(pos, Position::new(pos.line, end_col))
                } else {
                    Range::new(Position::new(0, 0), Position::new(0, 1))
                };
                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::WARNING),
                    message: msg,
                    source: Some("typr".to_string()),
                    ..Default::default()
                });
            }
            result.ast
        }
        Err(panic_info) => {
            // Extract diagnostic from the panic
            if let Some(diagnostic) = extract_diagnostic_from_panic(&panic_info, content) {
                diagnostics.push(diagnostic);
            } else {
                diagnostics.push(Diagnostic {
                    range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: "Syntax error in code".to_string(),
                    source: Some("typr".to_string()),
                    ..Default::default()
                });
            }
            return (diagnostics, None, None);
        }
    };

    // 1b. Resolve module imports the same way the CLI does, so that `use M::x`
    // and other cross-module references are known to the type checker. Without
    // this the LSP would flag every imported symbol as an "undefined variable"
    // even though the code compiles fine. Mirrors `lsp_parser::analyze_document`.
    let environment = lsp_parser::detect_environment(path);
    if let Ok(expanded) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::metaprogramming::metaprogrammation(ast.clone(), environment)
    })) {
        ast = expanded;
    }

    // 2. Attempt type checking with error collection
    let context = Context::default().set_environment(environment);
    let typing_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        typr_core::typing_with_errors(&context, &ast)
    }));

    let mut final_context = None;
    match typing_result {
        Ok(result) => {
            use typr_core::TypRError;

            final_context = Some(result.get_context().clone());
            for error in result.get_errors() {
                // Skip errors that originate from an imported module file: their
                // offsets are relative to *that* file and would map to bogus
                // positions in this buffer. Only report errors for the file the
                // editor is currently showing (empty file name = this buffer).
                if let Some(help_data) = error.get_help_data() {
                    let err_file = help_data.get_file_name();
                    if !err_file.is_empty() && err_file != path {
                        continue;
                    }
                }
                let msg = error.simple_message();
                let severity = match error {
                    TypRError::Type(_) => DiagnosticSeverity::ERROR,
                    TypRError::Syntax(_) => DiagnosticSeverity::WARNING,
                };
                let range = if let Some(help_data) = error.get_help_data() {
                    let offset = help_data.get_offset();
                    let pos = offset_to_position(offset, content);
                    let end_col = find_token_end(content, offset, pos);
                    Range::new(pos, Position::new(pos.line, end_col))
                } else {
                    Range::new(Position::new(0, 0), Position::new(0, 1))
                };
                diagnostics.push(Diagnostic {
                    range,
                    severity: Some(severity),
                    message: msg,
                    source: Some("typr".to_string()),
                    ..Default::default()
                });
            }
        }
        Err(panic_info) => {
            if let Some(diagnostic) = extract_diagnostic_from_panic(&panic_info, content) {
                diagnostics.push(diagnostic);
            } else {
                diagnostics.push(Diagnostic {
                    range: Range::new(Position::new(0, 0), Position::new(0, 1)),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: "Type error in code".to_string(),
                    source: Some("typr".to_string()),
                    ..Default::default()
                });
            }
        }
    }

    let final_ast = final_context.is_some().then(|| ast.clone());
    (diagnostics, final_context, final_ast)
}

/// Extract an LSP diagnostic from a panic payload.
///
/// A handful of production panic sites in `typr-core` panic with a structured
/// `SyntaxError`/`TypeError` value (via `std::panic::panic_any`) rather than a
/// rendered string, so that the LSP can recover an exact `HelpData` offset
/// instead of reverse-engineering one out of miette's ANSI report text. Try
/// those first; only fall back to the brittle text-scraping path
/// (`extract_position_from_error`) for panics that carry a plain string
/// (e.g. `.unwrap()`/`panic!("...")` with no structured payload).
fn extract_diagnostic_from_panic(
    panic_info: &Box<dyn std::any::Any + Send>,
    content: &str,
) -> Option<Diagnostic> {
    if let Some(err) = panic_info.downcast_ref::<SyntaxError>() {
        return Some(diagnostic_from_help_data(
            err.get_help_data(),
            err.simple_message(),
            DiagnosticSeverity::WARNING,
            content,
        ));
    }
    if let Some(err) = panic_info.downcast_ref::<TypeError>() {
        return Some(diagnostic_from_help_data(
            err.get_help_data(),
            err.simple_message(),
            DiagnosticSeverity::ERROR,
            content,
        ));
    }

    let message = if let Some(s) = panic_info.downcast_ref::<String>() {
        s.as_str()
    } else if let Some(s) = panic_info.downcast_ref::<&str>() {
        *s
    } else {
        return None;
    };

    let range = extract_position_from_error(message, content)
        .unwrap_or_else(|| Range::new(Position::new(0, 0), Position::new(0, 1)));

    Some(Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        message: clean_error_message(message),
        source: Some("typr".to_string()),
        ..Default::default()
    })
}

/// Build a `Diagnostic` from an optional `HelpData` offset, falling back to
/// `(0,0)-(0,1)` when the error carries no position (matches the convention
/// already used for the non-panic error paths above).
fn diagnostic_from_help_data(
    help_data: Option<HelpData>,
    message: String,
    severity: DiagnosticSeverity,
    content: &str,
) -> Diagnostic {
    let range = match help_data {
        Some(help_data) => {
            let offset = help_data.get_offset();
            let pos = offset_to_position(offset, content);
            let end_col = find_token_end(content, offset, pos);
            Range::new(pos, Position::new(pos.line, end_col))
        }
        None => Range::new(Position::new(0, 0), Position::new(0, 1)),
    };

    Diagnostic {
        range,
        severity: Some(severity),
        message,
        source: Some("typr".to_string()),
        ..Default::default()
    }
}

/// Clean an error message for display in the LSP.
fn clean_error_message(msg: &str) -> String {
    let without_ansi = strip_ansi_codes(msg);

    for line in without_ansi.lines() {
        let trimmed = line.trim();
        if let Some(pos) = trimmed.find("× ") {
            let message = &trimmed[pos + 2..];
            return message.trim().trim_end_matches('.').to_string();
        }
    }

    without_ansi
        .lines()
        .find(|line| !line.trim().is_empty())
        .unwrap_or(&without_ansi)
        .trim()
        .to_string()
}

/// Strip ANSI escape codes from a string.
fn strip_ansi_codes(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\x1b' {
            if chars.peek() == Some(&'[') {
                chars.next();
                while let Some(&c) = chars.peek() {
                    chars.next();
                    if c.is_ascii_alphabetic() {
                        break;
                    }
                }
            }
        } else {
            result.push(ch);
        }
    }

    result
}

/// Attempt to extract position information from an error message.
fn extract_position_from_error(message: &str, content: &str) -> Option<Range> {
    let without_ansi = strip_ansi_codes(message);

    for line in without_ansi.lines() {
        if let Some(bracket_start) = line.find('[') {
            if let Some(bracket_end) = line[bracket_start..].find(']') {
                let location = &line[bracket_start + 1..bracket_start + bracket_end];

                if let Some(last_colon) = location.rfind(':') {
                    if let Some(second_last_colon) = location[..last_colon].rfind(':') {
                        let line_str = &location[second_last_colon + 1..last_colon];
                        let col_str = &location[last_colon + 1..];

                        if let (Ok(line_num), Ok(col_num)) =
                            (line_str.parse::<u32>(), col_str.parse::<u32>())
                        {
                            let line = line_num.saturating_sub(1);
                            let col = col_num.saturating_sub(1);
                            let length = extract_error_length(&without_ansi, content, line);

                            return Some(Range::new(
                                Position::new(line, col),
                                Position::new(line, col + length),
                            ));
                        }
                    }
                }
            }
        }
    }

    None
}

/// Try to extract the length of the error token from the miette diagram.
fn extract_error_length(message: &str, content: &str, line: u32) -> u32 {
    let mut found_line_number = false;
    let mut marker_col = None;

    for msg_line in message.lines() {
        let trimmed = msg_line.trim_start();

        if let Some(pipe_pos) = trimmed.find("│") {
            if let Ok(num) = trimmed[..pipe_pos].trim().parse::<u32>() {
                if num == line + 1 {
                    found_line_number = true;
                    continue;
                }
            }
        }

        if found_line_number && trimmed.contains("▲") {
            if let Some(marker_pos) = trimmed.find("▲") {
                marker_col = Some(marker_pos as u32);
                break;
            }
        }
    }

    if let Some(_col) = marker_col {
        if let Some(content_line) = content.lines().nth(line as usize) {
            return content_line
                .split_whitespace()
                .next()
                .map(|s| s.len() as u32)
                .unwrap_or(1);
        }
    }

    1
}

/// Convert a byte offset to a Position. Delegates to `lsp_parser`'s
/// implementation so there is a single, UTF-16-correct source of truth
/// (see its doc comment for why UTF-16 code units are used).
use lsp_parser::offset_to_position;

/// Find the end column of a token starting at the given byte offset.
/// Scans by `char`, not by byte, so a multi-byte UTF-8 sequence is never
/// split mid-character; the resulting length is in UTF-16 code units to
/// match `start_pos.character` (see `offset_to_position`).
fn find_token_end(content: &str, offset: usize, start_pos: Position) -> u32 {
    let rest = &content[offset..];
    let mut token_len_utf16 = 0u32;

    for ch in rest.chars() {
        if ch.is_whitespace() || ch == ';' || ch == ',' || ch == ')' || ch == ']' || ch == '}' {
            break;
        }
        token_len_utf16 += ch.len_utf16() as u32;
    }

    if token_len_utf16 == 0 {
        start_pos.character + 1
    } else {
        start_pos.character + token_len_utf16
    }
}

// ══════════════════════════════════════════════════════════════════════════
// ── WORKSPACE SYMBOLS ─────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

/// Get all symbols from a document for workspace/symbol support.
#[allow(deprecated)]
fn get_workspace_symbols(content: &str, file_uri: &Uri) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();

    let span: Span = LocatedSpan::new_extra(content, file_uri.path().to_string());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));

    let ast = match parse_result {
        Ok(result) => result.ast,
        Err(_) => return symbols,
    };

    collect_symbols_from_ast(&ast, content, file_uri, None, &mut symbols);

    symbols
}

/// Recursively collect symbols from an AST node.
#[allow(deprecated)]
fn collect_symbols_from_ast(
    lang: &Lang,
    content: &str,
    file_uri: &Uri,
    container_name: Option<String>,
    symbols: &mut Vec<SymbolInformation>,
) {
    match lang {
        Lang::Lines {
            value: statements, ..
        } => {
            for stmt in statements {
                collect_symbols_from_ast(stmt, content, file_uri, container_name.clone(), symbols);
            }
        }

        Lang::Scope {
            body: statements, ..
        } => {
            for stmt in statements {
                collect_symbols_from_ast(stmt, content, file_uri, container_name.clone(), symbols);
            }
        }

        Lang::Let {
            variable: var_lang,
            r#type: typ,
            expression: body,
            is_public: _,
            is_testable: _,
            is_export: _,
            help_data: _,
        } => {
            if let Ok(var) = Var::try_from(var_lang) {
                let name = var.get_name();
                let help_data = var.get_help_data();
                let offset = help_data.get_offset();
                let pos = offset_to_position(offset, content);
                let end_col = find_token_end(content, offset, pos);

                let kind = if typ.is_function() || body.is_function() {
                    SymbolKind::FUNCTION
                } else {
                    SymbolKind::VARIABLE
                };

                symbols.push(SymbolInformation {
                    name: name.clone(),
                    kind,
                    location: Location {
                        uri: file_uri.clone(),
                        range: Range::new(pos, Position::new(pos.line, end_col)),
                    },
                    deprecated: None,
                    container_name: container_name.clone(),
                    tags: None,
                });

                collect_symbols_from_ast(body, content, file_uri, Some(name), symbols);
            }
        }

        Lang::Alias {
            identifier: var_lang,
            ..
        } => {
            if let Ok(var) = Var::try_from(var_lang) {
                let name = var.get_name();
                let help_data = var.get_help_data();
                let offset = help_data.get_offset();
                let pos = offset_to_position(offset, content);
                let end_col = find_token_end(content, offset, pos);

                symbols.push(SymbolInformation {
                    name,
                    kind: SymbolKind::TYPE_PARAMETER,
                    location: Location {
                        uri: file_uri.clone(),
                        range: Range::new(pos, Position::new(pos.line, end_col)),
                    },
                    deprecated: None,
                    container_name: container_name.clone(),
                    tags: None,
                });
            }
        }

        Lang::Module {
            name,
            body: members,
            help_data,
            ..
        } => {
            let offset = help_data.get_offset();
            let pos = offset_to_position(offset, content);
            let end_col = pos.character + name.len() as u32;

            symbols.push(SymbolInformation {
                name: name.clone(),
                kind: SymbolKind::MODULE,
                location: Location {
                    uri: file_uri.clone(),
                    range: Range::new(pos, Position::new(pos.line, end_col)),
                },
                deprecated: None,
                container_name: container_name.clone(),
                tags: None,
            });

            for member in members {
                collect_symbols_from_ast(member, content, file_uri, Some(name.clone()), symbols);
            }
        }

        Lang::Signature {
            identifier: var, ..
        } => {
            let name = var.get_name();
            let help_data = var.get_help_data();
            let offset = help_data.get_offset();
            let pos = offset_to_position(offset, content);
            let end_col = find_token_end(content, offset, pos);

            symbols.push(SymbolInformation {
                name,
                kind: SymbolKind::FUNCTION,
                location: Location {
                    uri: file_uri.clone(),
                    range: Range::new(pos, Position::new(pos.line, end_col)),
                },
                deprecated: None,
                container_name: container_name.clone(),
                tags: None,
            });
        }

        Lang::Function { body, .. } => {
            collect_symbols_from_ast(body, content, file_uri, container_name, symbols);
        }

        _ => {}
    }
}

// ══════════════════════════════════════════════════════════════════════════
// ── DOCUMENT SYMBOLS ──────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

/// Get the (nested) document-outline symbols for a single document.
fn get_document_symbols(content: &str) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    let span: Span = LocatedSpan::new_extra(content, String::new());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));

    let ast = match parse_result {
        Ok(result) => result.ast,
        Err(_) => return symbols,
    };

    collect_document_symbols_from_ast(&ast, content, &mut symbols);

    symbols
}

/// Recursively collect a nested `DocumentSymbol` tree from an AST node.
/// Mirrors `collect_symbols_from_ast`, but nests children under their
/// enclosing `let`/`module` instead of flattening with a `container_name`.
#[allow(deprecated)]
fn collect_document_symbols_from_ast(
    lang: &Lang,
    content: &str,
    symbols: &mut Vec<DocumentSymbol>,
) {
    match lang {
        Lang::Lines {
            value: statements, ..
        } => {
            for stmt in statements {
                collect_document_symbols_from_ast(stmt, content, symbols);
            }
        }

        Lang::Scope {
            body: statements, ..
        } => {
            for stmt in statements {
                collect_document_symbols_from_ast(stmt, content, symbols);
            }
        }

        Lang::Let {
            variable: var_lang,
            r#type: typ,
            expression: body,
            is_public: _,
            is_testable: _,
            is_export: _,
            help_data: _,
        } => {
            if let Ok(var) = Var::try_from(var_lang) {
                let name = var.get_name();
                let help_data = var.get_help_data();
                let offset = help_data.get_offset();
                let pos = offset_to_position(offset, content);
                let end_col = find_token_end(content, offset, pos);
                let range = Range::new(pos, Position::new(pos.line, end_col));

                let kind = if typ.is_function() || body.is_function() {
                    SymbolKind::FUNCTION
                } else {
                    SymbolKind::VARIABLE
                };

                let mut children = Vec::new();
                collect_document_symbols_from_ast(body, content, &mut children);

                symbols.push(DocumentSymbol {
                    name,
                    detail: None,
                    kind,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: (!children.is_empty()).then_some(children),
                });
            }
        }

        Lang::Alias {
            identifier: var_lang,
            ..
        } => {
            if let Ok(var) = Var::try_from(var_lang) {
                let name = var.get_name();
                let help_data = var.get_help_data();
                let offset = help_data.get_offset();
                let pos = offset_to_position(offset, content);
                let end_col = find_token_end(content, offset, pos);
                let range = Range::new(pos, Position::new(pos.line, end_col));

                symbols.push(DocumentSymbol {
                    name,
                    detail: None,
                    kind: SymbolKind::TYPE_PARAMETER,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range: range,
                    children: None,
                });
            }
        }

        Lang::Module {
            name,
            body: members,
            help_data,
            ..
        } => {
            let offset = help_data.get_offset();
            let pos = offset_to_position(offset, content);
            let end_col = pos.character + name.len() as u32;
            let range = Range::new(pos, Position::new(pos.line, end_col));

            let mut children = Vec::new();
            for member in members {
                collect_document_symbols_from_ast(member, content, &mut children);
            }

            symbols.push(DocumentSymbol {
                name: name.clone(),
                detail: None,
                kind: SymbolKind::MODULE,
                tags: None,
                deprecated: None,
                range,
                selection_range: range,
                children: (!children.is_empty()).then_some(children),
            });
        }

        Lang::Signature {
            identifier: var, ..
        } => {
            let name = var.get_name();
            let help_data = var.get_help_data();
            let offset = help_data.get_offset();
            let pos = offset_to_position(offset, content);
            let end_col = find_token_end(content, offset, pos);
            let range = Range::new(pos, Position::new(pos.line, end_col));

            symbols.push(DocumentSymbol {
                name,
                detail: None,
                kind: SymbolKind::FUNCTION,
                tags: None,
                deprecated: None,
                range,
                selection_range: range,
                children: None,
            });
        }

        Lang::Function { body, .. } => {
            collect_document_symbols_from_ast(body, content, symbols);
        }

        _ => {}
    }
}

/// Start the LSP server.  Blocks until the client disconnects.
pub async fn run_lsp() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
        analysis_cache: Arc::new(RwLock::new(HashMap::new())),
        diagnostics_generation: Arc::new(RwLock::new(HashMap::new())),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A token containing a multi-byte char must not have its scan split
    /// mid-character, and its length must be reported in UTF-16 units (not
    /// bytes) to stay consistent with `offset_to_position`'s start column.
    #[test]
    fn find_token_end_handles_multibyte_token() {
        let content = "café;";
        let pos = offset_to_position(0, content);
        let end_col = find_token_end(content, 0, pos);
        // "café" is 4 Unicode scalar values, all 1 UTF-16 unit each.
        assert_eq!(end_col, 4);
    }

    /// 'Å' (U+00C5) encodes to UTF-8 bytes `0xC3 0x85`; its second byte,
    /// `0x85`, is U+0085 (NEL) when cast directly to `char` — and NEL is
    /// Unicode whitespace. Byte-casting scanning would therefore truncate
    /// the token mid-character; scanning by `char` must not.
    #[test]
    fn find_token_end_does_not_truncate_inside_multibyte_char() {
        let content = "wÅrd;";
        let pos = offset_to_position(0, content);
        let end_col = find_token_end(content, 0, pos);
        assert_eq!(end_col, 4, "token 'wÅrd' must not be cut short at 'Å'");
    }

    /// `use` of a member from an inline module must not produce an
    /// "Undefined variable" diagnostic (regression for the editor showing
    /// false errors on `use` import lines).
    #[test]
    fn use_inline_module_member_is_not_undefined() {
        let content = "\
module Math {
    @pub let pi <- 3;
};
use Math::pi;
let x <- pi;
";
        let (diags, _, _) = check_code_and_extract_errors(content, "test.ty");
        let undefined: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("Undefined"))
            .collect();
        assert!(
            undefined.is_empty(),
            "unexpected undefined diagnostics: {:?}",
            undefined
        );
    }

    /// A genuine type error in the current file must still be reported (the
    /// import-aware resolution and cross-file filtering must not silence real
    /// errors that belong to this buffer).
    #[test]
    fn genuine_type_error_in_current_file_is_still_reported() {
        let content = "let x: int <- \"hello\";\n";
        let (diags, _, _) = check_code_and_extract_errors(content, "test.ty");
        assert!(
            !diags.is_empty(),
            "expected at least one diagnostic for a type error, got none"
        );
    }

    /// A `use` referencing a member of a module imported from another file via
    /// `mod`/`use` must not be flagged undefined: the diagnostics path resolves
    /// imports through `metaprogrammation`, like the CLI. This is the core
    /// regression — the editor previously showed false errors on `use` lines.
    #[test]
    fn use_of_module_imported_from_another_file_is_not_undefined() {
        let dir = std::env::temp_dir().join(format!("typr_lsp_test_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let module_file = dir.join("math.ty");
        std::fs::write(&module_file, "@pub let pi <- 3;\n").unwrap();

        let main_file = dir.join("main.ty");
        let content = "mod math;\nuse math::pi;\nlet x <- pi;\n";
        std::fs::write(&main_file, content).unwrap();

        let (diags, _, _) = check_code_and_extract_errors(content, main_file.to_str().unwrap());

        let _ = std::fs::remove_dir_all(&dir);

        let undefined: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("Undefined"))
            .collect();
        assert!(
            undefined.is_empty(),
            "imported module member wrongly flagged undefined: {:?}",
            undefined
        );
    }

    /// In Project mode (DESCRIPTION + NAMESPACE present), `mod foo;` resolves
    /// to `TypR/foo.ty` relative to the detected project root, not the LSP
    /// server process's cwd (which doesn't necessarily match the project
    /// root, unlike CLI invocations that are always run from it).
    #[test]
    fn use_of_module_imported_in_project_mode_is_not_undefined() {
        let dir = std::env::temp_dir().join(format!("typr_lsp_proj_test_{}", std::process::id()));
        let typr_dir = dir.join("TypR");
        let _ = std::fs::create_dir_all(&typr_dir);
        std::fs::write(dir.join("DESCRIPTION"), "Package: x\n").unwrap();
        std::fs::write(dir.join("NAMESPACE"), "").unwrap();
        std::fs::write(
            typr_dir.join("person.ty"),
            "@pub let make_person <- fn(name: char): char { name };\n",
        )
        .unwrap();

        let main_file = typr_dir.join("main.ty");
        let content = "mod person;\nuse person::make_person;\nlet x <- make_person(\"a\");\n";
        std::fs::write(&main_file, content).unwrap();

        let (diags, _, _) = check_code_and_extract_errors(content, main_file.to_str().unwrap());

        let _ = std::fs::remove_dir_all(&dir);

        let undefined: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("Undefined"))
            .collect();
        assert!(
            undefined.is_empty(),
            "imported module member wrongly flagged undefined: {:?}",
            undefined
        );
    }

    /// `documentSymbol` must report top-level `let`/`module`/`type` symbols,
    /// and nest a module's members under it as `children` rather than
    /// flattening them (the tree shape `documentSymbol` is expected to return,
    /// unlike `workspace/symbol`'s flat `SymbolInformation` list).
    #[test]
    fn document_symbols_nest_module_members() {
        let content = "\
type Point <- list { x: int, y: int };
module Math {
    @pub let pi <- 3;
};
let x <- 5;
";
        let symbols = get_document_symbols(content);

        let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
        assert_eq!(names, vec!["Point", "Math", "x"]);

        let math = symbols.iter().find(|s| s.name == "Math").unwrap();
        assert_eq!(math.kind, SymbolKind::MODULE);
        let children = math.children.as_ref().expect("Math should have children");
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "pi");
    }

    /// A missing function return type (`fn(...): { ... }` with no type after
    /// the `:`) panics inside `typr-core`'s parser with a structured
    /// `SyntaxError::FunctionWithoutReturnType` payload (via
    /// `std::panic::panic_any`), not a rendered miette string. The LSP must
    /// recover the exact `HelpData` offset from that payload rather than
    /// falling back to the `(0,0)-(0,1)` default used when no position can
    /// be recovered at all.
    #[test]
    fn missing_return_type_panic_yields_precise_position() {
        let content = "let f <- fn(x: int): { x };\n";
        let (diags, _, _) = check_code_and_extract_errors(content, "test.ty");

        assert!(!diags.is_empty(), "expected a diagnostic, got none");
        let diag = &diags[0];
        assert_ne!(
            diag.range.start,
            Position::new(0, 0),
            "position should be recovered from the panic's structured SyntaxError, not defaulted"
        );
        assert!(
            diag.message.contains("return type"),
            "unexpected message: {}",
            diag.message
        );
    }

    /// Build a `Backend` with a real (but unconnected) `Client` for testing
    /// the cache helper methods directly, without driving the LSP transport.
    fn test_backend() -> Backend {
        let (service, _socket) = LspService::new(|client| Backend {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
            analysis_cache: Arc::new(RwLock::new(HashMap::new())),
            diagnostics_generation: Arc::new(RwLock::new(HashMap::new())),
        });
        service.inner().clone()
    }

    /// `analysis_for` must populate the cache on a miss, and a second call
    /// with the *same* content must hit it (P5 perf fix: hover/goto-def/
    /// signatureHelp skip a full re-parse + re-type-check when the document
    /// hasn't changed since the last analysis).
    #[tokio::test]
    async fn analysis_for_caches_unchanged_content() {
        let backend = test_backend();
        let uri = "file:///test.ty".parse::<Uri>().unwrap();
        // Two statements, not one: a lone top-level statement hits a
        // pre-existing typr-core quirk where `Lang::Lines` with exactly one
        // entry doesn't thread that entry's context updates through (see
        // `typing()`'s `exprs.len() == 1` branch) — unrelated to this cache.
        let content = "let x <- 5;\nlet y <- x;\n";

        assert!(
            backend.get_cached_analysis(&uri, content).await.is_none(),
            "cache must start empty"
        );

        let analysis = backend
            .analysis_for(&uri, content, "test.ty")
            .await
            .expect("analysis should succeed for valid code");
        assert!(!analysis.context.get_types_from_name("x").is_empty());

        // Second call with identical content must come straight from the cache.
        let cached = backend
            .get_cached_analysis(&uri, content)
            .await
            .expect("unchanged content must hit the cache populated above");
        assert!(!cached.context.get_types_from_name("x").is_empty());
    }

    /// Changing the document's content must invalidate the cache entry: a
    /// stale `Context` from before the edit would resolve hover/definitions
    /// against outdated bindings.
    #[tokio::test]
    async fn analysis_for_invalidates_cache_on_content_change() {
        let backend = test_backend();
        let uri = "file:///test.ty".parse::<Uri>().unwrap();
        let original = "let x <- 5;\nlet z <- x;\n";
        let edited = "let y <- 5;\nlet z <- y;\n";

        backend
            .analysis_for(&uri, original, "test.ty")
            .await
            .expect("analysis should succeed for valid code");

        assert!(
            backend.get_cached_analysis(&uri, edited).await.is_none(),
            "edited content must not hit the cache entry from before the edit"
        );

        let analysis = backend
            .analysis_for(&uri, edited, "test.ty")
            .await
            .expect("analysis should succeed for the edited code");
        assert!(!analysis.context.get_types_from_name("y").is_empty());
    }
}
