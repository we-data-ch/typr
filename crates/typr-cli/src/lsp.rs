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
//!
//! Launch with `typr lsp`.  The server communicates over stdin/stdout using
//! the standard LSP JSON-RPC protocol.

use crate::lsp_parser;
use nom_locate::LocatedSpan;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use typr_core::components::context::Context;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::processes::parsing::parse;

type Span<'a> = LocatedSpan<&'a str, String>;

/// Shared state: one copy of each open document's full text.
struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, String>>>,
}

#[tower_lsp::async_trait]
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
                definition_provider: Some(OneOf::Left(true)),
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

            // Compute and publish diagnostics
            let diagnostics = self.compute_diagnostics(&content, &uri).await;
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    // ── hover ───────────────────────────────────────────────────────────────
    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        let content = match docs.get(&uri) {
            Some(c) => c,
            None => return Ok(None),
        };

        // Offload parsing + typing to a blocking thread so we don't stall
        // the LSP event loop.
        let content_owned = content.clone();
        let info = tokio::task::spawn_blocking(move || {
            lsp_parser::find_type_at(&content_owned, position.line, position.character)
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

        // Offload parsing + typing to a blocking thread (same strategy as hover).
        let content_owned = content.clone();
        let items = tokio::task::spawn_blocking(move || {
            lsp_parser::get_completions_at(&content_owned, position.line, position.character)
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
    ) -> Result<Option<Vec<SymbolInformation>>> {
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
            Ok(Some(all_symbols))
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
            Some(c) => c,
            None => return Ok(None),
        };

        // Extract the file path from the URI for cross-file definition lookup.
        let file_path = uri
            .to_file_path()
            .ok()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();

        // Offload parsing + typing to a blocking thread so we don't stall
        // the LSP event loop.
        let content_owned = content.clone();
        let uri_owned = uri.clone();
        let file_path_owned = file_path.clone();
        let info = tokio::task::spawn_blocking(move || {
            lsp_parser::find_definition_at(
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
                    Some(path) => Url::from_file_path(path).unwrap_or(uri_owned),
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
}

// ══════════════════════════════════════════════════════════════════════════
// ── DIAGNOSTICS ───────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

impl Backend {
    /// Compute diagnostics for a document by parsing and type-checking.
    async fn compute_diagnostics(&self, content: &str, uri: &Url) -> Vec<Diagnostic> {
        let content_owned = content.to_string();
        let file_name = uri.to_string();

        tokio::task::spawn_blocking(move || {
            check_code_and_extract_errors(&content_owned, &file_name)
        })
        .await
        .unwrap_or_else(|_| {
            // If the blocking task panicked, return a generic diagnostic
            vec![Diagnostic {
                range: Range::new(Position::new(0, 0), Position::new(0, 0)),
                severity: Some(DiagnosticSeverity::ERROR),
                message: "Internal error while checking code".to_string(),
                source: Some("typr".to_string()),
                ..Default::default()
            }]
        })
    }
}

/// Check the code and extract errors from parsing and type-checking.
fn check_code_and_extract_errors(content: &str, file_name: &str) -> Vec<Diagnostic> {
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

    let ast = match parse_result {
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
            return diagnostics;
        }
    };

    // 2. Attempt type checking with error collection
    let context = Context::default();
    let typing_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        typr_core::typing_with_errors(&context, &ast)
    }));

    match typing_result {
        Ok(result) => {
            use typr_core::TypRError;

            for error in result.get_errors() {
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

    diagnostics
}

/// Extract an LSP diagnostic from a panic payload.
fn extract_diagnostic_from_panic(
    panic_info: &Box<dyn std::any::Any + Send>,
    content: &str,
) -> Option<Diagnostic> {
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
                .trim()
                .split_whitespace()
                .next()
                .map(|s| s.len() as u32)
                .unwrap_or(1);
        }
    }

    1
}

/// Convert a character offset to a Position (line, column).
fn offset_to_position(offset: usize, content: &str) -> Position {
    let mut line = 0u32;
    let mut col = 0u32;

    for (i, ch) in content.chars().enumerate() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    Position::new(line, col)
}

/// Find the end column of a token starting at the given offset.
fn find_token_end(content: &str, offset: usize, start_pos: Position) -> u32 {
    let bytes = content.as_bytes();
    let mut end_offset = offset;

    while end_offset < bytes.len() {
        let ch = bytes[end_offset] as char;
        if ch.is_whitespace() || ch == ';' || ch == ',' || ch == ')' || ch == ']' || ch == '}' {
            break;
        }
        end_offset += 1;
    }

    let token_len = (end_offset - offset) as u32;
    if token_len == 0 {
        start_pos.character + 1
    } else {
        start_pos.character + token_len
    }
}

// ══════════════════════════════════════════════════════════════════════════
// ── WORKSPACE SYMBOLS ─────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

/// Get all symbols from a document for workspace/symbol support.
#[allow(deprecated)]
fn get_workspace_symbols(content: &str, file_uri: &Url) -> Vec<SymbolInformation> {
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
    file_uri: &Url,
    container_name: Option<String>,
    symbols: &mut Vec<SymbolInformation>,
) {
    match lang {
        Lang::Lines(statements, _) => {
            for stmt in statements {
                collect_symbols_from_ast(stmt, content, file_uri, container_name.clone(), symbols);
            }
        }

        Lang::Scope(statements, _) => {
            for stmt in statements {
                collect_symbols_from_ast(stmt, content, file_uri, container_name.clone(), symbols);
            }
        }

        Lang::Let(var_lang, typ, body, _) => {
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

        Lang::Alias(var_lang, _params, _typ, _) => {
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

        Lang::Module(name, members, _, _, help_data) => {
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

        Lang::Signature(var, _typ, _) => {
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

        Lang::Function(_, _, body, _) => {
            collect_symbols_from_ast(body, content, file_uri, container_name, symbols);
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
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
