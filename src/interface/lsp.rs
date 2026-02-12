//! LSP server for TypR.
//!
//! Currently exposes:
//!   - **Hover** provider: shows inferred types with Markdown syntax highlighting
//!   - **Completion** provider: context-aware autocompletion for variables, functions, and type aliases
//!     - Trigger characters: `.`, `$`, `>` (for `|>`), `:` (for type annotations)
//!   - **Diagnostics** provider: real-time error checking for syntax and type errors
//!   - **Go to Definition** provider: jump to symbol definitions (variables, functions, type aliases)
//!   - **Workspace Symbol** provider: search for symbols across all open documents
//!
//! Launch with `typr lsp`.  The server communicates over stdin/stdout using
//! the standard LSP JSON-RPC protocol.

use super::parser;
use crate::components::context::Context;
use crate::processes::parsing::parse;
use nom_locate::LocatedSpan;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

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
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        ..Default::default()
                    }
                )),
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
        self.client.publish_diagnostics(uri, diagnostics, None).await;
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
            self.client.publish_diagnostics(uri, diagnostics, None).await;
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
            parser::find_type_at(&content_owned, position.line, position.character)
        })
        .await
        .ok()  // if the blocking task panicked, treat as None
        .flatten();

        match info {
            Some(hover_info) => Ok(Some(Hover {
                // Use MarkupContent so the editor renders the Markdown
                // (bold, italic, code spans) produced by highlight_type().
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
            parser::get_completions_at(&content_owned, position.line, position.character)
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

        // Offload parsing + typing to a blocking thread so we don't stall
        // the LSP event loop.
        let content_owned = content.clone();
        let uri_owned = uri.clone();
        let info = tokio::task::spawn_blocking(move || {
            parser::find_definition_at(&content_owned, position.line, position.character)
        })
        .await
        .ok()
        .flatten();

        match info {
            Some(def_info) => Ok(Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri_owned,
                range: def_info.range,
            }))),
            None => Ok(None),
        }
    }
}

// ══════════════════════════════════════════════════════════════════════════
// ── DIAGNOSTICS ───────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

impl Backend {
    /// Compute diagnostics for a document by parsing and type-checking.
    ///
    /// This is called on `did_open` and `did_change` events to provide
    /// real-time error feedback to the user.
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
///
/// This function attempts to:
///   1. Parse the code using the TypR parser
///   2. Type-check the AST using the TypR type checker
///
/// If either step fails (panics), the panic is caught and converted into
/// an LSP diagnostic.
///
/// The `file_name` parameter is used by miette to include position information
/// in error messages (e.g., `[file.ty:4:1]`).
///
/// **Note**: We need to save the file to disk temporarily so that the error
/// system can read it back when formatting error messages.
fn check_code_and_extract_errors(content: &str, file_name: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    
    // Convert URI to file path if needed
    let path = if file_name.starts_with("file://") {
        file_name.strip_prefix("file://").unwrap_or(file_name)
    } else {
        file_name
    };
    
    // Note: We don't write to disk here to avoid triggering file change warnings in editors.
    // The error system should work with in-memory content provided by the LSP.
    // let _ = std::fs::write(path, content);
    
    // 1. Attempt parsing
    // Pass the file path (not URI) so HelpData can store it for error messages
    let span: Span = LocatedSpan::new_extra(content, path.to_string());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    
    let ast = match parse_result {
        Ok(result) => {
            // Collect syntax errors from the parsed AST
            for syntax_error in &result.errors {
                // Use simple_message() for LSP - it doesn't require file access
                let msg = syntax_error.simple_message();
                // Extract position from SyntaxError's HelpData
                let range = if let Some(help_data) = syntax_error.get_help_data() {
                    let offset = help_data.get_offset();
                    let pos = offset_to_position(offset, content);
                    // Create a range that spans a reasonable portion of the error
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
                // Fallback: generic syntax error
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
        crate::processes::type_checking::typing_with_errors(&context, &ast)
    }));
    
    match typing_result {
        Ok(result) => {
            // Collect type errors from the typing result
            use crate::components::error_message::typr_error::TypRError;
            
            for error in result.get_errors() {
                // Use simple_message() for LSP - it doesn't require file access
                let msg = error.simple_message();
                let severity = match error {
                    TypRError::Type(_) => DiagnosticSeverity::ERROR,
                    TypRError::Syntax(_) => DiagnosticSeverity::WARNING,
                };
                // Extract position from TypRError's HelpData
                let range = if let Some(help_data) = error.get_help_data() {
                    let offset = help_data.get_offset();
                    let pos = offset_to_position(offset, content);
                    // Try to find a reasonable end position (end of token or line)
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
            // Fallback for unexpected panics during type checking
            if let Some(diagnostic) = extract_diagnostic_from_panic(&panic_info, content) {
                diagnostics.push(diagnostic);
            } else {
                // Fallback: generic type error
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
///
/// The panic payload from TypR errors contains formatted error messages
/// (usually from the `miette` library). This function attempts to extract:
///   - The error message
///   - The position/range where the error occurred
///
/// Returns `None` if the panic payload cannot be interpreted.
fn extract_diagnostic_from_panic(
    panic_info: &Box<dyn std::any::Any + Send>,
    content: &str,
) -> Option<Diagnostic> {
    // The panic can be either a String or a &str
    let message = if let Some(s) = panic_info.downcast_ref::<String>() {
        s.as_str()
    } else if let Some(s) = panic_info.downcast_ref::<&str>() {
        *s
    } else {
        return None;
    };
    
    // Debug: uncomment to see the raw error message
    // eprintln!("=== RAW ERROR MESSAGE ===\n{}\n=== END ===", message);
    
    // Try to extract position information from the error message
    let range = extract_position_from_error(message, content)
        .unwrap_or_else(|| {
            // Fallback: point to the beginning of the file
            Range::new(Position::new(0, 0), Position::new(0, 1))
        });
    
    Some(Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        message: clean_error_message(message),
        source: Some("typr".to_string()),
        ..Default::default()
    })
}

/// Clean an error message for display in the LSP.
///
/// This removes ANSI color codes, miette formatting artifacts, and
/// extracts just the main error message.
///
/// Example input:
/// ```
/// thread 'main' panicked at src/processes/type_checking/function_application.rs:39:28:
/// Err(  × Type error: Function `+` not defined in this scope.
///    ╭─[TypR/main.ty:4:1]
///  3 │
///  4 │ 1 + true
///    · ▲
///    · ╰── Not defined in this scope
///    ╰────
/// )
/// ```
/// Should extract: "Type error: Function `+` not defined in this scope."
fn clean_error_message(msg: &str) -> String {
    // Remove ANSI escape codes
    let without_ansi = strip_ansi_codes(msg);
    
    // Look for the error message after "× " (miette marker)
    for line in without_ansi.lines() {
        let trimmed = line.trim();
        if let Some(pos) = trimmed.find("× ") {
            let message = &trimmed[pos + 2..];
            // Remove trailing period and trim whitespace
            return message.trim().trim_end_matches('.').to_string();
        }
    }
    
    // Fallback: take the first non-empty line
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
            // Skip ANSI escape sequence
            if chars.peek() == Some(&'[') {
                chars.next(); // consume '['
                // Skip until we find a letter (end of ANSI sequence)
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
///
/// TypR error messages contain miette-formatted position info like:
/// ```
///    ╭─[TypR/main.ty:4:1]
///  4 │ 1 + true
///    · ▲
/// ```
/// This extracts the line number (4) and tries to find the column from the "▲" marker.
fn extract_position_from_error(message: &str, content: &str) -> Option<Range> {
    let without_ansi = strip_ansi_codes(message);
    
    // Look for the position marker: "╭─[filename:line:col]" or just "[filename:line:col]"
    for line in without_ansi.lines() {
        // Find the opening bracket [ and closing bracket ]
        if let Some(bracket_start) = line.find('[') {
            if let Some(bracket_end) = line[bracket_start..].find(']') {
                let location = &line[bracket_start + 1..bracket_start + bracket_end];
                
                // Parse "TypR/main.ty:4:1" or "/path/to/file.ty:4:1" -> line 4, col 1
                if let Some(last_colon) = location.rfind(':') {
                    if let Some(second_last_colon) = location[..last_colon].rfind(':') {
                        let line_str = &location[second_last_colon + 1..last_colon];
                        let col_str = &location[last_colon + 1..];
                        
                        if let (Ok(line_num), Ok(col_num)) = (line_str.parse::<u32>(), col_str.parse::<u32>()) {
                            // miette uses 1-based indexing, LSP uses 0-based
                            let line = line_num.saturating_sub(1);
                            let col = col_num.saturating_sub(1);
                            
                            // Try to find the length of the error token
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
/// Looks for the "▲" or "╰──" markers to determine span length.
fn extract_error_length(message: &str, content: &str, line: u32) -> u32 {
    // Look for lines with the error marker "▲" or underline "╰──"
    let mut found_line_number = false;
    let mut marker_col = None;
    
    for msg_line in message.lines() {
        let trimmed = msg_line.trim_start();
        
        // Check if this line shows the line number we're looking for
        if let Some(pipe_pos) = trimmed.find("│") {
            if let Ok(num) = trimmed[..pipe_pos].trim().parse::<u32>() {
                if num == line + 1 {
                    found_line_number = true;
                    continue;
                }
            }
        }
        
        // If we found the line, look for the marker on the next line
        if found_line_number && trimmed.contains("▲") {
            // Count spaces before the marker to find column
            if let Some(marker_pos) = trimmed.find("▲") {
                marker_col = Some(marker_pos as u32);
                break;
            }
        }
    }
    
    // If we found a marker column, try to get the token length from the actual content
    if let Some(_col) = marker_col {
        // Get the actual line from content
        if let Some(content_line) = content.lines().nth(line as usize) {
            // Try to find the token at that position
            // For now, return a reasonable default length
            return content_line.trim().split_whitespace()
                .next()
                .map(|s| s.len() as u32)
                .unwrap_or(1);
        }
    }
    
    // Default: single character
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
/// This helps create a more accurate range for the diagnostic.
fn find_token_end(content: &str, offset: usize, start_pos: Position) -> u32 {
    let bytes = content.as_bytes();
    let mut end_offset = offset;
    
    // Skip until we find a token boundary (whitespace, punctuation, or end of line)
    while end_offset < bytes.len() {
        let ch = bytes[end_offset] as char;
        if ch.is_whitespace() || ch == ';' || ch == ',' || ch == ')' || ch == ']' || ch == '}' {
            break;
        }
        end_offset += 1;
    }
    
    // Calculate the end column
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

use crate::components::language::Lang;
use crate::components::language::var::Var;

/// Get all symbols from a document for workspace/symbol support.
///
/// This parses the document and extracts all top-level symbols including:
///   - Let bindings (variables and functions)
///   - Type aliases
///   - Modules
///   - Signatures
#[allow(deprecated)]
fn get_workspace_symbols(content: &str, file_uri: &Url) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    
    // Parse the document
    let span: Span = LocatedSpan::new_extra(content, file_uri.path().to_string());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    
    let ast = match parse_result {
        Ok(result) => result.ast,
        Err(_) => return symbols, // If parsing fails, return empty symbols
    };
    
    // Collect symbols from the AST
    collect_symbols_from_ast(&ast, content, file_uri, None, &mut symbols);
    
    symbols
}

/// Recursively collect symbols from an AST node.
///
/// This traverses the AST and extracts symbol information for:
///   - `Let` bindings: variables and functions
///   - `Alias`: type aliases
///   - `Module`: module definitions
///   - `Signature`: type signatures
///   - `Lines` and `Scope`: recursively process children
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
                
                // Determine symbol kind based on type annotation or body
                // Check both the annotated type and the body to determine if it's a function
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
                
                // Recursively check body for nested declarations (e.g., functions defined inside let)
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
            
            // Recursively process module members
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
            // Check for nested declarations in function body
            collect_symbols_from_ast(body, content, file_uri, container_name, symbols);
        }
        
        // Other variants don't define workspace-level symbols
        _ => {}
    }
}

/// Start the LSP server.  Blocks until the client disconnects.
///
/// This is an `async` function meant to be driven by a `tokio` runtime
/// (see `cli.rs` for the `block_on` call).
pub async fn run_lsp() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}

// ══════════════════════════════════════════════════════════════════════════
// ── TESTS ─────────────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_ansi_codes() {
        let input = "\x1b[31mError\x1b[0m: Something went wrong";
        let expected = "Error: Something went wrong";
        assert_eq!(strip_ansi_codes(input), expected);
    }

    #[test]
    fn test_strip_ansi_codes_no_ansi() {
        let input = "Plain text without ANSI codes";
        assert_eq!(strip_ansi_codes(input), input);
    }

    #[test]
    fn test_clean_error_message() {
        let input = "\x1b[31m\nType error: int expected, got string\n\x1b[0m";
        let result = clean_error_message(input);
        assert_eq!(result, "Type error: int expected, got string");
    }

    #[test]
    fn test_clean_error_message_miette_format() {
        let input = r#"thread 'main' panicked at src/processes/type_checking/function_application.rs:39:28:
Err(  × Type error: Function `+` not defined in this scope.
   ╭─[TypR/main.ty:4:1]
 3 │
 4 │ 1 + true
   · ▲
   · ╰── Not defined in this scope
   ╰────
)"#;
        let result = clean_error_message(input);
        assert_eq!(result, "Type error: Function `+` not defined in this scope");
    }

    #[test]
    fn test_extract_position_from_miette_error() {
        let message = r#"Err(  × Type error: Function `+` not defined in this scope.
   ╭─[TypR/main.ty:4:1]
 3 │
 4 │ 1 + true
   · ▲
   · ╰── Not defined in this scope
   ╰────
)"#;
        let content = "line0\nline1\nline2\n1 + true\n";
        let range = extract_position_from_error(message, content);
        
        assert!(range.is_some());
        if let Some(r) = range {
            // Line 4 in 1-based = line 3 in 0-based
            assert_eq!(r.start.line, 3);
            // Column 1 in 1-based = column 0 in 0-based
            assert_eq!(r.start.character, 0);
        }
    }

    #[test]
    fn test_offset_to_position() {
        let content = "line 0\nline 1\nline 2";
        
        // Offset 0 should be at line 0, col 0
        assert_eq!(offset_to_position(0, content), Position::new(0, 0));
        
        // Offset 7 (just after first newline) should be at line 1, col 0
        assert_eq!(offset_to_position(7, content), Position::new(1, 0));
        
        // Offset 14 (just after second newline) should be at line 2, col 0
        assert_eq!(offset_to_position(14, content), Position::new(2, 0));
    }

    #[test]
    fn test_check_valid_code() {
        let code = "let x: int <- 42;";
        let diagnostics = check_code_and_extract_errors(code, "test.ty");
        // Valid code should produce no diagnostics
        assert_eq!(diagnostics.len(), 0);
    }

    #[test]
    fn test_check_syntax_error() {
        // Missing semicolon
        let code = "let x: int <- 42";
        let diagnostics = check_code_and_extract_errors(code, "test.ty");
        // Should produce at least one diagnostic
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn test_check_type_error() {
        // Type error: trying to use + with incompatible types
        let code = "1 + true;";
        let diagnostics = check_code_and_extract_errors(code, "test.ty");
        // Should produce at least one diagnostic
        assert!(!diagnostics.is_empty());
        if let Some(diag) = diagnostics.first() {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
            // Verify that the error position is extracted correctly
            // The error should be at position 0 (start of "1")
            assert_eq!(diag.range.start.line, 0);
        }
    }

    #[test]
    fn test_real_multiline_error() {
        // Real error case with multiple lines
        let code = "let a <- 1;\nlet b <- 2;\nlet c <- 3;\n1 + true;";
        let diagnostics = check_code_and_extract_errors(code, "test.ty");
        
        // Should detect the error
        assert!(!diagnostics.is_empty());
        
        if let Some(diag) = diagnostics.first() {
            assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
            assert!(!diag.message.is_empty());
            // The error "1 + true" is on line 4 (0-indexed: line 3)
            // Verify that the position is correctly extracted from HelpData
            assert_eq!(diag.range.start.line, 3, "Error should be on line 4 (0-indexed: 3)");
        }
    }

    #[test]
    fn test_error_position_extraction() {
        // Test that error positions are correctly extracted
        // Using "1 + true" which produces a FunctionNotFound error
        let code = "let x <- 1;\nlet y <- 2;\n1 + true;";
        let diagnostics = check_code_and_extract_errors(code, "test.ty");
        
        assert!(!diagnostics.is_empty());
        
        if let Some(diag) = diagnostics.first() {
            // The error "1 + true" is on line 3 (0-indexed: 2)
            assert_eq!(diag.range.start.line, 2, "Error should be on line 3 (0-indexed: 2)");
            // The error starts at column 0 (start of "1")
            assert_eq!(diag.range.start.character, 0);
        }
    }

    // ── workspace/symbol tests ───────────────────────────────────────────────

    #[test]
    fn test_workspace_symbols_let_binding() {
        let code = "let myVariable <- 42;";
        let uri = Url::parse("file:///test.ty").unwrap();
        let symbols = get_workspace_symbols(code, &uri);
        
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "myVariable");
        assert_eq!(symbols[0].kind, SymbolKind::VARIABLE);
    }

    #[test]
    fn test_workspace_symbols_function() {
        let code = "let add <- fn(a: int, b: int): int { a + b };";
        let uri = Url::parse("file:///test.ty").unwrap();
        let symbols = get_workspace_symbols(code, &uri);
        
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "add");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_workspace_symbols_type_alias() {
        let code = "type MyInt <- int;";
        let uri = Url::parse("file:///test.ty").unwrap();
        let symbols = get_workspace_symbols(code, &uri);
        
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "MyInt");
        assert_eq!(symbols[0].kind, SymbolKind::TYPE_PARAMETER);
    }

    #[test]
    fn test_workspace_symbols_multiple() {
        let code = "let x <- 1;\nlet y <- 2;\ntype MyType <- int;";
        let uri = Url::parse("file:///test.ty").unwrap();
        let symbols = get_workspace_symbols(code, &uri);
        
        assert_eq!(symbols.len(), 3);
        
        let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"x"));
        assert!(names.contains(&"y"));
        assert!(names.contains(&"MyType"));
    }

    #[test]
    fn test_workspace_symbols_empty() {
        let code = "42;";  // Just an expression, no symbols defined
        let uri = Url::parse("file:///test.ty").unwrap();
        let symbols = get_workspace_symbols(code, &uri);
        
        assert!(symbols.is_empty());
    }

    #[test]
    fn test_workspace_symbols_invalid_syntax() {
        let code = "let x <- ";  // Incomplete code
        let uri = Url::parse("file:///test.ty").unwrap();
        let symbols = get_workspace_symbols(code, &uri);
        
        // Should not panic, may return empty or partial results
        // Just verify it doesn't crash
        let _ = symbols;
    }
}
