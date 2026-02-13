//! Token resolution and Markdown-highlighted type display for LSP hover.
//!
//! Given the full source text and a cursor position (line, character),
//! this module:
//!   1. Identifies the word (identifier / literal) under the cursor.
//!   2. Parses and type-checks the whole document using the project's
//!      pipeline (`parse` → `typing`) to build a fully-populated `Context`.
//!   3. Looks up the identifier in that context and returns its type.
//!   4. Renders the type string with Markdown syntax highlighting, using
//!      the same semantic categories as the REPL highlighter in `repl.rs`.

use crate::components::context::Context;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::parsing::parse;
use crate::processes::type_checking::typing;
use nom_locate::LocatedSpan;
use tower_lsp::lsp_types::{Position, Range};

type Span<'a> = LocatedSpan<&'a str, String>;

/// A resolved hover result: the Markdown-highlighted type and the LSP range.
#[derive(Debug, Clone)]
pub struct HoverInfo {
    /// Markdown string ready to be sent as hover contents.
    pub type_display: String,
    /// The source range of the token that was resolved.
    pub range: Range,
}

/// A resolved definition result: the location where a symbol is defined.
#[derive(Debug, Clone)]
pub struct DefinitionInfo {
    /// The source range where the symbol is defined.
    pub range: Range,
}

// ── public entry-point ─────────────────────────────────────────────────────

/// Main entry-point called by the LSP hover handler.
///
/// Returns `None` when:
///   - the cursor is not on an identifier/literal, or
///   - parsing or type-checking fails (e.g. incomplete code).
pub fn find_type_at(content: &str, line: u32, character: u32) -> Option<HoverInfo> {
    // 1. Extract the word under the cursor.
    let (word, word_range) = extract_word_at(content, line, character)?;

    // 2. Parse the whole document.
    let span: Span = LocatedSpan::new_extra(content, String::new());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    let ast = parse_result.ok()?.ast;

    // 3. Type-check the whole document to build the context.
    let context = Context::default();
    let type_context =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)));
    let type_context = type_context.ok()?;
    let final_context = type_context.context;

    // 4. Look up the word in the context.
    let types = final_context.get_types_from_name(&word);

    let typ = if types.is_empty() {
        // Fallback: try to infer the type of the word as a literal.
        infer_literal_type(&word)?
    } else {
        // Pick the most specific (last) type when there are multiple overloads.
        types.last().unwrap().clone()
    };

    // 5. Render with Markdown highlighting.
    let highlighted = highlight_type(&typ.pretty());
    let markdown = format!(
        "**`{}`** : {}\n\n```\n{}\n```",
        word,         // variable name in bold code
        highlighted,  // inline Markdown-highlighted type
        typ.pretty()  // plain code-block fallback (always readable)
    );

    Some(HoverInfo {
        type_display: markdown,
        range: word_range,
    })
}

// ── definition lookup ──────────────────────────────────────────────────────

/// Main entry-point called by the LSP goto_definition handler.
///
/// Finds the definition location of the identifier under the cursor.
/// Returns `None` when:
///   - the cursor is not on an identifier, or
///   - parsing or type-checking fails, or
///   - the identifier is not found in the context (e.g., built-in or undefined).
pub fn find_definition_at(content: &str, line: u32, character: u32) -> Option<DefinitionInfo> {
    // 1. Extract the word under the cursor.
    let (word, _word_range) = extract_word_at(content, line, character)?;

    // 2. Parse the whole document.
    let span: Span = LocatedSpan::new_extra(content, String::new());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    let ast = parse_result.ok()?.ast;

    // 3. Type-check the whole document to build the context.
    let context = Context::default();
    let type_context =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)));
    let type_context = type_context.ok()?;
    let final_context = type_context.context;

    // 4. Look up the variable in the context to find its definition.
    // We need to find the Var that matches this name and get its HelpData.
    let definition_var = final_context
        .variables()
        .find(|(var, _)| var.get_name() == word)
        .map(|(var, _)| var.clone());

    // Also check aliases if not found in variables
    let definition_var = definition_var.or_else(|| {
        final_context
            .aliases()
            .find(|(var, _)| var.get_name() == word)
            .map(|(var, _)| var.clone())
    });

    let var = definition_var?;
    let help_data = var.get_help_data();
    let offset = help_data.get_offset();

    // 5. Convert offset to Position.
    let pos = offset_to_position(offset, content);
    let end_col = pos.character + word.len() as u32;

    Some(DefinitionInfo {
        range: Range::new(pos, Position::new(pos.line, end_col)),
    })
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

// ── word extraction ────────────────────────────────────────────────────────

/// Extract the contiguous word (identifier or numeric literal) that contains
/// the given cursor position.  Returns the word text and its LSP Range.
fn extract_word_at(content: &str, line: u32, character: u32) -> Option<(String, Range)> {
    let source_line = content.lines().nth(line as usize)?;

    if (character as usize) > source_line.len() {
        return None;
    }

    let bytes = source_line.as_bytes();
    let col = character as usize;

    // Ensure cursor is on a word character (or one position past one).
    if col >= bytes.len() || !is_word_char(bytes[col]) {
        if col == 0 {
            return None;
        }
        if !is_word_char(bytes[col - 1]) {
            return None;
        }
    }

    let anchor = if col < bytes.len() && is_word_char(bytes[col]) {
        col
    } else {
        col - 1
    };

    let start = {
        let mut i = anchor;
        while i > 0 && is_word_char(bytes[i - 1]) {
            i -= 1;
        }
        i
    };

    let end = {
        let mut i = anchor;
        while i + 1 < bytes.len() && is_word_char(bytes[i + 1]) {
            i += 1;
        }
        i + 1
    };

    let word = &source_line[start..end];
    if word.is_empty() {
        return None;
    }

    Some((
        word.to_string(),
        Range {
            start: Position::new(line, start as u32),
            end: Position::new(line, end as u32),
        },
    ))
}

/// A character is part of a word if it is alphanumeric, an underscore, or a dot
/// (TypR / R identifiers can contain dots).
fn is_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'.'
}

// ── literal fallback ───────────────────────────────────────────────────────

/// For numeric literals that are not in the context, return their literal type.
fn infer_literal_type(word: &str) -> Option<Type> {
    use crate::utils::builder;

    if let Ok(i) = word.parse::<i32>() {
        return Some(builder::integer_type(i));
    }
    if let Ok(_f) = word.parse::<f32>() {
        return Some(builder::number_type());
    }
    None
}

// ── Markdown type highlighter ──────────────────────────────────────────────
//
// Semantic colour mapping (mirrors repl.rs `colors` mod):
//
//   REPL colour   │ Markdown rendering     │ Applies to
//   ──────────────┼────────────────────────┼─────────────────────────────
//   KEYWORD       │ ***bold italic***      │ `fn`, `Module`, `interface`
//   FUNCTION/TYPE │ **bold**               │ primitive types, alias names,
//   (cyan)        │                        │   tag names, `list`, `Seq`
//   NUMBER        │ *italic*               │ numeric literals in types
//   STRING        │ `code`                 │ char-literal values
//   OPERATOR      │ plain                  │ `->`, `|`, `&`, `+`, `-`, …
//   BRACKET       │ plain                  │ `( ) [ ] { }`
//   GENERIC       │ *italic*               │ `T`, `A`, `#N`, `%L`
//

/// Primitive type names that should be rendered in **bold** (FUNCTION/TYPE colour).
const PRIMITIVE_TYPES: &[&str] = &["int", "num", "bool", "char", "any", "Empty"];

/// Keywords rendered in ***bold italic*** (KEYWORD colour).
const TYPE_KEYWORDS: &[&str] = &["fn", "Module", "interface", "class"];

/// Highlight a TypR type string into inline Markdown.
///
/// The function does a single forward scan, accumulating word tokens and
/// emitting formatted spans.  Non-word characters (operators, brackets,
/// punctuation) are emitted as-is.
pub fn highlight_type(type_str: &str) -> String {
    let mut out = String::with_capacity(type_str.len() * 2);
    let chars: Vec<char> = type_str.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        let ch = chars[i];

        // ── generic prefixes: #identifier  or  %identifier ──────────────
        if (ch == '#' || ch == '%')
            && i + 1 < len
            && (chars[i + 1].is_alphanumeric() || chars[i + 1] == '_')
        {
            let start = i;
            i += 1; // skip # or %
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("*{}*", token)); // italic (generic)
            continue;
        }

        // ── char-literal string values: sequences like "hello" or 'hello' ─
        if ch == '"' || ch == '\'' {
            let delim = ch;
            let start = i;
            i += 1;
            while i < len && chars[i] != delim {
                i += 1;
            }
            if i < len {
                i += 1; // consume closing delimiter
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("`{}`", token)); // code span (STRING colour)
            continue;
        }

        // ── word token (letter / underscore / digit-start handled below) ──
        if ch.is_alphabetic() || ch == '_' {
            let start = i;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_' || chars[i] == '.') {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();
            out.push_str(&colorize_word(&word));
            continue;
        }

        // ── numeric literal (NUMBER colour → italic) ─────────────────────
        if ch.is_ascii_digit() {
            let start = i;
            while i < len && (chars[i].is_ascii_digit() || chars[i] == '.') {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("*{}*", token)); // italic
            continue;
        }

        // ── tag dot-prefix: .TagName ──────────────────────────────────────
        if ch == '.' && i + 1 < len && chars[i + 1].is_alphabetic() {
            out.push('.');
            i += 1;
            let start = i;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();
            // Tag names → bold (same as type names)
            out.push_str(&format!("**{}**", word));
            continue;
        }

        // ── arrow operator `->` ───────────────────────────────────────────
        if ch == '-' && i + 1 < len && chars[i + 1] == '>' {
            out.push_str(" → "); // use the prettier unicode arrow, plain text
            i += 2;
            continue;
        }

        // ── everything else: operators, brackets, commas, spaces ──────────
        out.push(ch);
        i += 1;
    }

    out
}

/// Classify a word and wrap it in the appropriate Markdown formatting.
fn colorize_word(word: &str) -> String {
    if TYPE_KEYWORDS.contains(&word) {
        // ***bold italic*** — KEYWORD colour (magenta in REPL)
        format!("***{}***", word)
    } else if PRIMITIVE_TYPES.contains(&word) {
        // **bold** — FUNCTION/TYPE colour (cyan in REPL)
        format!("**{}**", word)
    } else if is_generic_name(word) {
        // *italic* — generic type variable
        format!("*{}*", word)
    } else if word.chars().next().map_or(false, |c| c.is_uppercase()) {
        // Capitalised → user-defined type / alias name → **bold**
        format!("**{}**", word)
    } else {
        // Anything else (field names, variable names inside types) → plain
        word.to_string()
    }
}

/// A generic name is a single uppercase ASCII letter, optionally followed by
/// digits (e.g. `T`, `A`, `T1`).
fn is_generic_name(word: &str) -> bool {
    let mut chars = word.chars();
    match chars.next() {
        Some(c) if c.is_ascii_uppercase() => chars.all(|c| c.is_ascii_digit()),
        _ => false,
    }
}

// ══════════════════════════════════════════════════════════════════════════
// ── AUTOCOMPLETION ────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

/// Main entry point for LSP completion requests.
///
/// Parses and type-checks the entire document, detects the syntactic context
/// at the cursor position, and returns context-appropriate completion items.
///
/// Supports three trigger types:
///   - `|>` (pipe): functions whose first parameter matches the left-hand type
///   - `$` (dollar): record/list fields
///   - `.` (dot): module members, record fields, or applicable functions (hybrid)
pub fn get_completions_at(content: &str, line: u32, character: u32) -> Vec<CompletionItem> {
    // 1. Parse + type-check the document WITHOUT the cursor line to avoid incomplete code
    let final_context = match parse_document_without_cursor_line(content, line) {
        Some(ctx) => ctx,
        None => {
            // Fallback: try parsing the whole document anyway
            let span: Span = LocatedSpan::new_extra(content, String::new());
            let parse_result =
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
            let context = Context::default();
            match parse_result {
                Ok(result) => {
                    let ast = result.ast;
                    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        typing(&context, &ast)
                    })) {
                        Ok(tc) => tc.context,
                        Err(_) => return get_fallback_completions(),
                    }
                }
                Err(_) => return get_fallback_completions(),
            }
        }
    };

    // 2. Extract multi-line prefix up to the cursor (like rust-analyzer).
    // This handles cases where triggers are on new lines with whitespace.
    let prefix = extract_multiline_prefix(content, line, character);

    // 3. Detect the completion context.
    let ctx = detect_completion_context(&prefix);

    // 4. Generate completions based on context.
    match ctx {
        CompletionCtx::Type => get_type_completions(&final_context),
        CompletionCtx::Module(name) => get_module_completions(&final_context, &name),
        CompletionCtx::Pipe(expr) => get_pipe_completions(&final_context, &expr),
        CompletionCtx::RecordField(expr) => get_record_field_completions(&final_context, &expr),
        CompletionCtx::DotAccess(expr) => get_dot_completions(&final_context, &expr),
        CompletionCtx::Expression => get_expression_completions(&final_context),
    }
}

/// Parse the document excluding the line containing the cursor.
/// This avoids parsing incomplete code that would cause panics.
fn parse_document_without_cursor_line(content: &str, cursor_line: u32) -> Option<Context> {
    let lines: Vec<&str> = content.lines().collect();

    // Build document without cursor line
    let mut filtered_lines = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        if idx != cursor_line as usize {
            filtered_lines.push(*line);
        }
    }

    let filtered_content = filtered_lines.join("\n");
    let span: Span = LocatedSpan::new_extra(&filtered_content, String::new());

    // Parse the document
    let parse_result =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span))).ok()?;
    let ast = parse_result.ast;

    // Use Context::default() to start with the standard library
    let context = Context::default();

    // For proper context propagation, we need to type each statement sequentially
    // so that earlier definitions are available in later expressions.
    // The standard typing() for Lang::Lines doesn't properly propagate context
    // for single statements, so we manually iterate.
    let final_context = if let Lang::Lines(exprs, _) = &ast {
        let mut ctx = context.clone();
        for expr in exprs {
            if let Ok(tc) =
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&ctx, expr)))
            {
                ctx = tc.context;
            }
        }
        ctx
    } else {
        // Fallback for non-Lines AST
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)))
            .ok()?
            .context
    };

    Some(final_context)
}

// ── Context detection ──────────────────────────────────────────────────────

#[derive(Debug, Clone)]
enum CompletionCtx {
    /// Type annotation context (e.g., after `let x: ` or in `fn(a: `)
    Type,
    /// Module member access (e.g., `MyModule.`)
    Module(String),
    /// Pipe operator (e.g., `value |>`)
    Pipe(String),
    /// Record field access via $ (e.g., `myrecord$`)
    RecordField(String),
    /// Dot access - hybrid (module members, record fields, or applicable functions)
    DotAccess(String),
    /// General expression context (runtime values)
    Expression,
}

/// Extract a multi-line prefix context (like rust-analyzer).
/// This includes previous lines to handle cases where the trigger is on a new line.
///
/// Example:
/// ```
/// mylist
///   .█
/// ```
/// Should extract "mylist\n  ." to properly detect the completion context.
fn extract_multiline_prefix(content: &str, line: u32, character: u32) -> String {
    let lines: Vec<&str> = content.lines().collect();
    let current_line_idx = line as usize;

    if current_line_idx >= lines.len() {
        return String::new();
    }

    // Get current line up to cursor
    let current_line_part = lines[current_line_idx]
        .get(..character as usize)
        .unwrap_or("");

    // Look back up to 10 lines to find context
    // This handles cases like:
    //   myvar
    //     .field
    let lookback_lines = 10;
    let start_line = current_line_idx.saturating_sub(lookback_lines);

    // Collect lines from start_line to current (inclusive)
    let mut context_lines = Vec::new();
    for i in start_line..current_line_idx {
        context_lines.push(lines[i]);
    }
    context_lines.push(current_line_part);

    // Join with newlines to preserve structure
    context_lines.join("\n")
}

fn detect_completion_context(prefix: &str) -> CompletionCtx {
    let trimmed = prefix.trim_end();

    // Pipe operator: "expr |>" (must check BEFORE checking '>' alone)
    if trimmed.ends_with("|>") {
        let before_pipe = trimmed[..trimmed.len() - 2].trim();
        return CompletionCtx::Pipe(extract_expression_before(before_pipe));
    }

    // Record field access via $: "record$"
    // Note: we check for $ at the end (just typed) OR just before cursor
    if let Some(dollar_pos) = trimmed.rfind('$') {
        // Only trigger if $ is at the end or followed by whitespace (including newlines)
        let after_dollar = &trimmed[dollar_pos + 1..];
        if after_dollar.is_empty() || after_dollar.chars().all(|c| c.is_whitespace()) {
            let before_dollar = trimmed[..dollar_pos].trim_end();
            if !before_dollar.is_empty() {
                // Extract the last expression before $ (handling multiline)
                let expr = extract_last_expression(before_dollar);
                return CompletionCtx::RecordField(expr);
            }
        }
    }

    // Dot access - could be module or record field or function application
    if let Some(dot_pos) = trimmed.rfind('.') {
        // Only trigger if . is at the end or followed by whitespace (including newlines)
        let after_dot = &trimmed[dot_pos + 1..];
        if after_dot.is_empty() || after_dot.chars().all(|c| c.is_whitespace()) {
            let before_dot = trimmed[..dot_pos].trim_end();
            if !before_dot.is_empty() {
                // Extract the last expression before . (handling multiline)
                let expr = extract_last_expression(before_dot);

                // Check if it looks like a module name (starts with uppercase)
                if expr.chars().next().map_or(false, |c| c.is_uppercase()) {
                    return CompletionCtx::Module(expr);
                } else {
                    return CompletionCtx::DotAccess(expr);
                }
            }
        }
    }

    // Type annotation: "let x: " or "fn(a: "
    if let Some(colon_pos) = trimmed.rfind(':') {
        let after_colon = &trimmed[colon_pos + 1..];
        if !after_colon.contains('=') && !after_colon.contains(';') {
            return CompletionCtx::Type;
        }
    }

    // Type alias definition: "type MyType = "
    if trimmed.trim_start().starts_with("type ") && trimmed.contains('=') {
        return CompletionCtx::Type;
    }

    // Default: expression (runtime values)
    CompletionCtx::Expression
}

/// Extract the last expression from a multi-line context.
/// This handles cases like:
/// ```
/// myvar
///   .
/// ```
/// Should extract "myvar" even with newlines/whitespace.
///
/// Also handles complex expressions like:
/// - `calculate(x).process()` → extracts the whole chain
/// - `list(a = 1, b = 2)` → extracts the whole literal
/// - `x[0].field` → extracts the whole accessor chain
fn extract_last_expression(s: &str) -> String {
    let trimmed = s.trim_end();

    // Split by statement terminators (;, newline when starting a new statement)
    // For simplicity, we split by ; and newlines, then take the last non-empty part
    let parts: Vec<&str> = trimmed
        .split(|c| c == ';' || c == '\n')
        .filter(|p| !p.trim().is_empty())
        .collect();

    let last_statement = parts.last().unwrap_or(&"").trim();

    if last_statement.is_empty() {
        return String::new();
    }

    // Now extract the last complete expression from this statement
    // We want to capture function calls, field access, indexing, etc.
    // Strategy: scan backwards and track depth of (), [], {}
    // Stop only at statement-level operators when at depth 0
    let mut depth_paren = 0;
    let mut depth_bracket = 0;
    let mut depth_brace = 0;
    let mut start = 0;

    for (i, ch) in last_statement.char_indices().rev() {
        match ch {
            ')' => depth_paren += 1,
            '(' => {
                depth_paren -= 1;
                if depth_paren < 0 {
                    // Unmatched opening paren - start here
                    start = i + 1;
                    break;
                }
            }
            ']' => depth_bracket += 1,
            '[' => {
                depth_bracket -= 1;
                if depth_bracket < 0 {
                    // Unmatched opening bracket - start here
                    start = i + 1;
                    break;
                }
            }
            '}' => depth_brace += 1,
            '{' => {
                depth_brace -= 1;
                if depth_brace < 0 {
                    // Unmatched opening brace - start here
                    start = i + 1;
                    break;
                }
            }
            // Only stop at these operators when at depth 0 (not inside any parens/brackets)
            // Don't stop at '=' inside function calls like `list(a = 1)`
            ',' if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 => {
                // Comma at top level - start after it
                start = i + 1;
                break;
            }
            // Assignment and comparison operators - but only at top level
            '<' | '>' if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 => {
                // Check if it's <- (assignment) or just comparison
                if i > 0 && last_statement.as_bytes().get(i - 1) == Some(&b'-') {
                    // It's part of <-, skip
                    continue;
                }
                start = i + 1;
                break;
            }
            _ => {}
        }
    }

    last_statement[start..].trim().to_string()
}

/// Extract the expression before a trigger (simple heuristic).
/// Looks backward for word boundaries, operators, or delimiters.
fn extract_expression_before(s: &str) -> String {
    let trimmed = s.trim_end();

    // Find the start of the last expression by scanning backwards
    // Stop at operators, semicolons, or opening delimiters
    let mut depth = 0;
    let mut start = trimmed.len();

    for (i, ch) in trimmed.char_indices().rev() {
        match ch {
            ')' | ']' | '}' => depth += 1,
            '(' | '[' | '{' => {
                depth -= 1;
                if depth < 0 {
                    start = i + 1;
                    break;
                }
            }
            ';' | ',' if depth == 0 => {
                start = i + 1;
                break;
            }
            _ => {}
        }
    }

    trimmed[start..].trim().to_string()
}

// ── Completion generators ──────────────────────────────────────────────────

/// Completions for type annotation contexts.
fn get_type_completions(context: &Context) -> Vec<CompletionItem> {
    use crate::utils::builder;

    let mut items = Vec::new();

    // Primitive types (keywords in the language, like TypeScript)
    let primitives = [
        ("int", builder::integer_type_default()),
        ("num", builder::number_type()),
        ("bool", builder::boolean_type()),
        ("char", builder::character_type_default()),
        ("any", builder::any_type()),
    ];

    for (name, typ) in &primitives {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(typ.pretty()),
            ..Default::default()
        });
    }

    // User-defined type aliases (interfaces/type contracts)
    for (var, typ) in context.aliases() {
        if var.is_alias() {
            items.push(var_to_completion_item(
                var,
                typ,
                CompletionItemKind::INTERFACE,
            ));
        }
    }

    // Module-exported type aliases (interfaces/type contracts)
    for (var, typ) in context.module_aliases() {
        items.push(var_to_completion_item(
            &var,
            &typ,
            CompletionItemKind::INTERFACE,
        ));
    }

    items
}

/// Completions for module member access (e.g., `MyModule.█`).
fn get_module_completions(context: &Context, module_name: &str) -> Vec<CompletionItem> {
    let module_context = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        context.extract_module_as_vartype(module_name)
    }));

    let module_ctx = match module_context {
        Ok(ctx) => ctx,
        Err(_) => return Vec::new(),
    };

    let mut items = Vec::new();

    for (var, typ) in module_ctx.variables() {
        let kind = if typ.is_function() {
            CompletionItemKind::FUNCTION
        } else {
            CompletionItemKind::VARIABLE
        };
        items.push(var_to_completion_item(var, typ, kind));
    }

    // Module type aliases (interfaces/type contracts)
    for (var, typ) in module_ctx.aliases() {
        items.push(var_to_completion_item(
            var,
            typ,
            CompletionItemKind::INTERFACE,
        ));
    }

    items
}

/// Completions for pipe operator (`expr |> █`).
///
/// Filters functions whose first parameter type is compatible with the
/// type of the expression on the left-hand side.
fn get_pipe_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    // Try to infer the type of the expression
    let expr_type = infer_expression_type(context, expr);

    let mut items = Vec::new();

    // Get all functions (user + std)
    let all_functions: Vec<_> = context
        .get_all_generic_functions()
        .into_iter()
        .chain(
            context
                .typing_context
                .standard_library()
                .into_iter()
                .filter(|(_, typ)| typ.is_function())
                .map(|(v, t)| (v.clone(), t.clone())),
        )
        .collect();

    for (var, typ) in all_functions {
        // Check if this function can accept expr_type as first parameter
        if let Some(first_param_type) = get_first_parameter_type(&typ) {
            if expr_type.is_subtype(&first_param_type, context) {
                items.push(var_to_completion_item(
                    &var,
                    &typ,
                    CompletionItemKind::FUNCTION,
                ));
            }
        }
    }

    items
}

/// Completions for record field access via $ (e.g., `myrecord$█`).
///
/// Looks up the type of the record expression and returns its fields.
fn get_record_field_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    let record_type = infer_expression_type(context, expr);

    match record_type {
        Type::Record(fields, _) => fields
            .iter()
            .map(|arg_type| CompletionItem {
                label: arg_type.get_argument_str(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(arg_type.get_type().pretty()),
                ..Default::default()
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Completions for dot access (`.`) — hybrid behavior.
///
/// Returns:
///   1. Module members (if expr looks like a module)
///   2. Record fields (if expr is a record type)
///   3. Applicable functions (like pipe, but for method-style calls)
fn get_dot_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    #[cfg(test)]
    eprintln!("DEBUG get_dot_completions: expr = {:?}", expr);

    let expr_type = infer_expression_type(context, expr);

    // 1. If it's a record, show fields
    if let Type::Record(fields, _) = &expr_type {
        for arg_type in fields {
            items.push(CompletionItem {
                label: arg_type.get_argument_str(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(arg_type.get_type().pretty()),
                ..Default::default()
            });
        }
    }

    // 2. Show applicable functions (same logic as pipe)
    // Functions whose first parameter type matches the expression type
    let all_functions: Vec<_> = context
        .get_all_generic_functions()
        .into_iter()
        .chain(
            context
                .typing_context
                .standard_library()
                .into_iter()
                .filter(|(_, typ)| typ.is_function())
                .map(|(v, t)| (v.clone(), t.clone())),
        )
        .collect();

    for (var, typ) in all_functions {
        if let Some(first_param_type) = get_first_parameter_type(&typ) {
            if expr_type.is_subtype(&first_param_type, context) {
                items.push(var_to_completion_item(
                    &var,
                    &typ,
                    CompletionItemKind::FUNCTION,
                ));
            }
        }
    }

    items
}

/// Completions for general expression contexts (runtime values).
fn get_expression_completions(context: &Context) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // User-defined variables (non-function values)
    for (var, typ) in context.variables() {
        if !typ.is_function() && !var.is_alias() {
            items.push(var_to_completion_item(
                var,
                typ,
                CompletionItemKind::VARIABLE,
            ));
        }
    }

    // User-defined functions
    for (var, typ) in context.get_all_generic_functions() {
        items.push(var_to_completion_item(
            &var,
            &typ,
            CompletionItemKind::FUNCTION,
        ));
    }

    // Standard library functions
    for (var, typ) in &context.typing_context.standard_library() {
        if typ.is_function() {
            items.push(var_to_completion_item(
                var,
                typ,
                CompletionItemKind::FUNCTION,
            ));
        }
    }

    items
}

/// Fallback completions when parsing or type-checking fails.
/// Returns primitive types as they are language keywords.
fn get_fallback_completions() -> Vec<CompletionItem> {
    use crate::utils::builder;

    let mut items = Vec::new();

    let primitives = [
        ("int", builder::integer_type_default()),
        ("num", builder::number_type()),
        ("bool", builder::boolean_type()),
        ("char", builder::character_type_default()),
        ("any", builder::any_type()),
    ];

    for (name, typ) in &primitives {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(typ.pretty()),
            ..Default::default()
        });
    }

    items
}

// ── Type inference helpers ─────────────────────────────────────────────────

/// Attempt to infer the type of an expression string.
///
/// This supports complex expressions like rust-analyzer:
///   - Simple variables: `myvar`
///   - Function calls: `calculate(x)`
///   - Field access: `record.field`
///   - Chaining: `x.process().result`
///   - Literals: `42`, `"hello"`, `list(a = 1)`
///
/// Strategy: parse and type-check the expression in the current context
fn infer_expression_type(context: &Context, expr: &str) -> Type {
    use crate::utils::builder;

    let trimmed = expr.trim();

    if trimmed.is_empty() {
        return builder::any_type();
    }

    // First, try the simple case: a known variable name
    let types = context.get_types_from_name(trimmed);
    if !types.is_empty() {
        return types.last().unwrap().clone();
    }

    // Try to parse as a literal
    if let Some(typ) = infer_literal_type(trimmed) {
        return typ;
    }

    // For complex expressions (function calls, field access, etc.),
    // we need to parse and type-check the expression
    // Strategy: parse the expression as a standalone statement and infer its type
    let result = parse_and_infer_expression_type(context, trimmed);

    result.unwrap_or_else(|| builder::any_type())
}

/// Parse and type-check an expression to infer its type.
/// This handles complex expressions like:
/// - `calculate(x)` → type of calculate's return value
/// - `list(a = 1, b = 2)` → {a: int, b: int}
/// - `myvar.field` → type of field
/// - `func(x).method()` → method-style chained calls
fn parse_and_infer_expression_type(context: &Context, expr: &str) -> Option<Type> {
    // First, try to handle method-style dot notation (e.g., `expr.func()`)
    // by converting it to standard function calls
    let normalized_expr = normalize_dot_calls(context, expr);

    // Wrap the expression in a minimal parseable statement
    // We'll try to parse it as: `__temp <- expr;`
    // Adding semicolon ensures proper statement termination
    let wrapped = format!("__temp <- {};", normalized_expr);
    let span: Span = LocatedSpan::new_extra(&wrapped, "<lsp-inference>".to_string());

    // Try to parse
    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)))
        .ok()?
        .ast;

    // Try to type-check using typing_with_errors to avoid panics
    let type_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::processes::type_checking::typing_with_errors(context, &ast)
    }))
    .ok()?;

    // The type of the expression is the value type from the TypeContext
    // (the result of typing the assignment `__temp <- expr`)
    Some(type_result.type_context.value.clone())
}

/// Normalize method-style dot calls to standard function calls.
///
/// Converts expressions like:
/// - `x.incr()` → `incr(x)`
/// - `x.incr().incr()` → `incr(incr(x))`
/// - `incr(1).incr()` → `incr(incr(1))`
///
/// This allows the type system to correctly infer the type of chained method calls.
fn normalize_dot_calls(context: &Context, expr: &str) -> String {
    let trimmed = expr.trim();

    // Pattern: something.identifier() or something.identifier
    // We need to find the rightmost dot that's followed by an identifier

    // Strategy: scan from right to left to find `.identifier(` or `.identifier` at the end
    // This handles nested cases like `a.b().c()` by processing from right to left

    let mut result = trimmed.to_string();
    let mut changed = true;

    // Keep transforming until no more changes (handles nested chains)
    while changed {
        changed = false;
        if let Some(transformed) = try_normalize_rightmost_dot_call(context, &result) {
            result = transformed;
            changed = true;
        }
    }

    result
}

/// Try to normalize the rightmost method-style dot call in an expression.
/// Returns Some(transformed) if a transformation was made, None otherwise.
fn try_normalize_rightmost_dot_call(context: &Context, expr: &str) -> Option<String> {
    // Find the rightmost dot that's followed by an identifier (potential method call)
    // We need to handle parentheses properly: `a.b(x).c()` → rightmost is `.c()`

    let chars: Vec<char> = expr.chars().collect();
    let len = chars.len();

    // Scan backwards to find the rightmost dot that could be a method call
    let mut paren_depth = 0;
    let mut bracket_depth = 0;
    let mut i = len;

    while i > 0 {
        i -= 1;
        match chars[i] {
            ')' => paren_depth += 1,
            '(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                }
            }
            ']' => bracket_depth += 1,
            '[' => {
                if bracket_depth > 0 {
                    bracket_depth -= 1;
                }
            }
            '.' if paren_depth == 0 && bracket_depth == 0 => {
                // Found a dot at the top level, check if it's followed by an identifier
                if i + 1 < len && (chars[i + 1].is_alphabetic() || chars[i + 1] == '_') {
                    // Extract the method name
                    let mut method_end = i + 1;
                    while method_end < len
                        && (chars[method_end].is_alphanumeric() || chars[method_end] == '_')
                    {
                        method_end += 1;
                    }
                    let method_name: String = chars[i + 1..method_end].iter().collect();

                    // Check if this is a known function in the context
                    let types = context.get_types_from_name(&method_name);
                    let is_known_function = types.iter().any(|t| t.is_function());

                    // Also check standard library
                    let is_std_function = context
                        .typing_context
                        .standard_library()
                        .iter()
                        .any(|(v, t)| v.get_name() == method_name && t.is_function());

                    if is_known_function || is_std_function {
                        // This is a method-style call, transform it
                        let receiver: String = chars[..i].iter().collect();
                        let after_method: String = chars[method_end..].iter().collect();

                        // Check if there are arguments: `.method(args)` vs `.method`
                        if after_method.starts_with('(') {
                            // Find matching closing paren
                            let after_chars: Vec<char> = after_method.chars().collect();
                            let mut depth = 0;
                            let mut close_idx = 0;
                            for (j, &c) in after_chars.iter().enumerate() {
                                match c {
                                    '(' => depth += 1,
                                    ')' => {
                                        depth -= 1;
                                        if depth == 0 {
                                            close_idx = j;
                                            break;
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            // Extract arguments (everything between parens, excluding the parens)
                            let args_content: String = after_chars[1..close_idx].iter().collect();
                            let rest: String = after_chars[close_idx + 1..].iter().collect();

                            // Transform: `receiver.method(args)rest` → `method(receiver, args)rest`
                            // or if no args: `receiver.method()rest` → `method(receiver)rest`
                            if args_content.trim().is_empty() {
                                return Some(format!(
                                    "{}({}){}",
                                    method_name,
                                    receiver.trim(),
                                    rest
                                ));
                            } else {
                                return Some(format!(
                                    "{}({}, {}){}",
                                    method_name,
                                    receiver.trim(),
                                    args_content.trim(),
                                    rest
                                ));
                            }
                        } else {
                            // No parentheses, it's a field access or method without call
                            // Don't transform - it could be a record field
                            return None;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    None
}

/// Extract the first parameter type from a function type.
fn get_first_parameter_type(typ: &Type) -> Option<Type> {
    match typ {
        Type::Function(params, _, _) => params.first().cloned(),
        _ => None,
    }
}

// ── Helpers ────────────────────────────────────────────────────────────────

/// Convert a `(Var, Type)` pair into a LSP `CompletionItem`.
fn var_to_completion_item(var: &Var, typ: &Type, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: var.get_name(),
        kind: Some(kind),
        detail: Some(typ.pretty()),
        ..Default::default()
    }
}

// ══════════════════════════════════════════════════════════════════════════
// ── TESTS ─────────────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_completion_context_pipe() {
        let prefix = "mylist |>";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::Pipe(_)));
    }

    #[test]
    fn test_detect_completion_context_pipe_with_space() {
        let prefix = "mylist |> ";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::Pipe(_)));
    }

    #[test]
    fn test_detect_completion_context_record_field() {
        let prefix = "myrecord$";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::RecordField(_)));
    }

    #[test]
    fn test_detect_completion_context_record_field_with_space() {
        let prefix = "myrecord$ ";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::RecordField(_)));
    }

    #[test]
    fn test_detect_completion_context_dot_module() {
        let prefix = "MyModule.";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::Module(_)));
    }

    #[test]
    fn test_detect_completion_context_dot_record() {
        let prefix = "myvar.";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::DotAccess(_)));
    }

    #[test]
    fn test_detect_completion_context_dot_with_space() {
        let prefix = "myvar. ";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::DotAccess(_)));
    }

    #[test]
    fn test_detect_completion_context_dot_multiline() {
        let prefix = "myvar\n  .";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::DotAccess(_)));
        if let CompletionCtx::DotAccess(expr) = ctx {
            assert_eq!(expr, "myvar");
        }
    }

    #[test]
    fn test_detect_completion_context_dollar_multiline() {
        let prefix = "myrecord\n  $";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::RecordField(_)));
        if let CompletionCtx::RecordField(expr) = ctx {
            assert_eq!(expr, "myrecord");
        }
    }

    #[test]
    fn test_detect_completion_context_pipe_multiline() {
        let prefix = "mylist\n  |>";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::Pipe(_)));
    }

    #[test]
    fn test_detect_completion_context_type_annotation() {
        let prefix = "let x: ";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::Type));
    }

    #[test]
    fn test_detect_completion_context_type_annotation_just_colon() {
        let prefix = "let x:";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::Type));
    }

    #[test]
    fn test_detect_completion_context_function_param_type() {
        let prefix = "fn foo(a:";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::Type));
    }

    #[test]
    fn test_detect_completion_context_expression() {
        let prefix = "let x = ";
        let ctx = detect_completion_context(prefix);
        assert!(matches!(ctx, CompletionCtx::Expression));
    }

    #[test]
    fn test_extract_expression_before() {
        // Test with semicolon separator (common in R-like syntax)
        assert_eq!(extract_expression_before("a; b; c"), "c");
        // Test with comma separator (function arguments)
        assert_eq!(extract_expression_before("x, y, z"), "z");
    }

    #[test]
    fn test_extract_last_expression() {
        // Simple identifier
        assert_eq!(extract_last_expression("myvar"), "myvar");

        // With trailing whitespace
        assert_eq!(extract_last_expression("myvar  "), "myvar");

        // With newlines
        assert_eq!(extract_last_expression("myvar\n  "), "myvar");

        // After semicolon
        assert_eq!(extract_last_expression("a; b; myvar"), "myvar");

        // Complex expression with multiple lines
        assert_eq!(extract_last_expression("let x = foo\nmyvar"), "myvar");

        // Function calls (NEW - like rust-analyzer)
        assert_eq!(extract_last_expression("calculate(x)"), "calculate(x)");

        // Function with multiple args
        assert_eq!(extract_last_expression("foo(a, b, c)"), "foo(a, b, c)");

        // Nested function calls
        assert_eq!(
            extract_last_expression("outer(inner(x))"),
            "outer(inner(x))"
        );

        // Field access chain
        assert_eq!(
            extract_last_expression("x.field.subfield"),
            "x.field.subfield"
        );

        // Function call with field access
        assert_eq!(
            extract_last_expression("calculate(x).result"),
            "calculate(x).result"
        );

        // List literal
        assert_eq!(
            extract_last_expression("list(a = 1, b = 2)"),
            "list(a = 1, b = 2)"
        );

        // Full function call (not after comma, the whole thing)
        assert_eq!(extract_last_expression("foo(a, bar(x))"), "foo(a, bar(x))");
    }

    #[test]
    fn test_extract_multiline_prefix() {
        let content = "line1\nline2\nline3";
        // Extract from line 2 (0-indexed), position 3
        let result = extract_multiline_prefix(content, 2, 3);
        assert!(result.contains("line1"));
        assert!(result.contains("line2"));
        assert!(result.ends_with("lin"));
    }

    // ── Go to definition tests ────────────────────────────────────────────────

    #[test]
    fn test_offset_to_position_start() {
        let content = "let x <- 42;";
        let pos = offset_to_position(0, content);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn test_offset_to_position_same_line() {
        let content = "let x <- 42;";
        let pos = offset_to_position(4, content);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 4);
    }

    #[test]
    fn test_offset_to_position_multiline() {
        let content = "let x <- 1;\nlet y <- 2;";
        // Position of 'y' (after newline: 12 chars + 4 = offset 16)
        let pos = offset_to_position(16, content);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 4);
    }

    #[test]
    fn test_find_definition_simple_variable() {
        let code = "let myVar <- 42;\nmyVar;";
        // Try to find definition of 'myVar' on line 1, col 0
        let result = find_definition_at(code, 1, 0);

        assert!(result.is_some());
        if let Some(def_info) = result {
            // The definition should point to line 0 (where 'let myVar' is)
            assert_eq!(def_info.range.start.line, 0);
        }
    }

    #[test]
    fn test_find_definition_function() {
        let code = "let add <- fn(a: int, b: int): int { a + b };\nadd(1, 2);";
        // Try to find definition of 'add' on line 1
        let result = find_definition_at(code, 1, 0);

        assert!(result.is_some());
        if let Some(def_info) = result {
            // The definition should point to line 0
            assert_eq!(def_info.range.start.line, 0);
        }
    }

    #[test]
    fn test_find_definition_type_alias() {
        let code = "type MyInt <- int;\nlet x: MyInt <- 42;";
        // Try to find definition of 'MyInt' on line 1
        let result = find_definition_at(code, 1, 7);

        assert!(result.is_some());
        if let Some(def_info) = result {
            // The definition should point to line 0
            assert_eq!(def_info.range.start.line, 0);
        }
    }

    #[test]
    fn test_find_definition_undefined() {
        let code = "let x <- undefined_var;";
        // Try to find definition of 'undefined_var' which doesn't exist
        let result = find_definition_at(code, 0, 9);

        // Should return None because 'undefined_var' is not defined
        assert!(result.is_none());
    }

    #[test]
    fn test_find_definition_literal() {
        let code = "let x <- 42;";
        // Try to find definition of '42' (a literal)
        let result = find_definition_at(code, 0, 9);

        // Literals don't have definitions
        assert!(result.is_none());
    }

    // ── Completion with function chaining tests ────────────────────────────────

    #[test]
    fn test_dot_completion_with_function_call() {
        // Define a function incr: (int) -> int
        // Then test that incr(1). suggests incr again
        let code = "let incr <- fn(x: int): int { x + 1 };\nincr(1).";
        let completions = get_completions_at(code, 1, 8);

        // Should find incr as a completion because incr(1) returns int
        // and incr takes int as first parameter
        let has_incr = completions.iter().any(|item| item.label == "incr");
        assert!(
            has_incr,
            "Expected 'incr' in completions, got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_dot_completion_with_chained_calls() {
        // Test incr(1).incr(). should suggest incr
        let code = "let incr <- fn(x: int): int { x + 1 };\nincr(1).incr().";
        let completions = get_completions_at(code, 1, 15);

        let has_incr = completions.iter().any(|item| item.label == "incr");
        assert!(
            has_incr,
            "Expected 'incr' in completions for chained call, got: {:?}",
            completions.iter().map(|c| &c.label).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_infer_expression_type_function_call() {
        use crate::utils::fluent_parser::FluentParser;

        // Use FluentParser to properly set up the context with user-defined function
        let parser = FluentParser::new()
            .push("let incr <- fn(x: int): int { x + 1 };")
            .run();

        let final_context = parser.get_context();

        // Verify incr is in the context
        let incr_types = final_context.get_types_from_name("incr");
        assert!(!incr_types.is_empty(), "incr should be in the context");

        // Test that infer_expression_type correctly infers incr(1) as int
        let expr_type = infer_expression_type(&final_context, "incr(1)");

        assert!(
            matches!(expr_type, Type::Integer(_, _)),
            "Expected Integer type for incr(1), got: {:?}",
            expr_type.pretty()
        );
    }

    #[test]
    fn test_extract_last_expression_with_method_chain() {
        // Test that function chains are extracted correctly
        assert_eq!(extract_last_expression("incr(1)"), "incr(1)");
        assert_eq!(extract_last_expression("incr(1).incr()"), "incr(1).incr()");
        assert_eq!(extract_last_expression("a.b().c()"), "a.b().c()");
    }
}
