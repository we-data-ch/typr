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
use std::path::Path;
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Position, Range};
use typr_core::components::context::config::Environment;
use typr_core::components::context::Context;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;
use typr_core::processes::parsing::parse;
use typr_core::typing;
use typr_core::utils::builder;

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
    /// The file path where the symbol is defined (None if same file or unknown).
    pub file_path: Option<String>,
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

/// Detect the environment (Project or StandAlone) by looking for DESCRIPTION
/// and NAMESPACE files in parent directories.
fn detect_environment(file_path: &str) -> Environment {
    let path = Path::new(file_path);
    let mut dir = path.parent();

    while let Some(d) = dir {
        let description = d.join("DESCRIPTION");
        let namespace = d.join("NAMESPACE");
        if description.exists() && namespace.exists() {
            return Environment::Project;
        }
        dir = d.parent();
    }

    Environment::StandAlone
}

/// Main entry-point called by the LSP goto_definition handler.
pub fn find_definition_at(
    content: &str,
    line: u32,
    character: u32,
    file_path: &str,
) -> Option<DefinitionInfo> {
    // 1. Extract the word under the cursor.
    let (word, _word_range) = extract_word_at(content, line, character)?;

    // 2. Parse the whole document with the file path.
    let span: Span = LocatedSpan::new_extra(content, file_path.to_string());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    let ast = parse_result.ok()?.ast;

    // 3. Detect environment and apply metaprogramming to resolve module imports.
    let environment = detect_environment(file_path);
    let ast = metaprogrammation(ast, environment);

    // 4. Type-check the whole document to build the context.
    let context = Context::default();
    let type_context =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)));
    let type_context = type_context.ok()?;
    let final_context = type_context.context;

    // 5. Look up the variable in the context to find its definition.
    let definition_var = final_context
        .variables()
        .find(|(var, _)| var.get_name() == word)
        .map(|(var, _)| var.clone());

    let definition_var = definition_var.or_else(|| {
        final_context
            .aliases()
            .find(|(var, _)| var.get_name() == word)
            .map(|(var, _)| var.clone())
    });

    let var = definition_var?;
    let help_data = var.get_help_data();
    let offset = help_data.get_offset();
    let definition_file = help_data.get_file_name();

    // 6. Determine if the definition is in a different file.
    let (source_content, file_path_result) =
        if definition_file.is_empty() || definition_file == file_path {
            (content.to_string(), None)
        } else {
            match std::fs::read_to_string(&definition_file) {
                Ok(external_content) => (external_content, Some(definition_file)),
                Err(_) => (content.to_string(), None),
            }
        };

    // 7. Convert offset to Position using the correct file content.
    let pos = offset_to_position(offset, &source_content);
    let end_col = pos.character + word.len() as u32;

    Some(DefinitionInfo {
        range: Range::new(pos, Position::new(pos.line, end_col)),
        file_path: file_path_result,
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
/// the given cursor position.
fn extract_word_at(content: &str, line: u32, character: u32) -> Option<(String, Range)> {
    let source_line = content.lines().nth(line as usize)?;

    if (character as usize) > source_line.len() {
        return None;
    }

    let bytes = source_line.as_bytes();
    let col = character as usize;

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

/// A character is part of a word if it is alphanumeric, an underscore, or a dot.
fn is_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'.'
}

// ── literal fallback ───────────────────────────────────────────────────────

/// For numeric literals that are not in the context, return their literal type.
fn infer_literal_type(word: &str) -> Option<Type> {
    if let Ok(i) = word.parse::<i32>() {
        return Some(builder::integer_type(i));
    }
    if let Ok(_f) = word.parse::<f32>() {
        return Some(builder::number_type());
    }
    None
}

// ── Markdown type highlighter ──────────────────────────────────────────────

/// Primitive type names that should be rendered in **bold**.
const PRIMITIVE_TYPES: &[&str] = &["int", "num", "bool", "char", "any", "Empty"];

/// Keywords rendered in ***bold italic***.
const TYPE_KEYWORDS: &[&str] = &["fn", "Module", "interface", "class"];

/// Highlight a TypR type string into inline Markdown.
pub fn highlight_type(type_str: &str) -> String {
    let mut out = String::with_capacity(type_str.len() * 2);
    let chars: Vec<char> = type_str.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        let ch = chars[i];

        // ── generic prefixes: #identifier or %identifier ──────────────
        if (ch == '#' || ch == '%')
            && i + 1 < len
            && (chars[i + 1].is_alphanumeric() || chars[i + 1] == '_')
        {
            let start = i;
            i += 1;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("*{}*", token));
            continue;
        }

        // ── char-literal string values ─────────────────────────────────
        if ch == '"' || ch == '\'' {
            let delim = ch;
            let start = i;
            i += 1;
            while i < len && chars[i] != delim {
                i += 1;
            }
            if i < len {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("`{}`", token));
            continue;
        }

        // ── word token ──────────────────────────────────────────────────
        if ch.is_alphabetic() || ch == '_' {
            let start = i;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_' || chars[i] == '.') {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();
            out.push_str(&colorize_word(&word));
            continue;
        }

        // ── numeric literal ─────────────────────────────────────────────
        if ch.is_ascii_digit() {
            let start = i;
            while i < len && (chars[i].is_ascii_digit() || chars[i] == '.') {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("*{}*", token));
            continue;
        }

        // ── tag dot-prefix: .TagName ────────────────────────────────────
        if ch == '.' && i + 1 < len && chars[i + 1].is_alphabetic() {
            out.push('.');
            i += 1;
            let start = i;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();
            out.push_str(&format!("**{}**", word));
            continue;
        }

        // ── arrow operator `->` ─────────────────────────────────────────
        if ch == '-' && i + 1 < len && chars[i + 1] == '>' {
            out.push_str(" → ");
            i += 2;
            continue;
        }

        // ── everything else ─────────────────────────────────────────────
        out.push(ch);
        i += 1;
    }

    out
}

/// Classify a word and wrap it in the appropriate Markdown formatting.
fn colorize_word(word: &str) -> String {
    if TYPE_KEYWORDS.contains(&word) {
        format!("***{}***", word)
    } else if PRIMITIVE_TYPES.contains(&word) {
        format!("**{}**", word)
    } else if is_generic_name(word) {
        format!("*{}*", word)
    } else if word.chars().next().map_or(false, |c| c.is_uppercase()) {
        format!("**{}**", word)
    } else {
        word.to_string()
    }
}

/// A generic name is a single uppercase ASCII letter, optionally followed by digits.
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

/// Main entry point for LSP completion requests.
pub fn get_completions_at(content: &str, line: u32, character: u32) -> Vec<CompletionItem> {
    // 1. Parse + type-check the document WITHOUT the cursor line
    let final_context = match parse_document_without_cursor_line(content, line) {
        Some(ctx) => ctx,
        None => {
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

    // 2. Extract multi-line prefix up to the cursor.
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
fn parse_document_without_cursor_line(content: &str, cursor_line: u32) -> Option<Context> {
    let lines: Vec<&str> = content.lines().collect();

    let mut filtered_lines = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        if idx != cursor_line as usize {
            filtered_lines.push(*line);
        }
    }

    let filtered_content = filtered_lines.join("\n");
    let span: Span = LocatedSpan::new_extra(&filtered_content, String::new());

    let parse_result =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span))).ok()?;
    let ast = parse_result.ast;

    let context = Context::default();

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
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)))
            .ok()?
            .context
    };

    Some(final_context)
}

// ── Context detection ──────────────────────────────────────────────────────

#[derive(Debug, Clone)]
enum CompletionCtx {
    Type,
    Module(String),
    Pipe(String),
    RecordField(String),
    DotAccess(String),
    Expression,
}

fn extract_multiline_prefix(content: &str, line: u32, character: u32) -> String {
    let lines: Vec<&str> = content.lines().collect();
    let current_line_idx = line as usize;

    if current_line_idx >= lines.len() {
        return String::new();
    }

    let current_line_part = lines[current_line_idx]
        .get(..character as usize)
        .unwrap_or("");

    let lookback_lines = 10;
    let start_line = current_line_idx.saturating_sub(lookback_lines);

    let mut context_lines = Vec::new();
    for i in start_line..current_line_idx {
        context_lines.push(lines[i]);
    }
    context_lines.push(current_line_part);

    context_lines.join("\n")
}

fn detect_completion_context(prefix: &str) -> CompletionCtx {
    let trimmed = prefix.trim_end();

    if trimmed.ends_with("|>") {
        let before_pipe = trimmed[..trimmed.len() - 2].trim();
        return CompletionCtx::Pipe(extract_expression_before(before_pipe));
    }

    if let Some(dollar_pos) = trimmed.rfind('$') {
        let after_dollar = &trimmed[dollar_pos + 1..];
        if after_dollar.is_empty() || after_dollar.chars().all(|c| c.is_whitespace()) {
            let before_dollar = trimmed[..dollar_pos].trim_end();
            if !before_dollar.is_empty() {
                let expr = extract_last_expression(before_dollar);
                return CompletionCtx::RecordField(expr);
            }
        }
    }

    if let Some(dot_pos) = trimmed.rfind('.') {
        let after_dot = &trimmed[dot_pos + 1..];
        if after_dot.is_empty() || after_dot.chars().all(|c| c.is_whitespace()) {
            let before_dot = trimmed[..dot_pos].trim_end();
            if !before_dot.is_empty() {
                let expr = extract_last_expression(before_dot);

                if expr.chars().next().map_or(false, |c| c.is_uppercase()) {
                    return CompletionCtx::Module(expr);
                } else {
                    return CompletionCtx::DotAccess(expr);
                }
            }
        }
    }

    if let Some(colon_pos) = trimmed.rfind(':') {
        let after_colon = &trimmed[colon_pos + 1..];
        if !after_colon.contains('=') && !after_colon.contains(';') {
            return CompletionCtx::Type;
        }
    }

    if trimmed.trim_start().starts_with("type ") && trimmed.contains('=') {
        return CompletionCtx::Type;
    }

    CompletionCtx::Expression
}

fn extract_last_expression(s: &str) -> String {
    let trimmed = s.trim_end();

    let parts: Vec<&str> = trimmed
        .split(|c| c == ';' || c == '\n')
        .filter(|p| !p.trim().is_empty())
        .collect();

    let last_statement = parts.last().unwrap_or(&"").trim();

    if last_statement.is_empty() {
        return String::new();
    }

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
                    start = i + 1;
                    break;
                }
            }
            ']' => depth_bracket += 1,
            '[' => {
                depth_bracket -= 1;
                if depth_bracket < 0 {
                    start = i + 1;
                    break;
                }
            }
            '}' => depth_brace += 1,
            '{' => {
                depth_brace -= 1;
                if depth_brace < 0 {
                    start = i + 1;
                    break;
                }
            }
            ',' if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 => {
                start = i + 1;
                break;
            }
            '<' | '>' if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 => {
                if i > 0 && last_statement.as_bytes().get(i - 1) == Some(&b'-') {
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

fn extract_expression_before(s: &str) -> String {
    let trimmed = s.trim_end();

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

fn get_type_completions(context: &Context) -> Vec<CompletionItem> {
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
            insert_text: Some(format!(" {}", name)),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(typ.pretty()),
            ..Default::default()
        });
    }

    for (var, typ) in context.aliases() {
        if var.is_alias() {
            items.push(CompletionItem {
                label: var.get_name(),
                insert_text: Some(format!(" {}", var.get_name())),
                kind: Some(CompletionItemKind::INTERFACE),
                detail: Some(typ.pretty()),
                ..Default::default()
            });
        }
    }

    for (var, typ) in context.module_aliases() {
        items.push(CompletionItem {
            label: var.get_name(),
            insert_text: Some(format!(" {}", var.get_name())),
            kind: Some(CompletionItemKind::INTERFACE),
            detail: Some(typ.pretty()),
            ..Default::default()
        });
    }

    items
}

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

    for (var, typ) in module_ctx.aliases() {
        items.push(var_to_completion_item(
            var,
            typ,
            CompletionItemKind::INTERFACE,
        ));
    }

    items
}

fn get_pipe_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    let expr_type = infer_expression_type(context, expr);

    let mut items = Vec::new();

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
            if expr_type.is_subtype(&first_param_type, context).0 {
                items.push(CompletionItem {
                    label: var.get_name(),
                    insert_text: Some(format!(" {}", var.get_name())),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(typ.pretty()),
                    ..Default::default()
                });
            }
        }
    }

    items
}

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

fn get_dot_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    let expr_type = infer_expression_type(context, expr);

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
            if expr_type.is_subtype(&first_param_type, context).0 {
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

fn get_expression_completions(context: &Context) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for (var, typ) in context.variables() {
        if !typ.is_function() && !var.is_alias() {
            items.push(var_to_completion_item(
                var,
                typ,
                CompletionItemKind::VARIABLE,
            ));
        }
    }

    for (var, typ) in context.get_all_generic_functions() {
        items.push(var_to_completion_item(
            &var,
            &typ,
            CompletionItemKind::FUNCTION,
        ));
    }

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

fn get_fallback_completions() -> Vec<CompletionItem> {
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

fn infer_expression_type(context: &Context, expr: &str) -> Type {
    let trimmed = expr.trim();

    if trimmed.is_empty() {
        return builder::any_type();
    }

    let types = context.get_types_from_name(trimmed);
    if !types.is_empty() {
        return types.last().unwrap().clone();
    }

    if let Some(typ) = infer_literal_type(trimmed) {
        return typ;
    }

    let result = parse_and_infer_expression_type(context, trimmed);

    result.unwrap_or_else(|| builder::any_type())
}

fn parse_and_infer_expression_type(context: &Context, expr: &str) -> Option<Type> {
    let normalized_expr = normalize_dot_calls(context, expr);

    let wrapped = format!("__temp <- {};", normalized_expr);
    let span: Span = LocatedSpan::new_extra(&wrapped, "<lsp-inference>".to_string());

    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)))
        .ok()?
        .ast;

    let type_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        typr_core::typing_with_errors(context, &ast)
    }))
    .ok()?;

    Some(type_result.type_context.value.clone())
}

fn normalize_dot_calls(context: &Context, expr: &str) -> String {
    let trimmed = expr.trim();

    let mut result = trimmed.to_string();
    let mut changed = true;

    while changed {
        changed = false;
        if let Some(transformed) = try_normalize_rightmost_dot_call(context, &result) {
            result = transformed;
            changed = true;
        }
    }

    result
}

fn try_normalize_rightmost_dot_call(context: &Context, expr: &str) -> Option<String> {
    let chars: Vec<char> = expr.chars().collect();
    let len = chars.len();

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
                if i + 1 < len && (chars[i + 1].is_alphabetic() || chars[i + 1] == '_') {
                    let mut method_end = i + 1;
                    while method_end < len
                        && (chars[method_end].is_alphanumeric() || chars[method_end] == '_')
                    {
                        method_end += 1;
                    }
                    let method_name: String = chars[i + 1..method_end].iter().collect();

                    let types = context.get_types_from_name(&method_name);
                    let is_known_function = types.iter().any(|t| t.is_function());

                    let is_std_function = context
                        .typing_context
                        .standard_library()
                        .iter()
                        .any(|(v, t)| v.get_name() == method_name && t.is_function());

                    if is_known_function || is_std_function {
                        let receiver: String = chars[..i].iter().collect();
                        let after_method: String = chars[method_end..].iter().collect();

                        if after_method.starts_with('(') {
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

                            let args_content: String = after_chars[1..close_idx].iter().collect();
                            let rest: String = after_chars[close_idx + 1..].iter().collect();

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

fn get_first_parameter_type(typ: &Type) -> Option<Type> {
    match typ {
        Type::Function(params, _, _) => params.first().cloned(),
        _ => None,
    }
}

fn var_to_completion_item(var: &Var, typ: &Type, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: var.get_name(),
        kind: Some(kind),
        detail: Some(typ.pretty()),
        ..Default::default()
    }
}
