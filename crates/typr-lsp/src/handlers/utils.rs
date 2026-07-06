//! Token resolution and Markdown-highlighted type display for LSP hover.
//!
//! Given the full source text and a cursor position (line, character),
//! this module:
//!   1. Identifies the word (identifier / literal) under the cursor.
//!   2. Parses and type-checks the whole document using the project's
//!      pipeline (`parse` → `typing`) to build a fully-populated `Context`.
//!   3. Looks up the identifier in that context and returns its type.
//!   4. Renders the type string with Markdown syntax highlighting.

use nom_locate::LocatedSpan;
use tower_lsp_server::ls_types::{
    CompletionItem, CompletionItemKind, Position, Range,
};
use typr_core::components::context::Context;
use typr_core::components::language::var::Var;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;
use typr_core::processes::parsing::parse;
use typr_core::utils::builder;

type Span<'a> = LocatedSpan<&'a str, String>;


pub fn offset_to_position(offset: usize, content: &str) -> Position {
    let mut line = 0u32;
    let mut col = 0u32;
    let mut byte_pos = 0usize;

    for ch in content.chars() {
        if byte_pos >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += ch.len_utf16() as u32;
        }
        byte_pos += ch.len_utf8();
    }

    Position::new(line, col)
}

// ── word extraction ────────────────────────────────────────────────────────

/// Extract the contiguous word (identifier or numeric literal) that contains
/// the given cursor position.
pub fn extract_word_at(content: &str, line: u32, character: u32) -> Option<(String, Range)> {
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
pub fn infer_literal_type(word: &str) -> Option<Type> {
    if let Ok(i) = word.parse::<i32>() {
        return Some(builder::integer_type(i));
    }
    if let Ok(_f) = word.parse::<f32>() {
        return Some(builder::number_type());
    }
    None
}

pub fn extract_multiline_prefix(content: &str, line: u32, character: u32) -> String {
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

    let mut context_lines = lines[start_line..current_line_idx].to_vec();
    context_lines.push(current_line_part);

    context_lines.join("\n")
}

// ── Type inference helpers ─────────────────────────────────────────────────

pub fn infer_expression_type(context: &Context, expr: &str) -> Type {
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

    result.unwrap_or_else(builder::any_type)
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
            '(' if paren_depth > 0 => {
                paren_depth -= 1;
            }
            ']' => bracket_depth += 1,
            '[' if bracket_depth > 0 => {
                bracket_depth -= 1;
            }
            '.' if paren_depth == 0
                && bracket_depth == 0
                && i + 1 < len
                && (chars[i + 1].is_alphabetic() || chars[i + 1] == '_') =>
            {
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
                            return Some(format!("{}({}){}", method_name, receiver.trim(), rest));
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
            _ => {}
        }
    }

    None
}

pub fn get_first_parameter_type(typ: &Type) -> Option<Type> {
    match typ {
        Type::Function(params, _, _) => params.first().map(|arg| arg.get_type()),
        _ => None,
    }
}

pub fn var_to_completion_item(var: &Var, typ: &Type, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: var.get_name(),
        kind: Some(kind),
        detail: Some(typ.pretty()),
        ..Default::default()
    }
}

#[cfg(test)]
mod offset_to_position_tests {
    use super::*;

    /// A French accented char (`é`, 2 UTF-8 bytes, 1 UTF-16 unit) before the
    /// target offset must shift the column by its UTF-16 width, not its byte
    /// width: byte-width accounting would overshoot the column.
    #[test]
    fn accented_char_before_offset_uses_utf16_width() {
        let content = "café x";
        // byte offset of 'x' (after "café "): "café ".len() == 6 bytes.
        let offset = "café ".len();
        let pos = offset_to_position(offset, content);
        assert_eq!(pos, Position::new(0, 5));
    }

    /// An emoji (4 UTF-8 bytes, 2 UTF-16 units, 1 Unicode scalar value) must
    /// count as 2 columns, not 1 (scalar-value counting) or 4 (byte counting).
    #[test]
    fn emoji_before_offset_counts_as_two_utf16_units() {
        let content = "😀x";
        let offset = "😀".len();
        let pos = offset_to_position(offset, content);
        assert_eq!(pos, Position::new(0, 2));
    }

    #[test]
    fn newline_resets_column_and_advances_line() {
        let content = "café\nx";
        let offset = content.find('x').unwrap();
        let pos = offset_to_position(offset, content);
        assert_eq!(pos, Position::new(1, 0));
    }
}

