//! Shared low-level helpers: source-position conversion, word extraction, literal typing.

use nom_locate::LocatedSpan;
use tower_lsp_server::ls_types::{Position, Range};
use typr_core::components::r#type::Type;
use typr_core::utils::builder;

pub type Span<'a> = LocatedSpan<&'a str, String>;

/// Convert a byte offset (as produced by `nom_locate`'s `location_offset`) to
/// an LSP `Position`. LSP positions use UTF-16 code units for the character
/// column by default (no `positionEncoding` is negotiated), so `col` is
/// accumulated in UTF-16 units rather than bytes or Unicode scalar values.
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
