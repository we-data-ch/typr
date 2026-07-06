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
    Position, Range,
};

type Span<'a> = LocatedSpan<&'a str, String>;


// ── rename / references (occurrence search) ────────────────────────────────

/// Main entry-point shared by the LSP rename and references handlers: finds
/// every occurrence of the plain identifier under the cursor within the
/// given document text.
///
/// This is a single-document, textual occurrence search (not a full
/// scope-aware resolution): it matches the identifier at word boundaries,
/// skips text after a `#` comment marker on each line, and skips occurrences
/// immediately preceded by `.` or `$` (field/method access on some other
/// value, not a use of the variable itself). Good enough for renaming a
/// `let`-bound name and its uses within one file; it does not follow the
/// symbol across files.
pub fn find_word_occurrences_at(content: &str, line: u32, character: u32) -> Option<Vec<Range>> {
    let (word, _) = extract_plain_word_at(content, line, character)?;
    if word.is_empty() {
        return None;
    }

    let mut ranges = Vec::new();
    for (idx, src_line) in content.lines().enumerate() {
        ranges.extend(find_word_occurrences_in_line(src_line, &word, idx as u32));
    }
    Some(ranges)
}

/// The range of the plain identifier under the cursor, for `prepareRename`.
pub fn find_renameable_range_at(content: &str, line: u32, character: u32) -> Option<Range> {
    extract_plain_word_at(content, line, character).map(|(_, range)| range)
}

fn find_word_occurrences_in_line(line: &str, word: &str, line_idx: u32) -> Vec<Range> {
    let scan_line = match line.find('#') {
        Some(idx) => &line[..idx],
        None => line,
    };

    let chars: Vec<char> = scan_line.chars().collect();
    let word_chars: Vec<char> = word.chars().collect();
    let wlen = word_chars.len();
    if wlen == 0 {
        return Vec::new();
    }

    let mut ranges = Vec::new();
    let mut i = 0;
    while i + wlen <= chars.len() {
        if chars[i..i + wlen] == word_chars[..] {
            let before_ok = i == 0
                || (!is_plain_word_char_ch(chars[i - 1])
                    && chars[i - 1] != '.'
                    && chars[i - 1] != '$');
            let after_ok = i + wlen == chars.len() || !is_plain_word_char_ch(chars[i + wlen]);

            if before_ok && after_ok {
                let start_col: u32 = chars[..i].iter().map(|c| c.len_utf16() as u32).sum();
                let token_width: u32 = chars[i..i + wlen]
                    .iter()
                    .map(|c| c.len_utf16() as u32)
                    .sum();
                ranges.push(Range::new(
                    Position::new(line_idx, start_col),
                    Position::new(line_idx, start_col + token_width),
                ));
                i += wlen;
                continue;
            }
        }
        i += 1;
    }
    ranges
}

/// Extract the contiguous plain identifier (alphanumeric/underscore only, no
/// dots) under the cursor. Unlike `extract_word_at` (used by hover/goto, which
/// deliberately includes `.` so `Math.pi`-style dotted paths resolve as one
/// token), rename/references need the bare variable name on either side of a
/// dot to be treated as separate tokens.
pub fn extract_plain_word_at(content: &str, line: u32, character: u32) -> Option<(String, Range)> {
    let source_line = content.lines().nth(line as usize)?;

    if (character as usize) > source_line.len() {
        return None;
    }

    let bytes = source_line.as_bytes();
    let col = character as usize;

    if col >= bytes.len() || !is_plain_word_char(bytes[col]) {
        if col == 0 {
            return None;
        }
        if !is_plain_word_char(bytes[col - 1]) {
            return None;
        }
    }

    let anchor = if col < bytes.len() && is_plain_word_char(bytes[col]) {
        col
    } else {
        col - 1
    };

    let start = {
        let mut i = anchor;
        while i > 0 && is_plain_word_char(bytes[i - 1]) {
            i -= 1;
        }
        i
    };

    let end = {
        let mut i = anchor;
        while i + 1 < bytes.len() && is_plain_word_char(bytes[i + 1]) {
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

/// A character is part of a plain identifier if it is alphanumeric or `_`
/// (unlike `is_word_char`, dots are not included — see `extract_plain_word_at`).
pub fn is_plain_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn is_plain_word_char_ch(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[cfg(test)]
mod rename_and_references_tests {
    use super::*;

    /// Renaming a `let`-bound variable must find every plain-identifier use,
    /// in declaration order, while skipping unrelated substrings (e.g. `pi`
    /// must not match inside `pi_approx`).
    #[test]
    fn finds_all_occurrences_of_a_variable() {
        let content = "let pi <- 3;\nlet pi_approx <- 3.14;\nlet x <- pi + pi;\n";
        // Cursor on the `pi` in `let pi <- 3;` (line 0, col 4).
        let ranges =
            find_word_occurrences_at(content, 0, 5).expect("expected occurrences for `pi`");

        // Exactly 3: the declaration plus the two uses in `pi + pi` — not the
        // `pi` inside `pi_approx`.
        assert_eq!(ranges.len(), 3, "ranges: {:?}", ranges);
    }

    /// A field/method access like `obj.pi` must not be counted as a use of
    /// the variable `pi` (the `.`-prefixed occurrence is skipped).
    #[test]
    fn skips_dot_prefixed_occurrences() {
        let content = "let pi <- 3;\nlet y <- obj.pi;\n";
        let ranges = find_word_occurrences_at(content, 0, 5).unwrap();
        assert_eq!(
            ranges.len(),
            1,
            "expected only the declaration, got: {:?}",
            ranges
        );
    }

    /// Text after a `#` comment marker on a line must not be scanned for
    /// occurrences.
    #[test]
    fn skips_occurrences_in_comments() {
        let content = "let pi <- 3; # pi note\n";
        let ranges = find_word_occurrences_at(content, 0, 5).unwrap();
        assert_eq!(
            ranges.len(),
            1,
            "expected only the declaration, got: {:?}",
            ranges
        );
    }

    /// `prepareRename` must report the bare identifier's range, not a
    /// dot-extended token (unlike hover's `extract_word_at`).
    #[test]
    fn renameable_range_excludes_dots() {
        let content = "let x <- Math.pi;\n";
        // Cursor on `pi` within `Math.pi` (byte offset of 'p' is 14).
        let range = find_renameable_range_at(content, 0, 15).expect("expected a range");
        assert_eq!(range.start, Position::new(0, 14));
        assert_eq!(range.end, Position::new(0, 16));
    }
}

