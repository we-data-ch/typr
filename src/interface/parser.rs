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
    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    let ast = ast.ok()?;

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
        word,                          // variable name in bold code
        highlighted,                   // inline Markdown-highlighted type
        typ.pretty()                   // plain code-block fallback (always readable)
    );

    Some(HoverInfo {
        type_display: markdown,
        range: word_range,
    })
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
        if (ch == '#' || ch == '%') && i + 1 < len && (chars[i + 1].is_alphanumeric() || chars[i + 1] == '_') {
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
        Some(c) if c.is_ascii_uppercase() => {
            chars.all(|c| c.is_ascii_digit())
        }
        _ => false,
    }
}
