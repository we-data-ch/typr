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
use typr_core::components::r#type::type_system::TypeSystem;

type Span<'a> = LocatedSpan<&'a str, String>;

use super::*;

// ── public entry-point ─────────────────────────────────────────────────────

/// Main entry-point called by the LSP hover handler: resolve the type of the
/// word under the cursor against an already-built [`DocumentAnalysis`] (see
/// `analyze_document`; `Backend` caches it per-URI so unedited text doesn't
/// pay for a fresh parse + type-check on every hover).
///
/// Returns `None` when the cursor is not on an identifier/literal.
pub fn resolve_hover(
    analysis: &DocumentAnalysis,
    content: &str,
    line: u32,
    character: u32,
) -> Option<HoverInfo> {
    // 1. Extract the word under the cursor.
    let (word, word_range) = extract_word_at(content, line, character)?;

    // 2. Look up the word in the context.
    let types = analysis.context.get_types_from_name(&word);

    let typ = if types.is_empty() {
        // Fallback: try to infer the type of the word as a literal.
        infer_literal_type(&word)?
    } else {
        // Pick the most specific (last) type when there are multiple overloads.
        types.last().unwrap().clone()
    };

    // 6. Render with Markdown highlighting.
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
    } else if word.chars().next().is_some_and(|c| c.is_uppercase()) {
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
#[cfg(test)]
mod hover_project_mode_tests {
    use super::*;

    /// Hover on a call to a function imported via `mod`/`use` in Project mode
    /// must resolve the real signature, not fall back to `Any`. Regression
    /// test for the hover path skipping `metaprogrammation()`.
    #[test]
    fn hover_on_imported_function_call_keeps_real_type() {
        let dir = std::env::temp_dir().join(format!("typr_lsp_hover_proj_{}", std::process::id()));
        let typr_dir = dir.join("TypR");
        let _ = std::fs::create_dir_all(&typr_dir);
        std::fs::write(dir.join("DESCRIPTION"), "Package: x\n").unwrap();
        std::fs::write(dir.join("NAMESPACE"), "").unwrap();
        std::fs::write(
            typr_dir.join("mathmod.ty"),
            "@pub let add_two <- fn(a: int, b: int): int { a + b };\n",
        )
        .unwrap();

        let main_file = typr_dir.join("main.ty");
        let content = "mod mathmod;\n\nuse mathmod::add_two;\n\nlet x <- add_two(1, 2);\n";
        std::fs::write(&main_file, content).unwrap();

        let analysis = analyze_document(content, main_file.to_str().unwrap());
        let info = analysis.and_then(|a| resolve_hover(&a, content, 4, 4));

        let _ = std::fs::remove_dir_all(&dir);

        let info = info.expect("hover should resolve a type for `x`");
        assert!(
            info.type_display.contains("int"),
            "expected the imported function's real return type (int), got: {}",
            info.type_display
        );
        assert!(
            !info.type_display.to_lowercase().contains("any"),
            "hover regressed to Any: {}",
            info.type_display
        );
    }
}

