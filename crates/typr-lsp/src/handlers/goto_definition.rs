//! Go-to-definition: resolve a symbol's declaration site, plus environment detection.

use super::document::DocumentAnalysis;
use super::rename::{extract_plain_word_at, is_plain_word_char};
use super::utils::offset_to_position;
use tower_lsp_server::ls_types::{Position, Range};
use typr_core::components::context::config::Environment;
use typr_core::components::error_message::help_data::HelpData;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::Type;

/// A resolved definition result: the location where a symbol is defined.
#[derive(Debug, Clone)]
pub struct DefinitionInfo {
    /// The source range where the symbol is defined.
    pub range: Range,
    /// The file path where the symbol is defined (None if same file or unknown).
    pub file_path: Option<String>,
}

// ── definition lookup ──────────────────────────────────────────────────────

/// Detect the environment (Project or StandAlone) by looking for DESCRIPTION
/// and NAMESPACE files in parent directories.
pub fn detect_environment(file_path: &str) -> Environment {
    match crate::metaprogramming::find_project_root(file_path) {
        Some(_) => Environment::Project,
        None => Environment::StandAlone,
    }
}

/// Main entry-point called by the LSP goto_definition handler: resolve the
/// definition of the word under the cursor against an already-built
/// [`DocumentAnalysis`] (see `analyze_document`).
///
/// Beyond plain `let`/`type` names (looked up in `Context::variables`/
/// `aliases`), this also resolves:
/// - **union tag variants** (`.Circle` in `match s { .Circle(r) => ... }` or
///   a `.Circle(3.14)` construction) — found by walking the AST for a
///   `type X <- .Circle(..) | ...;` declaration containing a matching
///   `Type::Tag` (see `find_tag_definition_in_ast` for why this walks the
///   AST rather than `Context::aliases()`). Detected by checking the
///   character just *before* the plain identifier, since `.` isn't itself
///   an identifier character (unlike hover's `extract_word_at`, this never
///   swallows the dot into the token).
/// - **module fields** (`Math$pi_approx` — `$`, not `.`, is TypR's
///   field/module-member-access operator; `.` is reserved for UFCS sugar,
///   see the comment on `Op::Dollar` in `embedding.rs`) — found by walking
///   the AST for the `module Math { ... }` declaration and its public
///   `let`/`type` members, because the `Context`'s exported `Type::Module`
///   field names carry no real `HelpData` (`ArgumentType::new` in
///   typr-core stamps them with `HelpData::default()` — only the field's
///   *type* keeps a real one);
/// - **function parameters** — resolved textually (see
///   `find_param_declaration`) since a parameter only lives in the
///   *sub-context* used to type its function's body and never reaches the
///   final `Context` (`processes/type_checking/function.rs`).
#[tracing::instrument(skip_all)]
pub fn resolve_definition(
    analysis: &DocumentAnalysis,
    content: &str,
    line: u32,
    character: u32,
    file_path: &str,
) -> Option<DefinitionInfo> {
    let (word, word_range) = extract_plain_word_at(content, line, character)?;

    // Union tag variant, e.g. `.Circle`. Trying this first is harmless even
    // when the preceding `.` is actually UFCS/record access (`p.foo`) —
    // `find_tag_definition_in_ast` simply returns `None` and the lookup
    // falls through to the plain-identifier path below.
    if preceded_by(content, line, word_range.start.character, '.') {
        if let Some(help_data) = find_tag_definition_in_ast(&analysis.ast, &word) {
            return Some(definition_info_from_help_data(
                &help_data,
                word.len(),
                content,
                file_path,
            ));
        }
    }

    // Module field access, e.g. `Math$pi_approx`. Same reasoning: if the
    // qualifier isn't actually a module (e.g. record `$` access like
    // `personne$age`), this just finds nothing and falls through.
    if let Some(qualifier) = preceding_dollar_qualifier(content, line, word_range.start.character) {
        if let Some(help_data) = find_module_in_ast(&analysis.ast, &qualifier)
            .and_then(|members| find_field_help_data(members, &word))
        {
            return Some(definition_info_from_help_data(
                &help_data,
                word.len(),
                content,
                file_path,
            ));
        }
    }

    // Plain `let`/`type` lookup.
    let definition_var = analysis
        .context
        .variables()
        .find(|(var, _)| var.get_name() == word)
        .map(|(var, _)| var.clone())
        .or_else(|| {
            analysis
                .context
                .aliases()
                .find(|(var, _)| var.get_name() == word)
                .map(|(var, _)| var.clone())
        });

    if let Some(var) = definition_var {
        return Some(definition_info_from_help_data(
            &var.get_help_data(),
            word.len(),
            content,
            file_path,
        ));
    }

    let range = find_param_declaration(content, &word, line)?;
    Some(DefinitionInfo {
        range,
        file_path: None,
    })
}

/// Whether the character immediately before `col` on `line` is `ch`.
fn preceded_by(content: &str, line: u32, col: u32, ch: char) -> bool {
    let Some(source_line) = content.lines().nth(line as usize) else {
        return false;
    };
    let col = col as usize;
    if col == 0 || col > source_line.len() {
        return false;
    }
    source_line.as_bytes()[col - 1] as char == ch
}

/// If the identifier starting at `word_start_col` is directly preceded by
/// `$<qualifier>`, return `qualifier` — e.g. for `Math$pi_approx` with the
/// cursor on `pi_approx`, returns `Some("Math")`. Only resolves the single
/// immediately-preceding segment (the direct qualifier); a longer chain like
/// `A$B$field` resolves `B`, not `A`.
fn preceding_dollar_qualifier(content: &str, line: u32, word_start_col: u32) -> Option<String> {
    if !preceded_by(content, line, word_start_col, '$') {
        return None;
    }
    let source_line = content.lines().nth(line as usize)?;
    let bytes = source_line.as_bytes();
    let dollar_idx = word_start_col as usize - 1;

    let mut start = dollar_idx;
    while start > 0 && is_plain_word_char(bytes[start - 1]) {
        start -= 1;
    }
    if start == dollar_idx {
        return None;
    }
    Some(source_line[start..dollar_idx].to_string())
}

/// Build a `DefinitionInfo` from a `HelpData` and the resolved name's byte
/// length, redirecting to an external file's content when the definition
/// lives outside the current document.
fn definition_info_from_help_data(
    help_data: &HelpData,
    name_len: usize,
    content: &str,
    file_path: &str,
) -> DefinitionInfo {
    let offset = help_data.get_offset();
    let definition_file = help_data.get_file_name();

    let (source_content, file_path_result) =
        if definition_file.is_empty() || definition_file == file_path {
            (content.to_string(), None)
        } else {
            match std::fs::read_to_string(&definition_file) {
                Ok(external_content) => (external_content, Some(definition_file)),
                Err(_) => (content.to_string(), None),
            }
        };

    let pos = offset_to_position(offset, &source_content);
    let end_col = pos.character + name_len as u32;

    DefinitionInfo {
        range: Range::new(pos, Position::new(pos.line, end_col)),
        file_path: file_path_result,
    }
}

/// Search the AST for a `type X <- .Tag(..) | ...;` declaration containing a
/// `Type::Tag` named `tag_name`, returning its `HelpData` (the position of
/// the `.Tag` token). Deliberately walks the AST rather than
/// `Context::aliases()`: type-checking a standalone tag literal (e.g.
/// `.Circle(3.14)`) registers a *synthetic* alias (named `Tag0`, `Tag1`, ...)
/// pointing at the literal's own usage site, and since `aliases()` iterates
/// most-recently-pushed first, that synthetic entry would shadow the real
/// declaration for any tag name that's ever been used standalone.
fn find_tag_definition_in_ast(ast: &Lang, tag_name: &str) -> Option<HelpData> {
    match ast {
        Lang::Alias { target_type, .. } => find_tag_in_type(target_type, tag_name),
        Lang::Lines { value, .. } => value
            .iter()
            .find_map(|m| find_tag_definition_in_ast(m, tag_name)),
        Lang::Scope { body, .. } => body
            .iter()
            .find_map(|m| find_tag_definition_in_ast(m, tag_name)),
        Lang::Module { body, .. } => body
            .iter()
            .find_map(|m| find_tag_definition_in_ast(m, tag_name)),
        _ => None,
    }
}

fn find_tag_in_type(typ: &Type, tag_name: &str) -> Option<HelpData> {
    match typ {
        Type::Tag(name, _, help_data) if name == tag_name => Some(help_data.clone()),
        Type::Operator(_, left, right, _) => {
            find_tag_in_type(left, tag_name).or_else(|| find_tag_in_type(right, tag_name))
        }
        Type::Params(types, _) => types.iter().find_map(|t| find_tag_in_type(t, tag_name)),
        _ => None,
    }
}

/// Find a `Lang::Module` named `module_name` anywhere in the AST (top level,
/// or nested inside `Lines`/`Scope`/another module) and return its member
/// list. Only one nesting level of module *name* resolution is attempted by
/// the caller, but the module itself may appear anywhere structurally.
fn find_module_in_ast<'a>(ast: &'a Lang, module_name: &str) -> Option<&'a Vec<Lang>> {
    match ast {
        Lang::Module { name, body, .. } if name == module_name => Some(body),
        Lang::Module { body, .. } => body.iter().find_map(|m| find_module_in_ast(m, module_name)),
        Lang::Lines { value, .. } => value
            .iter()
            .find_map(|m| find_module_in_ast(m, module_name)),
        Lang::Scope { body, .. } => body.iter().find_map(|m| find_module_in_ast(m, module_name)),
        _ => None,
    }
}

/// Find a public `let`/`type` member named `field_name` among a module's
/// direct members and return the identifier's own `HelpData` (unlike the
/// `Context`'s exported `Type::Module`, this is the real declaration
/// position — see `resolve_definition`'s doc comment).
pub fn find_field_help_data(members: &[Lang], field_name: &str) -> Option<HelpData> {
    members.iter().find_map(|member| match member {
        Lang::Let {
            variable,
            is_public: true,
            ..
        } => {
            let var = Var::try_from(variable).ok()?;
            (var.get_name() == field_name).then(|| var.get_help_data())
        }
        Lang::Alias {
            identifier,
            is_public: true,
            ..
        } => {
            let var = Var::try_from(identifier).ok()?;
            (var.get_name() == field_name).then(|| var.get_help_data())
        }
        _ => None,
    })
}

/// Find the nearest preceding `fn(...)` parameter list (by source position)
/// that declares `word` as a parameter name. Purely textual, single-document
/// — a parameter never reaches the final `Context` (see this function's
/// caller), so there is no structured signal to resolve against; this scans
/// for the literal `fn(` token, depth-tracks to its matching `)`, and splits
/// the parameter list at top-level commas. Picks whichever matching `fn(`
/// starts on the latest line at or before `cursor_line` — correct for the
/// common case (cursor inside that function's own body), but a same-named
/// parameter in an unrelated function that doesn't actually enclose the
/// cursor isn't distinguished from a real enclosing one.
fn find_param_declaration(content: &str, word: &str, cursor_line: u32) -> Option<Range> {
    if word.is_empty() {
        return None;
    }

    let bytes = content.as_bytes();
    let mut search_from = 0usize;
    let mut best: Option<(usize, usize)> = None;

    while let Some(rel) = content[search_from..].find("fn(") {
        let open_paren = search_from + rel + 2;
        let mut depth = 0i32;
        let mut j = open_paren;
        let mut close_paren = None;
        while j < bytes.len() {
            match bytes[j] {
                b'(' => depth += 1,
                b')' => {
                    depth -= 1;
                    if depth == 0 {
                        close_paren = Some(j);
                        break;
                    }
                }
                _ => {}
            }
            j += 1;
        }
        let Some(close_paren) = close_paren else {
            break;
        };

        let start_line = offset_to_position(open_paren, content).line;
        if start_line <= cursor_line {
            let params_text = &content[open_paren + 1..close_paren];
            for (rel_start, entry) in split_top_level(params_text) {
                if let Some((name_rel_start, name)) = entry_param_name(entry) {
                    if name == word {
                        let name_abs_start = open_paren + 1 + rel_start + name_rel_start;
                        best = Some((name_abs_start, name_abs_start + name.len()));
                    }
                }
            }
        }

        search_from = close_paren + 1;
    }

    best.map(|(s, e)| {
        Range::new(
            offset_to_position(s, content),
            offset_to_position(e, content),
        )
    })
}

/// Split a comma-separated list into `(start_offset, text)` entries at depth
/// 0, so a parameter whose type itself contains commas (e.g. a function
/// type) isn't split mid-type.
fn split_top_level(text: &str) -> Vec<(usize, &str)> {
    let mut entries = Vec::new();
    let mut depth = 0i32;
    let mut entry_start = 0usize;
    for (idx, ch) in text.char_indices() {
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            ',' if depth == 0 => {
                entries.push((entry_start, &text[entry_start..idx]));
                entry_start = idx + ch.len_utf8();
            }
            _ => {}
        }
    }
    entries.push((entry_start, &text[entry_start..]));
    entries
}

/// Extract a parameter entry's name and its byte offset relative to the
/// entry's own start, skipping leading whitespace and a variadic `...`
/// prefix, and stopping at `:` (typed param) or the entry's end (untyped
/// lambda param).
fn entry_param_name(entry: &str) -> Option<(usize, &str)> {
    let trimmed_start = entry.len() - entry.trim_start().len();
    let rest = &entry[trimmed_start..];
    let after_dots = rest.strip_prefix("...").unwrap_or(rest);
    let dots_len = rest.len() - after_dots.len();

    let name_len = after_dots
        .find(|c: char| !(c.is_alphanumeric() || c == '_'))
        .unwrap_or(after_dots.len());
    if name_len == 0 {
        return None;
    }

    Some((trimmed_start + dots_len, &after_dots[..name_len]))
}

#[cfg(test)]
mod goto_definition_tests {
    use super::*;
    use crate::handlers::document::analyze_document;

    /// Test-only convenience wrapper around `analyze_document` +
    /// `resolve_definition`, mirroring the old single-shot entry point.
    fn find_definition_at(
        content: &str,
        line: u32,
        character: u32,
        file_path: &str,
    ) -> Option<DefinitionInfo> {
        let analysis = analyze_document(content, file_path)?;
        resolve_definition(&analysis, content, line, character, file_path)
    }

    /// A union tag variant reference (`.Circle` in a `match`) must resolve to
    /// the `.Circle` token in its `type Shape <- .Circle(..) | ...;` alias —
    /// not to wherever a same-named plain identifier happens to live.
    #[test]
    fn tag_variant_resolves_to_alias_declaration() {
        let content = "type Shape <- .Circle(num) | .Square(num);\nlet s <- .Circle(3.14);\nlet area <- match s { .Circle(r) => r, .Square(side) => side };\n";
        // Cursor on `Circle` inside the `.Circle(3.14)` construction (line 1).
        let col = content.lines().nth(1).unwrap().find("Circle").unwrap() as u32;
        let def = find_definition_at(content, 1, col, "test.ty")
            .expect("expected a definition for the `.Circle` tag");

        // Must land on the `.Circle` in the alias declaration (line 0), not
        // on the construction site itself. `Type::Tag`'s `HelpData` is the
        // span of the `.` token itself (see `tag_type` in
        // `parsing/types.rs`), so the resolved column is the dot's position,
        // not `Circle`'s.
        assert_eq!(def.range.start.line, 0);
        let decl_col = content.lines().next().unwrap().find(".Circle").unwrap() as u32;
        assert_eq!(def.range.start.character, decl_col);
    }

    /// A module field accessed via `Module$field` (the real TypR field/module
    /// access operator — see `Op::Dollar`) must resolve to the `let`
    /// declaration inside the `module { ... }` body, not fail silently.
    #[test]
    fn module_field_access_resolves_to_member_let() {
        let content =
            "module Math {\n    @pub let pi_approx <- 3.14;\n};\nlet x <- Math$pi_approx;\n";
        let line3 = content.lines().nth(3).unwrap();
        let col = line3.find("pi_approx").unwrap() as u32;
        let def = find_definition_at(content, 3, col, "test.ty")
            .expect("expected a definition for `Math$pi_approx`");

        assert_eq!(def.range.start.line, 1);
        let decl_col = content.lines().nth(1).unwrap().find("pi_approx").unwrap() as u32;
        // +1: `Var`'s `HelpData` is captured from `starting_char`'s
        // *remaining* span (i.e. just past the first character) rather than
        // the identifier's own start — a pre-existing typr-core parsing
        // quirk (see `reference-ty-gotchas`), not specific to this lookup.
        assert_eq!(def.range.start.character, decl_col + 1);
    }

    /// A function parameter, used inside the body, must resolve to its own
    /// declaration in the enclosing `fn(...)` parameter list — parameters
    /// never reach the final `Context` (see `find_param_declaration`'s doc
    /// comment), so this is a purely textual fallback.
    #[test]
    fn function_parameter_resolves_to_its_declaration() {
        let content = "let add <- fn(a: int, b: int): int { a + b };\n";
        let col = content.find("a + b").unwrap() as u32;
        let def = find_definition_at(content, 0, col, "test.ty")
            .expect("expected a definition for parameter `a`");

        assert_eq!(def.range.start.line, 0);
        let decl_col = content.find("a: int").unwrap() as u32;
        assert_eq!(def.range.start.character, decl_col);
    }

    /// A record `$` field access (`personne$age`) is not a module field —
    /// the qualifier resolves to a `Record`, not a `Module`, in the AST, so
    /// this must fall through cleanly to `None` rather than panicking or
    /// resolving to something unrelated.
    #[test]
    fn record_dollar_access_does_not_false_positive_as_module_field() {
        let content =
            "type Person <- list { age: int };\nlet p <- Person:{ age = 1 };\nlet a <- p$age;\n";
        let col = content.lines().nth(2).unwrap().find("age").unwrap() as u32;
        let def = find_definition_at(content, 2, col, "test.ty");
        assert!(def.is_none(), "expected no definition, got: {:?}", def);
    }
}
