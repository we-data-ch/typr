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
use tower_lsp_server::ls_types::{
    CompletionItem, CompletionItemKind, ParameterInformation, ParameterLabel, Position, Range,
    SignatureHelp, SignatureInformation,
};
use typr_core::components::context::config::Environment;
use typr_core::components::context::Context;
use typr_core::components::error_message::help_data::HelpData;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::vector_type::VecType;
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

/// The result of parsing + metaprogramming + type-checking a whole document
/// once. Hover, go-to-definition and signature-help all only need the final
/// `Context` — they previously each ran this same parse→metaprogram→typing
/// pipeline from scratch. `Backend` caches this per-URI keyed by a content
/// hash (see `lsp.rs`) so repeated requests against unchanged text skip
/// straight to the cheap position-specific lookup below.
#[derive(Debug, Clone)]
pub struct DocumentAnalysis {
    pub context: Context,
    /// The parsed (post-metaprogrammation) AST. Go-to-definition needs this
    /// alongside `context`: module field exports and function parameters
    /// either lose their precise `HelpData` or disappear entirely by the time
    /// the final `Context` is built (see `resolve_definition`'s module-field
    /// and parameter branches for why each needs the AST specifically).
    pub ast: Lang,
}

/// Parse, resolve `mod`/`use` imports, and type-check `content` once. Returns
/// `None` if parsing or type-checking panics (e.g. incomplete/invalid code
/// mid-edit) — callers treat that the same way the old per-feature pipelines
/// did (silently produce no result rather than surfacing an error).
pub fn analyze_document(content: &str, file_path: &str) -> Option<DocumentAnalysis> {
    let span: Span = LocatedSpan::new_extra(content, file_path.to_string());
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    let ast = parse_result.ok()?.ast;

    let environment = detect_environment(file_path);
    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        metaprogrammation(ast, environment)
    }))
    .ok()?;

    let context = Context::default().set_environment(environment);
    let type_context =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)));
    let context = type_context.ok()?.context;

    Some(DocumentAnalysis { context, ast })
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
fn find_field_help_data(members: &[Lang], field_name: &str) -> Option<HelpData> {
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

// ── signature help ──────────────────────────────────────────────────────────

/// Main entry-point called by the LSP signatureHelp handler: resolve the
/// active call's signature(s) against an already-built [`DocumentAnalysis`]
/// (see `analyze_document`).
///
/// Returns `None` when the cursor is not inside a function-call's argument
/// list, or when the call's callee can't be resolved to a `Type::Function`
/// in the typing context.
pub fn resolve_signature_help(
    analysis: &DocumentAnalysis,
    content: &str,
    line: u32,
    character: u32,
) -> Option<SignatureHelp> {
    // 1. Find the enclosing call's callee name and which argument the cursor
    // currently sits in (by counting top-level commas back to the call's `(`).
    let (call_name, active_param) = find_enclosing_call(content, line, character)?;

    // 2. Resolve every `Type::Function` overload registered under that name.
    let types = analysis.context.get_types_from_name(&call_name);
    let function_types: Vec<&Type> = types.iter().filter(|t| t.is_function()).collect();
    if function_types.is_empty() {
        return None;
    }

    let signatures: Vec<SignatureInformation> = function_types
        .iter()
        .map(|t| build_signature_information(&call_name, t))
        .collect();

    // Prefer the first overload whose parameter count covers the active
    // index; fall back to the first overload otherwise.
    let active_signature = function_types
        .iter()
        .position(|t| matches!(t, Type::Function(params, _, _) if active_param < params.len()))
        .unwrap_or(0);

    Some(SignatureHelp {
        signatures,
        active_signature: Some(active_signature as u32),
        active_parameter: Some(active_param as u32),
    })
}

/// Build a `SignatureInformation` (label + per-parameter labels) from a
/// `Type::Function`.
fn build_signature_information(name: &str, typ: &Type) -> SignatureInformation {
    if let Type::Function(params, ret, _) = typ {
        let param_labels: Vec<String> = params
            .iter()
            .map(|p| format!("{}: {}", p.get_argument_str(), p.get_type().pretty()))
            .collect();

        let parameters: Vec<ParameterInformation> = param_labels
            .iter()
            .map(|label| ParameterInformation {
                label: ParameterLabel::Simple(label.clone()),
                documentation: None,
            })
            .collect();

        let label = format!("{}({}) -> {}", name, param_labels.join(", "), ret.pretty());

        SignatureInformation {
            label,
            documentation: None,
            parameters: Some(parameters),
            active_parameter: None,
        }
    } else {
        SignatureInformation {
            label: format!("{}: {}", name, typ.pretty()),
            documentation: None,
            parameters: None,
            active_parameter: None,
        }
    }
}

/// Scan backward from the cursor (across up to a few preceding lines, like
/// `extract_multiline_prefix`) to find the innermost unmatched `(` and the
/// callee name immediately preceding it, plus the number of top-level commas
/// between that `(` and the cursor (the active parameter index).
///
/// Returns `None` when the cursor sits directly inside a `[...]`/`{...}`
/// without an enclosing call between it and the scan window (no signature
/// help applies there), or when no callee name precedes the `(`.
fn find_enclosing_call(content: &str, line: u32, character: u32) -> Option<(String, usize)> {
    let prefix = extract_multiline_prefix(content, line, character);
    let chars: Vec<char> = prefix.chars().collect();

    let mut depth = 0i32;
    let mut comma_count = 0usize;
    let mut call_open_idx = None;

    let mut i = chars.len();
    while i > 0 {
        i -= 1;
        match chars[i] {
            ')' | ']' | '}' => depth += 1,
            '(' => {
                if depth == 0 {
                    call_open_idx = Some(i);
                    break;
                }
                depth -= 1;
            }
            '[' | '{' if depth == 0 => return None,
            '[' | '{' => depth -= 1,
            ',' if depth == 0 => comma_count += 1,
            _ => {}
        }
    }

    let open_idx = call_open_idx?;

    let mut end = open_idx;
    while end > 0 && chars[end - 1].is_whitespace() {
        end -= 1;
    }
    let mut start = end;
    while start > 0
        && (chars[start - 1].is_alphanumeric()
            || chars[start - 1] == '_'
            || chars[start - 1] == '.')
    {
        start -= 1;
    }
    if start == end {
        return None;
    }

    let name: String = chars[start..end].iter().collect();
    Some((name, comma_count))
}

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
fn extract_plain_word_at(content: &str, line: u32, character: u32) -> Option<(String, Range)> {
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
fn is_plain_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn is_plain_word_char_ch(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
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
// ── AUTOCOMPLETION ────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

/// Main entry point for LSP completion requests.
pub fn get_completions_at(
    content: &str,
    line: u32,
    character: u32,
    file_path: &str,
) -> Vec<CompletionItem> {
    // `mod <name>;` completions only need the file system, not the typing
    // context, so handle them before the (possibly expensive) parse/type-check.
    let prefix = extract_multiline_prefix(content, line, character);
    let ctx = detect_completion_context(&prefix);
    if let CompletionCtx::ModuleFile = ctx {
        return get_module_file_completions(file_path);
    }

    // 1. Parse + type-check the document WITHOUT the cursor line
    let final_context = match parse_document_without_cursor_line(content, line, file_path) {
        Some(ctx) => ctx,
        None => {
            let environment = detect_environment(file_path);
            let span: Span = LocatedSpan::new_extra(content, file_path.to_string());
            let parse_result =
                std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
            let context = Context::default().set_environment(environment);
            match parse_result {
                Ok(result) => {
                    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        metaprogrammation(result.ast, environment)
                    }));
                    match ast {
                        Ok(ast) => {
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
                Err(_) => return get_fallback_completions(),
            }
        }
    };

    // 2. Generate completions based on the already-detected context.
    match ctx {
        CompletionCtx::Type => get_type_completions(&final_context),
        CompletionCtx::Module(name) => get_module_completions(&final_context, &name),
        CompletionCtx::Pipe(expr) => get_pipe_completions(&final_context, &expr),
        CompletionCtx::RecordField(expr) => get_record_field_completions(&final_context, &expr),
        CompletionCtx::DotAccess(expr) => get_dot_completions(&final_context, &expr),
        CompletionCtx::ModuleFile => get_module_file_completions(file_path),
        CompletionCtx::Expression => get_expression_completions(&final_context),
    }
}

/// Parse the document excluding the line containing the cursor.
fn parse_document_without_cursor_line(
    content: &str,
    cursor_line: u32,
    file_path: &str,
) -> Option<Context> {
    let lines: Vec<&str> = content.lines().collect();

    let mut filtered_lines = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        if idx != cursor_line as usize {
            filtered_lines.push(*line);
        }
    }

    let filtered_content = filtered_lines.join("\n");
    let span: Span = LocatedSpan::new_extra(&filtered_content, file_path.to_string());

    let parse_result =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span))).ok()?;
    let environment = detect_environment(file_path);
    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        metaprogrammation(parse_result.ast, environment)
    }))
    .ok()?;

    let context = Context::default().set_environment(environment);

    let final_context = if let Lang::Lines { value: exprs, .. } = &ast {
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
    ModuleFile,
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

    let mut context_lines = lines[start_line..current_line_idx].to_vec();
    context_lines.push(current_line_part);

    context_lines.join("\n")
}

fn detect_completion_context(prefix: &str) -> CompletionCtx {
    // Checked against the raw (un-trimmed) prefix: `trim_end()` would eat the
    // trailing space right after `mod `, which is the signal that the user
    // wants module-file completions.
    let current_line = prefix.rsplit('\n').next().unwrap_or(prefix).trim_start();
    if let Some(rest) = current_line.strip_prefix("mod ") {
        if !rest.contains(';') {
            return CompletionCtx::ModuleFile;
        }
    }

    let trimmed = prefix.trim_end();

    if let Some(before_pipe) = trimmed.strip_suffix("|>") {
        return CompletionCtx::Pipe(extract_expression_before(before_pipe.trim()));
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

                if expr.chars().next().is_some_and(|c| c.is_uppercase()) {
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
        .split([';', '\n'])
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

/// Suggest sibling `.ty` files for `mod <name>;`. The directory scanned mirrors
/// the resolution rule in `metaprogramming::import_file_module_code`: `TypR/`
/// under the project root in Project mode, the current file's directory otherwise.
fn get_module_file_completions(file_path: &str) -> Vec<CompletionItem> {
    if file_path.is_empty() {
        return Vec::new();
    }

    let scan_dir = match detect_environment(file_path) {
        Environment::Project => {
            crate::metaprogramming::find_project_root(file_path).map(|root| root.join("TypR"))
        }
        _ => std::path::Path::new(file_path)
            .parent()
            .map(|p| p.to_path_buf()),
    };

    let Some(dir) = scan_dir else {
        return Vec::new();
    };

    let Ok(entries) = std::fs::read_dir(&dir) else {
        return Vec::new();
    };

    let current_stem = std::path::Path::new(file_path)
        .file_stem()
        .and_then(|s| s.to_str());

    let mut items = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("ty") {
            continue;
        }
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };
        if Some(stem) == current_stem {
            continue;
        }
        items.push(CompletionItem {
            label: stem.to_string(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(format!("{}.ty", stem)),
            ..Default::default()
        });
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

    match &record_type {
        Type::Record(fields, _) => fields
            .iter()
            .map(|arg_type| CompletionItem {
                label: arg_type.get_argument_str(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(arg_type.get_type().pretty()),
                ..Default::default()
            })
            .collect(),
        Type::Vec(VecType::DataFrame, _, inner_type, _) => {
            if let Type::Record(fields, _) = inner_type.as_ref() {
                fields
                    .iter()
                    .map(|arg_type| CompletionItem {
                        label: arg_type.get_argument_str(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(arg_type.get_type().pretty()),
                        ..Default::default()
                    })
                    .collect()
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}

fn get_dot_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    let expr_type = infer_expression_type(context, expr);

    match &expr_type {
        Type::Record(fields, _) => {
            for arg_type in fields {
                items.push(CompletionItem {
                    label: arg_type.get_argument_str(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some(arg_type.get_type().pretty()),
                    ..Default::default()
                });
            }
        }
        Type::Vec(VecType::DataFrame, _, inner_type, _) => {
            if let Type::Record(fields, _) = inner_type.as_ref() {
                for arg_type in fields {
                    items.push(CompletionItem {
                        label: arg_type.get_argument_str(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(arg_type.get_type().pretty()),
                        ..Default::default()
                    });
                }
            }
        }
        _ => {}
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

fn get_first_parameter_type(typ: &Type) -> Option<Type> {
    match typ {
        Type::Function(params, _, _) => params.first().map(|arg| arg.get_type()),
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

#[cfg(test)]
mod mod_completion_tests {
    use super::*;

    /// In Script/StandAlone mode, `mod ` should suggest sibling `.ty` files
    /// from the current file's own directory, excluding the current file itself.
    #[test]
    fn suggests_sibling_files_in_script_mode() {
        let dir = std::env::temp_dir().join(format!("typr_lsp_modcomp_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        std::fs::write(dir.join("person.ty"), "").unwrap();
        std::fs::write(dir.join("animal.ty"), "").unwrap();
        let main_file = dir.join("main.ty");
        std::fs::write(&main_file, "").unwrap();

        let content = "mod ";
        let items = get_completions_at(
            content,
            0,
            content.len() as u32,
            main_file.to_str().unwrap(),
        );

        let _ = std::fs::remove_dir_all(&dir);

        let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"person"), "labels: {:?}", labels);
        assert!(labels.contains(&"animal"), "labels: {:?}", labels);
        assert!(
            !labels.contains(&"main"),
            "should not suggest itself: {:?}",
            labels
        );
    }

    /// In Project mode (DESCRIPTION + NAMESPACE present), `mod ` should scan
    /// `TypR/` under the detected project root, not the file's own directory.
    #[test]
    fn suggests_files_from_typr_dir_in_project_mode() {
        let dir =
            std::env::temp_dir().join(format!("typr_lsp_modcomp_proj_{}", std::process::id()));
        let typr_dir = dir.join("TypR");
        let _ = std::fs::create_dir_all(&typr_dir);
        std::fs::write(dir.join("DESCRIPTION"), "Package: x\n").unwrap();
        std::fs::write(dir.join("NAMESPACE"), "").unwrap();
        std::fs::write(typr_dir.join("person.ty"), "").unwrap();
        let main_file = typr_dir.join("main.ty");
        std::fs::write(&main_file, "").unwrap();

        let content = "mod pe";
        let items = get_completions_at(
            content,
            0,
            content.len() as u32,
            main_file.to_str().unwrap(),
        );

        let _ = std::fs::remove_dir_all(&dir);

        let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"person"), "labels: {:?}", labels);
    }

    /// A function imported via `mod`/`use` in Project mode must show up in
    /// expression completions with its real signature. Regression test for
    /// the completion path skipping `metaprogrammation()`, which used to
    /// leave imported symbols entirely absent from the typing context.
    #[test]
    fn completions_include_function_imported_in_project_mode() {
        let dir =
            std::env::temp_dir().join(format!("typr_lsp_completion_proj_{}", std::process::id()));
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
        let content = "mod mathmod;\n\nuse mathmod::add_two;\n\nlet x <- a";
        std::fs::write(&main_file, content).unwrap();

        let items = get_completions_at(
            content,
            4,
            content.lines().last().unwrap().len() as u32,
            main_file.to_str().unwrap(),
        );

        let _ = std::fs::remove_dir_all(&dir);

        let add_two = items.iter().find(|i| i.label == "add_two");
        assert!(
            add_two.is_some(),
            "imported function missing from completions: {:?}",
            items.iter().map(|i| &i.label).collect::<Vec<_>>()
        );
        let detail = add_two.unwrap().detail.clone().unwrap_or_default();
        assert!(
            detail.contains("int"),
            "expected the imported function's real signature, got: {}",
            detail
        );
    }

    /// Once the statement is finished (`;` typed), `mod ` completions must not
    /// fire anymore, so the user falls back to normal expression completions.
    #[test]
    fn does_not_trigger_after_semicolon() {
        let prefix = "mod person;\nlet x <- ";
        match detect_completion_context(prefix) {
            CompletionCtx::ModuleFile => panic!("should not detect ModuleFile after `;`"),
            _ => {}
        }
    }
}

#[cfg(test)]
mod signature_help_tests {
    use super::*;

    /// Test-only convenience wrapper around `analyze_document` +
    /// `resolve_signature_help`, mirroring the old single-shot entry point.
    fn find_signature_help_at(
        content: &str,
        line: u32,
        character: u32,
        file_path: &str,
    ) -> Option<SignatureHelp> {
        let analysis = analyze_document(content, file_path)?;
        resolve_signature_help(&analysis, content, line, character)
    }

    /// Cursor right after the `(` of a call to a known function must resolve
    /// its signature with `active_parameter` 0.
    #[test]
    fn resolves_signature_for_first_argument() {
        let content = "let add <- fn(a: int, b: int): int { a + b };\nlet x <- add(";
        let line = content.lines().last().unwrap();
        let help = find_signature_help_at(content, 1, line.len() as u32, "test.ty")
            .expect("expected signature help inside add(...)");

        assert_eq!(help.active_parameter, Some(0));
        let sig = &help.signatures[help.active_signature.unwrap() as usize];
        assert!(
            sig.label.starts_with("add("),
            "unexpected label: {}",
            sig.label
        );
        assert_eq!(sig.parameters.as_ref().unwrap().len(), 2);
    }

    /// After the first comma, the active parameter must advance to index 1.
    #[test]
    fn active_parameter_advances_past_comma() {
        let content = "let add <- fn(a: int, b: int): int { a + b };\nlet x <- add(1, ";
        let line = content.lines().last().unwrap();
        let help = find_signature_help_at(content, 1, line.len() as u32, "test.ty")
            .expect("expected signature help inside add(1, ...)");

        assert_eq!(help.active_parameter, Some(1));
    }

    /// Cursor inside an unrelated `[...]` with no enclosing call must not
    /// produce signature help.
    #[test]
    fn no_signature_help_inside_bare_brackets() {
        let content = "let x <- [1, 2, ";
        let help = find_signature_help_at(content, 0, content.len() as u32, "test.ty");
        assert!(help.is_none());
    }
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

#[cfg(test)]
mod goto_definition_tests {
    use super::*;

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
