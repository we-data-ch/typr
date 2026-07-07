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
use std::collections::HashMap;
use tower_lsp_server::ls_types::{
    CompletionItem, CompletionItemKind, Diagnostic, Documentation, InlayHint, InlayHintKind,
    InlayHintLabel, MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, Position,
    Range, SemanticToken, SemanticTokenModifier, SemanticTokenType, SignatureHelp,
    SignatureInformation, TextEdit,
};
use typr_core::components::context::config::Environment;
use typr_core::components::context::Context;
use typr_core::components::error_message::help_data::HelpData;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::tchar::Tchar;
use typr_core::components::r#type::type_operator::TypeOperator;
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
    /// Declared-name → doc-comment text, built once from `ast` (see
    /// `build_name_doc_map`) so hover/completion don't re-walk the AST on
    /// every request against unchanged text.
    pub doc_map: HashMap<String, String>,
}

impl DocumentAnalysis {
    /// Build a `DocumentAnalysis` from an already parsed+typed `context`/`ast`
    /// pair, deriving `doc_map` from `ast` so every construction site stays
    /// in sync (see `check_code_and_extract_errors` in `lsp.rs` for the other
    /// caller, which type-checks separately for diagnostics purposes).
    pub fn new(context: Context, ast: Lang) -> Self {
        let doc_map = build_name_doc_map(&ast);
        Self {
            context,
            ast,
            doc_map,
        }
    }
}

/// Parse, resolve `mod`/`use` imports, and type-check `content` once. Returns
/// `None` if parsing or type-checking panics (e.g. incomplete/invalid code
/// mid-edit) — callers treat that the same way the old per-feature pipelines
/// did (silently produce no result rather than surfacing an error).
#[tracing::instrument(skip_all)]
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

    Some(DocumentAnalysis::new(context, ast))
}

/// A resolved definition result: the location where a symbol is defined.
#[derive(Debug, Clone)]
pub struct DefinitionInfo {
    /// The source range where the symbol is defined.
    pub range: Range,
    /// The file path where the symbol is defined (None if same file or unknown).
    pub file_path: Option<String>,
}

// ── doc-comment lookup ──────────────────────────────────────────────────────

/// Walk `ast`, collecting doc-comment blocks (consecutive `Lang::Comment`
/// lines immediately preceding a `let`/`type`/`module` declaration, no
/// intervening non-comment nodes) into a map from the declared name to the
/// joined comment text. Mirrors `typr_core::processes::spg::doc_attach`'s
/// `build_doc_map`, but keys by name instead of byte offset: hover and
/// completion look a symbol up by identifier, not by AST position, and the
/// SPG's offset (the `let`/`type`/`module` keyword's own position) doesn't
/// line up with the `Var`'s offset the rest of this module resolves against.
fn build_name_doc_map(ast: &Lang) -> HashMap<String, String> {
    let mut map = HashMap::new();
    collect_name_docs(ast, &mut map);
    map
}

fn collect_name_docs(lang: &Lang, map: &mut HashMap<String, String>) {
    match lang {
        Lang::Lines { value, .. } => {
            attach_name_docs(value, map);
            for item in value {
                collect_name_docs(item, map);
            }
        }
        Lang::Module { body, .. } => {
            attach_name_docs(body, map);
            for item in body {
                collect_name_docs(item, map);
            }
        }
        _ => {}
    }
}

/// Scan a flat slice of `Lang` items, flushing any accumulated comment block
/// onto the next declaration's name when one is found.
fn attach_name_docs(items: &[Lang], map: &mut HashMap<String, String>) {
    let mut pending: Vec<String> = Vec::new();

    for item in items {
        if let Lang::Comment { value, .. } = item {
            pending.push(value.trim().to_string());
            continue;
        }
        if !pending.is_empty() {
            if let Some(name) = declared_name(item) {
                map.insert(name, pending.join("\n"));
            }
            pending.clear();
        }
    }
}

/// The name a top-level declaration binds, for doc-comment attachment
/// purposes. `None` for anything that isn't a documentable declaration.
fn declared_name(lang: &Lang) -> Option<String> {
    match lang {
        Lang::Let { variable, .. } => doc_target_name(variable),
        Lang::Alias { identifier, .. } => doc_target_name(identifier),
        Lang::Module { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn doc_target_name(lang: &Lang) -> Option<String> {
    match lang {
        Lang::Variable { name, .. } => Some(name.clone()),
        _ => None,
    }
}

// ── public entry-point ─────────────────────────────────────────────────────

/// Main entry-point called by the LSP hover handler: resolve the type of the
/// word under the cursor against an already-built [`DocumentAnalysis`] (see
/// `analyze_document`; `Backend` caches it per-URI so unedited text doesn't
/// pay for a fresh parse + type-check on every hover).
///
/// Returns `None` when the cursor is not on an identifier/literal.
#[tracing::instrument(skip_all)]
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
    let mut markdown = format!(
        "**`{}`** : {}\n\n```\n{}\n```",
        word,         // variable name in bold code
        highlighted,  // inline Markdown-highlighted type
        typ.pretty()  // plain code-block fallback (always readable)
    );

    // 7. Append the declaration's doc-comment, if any.
    if let Some(doc) = analysis.doc_map.get(&word) {
        markdown.push_str("\n\n---\n\n");
        markdown.push_str(doc);
    }

    Some(HoverInfo {
        type_display: markdown,
        range: word_range,
    })
}

// ── inlay hints ─────────────────────────────────────────────────────────────

/// Main entry-point called by the LSP inlayHint handler: walk the whole
/// document AST and emit two kinds of hints against an already-built
/// [`DocumentAnalysis`] (see `analyze_document`):
///   - **type hints** on `let`s with no `: Type` annotation in the source
///     (`Lang::Let.r#type` is `Type::Empty` exactly when the annotation was
///     omitted — see the parser's `let_exp`/`let_tuple_exp`), looked up by
///     name in the final `Context` the same way `resolve_hover` does;
///   - **parameter-name hints** before each positional argument at a call
///     site, resolved from the callee's `Type::Function` signature.
///
/// Walks the *pre-typing* AST (`analysis.ast`), not the `Context`, so every
/// node keeps the exact source position it had while parsing — the same
/// reason `resolve_definition`'s module-field/parameter lookups need the AST
/// instead of the `Context`.
///
/// **Known scope limit**, shared with `resolve_hover` (same `Context::
/// get_types_from_name` lookup): a `let` whose binding only lives in a
/// sub-context discarded after typing — e.g. a local inside a `fn`/`\(...)`
/// body (`processes/type_checking/function.rs`: "Only alias registrations
/// are hoisted — body variables stay local"), or inside an `if`/`else`
/// branch — gets no type hint, because the final `Context` never saw it.
/// Top-level `let`s (and anything else whose binding survives into the
/// final `Context`) resolve correctly. Fixing the general case would mean
/// re-threading intermediate contexts through this walk instead of
/// consulting one flat final `Context`, which is a bigger lift than this
/// first pass — see `toward_LSP.md`.
#[tracing::instrument(skip_all)]
pub fn resolve_inlay_hints(analysis: &DocumentAnalysis, content: &str) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    collect_inlay_hints(&analysis.ast, &analysis.context, content, &mut hints);
    hints
}

/// Recursively walk `lang`, appending inlay hints to `hints`. Covers every
/// `Lang` variant that can contain nested statements/expressions; leaf
/// variants (literals, bare `Variable`, R-specific raw-string bodies, …) are
/// caught by the final `_ => {}` arm.
fn collect_inlay_hints(lang: &Lang, context: &Context, content: &str, hints: &mut Vec<InlayHint>) {
    match lang {
        Lang::Lines { value, .. }
        | Lang::Scope { body: value, .. }
        | Lang::Sequence { body: value, .. }
        | Lang::Test { value, .. }
        | Lang::Array { value, .. }
        | Lang::Vector { value, .. }
        | Lang::Tuple { value, .. } => {
            for stmt in value {
                collect_inlay_hints(stmt, context, content, hints);
            }
        }

        Lang::Module { body, .. } => {
            for member in body {
                collect_inlay_hints(member, context, content, hints);
            }
        }

        Lang::Function { body, .. } | Lang::Lambda { body, .. } => {
            collect_inlay_hints(body, context, content, hints);
        }

        Lang::Let {
            variable,
            r#type,
            expression,
            ..
        } => {
            // Skip `let f <- fn(...): T { ... }`: the function's own
            // signature is already fully spelled out in the source, so a
            // type hint on `f` itself would just be noise.
            let is_explicit_function = matches!(expression.as_ref(), Lang::Function { .. });
            if matches!(r#type, Type::Empty(_)) && !is_explicit_function {
                if let Ok(var) = Var::try_from(variable) {
                    let types = context.get_types_from_name(&var.get_name());
                    if let Some(typ) = types.last().filter(|t| !matches!(t, Type::Empty(_))) {
                        // `Var`'s `HelpData` offset is captured one byte past
                        // the identifier's real start (see the `+1` note on
                        // `module_field_access_resolves_to_member_let` — the
                        // same parser quirk applies to every `Var`, not just
                        // module fields), so the identifier's real start is
                        // one byte earlier; from there, its end is `name`'s
                        // byte length further.
                        let name = var.get_name();
                        let real_start_offset = var.get_help_data().get_offset().saturating_sub(1);
                        let end_offset = real_start_offset + name.len();
                        let position = offset_to_position(end_offset, content);
                        hints.push(InlayHint {
                            position,
                            label: InlayHintLabel::String(format!(": {}", typ.pretty())),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(true),
                            padding_right: Some(false),
                            data: None,
                        });
                    }
                }
            }
            collect_inlay_hints(expression, context, content, hints);
        }

        Lang::If {
            condition,
            if_block,
            else_block,
            ..
        } => {
            collect_inlay_hints(condition, context, content, hints);
            collect_inlay_hints(if_block, context, content, hints);
            collect_inlay_hints(else_block, context, content, hints);
        }

        Lang::Match {
            target, branches, ..
        } => {
            collect_inlay_hints(target, context, content, hints);
            for (_, body) in branches {
                collect_inlay_hints(body, context, content, hints);
            }
        }

        Lang::ForLoop {
            expression, body, ..
        } => {
            collect_inlay_hints(expression, context, content, hints);
            collect_inlay_hints(body, context, content, hints);
        }

        Lang::WhileLoop {
            condition, body, ..
        } => {
            collect_inlay_hints(condition, context, content, hints);
            collect_inlay_hints(body, context, content, hints);
        }

        Lang::Loop { body, .. } => collect_inlay_hints(body, context, content, hints),

        Lang::Return { value, .. }
        | Lang::Not { value, .. }
        | Lang::Tag { value, .. }
        | Lang::TestBlock { value, .. }
        | Lang::KeyValue { value, .. } => {
            collect_inlay_hints(value, context, content, hints);
        }

        Lang::Assign { expression, .. } => {
            collect_inlay_hints(expression, context, content, hints);
        }

        Lang::ArrayIndexing {
            identifier,
            indexing,
            ..
        } => {
            collect_inlay_hints(identifier, context, content, hints);
            collect_inlay_hints(indexing, context, content, hints);
        }

        Lang::Operator { lhs, rhs, .. } => {
            collect_inlay_hints(lhs, context, content, hints);
            collect_inlay_hints(rhs, context, content, hints);
        }

        Lang::Union(a, b, _) => {
            collect_inlay_hints(a, context, content, hints);
            collect_inlay_hints(b, context, content, hints);
        }

        Lang::JSBlock(inner, _, _) => collect_inlay_hints(inner, context, content, hints),

        Lang::Use { lang, members, .. } => {
            collect_inlay_hints(lang, context, content, hints);
            collect_inlay_hints(members, context, content, hints);
        }

        Lang::List { value, spreads, .. } => {
            for arg in value {
                collect_inlay_hints(&arg.get_value(), context, content, hints);
            }
            for spread in spreads {
                collect_inlay_hints(spread, context, content, hints);
            }
        }

        Lang::DataFrame { value, .. } => {
            for arg in value {
                collect_inlay_hints(&arg.get_value(), context, content, hints);
            }
        }

        Lang::ConstructorCall {
            fields, spreads, ..
        } => {
            for arg in fields {
                collect_inlay_hints(&arg.get_value(), context, content, hints);
            }
            for spread in spreads {
                collect_inlay_hints(spread, context, content, hints);
            }
        }

        Lang::UnionConstructor { fields, .. } => {
            for arg in fields {
                collect_inlay_hints(&arg.get_value(), context, content, hints);
            }
        }

        Lang::ArrayConstructorCall { elements, .. } => {
            for elem in elements {
                collect_inlay_hints(elem, context, content, hints);
            }
        }

        Lang::ValidatingCast { expression, .. } => {
            collect_inlay_hints(expression, context, content, hints);
        }

        Lang::PartialApp {
            function,
            arguments,
            ..
        } => {
            collect_inlay_hints(function, context, content, hints);
            for arg in arguments {
                collect_inlay_hints(arg, context, content, hints);
            }
        }

        Lang::FunctionApp {
            identifier,
            arguments,
            ..
        }
        | Lang::VecFunctionApp {
            identifier,
            arguments,
            ..
        } => {
            parameter_name_hints(identifier, arguments, context, content, hints);
            collect_inlay_hints(identifier, context, content, hints);
            for arg in arguments {
                collect_inlay_hints(arg, context, content, hints);
            }
        }

        _ => {}
    }
}

/// Whether `typ` is a `Type::Function` whose arity can accept `arg_count`
/// positional arguments — accounting for a variadic tail parameter (`...xs`)
/// absorbing any number of trailing arguments.
fn function_type_matches_arity(typ: &Type, arg_count: usize) -> bool {
    match typ {
        Type::Function(params, _, _) => {
            if params.last().is_some_and(|p| p.is_variadic()) {
                arg_count + 1 >= params.len()
            } else {
                params.len() == arg_count
            }
        }
        _ => false,
    }
}

/// Emit a `PARAMETER` inlay hint before each positional argument of a call,
/// resolved from the callee's `Type::Function` signature in `context`. Only
/// handles a plain `Lang::Variable` callee (matching `resolve_hover`'s
/// lookup); best-effort overload resolution just picks the first signature
/// whose arity fits. Skipped for a variadic parameter (labeling every
/// trailing positional argument with the same name is more noise than
/// signal) and suppressed when the argument is already a bare variable with
/// the same name as the parameter (rust-analyzer-style convention to cut
/// redundant hints).
fn parameter_name_hints(
    identifier: &Lang,
    arguments: &[Lang],
    context: &Context,
    content: &str,
    hints: &mut Vec<InlayHint>,
) {
    let Lang::Variable { name, .. } = identifier else {
        return;
    };
    let types = context.get_types_from_name(name);
    let Some(Type::Function(params, _, _)) = types
        .iter()
        .rev()
        .find(|t| function_type_matches_arity(t, arguments.len()))
    else {
        return;
    };

    for (i, arg) in arguments.iter().enumerate() {
        let param = if i < params.len() {
            &params[i]
        } else {
            match params.last() {
                Some(p) => p,
                None => continue,
            }
        };
        if param.is_variadic() {
            continue;
        }
        let param_name = match &param.0 {
            Type::Char(Tchar::Val(n), _) if !n.is_empty() => n,
            _ => continue,
        };
        if let Lang::Variable { name: arg_name, .. } = arg {
            if arg_name == param_name {
                continue;
            }
        }

        let offset = arg.get_help_data().get_offset();
        let position = offset_to_position(offset, content);
        hints.push(InlayHint {
            position,
            label: InlayHintLabel::String(format!("{}:", param_name)),
            kind: Some(InlayHintKind::PARAMETER),
            text_edits: None,
            tooltip: None,
            padding_left: Some(false),
            padding_right: Some(true),
            data: None,
        });
    }
}

// ── semantic tokens ──────────────────────────────────────────────────────

/// Token type indices into the legend advertised by `ServerCapabilities`
/// (`lsp.rs`'s `semantic_tokens_provider`) — order here must match the
/// `SEMANTIC_TOKEN_TYPES` legend array exactly, since the wire format refers
/// to types by index, not name.
pub const TOKEN_NAMESPACE: u32 = 0;
pub const TOKEN_TYPE: u32 = 1;
pub const TOKEN_ENUM_MEMBER: u32 = 2;
pub const TOKEN_FUNCTION: u32 = 3;
pub const TOKEN_VARIABLE: u32 = 4;

pub const SEMANTIC_TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::TYPE,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
];

/// Bit 0 of a token's modifier set: the token is a declaration site (`let`
/// name, `type` name, `module` name, `@signature` name) rather than a use.
pub const TOKEN_MODIFIER_DECLARATION: u32 = 1;

pub const SEMANTIC_TOKEN_MODIFIERS: &[SemanticTokenModifier] =
    &[SemanticTokenModifier::DECLARATION];

/// One token before delta-encoding: an absolute byte span plus its
/// classification.
struct RawToken {
    start: usize,
    end: usize,
    token_type: u32,
    modifiers: u32,
}

/// Main entry-point called by the LSP `semanticTokens/full` handler: walk
/// the whole document AST and emit tokens for the constructs whose
/// parser-recorded `HelpData` is reliable enough to anchor a token to (see
/// the span helpers below for exactly which conventions are trusted and
/// why — the parser uses at least three different, undocumented offset
/// conventions across `Lang`/`Type` identifiers, and several call sites
/// stamp `HelpData::default()` instead of a real position).
///
/// **Known scope limits**, matching gaps in the parser's own
/// position-tracking rather than anything fixable here:
/// - **generics** (`T`, `U` in `@f: (T) -> U;`) carry `HelpData::default()`
///   from `parsing/types.rs`'s `generic()` — there is no real offset to
///   anchor a `typeParameter` token to, so generics are not highlighted;
/// - **module member / constructor / union-variant names** (`use M::{a}`,
///   `M$field`, `TypeName:{...}`, `Union.Variant`) are only positioned as
///   part of their enclosing call's `HelpData`, not individually, and
///   `ArgumentType::new` stamps exported module field names with
///   `HelpData::default()` too (see `resolve_definition`'s doc comment for
///   the same finding in the goto-definition path) — skipped for the same
///   reason;
/// - a `for`-loop's own binding (`Var::from_name` with no `set_help_data`
///   call in `parsing/mod.rs`'s `for_loop`) has no real position either.
#[tracing::instrument(skip_all)]
pub fn resolve_semantic_tokens(analysis: &DocumentAnalysis, content: &str) -> Vec<SemanticToken> {
    let mut raw = Vec::new();
    collect_semantic_tokens(&analysis.ast, &analysis.context, content, &mut raw);
    raw.sort_by_key(|t| t.start);
    raw.dedup_by(|a, b| a.start == b.start && a.end == b.end);
    encode_semantic_tokens(&raw, content)
}

/// Delta-encode a byte-offset-sorted token list into the LSP wire format
/// (each token's line/character delta relative to the previous token).
fn encode_semantic_tokens(raw: &[RawToken], content: &str) -> Vec<SemanticToken> {
    let mut result = Vec::with_capacity(raw.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;
    for tok in raw {
        let length = tok.end.saturating_sub(tok.start) as u32;
        if length == 0 {
            continue;
        }
        let position = offset_to_position(tok.start, content);
        let delta_line = position.line - prev_line;
        let delta_start = if delta_line == 0 {
            position.character.saturating_sub(prev_start)
        } else {
            position.character
        };
        result.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: tok.token_type,
            token_modifiers_bitset: tok.modifiers,
        });
        prev_line = position.line;
        prev_start = position.character;
    }
    result
}

/// Locate a `Lang::Variable`/`Var` identifier's byte span from its
/// `HelpData`. The parser uses two different conventions depending on which
/// grammar branch matched: a lowercase-first name goes through
/// `variable_exp`, whose per-character combinator loop returns the
/// *remaining* span after consuming just the first character — so the
/// offset lands one byte past the real start (the same quirk
/// `collect_inlay_hints` already corrects for `let` type hints). A
/// Pascal-case name goes through `pascal_case_helper`, which consumes the
/// whole identifier in one shot and returns the remaining span *after* it —
/// so the offset lands at the identifier's end. Returns `None` for
/// `HelpData::default()` stand-ins (no real source position).
fn lang_variable_span(name: &str, help_data: &HelpData) -> Option<(usize, usize)> {
    if help_data.get_file_name().is_empty() || name.is_empty() {
        return None;
    }
    let offset = help_data.get_offset();
    if name.starts_with(|c: char| c.is_ascii_uppercase()) {
        let end = offset;
        let start = end.checked_sub(name.len())?;
        Some((start, end))
    } else {
        let start = offset.checked_sub(1)?;
        Some((start, start + name.len()))
    }
}

/// Locate a `Type::Alias`/`Lang::Alias` name's byte span. Both are captured
/// via `parsing/types.rs`'s `type_alias` (built on `pascal_case_no_space`),
/// whose `HelpData` marks the identifier's *second* character regardless of
/// case (`alphanumeric1`'s own start) — a third convention, distinct from
/// `lang_variable_span`'s two. `Lang::Alias`'s own `identifier` field is
/// rebuilt with a fresh, unpositioned `Var::from_name` (see
/// `base_type_exp`/`base_opaque_exp` in `processes/parsing/mod.rs`), so only
/// the variant's outer `help_data` (copied from `type_alias`'s own `h`)
/// keeps a real offset — callers must pass that, not the identifier's.
fn type_alias_span(name: &str, help_data: &HelpData) -> Option<(usize, usize)> {
    if help_data.get_file_name().is_empty() || name.is_empty() {
        return None;
    }
    let start = help_data.get_offset().checked_sub(1)?;
    Some((start, start + name.len()))
}

/// Locate a `Lang::Tag`'s span (`.Circle`), including the leading `.` —
/// `tag_exp`'s `HelpData` marks the dot itself.
fn tag_span(name: &str, help_data: &HelpData) -> Option<(usize, usize)> {
    if help_data.get_file_name().is_empty() {
        return None;
    }
    let start = help_data.get_offset();
    Some((start, start + 1 + name.len()))
}

/// Locate a `Lang::Module`'s declared name. `help_data` marks the `module`
/// keyword itself, not the name (`module`'s own parser only keeps the
/// keyword's `HelpData`), so scan forward in `content` past the keyword for
/// the name text — safe because module names are plain ASCII identifiers
/// and nothing but whitespace can separate the keyword from the name.
fn module_name_span(help_data: &HelpData, name: &str, content: &str) -> Option<(usize, usize)> {
    if help_data.get_file_name().is_empty() || name.is_empty() {
        return None;
    }
    let after_keyword = help_data.get_offset() + "module".len();
    let hay = content.get(after_keyword..)?;
    let idx = hay.find(name)?;
    let start = after_keyword + idx;
    Some((start, start + name.len()))
}

/// Classify a variable reference by its resolved type: a `Type::Function`
/// colors as a function/call, a `Type::Module` as a namespace, anything
/// else (or unresolved) as a plain variable.
///
/// Scans *all* entries `get_types_from_name` returns rather than trusting
/// the last one: a name can carry a resolved type alongside a trailing
/// `Type::UnknownFunction` placeholder (a forward-reference marker, not an
/// actual signature), in no guaranteed order — and `Type::is_function()`
/// itself treats `UnknownFunction` as a function, so it can't be used here
/// without misclassifying an ordinary `let` as a call.
fn classify_variable_token(name: &str, context: &Context) -> u32 {
    let types = context.get_types_from_name(name);
    if types.iter().any(|t| matches!(t, Type::Function(..))) {
        TOKEN_FUNCTION
    } else if types.iter().any(|t| matches!(t, Type::Module(..))) {
        TOKEN_NAMESPACE
    } else {
        TOKEN_VARIABLE
    }
}

/// Walk a `Type` tree collecting `Type::Alias` references (`x: Point`, a
/// function's return type, a record field's type, …) as `TYPE` tokens.
/// `Type::Generic` is deliberately not collected here — see
/// `resolve_semantic_tokens`'s "known scope limits".
fn collect_type_tokens(typ: &Type, tokens: &mut Vec<RawToken>) {
    match typ {
        Type::Alias(name, params, _, help_data) => {
            if let Some((start, end)) = type_alias_span(name, help_data) {
                tokens.push(RawToken {
                    start,
                    end,
                    token_type: TOKEN_TYPE,
                    modifiers: 0,
                });
            }
            for p in params {
                collect_type_tokens(p, tokens);
            }
        }
        Type::Function(params, ret, _) => {
            for p in params {
                collect_type_tokens(&p.1, tokens);
            }
            collect_type_tokens(ret, tokens);
        }
        Type::Vec(_, a, b, _) => {
            collect_type_tokens(a, tokens);
            collect_type_tokens(b, tokens);
        }
        Type::Record(fields, _) | Type::Interface(fields, _) => {
            for f in fields {
                collect_type_tokens(&f.1, tokens);
            }
        }
        Type::Tag(_, inner, _) => collect_type_tokens(inner, tokens),
        Type::Multi(inner, _) => collect_type_tokens(inner, tokens),
        Type::Tuple(v, _) | Type::Params(v, _) => {
            for t in v {
                collect_type_tokens(t, tokens);
            }
        }
        Type::If(c, v, _) => {
            collect_type_tokens(c, tokens);
            for t in v {
                collect_type_tokens(t, tokens);
            }
        }
        Type::Condition(a, b, c, _) => {
            collect_type_tokens(a, tokens);
            collect_type_tokens(b, tokens);
            collect_type_tokens(c, tokens);
        }
        Type::Operator(_, a, b, _) => {
            collect_type_tokens(a, tokens);
            collect_type_tokens(b, tokens);
        }
        _ => {}
    }
}

/// Recursively walk `lang`, appending semantic tokens to `tokens`. Shares
/// its traversal shape with `collect_inlay_hints` (same `Lang` variants
/// need visiting), but the emission logic differs enough — declaration vs.
/// use classification, `Type` sub-trees to walk — that it's its own
/// function rather than piggy-backing on the hints walk.
fn collect_semantic_tokens(
    lang: &Lang,
    context: &Context,
    content: &str,
    tokens: &mut Vec<RawToken>,
) {
    match lang {
        Lang::Lines { value, .. }
        | Lang::Scope { body: value, .. }
        | Lang::Sequence { body: value, .. }
        | Lang::Test { value, .. }
        | Lang::Array { value, .. }
        | Lang::Vector { value, .. }
        | Lang::Tuple { value, .. } => {
            for stmt in value {
                collect_semantic_tokens(stmt, context, content, tokens);
            }
        }

        Lang::Module {
            name,
            body,
            help_data,
            ..
        } => {
            if let Some((start, end)) = module_name_span(help_data, name, content) {
                tokens.push(RawToken {
                    start,
                    end,
                    token_type: TOKEN_NAMESPACE,
                    modifiers: TOKEN_MODIFIER_DECLARATION,
                });
            }
            for member in body {
                collect_semantic_tokens(member, context, content, tokens);
            }
        }

        Lang::Function {
            parameters,
            return_type,
            body,
            ..
        } => {
            for p in parameters {
                collect_type_tokens(&p.1, tokens);
                if let Some(default) = &p.4 {
                    collect_semantic_tokens(default, context, content, tokens);
                }
            }
            collect_type_tokens(return_type, tokens);
            collect_semantic_tokens(body, context, content, tokens);
        }

        Lang::Lambda {
            parameters, body, ..
        } => {
            for param in parameters {
                if let Lang::Variable {
                    name,
                    related_type,
                    help_data,
                    ..
                } = param
                {
                    if let Some((start, end)) = lang_variable_span(name, help_data) {
                        tokens.push(RawToken {
                            start,
                            end,
                            token_type: classify_variable_token(name, context),
                            modifiers: TOKEN_MODIFIER_DECLARATION,
                        });
                    }
                    if !matches!(related_type, Type::Empty(_)) {
                        collect_type_tokens(related_type, tokens);
                    }
                }
            }
            collect_semantic_tokens(body, context, content, tokens);
        }

        Lang::Variable {
            name, help_data, ..
        } => {
            if let Some((start, end)) = lang_variable_span(name, help_data) {
                tokens.push(RawToken {
                    start,
                    end,
                    token_type: classify_variable_token(name, context),
                    modifiers: 0,
                });
            }
        }

        Lang::Let {
            variable,
            r#type,
            expression,
            ..
        } => {
            if !matches!(r#type, Type::Empty(_)) {
                collect_type_tokens(r#type, tokens);
            }
            if let Ok(var) = Var::try_from(variable) {
                let name = var.get_name();
                if let Some((start, end)) = lang_variable_span(&name, &var.get_help_data()) {
                    tokens.push(RawToken {
                        start,
                        end,
                        token_type: classify_variable_token(&name, context),
                        modifiers: TOKEN_MODIFIER_DECLARATION,
                    });
                }
            }
            collect_semantic_tokens(expression, context, content, tokens);
        }

        Lang::Alias {
            identifier,
            target_type,
            help_data,
            ..
        } => {
            if let Some(name) = doc_target_name(identifier) {
                if let Some((start, end)) = type_alias_span(&name, help_data) {
                    tokens.push(RawToken {
                        start,
                        end,
                        token_type: TOKEN_TYPE,
                        modifiers: TOKEN_MODIFIER_DECLARATION,
                    });
                }
            }
            collect_type_tokens(target_type, tokens);
        }

        Lang::Signature {
            identifier,
            target_type,
            ..
        } => {
            let name = identifier.get_name();
            if let Some((start, end)) = lang_variable_span(&name, &identifier.get_help_data()) {
                let token_type = if target_type.is_function() {
                    TOKEN_FUNCTION
                } else {
                    TOKEN_VARIABLE
                };
                tokens.push(RawToken {
                    start,
                    end,
                    token_type,
                    modifiers: TOKEN_MODIFIER_DECLARATION,
                });
            }
            collect_type_tokens(target_type, tokens);
        }

        Lang::ExternBlock {
            parameters,
            return_type,
            ..
        } => {
            for p in parameters {
                collect_type_tokens(&p.1, tokens);
            }
            collect_type_tokens(return_type, tokens);
        }

        Lang::Import { value, .. } => collect_type_tokens(value, tokens),

        Lang::ValidatingCast {
            expression,
            literal_type,
            ..
        } => {
            collect_semantic_tokens(expression, context, content, tokens);
            if let Some(t) = literal_type {
                collect_type_tokens(t, tokens);
            }
        }

        Lang::TypePattern {
            variable_name,
            matched_type,
            help_data,
        } => {
            if let Some((start, end)) = lang_variable_span(variable_name, help_data) {
                tokens.push(RawToken {
                    start,
                    end,
                    token_type: TOKEN_VARIABLE,
                    modifiers: TOKEN_MODIFIER_DECLARATION,
                });
            }
            collect_type_tokens(matched_type, tokens);
        }

        Lang::Tag {
            name,
            value,
            help_data,
        } => {
            if let Some((start, end)) = tag_span(name, help_data) {
                tokens.push(RawToken {
                    start,
                    end,
                    token_type: TOKEN_ENUM_MEMBER,
                    modifiers: 0,
                });
            }
            collect_semantic_tokens(value, context, content, tokens);
        }

        Lang::If {
            condition,
            if_block,
            else_block,
            ..
        } => {
            collect_semantic_tokens(condition, context, content, tokens);
            collect_semantic_tokens(if_block, context, content, tokens);
            collect_semantic_tokens(else_block, context, content, tokens);
        }

        Lang::Match {
            target, branches, ..
        } => {
            collect_semantic_tokens(target, context, content, tokens);
            for (pattern, body) in branches {
                collect_semantic_tokens(pattern, context, content, tokens);
                collect_semantic_tokens(body, context, content, tokens);
            }
        }

        Lang::ForLoop {
            expression, body, ..
        } => {
            collect_semantic_tokens(expression, context, content, tokens);
            collect_semantic_tokens(body, context, content, tokens);
        }

        Lang::WhileLoop {
            condition, body, ..
        } => {
            collect_semantic_tokens(condition, context, content, tokens);
            collect_semantic_tokens(body, context, content, tokens);
        }

        Lang::Loop { body, .. } => collect_semantic_tokens(body, context, content, tokens),

        Lang::Return { value, .. }
        | Lang::Not { value, .. }
        | Lang::TestBlock { value, .. }
        | Lang::KeyValue { value, .. } => {
            collect_semantic_tokens(value, context, content, tokens);
        }

        Lang::Assign {
            identifier,
            expression,
            ..
        } => {
            collect_semantic_tokens(identifier, context, content, tokens);
            collect_semantic_tokens(expression, context, content, tokens);
        }

        Lang::ArrayIndexing {
            identifier,
            indexing,
            ..
        } => {
            collect_semantic_tokens(identifier, context, content, tokens);
            collect_semantic_tokens(indexing, context, content, tokens);
        }

        Lang::Operator { lhs, rhs, .. } => {
            collect_semantic_tokens(lhs, context, content, tokens);
            collect_semantic_tokens(rhs, context, content, tokens);
        }

        Lang::Union(a, b, _) => {
            collect_semantic_tokens(a, context, content, tokens);
            collect_semantic_tokens(b, context, content, tokens);
        }

        Lang::JSBlock(inner, _, _) => collect_semantic_tokens(inner, context, content, tokens),

        Lang::Use { lang, members, .. } => {
            collect_semantic_tokens(lang, context, content, tokens);
            collect_semantic_tokens(members, context, content, tokens);
        }

        Lang::List { value, spreads, .. } => {
            for arg in value {
                collect_semantic_tokens(&arg.get_value(), context, content, tokens);
            }
            for spread in spreads {
                collect_semantic_tokens(spread, context, content, tokens);
            }
        }

        Lang::DataFrame { value, .. } => {
            for arg in value {
                collect_semantic_tokens(&arg.get_value(), context, content, tokens);
            }
        }

        Lang::ConstructorCall {
            fields, spreads, ..
        } => {
            for arg in fields {
                collect_semantic_tokens(&arg.get_value(), context, content, tokens);
            }
            for spread in spreads {
                collect_semantic_tokens(spread, context, content, tokens);
            }
        }

        Lang::UnionConstructor { fields, .. } => {
            for arg in fields {
                collect_semantic_tokens(&arg.get_value(), context, content, tokens);
            }
        }

        Lang::ArrayConstructorCall { elements, .. } => {
            for elem in elements {
                collect_semantic_tokens(elem, context, content, tokens);
            }
        }

        Lang::PartialApp {
            function,
            arguments,
            ..
        } => {
            collect_semantic_tokens(function, context, content, tokens);
            for arg in arguments {
                collect_semantic_tokens(arg, context, content, tokens);
            }
        }

        Lang::FunctionApp {
            identifier,
            arguments,
            ..
        }
        | Lang::VecFunctionApp {
            identifier,
            arguments,
            ..
        } => {
            collect_semantic_tokens(identifier, context, content, tokens);
            for arg in arguments {
                collect_semantic_tokens(arg, context, content, tokens);
            }
        }

        Lang::RFunction { parameters, .. } => {
            for param in parameters {
                collect_semantic_tokens(param, context, content, tokens);
            }
        }

        _ => {}
    }
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
#[tracing::instrument(skip_all)]
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
/// Context needed to fill in `detail`/`documentation` for a completion item
/// on `completionItem/resolve`. Captured once per `textDocument/completion`
/// request (the `Context`/`doc_map` that produced the item list) and cached
/// per-file by `Backend` (`lsp.rs`) so `resolve_completion_item` can look a
/// single item's type/doc up cheaply instead of every item in the list
/// paying for `Type::pretty()` + a doc-comment lookup up front.
#[derive(Debug, Clone)]
pub struct CompletionResolveCtx {
    pub context: Context,
    pub doc_map: HashMap<String, String>,
}

#[tracing::instrument(skip_all)]
pub fn get_completions_at(
    content: &str,
    line: u32,
    character: u32,
    file_path: &str,
) -> (Vec<CompletionItem>, Option<CompletionResolveCtx>) {
    // `mod <name>;` completions only need the file system, not the typing
    // context, so handle them before the (possibly expensive) parse/type-check.
    let prefix = extract_multiline_prefix(content, line, character);
    let ctx = detect_completion_context(&prefix);
    if let CompletionCtx::ModuleFile = ctx {
        return (get_module_file_completions(file_path), None);
    }

    // 1. Parse + type-check the document WITHOUT the cursor line
    let (final_context, doc_map) =
        match parse_document_without_cursor_line(content, line, file_path) {
            Some((ctx, ast)) => (ctx, build_name_doc_map(&ast)),
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
                                    Ok(tc) => (tc.context, build_name_doc_map(&ast)),
                                    Err(_) => return (get_fallback_completions(), None),
                                }
                            }
                            Err(_) => return (get_fallback_completions(), None),
                        }
                    }
                    Err(_) => return (get_fallback_completions(), None),
                }
            }
        };

    // 2. Generate completions based on the already-detected context.
    let mut items = match &ctx {
        CompletionCtx::Type => get_type_completions(&final_context),
        CompletionCtx::Module(name) => get_module_completions(&final_context, name, &doc_map),
        CompletionCtx::Pipe(expr) => get_pipe_completions(&final_context, expr),
        CompletionCtx::RecordField(expr) => get_record_field_completions(&final_context, expr),
        CompletionCtx::DotAccess(expr) => get_dot_completions(&final_context, expr),
        CompletionCtx::ModuleFile => get_module_file_completions(file_path),
        CompletionCtx::Expression => get_expression_completions(&final_context),
    };

    // Tag every lazily-built item (those carrying a `data` payload from
    // `lazy_completion_item`) with the file this request was made against,
    // so `completionItem/resolve` can find the matching cached context back.
    for item in items.iter_mut() {
        if let Some(serde_json::Value::Object(obj)) = item.data.as_mut() {
            obj.insert(
                "file_path".to_string(),
                serde_json::Value::String(file_path.to_string()),
            );
        }
    }

    (
        items,
        Some(CompletionResolveCtx {
            context: final_context,
            doc_map,
        }),
    )
}

/// Fill in `detail` (`Type::pretty()`) and `documentation` (doc-comment) for
/// a single completion item previously built by `lazy_completion_item`, using
/// the `CompletionResolveCtx` captured when the completion list was first
/// generated. No-op if `item` carries no `data` (e.g. items built eagerly by
/// `get_type_completions`/`get_record_field_completions`/`var_to_completion_item`,
/// which don't need resolving).
#[tracing::instrument(skip_all)]
pub fn resolve_completion_item(ctx: &CompletionResolveCtx, item: &mut CompletionItem) {
    let Some(name) = item
        .data
        .as_ref()
        .and_then(|d| d.get("name"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
    else {
        return;
    };

    let typ = infer_expression_type(&ctx.context, &name);
    item.detail = Some(typ.pretty());
    item.documentation = doc_documentation(&ctx.doc_map, &name);
}

/// Parse the document excluding the line containing the cursor. Returns the
/// resulting `Context` alongside the (post-metaprogrammation) `ast`, so
/// callers can derive a doc-comment map from it without a second parse.
fn parse_document_without_cursor_line(
    content: &str,
    cursor_line: u32,
    file_path: &str,
) -> Option<(Context, Lang)> {
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

    Some((final_context, ast))
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

fn get_module_completions(
    context: &Context,
    module_name: &str,
    doc_map: &HashMap<String, String>,
) -> Vec<CompletionItem> {
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
        items.push(var_to_completion_item(var, typ, kind, doc_map));
    }

    for (var, typ) in module_ctx.aliases() {
        items.push(var_to_completion_item(
            var,
            typ,
            CompletionItemKind::INTERFACE,
            doc_map,
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
                items.push(lazy_completion_item(
                    &var.get_name(),
                    CompletionItemKind::FUNCTION,
                    Some(format!(" {}", var.get_name())),
                ));
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
                items.push(lazy_completion_item(
                    &var.get_name(),
                    CompletionItemKind::FUNCTION,
                    None,
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
            items.push(lazy_completion_item(
                &var.get_name(),
                CompletionItemKind::VARIABLE,
                None,
            ));
        }
    }

    for (var, _typ) in context.get_all_generic_functions() {
        items.push(lazy_completion_item(
            &var.get_name(),
            CompletionItemKind::FUNCTION,
            None,
        ));
    }

    for (var, typ) in &context.typing_context.standard_library() {
        if typ.is_function() {
            items.push(lazy_completion_item(
                &var.get_name(),
                CompletionItemKind::FUNCTION,
                None,
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

fn var_to_completion_item(
    var: &Var,
    typ: &Type,
    kind: CompletionItemKind,
    doc_map: &HashMap<String, String>,
) -> CompletionItem {
    CompletionItem {
        label: var.get_name(),
        kind: Some(kind),
        detail: Some(typ.pretty()),
        documentation: doc_documentation(doc_map, &var.get_name()),
        ..Default::default()
    }
}

/// Build a completion item without eagerly computing `detail`/`documentation`.
/// Used for the large candidate lists (whole-stdlib expression/pipe/dot
/// completions) where formatting every item's `Type` and looking up its doc
/// comment up front would be wasted work for the entries the user never
/// scrolls to. `data` carries just the declared name; `get_completions_at`
/// stamps the originating file path onto it afterwards, and
/// `resolve_completion_item` uses both to fill `detail`/`documentation` in
/// lazily, on `completionItem/resolve`.
fn lazy_completion_item(
    name: &str,
    kind: CompletionItemKind,
    insert_text: Option<String>,
) -> CompletionItem {
    CompletionItem {
        label: name.to_string(),
        insert_text,
        kind: Some(kind),
        data: Some(serde_json::json!({ "name": name })),
        ..Default::default()
    }
}

/// Look up `name`'s doc-comment in `doc_map` and wrap it as an LSP
/// `Documentation` value (plain Markdown, no code fences — the type already
/// appears in `detail`).
fn doc_documentation(doc_map: &HashMap<String, String>, name: &str) -> Option<Documentation> {
    doc_map.get(name).map(|doc| {
        Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc.clone(),
        })
    })
}

// ── code actions (quickfixes) ───────────────────────────────────────────────

/// A quick fix ready to become an LSP `CodeAction`. Kept `Uri`-agnostic the
/// same way `find_word_occurrences_at` (used by rename/references) stays
/// `Uri`-agnostic: `lsp.rs` wraps `edits` into a `WorkspaceEdit` keyed by the
/// real `Uri` of the document the request came in on.
pub struct QuickFix {
    pub title: String,
    pub edits: Vec<TextEdit>,
}

/// Whether `[start_line, end_line]` (a construct's own line span) overlaps
/// the range the client sent with `textDocument/codeAction` (usually the
/// cursor's line, occasionally a real selection).
fn range_overlaps_lines(range: Range, start_line: u32, end_line: u32) -> bool {
    range.start.line <= end_line && range.end.line >= start_line
}

/// Walk `lang`'s children first (so a nested construct wins over an
/// enclosing one that also happens to overlap `range` — e.g. an inner
/// `match` inside an outer one), then check `lang` itself via `find`.
/// Shared by the `let`-annotation and `match`-arms quick fixes below; each
/// only recognises its own leaf shape and returns `None` for everything
/// else, so reusing one traversal keeps them from duplicating this list of
/// `Lang` variants (mirrors the same tradeoff already made between
/// `collect_inlay_hints` and `collect_semantic_tokens`).
fn walk_for_quick_fix(
    lang: &Lang,
    find: &mut impl FnMut(&Lang) -> Option<QuickFix>,
) -> Option<QuickFix> {
    let from_children = match lang {
        Lang::Lines { value, .. }
        | Lang::Scope { body: value, .. }
        | Lang::Sequence { body: value, .. }
        | Lang::Test { value, .. }
        | Lang::Array { value, .. }
        | Lang::Vector { value, .. }
        | Lang::Tuple { value, .. }
        | Lang::Module { body: value, .. } => {
            value.iter().find_map(|item| walk_for_quick_fix(item, find))
        }

        Lang::Function { body, .. } | Lang::Lambda { body, .. } => walk_for_quick_fix(body, find),

        Lang::Let { expression, .. } => walk_for_quick_fix(expression, find),

        Lang::If {
            condition,
            if_block,
            else_block,
            ..
        } => walk_for_quick_fix(condition, find)
            .or_else(|| walk_for_quick_fix(if_block, find))
            .or_else(|| walk_for_quick_fix(else_block, find)),

        Lang::Match {
            target, branches, ..
        } => walk_for_quick_fix(target, find).or_else(|| {
            branches
                .iter()
                .find_map(|(_, body)| walk_for_quick_fix(body, find))
        }),

        Lang::ForLoop {
            expression, body, ..
        } => walk_for_quick_fix(expression, find).or_else(|| walk_for_quick_fix(body, find)),

        Lang::WhileLoop {
            condition, body, ..
        } => walk_for_quick_fix(condition, find).or_else(|| walk_for_quick_fix(body, find)),

        Lang::Loop { body, .. } => walk_for_quick_fix(body, find),

        Lang::Return { value, .. }
        | Lang::Not { value, .. }
        | Lang::Tag { value, .. }
        | Lang::TestBlock { value, .. }
        | Lang::KeyValue { value, .. } => walk_for_quick_fix(value, find),

        Lang::Assign { expression, .. } => walk_for_quick_fix(expression, find),

        Lang::ArrayIndexing {
            identifier,
            indexing,
            ..
        } => walk_for_quick_fix(identifier, find).or_else(|| walk_for_quick_fix(indexing, find)),

        Lang::Operator { lhs, rhs, .. } => {
            walk_for_quick_fix(lhs, find).or_else(|| walk_for_quick_fix(rhs, find))
        }

        Lang::FunctionApp {
            identifier,
            arguments,
            ..
        }
        | Lang::VecFunctionApp {
            identifier,
            arguments,
            ..
        } => walk_for_quick_fix(identifier, find)
            .or_else(|| arguments.iter().find_map(|a| walk_for_quick_fix(a, find))),

        _ => None,
    };
    from_children.or_else(|| find(lang))
}

/// Main entry-point called by the LSP codeAction handler: "add inferred type
/// annotation" for an un-annotated `let` overlapping `range`. Reuses the
/// exact detection `collect_inlay_hints` already does (`r#type` still
/// `Type::Empty` because the source omitted the annotation, looked up by
/// name in the final `Context`), just narrowed to the one `let` under the
/// cursor and turned into an edit instead of a rendered hint.
#[tracing::instrument(skip_all)]
pub fn type_annotation_action(
    analysis: &DocumentAnalysis,
    content: &str,
    range: Range,
) -> Option<QuickFix> {
    let context = &analysis.context;
    walk_for_quick_fix(&analysis.ast, &mut |lang| {
        annotation_quick_fix(lang, context, content, range)
    })
}

fn annotation_quick_fix(
    lang: &Lang,
    context: &Context,
    content: &str,
    range: Range,
) -> Option<QuickFix> {
    let Lang::Let {
        variable,
        r#type,
        expression,
        ..
    } = lang
    else {
        return None;
    };
    if !matches!(r#type, Type::Empty(_)) || matches!(expression.as_ref(), Lang::Function { .. }) {
        return None;
    }
    let var = Var::try_from(variable).ok()?;
    let name = var.get_name();
    // See `collect_inlay_hints`'s `Lang::Let` arm: a `Var`'s `HelpData` offset
    // is captured one byte past the identifier's real start.
    let real_start_offset = var.get_help_data().get_offset().saturating_sub(1);
    let end_offset = real_start_offset + name.len();
    let start_line = offset_to_position(real_start_offset, content).line;
    let end_pos = offset_to_position(end_offset, content);
    if !range_overlaps_lines(range, start_line, end_pos.line) {
        return None;
    }
    let typ = context
        .get_types_from_name(&name)
        .into_iter()
        .rev()
        .find(|t| !matches!(t, Type::Empty(_)))?;
    Some(QuickFix {
        title: format!("Add inferred type annotation: {}", typ.pretty()),
        edits: vec![TextEdit {
            range: Range::new(end_pos, end_pos),
            new_text: format!(": {}", typ.pretty()),
        }],
    })
}

/// Main entry-point called by the LSP codeAction handler: "fill missing
/// match arms" for a `match` overlapping `range` whose target resolves to a
/// tag-union (or `Option`) type and doesn't already cover every variant.
///
/// Unlike `type_annotation_action`, this doesn't only consult the final
/// `Context` — the overwhelmingly common case is `match` over a **function
/// parameter** (`fn(x: Option<int>): T { match x { ... } }`), and parameters
/// never reach the final flat `Context` (the same limitation
/// `collect_inlay_hints`'s doc comment describes for body-local `let`s). So
/// this walk threads its own `locals: name -> Type` scope map, populated
/// from each `Lang::Function`'s typed parameter list as it descends, and
/// only falls back to `Context::get_types_from_name` for names that aren't
/// a local parameter (e.g. a top-level `let` used directly as a match
/// target).
#[tracing::instrument(skip_all)]
pub fn missing_match_arms_action(
    analysis: &DocumentAnalysis,
    content: &str,
    range: Range,
) -> Option<QuickFix> {
    let locals = HashMap::new();
    find_match_for_quick_fix(
        &analysis.ast,
        &analysis.ast,
        &analysis.context,
        content,
        range,
        &locals,
    )
}

/// Children-first walk (so a nested `match` wins over an enclosing one) that
/// threads a `locals` scope map through `Lang::Function` bodies. See
/// `missing_match_arms_action`'s doc comment for why this can't simply reuse
/// `walk_for_quick_fix`.
fn find_match_for_quick_fix(
    lang: &Lang,
    ast: &Lang,
    context: &Context,
    content: &str,
    range: Range,
    locals: &HashMap<String, Type>,
) -> Option<QuickFix> {
    let from_children = match lang {
        Lang::Lines { value, .. }
        | Lang::Scope { body: value, .. }
        | Lang::Sequence { body: value, .. }
        | Lang::Test { value, .. }
        | Lang::Array { value, .. }
        | Lang::Vector { value, .. }
        | Lang::Tuple { value, .. }
        | Lang::Module { body: value, .. } => value
            .iter()
            .find_map(|item| find_match_for_quick_fix(item, ast, context, content, range, locals)),

        Lang::Function {
            parameters, body, ..
        } => {
            let mut scoped = locals.clone();
            for param in parameters {
                scoped.insert(param.get_argument_str(), param.get_type());
            }
            find_match_for_quick_fix(body, ast, context, content, range, &scoped)
        }

        Lang::Lambda { body, .. } => {
            find_match_for_quick_fix(body, ast, context, content, range, locals)
        }

        Lang::Let { expression, .. } => {
            find_match_for_quick_fix(expression, ast, context, content, range, locals)
        }

        Lang::If {
            condition,
            if_block,
            else_block,
            ..
        } => find_match_for_quick_fix(condition, ast, context, content, range, locals)
            .or_else(|| find_match_for_quick_fix(if_block, ast, context, content, range, locals))
            .or_else(|| find_match_for_quick_fix(else_block, ast, context, content, range, locals)),

        Lang::Match {
            target, branches, ..
        } => branches
            .iter()
            .find_map(|(_, body)| {
                find_match_for_quick_fix(body, ast, context, content, range, locals)
            })
            .or_else(|| find_match_for_quick_fix(target, ast, context, content, range, locals)),

        Lang::ForLoop {
            expression, body, ..
        } => find_match_for_quick_fix(expression, ast, context, content, range, locals)
            .or_else(|| find_match_for_quick_fix(body, ast, context, content, range, locals)),

        Lang::WhileLoop {
            condition, body, ..
        } => find_match_for_quick_fix(condition, ast, context, content, range, locals)
            .or_else(|| find_match_for_quick_fix(body, ast, context, content, range, locals)),

        Lang::Loop { body, .. } => {
            find_match_for_quick_fix(body, ast, context, content, range, locals)
        }

        Lang::Return { value, .. }
        | Lang::Not { value, .. }
        | Lang::Tag { value, .. }
        | Lang::TestBlock { value, .. }
        | Lang::KeyValue { value, .. } => {
            find_match_for_quick_fix(value, ast, context, content, range, locals)
        }

        Lang::Assign { expression, .. } => {
            find_match_for_quick_fix(expression, ast, context, content, range, locals)
        }

        Lang::ArrayIndexing {
            identifier,
            indexing,
            ..
        } => find_match_for_quick_fix(identifier, ast, context, content, range, locals)
            .or_else(|| find_match_for_quick_fix(indexing, ast, context, content, range, locals)),

        Lang::Operator { lhs, rhs, .. } => {
            find_match_for_quick_fix(lhs, ast, context, content, range, locals)
                .or_else(|| find_match_for_quick_fix(rhs, ast, context, content, range, locals))
        }

        Lang::FunctionApp {
            identifier,
            arguments,
            ..
        }
        | Lang::VecFunctionApp {
            identifier,
            arguments,
            ..
        } => find_match_for_quick_fix(identifier, ast, context, content, range, locals).or_else(
            || {
                arguments
                    .iter()
                    .find_map(|a| find_match_for_quick_fix(a, ast, context, content, range, locals))
            },
        ),

        _ => None,
    };

    from_children.or_else(|| match_arms_quick_fix(lang, ast, context, content, range, locals))
}

/// Checks whether `lang` is itself a `Lang::Match` needing a quick fix.
/// Takes the whole node (rather than its `target`/`branches`/`help_data`
/// fields split out) purely to stay under `clippy::too_many_arguments`.
fn match_arms_quick_fix(
    lang: &Lang,
    ast: &Lang,
    context: &Context,
    content: &str,
    range: Range,
    locals: &HashMap<String, Type>,
) -> Option<QuickFix> {
    let Lang::Match {
        target,
        branches,
        help_data,
    } = lang
    else {
        return None;
    };

    let match_offset = help_data.get_offset();
    let closing_offset = find_match_closing_brace_offset(content, match_offset)?;
    let start_line = offset_to_position(match_offset, content).line;
    let closing_pos = offset_to_position(closing_offset, content);
    if !range_overlaps_lines(range, start_line, closing_pos.line) {
        return None;
    }

    // A bare-variable pattern (`_ => ...` or `other => ...`) already makes the
    // match exhaustive by convention — nothing to fill in.
    if branches
        .iter()
        .any(|(pat, _)| matches!(pat, Lang::Variable { .. }))
    {
        return None;
    }

    let Lang::Variable {
        name: target_name, ..
    } = target.as_ref()
    else {
        return None;
    };
    let target_type = locals.get(target_name).cloned().or_else(|| {
        context
            .get_types_from_name(target_name)
            .into_iter()
            .rev()
            .find(|t| !matches!(t, Type::Empty(_)))
    })?;
    let variants = resolve_union_variants(&target_type, ast)?;

    let existing: std::collections::HashSet<&str> = branches
        .iter()
        .filter_map(|(pat, _)| match pat {
            Lang::Tag { name, .. } => Some(name.as_str()),
            _ => None,
        })
        .collect();
    let missing: Vec<&(String, bool)> = variants
        .iter()
        .filter(|(name, _)| !existing.contains(name.as_str()))
        .collect();
    if missing.is_empty() {
        return None;
    }

    // Indentation is taken from the last existing branch's body — whatever
    // whitespace/tabs the user is already using for arms in this match.
    let arm_indent = branches
        .last()
        .map(|(_, body)| {
            line_leading_whitespace(
                content,
                offset_to_position(body.get_help_data().get_offset(), content).line,
            )
        })
        .unwrap_or_default();
    let closing_indent = line_leading_whitespace(content, closing_pos.line);

    let mut new_text = String::new();
    let mut names = Vec::new();
    for (name, has_payload) in &missing {
        names.push(name.as_str());
        if *has_payload {
            new_text.push_str(&format!(
                "{arm_indent}.{name}(v) => stop(\"todo: {name}\"),\n"
            ));
        } else {
            new_text.push_str(&format!("{arm_indent}.{name} => stop(\"todo: {name}\"),\n"));
        }
    }
    new_text.push_str(&closing_indent);
    new_text.push('}');

    // Replace just the closing `}` itself: whatever preceded it (a whole
    // line of its own, or trailing content on the last arm's line) is left
    // untouched, and the new arms + a freshly indented `}` take its place.
    let brace_end = Position::new(closing_pos.line, closing_pos.character + 1);
    Some(QuickFix {
        title: format!("Fill missing match arm(s): {}", names.join(", ")),
        edits: vec![TextEdit {
            range: Range::new(closing_pos, brace_end),
            new_text,
        }],
    })
}

/// Scan forward from `match_offset` (the `match` keyword's own offset) for
/// the `{` that opens the branch list, then depth-track to its matching `}`.
/// Byte-indexed comparisons against ASCII `{`/`}` are safe even though
/// `content` may contain multi-byte UTF-8 elsewhere: a continuation byte
/// never equals an ASCII byte value.
fn find_match_closing_brace_offset(content: &str, match_offset: usize) -> Option<usize> {
    let bytes = content.as_bytes();
    let mut i = match_offset;
    while i < bytes.len() && bytes[i] != b'{' {
        i += 1;
    }
    if i >= bytes.len() {
        return None;
    }
    let mut depth = 1;
    i += 1;
    while i < bytes.len() {
        match bytes[i] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
        i += 1;
    }
    None
}

/// The leading whitespace (spaces or tabs) of `content`'s `line`-th line.
fn line_leading_whitespace(content: &str, line: u32) -> String {
    content
        .lines()
        .nth(line as usize)
        .map(|l| l.chars().take_while(|c| *c == ' ' || *c == '\t').collect())
        .unwrap_or_default()
}

/// Resolve `typ` to the full `(tag_name, has_payload)` list of a tag-union
/// it belongs to, or `None` if `typ` isn't tag-union shaped. `Option<T>` is
/// hardcoded (`Some`/`None`) the same way the `Lang::Match` typing arm in
/// `typr-core` special-cases it — the stdlib declares it in a `.ty` file, not
/// user AST, so there is no `Lang::Alias` to walk for it.
fn resolve_union_variants(typ: &Type, ast: &Lang) -> Option<Vec<(String, bool)>> {
    match typ {
        Type::Alias(name, _, _, _) if name == "Option" => Some(vec![
            ("Some".to_string(), true),
            ("None".to_string(), false),
        ]),
        Type::Alias(name, _, _, _) => {
            let target = find_alias_target_type(ast, name)?;
            let mut out = Vec::new();
            collect_tag_variants(&target, &mut out);
            (!out.is_empty()).then_some(out)
        }
        Type::Operator(TypeOperator::Union, _, _, _) | Type::Tag(_, _, _) => {
            let mut out = Vec::new();
            collect_tag_variants(typ, &mut out);
            (!out.is_empty()).then_some(out)
        }
        _ => None,
    }
}

fn collect_tag_variants(typ: &Type, out: &mut Vec<(String, bool)>) {
    match typ {
        Type::Tag(name, inner, _) => {
            let has_payload = !matches!(inner.as_ref(), Type::Empty(_));
            out.push((name.clone(), has_payload));
        }
        Type::Operator(TypeOperator::Union, left, right, _) => {
            collect_tag_variants(left, out);
            collect_tag_variants(right, out);
        }
        _ => {}
    }
}

/// Search the AST for a `type X <- .Tag(..) | ...;` declaration named `name`
/// and return its target type (the union tree). Deliberately walks the AST
/// rather than `Context::get_aliases()` for the same reason
/// `find_tag_definition_in_ast` does: a standalone tag literal registers a
/// synthetic alias that would shadow the real declaration.
fn find_alias_target_type(ast: &Lang, name: &str) -> Option<Type> {
    match ast {
        Lang::Alias {
            identifier,
            target_type,
            ..
        } => {
            let var = Var::try_from(identifier).ok()?;
            (var.get_name() == name).then(|| target_type.clone())
        }
        Lang::Lines { value, .. }
        | Lang::Scope { body: value, .. }
        | Lang::Module { body: value, .. } => {
            value.iter().find_map(|m| find_alias_target_type(m, name))
        }
        _ => None,
    }
}

/// Main entry-point called by the LSP codeAction handler: "import missing
/// module member". For each diagnostic whose message is `Undefined
/// variable: X` / `Undefined function: X`, scans every currently open
/// document's AST (parsed fresh, the same way `get_workspace_symbols` in
/// `lsp.rs` does — there is no persistent cross-file index) for a `module M
/// { @pub let/type X ... }` declaration, and offers to insert `use M::X;`
/// into `current_path`'s own document.
#[tracing::instrument(skip_all)]
pub fn import_missing_member_actions(
    current_path: &str,
    diagnostics: &[Diagnostic],
    all_docs: &[(String, String)],
) -> Vec<QuickFix> {
    let Some((_, current_content)) = all_docs.iter().find(|(path, _)| path == current_path) else {
        return Vec::new();
    };

    let mut seen_names = std::collections::HashSet::new();
    let mut fixes = Vec::new();

    for diagnostic in diagnostics {
        let Some(name) = undefined_name_from_message(&diagnostic.message) else {
            continue;
        };
        if !seen_names.insert(name) {
            continue;
        }

        for (path, content) in all_docs {
            let span: Span = LocatedSpan::new_extra(content.as_str(), path.clone());
            let Ok(parsed) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)))
            else {
                continue;
            };

            for module_name in find_exporting_modules(&parsed.ast, name) {
                // Skip if this exact `Module::name` path is already
                // referenced somewhere in the file (already imported).
                if current_content.contains(&format!("{module_name}::{name}")) {
                    continue;
                }
                fixes.push(QuickFix {
                    title: format!("Import `{name}` from module `{module_name}`"),
                    edits: vec![insert_use_edit(current_content, &module_name, name)],
                });
            }
        }
    }

    fixes
}

fn undefined_name_from_message(message: &str) -> Option<&str> {
    message
        .strip_prefix("Undefined variable: ")
        .or_else(|| message.strip_prefix("Undefined function: "))
}

fn find_exporting_modules(ast: &Lang, member_name: &str) -> Vec<String> {
    let mut out = Vec::new();
    collect_exporting_modules(ast, member_name, &mut out);
    out
}

fn collect_exporting_modules(ast: &Lang, member_name: &str, out: &mut Vec<String>) {
    match ast {
        Lang::Module { name, body, .. } => {
            if find_field_help_data(body, member_name).is_some() {
                out.push(name.clone());
            }
            for member in body {
                collect_exporting_modules(member, member_name, out);
            }
        }
        Lang::Lines { value, .. } | Lang::Scope { body: value, .. } => {
            for item in value {
                collect_exporting_modules(item, member_name, out);
            }
        }
        _ => {}
    }
}

/// Insert `use Module::name;` right after the document's existing leading
/// block of `use`/blank lines (or at the very top if there is none).
fn insert_use_edit(content: &str, module_name: &str, name: &str) -> TextEdit {
    let mut insert_line = 0u32;
    for (i, line) in content.lines().enumerate() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("use ") || trimmed.is_empty() {
            insert_line = (i + 1) as u32;
        } else {
            break;
        }
    }
    let pos = Position::new(insert_line, 0);
    TextEdit {
        range: Range::new(pos, pos),
        new_text: format!("use {module_name}::{name};\n"),
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
        let (items, _) = get_completions_at(
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
        let (items, _) = get_completions_at(
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

        let (items, resolve_ctx) = get_completions_at(
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
        let mut resolved = add_two.unwrap().clone();
        resolve_completion_item(
            resolve_ctx
                .as_ref()
                .expect("completions should have a resolve context"),
            &mut resolved,
        );
        let detail = resolved.detail.clone().unwrap_or_default();
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

#[cfg(test)]
mod inlay_hint_tests {
    use super::*;

    fn hints_for(content: &str) -> Vec<InlayHint> {
        let analysis =
            analyze_document(content, "test.ty").expect("analysis should succeed for valid code");
        resolve_inlay_hints(&analysis, content)
    }

    fn label_text(hint: &InlayHint) -> String {
        match &hint.label {
            InlayHintLabel::String(s) => s.clone(),
            InlayHintLabel::LabelParts(parts) => {
                parts.iter().map(|p| p.value.clone()).collect::<String>()
            }
        }
    }

    /// An un-annotated `let` must get a `TYPE` hint right after the variable
    /// name, showing the type the checker actually inferred.
    #[test]
    fn unannotated_let_gets_type_hint() {
        // Two statements, not one: a lone top-level statement hits a
        // pre-existing typr-core quirk where `Lang::Lines` with exactly one
        // entry doesn't thread that entry's context updates through (see
        // `typing()`'s `exprs.len() == 1` branch) — unrelated to this test.
        let content = "let x <- 5;\nlet y <- x;\n";
        let hints = hints_for(content);

        let name_col = content.find('x').unwrap() as u32;
        let expected_position = Position::new(0, name_col + 1);
        let hint = hints
            .iter()
            .find(|h| h.kind == Some(InlayHintKind::TYPE) && h.position == expected_position)
            .expect("expected a type hint for `x`");
        assert_eq!(label_text(hint), ": int");
    }

    /// A `let` with an explicit `: Type` annotation must not get a
    /// (redundant) type hint.
    #[test]
    fn annotated_let_gets_no_type_hint() {
        let content = "let x: int <- 5;\n";
        let hints = hints_for(content);
        assert!(
            hints.iter().all(|h| h.kind != Some(InlayHintKind::TYPE)),
            "unexpected type hint(s): {:?}",
            hints
        );
    }

    /// `let f <- fn(x: int): int { x };` already spells out its signature in
    /// full — a type hint on `f` would just repeat it, so it must be
    /// suppressed.
    #[test]
    fn function_literal_let_gets_no_type_hint() {
        let content = "let f <- fn(x: int): int { x };\n";
        let hints = hints_for(content);
        assert!(
            hints.iter().all(|h| h.kind != Some(InlayHintKind::TYPE)),
            "unexpected type hint(s): {:?}",
            hints
        );
    }

    /// A call site must get a `PARAMETER` hint labeling each positional
    /// argument with the callee's declared parameter name.
    #[test]
    fn call_site_gets_parameter_name_hints() {
        let content = "let add <- fn(a: int, b: int): int { a + b };\nlet r <- add(1, 2);\n";
        let hints = hints_for(content);

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();
        assert_eq!(
            param_hints.len(),
            2,
            "expected 2 parameter hints, got {:?}",
            hints
        );
        assert_eq!(label_text(param_hints[0]), "a:");
        assert_eq!(label_text(param_hints[1]), "b:");

        let call_line = content.lines().nth(1).unwrap();
        let arg1_col = call_line.find('1').unwrap() as u32;
        let arg2_col = call_line.find('2').unwrap() as u32;
        assert_eq!(param_hints[0].position, Position::new(1, arg1_col));
        assert_eq!(param_hints[1].position, Position::new(1, arg2_col));
    }

    /// When the argument is already a bare variable sharing the parameter's
    /// name, the hint would be pure noise — it must be suppressed.
    #[test]
    fn parameter_hint_suppressed_when_argument_name_matches() {
        let content =
            "let add <- fn(a: int, b: int): int { a + b };\nlet a <- 1;\nlet r <- add(a, 2);\n";
        let hints = hints_for(content);

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();
        assert_eq!(
            param_hints.len(),
            1,
            "expected only the `b` hint, got {:?}",
            hints
        );
        assert_eq!(label_text(param_hints[0]), "b:");
    }

    /// A variadic parameter (`...xs: T`) must not get labeled — repeating
    /// the same name on every trailing positional argument is noise, not
    /// signal.
    #[test]
    fn variadic_parameter_gets_no_hint() {
        let content = "let sum <- fn(...xs: int): int { 0 };\nlet r <- sum(1, 2, 3);\n";
        let hints = hints_for(content);
        assert!(
            hints
                .iter()
                .all(|h| h.kind != Some(InlayHintKind::PARAMETER)),
            "unexpected parameter hint(s): {:?}",
            hints
        );
    }

    /// The walk must still recurse into a function body (so a call *inside*
    /// it, e.g. to a top-level function, can still get parameter hints) even
    /// though a `let` local to that body gets no type hint — its binding
    /// lives only in a sub-context that's discarded once the function is
    /// typed (see the "Known scope limit" note on `resolve_inlay_hints`).
    #[test]
    fn call_inside_function_body_still_gets_parameter_hints_but_local_let_does_not() {
        let content = "let add <- fn(a: int, b: int): int { a + b };\n\
                        let f <- fn(n: int): int {\n    \
                        let doubled <- add(n, n);\n    \
                        doubled\n\
                        };\n";
        let hints = hints_for(content);

        assert!(
            hints.iter().all(|h| h.kind != Some(InlayHintKind::TYPE)),
            "local `doubled` should get no type hint yet (known scope limit): {:?}",
            hints
        );

        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();
        assert_eq!(
            param_hints.len(),
            2,
            "expected parameter hints for the `add(n, n)` call inside `f`'s body: {:?}",
            hints
        );
        assert_eq!(label_text(param_hints[0]), "a:");
        assert_eq!(label_text(param_hints[1]), "b:");
    }
}

#[cfg(test)]
mod doc_comment_tests {
    use super::*;

    /// A doc-comment block directly preceding a `let` must surface in hover,
    /// appended after the type.
    #[test]
    fn hover_includes_preceding_doc_comment() {
        let content = "# Adds two integers together.\n\
                        # Returns their sum.\n\
                        let add <- fn(a: int, b: int): int { a + b };\n\
                        let result <- add(1, 2);\n";
        let analysis =
            analyze_document(content, "test.ty").expect("analysis should succeed for valid code");

        let name_col = content.rfind("add(1, 2)").unwrap() as u32;
        let line = content[..name_col as usize].matches('\n').count() as u32;
        let col_on_line = content[..name_col as usize]
            .rfind('\n')
            .map_or(name_col, |nl| name_col - nl as u32 - 1);

        let info = resolve_hover(&analysis, content, line, col_on_line + 1)
            .expect("hover should resolve `add`");
        assert!(
            info.type_display.contains("Adds two integers together."),
            "expected the doc-comment in hover output, got: {}",
            info.type_display
        );
        assert!(info.type_display.contains("Returns their sum."));
    }

    /// A doc-comment on a top-level `let` must surface as the `documentation`
    /// field of its completion item.
    #[test]
    fn completion_includes_preceding_doc_comment() {
        let content = "# Adds two integers together.\n\
                        let add <- fn(a: int, b: int): int { a + b };\n\
                        let result <- ad";
        let last_line = content.lines().count() as u32 - 1;
        let last_col = content.lines().last().unwrap().len() as u32;

        let (items, resolve_ctx) = get_completions_at(content, last_line, last_col, "test.ty");
        let add_item = items
            .iter()
            .find(|i| i.label == "add")
            .expect("expected a completion item for `add`");

        let mut resolved = add_item.clone();
        resolve_completion_item(
            resolve_ctx
                .as_ref()
                .expect("completions should have a resolve context"),
            &mut resolved,
        );

        match &resolved.documentation {
            Some(Documentation::MarkupContent(markup)) => {
                assert!(markup.value.contains("Adds two integers together."));
            }
            other => panic!("expected doc-comment documentation, got: {:?}", other),
        }
    }
}

#[cfg(test)]
mod completion_resolve_tests {
    use super::*;

    /// Expression completions for user/stdlib functions must NOT eagerly carry
    /// `detail`/`documentation` — that's the whole point of deferring them to
    /// `completionItem/resolve` (Tier 1 item #3) instead of formatting every
    /// candidate's `Type` up front. `resolve_completion_item` should fill the
    /// real signature in only once asked.
    #[test]
    fn expression_completion_is_lazy_until_resolved() {
        // Named `bump` (not `add`) to avoid colliding with the stdlib's own
        // overloaded `add` (int,int)->int / (num,num)->num, which would make
        // `detail` ambiguous between overloads.
        let content = "let bump <- fn(a: int, b: int): int { a + b };\nlet x <- bu";
        let last_line = content.lines().count() as u32 - 1;
        let last_col = content.lines().last().unwrap().len() as u32;

        let (items, resolve_ctx) = get_completions_at(content, last_line, last_col, "test.ty");
        let bump_item = items
            .iter()
            .find(|i| i.label == "bump")
            .expect("expected a completion item for `bump`");

        assert!(
            bump_item.detail.is_none(),
            "expected completion() to defer `detail` to resolve, got: {:?}",
            bump_item.detail
        );

        let mut resolved = bump_item.clone();
        resolve_completion_item(
            resolve_ctx
                .as_ref()
                .expect("completions should have a resolve context"),
            &mut resolved,
        );

        let detail = resolved
            .detail
            .expect("completionItem/resolve should fill in `detail`");
        assert!(
            detail.contains("int"),
            "expected the real function signature, got: {}",
            detail
        );
    }

    /// `data` must carry the originating file path (stamped on by
    /// `get_completions_at`) so `Backend::completion_resolve` in `lsp.rs` can
    /// find the right cached `CompletionResolveCtx` back.
    #[test]
    fn lazy_item_data_carries_file_path() {
        let content = "let bump <- fn(a: int, b: int): int { a + b };\nlet x <- bu";
        let last_line = content.lines().count() as u32 - 1;
        let last_col = content.lines().last().unwrap().len() as u32;

        let (items, _) = get_completions_at(content, last_line, last_col, "test.ty");
        let bump_item = items
            .iter()
            .find(|i| i.label == "bump")
            .expect("expected a completion item for `bump`");

        let file_path = bump_item
            .data
            .as_ref()
            .and_then(|d| d.get("file_path"))
            .and_then(|v| v.as_str());
        assert_eq!(file_path, Some("test.ty"));
    }
}

#[cfg(test)]
mod semantic_tokens_tests {
    use super::*;

    /// A decoded token, reconstructed from the delta-encoded wire format for
    /// easier assertions: absolute `(line, character)`, `length`, and the
    /// resolved `SemanticTokenType`/`SemanticTokenModifier` names (looked up
    /// through the same legend `lsp.rs` advertises).
    #[derive(Debug)]
    struct Decoded {
        line: u32,
        character: u32,
        length: u32,
        token_type: SemanticTokenType,
        has_declaration: bool,
    }

    fn resolve_tokens(content: &str) -> Vec<Decoded> {
        let analysis = analyze_document(content, "test.ty").expect("expected valid document");
        let raw = resolve_semantic_tokens(&analysis, content);

        let mut line = 0u32;
        let mut character = 0u32;
        raw.into_iter()
            .map(|t| {
                line += t.delta_line;
                if t.delta_line != 0 {
                    character = 0;
                }
                character += t.delta_start;
                Decoded {
                    line,
                    character,
                    length: t.length,
                    token_type: SEMANTIC_TOKEN_TYPES[t.token_type as usize].clone(),
                    has_declaration: t.token_modifiers_bitset & TOKEN_MODIFIER_DECLARATION != 0,
                }
            })
            .collect()
    }

    fn find<'a>(tokens: &'a [Decoded], line: u32, character: u32) -> Option<&'a Decoded> {
        tokens
            .iter()
            .find(|t| t.line == line && t.character == character)
    }

    /// A `let`-bound function name gets a `FUNCTION` token at its
    /// declaration, and every call-site reference resolves the same way.
    #[test]
    fn function_declaration_and_call_site() {
        let content = "let add <- fn(a: int, b: int): int { a + b };\nlet x <- add(1, 2);\n";
        let tokens = resolve_tokens(content);

        let decl = find(&tokens, 0, 4).expect("expected a token at `add`'s declaration");
        assert_eq!(decl.token_type, SemanticTokenType::FUNCTION);
        assert_eq!(decl.length, 3);
        assert!(decl.has_declaration);

        let call_site = find(&tokens, 1, 9).expect("expected a token at the `add(` call site");
        assert_eq!(call_site.token_type, SemanticTokenType::FUNCTION);
        assert!(!call_site.has_declaration);
    }

    /// A plain (non-function) `let` binding gets a `VARIABLE` token, not
    /// `FUNCTION`.
    #[test]
    fn plain_variable_is_not_function() {
        // Two statements, not one: a single top-level statement hits a
        // separate, already-documented context bug where the real binding
        // is dropped in favor of a placeholder `UnknownFunction` (see
        // `bug_lines_single_statement_context_drop`) — unrelated to
        // semantic tokens, just an edge case this test must avoid to
        // exercise the real classification logic.
        let content = "let pi <- 3.14;\nlet two_pi <- pi * 2.0;\n";
        let tokens = resolve_tokens(content);
        let decl = find(&tokens, 0, 4).expect("expected a token at `pi`'s declaration");
        assert_eq!(decl.token_type, SemanticTokenType::VARIABLE);
        assert!(decl.has_declaration);
    }

    /// `type Point <- ...` gets a `TYPE` token at the alias name, and a
    /// later `let` annotated with that alias gets a matching `TYPE` token at
    /// the annotation site.
    #[test]
    fn type_alias_declaration_and_annotation_usage() {
        let content =
            "type Point <- list { x: int, y: int };\nlet p: Point <- Point:{ x = 1, y = 2 };\n";
        let tokens = resolve_tokens(content);

        let decl = find(&tokens, 0, 5).expect("expected a token at `Point`'s declaration");
        assert_eq!(decl.token_type, SemanticTokenType::TYPE);
        assert_eq!(decl.length, 5);
        assert!(decl.has_declaration);

        let annotation =
            find(&tokens, 1, 7).expect("expected a TYPE token at the `: Point` annotation");
        assert_eq!(annotation.token_type, SemanticTokenType::TYPE);
        assert_eq!(annotation.length, 5);
    }

    /// A union tag constructor (`.Circle(3.14)`) gets an `ENUM_MEMBER` token
    /// spanning the leading dot and the tag name.
    #[test]
    fn tag_usage_is_enum_member() {
        let content =
            "type Shape <- .Circle(num) | .Square(num);\nlet s: Shape <- .Circle(3.14);\n";
        let tokens = resolve_tokens(content);

        let tag = find(&tokens, 1, 16).expect("expected a token at `.Circle`");
        assert_eq!(tag.token_type, SemanticTokenType::ENUM_MEMBER);
        // `.Circle` — the dot plus the 6-letter name.
        assert_eq!(tag.length, 7);
    }

    /// `module M { ... }` gets a `NAMESPACE` token at the module's name (not
    /// at the `module` keyword itself).
    #[test]
    fn module_declaration_is_namespace() {
        let content = "module Math {\n  let pi <- 3.14159;\n};\n";
        let tokens = resolve_tokens(content);

        let decl = find(&tokens, 0, 7).expect("expected a token at `Math`");
        assert_eq!(decl.token_type, SemanticTokenType::NAMESPACE);
        assert_eq!(decl.length, 4);
        assert!(decl.has_declaration);
    }
}

#[cfg(test)]
mod code_action_tests {
    use super::*;

    /// A range wide enough to overlap any construct in `content`, so tests
    /// don't need to compute an exact cursor position.
    fn full_range(content: &str) -> Range {
        let last_line = content.lines().count().max(1) as u32 - 1;
        Range::new(Position::new(0, 0), Position::new(last_line, 0))
    }

    /// An un-annotated `let` overlapping the requested range gets a quick fix
    /// that inserts `: int` right after the variable name.
    #[test]
    fn type_annotation_quick_fix_inserts_inferred_type() {
        let content = "let x <- 5;\nlet y <- x;\n";
        let analysis = analyze_document(content, "test.ty").expect("valid code");
        let fix = type_annotation_action(&analysis, content, full_range(content))
            .expect("expected a quick fix for the un-annotated `x`");
        assert_eq!(fix.edits.len(), 1);
        assert_eq!(fix.edits[0].new_text, ": int");
    }

    /// A `let` that already has an annotation must not produce a quick fix.
    #[test]
    fn annotated_let_gets_no_quick_fix() {
        let content = "let x: int <- 5;\n";
        let analysis = analyze_document(content, "test.ty").expect("valid code");
        assert!(type_annotation_action(&analysis, content, full_range(content)).is_none());
    }

    /// A `match` over a user-declared tag union missing one variant gets a
    /// quick fix that appends a stub arm for it.
    #[test]
    fn missing_match_arm_quick_fix_for_user_union() {
        let content = "type Shape <- .Circle(num) | .Square(num);\n\
             let area <- fn(s: Shape): num {\n\
             \tmatch s {\n\
             \t\t.Circle(r) => r\n\
             \t}\n\
             };\n";
        let analysis = analyze_document(content, "test.ty").expect("valid code");
        let fix = missing_match_arms_action(&analysis, content, full_range(content))
            .expect("expected a quick fix for the missing `.Square` arm");
        assert!(
            fix.edits[0].new_text.contains(".Square"),
            "expected a `.Square` stub, got: {}",
            fix.edits[0].new_text
        );
    }

    /// `Option<T>`'s variants are hardcoded (no user `Lang::Alias` to walk
    /// for it — it's declared in a stdlib `.ty` file), so a `match` missing
    /// `.None` must still get a quick fix.
    #[test]
    fn missing_match_arm_quick_fix_for_option() {
        let content = "let unwrap_or_zero <- fn(o: Option<int>): int {\n\
             \tmatch o {\n\
             \t\t.Some(v) => v\n\
             \t}\n\
             };\n";
        let analysis = analyze_document(content, "test.ty").expect("valid code");
        let fix = missing_match_arms_action(&analysis, content, full_range(content))
            .expect("expected a quick fix for the missing `.None` arm");
        assert!(
            fix.edits[0].new_text.contains(".None"),
            "expected a `.None` stub, got: {}",
            fix.edits[0].new_text
        );
    }

    /// A `match` that already covers every variant must not produce a quick
    /// fix.
    #[test]
    fn exhaustive_match_gets_no_quick_fix() {
        let content = "type Shape <- .Circle(num) | .Square(num);\n\
             let area <- fn(s: Shape): num {\n\
             \tmatch s {\n\
             \t\t.Circle(r) => r,\n\
             \t\t.Square(side) => side\n\
             \t}\n\
             };\n";
        let analysis = analyze_document(content, "test.ty").expect("valid code");
        assert!(missing_match_arms_action(&analysis, content, full_range(content)).is_none());
    }

    /// A `match` with a catch-all binding is already exhaustive by
    /// convention, even if it doesn't literally name every variant.
    #[test]
    fn match_with_catch_all_gets_no_quick_fix() {
        let content = "type Shape <- .Circle(num) | .Square(num);\n\
             let area <- fn(s: Shape): num {\n\
             \tmatch s {\n\
             \t\t.Circle(r) => r,\n\
             \t\tother => 0.0\n\
             \t}\n\
             };\n";
        let analysis = analyze_document(content, "test.ty").expect("valid code");
        assert!(missing_match_arms_action(&analysis, content, full_range(content)).is_none());
    }

    /// A public member declared in one open document is offered as an
    /// importable `use` statement for another document referencing it.
    #[test]
    fn import_quick_fix_finds_public_module_member() {
        let other = "module Greeter {\n\t@pub let greet <- fn(): char { \"hi\" };\n};\n";
        let current = "let x <- greet;\n";
        let diagnostics = vec![Diagnostic {
            message: "Undefined variable: greet".to_string(),
            ..Default::default()
        }];
        let docs = vec![
            ("other.ty".to_string(), other.to_string()),
            ("current.ty".to_string(), current.to_string()),
        ];
        let fixes = import_missing_member_actions("current.ty", &diagnostics, &docs);
        assert_eq!(fixes.len(), 1);
        assert_eq!(fixes[0].edits[0].new_text, "use Greeter::greet;\n");
    }

    /// A member that is declared but not `@pub` must not be offered as an
    /// import target.
    #[test]
    fn import_quick_fix_ignores_private_member() {
        let other = "module Greeter {\n\tlet greet <- fn(): char { \"hi\" };\n};\n";
        let current = "let x <- greet;\n";
        let diagnostics = vec![Diagnostic {
            message: "Undefined variable: greet".to_string(),
            ..Default::default()
        }];
        let docs = vec![
            ("other.ty".to_string(), other.to_string()),
            ("current.ty".to_string(), current.to_string()),
        ];
        let fixes = import_missing_member_actions("current.ty", &diagnostics, &docs);
        assert!(fixes.is_empty());
    }
}
