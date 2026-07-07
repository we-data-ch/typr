//! Semantic tokens: full-document syntax classification from the typed AST.

use super::document::{doc_target_name, DocumentAnalysis};
use super::utils::offset_to_position;
use tower_lsp_server::ls_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType};
use typr_core::components::context::Context;
use typr_core::components::error_message::help_data::HelpData;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::Type;

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

#[cfg(test)]
mod semantic_tokens_tests {
    use super::*;
    use crate::handlers::document::analyze_document;

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
