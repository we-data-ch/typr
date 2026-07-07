//! Code actions (quickfixes): type annotations, missing match arms, missing imports.

use super::document::DocumentAnalysis;
use super::goto_definition::find_field_help_data;
use super::utils::{offset_to_position, Span};
use nom_locate::LocatedSpan;
use std::collections::HashMap;
use tower_lsp_server::ls_types::{Diagnostic, Position, Range, TextEdit};
use typr_core::components::context::Context;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::type_operator::TypeOperator;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;
use typr_core::processes::parsing::parse;

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
mod code_action_tests {
    use super::*;
    use crate::handlers::document::analyze_document;

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
