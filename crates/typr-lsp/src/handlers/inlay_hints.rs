//! Inlay hints: inferred types on un-annotated `let`s and parameter names at call sites.

use super::document::DocumentAnalysis;
use super::utils::offset_to_position;
use tower_lsp_server::ls_types::{InlayHint, InlayHintKind, InlayHintLabel};
use typr_core::components::context::Context;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::tchar::Tchar;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;

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

#[cfg(test)]
mod inlay_hint_tests {
    use super::*;
    use crate::handlers::document::analyze_document;
    use tower_lsp_server::ls_types::Position;

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
