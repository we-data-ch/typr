//! Completion provider: context-aware candidates plus lazy completionItem/resolve.

use super::document::build_name_doc_map;
use super::goto_definition::detect_environment;
use super::utils::{infer_literal_type, Span};
use crate::metaprogramming::metaprogrammation;
use nom_locate::LocatedSpan;
use std::collections::HashMap;
use tower_lsp_server::ls_types::{CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind};
use typr_core::components::context::config::Environment;
use typr_core::components::context::Context;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::vector_type::VecType;
use typr_core::components::r#type::Type;
use typr_core::processes::parsing::parse;
use typr_core::typing;
use typr_core::utils::builder;

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
    let (final_context, doc_map) = match parse_document_without_cursor_line(content, line, file_path) {
        Some((ctx, ast)) => (ctx, build_name_doc_map(&ast)),
        None => {
            let environment = detect_environment(file_path);
            let span: Span = LocatedSpan::new_extra(content, file_path.to_string());
            let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
            let context = Context::default().set_environment(environment);
            match parse_result {
                Ok(result) => {
                    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        metaprogrammation(result.ast, environment)
                    }));
                    match ast {
                        Ok(ast) => {
                            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast))) {
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
fn parse_document_without_cursor_line(content: &str, cursor_line: u32, file_path: &str) -> Option<(Context, Lang)> {
    let lines: Vec<&str> = content.lines().collect();

    let mut filtered_lines = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        if idx != cursor_line as usize {
            filtered_lines.push(*line);
        }
    }

    let filtered_content = filtered_lines.join("\n");
    let span: Span = LocatedSpan::new_extra(&filtered_content, file_path.to_string());

    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span))).ok()?;
    let environment = detect_environment(file_path);
    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        metaprogrammation(parse_result.ast, environment)
    }))
    .ok()?;

    let context = Context::default().set_environment(environment);

    let final_context = if let Lang::Lines { value: exprs, .. } = &ast {
        let mut ctx = context.clone();
        for expr in exprs {
            if let Ok(tc) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&ctx, expr))) {
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

pub fn extract_multiline_prefix(content: &str, line: u32, character: u32) -> String {
    let lines: Vec<&str> = content.lines().collect();
    let current_line_idx = line as usize;

    if current_line_idx >= lines.len() {
        return String::new();
    }

    let current_line_part = lines[current_line_idx].get(..character as usize).unwrap_or("");

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

    let parts: Vec<&str> = trimmed.split([';', '\n']).filter(|p| !p.trim().is_empty()).collect();

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
        items.push(var_to_completion_item(var, typ, CompletionItemKind::INTERFACE, doc_map));
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
        Environment::Project => crate::metaprogramming::find_project_root(file_path).map(|root| root.join("TypR")),
        _ => std::path::Path::new(file_path).parent().map(|p| p.to_path_buf()),
    };

    let Some(dir) = scan_dir else {
        return Vec::new();
    };

    let Ok(entries) = std::fs::read_dir(&dir) else {
        return Vec::new();
    };

    let current_stem = std::path::Path::new(file_path).file_stem().and_then(|s| s.to_str());

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
                while method_end < len && (chars[method_end].is_alphanumeric() || chars[method_end] == '_') {
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
fn lazy_completion_item(name: &str, kind: CompletionItemKind, insert_text: Option<String>) -> CompletionItem {
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
        let (items, _) = get_completions_at(content, 0, content.len() as u32, main_file.to_str().unwrap());

        let _ = std::fs::remove_dir_all(&dir);

        let labels: Vec<_> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"person"), "labels: {:?}", labels);
        assert!(labels.contains(&"animal"), "labels: {:?}", labels);
        assert!(!labels.contains(&"main"), "should not suggest itself: {:?}", labels);
    }

    /// In Project mode (DESCRIPTION + NAMESPACE present), `mod ` should scan
    /// `TypR/` under the detected project root, not the file's own directory.
    #[test]
    fn suggests_files_from_typr_dir_in_project_mode() {
        let dir = std::env::temp_dir().join(format!("typr_lsp_modcomp_proj_{}", std::process::id()));
        let typr_dir = dir.join("TypR");
        let _ = std::fs::create_dir_all(&typr_dir);
        std::fs::write(dir.join("DESCRIPTION"), "Package: x\n").unwrap();
        std::fs::write(dir.join("NAMESPACE"), "").unwrap();
        std::fs::write(typr_dir.join("person.ty"), "").unwrap();
        let main_file = typr_dir.join("main.ty");
        std::fs::write(&main_file, "").unwrap();

        let content = "mod pe";
        let (items, _) = get_completions_at(content, 0, content.len() as u32, main_file.to_str().unwrap());

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
        let dir = std::env::temp_dir().join(format!("typr_lsp_completion_proj_{}", std::process::id()));
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
            resolve_ctx.as_ref().expect("completions should have a resolve context"),
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
            resolve_ctx.as_ref().expect("completions should have a resolve context"),
            &mut resolved,
        );

        let detail = resolved.detail.expect("completionItem/resolve should fill in `detail`");
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
