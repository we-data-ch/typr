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
    CompletionItem, CompletionItemKind,
};
use typr_core::components::context::config::Environment;
use typr_core::components::context::Context;
use typr_core::components::language::Lang;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::vector_type::VecType;
use typr_core::components::r#type::Type;
use typr_core::processes::parsing::parse;
use typr_core::typing;
use typr_core::utils::builder;

type Span<'a> = LocatedSpan<&'a str, String>;

use super::*;

// ── AUTOCOMPLETION ────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

/// Main entry point for LSP completion requests.
#[tracing::instrument(skip_all)]
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
        Environment::Project => crate::metaprogramming::find_project_root(file_path)
            .map(|root: std::path::PathBuf| root.join("TypR")),
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
        if let CompletionCtx::ModuleFile = detect_completion_context(prefix) {
            panic!("should not detect ModuleFile after `;`")
        }
    }
}

