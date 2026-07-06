use crate::handlers::document::DocumentAnalysis;
use std::collections::HashMap;
use tower_lsp_server::ls_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    Position, Range, TextEdit, WorkspaceEdit,
};
use typr_core::components::r#type::type_system::TypeSystem;

fn find_function_apps(val: &serde_json::Value) -> Vec<(String, String, usize)> {
    let mut results = Vec::new();

    fn traverse(val: &serde_json::Value, results: &mut Vec<(String, String, usize)>) {
        if let serde_json::Value::Object(map) = val {
            if let Some(func_app) = map.get("FunctionApp") {
                if let Some(args) = func_app.get("arguments").and_then(|a| a.as_array()) {
                    if args.len() == 1 {
                        let name = func_app
                            .get("identifier")
                            .and_then(|i| i.get("Variable"))
                            .and_then(|v| v.get("name"))
                            .and_then(|n| n.as_str());
                        let offset = func_app
                            .get("help_data")
                            .and_then(|h| h.get("offset"))
                            .and_then(|o| o.as_u64());
                        
                        // Extract first argument as string if it's a Variable for simple rewriting
                        let first_arg_name = args[0]
                            .get("Variable")
                            .and_then(|v| v.get("name"))
                            .and_then(|n| n.as_str());

                        if let (Some(n), Some(o), Some(a)) = (name, offset, first_arg_name) {
                            results.push((n.to_string(), a.to_string(), o as usize));
                        }
                    }
                }
            }
            for v in map.values() {
                traverse(v, results);
            }
        } else if let serde_json::Value::Array(arr) = val {
            for v in arr {
                traverse(v, results);
            }
        }
    }

    traverse(val, &mut results);
    results
}

#[tracing::instrument(skip_all)]
pub fn resolve_code_actions(
    params: CodeActionParams,
    analysis: &DocumentAnalysis,
    content: &str,
) -> Option<CodeActionResponse> {
    let mut actions = Vec::new();

    let uri = params.text_document.uri.clone();
    let cursor_line = params.range.start.line;

    if let Ok(ast_value) = serde_json::to_value(&analysis.ast) {
        // 1. Inlay hints / Let type annotations
        let let_nodes = crate::handlers::inlay_hints::find_untyped_lets(&ast_value);
        for (name, offset) in let_nodes {
            let pos = crate::handlers::utils::offset_to_position(offset, content);
            if pos.line == cursor_line {
                let types = analysis.context.get_types_from_name(&name);
                if let Some(inferred_type) = types.last() {
                    let line_text = content.lines().nth(pos.line as usize).unwrap_or("");
                    let true_start = line_text.find(&name).unwrap_or(pos.character as usize);
                    let end_col = (true_start + name.len()) as u32;
                    let end_pos = Position::new(pos.line, end_col);

                    let edit = TextEdit {
                        range: Range::new(end_pos, end_pos),
                        new_text: format!(": {}", inferred_type.pretty()),
                    };

                    let mut changes = HashMap::new();
                    changes.insert(uri.clone(), vec![edit]);

                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: format!("Add explicit type annotation for '{}'", name),
                        kind: Some(CodeActionKind::QUICKFIX),
                        edit: Some(WorkspaceEdit {
                            changes: Some(changes),
                            document_changes: None,
                            change_annotations: None,
                        }),
                        diagnostics: None,
                        ..Default::default()
                    }));
                }
            }
        }

        // 2. UFCS / Method syntax refactoring
        let func_apps = find_function_apps(&ast_value);
        for (func_name, first_arg, offset) in func_apps {
            let pos = crate::handlers::utils::offset_to_position(offset, content);
            if pos.line == cursor_line {
                // Find matching ')' for this function call
                let mut end_col = pos.character;
                let line_text = content.lines().nth(pos.line as usize).unwrap_or("");
                
                let mut depth = 0;
                let mut found_start = false;
                for (i, c) in line_text[pos.character as usize..].char_indices() {
                    if c == '(' {
                        depth += 1;
                        found_start = true;
                    } else if c == ')' {
                        depth -= 1;
                        if found_start && depth == 0 {
                            end_col = pos.character + i as u32 + 1;
                            break;
                        }
                    }
                }

                if found_start && depth == 0 {
                    let start_pos = Position::new(pos.line, pos.character);
                    let end_pos = Position::new(pos.line, end_col);

                    let edit = TextEdit {
                        range: Range::new(start_pos, end_pos),
                        new_text: format!("{}.{}()", first_arg, func_name),
                    };

                    let mut changes = HashMap::new();
                    changes.insert(uri.clone(), vec![edit]);

                    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                        title: format!("Rewrite to method syntax: '{}.{}()'", first_arg, func_name),
                        kind: Some(CodeActionKind::REFACTOR_REWRITE),
                        edit: Some(WorkspaceEdit {
                            changes: Some(changes),
                            document_changes: None,
                            change_annotations: None,
                        }),
                        diagnostics: None,
                        ..Default::default()
                    }));
                }
            }
        }
    }

    if actions.is_empty() {
        None
    } else {
        Some(actions)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::check_code_and_extract_errors;

    #[test]
    fn test_find_function_app() {
        let code = "is_minor(alice);";
        let (_, _, ast) = check_code_and_extract_errors(code, "test.typr");
        let ast = ast.unwrap();
        let ast_value = serde_json::to_value(&ast).unwrap();
        let apps = find_function_apps(&ast_value);
        assert_eq!(apps.len(), 1);
        assert_eq!(apps[0].0, "is_minor");
        assert_eq!(apps[0].1, "alice");
    }
}
