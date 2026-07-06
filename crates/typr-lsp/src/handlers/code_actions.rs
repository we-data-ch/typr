use crate::handlers::document::DocumentAnalysis;
use std::collections::HashMap;
use tower_lsp_server::ls_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    Position, Range, TextEdit, WorkspaceEdit,
};
use typr_core::components::r#type::type_system::TypeSystem;

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
        let let_nodes = crate::handlers::inlay_hints::find_untyped_lets(&ast_value);
        for (name, offset) in let_nodes {
            let pos = crate::handlers::utils::offset_to_position(offset, content);
            if pos.line == cursor_line {
                let types = analysis.context.get_types_from_name(&name);
                if let Some(inferred_type) = types.last() {
                    let end_col = pos.character + name.len() as u32;
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
    }

    if actions.is_empty() {
        None
    } else {
        Some(actions)
    }
}
