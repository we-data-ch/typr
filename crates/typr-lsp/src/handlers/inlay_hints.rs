use tower_lsp_server::ls_types::{InlayHint, InlayHintKind, InlayHintLabel, Position};
use typr_core::components::r#type::type_system::TypeSystem;

use crate::handlers::document::DocumentAnalysis;
use crate::handlers::utils::offset_to_position;

#[tracing::instrument(skip_all)]
pub fn resolve_inlay_hints(analysis: &DocumentAnalysis, content: &str) -> Option<Vec<InlayHint>> {
    let mut hints = Vec::new();

    let ast_value = serde_json::to_value(&analysis.ast).ok()?;
    let let_nodes = find_untyped_lets(&ast_value);

    for (name, offset) in let_nodes {
        let types = analysis.context.get_types_from_name(&name);
        if let Some(inferred_type) = types.last() {
            let pos = offset_to_position(offset, content);

            let end_col = pos.character + name.len() as u32;
            let end_pos = Position::new(pos.line, end_col);

            hints.push(InlayHint {
                position: end_pos,
                label: InlayHintLabel::String(format!(": {}", inferred_type.pretty())),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: Some(false),
                padding_right: Some(false),
                data: None,
            });
        }
    }

    Some(hints)
}

pub fn find_untyped_lets(val: &serde_json::Value) -> Vec<(String, usize)> {
    let mut results = Vec::new();
    match val {
        serde_json::Value::Object(map) => {
            if let Some(let_node) = map.get("Let") {
                if let Some(typ) = let_node.get("r#type").or_else(|| let_node.get("type")) {
                    if typ.get("Empty").is_some() {
                        if let Some(var) = let_node.get("variable").and_then(|v| v.get("Variable"))
                        {
                            if let (Some(name), Some(offset)) = (
                                var.get("name").and_then(|n| n.as_str()),
                                var.get("help_data")
                                    .and_then(|h| h.get("offset"))
                                    .and_then(|o| o.as_u64()),
                            ) {
                                results.push((name.to_string(), offset as usize));
                            }
                        }
                    }
                }
            }
            for v in map.values() {
                results.extend(find_untyped_lets(v));
            }
        }
        serde_json::Value::Array(arr) => {
            for v in arr {
                results.extend(find_untyped_lets(v));
            }
        }
        _ => {}
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_untyped_lets() {
        let code = "let my_var = 42;\nlet message = \"Hello\";";
        let (_, context, ast) = crate::lsp::check_code_and_extract_errors(code, "test.typr");
        let ast = ast.unwrap();

        let ast_value = serde_json::to_value(&ast).unwrap();
        println!("{}", serde_json::to_string_pretty(&ast_value).unwrap());

        let lets = find_untyped_lets(&ast_value);
        assert_eq!(lets.len(), 2);

        for (name, _) in lets {
            let types = context.as_ref().unwrap().get_types_from_name(&name);
            println!("Var {}: {:?}", name, types);
        }
    }
}
