#![allow(dead_code, unused_variables, unused_imports)]

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;
use typr_core::processes::spg::model::{NodePayload, Spg};

use crate::rd_renderer::{parse_doc, ParsedDoc};

/// Generate SPG fragment files into `fragments_dir`.
///
/// For each documented node, writes:
///   fragments_dir/<name>/description.md
///   fragments_dir/<name>/signature.md   (functions and records)
///   fragments_dir/<name>/examples.md    (when @examples present)
///   fragments_dir/<name>/fields.md      (record types only)
///   fragments_dir/<name>/graph.md
pub fn generate_spg_fragments(spg: &Spg, fragments_dir: &Path) -> io::Result<()> {
    for node in &spg.nodes {
        if matches!(node.payload, NodePayload::None) {
            continue;
        }

        let node_dir = fragments_dir.join(&node.name);
        fs::create_dir_all(&node_dir)?;

        let parsed = node.doc.as_deref().map(parse_doc).unwrap_or_else(|| ParsedDoc {
            title: node.name.clone(),
            description: String::new(),
            param_docs: HashMap::new(),
            return_doc: String::new(),
            examples: Vec::new(),
        });

        // description
        let description = if !parsed.description.is_empty() {
            parsed.description.clone()
        } else if !parsed.title.is_empty() {
            parsed.title.clone()
        } else {
            node.name.clone()
        };
        fs::write(node_dir.join("description.md"), description)?;

        // examples (only when present)
        if !parsed.examples.is_empty() {
            fs::write(node_dir.join("examples.md"), parsed.examples.join("\n"))?;
        }

        // signature
        if let Some(sig) = render_signature(node) {
            fs::write(node_dir.join("signature.md"), sig)?;
        }

        // fields (record types only)
        if let NodePayload::Record { fields } = &node.payload {
            let fields_md = render_fields(fields, &parsed.param_docs);
            fs::write(node_dir.join("fields.md"), fields_md)?;
        }

        // graph
        let graph_md = render_graph(spg, node);
        fs::write(node_dir.join("graph.md"), graph_md)?;
    }
    Ok(())
}

fn render_signature(node: &typr_core::processes::spg::model::Node) -> Option<String> {
    match &node.payload {
        NodePayload::Function { params, .. } => {
            let args = params.iter().map(|(p, _)| p.as_str()).collect::<Vec<_>>().join(", ");
            Some(format!("{}({})", node.name, args))
        }
        NodePayload::Record { fields } => {
            let args = fields.iter().map(|(f, _)| f.as_str()).collect::<Vec<_>>().join(", ");
            Some(format!("{}({})", node.name, args))
        }
        _ => None,
    }
}

fn render_fields(fields: &[(String, String)], param_docs: &HashMap<String, String>) -> String {
    fields
        .iter()
        .map(|(name, ty)| {
            let doc = param_docs.get(name).cloned().unwrap_or_default();
            if doc.is_empty() {
                format!("- {}: {}", name, ty)
            } else {
                format!("- {}: {} — {}", name, ty, doc)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn render_graph(spg: &Spg, node: &typr_core::processes::spg::model::Node) -> String {
    let node_edges: Vec<_> = spg.edges.iter().filter(|e| e.from == node.id).collect();

    if node_edges.is_empty() {
        return format!("```mermaid\ngraph TD\n  {}\n```", node.name);
    }

    let mut lines = vec!["```mermaid".to_string(), "graph TD".to_string()];
    for edge in node_edges {
        // id format: "kind:path/name" — extract the final name component
        let to_name = edge.to.split(':').nth(1).unwrap_or(&edge.to);
        let to_name = to_name.split('/').last().unwrap_or(to_name);
        lines.push(format!("  {} --> {}", node.name, to_name));
    }
    lines.push("```".to_string());
    lines.join("\n")
}

/// Process `.Rmd` templates in `vignettes_dir`, replace `{{ type:entity }}` placeholders
/// with fragments from `fragments_dir`, and write compiled files to `compiled_dir`.
///
/// Returns the number of template files processed.
pub fn process_vignette_templates(
    fragments_dir: &Path,
    vignettes_dir: &Path,
    compiled_dir: &Path,
) -> io::Result<usize> {
    if !vignettes_dir.exists() {
        return Ok(0);
    }

    fs::create_dir_all(compiled_dir)?;

    let mut count = 0;
    for entry in fs::read_dir(vignettes_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            continue;
        }
        let name = match path.file_name().and_then(|n| n.to_str()) {
            Some(n) => n.to_string(),
            None => continue,
        };
        if !name.ends_with(".Rmd") && !name.ends_with(".rmd") {
            continue;
        }

        let content = fs::read_to_string(&path)?;
        let injected = inject_placeholders(&content, fragments_dir);
        fs::write(compiled_dir.join(&name), injected)?;
        count += 1;
    }
    Ok(count)
}

/// Replace all `{{ fragment_type:entity }}` placeholders in `content` with the
/// contents of `fragments_dir/<entity>/<fragment_type>.md`.
///
/// Unknown placeholders are replaced with an HTML comment.
fn inject_placeholders(content: &str, fragments_dir: &Path) -> String {
    let mut result = String::with_capacity(content.len());
    let mut rest = content;

    while let Some(start) = rest.find("{{") {
        result.push_str(&rest[..start]);
        rest = &rest[start + 2..];

        match rest.find("}}") {
            None => {
                // Unclosed {{ — emit as-is and stop processing
                result.push_str("{{");
                break;
            }
            Some(end) => {
                let placeholder = rest[..end].trim();
                rest = &rest[end + 2..];

                match placeholder.find(':') {
                    None => {
                        // Not a type:entity placeholder — keep as-is
                        result.push_str("{{");
                        result.push_str(placeholder);
                        result.push_str("}}");
                    }
                    Some(colon) => {
                        let fragment_type = placeholder[..colon].trim();
                        let entity = placeholder[colon + 1..].trim();
                        let fragment_path =
                            fragments_dir.join(entity).join(format!("{}.md", fragment_type));

                        let replacement = if fragment_path.exists() {
                            fs::read_to_string(&fragment_path).unwrap_or_else(|_| {
                                format!("<!-- error reading fragment: {}/{} -->", entity, fragment_type)
                            })
                        } else {
                            format!("<!-- fragment not found: {}/{} -->", entity, fragment_type)
                        };
                        result.push_str(&replacement);
                    }
                }
            }
        }
    }

    result.push_str(rest);
    result
}
