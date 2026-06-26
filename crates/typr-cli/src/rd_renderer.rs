#![allow(dead_code)]

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;
use typr_core::processes::spg::model::{Node, NodePayload, Spg, Visibility};

struct ParsedDoc {
    title: String,
    description: String,
    param_docs: HashMap<String, String>,
    return_doc: String,
    examples: Vec<String>,
}

fn parse_doc(raw: &str) -> ParsedDoc {
    let mut lines = raw.lines();
    let title = lines.next().unwrap_or("").trim().to_string();

    let mut description_lines: Vec<String> = Vec::new();
    let mut param_docs: HashMap<String, String> = HashMap::new();
    let mut return_doc = String::new();
    let mut examples: Vec<String> = Vec::new();
    let mut in_examples = false;

    for line in lines {
        let trimmed = line.trim();
        if trimmed.starts_with("@param") {
            in_examples = false;
            let rest = trimmed.trim_start_matches("@param").trim();
            if let Some(space_pos) = rest.find(' ') {
                let param_name = rest[..space_pos].trim().to_string();
                let param_doc = rest[space_pos..].trim().to_string();
                param_docs.insert(param_name, param_doc);
            }
        } else if trimmed.starts_with("@return") {
            in_examples = false;
            return_doc = trimmed.trim_start_matches("@return").trim().to_string();
        } else if trimmed.starts_with("@examples") {
            in_examples = true;
        } else if in_examples {
            examples.push(line.to_string());
        } else if !trimmed.is_empty() {
            description_lines.push(trimmed.to_string());
        }
    }

    ParsedDoc {
        title,
        description: description_lines.join(" "),
        param_docs,
        return_doc,
        examples,
    }
}

fn escape_rd(s: &str) -> String {
    s.replace('%', "\\%")
        .replace('{', "\\{")
        .replace('}', "\\}")
}

fn render_usage(name: &str, params: &[(String, String)]) -> String {
    let args = params.iter().map(|(p, _)| p.as_str()).collect::<Vec<_>>().join(", ");
    format!("{}({})", name, args)
}

fn render_arguments(params: &[(String, String)], param_docs: &HashMap<String, String>) -> String {
    params
        .iter()
        .map(|(name, ty)| {
            let doc = param_docs.get(name).cloned().unwrap_or_default();
            let desc = if doc.is_empty() {
                escape_rd(ty)
            } else {
                format!("{}. {}", escape_rd(ty), escape_rd(&doc))
            };
            format!("  \\item{{{}}}{{{}}}", escape_rd(name), desc)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn render_rd_for_node(node: &Node) -> Option<String> {
    if node.visibility == Visibility::Private {
        return None;
    }

    let parsed = node.doc.as_deref().map(parse_doc).unwrap_or_else(|| ParsedDoc {
        title: node.name.clone(),
        description: String::new(),
        param_docs: HashMap::new(),
        return_doc: String::new(),
        examples: Vec::new(),
    });

    match &node.payload {
        NodePayload::Function { params, returns } => {
            let title = if parsed.title.is_empty() { escape_rd(&node.name) } else { escape_rd(&parsed.title) };
            let description = if parsed.description.is_empty() { title.clone() } else { escape_rd(&parsed.description) };
            let usage = render_usage(&node.name, params);
            let args_block = render_arguments(params, &parsed.param_docs);
            let value_str = if parsed.return_doc.is_empty() {
                escape_rd(returns)
            } else {
                format!("{}. {}", escape_rd(returns), escape_rd(&parsed.return_doc))
            };

            let mut rd = format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n\\usage{{{}}}\n",
                node.name, node.name, title, description, usage
            );
            if !args_block.is_empty() {
                rd.push_str(&format!("\\arguments{{\n{}\n}}\n", args_block));
            }
            rd.push_str(&format!("\\value{{{}}}\n", value_str));
            if !parsed.examples.is_empty() {
                rd.push_str(&format!("\\examples{{\n{}\n}}\n", parsed.examples.join("\n")));
            }
            Some(rd)
        }

        NodePayload::Record { fields } => {
            let title = if parsed.title.is_empty() {
                format!("Record type {}", node.name)
            } else {
                escape_rd(&parsed.title)
            };
            let description = if parsed.description.is_empty() { title.clone() } else { escape_rd(&parsed.description) };
            let usage = render_usage(&node.name, fields);
            let args_block = render_arguments(fields, &parsed.param_docs);

            let mut rd = format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n\\usage{{{}}}\n",
                node.name, node.name, title, description, usage
            );
            if !args_block.is_empty() {
                rd.push_str(&format!("\\arguments{{\n{}\n}}\n", args_block));
            }
            rd.push_str(&format!("\\value{{An object of class '{}'}}\n", node.name));
            Some(rd)
        }

        NodePayload::Union { variants } => {
            let title = if parsed.title.is_empty() {
                format!("Union type {}", node.name)
            } else {
                escape_rd(&parsed.title)
            };
            let description = if parsed.description.is_empty() {
                let variants_str = variants
                    .iter()
                    .map(|(n, t)| match t {
                        Some(t) => format!(".{}({})", n, t),
                        None => format!(".{}", n),
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");
                format!("Union type: {}", escape_rd(&variants_str))
            } else {
                escape_rd(&parsed.description)
            };

            Some(format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n",
                node.name, node.name, title, description
            ))
        }

        NodePayload::Alias { underlying, opaque } => {
            let title = if parsed.title.is_empty() {
                format!("Type alias {}", node.name)
            } else {
                escape_rd(&parsed.title)
            };
            let description = if parsed.description.is_empty() {
                if *opaque {
                    format!("Opaque type alias for {}", escape_rd(underlying))
                } else {
                    format!("Type alias for {}", escape_rd(underlying))
                }
            } else {
                escape_rd(&parsed.description)
            };

            Some(format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n",
                node.name, node.name, title, description
            ))
        }

        NodePayload::Module { exports } => {
            let title = if parsed.title.is_empty() {
                format!("Module {}", node.name)
            } else {
                escape_rd(&parsed.title)
            };
            let description = if parsed.description.is_empty() {
                if exports.is_empty() {
                    format!("Module {}", node.name)
                } else {
                    format!("Module {} — exports: {}", node.name, exports.join(", "))
                }
            } else {
                escape_rd(&parsed.description)
            };

            Some(format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n",
                node.name, node.name, title, description
            ))
        }

        NodePayload::None => None,
    }
}

/// Generate `.Rd` files in `man_dir` for every public/exported node in `spg`.
/// Returns the number of files written.
pub fn generate_rd_files(spg: &Spg, man_dir: &Path) -> io::Result<usize> {
    let mut count = 0;
    for node in &spg.nodes {
        if let Some(rd_content) = render_rd_for_node(node) {
            let file_path = man_dir.join(format!("{}.Rd", node.name));
            fs::write(&file_path, rd_content)?;
            count += 1;
        }
    }
    Ok(count)
}
