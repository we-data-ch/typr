#![allow(dead_code)]

use std::collections::HashMap;
use std::collections::HashSet;
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

/// Build the set of node names that will have their own .Rd page.
/// Used to generate \link{} cross-references between pages.
fn collect_linkable_names(spg: &Spg) -> HashSet<String> {
    spg.nodes
        .iter()
        .filter(|n| n.visibility != Visibility::Private)
        .filter(|n| !matches!(n.payload, NodePayload::None))
        .map(|n| n.name.clone())
        .collect()
}

/// Replace PascalCase identifiers known to `linkable` (excluding `self_name`) with `\link{...}`.
/// Must be called AFTER `escape_rd()` so that user-supplied `{` and `}` are already escaped.
fn linkify(text: &str, linkable: &HashSet<String>, self_name: &str) -> String {
    if linkable.is_empty() {
        return text.to_string();
    }
    let mut result = String::with_capacity(text.len() + 32);
    let mut chars = text.chars().peekable();

    while let Some(c) = chars.next() {
        if c.is_ascii_uppercase() {
            let mut word = String::new();
            word.push(c);
            while let Some(&nc) = chars.peek() {
                if nc.is_alphanumeric() || nc == '_' {
                    word.push(nc);
                    chars.next();
                } else {
                    break;
                }
            }
            if linkable.contains(&word) && word != self_name {
                result.push_str("\\link{");
                result.push_str(&word);
                result.push('}');
            } else {
                result.push_str(&word);
            }
        } else {
            result.push(c);
        }
    }
    result
}

fn render_usage(name: &str, params: &[(String, String)]) -> String {
    let args = params.iter().map(|(p, _)| p.as_str()).collect::<Vec<_>>().join(", ");
    format!("{}({})", name, args)
}

fn render_arguments(
    params: &[(String, String)],
    param_docs: &HashMap<String, String>,
    linkable: &HashSet<String>,
    self_name: &str,
) -> String {
    params
        .iter()
        .map(|(name, ty)| {
            let doc = param_docs.get(name).cloned().unwrap_or_default();
            let ty_linked = linkify(&escape_rd(ty), linkable, self_name);
            let desc = if doc.is_empty() {
                ty_linked
            } else {
                format!("{}. {}", ty_linked, linkify(&escape_rd(&doc), linkable, self_name))
            };
            format!("  \\item{{{}}}{{{}}}", escape_rd(name), desc)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn render_rd_for_node(node: &Node, linkable: &HashSet<String>) -> Option<String> {
    if node.visibility == Visibility::Private {
        return None;
    }

    let self_name = node.name.as_str();
    let lk = |s: &str| linkify(&escape_rd(s), linkable, self_name);

    let parsed = node.doc.as_deref().map(parse_doc).unwrap_or_else(|| ParsedDoc {
        title: node.name.clone(),
        description: String::new(),
        param_docs: HashMap::new(),
        return_doc: String::new(),
        examples: Vec::new(),
    });

    match &node.payload {
        NodePayload::Function { params, returns } => {
            let title = if parsed.title.is_empty() { escape_rd(self_name) } else { lk(&parsed.title) };
            let description = if parsed.description.is_empty() { title.clone() } else { lk(&parsed.description) };
            let usage = render_usage(self_name, params);
            let args_block = render_arguments(params, &parsed.param_docs, linkable, self_name);
            let value_str = if parsed.return_doc.is_empty() {
                lk(returns)
            } else {
                format!("{}. {}", lk(returns), lk(&parsed.return_doc))
            };

            let mut rd = format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n\\usage{{{}}}\n",
                self_name, self_name, title, description, usage
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
                format!("Record type {}", self_name)
            } else {
                lk(&parsed.title)
            };
            let description = if parsed.description.is_empty() { title.clone() } else { lk(&parsed.description) };
            let usage = render_usage(self_name, fields);
            let args_block = render_arguments(fields, &parsed.param_docs, linkable, self_name);

            let mut rd = format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n\\usage{{{}}}\n",
                self_name, self_name, title, description, usage
            );
            if !args_block.is_empty() {
                rd.push_str(&format!("\\arguments{{\n{}\n}}\n", args_block));
            }
            rd.push_str(&format!("\\value{{An object of class '{}'}}\n", self_name));
            Some(rd)
        }

        NodePayload::Union { variants } => {
            let title = if parsed.title.is_empty() {
                format!("Union type {}", self_name)
            } else {
                lk(&parsed.title)
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
                format!("Union type: {}", lk(&variants_str))
            } else {
                lk(&parsed.description)
            };

            Some(format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n",
                self_name, self_name, title, description
            ))
        }

        NodePayload::Alias { underlying, opaque } => {
            let title = if parsed.title.is_empty() {
                format!("Type alias {}", self_name)
            } else {
                lk(&parsed.title)
            };
            let description = if parsed.description.is_empty() {
                if *opaque {
                    format!("Opaque type alias for {}", lk(underlying))
                } else {
                    format!("Type alias for {}", lk(underlying))
                }
            } else {
                lk(&parsed.description)
            };

            Some(format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n",
                self_name, self_name, title, description
            ))
        }

        NodePayload::Module { exports } => {
            let title = if parsed.title.is_empty() {
                format!("Module {}", self_name)
            } else {
                lk(&parsed.title)
            };
            let description = if parsed.description.is_empty() {
                if exports.is_empty() {
                    format!("Module {}", self_name)
                } else {
                    format!("Module {} — exports: {}", self_name, exports.join(", "))
                }
            } else {
                lk(&parsed.description)
            };

            Some(format!(
                "\\name{{{}}}\n\\alias{{{}}}\n\\title{{{}}}\n\\description{{{}}}\n",
                self_name, self_name, title, description
            ))
        }

        NodePayload::None => None,
    }
}

/// Generate `.Rd` files in `man_dir` for every public/exported node in `spg`.
/// Returns the number of files written.
pub fn generate_rd_files(spg: &Spg, man_dir: &Path) -> io::Result<usize> {
    let linkable = collect_linkable_names(spg);
    let mut count = 0;
    for node in &spg.nodes {
        if let Some(rd_content) = render_rd_for_node(node, &linkable) {
            let file_path = man_dir.join(format!("{}.Rd", node.name));
            fs::write(&file_path, rd_content)?;
            count += 1;
        }
    }
    Ok(count)
}
