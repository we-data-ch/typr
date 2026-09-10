//! Compact markdown renderer for the stdlib SPG.
//!
//! Produces a dense, MCP-friendly digest of the standard library's documented
//! entities — grouped by package, one entry per function/type, with a tier
//! badge, short signature, one-line description, and a single example.
//!
//! Target size: ~10–20 KB per package (RFC-STDLIB-0001 §3, Phase 3).
//!
//! The output is consumed by the MCP server as a read-only resource
//! (`typr://stdlib`), or written to a file via `typr std doc --format md`.

use std::collections::BTreeMap;
use typr_core::processes::spg::model::{Node, NodePayload, Spg};

/// Render an SPG into a compact markdown document.
///
/// Nodes are grouped by package (from `meta.pkg`), then sorted alphabetically
/// within each group.  Unnamed / non-stdlib nodes land in a `_typR` bucket.
pub fn render_stdlib_markdown(spg: &Spg) -> String {
    let groups = group_by_package(spg);
    let mut out = String::with_capacity(spg.nodes.len() * 120);

    out.push_str("# TypR Standard Library\n\n");
    out.push_str("> Auto-generated from `configs/std/*.ty` by `typr std doc --format md`.\n");
    out.push_str("> Compiled signatures (T1) are verified by the type-checker.\n\n");

    let total = spg.nodes.len();
    let with_meta = spg.nodes.iter().filter(|n| n.meta.is_some()).count();
    out.push_str(&format!(
        "**{}** documented entities ({} with structured metadata).\n\n",
        total, with_meta
    ));

    for (pkg, nodes) in &groups {
        render_package_section(&mut out, pkg, nodes);
    }

    out
}

/// Group SPG nodes by their `meta.pkg` field, sorted alphabetically.
/// Nodes without a package go into a `_typR` bucket.
fn group_by_package(spg: &Spg) -> BTreeMap<String, Vec<&Node>> {
    let mut groups: BTreeMap<String, Vec<&Node>> = BTreeMap::new();
    for node in &spg.nodes {
        let pkg = node
            .meta
            .as_ref()
            .and_then(|m| m.pkg.clone())
            .unwrap_or_else(|| "_typR".to_string());
        groups.entry(pkg).or_default().push(node);
    }
    groups
}

/// Render a single package section.
fn render_package_section(out: &mut String, pkg: &str, nodes: &[&Node]) {
    out.push_str(&format!("## `{}` package  \n", pkg));

    for node in nodes {
        render_node_entry(out, node);
    }
    out.push('\n');
}

/// Render a single function/type entry as a compact markdown block.
fn render_node_entry(out: &mut String, node: &Node) {
    let name = &node.name;
    let meta = node.meta.as_ref();

    // Tier badge
    let tier_badge = meta
        .and_then(|m| m.tier.as_deref())
        .map(|t| format!("`{}` ", t))
        .unwrap_or_default();

    // Signature
    let sig = render_signature(node);

    // One-line description (ret_doc or coercion_notes)
    let desc = meta
        .and_then(|m| {
            m.ret_doc
                .as_ref()
                .map(|d| d.as_str())
                .or(m.coercion_notes.as_deref())
        })
        .unwrap_or("");

    // First example (truncated to one line, stripping # noplayground prefix)
    let example = meta
        .and_then(|m| m.examples.first())
        .map(|e| {
            let clean = e.strip_prefix("# noplayground:").unwrap_or(e).trim();
            let clean = clean.strip_prefix("# noplayground").unwrap_or(clean).trim();
            let one_line = clean.lines().next().unwrap_or(clean);
            if one_line.len() > 80 {
                format!("{}...", &one_line[..77])
            } else {
                one_line.to_string()
            }
        });

    out.push_str(&format!("- {}**`{}`** `{}`", tier_badge, name, sig));
    if !desc.is_empty() {
        // First sentence only, truncated to 100 chars.
        let short = truncate_sentence(desc, 100);
        out.push_str(&format!(" — {}", short));
    }
    out.push('\n');

    if let Some(ex) = example {
        out.push_str(&format!("  ```\n  {}\n  ```\n", ex));
    }

    // Seealso links (compact, one line)
    if let Some(m) = meta {
        if !m.seealso.is_empty() {
            let links: Vec<String> = m.seealso.iter().map(|s| format!("`{}`", s)).collect();
            out.push_str(&format!("  See also: {}\n", links.join(", ")));
        }
    }
}

/// Render a function signature as a compact string.
///
/// For functions: `(T, U) -> V`
/// For records: `{ field: T, ... }`
/// For aliases: `= Underlying`
fn render_signature(node: &Node) -> String {
    match &node.payload {
        NodePayload::Function { params, returns } => {
            let params_str: Vec<String> = params
                .iter()
                .map(|(name, ty)| {
                    if name.is_empty() {
                        ty.clone()
                    } else {
                        format!("{}: {}", name, ty)
                    }
                })
                .collect();
            format!("({}) -> {}", params_str.join(", "), returns)
        }
        NodePayload::Record { fields } => {
            let fields_str: Vec<String> = fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, ty))
                .collect();
            format!("{{ {} }}", fields_str.join(", "))
        }
        NodePayload::Alias { underlying, opaque } => {
            if *opaque {
                format!("opaque = {}", underlying)
            } else {
                format!("= {}", underlying)
            }
        }
        NodePayload::Union { variants } => {
            let v: Vec<String> = variants
                .iter()
                .map(|(n, t)| match t {
                    Some(t) => format!(".{}({})", n, t),
                    None => format!(".{}", n),
                })
                .collect();
            v.join(" | ")
        }
        NodePayload::Variable { type_str } => type_str.clone(),
        NodePayload::Module { exports } => {
            format!("module [{}]", exports.join(", "))
        }
        NodePayload::None => String::new(),
    }
}

/// Truncate a description to the first sentence, max `max_len` chars.
fn truncate_sentence(desc: &str, max_len: usize) -> String {
    // Find the first sentence-ending punctuation or newline.
    let end = desc
        .find(|c: char| c == '.' || c == '\n')
        .unwrap_or(desc.len());
    let sentence = &desc[..end];
    if sentence.len() > max_len {
        // Use ASCII "..." to keep byte-length predictable.
        format!("{}...", &sentence[..max_len.saturating_sub(3)])
    } else {
        sentence.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use typr_core::processes::spg::model::{NodeKind, SourceLoc, StdlibMeta, Visibility};

    fn make_function_node(name: &str, params: Vec<(&str, &str)>, returns: &str) -> Node {
        Node {
            id: format!("function:{}", name),
            kind: NodeKind::Function,
            name: name.to_string(),
            module_path: vec![],
            visibility: Visibility::Public,
            doc: None,
            source: Some(SourceLoc {
                file: "test.ty".to_string(),
                offset: 0,
                line: 1,
            }),
            payload: NodePayload::Function {
                params: params
                    .into_iter()
                    .map(|(n, t)| (n.to_string(), t.to_string()))
                    .collect(),
                returns: returns.to_string(),
            },
            meta: None,
        }
    }

    fn with_meta(node: Node, tier: &str, pkg: &str, ret_doc: &str) -> Node {
        let mut node = node;
        node.meta = Some(StdlibMeta {
            tier: Some(tier.to_string()),
            param_docs: vec![],
            ret_doc: Some(ret_doc.to_string()),
            coercion_notes: None,
            examples: vec!["abs(-5)   # -> 5".to_string()],
            seealso: vec!["sign".to_string()],
            pkg: Some(pkg.to_string()),
        });
        node
    }

    #[test]
    fn render_empty_spg_produces_header() {
        let spg = Spg::new("test", "0.1");
        let md = render_stdlib_markdown(&spg);
        assert!(md.contains("# TypR Standard Library"));
        assert!(md.contains("**0** documented entities"));
    }

    #[test]
    fn render_function_with_meta_groups_by_package() {
        let spg = Spg {
            nodes: vec![with_meta(
                make_function_node("abs", vec![("x", "num")], "num"),
                "T1",
                "base",
                "absolute value",
            )],
            ..Spg::new("test", "0.1")
        };
        let md = render_stdlib_markdown(&spg);
        assert!(md.contains("## `base` package"));
        assert!(md.contains("**`abs`**"));
        assert!(md.contains("`T1`"));
        assert!(md.contains("absolute value"));
        assert!(md.contains("abs(-5)"));
        assert!(md.contains("`sign`"));
    }

    #[test]
    fn render_function_without_meta_goes_to_typr_bucket() {
        let spg = Spg {
            nodes: vec![make_function_node("sqrt", vec![("x", "num")], "num")],
            ..Spg::new("test", "0.1")
        };
        let md = render_stdlib_markdown(&spg);
        assert!(md.contains("## `_typR` package"));
        assert!(md.contains("**`sqrt`**"));
    }

    #[test]
    fn render_signature_function() {
        let node = make_function_node("add", vec![("a", "num"), ("b", "num")], "num");
        assert_eq!(render_signature(&node), "(a: num, b: num) -> num");
    }

    #[test]
    fn render_signature_unnamed_params() {
        let mut node = make_function_node("neg", vec![("", "num")], "num");
        // Override params to have empty names (as some signatures do).
        if let NodePayload::Function { ref mut params, .. } = node.payload {
            *params = vec![("".to_string(), "num".to_string())];
        }
        assert_eq!(render_signature(&node), "(num) -> num");
    }

    #[test]
    fn truncate_sentence_stops_at_period() {
        assert_eq!(truncate_sentence("absolute value.", 100), "absolute value");
        assert_eq!(
            truncate_sentence("absolute value of a number", 100),
            "absolute value of a number"
        );
        assert_eq!(truncate_sentence("ab", 10), "ab");
    }

    #[test]
    fn truncate_sentence_caps_length() {
        let long = "a]very long description that goes on and on.";
        assert_eq!(truncate_sentence(long, 10).len(), 10);
    }
}
