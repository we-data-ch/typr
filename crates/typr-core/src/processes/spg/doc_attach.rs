#![allow(dead_code)]

use crate::components::language::Lang;
use std::collections::HashMap;

/// Walk the AST and build a map from each declaration's byte offset to
/// its preceding doc-comment block.
///
/// A doc-comment block is formed by consecutive `Lang::Comment` lines
/// immediately before a declaration (no intervening non-comment nodes).
/// Multiple lines are joined with `\n`. Leading/trailing whitespace on
/// each line is stripped.
pub fn build_doc_map(lang: &Lang) -> HashMap<usize, String> {
    let mut map = HashMap::new();
    collect_docs(lang, &mut map);
    map
}

pub fn build_doc_map_from_slice(items: &[Lang]) -> HashMap<usize, String> {
    let mut map = HashMap::new();
    attach_from_slice(items, &mut map);
    for item in items {
        collect_docs(item, &mut map);
    }
    map
}

fn collect_docs(lang: &Lang, map: &mut HashMap<usize, String>) {
    match lang {
        Lang::Lines { value, .. } => {
            attach_from_slice(value, map);
            for item in value {
                collect_docs(item, map);
            }
        }
        Lang::Module { body, .. } => {
            attach_from_slice(body, map);
            for item in body {
                collect_docs(item, map);
            }
        }
        _ => {}
    }
}

/// Scan a flat slice of `Lang` items, flushing any accumulated comment
/// block to the map when the next non-comment declaration is seen.
fn attach_from_slice(items: &[Lang], map: &mut HashMap<usize, String>) {
    let mut pending: Vec<String> = Vec::new();

    for item in items {
        match item {
            Lang::Comment { value, .. } => {
                pending.push(value.trim().to_string());
            }
            other => {
                if !pending.is_empty() {
                    let doc = pending.join("\n");
                    let offset = other.get_help_data().get_offset();
                    map.insert(offset, doc);
                    pending.clear();
                }
            }
        }
    }
}
