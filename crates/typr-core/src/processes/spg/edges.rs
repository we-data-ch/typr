#![allow(dead_code, unused_imports)]

use super::model::{Edge, EdgeKind, Node, NodeKind, NodePayload, Spg};
use std::collections::HashMap;
use std::collections::HashSet;

/// Infer all edges from the nodes already in `spg` and append them.
/// Called by `build_spg` after the node-collection pass.
pub fn infer_edges(spg: &mut Spg) {
    // name → id index; on collision (same name in different modules) first wins.
    let name_to_id: HashMap<String, String> = spg.nodes.iter().map(|n| (n.name.clone(), n.id.clone())).collect();

    let id_set: HashSet<String> = spg.nodes.iter().map(|n| n.id.clone()).collect();

    // Snapshot what we need so we can mutate `spg.edges` afterwards.
    let snapshots: Vec<(String, Vec<String>, NodePayload)> = spg
        .nodes
        .iter()
        .map(|n| (n.id.clone(), n.module_path.clone(), n.payload.clone()))
        .collect();

    let mut new_edges: Vec<Edge> = Vec::new();

    for (node_id, module_path, payload) in &snapshots {
        match payload {
            NodePayload::Function { params, returns } => {
                for (_, param_type) in params {
                    if let Some(tid) = resolve_type(param_type, &name_to_id) {
                        new_edges.push(mk_edge(node_id, &tid, EdgeKind::ConsumesType));
                        new_edges.push(mk_edge(node_id, &tid, EdgeKind::Uses));
                    }
                }
                if let Some(tid) = resolve_type(returns, &name_to_id) {
                    new_edges.push(mk_edge(node_id, &tid, EdgeKind::Returns));
                    new_edges.push(mk_edge(node_id, &tid, EdgeKind::Uses));
                }
            }

            NodePayload::Record { fields } => {
                for (_, field_type) in fields {
                    if let Some(tid) = resolve_type(field_type, &name_to_id) {
                        new_edges.push(mk_edge(node_id, &tid, EdgeKind::HasField));
                    }
                }
            }

            NodePayload::Union { variants } => {
                for (_, payload_type) in variants {
                    if let Some(pt) = payload_type {
                        if let Some(tid) = resolve_type(pt, &name_to_id) {
                            new_edges.push(mk_edge(node_id, &tid, EdgeKind::ProducesType));
                        }
                    }
                }
            }

            NodePayload::Alias { underlying, .. } => {
                if let Some(tid) = resolve_type(underlying, &name_to_id) {
                    new_edges.push(mk_edge(node_id, &tid, EdgeKind::Uses));
                }
            }

            NodePayload::Module { .. } | NodePayload::None => {}
        }

        // belongs-to-module: nested node → its immediate parent module.
        if !module_path.is_empty() {
            let parent_name = module_path.last().unwrap();
            let parent_path = &module_path[..module_path.len() - 1];
            let parent_id = Node::make_id(&NodeKind::Module, parent_path, parent_name);
            if id_set.contains(&parent_id) {
                new_edges.push(mk_edge(node_id, &parent_id, EdgeKind::BelongsToModule));
            }
        }
    }

    // Auto-generate inverse edges (parameter-of, used-by).
    let inverse: Vec<Edge> = new_edges.iter().filter_map(inverse_of).collect();
    new_edges.extend(inverse);

    // Deduplicate before appending (self-referential edges are also dropped).
    let mut seen: HashSet<(String, String, String)> = HashSet::new();
    for e in new_edges {
        if e.from == e.to {
            continue;
        }
        let key = (e.from.clone(), e.to.clone(), format!("{:?}", e.kind));
        if seen.insert(key) {
            spg.add_edge(e);
        }
    }
}

fn mk_edge(from: &str, to: &str, kind: EdgeKind) -> Edge {
    Edge {
        from: from.to_string(),
        to: to.to_string(),
        kind,
    }
}

/// Produce the inverse edge for edge kinds that have one.
fn inverse_of(e: &Edge) -> Option<Edge> {
    match e.kind {
        EdgeKind::ConsumesType => Some(mk_edge(&e.to, &e.from, EdgeKind::ParameterOf)),
        EdgeKind::Uses => Some(mk_edge(&e.to, &e.from, EdgeKind::UsedBy)),
        _ => None,
    }
}

/// Resolve a type-string to a node id via exact name match.
/// v1: generic instantiations (e.g. `Option<int>`) are not resolved.
fn resolve_type(type_str: &str, name_to_id: &HashMap<String, String>) -> Option<String> {
    name_to_id.get(type_str).cloned()
}
