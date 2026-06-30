#![allow(dead_code, unused_variables, unused_imports)]

use super::doc_attach::build_doc_map;
use super::doc_attach::build_doc_map_from_slice;
use super::edges::infer_edges;
use super::model::{Edge, EdgeKind, Node, NodeKind, NodePayload, SourceLoc, Spg, Visibility};
use crate::components::error_message::help_data::HelpData;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::Type;
use std::collections::HashMap;

/// Walk a typed AST and produce a `Spg` with all documentable nodes and edges.
pub fn build_spg(ast: &Lang, package: &str, version: &str) -> Spg {
    let mut spg = Spg::new(package, version);
    let doc_map = build_doc_map(ast);
    collect_nodes(ast, &mut spg, &[], &doc_map);
    infer_edges(&mut spg);
    spg
}

/// Build an SPG from a flat slice of already-typed `Lang` items (e.g. from
/// `TypeChecker::get_code()`) without needing a wrapping `Lang::Lines`.
pub fn build_spg_from_items(items: &[Lang], package: &str, version: &str) -> Spg {
    let mut spg = Spg::new(package, version);
    let doc_map = build_doc_map_from_slice(items);
    for item in items {
        collect_nodes(item, &mut spg, &[], &doc_map);
    }
    infer_edges(&mut spg);
    spg
}

fn collect_nodes(
    lang: &Lang,
    spg: &mut Spg,
    module_path: &[String],
    doc_map: &HashMap<usize, String>,
) {
    match lang {
        Lang::Lines { value, .. } => {
            for item in value {
                collect_nodes(item, spg, module_path, doc_map);
            }
        }

        Lang::Module {
            name,
            body,
            help_data,
            ..
        } => {
            let exports = body.iter().filter_map(exported_name).collect::<Vec<_>>();
            let id = Node::make_id(&NodeKind::Module, module_path, name);
            spg.add_node(Node {
                id,
                kind: NodeKind::Module,
                name: name.clone(),
                module_path: module_path.to_vec(),
                visibility: Visibility::Public,
                doc: doc_map.get(&help_data.get_offset()).cloned(),
                source: source_from_help(help_data),
                payload: NodePayload::Module { exports },
            });
            let mut child_path = module_path.to_vec();
            child_path.push(name.clone());
            for item in body {
                collect_nodes(item, spg, &child_path, doc_map);
            }
        }

        Lang::Let {
            variable,
            r#type,
            expression,
            is_public,
            is_export,
            help_data,
            ..
        } => {
            let name = lang_var_name(variable);
            if name.is_empty() {
                return;
            }
            // v1 scope: only document function bindings.
            // r#type holds the explicit let-annotation; when absent (Type::Empty),
            // fall back to the function literal's own parameter/return annotations.
            let fn_node: Option<(Vec<(String, String)>, String)> = match r#type {
                Type::Function(params, ret, _) => Some((
                    params
                        .iter()
                        .map(|p| (safe_arg_name(p), p.get_type().to_string()))
                        .collect(),
                    ret.to_string(),
                )),
                _ => {
                    if let Lang::Function {
                        parameters,
                        return_type,
                        ..
                    } = expression.as_ref()
                    {
                        if !matches!(return_type, Type::Empty(_)) {
                            Some((
                                parameters
                                    .iter()
                                    .map(|p| (safe_arg_name(p), p.get_type().to_string()))
                                    .collect(),
                                return_type.to_string(),
                            ))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
            };
            if let Some((param_list, returns)) = fn_node {
                let id = Node::make_id(&NodeKind::Function, module_path, &name);
                spg.add_node(Node {
                    id,
                    kind: NodeKind::Function,
                    name,
                    module_path: module_path.to_vec(),
                    visibility: to_visibility(*is_public, *is_export),
                    doc: doc_map.get(&help_data.get_offset()).cloned(),
                    source: source_from_help(help_data),
                    payload: NodePayload::Function {
                        params: param_list,
                        returns,
                    },
                });
            }
        }

        Lang::Alias {
            identifier,
            target_type,
            is_public,
            help_data,
            ..
        } => {
            let name = lang_var_name(identifier);
            if name.is_empty() {
                return;
            }
            let (kind, payload) = alias_node(target_type);
            let id = Node::make_id(&kind, module_path, &name);
            spg.add_node(Node {
                id,
                kind,
                name,
                module_path: module_path.to_vec(),
                visibility: to_visibility(*is_public, false),
                doc: doc_map.get(&help_data.get_offset()).cloned(),
                source: source_from_help(help_data),
                payload,
            });
        }

        _ => {}
    }
}

/// Extract the name from a `Lang::Variable` node (LHS of `let` / `type` alias).
fn lang_var_name(lang: &Lang) -> String {
    match lang {
        Lang::Variable { name, .. } => name.clone(),
        _ => String::new(),
    }
}

fn to_visibility(is_public: bool, is_export: bool) -> Visibility {
    if is_export {
        Visibility::Export
    } else if is_public {
        Visibility::Public
    } else {
        Visibility::Private
    }
}

fn source_from_help(hd: &HelpData) -> Option<SourceLoc> {
    hd.get_file_data()
        .map(|(file, src)| SourceLoc::from_offset(file, hd.get_offset(), &src))
}

/// Determine kind + payload for a `Lang::Alias` target type.
fn alias_node(t: &Type) -> (NodeKind, NodePayload) {
    match t {
        Type::Record(fields, _) => {
            let mut sorted: Vec<(String, String)> = fields
                .iter()
                .map(|f| (safe_arg_name(f), f.get_type().to_string()))
                .collect();
            sorted.sort_by(|a, b| a.0.cmp(&b.0));
            (NodeKind::TypeDef, NodePayload::Record { fields: sorted })
        }
        Type::Operator(TypeOperator::Union, _, _, _) => {
            let variants = collect_union_tags(t);
            (NodeKind::TypeDef, NodePayload::Union { variants })
        }
        Type::Alias(name, _, opaque, _) => (
            NodeKind::Alias,
            NodePayload::Alias {
                underlying: name.clone(),
                opaque: *opaque,
            },
        ),
        other => (
            NodeKind::Alias,
            NodePayload::Alias {
                underlying: other.to_string(),
                opaque: false,
            },
        ),
    }
}

/// Recursively flatten `Type::Operator(Union, …)` trees into their `Tag` leaves.
fn collect_union_tags(t: &Type) -> Vec<(String, Option<String>)> {
    match t {
        Type::Operator(TypeOperator::Union, lhs, rhs, _) => {
            let mut v = collect_union_tags(lhs);
            v.extend(collect_union_tags(rhs));
            v
        }
        Type::Tag(name, body, _) => {
            let payload = match body.as_ref() {
                Type::Empty(_) => None,
                other => Some(other.to_string()),
            };
            vec![(name.clone(), payload)]
        }
        other => vec![("_".to_string(), Some(other.to_string()))],
    }
}

/// Safely extract the parameter name from an `ArgumentType` (handles variadic).
fn safe_arg_name(at: &ArgumentType) -> String {
    if at.3 {
        return "...".to_string();
    }
    match &at.0 {
        Type::Char(Tchar::Val(s), _) => s.clone(),
        other => other.to_string(),
    }
}

/// Returns the exported/public name of a top-level binding if it carries one.
fn exported_name(lang: &Lang) -> Option<String> {
    match lang {
        Lang::Let {
            variable,
            is_public,
            is_export,
            ..
        } if *is_public || *is_export => {
            let name = lang_var_name(variable);
            if name.is_empty() {
                None
            } else {
                Some(name)
            }
        }
        Lang::Alias {
            identifier,
            is_public,
            ..
        } if *is_public => {
            let name = lang_var_name(identifier);
            if name.is_empty() {
                None
            } else {
                Some(name)
            }
        }
        _ => None,
    }
}
