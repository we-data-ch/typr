//! DOT export — one level at a time (spec §12 étape 2: "un niveau (le bloc `--focus`) avec ses
//! ports"). Renders [`BlockGraph::one_level`]: the focus block's direct children as record nodes
//! with their ports as fields, plus solid wire edges and dashed `Ref` edges.
//!
//! A wire's `to` is always some port inside the block that was being built when the builder
//! wired it in (`build_operand`'s `owner`) — sometimes that's the focus block's own input feeding
//! straight into a child (e.g. an `Apply`'s own body wires an argument in), sometimes it's a
//! child's own body wiring one of *its* ports from an ancestor's port or a sibling's output
//! (spec §7.1, see `typr-graph` builder notes). So a one-level view has to collect wires from
//! every block in the level, not just the focus block's own `body.wires`.

use crate::key::BlockKey;
use crate::model::{Block, RelationKind};
use crate::BlockGraph;
use std::fmt::Write as _;

/// `None` if `focus` isn't a block of `graph`.
pub fn to_dot(graph: &BlockGraph, focus: &BlockKey) -> Option<String> {
    let level = graph.one_level(focus)?;
    let mut out = String::new();
    let _ = writeln!(out, "digraph \"{}\" {{", escape(focus.as_str()));
    let _ = writeln!(out, "  rankdir=LR;");
    let _ = writeln!(out, "  node [shape=record, fontsize=10];");

    for (key, block) in &level.blocks {
        write_node(&mut out, key, block);
    }

    for block in level.blocks.values() {
        let Some(body) = &block.body else { continue };
        for wire in &body.wires {
            let _ = writeln!(
                out,
                "  \"{}\":\"{}\" -> \"{}\":\"{}\";",
                escape(wire.from.block.as_str()),
                escape(&wire.from.port),
                escape(wire.to.block.as_str()),
                escape(&wire.to.port),
            );
        }
    }

    for rel in &level.relations {
        if rel.kind != RelationKind::Ref {
            continue;
        }
        let label = rel.port.as_deref().unwrap_or("");
        let _ = writeln!(
            out,
            "  \"{}\" -> \"{}\" [style=dashed, label=\"{}\"];",
            escape(rel.from.as_str()),
            escape(rel.to.as_str()),
            escape(label),
        );
    }

    let _ = writeln!(out, "}}");
    Some(out)
}

fn write_node(out: &mut String, key: &BlockKey, block: &Block) {
    let name = block.name.clone().unwrap_or_else(|| format!("{:?}", block.kind));
    let inputs = ports_field(block.inputs.iter().map(|p| p.name.as_str()));
    let outputs = ports_field(block.outputs.iter().map(|p| p.name.as_str()));
    let label = match (inputs.is_empty(), outputs.is_empty()) {
        (true, true) => escape(&name),
        (false, true) => format!("{{{}}}|{}", inputs, escape(&name)),
        (true, false) => format!("{}|{{{}}}", escape(&name), outputs),
        (false, false) => format!("{{{}}}|{}|{{{}}}", inputs, escape(&name), outputs),
    };
    let _ = writeln!(out, "  \"{}\" [label=\"{}\"];", escape(key.as_str()), label);
}

/// A `{<port>label|<port>label|...}` record field group.
fn ports_field<'a>(ports: impl Iterator<Item = &'a str>) -> String {
    ports
        .map(|p| format!("<{}> {}", escape(p), escape(p)))
        .collect::<Vec<_>>()
        .join("|")
}

fn escape(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key::Namespace;
    use typr_core::components::context::Context;
    use typr_core::processes::parsing::parse_from_string;
    use typr_core::processes::type_checking::type_recorder::with_recording;
    use typr_core::processes::type_checking::typing_with_errors;

    #[test]
    fn one_level_dot_view_has_child_nodes_and_wires() {
        let source = "let sq <- fn(n: int): int { n * n };";
        let lang = parse_from_string(source, "dot_test");
        let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));
        assert!(!result.has_errors(), "{:?}", result.display_errors());
        let graph = crate::build(&lang, &result.type_context.context, &table);

        let sq = BlockKey::top_level(Namespace::Val, "sq");
        let dot = to_dot(&graph, &sq).expect("sq is a block of this graph");
        assert!(dot.starts_with("digraph"));
        assert!(dot.contains(&format!("\"{}\"", sq.as_str())));
        assert!(dot.contains(" -> "), "expected at least one edge in:\n{dot}");
    }

    #[test]
    fn missing_focus_returns_none() {
        let source = "let x <- 1;";
        let lang = parse_from_string(source, "dot_missing_test");
        let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));
        assert!(!result.has_errors());
        let graph = crate::build(&lang, &result.type_context.context, &table);

        assert!(to_dot(&graph, &BlockKey::from_raw("val:does-not-exist")).is_none());
    }
}
