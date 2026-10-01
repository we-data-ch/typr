//! Projections (spec §8): flatten the block hierarchy and filter by relation/block kind. A
//! projection always drops nested bodies — it's a flat map of the blocks it keeps, plus the
//! relations whose kind it keeps, each re-pointed at whichever kept block is its nearest match
//! ("on remplace chaque chaîne capture → `Ref` par une arête directe", spec §8).

use crate::key::BlockKey;
use crate::model::{BlockGraph, BlockKind, Relation, RelationKind};
use std::collections::BTreeSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Projection {
    /// Top-level named blocks + flattened `Ref` edges (étape 2).
    Dependencies,
    /// `TypeDecl`/`Interface` blocks plus the type-relation kinds that touch them. `Satisfies`,
    /// `DeclaredAs` and `Subtype` don't exist in the graph yet (étape 3): until then this
    /// projection only ever has `HasType`/`TypePosition` edges to show.
    Types,
}

pub fn project(graph: &BlockGraph, projection: Projection) -> BlockGraph {
    match projection {
        Projection::Dependencies => dependencies(graph),
        Projection::Types => types(graph),
    }
}

fn dependencies(graph: &BlockGraph) -> BlockGraph {
    let mut out = BlockGraph::new(graph.root.clone());
    for (key, block) in &graph.blocks {
        // The synthetic Program root is top-level-shaped (no `/`) but isn't itself a "named"
        // declaration (spec §8) — left out so the projection is purely the named blocks.
        if key.is_top_level() && *key != graph.root {
            let mut flat = block.clone();
            flat.body = None;
            out.blocks.insert(key.clone(), flat);
        }
    }

    let mut seen: BTreeSet<(BlockKey, BlockKey)> = BTreeSet::new();
    for rel in &graph.relations {
        if rel.kind != RelationKind::Ref {
            continue;
        }
        let from = rel.from.top_level_ancestor();
        let to = rel.to.top_level_ancestor();
        if from == to || !out.blocks.contains_key(&from) || !out.blocks.contains_key(&to) {
            continue;
        }
        if !seen.insert((from.clone(), to.clone())) {
            continue;
        }
        out.relations.push(Relation {
            kind: RelationKind::Ref,
            from,
            to,
            port: None,
            index: None,
            confidence: rel.confidence.clone(),
            evidence: None,
        });
    }
    out
}

fn types(graph: &BlockGraph) -> BlockGraph {
    let mut out = BlockGraph::new(graph.root.clone());

    let kept: Vec<&Relation> = graph
        .relations
        .iter()
        .filter(|r| {
            matches!(
                r.kind,
                RelationKind::HasType
                    | RelationKind::TypePosition
                    | RelationKind::Satisfies
                    | RelationKind::DeclaredAs
                    | RelationKind::Subtype
            )
        })
        .collect();

    // Every TypeDecl/Interface, plus whichever value blocks a kept relation points from (e.g. a
    // `HasType` edge's `from` is the value block that has the type, not a type block itself) —
    // otherwise those relations would dangle at a block this projection dropped.
    let mut keys: BTreeSet<BlockKey> = graph
        .blocks
        .iter()
        .filter(|(_, b)| matches!(b.kind, BlockKind::TypeDecl | BlockKind::Interface))
        .map(|(k, _)| k.clone())
        .collect();
    for rel in &kept {
        keys.insert(rel.from.clone());
        keys.insert(rel.to.clone());
    }

    for key in keys {
        if let Some(block) = graph.blocks.get(&key) {
            let mut flat = block.clone();
            flat.body = None;
            out.blocks.insert(key, flat);
        }
    }
    out.relations = kept.into_iter().cloned().collect();
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key::Namespace;
    use typr_core::components::context::Context;
    use typr_core::processes::parsing::parse_from_string;
    use typr_core::processes::type_checking::type_recorder::with_recording;
    use typr_core::processes::type_checking::typing_with_errors;

    const SOURCE: &str = r#"
type Point <- list { x: int, y: int };

let sq <- fn(n: int): int { n * n };

let norm2 <- fn(p: Point): int {
    let a <- sq(p$x);
    a + sq(p$y)
};

let total <- norm2(Point:{ x = 3, y = 4 });
"#;

    fn build_graph() -> BlockGraph {
        let lang = parse_from_string(SOURCE, "project_test");
        let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));
        assert!(!result.has_errors(), "{:?}", result.display_errors());
        crate::build(&lang, &result.type_context.context, &table)
    }

    #[test]
    fn dependencies_flattens_captures_into_top_level_edges() {
        let graph = build_graph();
        let deps = project(&graph, Projection::Dependencies);

        // Every kept block is top-level and the synthetic root is gone.
        assert!(deps.blocks.keys().all(|k| k.is_top_level()));
        assert!(!deps.blocks.contains_key(&graph.root));

        // `norm2`'s body calls `sq` twice, nested two levels deep (`norm2/a` and `norm2/#1/lhs`
        // or similar) — the projection must still surface a single top-level `norm2` -> `sq`
        // edge, not one per call site and not one that still points at the nested key.
        let norm2 = BlockKey::top_level(Namespace::Val, "norm2");
        let sq = BlockKey::top_level(Namespace::Val, "sq");
        let norm2_to_sq: Vec<_> = deps
            .relations
            .iter()
            .filter(|r| r.kind == RelationKind::Ref && r.from == norm2 && r.to == sq)
            .collect();
        assert_eq!(
            norm2_to_sq.len(),
            1,
            "expected exactly one flattened norm2 -> sq edge, got {:?}",
            norm2_to_sq
        );

        // `total` calls `norm2`.
        let total = BlockKey::top_level(Namespace::Val, "total");
        assert!(deps
            .relations
            .iter()
            .any(|r| r.kind == RelationKind::Ref && r.from == total && r.to == norm2));

        // Flattening never introduces a self-loop.
        assert!(deps.relations.iter().all(|r| r.from != r.to));
    }

    #[test]
    fn types_keeps_type_decls_and_has_type_edges() {
        let graph = build_graph();
        let types_view = project(&graph, Projection::Types);

        let point = BlockKey::top_level(Namespace::Type, "Point");
        assert!(types_view.blocks.contains_key(&point));
        assert!(types_view
            .relations
            .iter()
            .any(|r| r.kind == RelationKind::HasType && r.to == point));
        assert!(types_view.relations.iter().all(|r| matches!(
            r.kind,
            RelationKind::HasType
                | RelationKind::TypePosition
                | RelationKind::Satisfies
                | RelationKind::DeclaredAs
                | RelationKind::Subtype
        )));
    }
}
