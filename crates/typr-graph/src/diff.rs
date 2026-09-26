//! `diff(old, new)` (spec §12 étape 6): two block-graphs of the *same program at two points in
//! time*, paired up by [`BlockKey`] — the whole point of a stable semantic identity (§6) is that
//! it survives an edit, so a block that didn't move keeps its key and a block that did shows up
//! as removed+added rather than silently "modified" into something unrelated.
//!
//! Scope is deliberately block-level, matching the spec's own list: added/removed blocks, plus
//! for a block present in both, whether its `type`, `captures` (implicit input ports, §3.3) or
//! `interface` (`Satisfies`/`DeclaredAs` targets, for a `TypeDecl`) changed. Wire-level and
//! `Ref`-level diffing is out of scope here — the spec's own wording is `type, interface,
//! captures`, not "every relation".

use crate::key::BlockKey;
use crate::model::{BlockGraph, BlockKind, Port, RelationKind};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CaptureChange {
    pub added: Vec<Port>,
    pub removed: Vec<Port>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InterfaceChange {
    /// `Satisfies`/`DeclaredAs` targets (interfaces) gained or lost — the block's declared
    /// contract, not its full relation list.
    pub added: Vec<BlockKey>,
    pub removed: Vec<BlockKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlockDiff {
    pub key: BlockKey,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<(BlockKind, BlockKind)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<(Option<String>, Option<String>)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#type: Option<(Option<String>, Option<String>)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub captures: Option<CaptureChange>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub interface: Option<InterfaceChange>,
}

impl BlockDiff {
    fn is_empty(&self) -> bool {
        self.kind.is_none()
            && self.name.is_none()
            && self.r#type.is_none()
            && self.captures.is_none()
            && self.interface.is_none()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct GraphDiff {
    pub added: Vec<BlockKey>,
    pub removed: Vec<BlockKey>,
    pub modified: Vec<BlockDiff>,
}

/// Pairs `old` and `new` by [`BlockKey`] and reports what changed (spec §12 étape 6). Both
/// graphs are expected to come from the same builder (`typr_graph::build`) run on two versions
/// of the same source — nothing here assumes that, but a `BlockKey` computed by a different
/// scheme would just never match and everything would show up as removed+added.
pub fn diff(old: &BlockGraph, new: &BlockGraph) -> GraphDiff {
    let mut out = GraphDiff::default();

    for key in old.blocks.keys() {
        if !new.blocks.contains_key(key) {
            out.removed.push(key.clone());
        }
    }
    for key in new.blocks.keys() {
        if !old.blocks.contains_key(key) {
            out.added.push(key.clone());
        }
    }

    for (key, old_block) in &old.blocks {
        let Some(new_block) = new.blocks.get(key) else {
            continue;
        };

        let mut d = BlockDiff {
            key: key.clone(),
            kind: None,
            name: None,
            r#type: None,
            captures: None,
            interface: None,
        };

        if old_block.kind != new_block.kind {
            d.kind = Some((old_block.kind, new_block.kind));
        }
        if old_block.name != new_block.name {
            d.name = Some((old_block.name.clone(), new_block.name.clone()));
        }
        if old_block.r#type != new_block.r#type {
            d.r#type = Some((old_block.r#type.clone(), new_block.r#type.clone()));
        }

        let old_captures: Vec<&Port> = old_block.inputs.iter().filter(|p| p.implicit).collect();
        let new_captures: Vec<&Port> = new_block.inputs.iter().filter(|p| p.implicit).collect();
        let captures_added: Vec<Port> = new_captures
            .iter()
            .filter(|p| !old_captures.contains(p))
            .map(|p| (*p).clone())
            .collect();
        let captures_removed: Vec<Port> = old_captures
            .iter()
            .filter(|p| !new_captures.contains(p))
            .map(|p| (*p).clone())
            .collect();
        if !captures_added.is_empty() || !captures_removed.is_empty() {
            d.captures = Some(CaptureChange {
                added: captures_added,
                removed: captures_removed,
            });
        }

        let old_interface = interface_targets(old, key);
        let new_interface = interface_targets(new, key);
        let interface_added: Vec<BlockKey> = new_interface
            .difference(&old_interface)
            .cloned()
            .collect();
        let interface_removed: Vec<BlockKey> = old_interface
            .difference(&new_interface)
            .cloned()
            .collect();
        if !interface_added.is_empty() || !interface_removed.is_empty() {
            d.interface = Some(InterfaceChange {
                added: interface_added,
                removed: interface_removed,
            });
        }

        if !d.is_empty() {
            out.modified.push(d);
        }
    }

    out.added.sort();
    out.removed.sort();
    out.modified.sort_by(|a, b| a.key.cmp(&b.key));
    out
}

/// The `Satisfies`/`DeclaredAs` targets of `key` in `graph` — its declared type contract.
fn interface_targets(graph: &BlockGraph, key: &BlockKey) -> BTreeSet<BlockKey> {
    graph
        .relations
        .iter()
        .filter(|r| {
            r.from == *key && matches!(r.kind, RelationKind::Satisfies | RelationKind::DeclaredAs)
        })
        .map(|r| r.to.clone())
        .collect()
}

/// A compact human-readable rendering of a [`GraphDiff`], one line per block
/// (`+`/`-`/`~` prefix, like a source diff's file-level summary) with indented sub-lines for
/// what changed on a modified block. `--format json` is the machine contract (spec §9-shaped);
/// this is the CLI's `--format text` and the playground diff view's textual fallback.
pub fn to_text(diff: &GraphDiff) -> String {
    let mut out = String::new();
    for key in &diff.added {
        out.push_str(&format!("+ {}\n", key.as_str()));
    }
    for key in &diff.removed {
        out.push_str(&format!("- {}\n", key.as_str()));
    }
    for m in &diff.modified {
        out.push_str(&format!("~ {}\n", m.key.as_str()));
        if let Some((before, after)) = &m.kind {
            out.push_str(&format!("    kind: {:?} -> {:?}\n", before, after));
        }
        if let Some((before, after)) = &m.name {
            out.push_str(&format!(
                "    name: {} -> {}\n",
                before.as_deref().unwrap_or("_"),
                after.as_deref().unwrap_or("_")
            ));
        }
        if let Some((before, after)) = &m.r#type {
            out.push_str(&format!(
                "    type: {} -> {}\n",
                before.as_deref().unwrap_or("_"),
                after.as_deref().unwrap_or("_")
            ));
        }
        if let Some(c) = &m.captures {
            out.push_str(&format!(
                "    captures: {}\n",
                format_port_delta(&c.added, &c.removed)
            ));
        }
        if let Some(i) = &m.interface {
            out.push_str(&format!(
                "    interface: {}\n",
                format_key_delta(&i.added, &i.removed)
            ));
        }
    }
    out
}

fn format_port_delta(added: &[Port], removed: &[Port]) -> String {
    let plus = added.iter().map(|p| format!("+{}", p.name));
    let minus = removed.iter().map(|p| format!("-{}", p.name));
    plus.chain(minus).collect::<Vec<_>>().join(" ")
}

fn format_key_delta(added: &[BlockKey], removed: &[BlockKey]) -> String {
    let plus = added.iter().map(|k| format!("+{}", k.as_str()));
    let minus = removed.iter().map(|k| format!("-{}", k.as_str()));
    plus.chain(minus).collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Block, Body, Origin};

    fn block(key: &str, kind: BlockKind) -> Block {
        Block {
            key: BlockKey::from_raw(key),
            kind,
            name: None,
            span: None,
            r#type: None,
            inputs: Vec::new(),
            outputs: Vec::new(),
            origin: Origin::User,
            body: None,
        }
    }

    #[test]
    fn detects_added_and_removed_blocks() {
        let mut old = BlockGraph::new(BlockKey::from_raw("val:@program"));
        old.insert(block("val:@program", BlockKind::Program));
        old.insert(block("val:a", BlockKind::Function));

        let mut new = BlockGraph::new(BlockKey::from_raw("val:@program"));
        new.insert(block("val:@program", BlockKind::Program));
        new.insert(block("val:b", BlockKind::Function));

        let d = diff(&old, &new);
        assert_eq!(d.added, vec![BlockKey::from_raw("val:b")]);
        assert_eq!(d.removed, vec![BlockKey::from_raw("val:a")]);
        assert!(d.modified.is_empty());
    }

    #[test]
    fn detects_type_change() {
        let mut old = BlockGraph::new(BlockKey::from_raw("val:@program"));
        let mut a = block("val:a", BlockKind::Function);
        a.r#type = Some("int".to_string());
        old.insert(a);

        let mut new = BlockGraph::new(BlockKey::from_raw("val:@program"));
        let mut a2 = block("val:a", BlockKind::Function);
        a2.r#type = Some("char".to_string());
        new.insert(a2);

        let d = diff(&old, &new);
        assert_eq!(d.modified.len(), 1);
        assert_eq!(
            d.modified[0].r#type,
            Some((Some("int".to_string()), Some("char".to_string())))
        );
        assert!(d.modified[0].captures.is_none());
        assert!(d.modified[0].interface.is_none());
    }

    #[test]
    fn detects_capture_change() {
        let mut old = BlockGraph::new(BlockKey::from_raw("val:@program"));
        let mut a = block("val:a", BlockKind::Function);
        a.inputs = vec![Port::implicit("sq", None)];
        old.insert(a);

        let mut new = BlockGraph::new(BlockKey::from_raw("val:@program"));
        let mut a2 = block("val:a", BlockKind::Function);
        a2.inputs = vec![Port::implicit("norm", None)];
        new.insert(a2);

        let d = diff(&old, &new);
        assert_eq!(d.modified.len(), 1);
        let captures = d.modified[0].captures.as_ref().unwrap();
        assert_eq!(captures.added, vec![Port::implicit("norm", None)]);
        assert_eq!(captures.removed, vec![Port::implicit("sq", None)]);
    }

    #[test]
    fn ignores_explicit_input_changes_as_captures() {
        let mut old = BlockGraph::new(BlockKey::from_raw("val:@program"));
        let mut a = block("val:a", BlockKind::Function);
        a.inputs = vec![Port::explicit("n", Some("int".to_string()))];
        old.insert(a);

        let mut new = BlockGraph::new(BlockKey::from_raw("val:@program"));
        let mut a2 = block("val:a", BlockKind::Function);
        a2.inputs = vec![Port::explicit("m", Some("int".to_string()))];
        new.insert(a2);

        let d = diff(&old, &new);
        assert!(d.modified.is_empty(), "explicit params aren't captures");
    }

    #[test]
    fn detects_interface_change() {
        use crate::model::Relation;

        let mut old = BlockGraph::new(BlockKey::from_raw("val:@program"));
        old.insert(block("type:Point", BlockKind::TypeDecl));
        old.insert(block("type:Printable", BlockKind::Interface));
        old.relations.push(Relation::satisfies(
            BlockKey::from_raw("type:Point"),
            BlockKey::from_raw("type:Printable"),
            Vec::new(),
        ));

        let mut new = BlockGraph::new(BlockKey::from_raw("val:@program"));
        new.insert(block("type:Point", BlockKind::TypeDecl));
        new.insert(block("type:Printable", BlockKind::Interface));
        // Printable satisfaction dropped in the new version.

        let d = diff(&old, &new);
        let point_diff = d
            .modified
            .iter()
            .find(|m| m.key == BlockKey::from_raw("type:Point"))
            .expect("Point should be reported modified");
        let interface = point_diff.interface.as_ref().unwrap();
        assert_eq!(interface.removed, vec![BlockKey::from_raw("type:Printable")]);
        assert!(interface.added.is_empty());
    }

    #[test]
    fn unchanged_block_is_not_reported() {
        let mut old = BlockGraph::new(BlockKey::from_raw("val:@program"));
        old.insert(block("val:a", BlockKind::Function));
        let mut new = BlockGraph::new(BlockKey::from_raw("val:@program"));
        new.insert(block("val:a", BlockKind::Function));

        let d = diff(&old, &new);
        assert!(d.added.is_empty());
        assert!(d.removed.is_empty());
        assert!(d.modified.is_empty());
    }

    #[test]
    fn text_rendering_is_stable_and_readable() {
        let mut old = BlockGraph::new(BlockKey::from_raw("val:@program"));
        old.insert(block("val:a", BlockKind::Function));
        let mut new = BlockGraph::new(BlockKey::from_raw("val:@program"));
        new.insert(block("val:b", BlockKind::Function));

        let d = diff(&old, &new);
        let text = to_text(&d);
        assert_eq!(text, "+ val:b\n- val:a\n");
    }

    #[test]
    fn body_only_changes_are_not_flagged_at_this_key() {
        // A body's children/wires changing (e.g. a new statement inside a Scope) isn't one of
        // the spec's tracked attributes (type/interface/captures) — it shows up as the *inner*
        // blocks being added/removed instead, not as a change on the outer block itself.
        let mut old = BlockGraph::new(BlockKey::from_raw("val:@program"));
        let mut a = block("val:a", BlockKind::Scope);
        a.body = Some(Body {
            children: vec![BlockKey::from_raw("val:a/#0")],
            wires: Vec::new(),
        });
        old.insert(a);

        let mut new = BlockGraph::new(BlockKey::from_raw("val:@program"));
        let mut a2 = block("val:a", BlockKind::Scope);
        a2.body = Some(Body {
            children: vec![
                BlockKey::from_raw("val:a/#0"),
                BlockKey::from_raw("val:a/#1"),
            ],
            wires: Vec::new(),
        });
        new.insert(a2);

        let d = diff(&old, &new);
        assert!(d.modified.is_empty());
    }
}
