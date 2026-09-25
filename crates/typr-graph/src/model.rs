//! The block-graph data model (spec §3.2, §4, §5, §9). This is the external JSON contract
//! (CLI, WASM, playground, docs): field names mirror the spec's example verbatim, and any
//! incompatible change must bump [`FORMAT_VERSION`].

use crate::key::BlockKey;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

pub const FORMAT: &str = "typr-block-graph";
pub const FORMAT_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

/// Every block kind of the catalogue (spec §4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BlockKind {
    Program,
    Literal,
    Operator,
    Apply,
    Scope,
    Function,
    Record,
    Access,
    TypeDecl,
    TypeExpr,
    Interface,
    If,
    Tuple,
    Array,
    Opaque,
    Loop,
    Match,
    Module,
    RCode,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Origin {
    User,
    Std,
    RPackage(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Port {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#type: Option<String>,
    pub implicit: bool,
    pub visibility: Visibility,
}

impl Port {
    pub fn explicit(name: impl Into<String>, r#type: Option<String>) -> Self {
        Port {
            name: name.into(),
            r#type,
            implicit: false,
            visibility: Visibility::Public,
        }
    }

    pub fn implicit(name: impl Into<String>, r#type: Option<String>) -> Self {
        Port {
            name: name.into(),
            r#type,
            implicit: true,
            visibility: Visibility::Public,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PortRef {
    pub block: BlockKey,
    pub port: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Wire {
    pub from: PortRef,
    pub to: PortRef,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Body {
    pub children: Vec<BlockKey>,
    pub wires: Vec<Wire>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Block {
    pub key: BlockKey,
    pub kind: BlockKind,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<Span>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#type: Option<String>,
    pub inputs: Vec<Port>,
    pub outputs: Vec<Port>,
    pub origin: Origin,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<Body>,
}

/// Confidence of a by-name reference (spec §5.3) — a stand-in until a real name-resolution pass
/// exists (étape 7). `Exact` unifies "only one candidate" and "several candidates, but the type
/// of the first argument at the call site narrows it to one."
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "confidence", rename_all = "snake_case")]
pub enum Confidence {
    Exact,
    ByName,
    Ambiguous { candidates: Vec<BlockKey> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RelationKind {
    Ref,
    HasType,
    TypePosition,
    Satisfies,
    DeclaredAs,
    Subtype,
    Instantiates,
}

/// Why a `Satisfies`/`DeclaredAs` relation holds (spec §5.2) — the pedagogical payload: not just
/// *that* a type satisfies an interface, but *through which method*. `provided_by` is the block
/// supplying the required method: a free function's block when one was found by name, or the
/// `TypeDecl` itself when the requirement is met structurally (e.g. a record field read as a
/// trivial getter) rather than through a discovered function block.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Evidence {
    pub requires: String,
    pub provided_by: BlockKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Relation {
    pub kind: RelationKind,
    pub from: BlockKey,
    pub to: BlockKey,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub index: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub confidence: Option<Confidence>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub evidence: Option<Vec<Evidence>>,
}

impl Relation {
    pub fn r#ref(from: BlockKey, port: &str, to: BlockKey, confidence: Confidence) -> Self {
        Relation {
            kind: RelationKind::Ref,
            from,
            to,
            port: Some(port.to_string()),
            index: None,
            confidence: Some(confidence),
            evidence: None,
        }
    }

    pub fn has_type(from: BlockKey, to: BlockKey) -> Self {
        Relation {
            kind: RelationKind::HasType,
            from,
            to,
            port: None,
            index: None,
            confidence: None,
            evidence: None,
        }
    }

    pub fn type_position(from: BlockKey, to: BlockKey, index: usize) -> Self {
        Relation {
            kind: RelationKind::TypePosition,
            from,
            to,
            port: None,
            index: Some(index),
            confidence: None,
            evidence: None,
        }
    }

    /// `TypeDecl` → `Interface`, structurally computed (spec §5.1), with the evidence backing it.
    pub fn satisfies(from: BlockKey, to: BlockKey, evidence: Vec<Evidence>) -> Self {
        Relation {
            kind: RelationKind::Satisfies,
            from,
            to,
            port: None,
            index: None,
            confidence: None,
            evidence: Some(evidence),
        }
    }

    /// `TypeDecl` → `Interface`, for a `Record & Interface` intersection alias: the interface
    /// member named directly in the declaration, checked at declaration time (spec §5.1).
    pub fn declared_as(from: BlockKey, to: BlockKey) -> Self {
        Relation {
            kind: RelationKind::DeclaredAs,
            from,
            to,
            port: None,
            index: None,
            confidence: None,
            evidence: None,
        }
    }

    /// type → supertype (spec §5.1), from the structural subtyping relation between two declared
    /// type aliases.
    pub fn subtype(from: BlockKey, to: BlockKey) -> Self {
        Relation {
            kind: RelationKind::Subtype,
            from,
            to,
            port: None,
            index: None,
            confidence: None,
            evidence: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlockGraph {
    pub format: String,
    pub version: u32,
    pub root: BlockKey,
    pub blocks: BTreeMap<BlockKey, Block>,
    pub relations: Vec<Relation>,
}

impl BlockGraph {
    pub fn new(root: BlockKey) -> Self {
        BlockGraph {
            format: FORMAT.to_string(),
            version: FORMAT_VERSION,
            root,
            blocks: BTreeMap::new(),
            relations: Vec::new(),
        }
    }

    pub fn insert(&mut self, block: Block) {
        self.blocks.insert(block.key.clone(), block);
    }

    /// The one-level view centered on `focus` (spec §11's "un niveau rendu à la fois", spec §12
    /// étape 2's DOT export): `focus` itself plus its direct children, and every relation with at
    /// least one endpoint in that set. `None` if `focus` isn't a block of this graph.
    pub fn one_level(&self, focus: &BlockKey) -> Option<BlockGraph> {
        let block = self.blocks.get(focus)?;
        let mut out = BlockGraph::new(focus.clone());
        out.blocks.insert(focus.clone(), block.clone());

        let mut scope: BTreeSet<BlockKey> = BTreeSet::new();
        scope.insert(focus.clone());
        if let Some(body) = &block.body {
            for child in &body.children {
                if let Some(child_block) = self.blocks.get(child) {
                    out.blocks.insert(child.clone(), child_block.clone());
                    scope.insert(child.clone());
                }
            }
        }

        out.relations = self
            .relations
            .iter()
            .filter(|r| scope.contains(&r.from) || scope.contains(&r.to))
            .cloned()
            .collect();
        Some(out)
    }
}
