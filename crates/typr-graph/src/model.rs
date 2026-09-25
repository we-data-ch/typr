//! The block-graph data model (spec §3.2, §4, §5, §9). This is the external JSON contract
//! (CLI, WASM, playground, docs): field names mirror the spec's example verbatim, and any
//! incompatible change must bump [`FORMAT_VERSION`].

use crate::key::BlockKey;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

pub const FORMAT: &str = "typr-block-graph";
pub const FORMAT_VERSION: u32 = 1;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

/// Every block kind of the catalogue (spec §4). Variants not yet built at this step (`Loop`,
/// `Match`, `Module`, `RCode` — étape 5) exist so the JSON contract is stable ahead of time, but
/// no constructor emits them yet: the `Lang` shapes that would need them fall back to `Opaque`.
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
}
