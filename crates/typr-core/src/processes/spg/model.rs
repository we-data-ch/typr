#![allow(dead_code)]

use serde::{Deserialize, Serialize};

/// Source location for an SPG node (file + byte offset + 1-based line).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SourceLoc {
    pub file: String,
    pub offset: usize,
    pub line: u32,
}

impl SourceLoc {
    /// Compute line number by counting '\n' in the source up to `offset`.
    pub fn from_offset(file: impl Into<String>, offset: usize, source: &str) -> Self {
        let line = source[..offset.min(source.len())]
            .chars()
            .filter(|&c| c == '\n')
            .count() as u32
            + 1;
        SourceLoc {
            file: file.into(),
            offset,
            line,
        }
    }
}

/// Coarse kind of a documented entity.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum NodeKind {
    Function,
    Alias,
    TypeDef,
    Module,
    Variable,
}

impl NodeKind {
    pub fn prefix(&self) -> &'static str {
        match self {
            NodeKind::Function => "function",
            NodeKind::Alias => "alias",
            NodeKind::TypeDef => "type",
            NodeKind::Module => "module",
            NodeKind::Variable => "variable",
        }
    }
}

/// Visibility of a binding.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Visibility {
    Private,
    Public,
    Export,
}

/// Per-kind extra data carried by a node.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum NodePayload {
    Function {
        /// (param_name, type_str) pairs, in declaration order.
        params: Vec<(String, String)>,
        returns: String,
    },
    Record {
        /// (field_name, type_str) pairs, sorted alphabetically for snapshot stability.
        fields: Vec<(String, String)>,
    },
    Alias {
        underlying: String,
        opaque: bool,
    },
    Union {
        variants: Vec<(String, Option<String>)>,
    },
    Module {
        exports: Vec<String>,
    },
    /// A plain (non-function) `let` binding with an explicit type annotation —
    /// e.g. `@export let PI: num <- 3.14159;`. Un-annotated bindings carry no
    /// static type on the AST alone (see `builder.rs`'s `Lang::Let` arm), so
    /// they are not represented here; this mirrors the same annotated-only
    /// scope `--checked` uses for the same reason (soundness_transpilation.md
    /// Phase A).
    Variable {
        type_str: String,
    },
    None,
}

/// Structured stdlib metadata extracted from `#!` annotations in `.ty` files.
///
/// Carried on `Node::meta` when the node originates from the stdlib catalogue.
/// All fields are optional — partial metadata is valid (e.g. a function with
/// only `tier` and `param` descriptions but no examples).
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StdlibMeta {
    /// Confidence tier: `T1` (compiler, high confidence), `T2` (MCP/doc,
    /// medium confidence), `T3` (doc only, not type-safe for compiler).
    pub tier: Option<String>,
    /// Per-parameter descriptions: `(param_name, description)`.
    pub param_docs: Vec<(String, String)>,
    /// Return-value description.
    pub ret_doc: Option<String>,
    /// Coercion / silent-cast notes (e.g. "logical -> num par R").
    pub coercion_notes: Option<String>,
    /// Example code blocks (each a valid `typr check` candidate).
    pub examples: Vec<String>,
    /// Related function names (comma-separated in source, split into Vec).
    pub seealso: Vec<String>,
    /// The R package of origin (e.g. "base", "stats").
    pub pkg: Option<String>,
}

impl StdlibMeta {
    pub fn empty() -> Self {
        StdlibMeta {
            tier: None,
            param_docs: Vec::new(),
            ret_doc: None,
            coercion_notes: None,
            examples: Vec::new(),
            seealso: Vec::new(),
            pkg: None,
        }
    }

    /// Returns true if this metadata carries any non-empty field.
    pub fn is_non_empty(&self) -> bool {
        self.tier.is_some()
            || !self.param_docs.is_empty()
            || self.ret_doc.is_some()
            || self.coercion_notes.is_some()
            || !self.examples.is_empty()
            || !self.seealso.is_empty()
            || self.pkg.is_some()
    }
}

/// A node in the Semantic Package Graph.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Node {
    /// Stable deterministic id: `<kind>:<module_path>/<name>`
    pub id: String,
    pub kind: NodeKind,
    pub name: String,
    /// Nesting path of module names (empty for top-level).
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    /// Doc-comment attached by `doc_attach.rs` (Phase 4). `None` until then.
    pub doc: Option<String>,
    pub source: Option<SourceLoc>,
    pub payload: NodePayload,
    /// Structured stdlib metadata from `#!` annotations. `None` for
    /// non-stdlib nodes or when no annotations were present.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub meta: Option<StdlibMeta>,
}

impl Node {
    /// Build the canonical `id` from kind, module path, and name.
    pub fn make_id(kind: &NodeKind, module_path: &[String], name: &str) -> String {
        if module_path.is_empty() {
            format!("{}:{}", kind.prefix(), name)
        } else {
            format!("{}:{}/{}", kind.prefix(), module_path.join("/"), name)
        }
    }
}

/// Kind of a directed edge in the graph.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum EdgeKind {
    HasField,
    Returns,
    ProducesType,
    ParameterOf,
    ConsumesType,
    BelongsToModule,
    Uses,
    UsedBy,
}

/// A directed edge between two nodes.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Edge {
    pub from: String,
    pub to: String,
    pub kind: EdgeKind,
}

/// Root of the Semantic Package Graph (serialises to `spg.json`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Spg {
    /// JSON-LD `@context` — placeholder until a proper vocabulary is published.
    #[serde(rename = "@context")]
    pub context: String,
    pub package: String,
    pub version: String,
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
}

impl Spg {
    pub fn new(package: impl Into<String>, version: impl Into<String>) -> Self {
        Spg {
            context: "https://typr-lang.dev/spg/v1/context.jsonld".into(),
            package: package.into(),
            version: version.into(),
            nodes: Vec::new(),
            edges: Vec::new(),
        }
    }

    pub fn add_node(&mut self, node: Node) {
        self.nodes.push(node);
    }

    pub fn add_edge(&mut self, edge: Edge) {
        self.edges.push(edge);
    }
}
