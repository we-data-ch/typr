pub mod builder;
pub mod doc_attach;
pub mod edges;
pub mod model;
pub mod stdlib_meta;

pub use builder::build_spg;
pub use builder::build_spg_from_items;
pub use doc_attach::build_doc_map;
pub use edges::infer_edges;
pub use model::{Edge, EdgeKind, Node, NodeKind, NodePayload, SourceLoc, Spg, StdlibMeta, Visibility};
pub use stdlib_meta::{parse_meta_from_source, FunctionMeta};
