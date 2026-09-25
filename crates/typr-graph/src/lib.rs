//! Builds a hierarchical block-graph view of a TypR program (`visualization_graph_v2.md`).
//!
//! Entry point: [`build`], which turns `Lang` + the `Context` produced by type-checking it + a
//! recorded [`TypeTable`] into a [`model::BlockGraph`] — a JSON-serializable, versioned external
//! contract (spec §9). Pure logic, no I/O, WASM-safe.

mod build;
pub mod key;
pub mod model;

pub use key::{BlockKey, Namespace};
pub use model::BlockGraph;

use typr_core::components::context::Context;
use typr_core::components::language::Lang;
use typr_core::processes::type_checking::type_recorder::TypeTable;

pub fn build(lang: &Lang, context: &Context, types: &TypeTable) -> BlockGraph {
    build::build(lang, context, types)
}
