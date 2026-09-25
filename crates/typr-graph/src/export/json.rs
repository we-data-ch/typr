//! JSON export — the external contract of spec §9, verbatim: `serde_json` over [`BlockGraph`].

use crate::model::BlockGraph;

pub fn to_string_pretty(graph: &BlockGraph) -> serde_json::Result<String> {
    serde_json::to_string_pretty(graph)
}
