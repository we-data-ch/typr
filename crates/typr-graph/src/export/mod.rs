//! Serializations of a [`crate::model::BlockGraph`] (spec §10): `json` for the external contract
//! (§9), `dot` for a one-level Graphviz view (§12 étape 2).

pub mod dot;
pub mod json;
