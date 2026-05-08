use serde::{Deserialize, Serialize};

/// A single item in a use selector: `name` or `name as alias`
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct UseItem {
    pub name: String,
    pub alias: Option<String>,
}

/// The selector part of a use directive
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum UseSelector {
    /// use M::*;  — import all public symbols
    Wildcard,
    /// use M::{a, b as c};  — selective import with optional renaming
    Items(Vec<UseItem>),
}
