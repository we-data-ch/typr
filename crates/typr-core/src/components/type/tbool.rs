use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Tbool {
    Val(bool),
    Unknown,
}

impl Tbool {
    pub fn is_subtype(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Tbool::Unknown) => true,
            (Tbool::Val(v1), Tbool::Val(v2)) => v1 == v2,
            _ => false,
        }
    }

    pub fn get_val(&self) -> Option<bool> {
        match self {
            Tbool::Val(v) => Some(*v),
            _ => None,
        }
    }
}

impl std::fmt::Display for Tbool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tbool::Val(true) => write!(f, "true"),
            Tbool::Val(false) => write!(f, "false"),
            Tbool::Unknown => write!(f, "bool"),
        }
    }
}
