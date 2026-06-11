use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Tnum {
    Val(f64),
    Unknown,
}

impl Tnum {
    pub fn is_subtype(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Tnum::Unknown) => true,
            (Tnum::Val(v1), Tnum::Val(v2)) => v1 == v2,
            _ => false,
        }
    }

    pub fn get_val(&self) -> Option<f64> {
        match self {
            Tnum::Val(v) => Some(*v),
            _ => None,
        }
    }
}

impl Eq for Tnum {}

impl std::hash::Hash for Tnum {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Tnum::Val(v) => {
                0u8.hash(state);
                v.to_bits().hash(state);
            }
            Tnum::Unknown => 1u8.hash(state),
        }
    }
}

impl std::fmt::Display for Tnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tnum::Val(v) => write!(f, "{}", v),
            Tnum::Unknown => write!(f, "num"),
        }
    }
}
