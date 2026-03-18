use serde::Deserialize;
use serde::Serialize;
use std::fmt;

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone, Copy, Hash)]
pub enum VecType {
    Vector,
    S3,
    Array,
    DataFrame,
    Unknown,
}

impl fmt::Display for VecType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            VecType::Vector => "Vec",
            VecType::S3 => "",
            VecType::Array => "Array",
            VecType::DataFrame => "DataFrame",
            VecType::Unknown => "Unknown",
        };
        write!(f, "{}", res)
    }
}

impl VecType {
    pub fn is_vector(&self) -> bool {
        match self {
            VecType::Vector => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            VecType::S3 => true,
            _ => false,
        }
    }
}

impl Default for VecType {
    fn default() -> Self {
        VecType::S3
    }
}
