use serde::Deserialize;
use serde::Serialize;
use std::fmt;

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone, Copy, Hash, Default)]
pub enum VecType {
    Vector,
    #[default]
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
        matches!(self, VecType::Vector)
    }

    pub fn is_array(&self) -> bool {
        matches!(self, VecType::S3)
    }
}
