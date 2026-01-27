use serde::Deserialize;
use serde::Serialize;
use std::fmt;

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone, Copy)]
pub enum VecType {
    Vector,
    Array
}

impl fmt::Display for VecType {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            VecType::Vector => "Vec",
            VecType::Array => "",
        };
        write!(f, "{}", res)       
    }
}

impl VecType {
    pub fn is_vector(&self) -> bool {
        match self {
            VecType::Vector => true,
            _ => false
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            VecType::Array => true,
            _ => false
        }
    }
}
