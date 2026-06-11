use serde::Deserialize;
use serde::Serialize;
use std::fmt;

/// Category of a declared `typeconstructor`.
/// - `Recursive`: homogeneous, indexed types (`Vec[N, T]`, `Array[N, T]`, `Matrix[...]`).
/// - `Record`: heterogeneous, named-field types (`Df[N]{...}`, `Tibble[N]{...}`).
#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone, Copy, Hash)]
pub enum ConstructorCategory {
    Recursive,
    Record,
}

impl fmt::Display for ConstructorCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            ConstructorCategory::Recursive => "recursive",
            ConstructorCategory::Record => "record",
        };
        write!(f, "{}", res)
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone, Hash, Default)]
pub enum VecType {
    Vector,
    #[default]
    S3,
    Array,
    DataFrame,
    Empty,
    /// User-declared record constructor (e.g. `Tibble`, `Table`), keeping its name
    /// so it can be transpiled to a distinct R S3 class.
    Named(String),
}

impl fmt::Display for VecType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            VecType::Vector => "Vec",
            VecType::S3 => "",
            VecType::Array => "Array",
            VecType::DataFrame => "DataFrame",
            VecType::Empty => "Empty",
            VecType::Named(name) => name,
        };
        write!(f, "{}", res)
    }
}

impl VecType {
    pub fn is_empty(&self) -> bool {
        matches!(self, VecType::Empty)
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, VecType::Vector)
    }

    pub fn is_array(&self) -> bool {
        matches!(self, VecType::S3)
    }
}
