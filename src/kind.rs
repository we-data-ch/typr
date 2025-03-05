use serde::Serialize;

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
pub enum Kind {
    Type,
    Dim
}

use std::fmt;
impl fmt::Display for Kind {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = match self {
            Kind::Type => "k_type",
            _ => "k_index"
        };
        write!(f, "{}", kind)       
    }
}
