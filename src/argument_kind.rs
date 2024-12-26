use std::fmt;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ArgumentKind(pub String, pub String);

impl fmt::Display for ArgumentKind {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[gen('{}'),{}]", self.0, self.1)       
    }
}
