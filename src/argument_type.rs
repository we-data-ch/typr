use std::fmt;
use serde::Serialize;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq, Serialize)] // 3 argument is for the embedding
pub struct ArgumentType(pub String, pub Type, pub bool);

impl ArgumentType {
    pub fn to_r(&self) -> String {
        self.0.clone()
    }
}

impl fmt::Display for ArgumentType {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[var('{}'),{}]", self.0, self.1)       
    }
}
