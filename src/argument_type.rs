use std::fmt;
use serde::Serialize;
use crate::r#type::Type;

#[derive(Debug, Clone, PartialEq, Serialize, Eq, Hash)] // 3 argument is for the embedding
pub struct ArgumentType(pub String, pub Type, pub bool);

impl ArgumentType {
    pub fn to_r(&self) -> String {
        self.0.clone()
    }
    pub fn new(name: &str, type_: &Type) -> Self {
        ArgumentType(name.to_string(), type_.clone(), false)
    }

    pub fn embedded(name: &str, type_: &Type) -> Self {
        ArgumentType(name.to_string(), type_.clone(), true)
    }
}

impl fmt::Display for ArgumentType {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[var('{}'),{}]", self.0, self.1)       
    }
}
