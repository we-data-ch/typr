use std::fmt;
use serde::Serialize;
use crate::Type;
use crate::kind::Kind;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Hash)]
pub struct ArgumentKind(pub Type, pub Kind);

impl ArgumentKind {
    pub fn get_kind(&self) -> Kind {
        self.1.clone()
    }
}

impl fmt::Display for ArgumentKind {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[gen('{}'),{}]", self.0, self.1)       
    }
}

impl From<(Type, Kind)> for  ArgumentKind {
   fn from(val: (Type, Kind)) -> Self {
        ArgumentKind(val.0, val.1)
   } 
}
