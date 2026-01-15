#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::r#type::Type;
use crate::components::error_message::help_data::HelpData;

#[derive(Debug, PartialEq, Clone)]
pub struct Index(u32, HelpData);

impl Index {
    pub fn from_type(typ: &Type) -> Option<Index> {
        match typ {
            Type::Integer(id, h) => Some(Index((*id).into(), h.clone())),
            _ => None
        }
    }

    pub fn get_value(&self) -> u32 {
        self.0.clone()
    }
}
