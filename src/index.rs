use crate::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Index(u32);

impl Index {
    pub fn from_type(typ: &Type) -> Option<Index> {
        match typ {
            Type::Index(id) => Some(Index(*id)),
            _ => None
        }
    }

    pub fn get_value(&self) -> u32 {
        self.0.clone()
    }
}
