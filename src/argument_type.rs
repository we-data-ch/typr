use std::fmt;
use serde::Serialize;
use crate::r#type::Type;

#[derive(Debug, Clone, PartialEq, Serialize, Eq, Hash)] // 3 argument is for the embedding
pub struct ArgumentType(pub Type, pub Type, pub bool);

impl ArgumentType {
    pub fn to_r(&self) -> String {
        self.0.to_string()
    }

    pub fn new(name: &str, type_: &Type) -> Self {
        ArgumentType(
            Type::Label(name.to_string()),
            type_.clone(), false)
    }

    pub fn get_type(&self) -> Type {
        self.1.clone()
    }

    pub fn get_argument(&self) -> Type {
        self.0.clone()
    }

    pub fn get_argument_str(&self) -> String {
        match self.0.clone() {
            Type::Label(l) => l.to_string(),
            Type::LabelGen(l) => l.to_string().to_uppercase(),
            Type::Multi(t) => ArgumentType(*t, self.1.clone(), false)
                .get_argument_str(),
            _ => panic!("The argument wasn't a label")
        }
    }

    pub fn remove_embeddings(&self) -> ArgumentType {
        ArgumentType(self.0.clone(), self.1.clone(), false)
    }

    pub fn is_embedded(&self) -> bool {
        self.2
    }
}

impl fmt::Display for ArgumentType {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.0, self.1)       
    }
}

impl From<(String, Type)> for ArgumentType {
   fn from(val: (String, Type)) -> Self {
        ArgumentType(
            Type::Label(val.0),
            val.1, false)
   } 
}
