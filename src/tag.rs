use crate::Type;
use serde::Serialize;
use crate::Lang;
use crate::context::Context;
use crate::type_checker::typing;

type Name = String;

#[derive(Debug, Clone, PartialEq, Serialize, Eq, Hash)]
pub struct Tag(Name, Type);

impl Tag {
    pub fn new(name: String, typ: Type) -> Tag {
        Tag(name, typ)
    }

    pub fn from_type(typ: Type) -> Option<Tag> {
        match typ {
            Type::Tag(name, typ2) => 
                Some(Tag(name.to_string(), (*typ2).clone())),
            _ => None
        }
    }

    pub fn from_language(lang: Lang, context: &Context) -> Option<Tag> {
        match lang {
            Lang::Tag(name, typ) => Some(Tag(name, typing(context, &(*typ)))),
            _ => None
        }
    }

    pub fn to_type(&self) -> Type {
        Type::Tag(self.0.clone(), Box::new(self.1.clone()))
    }

    pub fn get_name(&self) -> String {
        self.0.clone()
    }

    pub fn get_type(&self) -> Type {
        self.1.clone()
    }
}

use std::fmt;
impl fmt::Display for Tag {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ":{}({})", self.0, self.1)       
    }
}
