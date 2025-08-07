use crate::Type;
use crate::Var;
use crate::help_data::HelpData;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TypeCategory {
    Array,
    Function,
    Record,
    Tag,
    Union,
    Interface,
    Boolean,
    Integer,
    Number,
    Char,
    Generic,
    DataFrame,
    Rest
}

impl TypeCategory {
    pub fn to_variable(self) -> Var {
        Var::from_name(&format!("{}", self))
            .set_type(Type::Params(vec![], HelpData::default()))
    }

}

use std::fmt;
impl fmt::Display for TypeCategory {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            TypeCategory::Array => "Array",
            TypeCategory::Function => "Function",
            TypeCategory::Record => "Record",
            TypeCategory::Tag => "Tag",
            TypeCategory::Union => "Union",
            TypeCategory::Interface => "Interface",
            TypeCategory::Boolean => "logical",
            TypeCategory::Integer => "integer",
            TypeCategory::Number => "numeric",
            TypeCategory::DataFrame => "data.frame",
            TypeCategory::Char => "character",
            TypeCategory::Generic => "Generic",
            TypeCategory::Rest => "Rest"
        };
        write!(f, "{}", res)       
    }
}

