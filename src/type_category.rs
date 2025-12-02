use crate::Type;
use crate::Var;
use crate::help_data::HelpData;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TypeCategory {
    Array,
    Function,
    Record,
    Tuple,
    Tag,
    Union,
    Interface,
    Boolean,
    Integer,
    Number,
    Char,
    Generic,
    DataFrame,
    Alias,
    Any,
    Empty,
    RClass,
    RFunction,
    Opaque(String),
    Template,
    Vector,
    Sequence,
    Rest,
    Intersection,
    Module,
    Operator,
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
            TypeCategory::Operator => "Operator",
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
            TypeCategory::Alias => "Alias",
            TypeCategory::Any => "Any",
            TypeCategory::Empty => "Empty",
            TypeCategory::RClass => "RClass",
            TypeCategory::RFunction => "RFunction",
            TypeCategory::Tuple => "Tuple",
            TypeCategory::Opaque(name) => &name.to_string(),
            TypeCategory::Template => "Template",
            TypeCategory::Vector => "Vector",
            TypeCategory::Sequence => "Sequence",
            TypeCategory::Rest => "Rest",
            TypeCategory::Intersection => "Intersection",
            TypeCategory::Module => "Module",
        };
        write!(f, "{}", res)       
    }
}

