use crate::parsing::operation_priority::TokenKind;
use serde::{Serialize, Deserialize};
use crate::help_data::HelpData;
use std::collections::HashSet;
use crate::Type;
use std::fmt;

#[derive(Debug, Default, Clone, Copy, PartialEq, Serialize, Deserialize, Eq)]
pub enum TypeOperator {
    Union,
    Intersection,
    Addition,
    Substraction,
    Multiplication,
    Division,
    Access,
    Arrow,
    #[default]
    Unknown,
}

impl TypeOperator {
    pub fn combine(self, exp1: Type, exp2: Type) -> Type {
        Type::Operator(self, Box::new(exp1.clone()), Box::new(exp2), exp1.get_help_data())
    }

    pub fn get_token_type(&self) -> TokenKind {
        TokenKind::Operator
    }

    pub fn get_binding_power(&self) -> i32 {
        match self {
            TypeOperator::Access | TypeOperator::Arrow => 3,
            TypeOperator::Addition | TypeOperator::Substraction |
            TypeOperator::Multiplication | TypeOperator::Division 
            => 2,
            _ => 1
        }
    }
}

impl fmt::Display for TypeOperator {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            TypeOperator::Arrow => "->",
            TypeOperator::Union => "|",
            TypeOperator::Intersection => "&",
            TypeOperator::Addition => "+",
            TypeOperator::Substraction => "-",
            TypeOperator::Multiplication => "*",
            TypeOperator::Division => "/",
            TypeOperator::Access => "$",
            TypeOperator::Unknown => "?",
        };
        write!(f, "{}", res)       
    }
}

impl From<TokenKind> for TypeOperator {
   fn from(_val: TokenKind) -> Self {
        TypeOperator::Unknown
   } 
}

impl TypeOperator {
    pub fn build_type(self, types: HashSet<Type>, help_data: HelpData) -> Type {
        match self {
            TypeOperator::Union => Type::Union(types, help_data),
            TypeOperator::Intersection => Type::Intersection(types, help_data),
            _ => panic!("We can't combine types with the empty type operator")
        }
    }
}

