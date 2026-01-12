#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use std::fmt;
use crate::Lang;
use crate::Type;

pub enum ErrorMessage {
    UnificationMatch(Vec<Lang>, Vec<Type>),
    Unknown
}

impl fmt::Display for ErrorMessage {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            ErrorMessage::UnificationMatch(args, param_types)
                => format!("The given values don't match:\nexpected:{:?}\nrecieved: {:?}", args, param_types),
            _ => "Unknonw error".to_string(),
        };
        write!(f, "{}", res)       
    }
}
