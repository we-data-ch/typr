#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]

pub mod help_data;
pub mod help_message;
pub mod locatable;
pub mod message_template;
pub mod syntax_error;
pub mod type_error;
pub mod typr_error;

use crate::components::error_message::message_template::MsgTemplate;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use std::fmt;

pub enum ErrorMessage {
    UnificationMatch(Vec<Lang>, Vec<Type>),
    Unknown,
}

impl fmt::Display for ErrorMessage {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            ErrorMessage::UnificationMatch(args, param_types) => format!(
                "The given values don't match:\nexpected:{:?}\nrecieved: {:?}",
                args, param_types
            ),
            _ => "Unknonw error".to_string(),
        };
        write!(f, "{}", res)
    }
}
