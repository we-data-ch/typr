#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::Type;
use crate::Lang;
use crate::help_data::HelpData;


pub fn empty_type() -> Type {
    Type::Empty(HelpData::default())
}

pub fn empty_lang() -> Lang {
    Lang::Empty(HelpData::default())
}

pub fn any_type() -> Type {
    Type::Any(HelpData::default())
}
