#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::Type;
use crate::Lang;
use crate::help_data::HelpData;
use crate::tint::Tint;


pub fn empty_type() -> Type {
    Type::Empty(HelpData::default())
}

pub fn empty_lang() -> Lang {
    Lang::Empty(HelpData::default())
}

pub fn any_type() -> Type {
    Type::Any(HelpData::default())
}

pub fn integer(i: i32) -> Type {
    Type::Integer(Tint::Val(i), HelpData::default())
}
