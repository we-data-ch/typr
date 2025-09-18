#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::Type;
use crate::Lang;
use crate::help_data::HelpData;
use crate::tint::Tint;
use crate::argument_type::ArgumentType;
use crate::tchar::Tchar;

pub fn generic_type() -> Type {
        Type::Generic("T".to_string(), HelpData::default())
}

pub fn empty_type() -> Type {
    Type::Empty(HelpData::default())
}

pub fn empty_lang() -> Lang {
    Lang::Empty(HelpData::default())
}

pub fn any_type() -> Type {
    Type::Any(HelpData::default())
}

pub fn integer_type(i: i32) -> Type {
    Type::Integer(Tint::Val(i), HelpData::default())
}

pub fn integer_type_default() -> Type {
    Type::Integer(Tint::Unknown, HelpData::default())
}

pub fn character_type_default() -> Type {
    Type::Char(Tchar::Unknown, HelpData::default())
}

pub fn number_type() -> Type {
    Type::Number(HelpData::default())
}

pub fn boolean_type() -> Type {
    Type::Boolean(HelpData::default())
}

pub fn record_type(params: &[(String, Type)]) -> Type {
    let args = params.iter()
        .map(|param| ArgumentType::from(param.to_owned()))
        .collect::<Vec<_>>();
    Type::Record(args, HelpData::default())
}

pub fn params_type() -> Type {
    Type::Params(vec![], HelpData::default())
}

pub fn generic_function(s: &str) -> Lang {
    let body = format!("{} <- function(x, ...) {{ UseMethod('{}') }}", s, s);
    Lang::GenFunc(body, "".to_string(), HelpData::default())
}

pub fn r_function_type() -> Type {
    Type::RFunction(HelpData::default())
}

pub fn tuple_type(types: &[Type]) -> Type {
    Type::Tuple(types.to_vec(), HelpData::default())
}

pub fn array_type(i: Type, t: Type) -> Type {
    Type::Array(Box::new(i), Box::new(t), HelpData::default())
}

