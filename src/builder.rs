#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::Type;
use crate::Lang;
use crate::help_data::HelpData;
use crate::tint::Tint;
use crate::argument_type::ArgumentType;
use crate::tchar::Tchar;
use std::collections::HashSet;

pub fn generic_type() -> Type {
        Type::Generic("T".to_string(), HelpData::default())
}

pub fn self_generic_type() -> Type {
        Type::Generic("Self".to_string(), HelpData::default())
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

pub fn character_type(s: &str) -> Type {
    Type::Char(Tchar::Val(s.to_string()), HelpData::default())
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
        .collect::<HashSet<_>>();
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
    Type::UnknownFunction(HelpData::default())
}

pub fn tuple_type(types: &[Type]) -> Type {
    Type::Tuple(types.to_vec(), HelpData::default())
}

pub fn array_type(i: Type, t: Type) -> Type {
    Type::Array(Box::new(i), Box::new(t), HelpData::default())
}

pub fn array_type2(i: i32, t: Type) -> Type {
    let i2 = integer_type(i);
    Type::Array(Box::new(i2), Box::new(t), HelpData::default())
}

pub fn opaque_type(name: &str) -> Type {
    Type::Opaque(name.to_string(), HelpData::default())
}

pub fn function_type(args: &[Type], return_type: Type) -> Type {
    Type::Function(args.to_vec(), 
                   Box::new(return_type),
                   HelpData::default())
}

pub fn interface_type(signatures: &[(&str, Type)]) -> Type {
    let args = signatures.iter()
        .cloned()
        .map(|(name, typ)| ArgumentType::from((name, typ)))
        .collect::<HashSet<_>>();
    Type::Interface(args, HelpData::default())
}

pub fn interface_type2(signatures: &[(String, Type)]) -> Type {
    let args = signatures.iter()
        .cloned()
        .map(|(name, typ)| ArgumentType::from((name, typ)))
        .collect::<HashSet<_>>();
    Type::Interface(args, HelpData::default())
}

pub fn intersection_type(types: &[Type]) -> Type {
    let type_set = types.iter().cloned().collect::<HashSet<_>>();
    Type::Intersection(type_set, HelpData::default())
}

pub fn union_type(types: &[Type]) -> Type {
    let type_set = types.iter().cloned().collect::<HashSet<_>>();
    Type::Union(type_set, HelpData::default())
}

pub fn unknown_function() -> Type {
    Type::UnknownFunction(HelpData::default())
}
