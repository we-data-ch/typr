#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]

use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::TypeError;
use crate::components::language::operators::Op;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::TypRError;
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
    let args = params
        .iter()
        .map(|param| ArgumentType::from(param.to_owned()))
        .collect::<HashSet<_>>();
    Type::Record(args, HelpData::default())
}

pub fn params_type() -> Type {
    Type::Params(vec![], HelpData::default())
}

pub fn generic_function(s: &str) -> Lang {
    let body = format!("{} <- function(x, ...) {{ UseMethod('{}') }}", s, s);
    Lang::GenFunc {
        name: body,
        help_data: HelpData::default(),
    }
}

pub fn tuple_type(types: &[Type]) -> Type {
    Type::Tuple(types.to_vec(), HelpData::default())
}

pub fn array_type(i: Type, t: Type) -> Type {
    Type::Vec(VecType::S3, Box::new(i), Box::new(t), HelpData::default())
}

pub fn array_type2(i: i32, t: Type) -> Type {
    let i2 = integer_type(i);
    Type::Vec(VecType::S3, Box::new(i2), Box::new(t), HelpData::default())
}

pub fn dataframe_type(i: Type, columns: &[(String, Type)]) -> Type {
    let fields = columns
        .iter()
        .map(|param| ArgumentType::from(param.to_owned()))
        .collect::<HashSet<_>>();
    Type::Vec(
        VecType::DataFrame,
        Box::new(i),
        Box::new(Type::Record(fields, HelpData::default())),
        HelpData::default(),
    )
}

pub fn opaque_type(name: &str) -> Type {
    Type::Opaque(name.to_string(), HelpData::default())
}

pub fn function_type(args: &[Type], return_type: Type) -> Type {
    let arg_types: Vec<ArgumentType> = args
        .iter()
        .enumerate()
        .map(|(i, typ)| {
            let arg_name = crate::components::r#type::generate_arg(i);
            ArgumentType::new(&arg_name, typ)
        })
        .collect();
    Type::Function(arg_types, Box::new(return_type), HelpData::default())
}

pub fn interface_type(signatures: &[(&str, Type)]) -> Type {
    let args = signatures
        .iter()
        .cloned()
        .map(|(name, typ)| ArgumentType::from((name, typ)))
        .collect::<HashSet<_>>();
    Type::Interface(args, HelpData::default())
}

pub fn interface_type2(signatures: &[(String, Type)]) -> Type {
    let args = signatures
        .iter()
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
    types
        .iter()
        .cloned()
        .reduce(|acc, t| {
            Type::Operator(
                TypeOperator::Union,
                Box::new(acc),
                Box::new(t),
                HelpData::default(),
            )
        })
        .unwrap_or(Type::Empty(HelpData::default()))
}

pub fn unknown_function_type() -> Type {
    Type::UnknownFunction(HelpData::default())
}

pub fn operation(operator: Op, left: Lang, right: Lang) -> Lang {
    Lang::Operator {
        operator,
        rhs: Box::new(left),
        lhs: Box::new(right),
        help_data: HelpData::default(),
    }
}

pub fn let_var(name: &str, typ: Type) -> (Var, Type) {
    (Var::from(name).set_type(typ.clone()), typ)
}

pub fn null_type() -> Type {
    Type::Null(HelpData::default())
}

pub fn na_type() -> Type {
    Type::NA(HelpData::default())
}

pub fn null_lang() -> Lang {
    Lang::Null(HelpData::default())
}

pub fn unmatching_return_type(typ1: &Type, typ2: &Type) -> TypRError {
    TypRError::Type(TypeError::UnmatchingReturnType(typ1.clone(), typ2.clone()))
}
