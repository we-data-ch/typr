use serde::Serialize;
use crate::argument_type::ArgumentType;
use crate::argument_kind::ArgumentKind;
use crate::tag::Tag;

fn to_string<T: ToString>(v: &[T]) -> String {
    let res = v.iter()
        .map(|x| x.to_string())
        .reduce(|acc, x| format!("{}, {}", acc, x))
        .unwrap_or("".to_string());
    format!("[{}]", res)
}

type Path = String;

#[derive(Debug, Clone, PartialEq, Serialize, Eq, Hash)]
pub enum Type {
    Number,
    Integer,
    Boolean,
    Char,
    Embedded(Box<Type>),
    Function(Vec<ArgumentKind>, Vec<Type>, Box<Type>),
    Generic(String),
    IndexGen(String),
    Array(Box<Type>, Box<Type>),
    Record(Vec<ArgumentType>),
    Index(u32),
    Alias(String, Vec<Type>, Path),
    Tag(String, Box<Type>),
    Union(Vec<Tag>),
    Interface(Vec<ArgumentType>),
    Params(Vec<Type>),
    Add(Box<Type>, Box<Type>),
    Minus(Box<Type>, Box<Type>),
    Div(Box<Type>, Box<Type>),
    Mul(Box<Type>, Box<Type>),
    Failed(String),
    Opaque(String),
    Empty,
    Any
}

impl Type {
    pub fn get_name(self) -> String {
        match self {
            Type::Alias(name, _args, _path) => name.to_string(),
            _ => todo!()
        }
    }
}

use std::fmt;
impl fmt::Display for Type {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Type::Embedded(t) => format!("tembedded({})", t),
            Type::Alias(name, params, path) => format!("var('{}', '{}', public, false, params({}))", name, path, to_string(params)),
            Type::Function(k, v, t) => format!("tfn({}, {}, {})", to_string(k), to_string(v), t) ,
            Type::Generic(g) => format!("gen('{}')", g.to_lowercase()),
            Type::IndexGen(g) => format!("ind('{}')", g.to_lowercase()),
            Type::Array(n, t) => format!("tarray({}, {})", n, t),
            Type::Record(r) => format!("trecord({})", to_string(r)),
            Type::Index(i) => i.to_string(),
            Type::Number => "num".to_string(),
            Type::Integer => "int".to_string(),
            Type::Boolean => "bool".to_string(),
            Type::Char => "chars".to_string(),
            Type::Tag(s, t) => format!("ttag('{}', {})", s, t),
            Type::Union(v) => format!("union({})", to_string(v)),
            Type::Interface(v) => format!("interface({})", to_string(v)),
            Type::Params(v) => format!("params({})", to_string(v)),
            Type::Add(id1, id2) => format!("add({}, {})", id1, id2),
            Type::Minus(id1, id2) => format!("minus({}, {})", id1, id2),
            Type::Mul(id1, id2) => format!("mul({}, {})", id1, id2),
            Type::Div(id1, id2) => format!("division({}, {})", id1, id2),
            Type::Empty => "any".to_string(),
            _ => "".to_string()
        };
        write!(f, "{}", res)       
    }
}
