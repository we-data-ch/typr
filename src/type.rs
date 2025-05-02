#![allow(dead_code)]
use serde::Serialize;
use crate::argument_type::ArgumentType;
use crate::argument_kind::ArgumentKind;
use crate::tag::Tag;
use crate::context::generate_arg;
use crate::kind::Kind;
use std::collections::HashSet;
use crate::help_data::HelpData;

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
    Number(HelpData),
    Integer,
    Boolean,
    Char,
    Embedded(Box<Type>),
    Function(Vec<ArgumentKind>, Vec<Type>, Box<Type>),
    Generic(String),
    IndexGen(String),
    LabelGen(String),
    Label(String),
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
    Multi(Box<Type>),
    Tuple(Vec<Type>),
    If(Box<Type>, Vec<Type>),
    Condition(Box<Type>, Box<Type>, Box<Type>),
    In,
    Empty,
    Any
}

//main
impl Type {
    pub fn type_extraction(&self) -> Vec<Type> {
        match self {
            Type::Function(_, args, ret)
                => {
                    let mut sol = args.clone();
                    sol.push((**ret).clone());
                    sol.push(self.clone()); sol
                }
            Type::Union(tags) => {
               let mut sol = tags.iter().map(|tag| tag.to_type()).collect::<Vec<_>>();
               sol.push(self.clone()); sol
            },
            typ => vec![typ.clone()]
        }.iter()
        .map(|typ| typ.add_kinds_in_functions_if_not())
        .collect()
    }

    pub fn get_label(&self) -> String {
        match self {
            Type::Label(l) => l.to_string(),
            Type::LabelGen(l) => l.to_string(),
            _ => panic!("The type {} wasn't a label", self)
        }
    }

    fn add_kinds_in_functions_if_not(&self) -> Type {
        match self {
            Type::Function(_kinds, args, ret) => {
                let new_kinds = args.iter()
                    .chain([(**ret).clone()].iter())
                    .flat_map(|typ| typ.extract_generics())
                    .collect::<HashSet<_>>()
                    .iter()
                    .map(|typ| ArgumentKind::from((typ.clone(), typ.get_kind())))
                    .collect::<Vec<_>>();
                Type::Function(new_kinds, args.clone(), ret.clone())
            },
            typ => typ.clone()
        }
    }

    pub fn replace_function_types(self: Type, t1: Type, t2: Type) -> Type {
        match self {
            Type::Function(kinds, args, ret) => {
                let new_args = args.iter()
                    .map(|typ| if *typ == t1 { t2.clone() } else {t1.clone()})
                    .collect::<Vec<_>>();
                let new_ret = if *ret == t1 { t2 } else { *ret };
                Type::Function(kinds.clone(), new_args, Box::new(new_ret))
            },
            _ => self
        }
    }

    pub fn without_embeddings(self) -> Type {
        match self {
            Type::Record(args) => {
                let new_args = args.iter()
                    .map(|arg| arg.remove_embeddings())
                    .collect();
                Type::Record(new_args)
            },
            typ => typ
        }
    }

    pub fn to_typescript(&self) -> String {
       match self {
           Type::Boolean => "boolean".to_string(),
           Type::Integer => "number".to_string(),
           Type::Number(_) => "number".to_string(),
           Type::Char => "string".to_string(),
           Type::Record(body) => {
                let res = body.iter()
                    .map(|at| format!("{}: {}", at.get_argument(), at.get_type().to_typescript()))
                    .collect::<Vec<_>>().join(", ");
                format!("{{ {} }}", res)
           },
           Type::Array(_size, body) => format!("{}[]", body.to_typescript()),
           Type::IndexGen(id) => id.to_uppercase(),
           Type::Generic(val) => val.to_uppercase(), 
           Type::Index(val) => val.to_string(),
           Type::Function(kinds, args, ret) => {
               let res = args.iter()
                    .enumerate()
                    .map(|(i, typ)| format!("{}: {}", generate_arg(i), typ.to_typescript()))
                    .collect::<Vec<_>>().join(", ");
               if kinds.len() == 0 {
                   format!("({}) => {}", res, ret.to_typescript())
               } else {
                   let res2 = kinds.iter()
                       .map(|ak| ak.get_argument().to_typescript())
                       .collect::<Vec<_>>()
                       .join(", ");
                   format!("<{}>({}) => {}", res2, res, ret.to_typescript())
               }
           },
           Type::Tag(name, typ) => format!("{{ _type: '{}',  _body: {} }}", name, typ.to_typescript()),
           Type::Any => "null".to_string(),
           Type::Empty => "null".to_string(),
           Type::Add(_,_) => "T".to_string(),
           Type::Minus(_,_) => "T".to_string(),
           Type::Div(_,_) => "T".to_string(),
           Type::Mul(_,_) => "T".to_string(),
           _ => format!("the type: {} is not yet in to_typescript()", self)
       } 
    }

    pub fn to_assemblyscript(&self) -> String {
       match self {
           Type::Boolean => "bool".to_string(),
           Type::Integer => "i32".to_string(),
           Type::Number(_) => "f64".to_string(),
           Type::Char => "string".to_string(),
           Type::Record(body) => {
                let res = body.iter()
                    .map(|at| format!("{}: {}", at.get_argument(), at.get_type().to_typescript()))
                    .collect::<Vec<_>>().join(", ");
                format!("{{ {} }}", res)
           },
           Type::Array(_size, body) => format!("{}[]", body.to_typescript()),
           Type::IndexGen(id) => id.to_uppercase(),
           Type::Generic(val) => val.to_uppercase(), 
           Type::Index(val) => val.to_string(),
           Type::Function(kinds, args, ret) => {
               let res = args.iter()
                    .enumerate()
                    .map(|(i, typ)| format!("{}: {}", generate_arg(i), typ.to_typescript()))
                    .collect::<Vec<_>>().join(", ");
               if kinds.len() == 0 {
                   format!("({}) => {}", res, ret.to_typescript())
               } else {
                   let res2 = kinds.iter()
                       .map(|ak| ak.get_argument().to_typescript())
                       .collect::<Vec<_>>()
                       .join(", ");
                   format!("<{}>({}) => {}", res2, res, ret.to_typescript())
               }
           },
           Type::Tag(name, typ) => format!("{{ _type: '{}',  _body: {} }}", name, typ.to_typescript()),
           Type::Any => "null".to_string(),
           Type::Empty => "null".to_string(),
           Type::Add(_,_) => "T".to_string(),
           Type::Minus(_,_) => "T".to_string(),
           Type::Div(_,_) => "T".to_string(),
           Type::Mul(_,_) => "T".to_string(),
           _ => format!("the type: {} is not yet in to_typescript()", self)
       } 
    }

    pub fn extract_generics(&self) -> Vec<Type> {
        match self {
            Type::Generic(_) | Type::IndexGen(_) | Type::LabelGen(_)
                => vec![self.clone()],
            Type::Function(kinds, args, ret_typ) => {
                kinds.iter()
                    .map(|ak| ak.get_argument())
                    .chain(args.iter().cloned())
                    .chain([(**ret_typ).clone()].iter().cloned())
                    .collect::<HashSet<_>>()
                    .iter().flat_map(|typ| typ.extract_generics())
                    .collect::<Vec<_>>()
            }
            Type::Array(ind, typ) => {
               typ.extract_generics() 
                   .iter()
                   .chain(ind.extract_generics().iter())
                   .collect::<HashSet<_>>()
                   .into_iter().cloned().collect::<Vec<_>>()
            },
                Type::Record(v) => {
                   v.iter() 
                       .flat_map(|argt| 
                                 [argt.get_argument(), argt.get_type()])
                       .flat_map(|typ| typ.extract_generics())
                       .collect::<HashSet<_>>()
                       .into_iter().collect::<Vec<_>>()
                }
            _ => vec![]
        }
    }

    pub fn get_kind(&self) -> Kind {
        match self {
            Type::IndexGen(_) | Type::Index(_) => Kind::Dim,
            _ => Kind::Type
        }
    }

    pub fn index_calculation(&self) -> Type {
        match self {
            Type::Add(a, b) 
                => a.index_calculation().sum_index(&b.index_calculation()),
            Type::Minus(a, b) 
                => a.index_calculation().minus_index(&b.index_calculation()),
            Type::Mul(a, b) 
                => a.index_calculation().mul_index(&b.index_calculation()),
            Type::Div(a, b) 
                => a.index_calculation().div_index(&b.index_calculation()),
            Type::Array(ind, typ) =>
                Type::Array(
                    Box::new(ind.index_calculation()), 
                    Box::new(typ.index_calculation())),
                    Type::Function(_kinds, args, ret_typ) => {
                        let new_args = args.iter()
                            .map(|typ| typ.index_calculation())
                            .collect::<Vec<_>>();
                        let new_kinds = args.iter()
                            .flat_map(|typ| typ.extract_generics())
                            .collect::<HashSet<_>>().iter()
                            .map(|typ| ArgumentKind::from((typ.clone(), typ.get_kind())))
                            .collect::<Vec<_>>();
                        Type::Function(
                            new_kinds,
                            new_args,
                            Box::new(ret_typ.index_calculation()))
                    }
            _ => self.clone()
        }
    }

    fn sum_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Index(a), Type::Index(b)) => Type::Index(a+b),
            (Type::Record(a), Type::Record(b)) 
                => Type::Record(
                    a.iter()
                    .chain(b.iter())
                    .cloned()
                    .collect::<Vec<_>>()),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn minus_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Index(a), Type::Index(b)) => Type::Index(a-b),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn mul_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Index(a), Type::Index(b)) => Type::Index(a*b),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn div_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Index(a), Type::Index(b)) => Type::Index(a.div_euclid(*b)),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    pub fn get_shape(&self) -> Option<String> {
        if let Type::Array(i, t) = self {
            match (*i.clone(), t.get_shape()) {
                (Type::Index(j), Some(rest)) => Some(format!("{}, {}", j, rest)),
                (Type::Index(j), None) => Some(j.to_string()),
                _ => None
            }
        } else { None }
    }

    pub fn get_function_elements(&self) -> Option<(Vec<ArgumentKind>, Vec<Type>, Type)> {
        if let Type::Function(kinds, args, ret_ty) = self {
            Some((kinds.clone(), args.clone(), (**ret_ty).clone()))
        } else { None }
    }

    pub fn get_type_pattern(&self) -> Option<ArgumentType> {
        if let Type::Record(fields) = self {
            (fields.len() == 1)
                .then(|| fields[0].clone())
        } else {None}
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
            Type::Record(r) => {
                format!("{{ {} }}", r.iter()
                        .map(|at| at.to_string()).collect::<Vec<_>>().join(", "))
            },
            Type::Index(i) => i.to_string(),
            Type::Number(_) => "num".to_string(),
            Type::Integer => "int".to_string(),
            Type::Boolean => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Tag(s, t) => format!("ttag('{}', {})", s, t),
            Type::Union(v) => format!("union({})", to_string(v)),
            Type::Interface(v) => format!("interface({})", to_string(v)),
            Type::Params(v) => format!("params({})", to_string(v)),
            Type::Add(id1, id2) => format!("add({}, {})", id1, id2),
            Type::Minus(id1, id2) => format!("minus({}, {})", id1, id2),
            Type::Mul(id1, id2) => format!("mul({}, {})", id1, id2),
            Type::Div(id1, id2) => format!("division({}, {})", id1, id2),
            Type::LabelGen(l) => format!("%{}", l),
            Type::Label(l) => format!("{}", l),
            Type::Empty => "any".to_string(),
            _ => "".to_string()
        };
        write!(f, "{}", res)       
    }
}
