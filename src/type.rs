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

#[derive(Debug, Clone, Serialize, Eq, Hash)]
pub enum Type {
    Number(HelpData),
    Integer(HelpData),
    Boolean(HelpData),
    Char(HelpData),
    Embedded(Box<Type>, HelpData),
    Function(Vec<ArgumentKind>, Vec<Type>, Box<Type>, HelpData),
    Generic(String, HelpData),
    IndexGen(String, HelpData),
    LabelGen(String, HelpData),
    Label(String, HelpData),
    Array(Box<Type>, Box<Type>, HelpData),
    Record(Vec<ArgumentType>, HelpData),
    Index(u32, HelpData),
    Alias(String, Vec<Type>, Path, HelpData),
    Tag(String, Box<Type>, HelpData),
    Union(Vec<Tag>, HelpData),
    Interface(Vec<ArgumentType>, HelpData),
    Params(Vec<Type>, HelpData),
    Add(Box<Type>, Box<Type>, HelpData),
    Mul(Box<Type>, Box<Type>, HelpData),
    Minus(Box<Type>, Box<Type>, HelpData),
    Div(Box<Type>, Box<Type>, HelpData),
    Failed(String, HelpData),
    Opaque(String, HelpData),
    Multi(Box<Type>, HelpData),
    Tuple(Vec<Type>, HelpData),
    If(Box<Type>, Vec<Type>, HelpData),
    Condition(Box<Type>, Box<Type>, Box<Type>, HelpData),
    In(HelpData),
    Empty(HelpData),
    Any(HelpData)
}

//main
impl Type {
    pub fn type_extraction(&self) -> Vec<Type> {
        match self {
            Type::Function(_, args, ret, _)
                => {
                    let mut sol = args.clone();
                    sol.push((**ret).clone());
                    sol.push(self.clone()); sol
                }
            Type::Union(tags, _) => {
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
            Type::Label(l, _) => l.to_string(),
            Type::LabelGen(l, _) => l.to_string(),
            _ => panic!("The type {} wasn't a label", self)
        }
    }

    fn add_kinds_in_functions_if_not(&self) -> Type {
        match self {
            Type::Function(_kinds, args, ret, h) => {
                let new_kinds = args.iter()
                    .chain([(**ret).clone()].iter())
                    .flat_map(|typ| typ.extract_generics())
                    .collect::<HashSet<_>>()
                    .iter()
                    .map(|typ| ArgumentKind::from((typ.clone(), typ.get_kind())))
                    .collect::<Vec<_>>();
                Type::Function(new_kinds, args.clone(), ret.clone(), h.clone())
            },
            typ => typ.clone()
        }
    }

    pub fn replace_function_types(self: Type, t1: Type, t2: Type) -> Type {
        match self {
            Type::Function(kinds, args, ret, h) => {
                let new_args = args.iter()
                    .map(|typ| if *typ == t1 { t2.clone() } else {t1.clone()})
                    .collect::<Vec<_>>();
                let new_ret = if *ret == t1 { t2 } else { *ret };
                Type::Function(kinds.clone(), new_args, Box::new(new_ret), h)
            },
            _ => self
        }
    }

    pub fn without_embeddings(self) -> Type {
        match self {
            Type::Record(args, h) => {
                let new_args = args.iter()
                    .map(|arg| arg.remove_embeddings())
                    .collect();
                Type::Record(new_args, h.clone())
            },
            typ => typ
        }
    }

    pub fn to_typescript(&self) -> String {
       match self {
           Type::Boolean(_) => "boolean".to_string(),
           Type::Integer(_) => "number".to_string(),
           Type::Number(_) => "number".to_string(),
           Type::Char(_) => "string".to_string(),
           Type::Record(body, _) => {
                let res = body.iter()
                    .map(|at| format!("{}: {}", at.get_argument(), at.get_type().to_typescript()))
                    .collect::<Vec<_>>().join(", ");
                format!("{{ {} }}", res)
           },
           Type::Array(_size, body, _) => format!("{}[]", body.to_typescript()),
           Type::IndexGen(id, _) => id.to_uppercase(),
           Type::Generic(val, _) => val.to_uppercase(), 
           Type::Index(val, _) => val.to_string(),
           Type::Function(kinds, args, ret, _) => {
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
           Type::Tag(name, typ, _) 
               => format!("{{ _type: '{}',  _body: {} }}", name, typ.to_typescript()),
           Type::Any(_) => "null".to_string(),
           Type::Empty(_) => "null".to_string(),
           Type::Add(_,_, _) => "T".to_string(),
           Type::Minus(_,_, _) => "T".to_string(),
           Type::Div(_,_, _) => "T".to_string(),
           Type::Mul(_,_, _) => "T".to_string(),
           _ => format!("the type: {} is not yet in to_typescript()", self)
       } 
    }

    pub fn to_assemblyscript(&self) -> String {
       match self {
           Type::Boolean(_) => "bool".to_string(),
           Type::Integer(_) => "i32".to_string(),
           Type::Number(_) => "f64".to_string(),
           Type::Char(_) => "string".to_string(),
           Type::Record(body, _) => {
                let res = body.iter()
                    .map(|at| format!("{}: {}", at.get_argument(), at.get_type().to_typescript()))
                    .collect::<Vec<_>>().join(", ");
                format!("{{ {} }}", res)
           },
           Type::Array(_size, body, _) => format!("{}[]", body.to_typescript()),
           Type::IndexGen(id, _) => id.to_uppercase(),
           Type::Generic(val, _) => val.to_uppercase(), 
           Type::Index(val, _) => val.to_string(),
           Type::Function(kinds, args, ret, _) => {
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
           Type::Tag(name, typ, _) => format!("{{ _type: '{}',  _body: {} }}", name, typ.to_typescript()),
           Type::Any(_) => "null".to_string(),
           Type::Empty(_) => "null".to_string(),
           Type::Add(_,_, _) => "T".to_string(),
           Type::Minus(_,_, _) => "T".to_string(),
           Type::Div(_,_, _) => "T".to_string(),
           Type::Mul(_,_, _) => "T".to_string(),
           _ => format!("the type: {} is not yet in to_typescript()", self)
       } 
    }

    pub fn extract_generics(&self) -> Vec<Type> {
        match self {
            Type::Generic(_, _) | Type::IndexGen(_, _) | Type::LabelGen(_, _)
                => vec![self.clone()],
            Type::Function(kinds, args, ret_typ, _) => {
                kinds.iter()
                    .map(|ak| ak.get_argument())
                    .chain(args.iter().cloned())
                    .chain([(**ret_typ).clone()].iter().cloned())
                    .collect::<HashSet<_>>()
                    .iter().flat_map(|typ| typ.extract_generics())
                    .collect::<Vec<_>>()
            }
            Type::Array(ind, typ, _) => {
               typ.extract_generics() 
                   .iter()
                   .chain(ind.extract_generics().iter())
                   .collect::<HashSet<_>>()
                   .into_iter().cloned().collect::<Vec<_>>()
            },
                Type::Record(v, _) => {
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
            Type::IndexGen(_, _) | Type::Index(_, _) => Kind::Dim,
            _ => Kind::Type
        }
    }

    pub fn index_calculation(&self) -> Type {
        match self {
            Type::Add(a, b, _) 
                => a.index_calculation().sum_index(&b.index_calculation()),
            Type::Minus(a, b, _) 
                => a.index_calculation().minus_index(&b.index_calculation()),
            Type::Mul(a, b, _) 
                => a.index_calculation().mul_index(&b.index_calculation()),
            Type::Div(a, b, _) 
                => a.index_calculation().div_index(&b.index_calculation()),
            Type::Array(ind, typ, h) =>
                Type::Array(
                    Box::new(ind.index_calculation()), 
                    Box::new(typ.index_calculation()),
                    h.clone()),
                    Type::Function(_kinds, args, ret_typ, h) => {
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
                            Box::new(ret_typ.index_calculation()), h.clone())
                    }
            _ => self.clone()
        }
    }

    fn sum_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Index(a, h), Type::Index(b, _)) => Type::Index(a+b, h.clone()),
            (Type::Record(a, h), Type::Record(b, _)) 
                => Type::Record(
                    a.iter()
                    .chain(b.iter())
                    .cloned()
                    .collect::<Vec<_>>(), h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn minus_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Index(a, h), Type::Index(b, _)) => Type::Index(a-b, h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn mul_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Index(a, h), Type::Index(b, _)) => Type::Index(a*b, h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn div_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Index(a, h), Type::Index(b, _)) => Type::Index(a.div_euclid(*b), h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    pub fn get_shape(&self) -> Option<String> {
        if let Type::Array(i, t, _) = self {
            match (*i.clone(), t.get_shape()) {
                (Type::Index(j, _), Some(rest)) => Some(format!("{}, {}", j, rest)),
                (Type::Index(j, _), None) => Some(j.to_string()),
                _ => None
            }
        } else { None }
    }

    pub fn get_function_elements(&self) -> Option<(Vec<ArgumentKind>, Vec<Type>, Type)> {
        if let Type::Function(kinds, args, ret_ty, _) = self {
            Some((kinds.clone(), args.clone(), (**ret_ty).clone()))
        } else { None }
    }

    pub fn get_type_pattern(&self) -> Option<ArgumentType> {
        if let Type::Record(fields, _) = self {
            (fields.len() == 1)
                .then(|| fields[0].clone())
        } else {None}
    }

    pub fn is_boolean(&self) -> bool {
        if let Type::Boolean(_) = self {
            true
        } else { false }
    }

    pub fn dependent_type(&self, dep_typ: &Type) -> bool {
        match (dep_typ, self) {
            (Type::Integer(_), Type::IndexGen(_, _)) => true,
            (Type::Char(_), Type::LabelGen(_, _)) => true,
            _ => false
        }
    }

}


impl From<Vec<Type>> for HelpData {
   fn from(val: Vec<Type>) -> Self {
        if val.len() > 0 {
            val[0].clone().into()
        } else { HelpData::default() }
   } 
}

impl From<Type> for HelpData {
   fn from(val: Type) -> Self {
       match val {
           Type::Empty(h) => h,
           Type::Generic(_, h) => h,
           Type::IndexGen(_, h) => h,
           Type::Minus(_, _, h) => h,
           Type::Add(_, _, h) => h,
           Type::Mul(_, _, h) => h,
           Type::Div(_, _, h) => h,
           Type::Tag(_, _, h) => h,
           Type::Function(_, _, _, h) => h,
           Type::Char(h) => h,
           Type::Integer(h) => h,
           Type::Record(_, h) => h,
           e => panic!("The type element {:?} is not yet implemented", e)
       }.clone()
   } 
}

use std::fmt;
impl fmt::Display for Type {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Type::Embedded(t, _) => format!("tembedded({})", t),
            Type::Alias(name, params, path, _) => format!("var('{}', '{}', public, false, params({}))", name, path, to_string(params)),
            Type::Function(k, v, t, _) => format!("tfn({}, {}, {})", to_string(k), to_string(v), t) ,
            Type::Generic(g, _) => format!("gen('{}')", g.to_lowercase()),
            Type::IndexGen(g, _) => format!("ind('{}')", g.to_lowercase()),
            Type::Array(n, t, _) => format!("tarray({}, {})", n, t),
            Type::Record(r, _) => {
                format!("{{ {} }}", r.iter()
                        .map(|at| at.to_string()).collect::<Vec<_>>().join(", "))
            },
            Type::Index(i, _) => i.to_string(),
            Type::Number(_) => "num".to_string(),
            Type::Integer(_) => "int".to_string(),
            Type::Boolean(_) => "bool".to_string(),
            Type::Char(_) => "char".to_string(),
            Type::Tag(s, t, _) => format!("ttag('{}', {})", s, t),
            Type::Union(v, _) => format!("union({})", to_string(v)),
            Type::Interface(v, _) => format!("interface({})", to_string(v)),
            Type::Params(v, _) => format!("params({})", to_string(v)),
            Type::Add(id1, id2, _) => format!("add({}, {})", id1, id2),
            Type::Minus(id1, id2, _) => format!("minus({}, {})", id1, id2),
            Type::Mul(id1, id2, _) => format!("mul({}, {})", id1, id2),
            Type::Div(id1, id2, _) => format!("division({}, {})", id1, id2),
            Type::LabelGen(l, _) => format!("%{}", l),
            Type::Label(l, _) => format!("{}", l),
            Type::Empty(_) => "any".to_string(),
            _ => "".to_string()
        };
        write!(f, "{}", res)       
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Number(_), Type::Number(_)) => true,
            (Type::Integer(_), Type::Integer(_)) => true,
            (Type::Boolean(_), Type::Boolean(_)) => true,
            (Type::Char(_), Type::Char(_)) => true,
            (Type::Embedded(e1, _), Type::Embedded(e2, _)) => e1 == e2,
            (Type::Function(a1, b1, c1, _), Type::Function(a2, b2, c2, _)) 
                => a1 == a2 && b1 == b2 && c1 == c2,
            (Type::Generic(e1, _), Type::Generic(e2, _)) => e1 == e2,
            (Type::IndexGen(e1, _), Type::IndexGen(e2, _)) => e1 == e2,
            (Type::LabelGen(e1, _), Type::LabelGen(e2, _)) => e1 == e2,
            (Type::Label(e1, _), Type::Label(e2, _)) => e1 == e2,
            (Type::Array(a1, b1, _), Type::Array(a2, b2, _)) 
                => a1 == a2 && b1 == b2,
            (Type::Record(e1, _), Type::Record(e2, _)) => e1 == e2,
            (Type::Index(e1, _), Type::Index(e2, _)) => e1 == e2,
            (Type::Alias(a1, b1, c1, _), Type::Alias(a2, b2, c2, _)) 
                => a1 == a2 && b1 == b2 && c1 == c2,
            (Type::Tag(a1, b1, _), Type::Tag(a2, b2, _)) 
                => a1 == a2 && b1 == b2,
            (Type::Union(e1, _), Type::Union(e2, _)) => e1 == e2,
            (Type::Interface(e1, _), Type::Interface(e2, _)) => e1 == e2,
            (Type::Params(e1, _), Type::Params(e2, _)) => e1 == e2,
            (Type::Add(a1, b1, _), Type::Add(a2, b2, _)) 
                => a1 == a2 && b1 == b2,
            (Type::Minus(a1, b1, _), Type::Minus(a2, b2, _)) 
                => a1 == a2 && b1 == b2,
            (Type::Mul(a1, b1, _), Type::Mul(a2, b2, _)) 
                => a1 == a2 && b1 == b2,
            (Type::Div(a1, b1, _), Type::Div(a2, b2, _)) 
                => a1 == a2 && b1 == b2,
            (Type::Failed(e1, _), Type::Failed(e2, _)) => e1 == e2,
            (Type::Opaque(e1, _), Type::Opaque(e2, _)) => e1 == e2,
            (Type::Multi(e1, _), Type::Multi(e2, _)) => e1 == e2,
            (Type::Tuple(e1, _), Type::Tuple(e2, _)) => e1 == e2,
            (Type::If(a1, b1, _), Type::If(a2, b2, _)) 
                => a1 == a2 && b1 == b2,
            (Type::Condition(a1, b1, c1, _), Type::Condition(a2, b2, c2, _)) 
                => a1 == a2 && b1 == b2 && c1 == c2,
            (Type::In(_), Type::In(_)) => true,
            (Type::Empty(_), Type::Empty(_)) => true,
            (Type::Any(_), Type::Any(_)) => true,
            _ => false
        }
    }
}
