#![allow(dead_code)]
pub mod array_type;
pub mod function_type;
pub mod generic;
pub mod index;
pub mod js_types;
pub mod module_type;
pub mod tchar;
pub mod tint;
pub mod type_category;
pub mod type_operator;
pub mod type_printer;
pub mod typer;
pub mod argument_type;
pub mod type_system;
pub mod vector_type;

use crate::processes::type_checking::type_comparison::reduce_type;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::type_error::TypeError;
use crate::processes::parsing::operation_priority::TokenKind;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::type_category::TypeCategory;
use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::module_type::ModuleType;
use crate::processes::parsing::type_token::TypeToken;
use crate::components::r#type::type_printer::verbose;
use crate::components::r#type::type_printer::format;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::type_printer::short;
use crate::processes::parsing::types::ltype;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::tint::Tint;
use crate::components::language::var::Var;
use crate::components::context::Context;
use serde::{Serialize, Deserialize};
use std::collections::HashSet;
use crate::utils::builder;
use std::cmp::Ordering;
use std::str::FromStr;
use std::hash::Hasher;
use std::hash::Hash;
use std::fmt;

pub fn generate_arg(num: usize) -> String {
    match num {
        0 => "a",
        1 => "b",
        2 => "c",
        3 => "d",
        4 => "e",
        5 => "f",
        6 => "g",
        7 => "h",
        8 => "i",
        9 => "j",
        10 => "k",
        _ => "overflow"
    }.to_string()
}

fn to_string<T: ToString>(v: &[T]) -> String {
    let res = v.iter()
        .map(|x| x.to_string())
        .reduce(|acc, x| format!("{}, {}", acc, x))
        .unwrap_or("".to_string());
    format!("[{}]", res)
}

pub fn pretty(hs: HashSet<ArgumentType>) -> String {
    hs.iter()
        .map(|arg_typ| format!("{}: {}", 
                               arg_typ.get_argument_str(), 
                               arg_typ.get_type().pretty()))
        .collect::<Vec<_>>().join("\n")
}


#[derive(Debug, Clone, Serialize, Deserialize, Eq)]
pub enum Type {
    Number(HelpData),
    Integer(Tint, HelpData),
    Boolean(HelpData),
    Char(Tchar, HelpData),
    Embedded(Box<Type>, HelpData),
    Function(Vec<Type>, Box<Type>, HelpData),
    Generic(String, HelpData),
    IndexGen(String, HelpData),
    LabelGen(String, HelpData),
    Vec(VecType, Box<Type>, Box<Type>, HelpData),
    Record(HashSet<ArgumentType>, HelpData),
    Module(Vec<ArgumentType>, HelpData),
    Alias(String, Vec<Type>, bool, HelpData), //for opacity
    Tag(String, Box<Type>, HelpData),
    Union(HashSet<Type>, HelpData),
    Intersection(HashSet<Type>, HelpData),
    Interface(HashSet<ArgumentType>, HelpData),
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
    UnknownFunction(HelpData),
    RClass(HashSet<String>, HelpData),
    Empty(HelpData),
    Operator(TypeOperator, Box<Type>, Box<Type>, HelpData),
    Any(HelpData),
    Variable(String, HelpData),
}

impl TypeSystem for Type {
    fn pretty(&self) -> String {
        format(self)
    }

    fn simple_pretty(&self) -> String {
        short(self)
    }

    fn verbose_pretty(&self) -> String {
        verbose(self)
    }

    fn is_subtype(&self, other: &Type, context: &Context) -> bool {
        match (self, other) {
            (Type::Empty(_), _) => true,
            (typ1, typ2) if typ1 == typ2 => true,
            (Type::Intersection(types, _), typ) => {
                types.iter()
                    .any(|x| x.is_subtype(typ, context))
            },
            (_, Type::Any(_)) => true,
            (Type::Vec(_, n1, t1, _), Type::Vec(_, n2, t2, _)) => {
                n1.is_subtype(&*n2, context) && t1.is_subtype(&*t2, context)
            },
            (Type::Function(args1, ret_typ1, _), Type::Function(args2, ret_typ2, _)) => {
                args1.len() == args2.len() &&
                args1.iter().chain([&(**ret_typ1)])
                    .zip(args2.iter().chain([&(**ret_typ2)]))
                    .all(|(typ1, typ2)| typ1.strict_subtype(typ2))
            },
            (_, Type::UnknownFunction(_)) => true,
            (Type::Interface(args1, _), Type::Interface(args2, _)) => {
                args1 == args2 || args1.is_superset(args2)
            },
            (typ, Type::Interface(args2, _)) => {
                match typ.to_interface(context) {
                    Type::Interface(args1, _) 
                        => {
                            &args1 == args2 || args1.is_superset(args2)
                        },
                    _ => todo!()
                }
            }
            // Record subtyping
            (Type::Record(r1, _), Type::Record(r2, _)) => {
                r1 == r2 || r1.is_superset(r2)
            },
            (Type::Tag(name1, body1, _h1), Type::Tag(name2, body2, _h2)) => {
                (name1 == name2) && body1.is_subtype(&*body2, context)
            },
            // Generic subtyping
            (_, Type::Generic(_, _)) => true,
            (Type::Integer(_, _), Type::IndexGen(_, _)) => true,
            (Type::Char(_, _), Type::LabelGen(_, _)) => true,
            (Type::IndexGen(_, _), Type::IndexGen(_, _)) => true,
            // Params subtyping
            (Type::Params(p1, _), Type::Params(p2, _)) => {
                p1.len() == p2.len() && 
                p1.iter().zip(p2.iter()).all(|(t1, t2)| t1.is_subtype(t2, context))
            }
            (Type::RClass(set1, _), Type::RClass(set2, _)) 
                => set1.is_subset(&set2),
            (Type::Operator(TypeOperator::Union, _t1, _t2, _), 
             Type::Operator(TypeOperator::Union, _tp1, _tp2, _))
                => true, //TODO: Fix this
            (typ, Type::Operator(TypeOperator::Union, t1, t2, _)) 
                => typ.is_subtype(&t1, context) || typ.is_subtype(t2, context),
            (Type::Char(_, _), Type::Char(_, _)) => true,
            (Type::Integer(_, _), Type::Integer(_, _)) => true,
            (Type::Tuple(types1, _), Type::Tuple(types2, _)) => {
                types1.iter()
                    .zip(types2.iter())
                    .all(|(typ1, typ2)| typ1.is_subtype(typ2, context))
            },
            (typ, Type::Operator(TypeOperator::Intersection, t1, t2, _)) => {
                typ.is_subtype(t1, context) && typ.is_subtype(t2, context)
            },
            _ => false
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        builder::any_type()
    }
}

//main
impl Type {
    pub fn add_to_context(self, var: Var, context: Context) -> (Type, Context) {
        let cont = context.clone().push_var_type(var.clone().set_type(self.clone()), self.clone(), &context);
        if self.is_function() {
            self.tuple(&cont) //TODO save if function
        } else {
            self.tuple(&cont)
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Type::Function(_, _, _) => true,
            Type::UnknownFunction(_) => true,
            _ => false
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Boolean(_) | Type::Number(_) | Type::Integer(_, _) | Type::Char(_, _)
                => true,
            _ => false
        }
    }

    pub fn get_covariant_type(&self, annotation: &Type, context: &Context) -> Type {
        let reduced_annotation = annotation.reduce(context);
        let reduced_type = self.reduce(context);
        if !annotation.is_empty() {
            reduced_type.is_subtype(&reduced_annotation, context)
                            .then_some(annotation.clone())
                            .expect(&TypeError::Let(annotation.clone(), self.clone()).display())
        } else { self.clone() }
    }

    pub fn extract_types(&self) -> Vec<Type> {
        match self {
            Type::Function(args, ret, _)
                => {
                    let mut sol = args.clone();
                    sol.push((**ret).clone());
                    sol.push(self.clone()); sol
                }
            Type::Module(argtypes, _) => {
                let mut sol = argtypes.iter()
                    .map(|argtype| argtype.get_type())
                    .collect::<Vec<_>>();
                sol.push(self.clone()); sol
            },
            typ => vec![typ.clone()]
        }
    }

    pub fn get_label(&self) -> String {
        match self {
            Type::Char(l, _) => l.to_string(),
            Type::LabelGen(l, _) => l.to_string(),
            _ => panic!("The type {} wasn't a label", self)
        }
    }

    pub fn replace_function_types(self: Type, t1: Type, t2: Type) -> Type {
        match self {
            Type::Function(args, ret, h) => {
                let new_args = args.iter()
                    .map(|typ| if *typ == t1 { t2.clone() } else {t1.clone()})
                    .collect::<Vec<_>>();
                let new_ret = if *ret == t1 { t2 } else { *ret };
                Type::Function(new_args, Box::new(new_ret), h)
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
           Type::Integer(_, _) => "number".to_string(),
           Type::Number(_) => "number".to_string(),
           Type::Char(_, _) => "string".to_string(),
           Type::Record(body, _) => {
                let res = body.iter()
                    .map(|at| format!("{}: {}", at.get_argument(), at.get_type().to_typescript()))
                    .collect::<Vec<_>>().join(", ");
                format!("{{ {} }}", res)
           },
           Type::Vec(_, _size, body, _) => format!("{}[]", body.to_typescript()),
           Type::IndexGen(id, _) => id.to_uppercase(),
           Type::Generic(val, _) => val.to_uppercase(), 
           Type::Function(args, ret, _) => {
               let res = args.iter()
                    .enumerate()
                    .map(|(i, typ)| format!("{}: {}", generate_arg(i), typ.to_typescript()))
                    .collect::<Vec<_>>().join(", ");
               format!("({}) => {}", res, ret.to_typescript())
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
           Type::Integer(_, _) => "i32".to_string(),
           Type::Number(_) => "f64".to_string(),
           Type::Char(_, _) => "string".to_string(),
           Type::Record(body, _) => {
                let res = body.iter()
                    .map(|at| format!("{}: {}", at.get_argument(), at.get_type().to_typescript()))
                    .collect::<Vec<_>>().join(", ");
                format!("{{ {} }}", res)
           },
           Type::Vec(_, _size, body, _) => format!("{}[]", body.to_typescript()),
           Type::IndexGen(id, _) => id.to_uppercase(),
           Type::Generic(val, _) => val.to_uppercase(), 
           Type::Function(args, ret, _) => {
               let res = args.iter()
                    .enumerate()
                    .map(|(i, typ)| format!("{}: {}", generate_arg(i), typ.to_typescript()))
                    .collect::<Vec<_>>().join(", ");
               format!("({}) => {}", res, ret.to_typescript())
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
            Type::Function(args, ret_typ, _) => {
                    args.iter().cloned()
                    .chain([(**ret_typ).clone()].iter().cloned())
                    .collect::<HashSet<_>>()
                    .iter().flat_map(|typ| typ.extract_generics())
                    .collect::<Vec<_>>()
            }
            Type::Vec(_, ind, typ, _) => {
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
            Type::Vec(vtype, ind, typ, h) =>
                Type::Vec(
                    *vtype, 
                    Box::new(ind.index_calculation()), 
                    Box::new(typ.index_calculation()),
                    h.clone()),
                    Type::Function(args, ret_typ, h) => {
                        let new_args = args.iter()
                            .map(|typ| typ.index_calculation())
                            .collect::<Vec<_>>();
                        Type::Function(
                            new_args,
                            Box::new(ret_typ.index_calculation()), h.clone())
                    }
            _ => self.clone()
        }
    }

    fn sum_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Integer(a, h), Type::Integer(b, _)) => Type::Integer(*a+*b, h.clone()),
            (Type::Record(a, h), Type::Record(b, _)) 
                => Type::Record(
                    a.iter()
                    .chain(b.iter())
                    .cloned()
                    .collect::<HashSet<_>>(), h.clone()),
            _ => Type::Add(Box::new(self.clone()), Box::new(i.clone()), HelpData::default())
            //_ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn minus_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Integer(a, h), Type::Integer(b, _)) => Type::Integer(*a-*b, h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn mul_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Integer(a, h), Type::Integer(b, _)) => Type::Integer(*a*(*b), h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn div_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Integer(a, h), Type::Integer(b, _)) => Type::Integer(*a/(*b), h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    pub fn get_shape(&self) -> Option<String> {
        if let Type::Vec(_, i, t, _) = self {
            match (*i.clone(), t.get_shape()) {
                (Type::IndexGen(_, _), _) => Some("dim(===)".to_string()),
                (Type::Integer(j, _), Some(rest)) => Some(format!("{}, {}", j, rest)),
                (Type::Integer(j, _), None) => Some(format!("{}", j)),
                _ => None
            }
        } else { None }
    }

    pub fn to_function_type(&self) -> Option<FunctionType> {
        match self {
            Type::Function(args, ret_ty, h) 
                => Some(FunctionType(args.clone(), (**ret_ty).clone(), h.clone())),
            Type::UnknownFunction(h) 
                => Some(FunctionType(vec![], builder::unknown_function(), h.clone())),
            _ => None
        }
    }

    pub fn get_type_pattern(&self) -> Option<ArgumentType> {
        if let Type::Record(fields, _) = self {
            (fields.len() == 1)
                .then(|| fields.iter().next().unwrap().clone())
        } else {None}
    }

    pub fn is_boolean(&self) -> bool {
        if let Type::Boolean(_) = self {
            true
        } else { false }
    }

    pub fn dependent_type(&self, dep_typ: &Type) -> bool {
        match (dep_typ, self) {
            (Type::Integer(_, _), Type::IndexGen(_, _)) => true,
            (Type::Char(_, _), Type::LabelGen(_, _)) => true,
            _ => false
        }
    }


    pub fn pretty2(&self) -> String {
        verbose(self)
    }

    pub fn is_tag_or_union(&self) -> bool {
        match self {
            Type::Tag(_, _, _) => true,
            _ => false
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Type::Generic(_, _) => true,
            Type::IndexGen(_, _) => true,
            Type::LabelGen(_, _) => true,
            _ => false
        }
    }

    pub fn exact_equality(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Integer(a, _), Type::Integer(b, _)) => a == b,
            (Type::Char(a, _), Type::Char(b, _)) => a == b,
            (Type::Generic(e1, _), Type::Generic(e2, _)) => e1 == e2,
            (Type::IndexGen(e1, _), Type::IndexGen(e2, _)) => e1 == e2,
            (Type::LabelGen(e1, _), Type::LabelGen(e2, _)) => e1 == e2,
            _ => self == other
        }
    }

    pub fn for_var(self) -> Type {
        match self.to_owned() {
            Type::Function(p, _r, _h) => {
                if p.len() > 0 {
                   p[0].to_owned()
                } else {
                    self
                }
            },
            t => t
        }
    }
    
    pub fn to_category(&self) -> TypeCategory {
        match self {
            Type::Vec(_, _, _, _) => TypeCategory::Array,
            Type::Function(_, _, _) => TypeCategory::Function,
            Type::Record(_, _) => TypeCategory::Record,
            Type::Tuple(_, _) => TypeCategory::Tuple,
            Type::Tag(_, _, _) => TypeCategory::Tag,
            Type::Interface(_, _) => TypeCategory::Interface,
            Type::Boolean(_) => TypeCategory::Boolean,
            Type::Number(_) => TypeCategory::Number,
            Type::Char(_, _) => TypeCategory::Char,
            Type::Generic(_, _) => TypeCategory::Generic,
            Type::IndexGen(_, _) => TypeCategory::Generic,
            Type::LabelGen(_, _) => TypeCategory::Generic,
            Type::Integer(_, _) => TypeCategory::Integer,
            Type::Alias(_, _, false, _) => TypeCategory::Alias,
            Type::Alias(name, _, _, _) => TypeCategory::Opaque(name.clone()),
            Type::Any(_) => TypeCategory::Any,
            Type::Empty(_) => TypeCategory::Empty,
            Type::RClass(_, _) => TypeCategory::RClass,
            Type::UnknownFunction(_) => TypeCategory::RFunction,
            Type::Union(_, _) => TypeCategory::Union,
            Type::Add(_, _, _) => TypeCategory::Template,
            Type::Minus(_, _, _) => TypeCategory::Template,
            Type::Mul(_, _, _) => TypeCategory::Template,
            Type::Div(_, _, _) => TypeCategory::Template,
            Type::Intersection(_, _) => TypeCategory::Intersection,
            Type::Module(_, _) => TypeCategory::Module,
            Type::Operator(_, _, _, _) => TypeCategory::Operator,
            _ => {
                println!("{:?} return Rest", self);
                TypeCategory::Rest
            }
        }
    }

    // for removing litteral data for Integer and Char
    pub fn generalize(self) -> Type {
        match self {
            Type::Integer(Tint::Val(_), h) 
                => Type::Integer(Tint::Unknown, h),
            Type::Char(Tchar::Val(_), h) 
                => Type::Char(Tchar::Unknown, h),
            t => t
        }
    }

    pub fn reduce(&self, context: &Context) -> Type {
        reduce_type(context, self)
    }

    fn get_type_shape(&self) -> usize {
        match self {
            Type::Function(v, _, _) => v.len(),
            Type::Tuple(v, _) => v.len(),
            _ => 0 as usize
        }
    }

    pub fn same_shape(&self, other: &Type) -> bool {
        self.get_type_shape() == other.get_type_shape()
    }

    pub fn get_help_data(&self) -> HelpData {
        match self {
            Type::Integer(_, h) => h.clone(),
            Type::Number(h) => h.clone(),
            Type::Char(_, h) => h.clone(),
            Type::Boolean(h) => h.clone(),
            Type::Embedded(_, h) => h.clone(),
            Type::Function(_, _, h) => h.clone(),
            Type::Generic(_, h) => h.clone(),
            Type::IndexGen(_, h) => h.clone(),
            Type::LabelGen(_, h) => h.clone(),
            Type::Vec(_, _, _, h) => h.clone(),
            Type::Record(_, h) => h.clone(),
            Type::Module(_, h) => h.clone(),
            Type::Alias(_, _, _, h) => h.clone(),
            Type::Tag(_, _, h) => h.clone(),
            Type::Interface(_, h) => h.clone(),
            Type::Params(_, h) => h.clone(),
            Type::Add(_, _, h) => h.clone(),
            Type::Mul(_, _, h) => h.clone(),
            Type::Minus(_, _, h) => h.clone(),
            Type::Div(_, _, h) => h.clone(),
            Type::Failed(_, h) => h.clone(),
            Type::Opaque(_, h) => h.clone(),
            Type::Multi(_, h) => h.clone(),
            Type::Tuple(_, h) => h.clone(),
            Type::If(_, _, h) => h.clone(),
            Type::Condition(_, _, _, h) => h.clone(),
            Type::In(h) => h.clone(),
            Type::UnknownFunction(h) => h.clone(),
            Type::Empty(h) => h.clone(),
            Type::Any(h) => h.clone(),
            Type::RClass(_, h) => h.clone(),
            Type::Union(_, h) => h.clone(),
            Type::Intersection(_, h) => h.clone(),
            Type::Operator(_, _, _, h) => h.clone(),
            Type::Variable(_, h) => h.clone(),
        }
    }

    pub fn set_help_data(self, h2: HelpData) -> Type {
        match self {
            Type::Integer(i, _) => Type::Integer(i, h2),
            Type::Number(_) => Type::Number(h2),
            Type::Char(a, _) => Type::Char(a, h2),
            Type::Boolean(_) => Type::Boolean(h2),
            Type::Embedded(a, _) => Type::Embedded(a, h2),
            Type::Function(a2, a3, _) => Type::Function(a2, a3, h2),
            Type::Generic(a, _) => Type::Generic(a, h2),
            Type::IndexGen(a, _) => Type::IndexGen(a, h2),
            Type::LabelGen(a, _) => Type::LabelGen(a, h2),
            Type::Vec(vt, a1, a2, _) => Type::Vec(vt, a1, a2, h2),
            Type::Record(a, _) => Type::Record(a, h2),
            Type::Module(a, _) => Type::Module(a, h2),
            Type::Alias(a1, a2, a3, _) => Type::Alias(a1, a2, a3, h2),
            Type::Tag(a1, a2, _) => Type::Tag(a1, a2, h2),
            Type::Interface(a, _) => Type::Interface(a, h2),
            Type::Params(a, _) => Type::Params(a, h2),
            Type::Add(a1, a2, _) => Type::Add(a1, a2, h2),
            Type::Mul(a1, a2, _) => Type::Mul(a1, a2, h2),
            Type::Minus(a1, a2, _) => Type::Minus(a1, a2, h2),
            Type::Div(a1, a2, _) => Type::Div(a1, a2, h2),
            Type::Failed(a, _) => Type::Failed(a, h2),
            Type::Opaque(a, _) => Type::Opaque(a, h2),
            Type::Multi(a, _) => Type::Multi(a, h2),
            Type::Tuple(a, _) => Type::Tuple(a, h2),
            Type::If(a1, a2, _) => Type::If(a1, a2, h2),
            Type::Condition(a1, a2, a3, _) => Type::Condition(a1, a2, a3, h2),
            Type::In(_) => Type::In(h2),
            Type::UnknownFunction(_) => Type::UnknownFunction(h2),
            Type::Empty(_) => Type::Empty(h2),
            Type::Any(_) => Type::Any(h2),
            Type::RClass(v, _) => Type::RClass(v, h2),
            Type::Union(v, _) => Type::Union(v, h2),
            Type::Intersection(i, _) => Type::Intersection(i, h2),
            Type::Operator(op, t1, t2, _) => Type::Operator(op, t1, t2, h2),
            Type::Variable(name, _) => Type::Variable(name, h2),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Type::Empty(_) => true,
            _ => false
        }
    }

    pub fn to_array(&self) -> Option<Array> {
        match self {
            Type::Vec(_, t1, t2, h) 
                => Some(Array{index: (**t1).clone(), base_type: (**t2).clone(), help_data: h.clone()}),
            _ => None
        }
    }

    pub fn to_array2(args: Vec<Type>) -> Type {
        let h = HelpData::default();
        if args.len() > 1 {
            Type::Vec(
                VecType::Array,
                Box::new(args[0].clone()), 
                Box::new(Self::to_array2(args[1..].to_vec())), h)
        } else {
            Type::Vec(
                VecType::Array,
                Box::new(args[0].clone()), 
                Box::new(builder::integer_type_default()), h)
        }
    }

    pub fn is_any(&self) -> bool {
        *self == builder::any_type()
    }

    pub fn tuple(self, context: &Context) -> (Type, Context) {
        (self, context.clone())
    }

    pub fn get_index(&self) -> Option<u32> {
        match self {
            Type::Integer(i, _) 
                => Some(i.get_value().map(|x| x as u32).unwrap_or(0 as u32)),
            Type::IndexGen(_, _) => Some(0 as u32),
            _ => None
        }
    }

    pub fn to_interface(&self, context: &Context) -> Type {
        match self {
            Type::Interface(_, _) => self.clone(),
            Type::Function(_, _, _) 
                => builder::interface_type(&[("_", builder::unknown_function())]),
            typ => {
                let function_signatures = context.get_functions_from_type(typ)
                    .iter().cloned()
                    .map(|(var, typ2)| {
                        (var.get_name(), typ2.replace_function_types(typ.clone(), builder::self_generic_type()))
                    })
                    .collect::<Vec<_>>();
                builder::interface_type2(&function_signatures)
            }
        }
    }

    fn strict_subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Empty(_), _) => true,
            (_, Type::Any(_)) => true,
            (typ1, typ2) if typ1 == typ2 => true,
            // Generic subtyping
            (_, Type::Generic(_, _)) => true,
            _ => false
        }
    }

    pub fn get_first_function_parameter_type(&self, name: &str) -> Option<Type> {
        match self {
            Type::Module(args, _) => {
                let res = args.iter()
                    .find(|arg_typ| arg_typ.get_argument_str() == name);
                match res {
                    Some(arg_typ) => arg_typ.get_type().get_first_parameter(),
                    _ => None
                }
            },
            _ => None
        }
    }

    pub fn get_first_parameter(&self) -> Option<Type> {
        match self {
            Type::Function(args, _, _) 
                => args.iter().next().cloned(),
            _ => None
        }
    }

    pub fn get_token_type(&self) -> TokenKind {
        TokenKind::Expression
    }

    pub fn get_binding_power(&self) -> i32 {
        0
    }

    pub fn to_module_type(self) -> Result<ModuleType, String> {
        match self {
            Type::Module(args, h) => Ok(ModuleType::from((args, h))),
            _ => Err(format!("{} can't be turn into a ModuleType", self.pretty()))
        }
    }

    pub fn linearize(self) -> Vec<Type> {
        match self {
            Type::Vec(_, t1, t2, _) 
                => [*t1].iter().chain((*t2).linearize().iter()).cloned().collect(),
            other => vec![other]
        }
    }

    pub fn is_upperrank_of(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Vec(_, _, typ1, _), typ2) => **typ1 == *typ2,
            _ => false
        }
    }

    pub fn has_generic(&self) -> bool {
        self.extract_generics().len() > 0
    }

    pub fn has_operation(&self) -> bool {
        false
    }

    pub fn is_reduced(&self) -> bool {
        !self.has_generic() && !self.has_operation()
    }

    pub fn get_size_type(&self) -> (i32, Type) {
        match self {
            Type::Vec(_, i, t, _) => (i.get_index().unwrap() as i32, (**t).clone()),
            typ => (1, typ.clone())
        }
    }

    pub fn is_unknown_function(&self) -> bool {
        match self {
            Type::UnknownFunction(_) => true,
            _ => false
        }
    }

    pub fn is_vector_of(&self, other: &Type, context: &Context) -> bool {
        let reduced_self = self.reduce(context);
        let reduced_other = other.reduce(context);
        match reduced_self {
            Type::Vec(_, _, t, _) => *t == reduced_other,
            _ => false
        }
    }

}

pub struct Array {
    pub index: Type,
    pub base_type: Type,
    pub help_data: HelpData
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
           Type::Function(_, _, h) => h,
           Type::Char(_, h) => h,
           Type::Integer(_, h) => h,
           Type::Record(_, h) => h,
           Type::Boolean(h) => h,
           Type::Vec(_, _, _, h) => h,
           Type::Number(h) => h,
           Type::Intersection(_, h) => h,
           Type::Union(_, h) => h,
           Type::Any(h) => h,
           Type::UnknownFunction(h) => h,
           e => panic!("The type element {:?} is not yet implemented", e)
       }.clone()
   } 
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Number(_), Type::Number(_)) => true,
            (Type::Integer(_, _), Type::Integer(_, _)) => true,
            (Type::Boolean(_), Type::Boolean(_)) => true,
            (Type::Char(_, _), Type::Char(_, _)) => true, 
            (Type::Embedded(e1, _), Type::Embedded(e2, _)) => e1 == e2,
            (Type::Function(b1, c1, _), Type::Function(b2, c2, _)) 
                => b1 == b2 && c1 == c2,
            (Type::Generic(_, _), Type::Generic(_, _)) => true,
            (Type::IndexGen(_, _), Type::IndexGen(_, _)) => true,
            (Type::LabelGen(_, _), Type::LabelGen(_, _)) => true,
            (Type::Vec(_, a1, b1, _), Type::Vec(_, a2, b2, _)) 
                => a1 == a2 && b1 == b2,
            (Type::Record(e1, _), Type::Record(e2, _)) => e1 == e2,
            (Type::Alias(a1, b1, c1, _), Type::Alias(a2, b2, c2, _)) 
                => a1 == a2 && b1 == b2 && c1 == c2,
            (Type::Tag(a1, b1, _), Type::Tag(a2, b2, _)) 
                => a1 == a2 && b1 == b2,
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
            (Type::RClass(el1, _), Type::RClass(el2, _)) 
                => {
                    el1.difference(&el2).collect::<Vec<_>>().len() == 0
                },
            (Type::Union(s1, _), Type::Union(s2, _)) => s1 == s2,
            (Type::Intersection(s1, _), Type::Intersection(s2, _)) => s1 == s2,
            (Type::Variable(s1, _), Type::Variable(s2, _)) => s1 == s2,
            (Type::UnknownFunction(_), Type::UnknownFunction(_)) => true,
            (Type::UnknownFunction(_), Type::Empty(_)) => true,
            (Type::Empty(_), Type::UnknownFunction(_)) => true,
            _ => false
        }
    }
}

impl fmt::Display for Type {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Function(p, r, h) 
                => write!(f, "({}) -> {}", Type::Params(p.clone(), h.clone()), r),
            Type::Params(v, _) => {
                let res = v.iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ");
                write!(f, "{}", res)
            }
            _ => write!(f, "{}", format(self))
        }
    }
}

// main subtype
impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Type::Empty(_), _) => Some(Ordering::Less),
            (typ1, typ2) if typ1 == typ2 => Some(Ordering::Equal),
            // Array subtyping
            (_, Type::Any(_)) => Some(Ordering::Less),
            (Type::Vec(_, n1, t1, _), Type::Vec(_, n2, t2, _)) => {
                (n1.partial_cmp(&*n2).is_some() && t1.partial_cmp(&*t2).is_some())
                    .then_some(Ordering::Less)
            },
            (Type::Function(args1, ret_typ1, _), Type::Function(args2, ret_typ2, _)) => {
                args1.iter().chain([&(**ret_typ1)])
                    .zip(args2.iter().chain([&(**ret_typ2)]))
                    .all(|(typ1, typ2)| typ1.partial_cmp(typ2).is_some())
                    .then_some(Ordering::Less)
            }
            (Type::Function(_, _, _), Type::UnknownFunction(_)) => Some(Ordering::Less),
            // Interface subtyping
            (Type::Interface(args1, _), Type::Interface(args2, _)) => {
                (args1 == args2 || args1.is_superset(args2))
                    .then_some(Ordering::Less)
            }
            // Record subtyping
            (Type::Record(r1, _), Type::Record(r2, _)) => {
                (r1 == r2 || r1.is_superset(r2))
                    .then_some(Ordering::Less)
            },

            (Type::Tag(name1, body1, _h1), Type::Tag(name2, body2, _h2)) => {
                ((name1 == name2) && body1.partial_cmp(&*body2).is_some())
                        .then_some(Ordering::Less)
            },

            // Generic subtyping
            (_, Type::Generic(_, _)) => Some(Ordering::Less),
            (Type::Integer(_, _), Type::IndexGen(_, _)) => Some(Ordering::Less),
            (Type::Char(_, _), Type::LabelGen(_, _)) => Some(Ordering::Less),
            (Type::IndexGen(_, _), Type::IndexGen(_, _)) => Some(Ordering::Less),

            // Params subtyping
            (Type::Params(p1, _), Type::Params(p2, _)) => {
                (p1.len() == p2.len() && 
                p1.iter().zip(p2.iter()).all(|(t1, t2)| t1.partial_cmp(t2).is_some()))
                        .then_some(Ordering::Less)
            }

            (Type::RClass(set1, _), Type::RClass(set2, _)) 
                => set1.is_subset(&set2).then_some(Ordering::Less),
            (Type::Union(s1, _), Type::Union(s2, _)) 
                => s1.is_subset(&s2).then_some(Ordering::Less),
            (typ, Type::Union(s2, _)) 
                => s2.contains(&typ).then_some(Ordering::Less),
            (Type::Char(_, _), Type::Char(_, _)) => Some(Ordering::Less),
            (Type::Integer(_, _), Type::Integer(_, _)) => Some(Ordering::Less),
            (Type::Tuple(types1, _), Type::Tuple(types2, _)) => {
                types1.iter()
                    .zip(types2.iter())
                    .all(|(typ1, typ2)| typ1.partial_cmp(typ2).is_some())
                    .then_some(Ordering::Less)
            },
            (typ, Type::Intersection(types, _)) => {
               types.iter() 
                   .all(|typ2| typ.partial_cmp(typ2) == Some(Ordering::Less))
                   .then_some(Ordering::Less)
            },
            _ => None
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Utiliser un discriminant pour différencier les variantes
        match self {
            Type::Number(_) => 0.hash(state),
            Type::Integer(_, _) => 1.hash(state),
            Type::Boolean(_) => 2.hash(state),
            Type::Char(_, _) => 3.hash(state),
            Type::Embedded(_, _) => 4.hash(state),
            Type::Function(_, _, _) => 5.hash(state),
            Type::Generic(_, _) => 6.hash(state),
            Type::IndexGen(_, _) => 7.hash(state),
            Type::LabelGen(_, _) => 8.hash(state),
            Type::Vec(_, _, _, _) => 9.hash(state),
            Type::Record(_, _) => 10.hash(state),
            Type::Alias(_, _, _, _) => 11.hash(state),
            Type::Tag(_, _, _) => 12.hash(state),
            Type::Interface(_, _) => 14.hash(state),
            Type::Params(_, _) => 15.hash(state),
            Type::Add(_, _, _) => 16.hash(state),
            Type::Minus(_, _, _) => 17.hash(state),
            Type::Mul(_, _, _) => 18.hash(state),
            Type::Div(_, _, _) => 19.hash(state),
            Type::Failed(_, _) => 20.hash(state),
            Type::Opaque(_, _) => 21.hash(state),
            Type::Multi(_, _) => 22.hash(state),
            Type::Tuple(_, _) => 23.hash(state),
            Type::If(_, _, _) => 24.hash(state),
            Type::Condition(_, _, _, _) => 25.hash(state),
            Type::In(_) => 26.hash(state),
            Type::Empty(_) => 27.hash(state),
            Type::Any(_) => 28.hash(state),
            Type::UnknownFunction(_) => 30.hash(state),
            Type::RClass(_, _) => 31.hash(state),
            Type::Union(_, _) => 33.hash(state),
            Type::Intersection(_, _) => 36.hash(state),
            Type::Module(_, _) => 37.hash(state),
            Type::Operator(_, _, _, _) => 38.hash(state),
            Type::Variable(_, _) => 39.hash(state),
        }
    }
}

pub fn display_types(v: &[Type]) -> String {
    v.iter()
        .map(|x| x.pretty())
        .collect::<Vec<_>>()
        .join(" | ")
}

impl From<FunctionType> for Type {
   fn from(val: FunctionType) -> Self {
        Type::Function(val.get_param_types(), 
                       Box::new(val.get_return_type()), 
                       val.get_help_data())
   } 
}



#[derive(Debug)]
pub struct ErrorStruct;

impl FromStr for Type {
    type Err = ErrorStruct;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let val = ltype(s.into())
            .map(|x| x.1).unwrap_or(builder::unknown_function());
        Ok(val)
    }

}

impl From<TokenKind> for  Type {
   fn from(val: TokenKind) -> Self {
        match val {
            TokenKind::Empty => builder::unknown_function(),
            _ => builder::any_type()
        }
   } 
}

impl From<TypeToken> for Type {
   fn from(val: TypeToken) -> Self {
       match val {
           TypeToken::Expression(typ) => typ,
           _ => builder::unknown_function()
       }
   } 
}

#[cfg(test)]
mod tests {
    use crate::utils::fluent_parser::FluentParser;
    use crate::processes::type_checking::typing;
    use crate::processes::parsing::parse;
    use super::*;

    #[test]
    fn test_record_hierarchy0(){
        let name = builder::record_type(&[
                ("name".to_string(), builder::character_type_default()),
        ]);
        let age = builder::record_type(&[
                ("age".to_string(), builder::integer_type_default()),
        ]);
        assert!(name.is_subtype(&age, &Context::default()));
    }

    #[test]
    fn test_record_hierarchy1(){
        let p1 = builder::record_type(&[
                ("age".to_string(), builder::integer_type_default()),
                ("name".to_string(), builder::character_type_default()),
        ]);
        let p2 = builder::record_type(&[
                ("name".to_string(), builder::character_type_default()),
                ("age".to_string(), builder::integer_type_default()),
        ]);
        assert_eq!(p1, p2);
    }

    #[test]
    fn test_record_subtyping1() {
        let name = builder::record_type(&[
                ("name".to_string(), builder::character_type_default()),
        ]);
        let person = builder::record_type(&[
                ("name".to_string(), builder::character_type_default()),
                ("age".to_string(), builder::integer_type_default()),
        ]);
        assert_eq!(person.is_subtype(&name, &Context::default()), true);
    }

    #[test]
    fn test_type_subtyping1() {
        let t1 = builder::number_type();
        assert_eq!(t1.is_subtype(&t1, &Context::default()), true);
    }

    #[test]
    fn test_tuple_subtyping2(){
        let typ1 = "{int, int}".parse::<Type>().unwrap();
        let typ2 = "{T, T}".parse::<Type>().unwrap();
        assert!(typ1 < typ2)
    }

    #[test]
    fn test_sequence_subtyping1() {
        let s1 = "Seq[0, Empty]".parse::<Type>().unwrap();
        let s2 = "Seq[0, int]".parse::<Type>().unwrap();
        assert!(s1.is_subtype(&s2, &Context::default()),
                "An Empty sequence should be subtype of an empty sequence of a defined type.");
    }

    #[test]
    fn test_interface_and_type_subtyping1() {
        let self_fn_type = builder::function_type(
            &[builder::self_generic_type()],
            builder::character_type_default());
        let int_type = builder::integer_type_default();
        let fn_type = builder::function_type(
            &[int_type.clone()],
            builder::character_type_default());
        let interface = builder::interface_type(&[("view", self_fn_type.clone())]);

        let var = Var::from_name("view").set_type(int_type.clone());
        let ctx = Context::default();
        let context = ctx.clone().push_var_type(var, fn_type, &ctx);

        assert_eq!(int_type.is_subtype(&interface, &context), true);
    }

    #[test]
    fn test_interface_and_type_subtyping2() {
        let self_fn_type = builder::function_type(
            &[builder::self_generic_type()],
            builder::character_type_default());
        let int_type = builder::integer_type_default();
        let fn_type = builder::function_type(
            &[int_type.clone()],
            builder::character_type_default());
        let interface = builder::interface_type(&[("view", self_fn_type.clone())]);

        let var = Var::from_name("view").set_type(int_type.clone());
        let ctx = Context::default();
        let context = ctx.clone()
            .push_var_type(var, fn_type, &ctx)
            .push_alias("Model".to_string(), interface.clone());
        let let_expression = 
            parse("let a: Model <- 10;".into());
        let _ = typing(&context, &let_expression).value;
        assert!(true)
        //assert_eq!(int_type.is_subtype(&interface, &context), true);
    }

    #[test]
    fn test_replace_function_type() {
        let int_type = builder::integer_type_default();
        let fn_type = builder::function_type(
            &[int_type.clone()],
            builder::character_type_default());

        let new_type = fn_type.replace_function_types(int_type, builder::self_generic_type());
        println!("new_type.pretty(): {:?}", new_type.pretty());
        assert!(true);
    }

    #[test]
    fn test_function_subtype_of_unknown_function() {
        let fun = builder::function_type(&[], builder::empty_type());
        let u_fun = builder::unknown_function();
        let context = Context::empty();
        assert!(fun.is_subtype(&u_fun, &context));
    }

    #[test]
    fn test_array_apply1() {
        let fp = FluentParser::new()
            .push("let v1 <- [1, 2, 3];")
            .parse_type_next()
            .push("let toto <- fn(a: int): bool { true };")
            .parse_type_next()
            .push("@apply: ([#N, T], (T) -> U) -> [#N, U];")
            .parse_type_next()
            .push("apply(v1, toto)")
            .parse_type_next();
        println!("fp: {}", fp);
        assert!(true);
    }

}
