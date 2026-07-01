#![allow(dead_code)]
pub mod alias_type;
pub mod argument_type;
pub mod array_type;
pub mod function_type;
pub mod generic;
pub mod index;
pub mod intersection_type;
pub mod kind;
pub mod module_type;
pub mod tbool;
pub mod tchar;
pub mod tint;
pub mod tnumber;
pub mod type_category;
pub mod type_operator;
pub mod type_printer;
pub mod type_system;
pub mod union_type;
pub mod vector_type;

use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::locatable::Locatable;
use crate::components::language::var::Var;
use crate::components::r#type::alias_type::Alias;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::intersection_type::IntersectionType;
use crate::components::r#type::kind::Kind;
use crate::components::r#type::module_type::ModuleType;
use crate::components::r#type::tbool::Tbool;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::tnumber::Tnum;
use crate::components::r#type::type_category::GKind;
use crate::components::r#type::type_category::TypeCategory;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_printer::litteral;
use crate::components::r#type::type_printer::{format, verbose};
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::union_type::UnionType;
use crate::components::r#type::vector_type::VecType;
use crate::processes::parsing::operation_priority::TokenKind;
use crate::processes::parsing::type_token::TypeToken;
use crate::processes::parsing::types::ltype;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::utils::builder;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::str::FromStr;

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
        _ => "overflow",
    }
    .to_string()
}

fn to_string<T: ToString>(v: &[T]) -> String {
    let res = v
        .iter()
        .map(|x| x.to_string())
        .reduce(|acc, x| format!("{}, {}", acc, x))
        .unwrap_or("".to_string());
    format!("[{}]", res)
}

pub fn pretty(hs: HashSet<ArgumentType>) -> String {
    hs.iter()
        .map(|arg_typ| {
            format!(
                "{}: {}",
                arg_typ.get_argument_str(),
                arg_typ.get_type().pretty()
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq)]
pub enum Type {
    Number(Tnum, HelpData),
    Integer(Tint, HelpData),
    Boolean(Tbool, HelpData),
    Char(Tchar, HelpData),
    Function(Vec<ArgumentType>, Box<Type>, HelpData),
    Generic(String, HelpData),
    IndexGen(String, HelpData),
    LabelGen(String, HelpData),
    Vec(VecType, Box<Type>, Box<Type>, HelpData),
    Record(HashSet<ArgumentType>, HelpData),
    Module(Vec<ArgumentType>, Vec<String>, HelpData),
    Alias(String, Vec<Type>, bool, HelpData), //for opacity
    Tag(String, Box<Type>, HelpData),
    Interface(HashSet<ArgumentType>, HelpData),
    Params(Vec<Type>, HelpData),
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
    Null(HelpData),
    NA(HelpData),
    KindedGen(Kind, String, HelpData),
}

impl TypeSystem for Type {
    fn pretty(&self) -> String {
        format(self)
    }

    fn reduce(&self, context: &Context) -> Type {
        reduce_type(context, self)
    }

    fn is_subtype(&self, other: &Type, context: &Context) -> (bool, Option<Context>) {
        // Vérifier le cache
        if let Some(cached) = context.subtypes.check_subtype_cache(self, other) {
            return (cached, None);
        }

        // Calculer le résultat
        let result = self.is_subtype_raw(other, context);

        // Mettre à jour le cache et retourner
        let new_subtypes =
            context
                .subtypes
                .clone()
                .cache_subtype(self.clone(), other.clone(), result);
        let new_context = context.clone().with_subtypes(new_subtypes);

        (result, Some(new_context))
    }

    //main
    fn is_subtype_raw(&self, other: &Type, context: &Context) -> bool {
        match (self, other) {
            (Type::Empty(_), _) => true,
            (typ1, typ2) if typ1 == typ2 => true,
            (Type::Operator(TypeOperator::Intersection, t1, t2, _), typ) => {
                t1.is_subtype_raw(typ, context) || t2.is_subtype_raw(typ, context)
            }
            (_, Type::Any(_)) => true,
            (Type::Vec(_, n1, t1, _), Type::Vec(_, n2, t2, _)) => {
                n1.is_subtype_raw(n2, context) && t1.is_subtype_raw(t2, context)
            }
            (Type::Function(args1, ret_typ1, _), Type::Function(args2, ret_typ2, _)) => {
                let args1_types: Vec<(String, Type)> = args1
                    .iter()
                    .map(|arg| (arg.get_argument_str(), arg.get_type()))
                    .collect();
                let args2_types: Vec<(String, Type)> = args2
                    .iter()
                    .map(|arg| (arg.get_argument_str(), arg.get_type()))
                    .collect();
                if args1_types.len() != args2_types.len() {
                    false
                } else {
                    let ret1 = (**ret_typ1).clone();
                    let ret2 = (**ret_typ2).clone();
                    let mut all_match = true;
                    for (p1, p2) in args1_types.iter().zip(args2_types.iter()) {
                        if !(p1.1.strict_subtype(&p2.1) && p1.0 == p2.0) {
                            all_match = false;
                            break;
                        }
                    }
                    all_match && ret1.strict_subtype(&ret2)
                }
            }
            (_, Type::UnknownFunction(_)) => true,
            (Type::Interface(args1, _), Type::Interface(args2, _)) => {
                args1 == args2 || args1.is_superset(args2)
            }
            (typ, Type::Interface(args2, _)) => match typ.to_interface(context) {
                Type::Interface(args1, _) => &args1 == args2 || args1.is_superset(args2),
                _ => todo!(),
            },
            // Record subtyping
            (Type::Record(r1, _), Type::Record(r2, _)) => r1 == r2 || r1.is_superset(r2),
            (Type::Tag(name1, body1, _h1), Type::Tag(name2, body2, _h2)) => {
                (name1 == name2) && body1.is_subtype_raw(body2, context)
            }
            // Generic subtyping
            (_, Type::Generic(_, _)) => true,
            (Type::Integer(_, _), Type::IndexGen(_, _)) => true,
            (Type::Char(_, _), Type::LabelGen(_, _)) => true,
            (Type::IndexGen(_, _), Type::IndexGen(_, _)) => true,
            (concrete, Type::KindedGen(k, _, _)) if kind::type_kind(concrete) == Some(*k) => true,
            (Type::KindedGen(k1, _, _), Type::KindedGen(k2, _, _)) => k1 == k2,
            // Params subtyping
            (Type::Params(p1, _), Type::Params(p2, _)) => {
                p1.len() == p2.len()
                    && p1
                        .iter()
                        .zip(p2.iter())
                        .all(|(t1, t2)| t1.is_subtype_raw(t2, context))
            }
            (Type::RClass(set1, _), Type::RClass(set2, _)) => set1.is_subset(set2),
            (
                Type::Operator(TypeOperator::Union, _t1, _t2, _),
                Type::Operator(TypeOperator::Union, _tp1, _tp2, _),
            ) => true, //TODO: Fix this
            (typ, Type::Operator(TypeOperator::Union, t1, t2, _)) => {
                typ.is_subtype_raw(t1, context) || typ.is_subtype_raw(t2, context)
            }
            (Type::Char(t1, _), Type::Char(t2, _)) => t1.is_subtype(t2),
            (Type::Integer(_, _), Type::Integer(_, _)) => true,
            (Type::Tuple(types1, _), Type::Tuple(types2, _)) => types1
                .iter()
                .zip(types2.iter())
                .all(|(typ1, typ2)| typ1.is_subtype_raw(typ2, context)),
            // Empty tuple <: empty record (spec §3.2: tuple{} <: list{})
            (Type::Tuple(ts, _), Type::Record(fs, _)) if ts.is_empty() && fs.is_empty() => true,
            (Type::Record(fs, _), Type::Tuple(ts, _)) if fs.is_empty() && ts.is_empty() => true,
            (typ, Type::Operator(TypeOperator::Intersection, t1, t2, _)) => {
                typ.is_subtype_raw(t1, context) && typ.is_subtype_raw(t2, context)
            }
            // Opaque alias is subtype of original alias (e.g., Addable_ -> Addable)
            (Type::Alias(name1, _, true, _), Type::Alias(name2, _, false, _)) => {
                name1 == &format!("{}_", name2)
            }
            // Reduce non-opaque aliases before comparing
            (_, Type::Alias(_, _, false, _)) => {
                let reduced = other.reduce(context);
                if reduced != *other {
                    self.is_subtype_raw(&reduced, context)
                } else {
                    false
                }
            }
            (Type::Alias(_, _, false, _), _) => {
                let reduced = self.reduce(context);
                if reduced != *self {
                    reduced.is_subtype_raw(other, context)
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        builder::any_type()
    }
}

impl Locatable for Type {
    fn get_help_data(&self) -> HelpData {
        Type::get_help_data(self)
    }
}

//main
impl Type {
    pub fn is_alias(&self) -> bool {
        matches!(self, Type::Alias(_, _, _, _))
    }

    pub fn to_alias(self, _context: &Context) -> Option<Alias> {
        match self {
            Type::Alias(name, params, opacity, hd) => Some(Alias::new(name, params, opacity, hd)),
            _ => None,
        }
    }

    pub fn lift(self, max_index: &(VecType, i32)) -> Type {
        match self.clone() {
            Type::Vec(_, i, _, _) if i.equal(max_index.1) => self,
            Type::Vec(v, _, t, h) => Type::Vec(
                v,
                Box::new(builder::integer_type(max_index.1)),
                t.clone(),
                h.clone(),
            ),
            t => Type::Vec(
                max_index.0.clone(),
                Box::new(builder::integer_type(max_index.1)),
                Box::new(t.clone()),
                t.get_help_data(),
            ),
        }
    }

    pub fn equal(&self, i: i32) -> bool {
        match self {
            Type::Integer(t, _) => t.equal(i),
            _ => false,
        }
    }
    pub fn is_interface(&self) -> bool {
        matches!(self, Type::Interface(_, _))
    }

    pub fn add_to_context(self, var: Var, context: Context) -> (Type, Context) {
        let cont = context.clone().push_var_type(
            var.clone().set_type(self.clone()),
            self.clone(),
            &context,
        );
        self.tuple(&cont)
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function(_, _, _) | Type::UnknownFunction(_))
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Type::Boolean(_, _)
                | Type::Number(_, _)
                | Type::Integer(_, _)
                | Type::Char(_, _)
                | Type::Null(_)
                | Type::NA(_)
        )
    }

    pub fn get_covariant_type(&self, annotation: &Type, context: &Context) -> Type {
        let reduced_annotation = annotation.reduce(context);
        let reduced_type = self.reduce(context);
        if !annotation.is_empty() {
            if reduced_type.is_subtype(&reduced_annotation, context).0 {
                annotation.clone()
            } else {
                // Return Any type instead of panicking - the error will be collected at typing level
                Type::Any(self.get_help_data())
            }
        } else {
            self.clone()
        }
    }

    pub fn extract_types(&self) -> Vec<Type> {
        match self {
            Type::Function(args, ret, _) => {
                let mut sol = args.iter().map(|arg| arg.get_type()).collect::<Vec<_>>();
                sol.push((**ret).clone());
                sol.push(self.clone());
                sol
            }
            Type::Module(argtypes, _, _) => {
                let mut sol = argtypes
                    .iter()
                    .map(|argtype| argtype.get_type())
                    .collect::<Vec<_>>();
                sol.push(self.clone());
                sol
            }
            Type::Interface(_, _) => vec![], // there is a special push for this
            typ => vec![typ.clone()],
        }
    }

    pub fn get_label(&self) -> String {
        match self {
            Type::Char(l, _) => l.to_string(),
            Type::LabelGen(l, _) => l.to_string(),
            _ => panic!("The type {} wasn't a label", self),
        }
    }

    pub fn replace_function_types(self, t1: Type, t2: Type) -> Type {
        match self {
            Type::Function(args, ret, h) => {
                let new_args = args
                    .iter()
                    .map(|arg| {
                        if arg.get_type() == t1 {
                            ArgumentType::new(&arg.get_argument_str(), &t2)
                        } else {
                            arg.clone()
                        }
                    })
                    .collect::<Vec<_>>();
                let new_ret = if *ret == t1 { t2 } else { *ret };
                Type::Function(new_args, Box::new(new_ret), h)
            }
            _ => self,
        }
    }

    pub fn without_embeddings(self) -> Type {
        match self {
            Type::Record(args, h) => {
                let new_args = args.iter().map(|arg| arg.remove_embeddings()).collect();
                Type::Record(new_args, h.clone())
            }
            typ => typ,
        }
    }

    pub fn to_typescript(&self) -> String {
        match self {
            Type::Boolean(_, _) => "boolean".to_string(),
            Type::Integer(_, _) => "number".to_string(),
            Type::Number(_, _) => "number".to_string(),
            Type::Char(_, _) => "string".to_string(),
            Type::Record(body, _) => {
                let res = body
                    .iter()
                    .map(|at| format!("{}: {}", at.get_argument(), at.get_type().to_typescript()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{ {} }}", res)
            }
            Type::Vec(_, _size, body, _) => format!("{}[]", body.to_typescript()),
            Type::IndexGen(id, _) => id.to_uppercase(),
            Type::Generic(val, _) => val.to_uppercase(),
            Type::KindedGen(_, name, _) => name.to_uppercase(),
            Type::Function(args, ret, _) => {
                let res = args
                    .iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        format!("{}: {}", generate_arg(i), arg.get_type().to_typescript())
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) => {}", res, ret.to_typescript())
            }
            Type::Tag(name, typ, _) => {
                format!("{{ _type: '{}',  _body: {} }}", name, typ.to_typescript())
            }
            Type::Null(_) => "null".to_string(),
            Type::NA(_) => "NA".to_string(),
            Type::Any(_) => "null".to_string(),
            Type::Empty(_) => "null".to_string(),
            Type::Operator(
                TypeOperator::Addition
                | TypeOperator::Substraction
                | TypeOperator::Multiplication
                | TypeOperator::Division,
                _,
                _,
                _,
            ) => "T".to_string(),
            _ => format!("the type: {} is not yet in to_typescript()", self),
        }
    }

    pub fn to_assemblyscript(&self) -> String {
        match self {
            Type::Boolean(_, _) => "bool".to_string(),
            Type::Integer(_, _) => "i32".to_string(),
            Type::Number(_, _) => "f64".to_string(),
            Type::Char(_, _) => "string".to_string(),
            Type::Record(body, _) => {
                let res = body
                    .iter()
                    .map(|at| format!("{}: {}", at.get_argument(), at.get_type().to_typescript()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{ {} }}", res)
            }
            Type::Vec(_, _size, body, _) => format!("{}[]", body.to_typescript()),
            Type::IndexGen(id, _) => id.to_uppercase(),
            Type::Generic(val, _) => val.to_uppercase(),
            Type::KindedGen(_, name, _) => name.to_uppercase(),
            Type::Function(args, ret, _) => {
                let res = args
                    .iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        format!("{}: {}", generate_arg(i), arg.get_type().to_typescript())
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) => {}", res, ret.to_typescript())
            }
            Type::Tag(name, typ, _) => {
                format!("{{ _type: '{}',  _body: {} }}", name, typ.to_typescript())
            }
            Type::Null(_) => "null".to_string(),
            Type::NA(_) => "NA".to_string(),
            Type::Any(_) => "null".to_string(),
            Type::Empty(_) => "null".to_string(),
            Type::Operator(
                TypeOperator::Addition
                | TypeOperator::Substraction
                | TypeOperator::Multiplication
                | TypeOperator::Division,
                _,
                _,
                _,
            ) => "T".to_string(),
            _ => format!("the type: {} is not yet in to_typescript()", self),
        }
    }

    pub fn extract_generics(&self) -> Vec<Type> {
        match self {
            Type::Generic(_, _)
            | Type::IndexGen(_, _)
            | Type::LabelGen(_, _)
            | Type::KindedGen(_, _, _) => vec![self.clone()],
            Type::Function(args, ret_typ, _) => args
                .iter()
                .flat_map(|arg| arg.get_type().extract_generics())
                .chain(ret_typ.extract_generics())
                .collect::<HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>(),
            Type::Vec(_, ind, typ, _) => typ
                .extract_generics()
                .iter()
                .chain(ind.extract_generics().iter())
                .collect::<HashSet<_>>()
                .into_iter()
                .cloned()
                .collect::<Vec<_>>(),
            Type::Record(v, _) => v
                .iter()
                .flat_map(|argt| [argt.get_argument(), argt.get_type()])
                .flat_map(|typ| typ.extract_generics())
                .collect::<HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>(),
            Type::Operator(TypeOperator::Union, t1, t2, _)
            | Type::Operator(TypeOperator::Intersection, t1, t2, _) => t1
                .extract_generics()
                .into_iter()
                .chain(t2.extract_generics())
                .collect::<HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>(),
            Type::Tag(_, inner, _) => inner.extract_generics(),
            _ => vec![],
        }
    }

    pub fn index_calculation(&self) -> Type {
        match self {
            Type::Operator(TypeOperator::Addition, a, b, _) => {
                a.index_calculation().sum_index(&b.index_calculation())
            }
            Type::Operator(TypeOperator::Substraction, a, b, _) => {
                a.index_calculation().minus_index(&b.index_calculation())
            }
            Type::Operator(TypeOperator::Multiplication, a, b, _) => {
                a.index_calculation().mul_index(&b.index_calculation())
            }
            Type::Operator(TypeOperator::Division, a, b, _) => {
                a.index_calculation().div_index(&b.index_calculation())
            }
            Type::Vec(vtype, ind, typ, h) => Type::Vec(
                vtype.clone(),
                Box::new(ind.index_calculation()),
                Box::new(typ.index_calculation()),
                h.clone(),
            ),
            Type::Function(args, ret_typ, h) => {
                let new_args = args
                    .iter()
                    .map(|arg| arg.clone().index_calculation())
                    .collect::<Vec<_>>();
                Type::Function(new_args, Box::new(ret_typ.index_calculation()), h.clone())
            }
            _ => self.clone(),
        }
    }

    fn sum_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Integer(a, h), Type::Integer(b, _)) => Type::Integer(*a + *b, h.clone()),
            (Type::Record(a, h), Type::Record(b, _)) => Type::Record(
                a.iter().chain(b.iter()).cloned().collect::<HashSet<_>>(),
                h.clone(),
            ),
            _ => Type::Operator(
                TypeOperator::Addition,
                Box::new(self.clone()),
                Box::new(i.clone()),
                HelpData::default(),
            ), //_ => panic!("Type {} and {} can't be added", self, i)
        }
    }

    fn minus_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Integer(a, h), Type::Integer(b, _)) => Type::Integer(*a - *b, h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i),
        }
    }

    fn mul_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Integer(a, h), Type::Integer(b, _)) => Type::Integer(*a * (*b), h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i),
        }
    }

    fn div_index(&self, i: &Type) -> Type {
        match (self, i) {
            (Type::Integer(a, h), Type::Integer(b, _)) => Type::Integer(*a / (*b), h.clone()),
            _ => panic!("Type {} and {} can't be added", self, i),
        }
    }

    pub fn get_shape(&self) -> Option<String> {
        if let Type::Vec(_, i, t, _) = self {
            match (*i.clone(), t.get_shape()) {
                (Type::IndexGen(_, _), _) => Some("dim(===)".to_string()),
                (Type::Integer(j, _), Some(rest)) => Some(format!("{}, {}", j, rest)),
                (Type::Integer(j, _), None) => Some(format!("{}", j)),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn to_function_type(&self) -> Option<FunctionType> {
        match self {
            Type::Function(args, ret_ty, h) => Some(FunctionType::new(
                VecType::Empty,
                args.clone(),
                (**ret_ty).clone(),
                h.clone(),
            )),
            Type::UnknownFunction(h) => Some(FunctionType::new(
                VecType::Empty,
                vec![],
                builder::unknown_function_type(),
                h.clone(),
            )),
            _ => None,
        }
    }

    pub fn get_type_pattern(&self) -> Option<ArgumentType> {
        if let Type::Record(fields, _) = self {
            (fields.len() == 1).then(|| fields.iter().next().unwrap().clone())
        } else {
            None
        }
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Type::Boolean(_, _))
    }

    pub fn dependent_type(&self, dep_typ: &Type) -> bool {
        matches!(
            (dep_typ, self),
            (Type::Integer(_, _), Type::IndexGen(_, _)) | (Type::Char(_, _), Type::LabelGen(_, _))
        )
    }

    pub fn pretty2(&self) -> String {
        verbose(self)
    }

    pub fn pretty3(&self) -> String {
        litteral(self)
    }

    pub fn is_tag_or_union(&self) -> bool {
        matches!(self, Type::Tag(_, _, _))
    }

    pub fn is_generic(&self) -> bool {
        matches!(
            self,
            Type::Generic(_, _)
                | Type::IndexGen(_, _)
                | Type::LabelGen(_, _)
                | Type::KindedGen(_, _, _)
        )
    }

    pub fn exact_equality(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Integer(a, _), Type::Integer(b, _)) => a == b,
            (Type::Char(a, _), Type::Char(b, _)) => a == b,
            (Type::Generic(e1, _), Type::Generic(e2, _)) => e1 == e2,
            (Type::IndexGen(e1, _), Type::IndexGen(e2, _)) => e1 == e2,
            (Type::LabelGen(e1, _), Type::LabelGen(e2, _)) => e1 == e2,
            (Type::KindedGen(k1, e1, _), Type::KindedGen(k2, e2, _)) => k1 == k2 && e1 == e2,
            _ => self == other,
        }
    }

    pub fn for_var(self) -> Type {
        match self.to_owned() {
            Type::Function(p, _r, _h) => {
                if !p.is_empty() {
                    p[0].get_type()
                } else {
                    self
                }
            }
            t => t,
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
            Type::Boolean(_, _) => TypeCategory::Boolean,
            Type::Number(_, _) => TypeCategory::Number,
            Type::Char(_, _) => TypeCategory::Char,
            Type::Generic(_, _) => TypeCategory::Generic,
            Type::LabelGen(_, _) => TypeCategory::Generic,
            Type::IndexGen(_, _) => TypeCategory::GenericKinded(GKind::Number),
            Type::KindedGen(k, _, _) => TypeCategory::GenericKinded(GKind::from_kind(*k)),
            Type::Integer(_, _) => TypeCategory::Integer,
            Type::Alias(_, _, false, _) => TypeCategory::Alias,
            Type::Alias(name, _, _, _) => TypeCategory::Opaque(name.clone()),
            Type::Any(_) => TypeCategory::Any,
            Type::Empty(_) => TypeCategory::Empty,
            Type::RClass(_, _) => TypeCategory::RClass,
            Type::UnknownFunction(_) => TypeCategory::RFunction,
            Type::Module(_, _, _) => TypeCategory::Module,
            Type::Null(_) => TypeCategory::Null,
            Type::NA(_) => TypeCategory::Null,
            Type::Operator(TypeOperator::Union, _, _, _) => TypeCategory::Union,
            Type::Operator(TypeOperator::Intersection, _, _, _) => TypeCategory::Intersection,
            Type::Operator(
                TypeOperator::Addition
                | TypeOperator::Substraction
                | TypeOperator::Multiplication
                | TypeOperator::Division,
                _,
                _,
                _,
            ) => TypeCategory::Template,
            Type::Operator(_, _, _, _) => TypeCategory::Operator,
            _ => TypeCategory::Rest,
        }
    }

    // for removing litteral data for Integer and Char
    pub fn generalize(self) -> Type {
        match self {
            Type::Integer(Tint::Val(_), h) => Type::Integer(Tint::Unknown, h),
            Type::Char(Tchar::Val(_), h) => Type::Char(Tchar::Unknown, h),
            t => t,
        }
    }

    fn get_type_shape(&self) -> usize {
        match self {
            Type::Function(v, _, _) => v.len(),
            Type::Tuple(v, _) => v.len(),
            _ => 0,
        }
    }

    pub fn same_shape(&self, other: &Type) -> bool {
        self.get_type_shape() == other.get_type_shape()
    }

    pub fn get_help_data(&self) -> HelpData {
        match self {
            Type::Integer(_, h) => h.clone(),
            Type::Number(_, h) => h.clone(),
            Type::Char(_, h) => h.clone(),
            Type::Boolean(_, h) => h.clone(),
            Type::Function(_, _, h) => h.clone(),
            Type::Generic(_, h) => h.clone(),
            Type::IndexGen(_, h) => h.clone(),
            Type::LabelGen(_, h) => h.clone(),
            Type::Vec(_, _, _, h) => h.clone(),
            Type::Record(_, h) => h.clone(),
            Type::Module(_, _, h) => h.clone(),
            Type::Alias(_, _, _, h) => h.clone(),
            Type::Tag(_, _, h) => h.clone(),
            Type::Interface(_, h) => h.clone(),
            Type::Params(_, h) => h.clone(),
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
            Type::Operator(_, _, _, h) => h.clone(),
            Type::Variable(_, h) => h.clone(),
            Type::Null(h) => h.clone(),
            Type::NA(h) => h.clone(),
            Type::KindedGen(_, _, h) => h.clone(),
        }
    }

    pub fn set_help_data(self, h2: HelpData) -> Type {
        match self {
            Type::Integer(i, _) => Type::Integer(i, h2),
            Type::Number(n, _) => Type::Number(n, h2),
            Type::Char(a, _) => Type::Char(a, h2),
            Type::Boolean(b, _) => Type::Boolean(b, h2),
            Type::Function(a2, a3, _) => Type::Function(a2, a3, h2),
            Type::Generic(a, _) => Type::Generic(a, h2),
            Type::IndexGen(a, _) => Type::IndexGen(a, h2),
            Type::LabelGen(a, _) => Type::LabelGen(a, h2),
            Type::Vec(vt, a1, a2, _) => Type::Vec(vt, a1, a2, h2),
            Type::Record(a, _) => Type::Record(a, h2),
            Type::Module(a, b, _) => Type::Module(a, b, h2),
            Type::Alias(a1, a2, a3, _) => Type::Alias(a1, a2, a3, h2),
            Type::Tag(a1, a2, _) => Type::Tag(a1, a2, h2),
            Type::Interface(a, _) => Type::Interface(a, h2),
            Type::Params(a, _) => Type::Params(a, h2),
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
            Type::Operator(op, t1, t2, _) => Type::Operator(op, t1, t2, h2),
            Type::Variable(name, _) => Type::Variable(name, h2),
            Type::Null(_) => Type::Null(h2),
            Type::NA(_) => Type::NA(h2),
            Type::KindedGen(k, a, _) => Type::KindedGen(k, a, h2),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Type::Empty(_))
    }

    pub fn unlift(self) -> Type {
        match self {
            Type::Vec(_, _, t, _) => (*t).clone(),
            a => a.clone(),
        }
    }

    pub fn to_array(&self) -> Option<Array> {
        match self {
            Type::Vec(_, t1, t2, h) => Some(Array {
                index: (**t1).clone(),
                base_type: (**t2).clone(),
                help_data: h.clone(),
            }),
            _ => None,
        }
    }

    pub fn to_array2(args: Vec<Type>) -> Type {
        let h = HelpData::default();
        if args.len() > 1 {
            Type::Vec(
                VecType::S3,
                Box::new(args[0].clone()),
                Box::new(Self::to_array2(args[1..].to_vec())),
                h,
            )
        } else {
            Type::Vec(
                VecType::S3,
                Box::new(args[0].clone()),
                Box::new(builder::integer_type_default()),
                h,
            )
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
            Type::Integer(i, _) => Some(i.get_value().map(|x| x as u32).unwrap_or(0_u32)),
            Type::IndexGen(_, _) => Some(0_u32),
            _ => None,
        }
    }

    /// Normalize parameter names in a function type to generated names ("a", "b", …).
    /// This ensures interface method signatures compare equal regardless of the
    /// parameter names used in the original function definition.
    pub fn normalize_fn_param_names(self) -> Type {
        match self {
            Type::Function(params, ret, h) => {
                let new_params = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| ArgumentType::new(&generate_arg(i), &p.get_type()))
                    .collect();
                Type::Function(new_params, ret, h)
            }
            other => other,
        }
    }

    pub fn to_interface(&self, context: &Context) -> Type {
        match self {
            Type::Interface(_, _) => self.clone(),
            Type::Function(_, _, _) => {
                builder::interface_type(&[("_", builder::unknown_function_type())])
            }
            typ => {
                let function_signatures = context
                    .get_functions_from_type(typ)
                    .iter()
                    .cloned()
                    .map(|(var, typ2)| {
                        let replaced =
                            typ2.replace_function_types(typ.clone(), builder::self_generic_type());
                        (var.get_name(), replaced.normalize_fn_param_names())
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
            _ => false,
        }
    }

    pub fn get_first_function_parameter_type(&self, name: &str) -> Option<Type> {
        match self {
            Type::Module(args, _, _) => {
                let res = args
                    .iter()
                    .find(|arg_typ| arg_typ.get_argument_str() == name);
                match res {
                    Some(arg_typ) => arg_typ.get_type().get_first_parameter(),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn get_first_parameter(&self) -> Option<Type> {
        match self {
            Type::Function(args, _, _) => args.iter().next().map(|arg| arg.get_type()),
            _ => None,
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
            Type::Module(args, priv_names, h) => Ok(ModuleType::from((args, priv_names, h))),
            _ => Err(format!("{} can't be turn into a ModuleType", self.pretty())),
        }
    }

    pub fn linearize(self) -> Vec<Type> {
        match self {
            Type::Vec(_, t1, t2, _) => [*t1]
                .iter()
                .chain((*t2).linearize().iter())
                .cloned()
                .collect(),
            other => vec![other],
        }
    }

    pub fn is_upperrank_of(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Vec(_, _, typ1, _), typ2) => **typ1 == *typ2,
            _ => false,
        }
    }

    pub fn has_generic(&self) -> bool {
        !self.extract_generics().is_empty()
    }

    pub fn has_operation(&self) -> bool {
        false
    }

    pub fn is_reduced(&self) -> bool {
        !self.has_generic() && !self.has_operation()
    }

    /// Help knowing the rank of a parameter according to it's vectorial property
    /// int -> rank: 1, vector type: any, Type int
    /// [3, T] -> rank: 3, vector type: Array, Type T
    pub fn get_size_type(&self) -> (i32, VecType, Type) {
        match self {
            Type::Vec(v, i, t, _) => (i.get_index().unwrap_or(0) as i32, v.clone(), (**t).clone()),
            typ => (1, VecType::Empty, typ.clone()),
        }
    }

    /// Recursively collect every `VecType::Named` record-constructor usage in this
    /// type, returning each `(constructor_name, location)`. Used to validate usages
    /// against the declared `typeconstructor` registry.
    pub fn collect_named_constructors(&self) -> Vec<(String, HelpData)> {
        let mut acc = Vec::new();
        self.collect_named_constructors_into(&mut acc);
        acc
    }

    fn collect_named_constructors_into(&self, acc: &mut Vec<(String, HelpData)>) {
        match self {
            Type::Vec(vtype, idx, body, h) => {
                if let VecType::Named(name) = vtype {
                    acc.push((name.clone(), h.clone()));
                }
                idx.collect_named_constructors_into(acc);
                body.collect_named_constructors_into(acc);
            }
            Type::Function(args, ret, _) => {
                args.iter()
                    .for_each(|a| a.get_type().collect_named_constructors_into(acc));
                ret.collect_named_constructors_into(acc);
            }
            Type::Record(fields, _) | Type::Interface(fields, _) => fields
                .iter()
                .for_each(|a| a.get_type().collect_named_constructors_into(acc)),
            Type::Tag(_, inner, _) | Type::Multi(inner, _) => {
                inner.collect_named_constructors_into(acc)
            }
            Type::Operator(_, a, b, _) => {
                a.collect_named_constructors_into(acc);
                b.collect_named_constructors_into(acc);
            }
            Type::Params(ts, _) | Type::Tuple(ts, _) => ts
                .iter()
                .for_each(|t| t.collect_named_constructors_into(acc)),
            Type::Alias(_, params, _, _) => params
                .iter()
                .for_each(|t| t.collect_named_constructors_into(acc)),
            _ => {}
        }
    }

    pub fn is_unknown_function(&self) -> bool {
        matches!(self, Type::UnknownFunction(_))
    }

    pub fn is_vector_of(&self, other: &Type, context: &Context) -> bool {
        let reduced_self = self.reduce(context);
        let reduced_other = other.reduce(context);
        match reduced_self {
            Type::Vec(_, _, t, _) => *t == reduced_other,
            _ => false,
        }
    }
}

pub struct Array {
    pub index: Type,
    pub base_type: Type,
    pub help_data: HelpData,
}

impl From<Vec<Type>> for HelpData {
    fn from(val: Vec<Type>) -> Self {
        if !val.is_empty() {
            val[0].clone().into()
        } else {
            HelpData::default()
        }
    }
}

impl From<Type> for HelpData {
    fn from(val: Type) -> Self {
        match val {
            Type::Empty(h) => h,
            Type::Generic(_, h) => h,
            Type::IndexGen(_, h) => h,
            Type::Operator(_, _, _, h) => h,
            Type::Tag(_, _, h) => h,
            Type::Function(_, _, h) => h,
            Type::Char(_, h) => h,
            Type::Integer(_, h) => h,
            Type::Record(_, h) => h,
            Type::Boolean(_, h) => h,
            Type::Vec(_, _, _, h) => h,
            Type::Number(_, h) => h,
            Type::Any(h) => h,
            Type::UnknownFunction(h) => h,
            Type::Null(h) => h,
            Type::NA(h) => h,
            Type::KindedGen(_, _, h) => h,
            e => panic!("The type element {:?} is not yet implemented", e),
        }
        .clone()
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Number(n1, _), Type::Number(n2, _)) => n1 == n2,
            (Type::Integer(_, _), Type::Integer(_, _)) => true,
            (Type::Boolean(b1, _), Type::Boolean(b2, _)) => b1 == b2,
            (Type::Char(t1, _), Type::Char(t2, _)) => t1 == t2,
            (Type::Function(b1, c1, _), Type::Function(b2, c2, _)) => b1 == b2 && c1 == c2,
            (Type::Generic(_, _), Type::Generic(_, _)) => true,
            (Type::IndexGen(_, _), Type::IndexGen(_, _)) => true,
            (Type::LabelGen(_, _), Type::LabelGen(_, _)) => true,
            (Type::KindedGen(k1, _, _), Type::KindedGen(k2, _, _)) => k1 == k2,
            (Type::Vec(_, a1, b1, _), Type::Vec(_, a2, b2, _)) => a1 == a2 && b1 == b2,
            (Type::Record(e1, _), Type::Record(e2, _)) => e1 == e2,
            (Type::Alias(a1, b1, c1, _), Type::Alias(a2, b2, c2, _)) => {
                a1 == a2 && b1 == b2 && c1 == c2
            }
            (Type::Tag(a1, b1, _), Type::Tag(a2, b2, _)) => a1 == a2 && b1 == b2,
            (Type::Interface(e1, _), Type::Interface(e2, _)) => e1 == e2,
            (Type::Params(e1, _), Type::Params(e2, _)) => e1 == e2,
            (Type::Failed(e1, _), Type::Failed(e2, _)) => e1 == e2,
            (Type::Opaque(e1, _), Type::Opaque(e2, _)) => e1 == e2,
            (Type::Multi(e1, _), Type::Multi(e2, _)) => e1 == e2,
            (Type::Tuple(e1, _), Type::Tuple(e2, _)) => e1 == e2,
            (Type::If(a1, b1, _), Type::If(a2, b2, _)) => a1 == a2 && b1 == b2,
            (Type::Condition(a1, b1, c1, _), Type::Condition(a2, b2, c2, _)) => {
                a1 == a2 && b1 == b2 && c1 == c2
            }
            (Type::In(_), Type::In(_)) => true,
            (Type::Empty(_), Type::Empty(_)) => true,
            (Type::Any(_), Type::Any(_)) => true,
            (Type::RClass(el1, _), Type::RClass(el2, _)) => el1.difference(el2).next().is_none(),
            (Type::Variable(s1, _), Type::Variable(s2, _)) => s1 == s2,
            (Type::Null(_), Type::Null(_)) => true,
            (Type::NA(_), Type::NA(_)) => true,
            (Type::UnknownFunction(_), Type::UnknownFunction(_)) => true,
            (Type::UnknownFunction(_), Type::Empty(_)) => true,
            (Type::Empty(_), Type::UnknownFunction(_)) => true,
            (
                Type::Operator(TypeOperator::Union, _, _, _),
                Type::Operator(TypeOperator::Union, _, _, _),
            ) => UnionType::try_from(self.clone()).ok() == UnionType::try_from(other.clone()).ok(),
            (
                Type::Operator(TypeOperator::Intersection, _, _, _),
                Type::Operator(TypeOperator::Intersection, _, _, _),
            ) => {
                IntersectionType::try_from(self.clone()).ok()
                    == IntersectionType::try_from(other.clone()).ok()
            }
            (Type::Operator(op1, a1, b1, _), Type::Operator(op2, a2, b2, _)) => {
                op1 == op2 && a1 == a2 && b1 == b2
            }
            (Type::Module(a1, _, _), Type::Module(a2, _, _)) => a1 == a2,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Function(p, r, h) => {
                let p_types: Vec<Type> = p.iter().map(|arg| arg.get_type()).collect();
                write!(f, "({}) -> {}", Type::Params(p_types, h.clone()), r)
            }
            Type::Params(v, _) => {
                let res = v
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}", res)
            }
            _ => write!(f, "{}", format(self)),
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
            (Type::Vec(_, n1, t1, _), Type::Vec(_, n2, t2, _)) => (n1.partial_cmp(n2).is_some()
                && t1.partial_cmp(t2).is_some())
            .then_some(Ordering::Less),
            (Type::Function(args1, ret_typ1, _), Type::Function(args2, ret_typ2, _)) => {
                let args1_types: Vec<Type> = args1.iter().map(|arg| arg.get_type()).collect();
                let args2_types: Vec<Type> = args2.iter().map(|arg| arg.get_type()).collect();
                let ret1 = (**ret_typ1).clone();
                let ret2 = (**ret_typ2).clone();
                let mut all_match = true;
                for (t1, t2) in args1_types.iter().zip(args2_types.iter()) {
                    if t1.partial_cmp(t2).is_none() {
                        all_match = false;
                        break;
                    }
                }
                if all_match && ret1.partial_cmp(&ret2).is_some() {
                    Some(Ordering::Less)
                } else {
                    None
                }
            }
            (Type::Function(_, _, _), Type::UnknownFunction(_)) => Some(Ordering::Less),
            // Interface subtyping
            (Type::Interface(args1, _), Type::Interface(args2, _)) => {
                (args1 == args2 || args1.is_superset(args2)).then_some(Ordering::Less)
            }
            // Record subtyping
            (Type::Record(r1, _), Type::Record(r2, _)) => {
                (r1 == r2 || r1.is_superset(r2)).then_some(Ordering::Less)
            }

            (Type::Tag(name1, body1, _h1), Type::Tag(name2, body2, _h2)) => {
                ((name1 == name2) && body1.partial_cmp(body2).is_some()).then_some(Ordering::Less)
            }

            // Generic subtyping
            (_, Type::Generic(_, _)) => Some(Ordering::Less),
            (Type::Integer(_, _), Type::IndexGen(_, _)) => Some(Ordering::Less),
            (Type::Char(_, _), Type::LabelGen(_, _)) => Some(Ordering::Less),
            (Type::IndexGen(_, _), Type::IndexGen(_, _)) => Some(Ordering::Less),
            (concrete, Type::KindedGen(k, _, _)) if kind::type_kind(concrete) == Some(*k) => {
                Some(Ordering::Less)
            }
            (Type::KindedGen(k1, _, _), Type::KindedGen(k2, _, _)) if k1 == k2 => {
                Some(Ordering::Less)
            }

            // Params subtyping
            (Type::Params(p1, _), Type::Params(p2, _)) => (p1.len() == p2.len()
                && p1
                    .iter()
                    .zip(p2.iter())
                    .all(|(t1, t2)| t1.partial_cmp(t2).is_some()))
            .then_some(Ordering::Less),

            (Type::RClass(set1, _), Type::RClass(set2, _)) => {
                set1.is_subset(set2).then_some(Ordering::Less)
            }
            (Type::Char(_, _), Type::Char(_, _)) => Some(Ordering::Less),
            (Type::Integer(_, _), Type::Integer(_, _)) => Some(Ordering::Less),
            (Type::Tuple(types1, _), Type::Tuple(types2, _)) => types1
                .iter()
                .zip(types2.iter())
                .all(|(typ1, typ2)| typ1.partial_cmp(typ2).is_some())
                .then_some(Ordering::Less),
            (typ, Type::Operator(TypeOperator::Intersection, _, _, _)) => {
                IntersectionType::try_from(other.clone())
                    .map(|intersection| {
                        intersection
                            .get_types()
                            .iter()
                            .all(|typ2| typ.partial_cmp(typ2) == Some(Ordering::Less))
                    })
                    .unwrap_or(false)
                    .then_some(Ordering::Less)
            }
            _ => None,
        }
    }
}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Utiliser un discriminant pour différencier les variantes
        match self {
            Type::Number(n, _) => {
                0.hash(state);
                n.hash(state);
            }
            Type::Integer(_, _) => 1.hash(state),
            Type::Boolean(b, _) => {
                2.hash(state);
                b.hash(state);
            }
            Type::Char(_, _) => 3.hash(state),
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
            Type::Module(_, _, _) => 37.hash(state),
            Type::Operator(_, _, _, _) => 38.hash(state),
            Type::Variable(_, _) => 39.hash(state),
            Type::Null(_) => 40.hash(state),
            Type::NA(_) => 41.hash(state),
            Type::KindedGen(k, _, _) => {
                42.hash(state);
                k.hash(state);
            }
        }
    }
}

pub fn display_types(v: &[Type]) -> String {
    v.iter().map(|x| x.pretty()).collect::<Vec<_>>().join(" | ")
}

impl From<FunctionType> for Type {
    fn from(val: FunctionType) -> Self {
        let args: Vec<ArgumentType> = val
            .get_param_types()
            .iter()
            .enumerate()
            .map(|(i, typ)| {
                let arg_name = generate_arg(i);
                ArgumentType::new(&arg_name, typ)
            })
            .collect();
        Type::Function(args, Box::new(val.get_return_type()), val.get_help_data())
    }
}

#[derive(Debug)]
pub struct ErrorStruct;

impl FromStr for Type {
    type Err = ErrorStruct;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let val = ltype(s.into())
            .map(|x| x.1)
            .unwrap_or(builder::unknown_function_type());
        Ok(val)
    }
}

impl From<TokenKind> for Type {
    fn from(val: TokenKind) -> Self {
        match val {
            TokenKind::Empty => builder::unknown_function_type(),
            _ => builder::any_type(),
        }
    }
}

impl From<TypeToken> for Type {
    fn from(val: TypeToken) -> Self {
        match val {
            TypeToken::Expression(typ) => typ,
            _ => builder::unknown_function_type(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::processes::parsing::parse;
    use crate::processes::type_checking::typing;
    use crate::utils::fluent_parser::FluentParser;

    /// Regression: `extract_generics` on `Type::Function` used to chain the
    /// literal return type into the result instead of its own extracted
    /// generics, so any concrete (non-generic) function type — e.g.
    /// `(int, int, int) -> int` — was wrongly reported as `has_generic() ==
    /// true`. That false positive made `Context::get_type_anotations()`
    /// skip emitting the `as.FunctionN <- ...` cast for the function's own
    /// auto-registered `FunctionN` alias, while the transpiler still
    /// referenced `as.FunctionN()` at the call site — an "could not find
    /// function as.FunctionN" error at R runtime for any plain typed
    /// function declaration built through the real `typr build` pipeline.
    #[test]
    fn test_concrete_function_type_has_no_generic() {
        let concrete_fn = builder::function_type(
            &[
                builder::integer_type_default(),
                builder::integer_type_default(),
            ],
            builder::integer_type_default(),
        );
        assert!(
            !concrete_fn.has_generic(),
            "a function type built entirely from concrete types must not be reported as generic"
        );
    }

    /// A generic nested inside the return type (not bare) must still be
    /// found — `extract_generics` recurses into the return type rather than
    /// taking it verbatim.
    #[test]
    fn test_function_type_finds_generic_nested_in_return_type() {
        let generic_t = Type::Generic("T".to_string(), HelpData::default());
        let fn_with_generic_return = builder::function_type(
            &[builder::integer_type_default()],
            builder::array_type(
                Type::IndexGen("N".to_string(), HelpData::default()),
                generic_t,
            ),
        );
        assert!(
            fn_with_generic_return.has_generic(),
            "a generic nested inside the return type must still be detected"
        );
    }

    #[test]
    fn test_sigil_generics_get_kinded_category() {
        let h = HelpData::default();
        let n = "T".to_string();
        assert_eq!(
            Type::KindedGen(Kind::Record, n.clone(), h.clone()).to_category(),
            TypeCategory::GenericKinded(GKind::Record)
        );
        assert_eq!(
            Type::KindedGen(Kind::Interface, n.clone(), h.clone()).to_category(),
            TypeCategory::GenericKinded(GKind::Interface)
        );
        assert_eq!(
            Type::KindedGen(Kind::String, n.clone(), h.clone()).to_category(),
            TypeCategory::GenericKinded(GKind::String)
        );
        assert_eq!(
            Type::KindedGen(Kind::Boolean, n.clone(), h.clone()).to_category(),
            TypeCategory::GenericKinded(GKind::Boolean)
        );
        // `#N` / IndexGen is the Number-kinded generic category.
        assert_eq!(
            Type::IndexGen(n.clone(), h.clone()).to_category(),
            TypeCategory::GenericKinded(GKind::Number)
        );
        // bare `T` and label generics stay plain Generic.
        assert_eq!(
            Type::Generic(n.clone(), h.clone()).to_category(),
            TypeCategory::Generic
        );
        assert_eq!(Type::LabelGen(n, h).to_category(), TypeCategory::Generic);
    }

    #[test]
    fn test_record_hierarchy1() {
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
        let name = builder::record_type(&[("name".to_string(), builder::character_type_default())]);
        let person = builder::record_type(&[
            ("name".to_string(), builder::character_type_default()),
            ("age".to_string(), builder::integer_type_default()),
        ]);
        assert_eq!(person.is_subtype(&name, &Context::default()).0, true);
    }

    #[test]
    fn test_type_subtyping1() {
        let t1 = builder::number_type();
        assert_eq!(t1.is_subtype(&t1, &Context::default()).0, true);
    }

    #[test]
    fn test_interface_and_type_subtyping1() {
        let self_fn_type = builder::function_type(
            &[builder::self_generic_type()],
            builder::character_type_default(),
        );
        let int_type = builder::integer_type_default();
        let fn_type =
            builder::function_type(&[int_type.clone()], builder::character_type_default());
        let interface = builder::interface_type(&[("view", self_fn_type.clone())]);

        let var = Var::from_name("view").set_type(int_type.clone());
        let ctx = Context::default();
        let context = ctx.clone().push_var_type(var, fn_type, &ctx);

        assert_eq!(int_type.is_subtype(&interface, &context).0, true);
    }

    #[test]
    fn test_interface_and_type_subtyping2() {
        let self_fn_type = builder::function_type(
            &[builder::self_generic_type()],
            builder::character_type_default(),
        );
        let int_type = builder::integer_type_default();
        let fn_type =
            builder::function_type(&[int_type.clone()], builder::character_type_default());
        let interface = builder::interface_type(&[("view", self_fn_type.clone())]);

        let var = Var::from_name("view").set_type(int_type.clone());
        let ctx = Context::default();
        let context = ctx
            .clone()
            .push_var_type(var, fn_type, &ctx)
            .push_alias("Model".to_string(), interface.clone());
        let let_expression = parse("let a: Model <- 10;".into()).ast;
        let _ = typing(&context, &let_expression).value;
        assert!(int_type.is_subtype(&interface, &context).0);
    }

    #[test]
    fn test_replace_function_type() {
        let int_type = builder::integer_type_default();
        let fn_type =
            builder::function_type(&[int_type.clone()], builder::character_type_default());

        let new_type = fn_type.replace_function_types(int_type, builder::self_generic_type());
        assert_eq!(
            new_type,
            builder::function_type(
                &[builder::self_generic_type()],
                builder::character_type_default()
            )
        );
    }

    #[test]
    fn test_function_subtype_of_unknown_function() {
        let fun = builder::function_type(&[], builder::empty_type());
        let u_fun = builder::unknown_function_type();
        let context = Context::empty();
        assert!(fun.is_subtype(&u_fun, &context).0);
    }

    #[test]
    fn test_array_apply1() {
        // `@apply: ([#N, T], (T) -> U) -> [#N, U]` with `toto: (int) -> bool`
        // should return `[3, bool]` (U bound to toto's return type).
        let fp = FluentParser::new()
            .push("let v1 <- [1, 2, 3];")
            .parse_type_next()
            .push("let toto <- fn(a: int): bool { true };")
            .parse_type_next()
            .push("@apply: ([#N, T], (T) -> U) -> [#N, U];")
            .parse_type_next()
            .push("apply(v1, toto)")
            .parse_type_next();
        assert_eq!(
            fp.get_last_type(),
            builder::array_type2(3, builder::boolean_type())
        );
    }

    #[test]
    fn test_litteral_subtyping1() {
        assert!(
            builder::character_type("hello")
                .is_subtype(&builder::character_type_default(), &Context::empty())
                .0
        );
    }

    #[test]
    fn test_litteral_subtyping2() {
        assert!(
            builder::character_type("hello")
                .is_subtype(&builder::character_type("hello"), &Context::empty())
                .0
        );
    }

    #[test]
    fn test_litteral_union_subtyping1() {
        let my_union = builder::union_type(&[
            builder::character_type("html"),
            builder::character_type("h1"),
        ]);
        assert!(
            builder::character_type("h1")
                .is_subtype(&my_union, &Context::empty())
                .0
        );
    }

    #[test]
    fn test_litteral_union_subtyping2() {
        let my_union = "\"html\" | \"h1\"".parse::<Type>().unwrap();
        assert!(
            builder::character_type("h1")
                .is_subtype(&my_union, &Context::empty())
                .0
        );
    }

    #[test]
    fn test_litteral_subtyping3() {
        assert!(
            !builder::character_type("html")
                .is_subtype(&builder::character_type("h1"), &Context::empty())
                .0
        );
    }

    #[test]
    fn test_intersection_pretty() {
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let b = builder::record_type(&[("y".to_string(), builder::character_type_default())]);
        let inter = builder::intersection_type(&[a, b]);
        assert!(matches!(
            inter,
            Type::Operator(TypeOperator::Intersection, _, _, _)
        ));
        let pretty = inter.pretty();
        assert!(pretty.contains(" & "));
    }

    #[test]
    fn test_intersection_equality_is_order_independent() {
        let a = builder::integer_type_default();
        let b = builder::character_type_default();
        let ab = builder::intersection_type(&[a.clone(), b.clone()]);
        let ba = builder::intersection_type(&[b, a]);
        assert_eq!(ab, ba);
    }

    #[test]
    fn test_intersection_subtype_self_side() {
        // (Record{x} & Record{y}) <: Record{x}: true if either member is a subtype.
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let b = builder::record_type(&[("y".to_string(), builder::character_type_default())]);
        let inter = builder::intersection_type(&[a.clone(), b]);
        assert!(inter.is_subtype(&a, &Context::empty()).0);
    }

    #[test]
    fn test_intersection_subtype_right_side() {
        // Record{x, y} <: (Record{x} & Record{y}): true only if subtype of both members.
        let a = builder::record_type(&[("x".to_string(), builder::integer_type_default())]);
        let b = builder::record_type(&[("y".to_string(), builder::character_type_default())]);
        let xy = builder::record_type(&[
            ("x".to_string(), builder::integer_type_default()),
            ("y".to_string(), builder::character_type_default()),
        ]);
        let inter = builder::intersection_type(&[a.clone(), b]);
        assert!(xy.is_subtype(&inter, &Context::empty()).0);
        assert!(!a.is_subtype(&inter, &Context::empty()).0);
    }

    #[test]
    fn test_union_equality_is_order_independent() {
        let a = builder::character_type("html");
        let b = builder::character_type("h1");
        let ab = builder::union_type(&[a.clone(), b.clone()]);
        let ba = builder::union_type(&[b, a]);
        assert_eq!(ab, ba);
    }

    #[test]
    fn test_union_inequality_with_different_members() {
        let a = builder::character_type("html");
        let b = builder::character_type("h1");
        let c = builder::character_type("h2");
        let ab = builder::union_type(&[a.clone(), b]);
        let ac = builder::union_type(&[a, c]);
        assert_ne!(ab, ac);
    }
}
