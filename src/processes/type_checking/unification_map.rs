use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::type_error::TypeError;
use crate::components::r#type::type_system::TypeSystem;
use crate::processes::type_checking::unification;
use crate::components::context::Context;
use crate::components::r#type::Type;
use std::collections::HashSet;
use crate::utils::builder;
use std::fmt;

#[derive(Debug)]
struct SafeHashMap {
    map: Vec<(Type, Type)>,
}

impl SafeHashMap {
    fn new() -> Self {
        SafeHashMap { map: vec![] }
    }

    fn insert(&mut self, key: Type, value: Type) {
        match self.map.iter().find(|(k, _v)| k == &key) {
            Some((Type::Generic(_, _), Type::Integer(_, _))) => {
                self.map.push((key, value.generalize()))
            }
            Some((_ke, va)) => {
                if !(va.exact_equality(&value)) {
                    None.expect(&TypeError::Param(va.to_owned(), value).display())
                }
            }
            None => self.map.push((key, value)),
        }
    }

    fn to_vec(self) -> Vec<(Type, Type)> {
        self.map.clone()
    }
}

#[derive(Debug)]
pub struct UnificationMap {
    pub mapping: Vec<(Type, Type)>,
    pub vectorized: Option<i32>,
}

impl UnificationMap {
    pub fn new(v: Vec<(Type, Type)>) -> Self {
        let mut safe_map = SafeHashMap::new();
        for (key, val) in v {
            safe_map.insert(key, val);
        }
        UnificationMap {
            mapping: safe_map.to_vec(),
            vectorized: None,
        }
    }

    pub fn set_vectorized(self, index: i32) -> Self {
        Self {
            vectorized: Some(index),
            ..self
        }
    }

    pub fn is_vectorized(&self) -> bool {
        self.vectorized.is_some()
    }

    pub fn get_vectorization(&self) -> Option<i32> {
        self.vectorized
    }

    pub fn superficial_substitution(&self, ret_ty: &Type) -> Type {
        self.mapping
            .iter()
            .find(|(typ1, _)| ret_ty == typ1)
            .map(|(_, typ2)| typ2.clone())
            .unwrap_or(ret_ty.clone())
    }

    pub fn type_substitution(&self, ret_ty: &Type) -> Type {
        let ret_ty = self.superficial_substitution(ret_ty);
        unification::type_substitution(&ret_ty, &self.mapping)
    }

    pub fn apply_unification_type(&self, context: &Context, ret_ty: &Type) -> (Type, Context) {
        let ret_ty = ret_ty.reduce(context);
        let new_type = self.type_substitution(&ret_ty).index_calculation();
        (new_type, context.clone())
    }

    pub fn append(self, other: Self) -> Self {
        Self {
            mapping: self
                .mapping
                .iter()
                .chain(other.mapping.iter())
                .cloned()
                .collect::<Vec<_>>(),
            vectorized: self.vectorized.or(other.vectorized),
        }
    }
}

impl std::iter::FromIterator<(Type, Type)> for UnificationMap {
    fn from_iter<I: IntoIterator<Item = (Type, Type)>>(iter: I) -> Self {
        UnificationMap {
            mapping: iter.into_iter().collect(),
            vectorized: None,
        }
    }
}

impl From<Vec<Vec<(Type, Type)>>> for UnificationMap {
    fn from(val: Vec<Vec<(Type, Type)>>) -> Self {
        val.iter().cloned().flatten().collect::<UnificationMap>()
    }
}

impl From<HashSet<(i32, Type)>> for UnificationMap {
    fn from(val: HashSet<(i32, Type)>) -> Self {
        let res = val
            .iter()
            .map(|(i, typ)| (typ.clone(), builder::array_type2(i.clone(), typ.clone())))
            .collect::<Vec<_>>();
        UnificationMap {
            mapping: res,
            vectorized: None,
        }
    }
}

impl fmt::Display for UnificationMap {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = self
            .mapping
            .iter()
            .map(|(ty1, ty2)| format!("{} = {}", ty1.pretty(), ty2.pretty2()))
            .fold("".to_string(), |acc, mapp| acc + " | " + &mapp);
        write!(f, "{}", res)
    }
}

impl Default for UnificationMap {
    fn default() -> Self {
        Self {
            mapping: vec![],
            vectorized: None,
        }
    }
}
