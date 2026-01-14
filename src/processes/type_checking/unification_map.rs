use crate::utils::standard_library::validate_vectorization;
use crate::type_checking::unification;
use crate::help_message::ErrorMsg;
use std::collections::HashSet;
use crate::graph::TypeSystem;
use crate::components::error_message::help_message::TypeError;
use crate::utils::builder;
use crate::components::context::context::Context;
use crate::components::r#type::r#type::Type;
use std::fmt;

#[derive(Debug)]
struct SafeHashMap {
    map: Vec<(Type, Type)>
}

impl SafeHashMap {
    fn new() -> Self {
        SafeHashMap {
            map: vec![]
        }
    }

    fn insert(&mut self, key: Type, value: Type) {
        match self.map.iter().find(|(k, _v)| k == &key) {
            Some((Type::Generic(_, _), Type::Integer(_, _))) => { 
               self.map.push((key, value.generalize())) 
            },
            Some((_ke, va)) => if !(va.exact_equality(&value)) { 
                None.expect(
                    &TypeError::Param(va.to_owned(), value).display()
                           )
            },
            None => self.map.push((key, value))
        }
    }

    fn to_vec(self) -> Vec<(Type, Type)> {
        self.map.clone()
    }
}

#[derive(Debug)]
pub struct UnificationMap(pub Vec<(Type, Type)>);

impl UnificationMap {
    pub fn new(v: Vec<(Type, Type)>) -> Self {
        let mut safe_map = SafeHashMap::new();
        for (key, val) in v {
            safe_map.insert(key, val);
        }
        UnificationMap(safe_map.to_vec())
    }

    pub fn superficial_substitution(&self, ret_ty: &Type) -> Type {
        self.0.iter()
            .find(|(typ1, _)| ret_ty == typ1)
            .map(|(_, typ2)| typ2.clone())
            .unwrap_or(ret_ty.clone())
    }

    pub fn type_substitution(&self, ret_ty: &Type) -> Type {
        let ret_ty = self.superficial_substitution(ret_ty) ;
        unification::type_substitution(&ret_ty, &self.0)
    }

    pub fn apply_unification_type(&self, context: &Context, ret_ty: &Type) -> (Type, Context) {
        let ret_ty = ret_ty.reduce(context);
        let new_type = self.type_substitution(&ret_ty)
            .index_calculation();
        (new_type, context.clone())
    }

    pub fn append(self, other: Self) -> Self {
        Self(self.0.iter().chain(other.0.iter()).cloned().collect::<Vec<_>>())
    }

}

impl std::iter::FromIterator<(Type, Type)> for UnificationMap {
    fn from_iter<I: IntoIterator<Item = (Type, Type)>>(iter: I) -> Self {
        UnificationMap(iter.into_iter().collect())
    }
}

impl From<Vec<Vec<(Type, Type)>>> for  UnificationMap {
   fn from(val: Vec<Vec<(Type, Type)>>) -> Self {
        val.iter().cloned().flatten().collect::<UnificationMap>()
   } 
}

impl From<HashSet<(i32, Type)>> for UnificationMap {
   fn from(val: HashSet<(i32, Type)>) -> Self {
       let res = val.iter() 
           .map(|(i, typ)| (typ.clone(), builder::array_type2(i.clone(), typ.clone())))
           .collect::<Vec<_>>();
       UnificationMap(res)
   } 
}

impl fmt::Display for UnificationMap {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = self.0.iter()
            .map(|(ty1, ty2)| format!("{} = {}", ty1.pretty(), ty2.pretty2()))
            .fold("".to_string(), |acc, mapp| acc + " | " + &mapp);
        write!(f, "{}", res)       
    }
}

impl Default for UnificationMap {
    fn default() -> Self {
        Self(vec![])
    }
}

pub fn get_unification_map_for_vectorizable_function(types: Vec<Type>, _name: &str) -> Option<UnificationMap> {
    let unique_types = types.iter()
        .map(|x| x.get_size_type())
        .collect::<HashSet<_>>();
    validate_vectorization(unique_types)
        .map(|x| UnificationMap::from(x))
}
