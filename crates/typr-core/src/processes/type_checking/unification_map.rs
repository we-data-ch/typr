use crate::components::context::Context;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::type_checking::unification;
use crate::utils::builder;
use std::collections::HashSet;
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
        let resolved_value = Self::resolve_generic(&self.map, &value);
        match self.map.iter().find(|(k, _v)| k == &key) {
            Some((Type::Generic(old_key_name, _), existing_val)) => {
                let final_value = Self::resolve_generic(&self.map, existing_val);
                if final_value.exact_equality(&resolved_value)
                    || matches!(&final_value, Type::Generic(_, _))
                {
                    self.map.push((key, resolved_value));
                }
            }
            Some((_ke, va)) => if va.exact_equality(&resolved_value) {},
            None => self.map.push((key, resolved_value)),
        }
    }

    fn resolve_generic(map: &[(Type, Type)], typ: &Type) -> Type {
        let mut current = typ.clone();
        let mut seen = HashSet::new();
        loop {
            let found = map.iter().find(|(k, _)| k == &current);
            match found {
                Some((_, next)) => {
                    if next == &current || !seen.insert(current.clone()) {
                        break;
                    }
                    current = next.clone();
                }
                None => break,
            }
        }
        current
    }

    fn to_vec(&self) -> Vec<(Type, Type)> {
        self.map.clone()
    }
}

#[derive(Debug)]
pub struct UnificationMap {
    pub mapping: Vec<(Type, Type)>,
    pub vectorized: Option<(VecType, i32)>,
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

    pub fn set_vectorized(self, vec_type: VecType, index: i32) -> Self {
        Self {
            vectorized: Some((vec_type, index)),
            ..self
        }
    }

    pub fn is_vectorized(&self) -> bool {
        self.vectorized.is_some()
    }

    pub fn get_vectorization(&self) -> Option<(VecType, i32)> {
        self.vectorized
    }

    pub fn superficial_substitution(&self, ret_ty: &Type) -> Type {
        let mut current = ret_ty.clone();
        let mut seen = HashSet::new();
        loop {
            let found = self.mapping.iter().find(|(typ1, _)| *typ1 == current);
            match found {
                Some((_, typ2)) => {
                    if typ2 == &current || !seen.insert(current.clone()) {
                        break;
                    }
                    current = typ2.clone();
                }
                None => break,
            }
        }
        current
    }

    pub fn type_substitution(&self, ret_ty: &Type) -> Type {
        let ret_ty = self.superficial_substitution(ret_ty);
        unification::type_substitution(&ret_ty, &self.mapping)
    }

    pub fn apply_unification_type(&self, context: &Context, ret_ty: &Type) -> (Type, Context) {
        // Try substitution first on the original type (preserving alias structure).
        // For example: Option<U> with {U→int} → Option<int> (alias kept, not expanded).
        // If substitution changed the type, use that result directly.
        // If nothing changed (e.g. a bare alias like `Int` with no matching generics),
        // fall back to reducing so that `Int` becomes `int`.
        let substituted = self.type_substitution(ret_ty);
        let new_type = if &substituted != ret_ty {
            substituted.index_calculation()
        } else {
            ret_ty.reduce(context).index_calculation()
        };
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
        val.into_iter().flatten().collect::<UnificationMap>()
    }
}

impl From<HashSet<(i32, Type)>> for UnificationMap {
    fn from(val: HashSet<(i32, Type)>) -> Self {
        let res = val
            .iter()
            .map(|(i, typ)| (typ.clone(), builder::array_type2(*i, typ.clone())))
            .collect::<Vec<_>>();
        UnificationMap {
            mapping: res,
            vectorized: None,
        }
    }
}

impl fmt::Display for UnificationMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = self
            .mapping
            .iter()
            .map(|(ty1, ty2)| format!("{} = {}", ty1.pretty(), ty2.pretty2()))
            .fold("".to_string(), |acc, mapp| acc + " | " + &mapp);
        write!(f, "{}", res)
    }
}

#[allow(clippy::derivable_impls)]
impl Default for UnificationMap {
    fn default() -> Self {
        Self {
            mapping: vec![],
            vectorized: None,
        }
    }
}
