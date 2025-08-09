use crate::Type;
use crate::unification;
use crate::Context;
use crate::TypeError;
use crate::help_message::ErrorMsg;

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
            Some((_ke, va)) => if !(va.exact_match(&value)) { 
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

    pub fn type_substitution(&self, ret_ty: &Type) -> Type {
        unification::type_substitution(ret_ty, &self.0)
    }

    pub fn apply_unification_type(&self, context: &Context, ret_ty: &Type) -> (Type, Context) {
        let new_type = self.type_substitution(ret_ty)
            .index_calculation();
        (new_type, context.clone())
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

