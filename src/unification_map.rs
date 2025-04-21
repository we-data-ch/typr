use crate::Type;
use crate::unification;


pub struct UnificationMap(pub Vec<(Type, Type)>);

impl UnificationMap {
    pub fn type_substitution(&self, ret_ty: &Type) -> Type {
        unification::type_substitution(ret_ty, &self.0)
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

