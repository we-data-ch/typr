use crate::Type;
use crate::unification;
use crate::Context;

#[derive(Debug)]
pub struct UnificationMap(pub Vec<(Type, Type)>);

impl UnificationMap {
    pub fn type_substitution(&self, ret_ty: &Type) -> Type {
        unification::type_substitution(ret_ty, &self.0)
    }

    pub fn apply_unification_type(self, context: &Context, ret_ty: &Type) -> (Type, Context) {
        let new_type = self.type_substitution(ret_ty)
            .index_calculation();
        (new_type, context.clone().push_unifications(self.0))
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

