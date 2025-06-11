use std::collections::HashMap;
use crate::nominal_context::TypeCategory;
use std::collections::BTreeSet;
use crate::Type;
use crate::Context;
use std::ops::Bound::*;

#[derive(Debug, Clone)]
pub struct TypeHierarchy(HashMap<TypeCategory, BTreeSet<Type>>);

impl TypeHierarchy {

    pub fn new() -> TypeHierarchy {
        TypeHierarchy(HashMap::new())
    }    

    pub fn get_supertypes(&self, typ: &Type) -> Vec<Type> {
        let category = typ.to_category();
        let subtypes = self.0.get(&category).unwrap();
        subtypes.range((Excluded(typ), Unbounded))
            .cloned().collect()
    }

    pub fn update(&mut self, types: &[Type], context: &Context) -> TypeHierarchy {
        types.iter().map(|typ| {
            let category = typ.to_category() ;
             let mut set = self.0.get(&category).unwrap().clone();
             set.insert(typ.clone());
             self.0.insert(category, set.to_owned());
        });
         TypeHierarchy(self.0.clone())
    }

}
