use std::collections::HashMap;
use crate::nominal_context::TypeCategory;
use std::collections::BTreeSet;
use crate::Type;
use crate::Context;
use std::ops::Bound::*;
use crate::builder;

#[derive(Debug, Clone)]
pub struct TypeHierarchy(HashMap<TypeCategory, BTreeSet<Type>>);

impl TypeHierarchy {

    pub fn new() -> TypeHierarchy {
        let mut th = TypeHierarchy(HashMap::new());
        th.update(&[builder::generic_type()])
    }    

    pub fn get_supertypes(&self, typ: &Type) -> Vec<Type> {
        let category = typ.to_category();
        let btree: BTreeSet<Type> = BTreeSet::new();
        let subtypes = self.0.get(&category).unwrap_or(&btree);
        let mut res = subtypes.range((Excluded(typ), Unbounded))
            .cloned().collect::<Vec<Type>>();
        res.push(builder::generic_type());
        res
    }

    pub fn update(&mut self, types: &[Type]) -> TypeHierarchy {
        types.iter().for_each(|typ| {
            let category = typ.to_category() ;
            let btree: BTreeSet<Type> = BTreeSet::new();
            let mut set = self.0.get(&category).unwrap_or(&btree).to_owned();
            set.insert(typ.clone());
            self.0.insert(category, set.to_owned());
        });
        self.to_owned()
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_hierarchy_emtpy(){
        let th = TypeHierarchy::new();
        let typ = th.get_supertypes(&builder::generic_type());
        assert_eq!(typ, vec![builder::generic_type()]);
    }

    #[test]
    fn test_type_hierarchy_update() {
        let th = TypeHierarchy::new();
        let typ = th.get_supertypes(&builder::integer_type(7));
        assert_eq!(typ, vec![]);
    }

    #[test]
    fn test_type_hierarchy_record_subtypes() {
        let mut th = TypeHierarchy::new();
        let rec1 = builder::record_type(&[("x".to_string(), builder::integer_type(1))]);
        let rec2 = builder::record_type(&[("x".to_string(), builder::integer_type(1))]);
        let rec3 = builder::record_type(&[
                                         ("x".to_string(), builder::integer_type(1)),
                                         ("y".to_string(), builder::integer_type(2)),
                                         ("z".to_string(), builder::integer_type(3))
                                         ]);
        th.update(&[rec1.clone()]);
        th.update(&[rec2.clone()]);
        let typ = th.get_supertypes(&rec3);
        assert_eq!(typ, vec![rec1, rec2, builder::generic_type()]);
    }

}
