use crate::type_comparison::is_subtype;
use std::collections::HashSet;
use crate::Context;
use crate::Type;
use crate::type_comparison::reduce_type;

#[derive(Debug, Clone)]
pub struct Subtypes(HashSet<(Type, Type)>);

fn not_alias_reverse_subtyping(typ1: &Type, typ2: &Type) -> bool {
    match (typ1, typ2) {
        (Type::Alias(_, _, _), Type::Alias(_, _, _)) => true,
        (Type::Alias(_, _, _), _) => false,
        _ => true
    }
}

pub fn is_true_subtype(context: &Context, typ1: &Type, typ2: &Type) -> bool {
    (typ1 != typ2) 
        && is_subtype(context, typ1, typ2) 
        //&& not_alias_reverse_subtyping(typ1, typ2)
}

impl Subtypes {

    pub fn new() -> Subtypes {
        Subtypes(HashSet::new())
    }    

    pub fn get_supertypes(&self, typ: &Type) -> Vec<Type> {
        self.0.iter()
            .filter(|(_typ1, typ2)| typ2 == typ)
            .map(|(typ1, _typ2)| typ1.clone())
            .collect::<Vec<_>>()
    }

    pub fn update(self, types: &[Type], context: &Context) -> Subtypes {
        Subtypes(types.iter().flat_map(|typ1| {
            types.iter()
                .map(move |typ2| {
                    let ty1 = reduce_type(context, typ1);
                    let ty2 = reduce_type(context, typ2);
                    let res1 = if is_true_subtype(context, &ty1, &ty2) {
                        Some((typ2.clone(), typ1.clone()))
                    } else { None };
                    let res2 = if is_true_subtype(context, &ty2, &ty1) {
                        Some((typ1.clone(), typ2.clone()))
                    } else { None };
                    match (res1, res2) {
                        (Some(v1), Some(v2)) => vec![v1, v2],
                        (Some(v1), None) => vec![v1],
                        (None, Some(v2)) => vec![v2],
                        (None, None) => vec![]
                    }
                    })
        }).fold(HashSet::new(), |acc: HashSet<(Type, Type)>, arr| acc.iter().chain(arr.iter()).cloned().collect()))
    }

}
