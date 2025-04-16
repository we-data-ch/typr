use crate::type_comparison::is_subtype;
use std::collections::HashSet;
use crate::Context;
use crate::Type;

#[derive(Debug, Clone)]
pub struct Subtypes(HashSet<(Type, Type)>);

fn not_alias_reverse_subtyping(typ1: &Type, typ2: &Type) -> bool {
    match (typ1, typ2) {
        (Type::Alias(_, _, _), ty) => {
            if let Type::Alias(_, _, _) = ty 
            { true } else { false }
        },
        _ => true
    }
}

fn is_true_subtype(context: &Context, typ1: &Type, typ2: &Type) -> bool {
    (typ1 != typ2) 
        && is_subtype(context, typ1, typ2) 
        && not_alias_reverse_subtyping(typ1, typ2)
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
                .flat_map(move |typ2| {
                    let res = if is_true_subtype(context, typ1, typ2) 
                        {Some((typ2.clone(), typ1.clone()))} 
                        else {None};
                    res})
        }).collect())
    }

}
