use std::collections::HashSet;
use crate::Type;

fn set_append(mut set1: HashSet<(Type, Kind)>, set2: HashSet<(Type, Kind)>) -> HashSet<(Type, Kind)> {
    set1.extend(set2);
    set1
}

fn contains_indices(type_: &Type) -> bool {
    match type_ {
        Type::Ind(_) => true,
        Type::TAlias(_, params, _) => params.iter().any(|param| contains_indices(param)),
        Type::TFn(_, types, _) => types.iter().any(|ty| contains_indices(ty)),
        Type::TArray(index, ty) => contains_indices(index) || contains_indices(ty),
        Type::TRecord(fields) => fields.iter().any(|(_, ty)| contains_indices(ty)),
        _ => false,
    }
}


fn get_gen_index(type_: &Type) -> HashSet<(Type, Kind)> {
    match type_ {
        Type::Gen(n) => vec![(Type::Gen(n.clone()), Kind::KIndex)].into_iter().collect(),
        _ => HashSet::new(),
    }
}

fn get_gen_kind(type_: &Type) -> HashSet<(Type, Kind)> {
    match type_ {
        Type::Gen(g) => vec![(Type::Gen(g.clone()), Kind::KType)].into_iter().collect(),
        Type::Ind(g) => vec![(Type::Ind(g.clone()), Kind::KIndex)].into_iter().collect(),
        Type::TAlias(_, params, _) => {
            let kinds2 = params.iter().flat_map(|param| get_gen_kind(param)).collect();
            set_append(HashSet::new(), kinds2)
        },
        Type::TFn(kinds, _, _) => kinds.clone().into_iter().collect(),
        Type::TArray(index, ty) => {
            let kinds1 = get_gen_index(index);
            let kinds2 = get_gen_kind(ty);
            set_append(kinds1, kinds2)
        },
        Type::TRecord(fields) => {
            let types = fields.iter().map(|(_, ty)| ty).collect::<Vec<_>>();
            types.iter().flat_map(|ty| get_gen_kind(ty)).collect()
        },
        _ => HashSet::new(),
    }
}

fn type_to_kind(type_: &Type) -> Kind {
    match type_ {
        Type::Num => Kind::KIndex,
        _ => Kind::KType,
    }
}


