use std::collections::HashSet;
use crate::Type;
use crate::kind::Kind;
use crate::argument_kind::ArgumentKind;

fn set_append(mut set1: HashSet<ArgumentKind>, set2: HashSet<ArgumentKind>) -> HashSet<ArgumentKind> {
    set1.extend(set2);
    set1
}

fn contains_indices(type_: &Type) -> bool {
    match type_ {
        Type::IndexGen(_) => true,
        Type::Alias(_, params, _) => params.iter().any(|param| contains_indices(param)),
        Type::Function(_, types, _) => types.iter().any(|ty| contains_indices(ty)),
        Type::Array(index, ty) => contains_indices(index) || contains_indices(ty),
        Type::Record(fields) => fields.iter().any(|argument_type| contains_indices(&argument_type.1)),
        _ => false,
    }
}


fn get_gen_index(type_: &Type) -> HashSet<ArgumentKind> {
    match type_ {
        Type::Generic(n) => vec![(Type::Generic(n.clone()), Kind::Dim).into()].into_iter().collect(),
        _ => HashSet::new(),
    }
}

pub fn get_gen_kind(type_: &Type) -> HashSet<ArgumentKind> {
    match type_ {
        Type::Generic(g) => vec![(Type::Generic(g.clone()), Kind::Type).into()].into_iter().collect(),
        Type::IndexGen(g) => vec![(Type::IndexGen(g.clone()), Kind::Dim).into()].into_iter().collect(),
        Type::Alias(_, params, _) => {
            let kinds2 = params.iter().flat_map(|param| get_gen_kind(param)).collect();
            set_append(HashSet::new(), kinds2)
        },
        Type::Function(kinds, _, _) => kinds.clone().into_iter().collect(),
        Type::Array(index, ty) => {
            let kinds1 = get_gen_index(index);
            let kinds2 = get_gen_kind(ty);
            set_append(kinds1, kinds2)
        },
        Type::Record(fields) => {
            let types = fields.iter().map(|argument_type| argument_type.1.clone()).collect::<Vec<_>>();
            types.iter().flat_map(|ty| get_gen_kind(ty)).collect()
        },
        _ => HashSet::new(),
    }
}

fn type_to_kind(type_: &Type) -> Kind {
    match type_ {
        Type::Number => Kind::Dim,
        _ => Kind::Type,
    }
}


