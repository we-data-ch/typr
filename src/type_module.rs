use crate::Type;
use crate::Lang;
use crate::context::Context;
use crate::var::Var;

pub struct Opaques(String);

fn list_atom_concat(list: &[String]) -> String {
    list.join(", ")
}

fn list_concat(list: &[String]) -> String {
    list.concat()
}

fn join(list: &[String], sep: &str) -> String {
    list.join(sep)
}

fn flatten<T>(list: &[T]) -> Vec<T>
where
    T: Clone,
{
    list.iter().cloned().collect()
}

fn get_base_name(ty: &Type) -> String {
    match ty {
        Type::Generic(name) => name.to_uppercase(),
        Type::Number => "Num".to_string(),
        Type::Boolean => "Bool".to_string(),
        Type::Array(dim, ty) => format!("array{}{}", dim, get_base_name(ty)),
        Type::Record(params) => {
            let flattened = flatten(params);
            let base_names = flattened.iter()
                .map(|arg_typ| get_base_name(&arg_typ.get_type()))
                .collect::<Vec<_>>();
            format!("record_{}", list_atom_concat(&base_names))
        }
        Type::Function(_, types, ret_ty) => {
            let base_names = types.iter().map(|ty| get_base_name(ty)).collect::<Vec<_>>();
            let concatenated = list_atom_concat(&base_names);
            format!("function{}{}", concatenated, get_base_name(ret_ty))
        }
        Type::Tag(name, value) => format!("tag{}{}", name, get_base_name(value)),
        Type::Integer => "Int".to_string(),
        _ => "Unknown".to_string(),
    }
}

fn base_name_from(name: &str, ty: &Type) -> String {
    let base = get_base_name(ty);
    format!("{}{}", base, name)
}

fn substitute_opaque(opaques: &Opaques, var: &Lang, context: &mut Context) {
    if let Lang::Variable(name, _, _, _, _) = var {
        if opaques.0 == *name {
            context.get_type_map()
                .push((
                        Var::from_language(var.clone()).unwrap(),
                        Type::Opaque(name.clone())));
        }
    }
}

pub fn add_path(name: &str, var: &Lang) -> Lang {
    match var {
        Lang::Variable(n, path, perm, mutable, ty) => {
            let context = Context::default();
            let new_path = concat_path(name, path);
            Lang::Variable(n.clone(), new_path, perm.clone(), *mutable, ty.clone())
        }
        lang => lang.clone()
    }
}

fn concat_path(name: &str, path: &str) -> String {
    if path.is_empty() {
        name.to_string()
    } else {
        format!("{}/{}", name, path)
    }
}
