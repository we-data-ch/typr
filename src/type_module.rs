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
        Type::Gen(name) => name.to_uppercase(),
        Type::Var(name, _, _, _, _) => name.clone(),
        Type::Num => "Num".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::TArray(dim, ty) => format!("array{}{}", dim, get_base_name(ty)),
        Type::TRecord(params) => {
            let flattened = flatten(params);
            let base_names = flattened.iter().map(|(_, ty)| get_base_name(ty)).collect::<Vec<_>>();
            format!("record_{}", list_atom_concat(&base_names))
        }
        Type::TFn(_, types, ret_ty) => {
            let base_names = types.iter().map(|ty| get_base_name(ty)).collect::<Vec<_>>();
            let concatenated = list_atom_concat(&base_names);
            format!("function{}{}", concatenated, get_base_name(ret_ty))
        }
        Type::Tag(name, value) => format!("tag{}{}", name, get_base_name(value)),
        Type::Int => "Int".to_string(),
        _ => "Unknown".to_string(),
    }
}

fn base_name_from(name: &str, ty: &Type) -> String {
    let base = get_base_name(ty);
    format!("{}{}", base, name)
}

fn substitute_opaque(opaques: &Opaques, var: &Expr, context: &mut Context) {
    if let Expr::Var(name, _, _, _, _) = var {
        if opaques.0.contains(name) {
            context.types.push((name.clone(), Type::Opaque(name.clone())));
        }
    }
}

fn add_path(name: &str, var: &Expr, opaques: &mut Opaques, context: &mut Context) {
    match var {
        Expr::Var(n, path, perm, mutable, ty) => {
            let new_path = concat_path(name, path);
            context.types.push((n.clone(), Type::Var(n.clone(), new_path, perm.clone(), *mutable, ty.clone())));
            if perm == "private" {
                opaques.0.push(n.clone());
            }
        }
        _ => {}
    }
}

fn concat_path(name: &str, path: &str) -> String {
    if path.is_empty() {
        name.to_string()
    } else {
        format!("{}/{}", name, path)
    }
}
