fn get_right_elements(params: &[(String, Type)]) -> Vec<Type> {
    params.iter().map(|(_, ty)| ty.clone()).collect()
}

fn get_left_elements(params: &[(String, Type)]) -> Vec<String> {
    params.iter().map(|(name, _)| name.clone()).collect()
}

fn get_right_element(var: &Expr, context: &[(String, Type)]) -> Option<Type> {
    match var {
        Expr::Var(name, path, perm, mutable, base) => {
            for (ctx_var, ty) in context {
                if let Expr::Var(ctx_name, ctx_path, ctx_perm, ctx_mutable, ctx_base) = ctx_var {
                    if name == ctx_name && path == ctx_path && perm == ctx_perm && mutable == ctx_mutable && base == ctx_base {
                        return Some(ty.clone());
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn concat<T>(list1: Vec<T>, list2: Vec<T>) -> Vec<T> {
    [list1, list2].concat()
}

fn concat_context(context1: &Context, context2: &Context) -> Context {
    Context {
        kinds: concat(context1.kinds.clone(), context2.kinds.clone()),
        types: concat(context1.types.clone(), context2.types.clone()),
    }
}

fn add_type(context: &mut Context, var: &Expr, ty: Type) {
    if let Expr::Var(name, path, perm, mutable, base) = var {
        context.types.push((format!("{}_{}_{}_{}_{}", name, path, perm, mutable, base), ty));
    }
}

fn add_kind(context: &mut Context, var: &Expr, kind: Type) {
    if let Expr::Var(name, path, perm, mutable, base) = var {
        context.kinds.push((format!("{}_{}_{}_{}_{}", name, path, perm, mutable, base), kind));
    }
}

fn intersection<T: Eq + std::hash::Hash>(list1: &[T], list2: &[T]) -> Vec<T> {
    let set: std::collections::HashSet<_> = list2.iter().collect();
    list1.iter().filter(|item| set.contains(item)).cloned().collect()
}


fn get_from_context(context: &Context, var: &Expr, context_list: &[(String, Type)]) -> Option<Type> {
    match var {
        Expr::Var(name, path, perm, mutable, base) => {
            for (ctx_var, ty) in context_list {
                if let Expr::Var(ctx_name, ctx_path, ctx_perm, ctx_mutable, ctx_base) = ctx_var {
                    if name == ctx_name && path == ctx_path && perm == ctx_perm && mutable == ctx_mutable && base == ctx_base {
                        return Some(ty.clone());
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn get_type(var: &Expr, context: &Context) -> Option<Type> {
    get_right_element(var, &context.types)
}

fn get_kind(var: &Expr, context: &Context) -> Option<Type> {
    get_right_element(var, &context.kinds)
}
