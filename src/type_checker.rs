use std::collections::HashSet;

fn get_tag_names(tags: &[Type]) -> Vec<String> {
    tags.iter()
        .filter_map(|tag| match tag {
            Type::TTag(name, _) => Some(name.clone()),
            _ => None,
        })
        .collect()
}

fn subset<T: Eq + std::hash::Hash>(subset: &[T], set: &[T]) -> bool {
    let set: HashSet<_> = set.iter().collect();
    subset.iter().all(|item| set.contains(item))
}

fn same_values<T: Eq + std::hash::Hash>(list1: &[T], list2: &[T]) -> bool {
    list1.len() == list2.len() && subset(list1, list2) && subset(list2, list1)
}

fn split_at_n<T>(n: usize, list: &[T]) -> (&[T], &[T]) {
    list.split_at(n)
}

fn unify_types(types: &[Type]) -> Type {
    if types.is_empty() {
        Type::Any
    } else if types.len() == 1 {
        types[0].clone()
    } else {
        let mut unified_type = types[0].clone();
        for ty in &types[1..] {
            unified_type = unify_type(&unified_type, ty);
        }
        unified_type
    }
}

fn contains_key(key: &str, list: &[(String, Type)]) -> bool {
    list.iter().any(|(k, _)| k == key)
}

fn add_if_not_present(key: &str, value: Type, list: &mut Vec<(String, Type)>) {
    if !contains_key(key, list) {
        list.push((key.to_string(), value));
    }
}

fn record_union(record1: &[(String, Type)], record2: &[(String, Type)]) -> Vec<(String, Type)> {
    let mut result = record1.to_vec();
    for (key, value) in record2 {
        add_if_not_present(key, value.clone(), &mut result);
    }
    result
}


fn eval(context: &Context, expr: &Expr, new_context: &mut Context) {
    match expr {
        Expr::Sequence(exprs) => {
            for expr in exprs {
                eval(context, expr, new_context);
            }
        }
        Expr::Comment => {}
        Expr::Var(name, path, perm, mut_opa, ty) => {
            let kinds = kinds::get_gen_kind(ty);
            new_context.types.push((name.clone(), Type::TAlias(kinds, vec![], ty.clone())));
        }
        Expr::Let(name, ty, expr) => {
            let ty = type_comparison::reduce_type(context, ty);
            let expr_ty = typing(context, expr);
            if type_comparison::is_matching(context, &expr_ty, &ty) {
                let best_ty = type_comparison::get_best_type(context, ty, expr_ty);
                new_context.types.push((name.clone(), best_ty));
            } else {
                panic!("Type error");
            }
        }
        Expr::Mod(name, exprs) => {
            let kinds = typing(&Context { kinds: vec![], types: vec![] }, &Expr::Sequence(exprs.clone()));
            type_module::add_path(name, kinds, new_context);
        }
        Expr::Assign(var, expr) => {
            if let Some((_, ty)) = context.types.iter().find(|(v, _)| v == var) {
                let var_ty = typing(context, var);
                let expr_ty = typing(context, expr);
                if type_comparison::is_matching(context, &var_ty, &expr_ty) {
                    // Assignment is valid
                } else {
                    panic!("Type error");
                }
            } else {
                panic!("Variable not found");
            }
        }
        _ => {}
    }
}

fn typing(context: &Context, expr: &Expr) -> Type {
    match expr {
        Expr::True | Expr::False => Type::Bool,
        Expr::Chars(_) => Type::Chars,
        Expr::Empty => Type::Any,
        Expr::And(e1, e2) | Expr::Or(e1, e2) => {
            if typing(context, e1) == Type::Bool && typing(context, e2) == Type::Bool {
                Type::Bool
            } else {
                panic!("Type error");
            }
        }
        Expr::Eq(e1, e2) | Expr::LesserOrEqual(e1, e2) | Expr::GreaterOrEqual(e1, e2) | Expr::GreaterThan(e1, e2) | Expr::LesserThan(e1, e2) => {
            let ty1 = typing(context, e1);
            let ty2 = typing(context, e2);
            if ty1 == ty2 {
                Type::Bool
            } else {
                panic!("Type error");
            }
        }
        Expr::Dot(e1, e2) => {
            let ty1 = typing(context, e1);
            match ty1 {
                Type::TRecord(fields) => {
                    if let Expr::Var(name, _, _, _, _) = e2 {
                        if let Some((_, ty)) = fields.iter().find(|(f, _)| f == name) {
                            ty.clone()
                        } else {
                            panic!("Field not found");
                        }
                    } else {
                        panic!("Type error");
                    }
                }
                _ => panic!("Type error"),
            }
        }
        Expr::Pipe(e1, e2) => {
            let ty1 = typing(context, e1);
            match ty1 {
                Type::TRecord(fields) => {
                    if let Expr::Var(name, _, _, _, _) = e2 {
                        if let Some((_, ty)) = fields.iter().find(|(f, _)| f == name) {
                            ty.clone()
                        } else {
                            panic!("Field not found");
                        }
                    } else {
                        panic!("Type error");
                    }
                }
                _ => panic!("Type error"),
            }
        }
        Expr::Fn(kinds, params, ret_ty, body) => {
            let param_types = params.iter().map(|param| typing(context, param)).collect();
            Type::TFn(kinds.clone(), param_types, ret_ty.clone())
        }
        Expr::Values(exprs) => {
            exprs.iter().map(|expr| typing(context, expr)).collect()
        }
        Expr::FnApp(fn_expr, args) => {
            let fn_ty = typing(context, fn_expr);
            match fn_ty {
                Type::TFn(kinds, param_types, ret_ty) => {
                    let arg_types = args.iter().map(|arg| typing(context, arg)).collect::<Vec<_>>();
                    if param_types == arg_types {
                        ret_ty
                    } else {
                        panic!("Type error");
                    }
                }
                _ => panic!("Type error"),
            }
        }
        Expr::Tag(name, expr) => {
            let ty = typing(context, expr);
            Type::TTag(name.clone(), ty)
        }
        Expr::If(cond, true_branch, false_branch) => {
            if typing(context, cond) == Type::Bool {
                let true_ty = typing(context, true_branch);
                let false_ty = typing(context, false_branch);
                unify_type(&true_ty, &false_ty)
            } else {
                panic!("Type error");
            }
        }
        Expr::Array(exprs) => {
            let types = exprs.iter().map(|expr| typing(context, expr)).collect::<Vec<_>>();
            if types.windows(2).all(|w| w[0] == w[1]) {
                Type::TArray(exprs.len(), types[0].clone())
            } else {
                panic!("Type error");
            }
        }
        Expr::Record(fields) => {
            let field_types = fields.iter().map(|(name, expr)| (name.clone(), typing(context, expr))).collect();
            Type::TRecord(field_types)
        }
        Expr::Match(val, branches) => {
            let val_ty = typing(context, val);
            match val_ty {
                Type::Union(union_types) => {
                    let tag_names = get_tag_names(&union_types);
                    let branch_types = branches.iter().map(|(pat, expr)| typing(context, expr)).collect::<Vec<_>>();
                    if same_values(&tag_names, &branch_types) {
                        unify_types(&branch_types)
                    } else {
                        panic!("Type error");
                    }
                }
                _ => panic!("Type error"),
            }
        }
        Expr::ArrayIndexing(expr, index) => {
            let ty = typing(context, expr);
            match ty {
                Type::TArray(len, elem_ty) => {
                    if *index < len {
                        elem_ty
                    } else {
                        panic!("Index out of bounds");
                    }
                }
                _ => panic!("Type error"),
            }
        }
        Expr::Var(_, _, _, _, ty) => ty.clone(),
        Expr::Sequence(exprs) => {
            let mut context = context.clone();
            for expr in exprs {
                eval(context, expr, &mut context);
            }
            Type::Empty
        }
        _ => Type::Any,
    }
}

fn unify_type(ty1: &Type, ty2: &Type) -> Type {
    match (ty1, ty2) {
        (Type::TTag(name1, params1), Type::TTag(name2, params2)) => {
            if name1 == name2 && params1 == params2 {
                Type::Union(vec![ty1.clone(), ty2.clone()])
            } else {
                panic!("Type error");
            }
        }
        (Type::Union(union1), Type::TTag(name, params)) => {
            let mut union2 = union1.clone();
            union2.push(Type::TTag(name.clone(), params.clone()));
            Type::Union(union2)
        }
        (Type::TTag(name, params), Type::Union(union1)) => {
            let mut union2 = union1.clone();
            union2.push(Type::TTag(name.clone(), params.clone()));
            Type::Union(union2)
        }
        (Type::Any, _) | (_, Type::Any) => Type::Any,
        (ty1, ty2) if ty1 == ty2 => ty1.clone(),
        _ => panic!("Type error"),
    }
}
