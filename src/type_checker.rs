use crate::language::build_generic_function;
use std::collections::HashSet;
use crate::Type;
use crate::context::Context;
use crate::Lang;
use crate::type_comparison;
use crate::var::Var;
use crate::tag::Tag;
use crate::index::Index;
use crate::unification;
use crate::type_comparison::is_matching;
use crate::type_comparison::reduce_type;
use crate::argument_type::ArgumentType;

fn get_tag_names(tags: &[Tag]) -> Vec<String> {
    tags.iter()
        .filter_map(|tag| match tag.to_type() {
            Type::Tag(name, _) => Some(name.clone()),
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

pub fn eval(context: &Context, expr: &Lang) -> Context {
    match expr {
        Lang::Sequence(exprs) => exprs.iter().fold(context.clone(), |ctx, expr| eval(&ctx, expr)),
        Lang::Let(name, ty, exp) => {
            let ty = if ty == &Type::Empty {Type::Any} else {ty.clone()};
            let expr_ty = typing(&context, exp).0;
            let new_context = type_comparison::is_matching(&context, &expr_ty, &ty).then(|| {
                if ty != Type::Any {
                    context.clone()
                        .push_var_type(name.clone().into(), ty.clone(), context)
                        .push_var_type(name.clone().into(), expr_ty.clone(), context)
                } else {
                    context.clone()
                        .push_var_type(name.clone().into(), expr_ty.clone(), context)
                }
            }).expect(&format!("Type error:\n {} don't match {}", expr_ty, ty));
            if !expr.is_undefined() {
                new_context.add_to_adt(&[Lang::GenFunc(build_generic_function(&name.get_name()))])
            } else {
                new_context
            }
        },
        Lang::Alias(name, params, typ) => {
            let var = name.clone().set_type(Type::Params(params.to_vec()));
            let new_context = context.clone()
                .push_var_type(var, typ.clone(), context);
            let (fn_typ, new_context2) = new_context.get_embeddings(typ);
            let new_context3 = fn_typ.iter()
                .fold(new_context2, |ctx, var_typfun| ctx.push_var_type(var_typfun.0.clone(), var_typfun.1.clone(), context));
            new_context3
        },
        Lang::Assign(var, expr) => {
            let type1 = context.get_type_from_variable(Var::from_language((**var).clone()).unwrap());
            let type2 = typing(&context, expr).0;
            if type_comparison::is_matching(&context, &type1, &type2) {
                let var_ty = typing(&context, var).0;
                let expr_ty = typing(&context, expr).0;
                if !type_comparison::is_matching(&context, &var_ty, &expr_ty) {
                    panic!("Type error");
                }
                context.clone()
            } else {
                panic!("Variableiable not found");
            }
        }
        _ => context.clone()
    }
}

// for the sugar syntax that can catch value as type for array
// 4 -> Index(4) if an index parameter is needed
fn index_conversion(arg_type: &Type, par_type: &Type, value: &Lang) -> (Type, Type) {
    match (par_type, arg_type, value) {
        (Type::IndexGen(_), _, Lang::Integer(i)) 
            => (par_type.clone(), Type::Index(*i as u32)),
        _ => (par_type.clone(), arg_type.clone())
    }
}

pub fn typing(context: &Context, expr: &Lang) -> (Type, Context) {
    match expr {
        Lang::Number(_) => (Type::Number, context.clone()),
        Lang::Integer(_) => (Type::Integer, context.clone()),
        Lang::Bool(_) => (Type::Boolean, context.clone()),
        Lang::Char(_) => (Type::Char, context.clone()),
        Lang::Empty => (Type::Empty, context.clone()),
        Lang::And(e1, e2) | Lang::Or(e1, e2) => {
            if typing(context, e1).0 == Type::Boolean && typing(context, e2).0 == Type::Boolean {
                (Type::Boolean, context.clone())
            } else {
                panic!("Type error");
            }
        }
        Lang::Eq(e1, e2) | Lang::LesserOrEqual(e1, e2) | Lang::GreaterOrEqual(e1, e2) | Lang::GreaterThan(e1, e2) | Lang::LesserThan(e1, e2) => {
            let ty1 = typing(context, e1).0;
            let ty2 = typing(context, e2).0;
            if ty1 == ty2 {
                (Type::Boolean, context.clone())
            } else {
                panic!("Type error");
            }
        }
        Lang::Dot(e1, e2) => {
            let ty2 = typing(context, e2).0;
            match (reduce_type(context, &ty2), *e1.clone()) {
                (Type::Record(fields), Lang::Variable(name, _, _, _, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect("Field not found")
                },
                (_, Lang::FunctionApp(name, args)) 
                    if Var::from_language((*name).clone()).unwrap().get_name() == "map"
                        => {
                            let new_args = [(**e2).clone()].into_iter()
                                .chain(args.into_iter())
                                .collect::<Vec<_>>();
                            typing(context, &Lang::FunctionApp(name, new_args))
                        },
                (Type::Record(fields1), Lang::Record(fields2)) => {
                    let at = fields2[0].clone();
                    let fields3 = fields1.iter()
                        .map(|arg_typ2| {
                            match arg_typ2.get_argument_str() == at.get_argument() {
                                true => ArgumentType::new(
                                            &at.get_argument(),
                                            &typing(context, &at.get_value()).0),
                                false => arg_typ2.clone()
                            }
                        }).collect::<Vec<_>>();
                    (Type::Record(fields3), context.clone())
                },
                _ => panic!("Type error")
            }
        },
        Lang::Pipe(e1, e2) => {
            let ty2 = typing(context, e2).0;
            match (ty2, *e1.clone()) {
                (Type::Record(fields), Lang::Variable(name, _, _, _, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect("Field not found")
                },
                _ => panic!("Type error")
            }
        },
        Lang::Function(kinds, params, ret_ty, body) => {
            let param_types = params.iter()
                .map(|arg_typ| arg_typ.get_type())
                .collect::<Vec<_>>();
            let sub_context = params.into_iter()
                .map(|arg_typ| 
                     Var::from_name(&arg_typ.get_argument_str())
                        .set_type(arg_typ.get_type()))
                .zip(param_types.clone().into_iter())
                .fold(context.clone(), |cont, (var, typ)| cont.clone().push_var_type(var, typ, &cont));
            let res = typing(&sub_context, body);
                if !is_matching(context, &res.0, ret_ty) {
                    panic!("Error:\nThe output type of the function don't match it's type annotation\nExpected: {}\nFound: {}", ret_ty, res.0)
                }
            let new_context = res.1.unifications.into_iter()
                .fold(context.clone(), |cont, uni_vec| cont.push_unifications(uni_vec));
            (Type::Function(kinds.clone(), param_types, Box::new(ret_ty.clone())), new_context)
        }
        Lang::Sequence(exprs) => {
            if exprs.len() == 1 {
                let res = exprs.clone().pop().unwrap();
                typing(context, &res)
            } else {
                let context2 = context.clone();
                let mut exprs2 = exprs.clone();
                let exp = exprs2.pop().unwrap();
                let new_context = exprs.iter()
                    .fold(context2, |ctx, expr| eval(&ctx, expr));
                typing(&new_context, &exp)
            }
        },
        Lang::FunctionApp(fn_var_name, args) => {
            let var_name = if args.len() > 0 {
                let first = typing(context, &args.iter().nth(0).unwrap().clone()).0;
                Var::from_language(*fn_var_name.clone())
                    .unwrap().set_type(first)
            } else {
                Var::from_language(*fn_var_name.clone()).unwrap()
            };
            let fn_ty = typing(context, &var_name.to_language()).0;
            match fn_ty {
                Type::Function(_kinds, param_types, ret_ty) => {
                    let arg_types = args.iter()
                        .map(|arg| typing(context, arg).0).collect::<Vec<_>>();
                    let arg_param_types = arg_types.iter()
                        .zip(param_types.iter()).collect::<Vec<_>>();
                    let conditions = arg_param_types.iter() 
                        .all(|(arg, par)| type_comparison::is_matching(context, arg, par));
                    if conditions {
                        let unification_map = arg_param_types.iter()
                            .zip(args.iter())
                            .map(|((arg, par), val)| index_conversion(arg, par, val))
                            .flat_map(|(arg, par)| unification::unify(&arg, &par))
                            .reduce(|res1, res2| res1.iter().chain(res2.iter()).cloned().collect())
                            .unwrap_or(vec![])
                            .into_iter().collect::<HashSet<_>>()
                            .into_iter().collect::<Vec<_>>();
                        let new_type = unification::type_substitution(&(*ret_ty), &unification_map).index_calculation();
                        (new_type, context.clone().push_unifications(unification_map))
                    } else {
                        panic!("The arguments types doesnt match:\nexpected: {:?}\nrecieved: {:?}", param_types, arg_types);
                    }
                }
                _ => panic!("{} is not a function but a {}", fn_var_name.to_r(&Context::new(vec![], vec![])).0, fn_ty.to_typescript()),
            }
        }
        Lang::Tag(name, expr) => {
            let ty = typing(context, expr).0;
            (Type::Tag(name.clone(), Box::new(ty)), context.clone())
        }
        Lang::If(cond, true_branch, false_branch) => {
            if typing(context, cond).0 == Type::Boolean {
                let true_ty = typing(context, true_branch).0;
                let false_ty = typing(context, false_branch).0;
                (unify_type(&true_ty, &false_ty), context.clone())
            } else {
                panic!("Type error");
            }
        }
        Lang::Array(exprs) => {
            let types = exprs.iter().map(|expr| typing(context, expr).0).collect::<Vec<_>>();
            if types.windows(2).all(|w| w[0] == w[1]) {
                let new_type = Type::Array(
                    Box::new(Type::Index(exprs.len() as u32)),
                    Box::new(types[0].clone()));
                (new_type, context.clone())
            } else {
                panic!("Type error");
            }
        }
        Lang::Record(fields) => {
            let field_types = fields.iter()
                .map(|arg_val| {
                    (arg_val.get_argument(),
                    typing(context, &arg_val.get_value()).0).into()
                }).collect();
            (Type::Record(field_types), context.clone())
        }
        Lang::Match(val, branches) => {
            let val_ty = typing(context, val).0;
            match val_ty {
                Type::Union(union_types) => {
                    let tag_names = get_tag_names(&union_types);
                    let tag_names2 = get_tag_names(&branches.iter()
                        .flat_map(|(tag, _)| Tag::from_language(*tag.clone(), context))
                        .collect::<Vec<_>>());
                    if same_values(&tag_names, &tag_names2) {
                        let branch_types = branches.iter()
                            .map(|(_, exp)| typing(context, exp).0)
                            .collect::<Vec<_>>();
                        (unify_types(&branch_types), context.clone())
                    } else {
                        panic!("Type error");
                    }
                }
                _ => panic!("Type error"),
            }
        }
        Lang::ArrayIndexing(expr, index) => {
            let ty = typing(context, expr).0;
            match ty {
                Type::Array(len, elem_ty) => {
                    let index2 = Index::from_type(&(*len)).unwrap().get_value();
                    if (*index as u32) <  index2 {
                        (*elem_ty, context.clone())
                    } else {
                        panic!("Index out of bounds");
                    }
                }
                _ => panic!("Type error"),
            }
        },
        Lang::Variable(na, pa, pe, sp, ty) => {
            let var = Var::from_language(
                Lang::Variable(na.clone(),
                pa.clone(),
                pe.clone(),
                sp.clone(),
                ty.clone())).unwrap();
            (context.get_type_from_variable(var), context.clone())
        },
        Lang::Scope(expr) if expr.len() == 1 => {
            typing(context, &expr[0])
        },
        Lang::Scope(expr) => typing(context, &Lang::Sequence(expr.to_vec())),
        Lang::VecBloc(_) => (Type::Any, context.clone()),
        _ => (Type::Any, context.clone()),
    }
}

fn unify_type(ty1: &Type, ty2: &Type) -> Type {
    match (ty1, ty2) {
        (Type::Tag(name1, params1), Type::Tag(name2, params2)) => {
            if name1 == name2 && params1 == params2 {
                Type::Union(vec![
                            Tag::from_type(ty1.clone()).unwrap(),
                            Tag::from_type(ty2.clone()).unwrap()])
            } else {
                panic!("Type error");
            }
        }
        (Type::Union(union1), Type::Tag(name, params)) => {
            let mut union2 = union1.clone();
            union2.push(Tag::new(name.clone(), *params.clone()));
            Type::Union(union2)
        }
        (Type::Tag(name, params), Type::Union(union1)) => {
            let mut union2 = union1.clone();
            union2.push(Tag::new(name.clone(), *params.clone()));
            Type::Union(union2)
        }
        (Type::Any, _) | (_, Type::Any) => Type::Any,
        (ty1, ty2) if ty1 == ty2 => ty1.clone(),
        _ => panic!("Type error"),
    }
}
