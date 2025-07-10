use crate::language::build_generic_function;
use std::collections::HashSet;
use crate::Type;
use crate::context::Context;
use crate::Lang;
use crate::var::Var;
use crate::tag::Tag;
use crate::index::Index;
use crate::unification;
use crate::type_comparison::reduce_type;
use crate::argument_type::ArgumentType;
use crate::unification_map::UnificationMap;
use std::process::Command;
use crate::AdtManager;
use std::fs::File;
use crate::parse;
use std::fs;
use crate::Environment;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;
use crate::CompileMode;
use crate::builder;
use crate::TypeError;
use crate::help_message::ErrorMsg;

fn unify_types(types: &[Type]) -> Type {
    if types.is_empty() {
        Type::Any(HelpData::default())
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

fn install_package(name: &str) -> () {
    let _status = Command::new("Rscript")
        .args([
            "-e",
            &format!("if (!requireNamespace(\"{}\", quietly = TRUE)) install.packages(\"{}\")", name, name)])
        .status()
        .expect("failed to execute Rscript");
}

fn install_header(name: &str, context: &Context) -> Context {
    let full_path = if context.environment == Environment::Project {
        String::from("headers/") + name + ".ty"
    } else {
        name.to_string() + ".ty"
    };
    // copy it from the package
    // create an header file if not exist
    if !std::path::Path::new(&full_path).exists() {
        let _ = File::create(&full_path); 
    }
    
    let content = fs::read_to_string(full_path.clone()).unwrap();
    let adt_manager = AdtManager::new()
        .add_to_header(parse(LocatedSpan::new_extra(&content, full_path)).unwrap().1);
    let context2 = context.clone();
    adt_manager.get_header().iter()
            .fold(context2, |ctx, expr| eval(&ctx, expr))
}

pub fn eval(context: &Context, expr: &Lang) -> Context {
    match expr {
        Lang::Sequence(exprs, _h) 
            => exprs.iter().fold(context.clone(), |ctx, expr| eval(&ctx, expr)),
        Lang::Let(name, ty, exp, _h) => {
            let expr_ty = typing(&context, exp).0;
            let reduced_expr_ty = expr_ty.reduce(context);
            if ty.is_empty() {
                if exp.is_function() {
                    let first_param = expr_ty.to_function_type()
                        .unwrap()
                        .get_param_types()[0].clone();
                    let new_name = name.to_owned().set_type(first_param);
                    context.to_owned()
                            .push_var_type(new_name, reduced_expr_ty.to_owned(), context)
                            .add_generic_function(&[Lang::GenFunc(build_generic_function(&name.get_name()), name.get_name(), HelpData::default())])
                } else {
                    let new_name = name.to_owned().set_type(expr_ty);
                    context.to_owned()
                            .push_var_type(new_name, reduced_expr_ty.to_owned(), context)
                }
            } else {
                let reduced_ty = ty.reduce(context);

                let new_context = reduced_expr_ty.is_subtype(&reduced_ty).then(|| {
                    if *ty != builder::any_type() {
                        context.to_owned()
                            .push_var_type(name.to_owned().into(), reduced_ty.to_owned(), context)
                    } else {
                        context.to_owned()
                            .push_var_type(name.to_owned().into(), reduced_expr_ty.to_owned(), context)
                    }
                }).expect(&TypeError::Let(ty.clone(), expr_ty).display());
                if exp.is_function() && !exp.is_undefined() && new_context.compile_mode == CompileMode::Body {
                    new_context.add_generic_function(&[Lang::GenFunc(build_generic_function(&name.get_name()), name.get_name(), HelpData::default())])
                } else {
                    new_context
                }
            }
        },
        Lang::Alias(name, params, typ, h) => {
            let var = name.clone()
                .set_type(Type::Params(params.to_vec(), h.clone()));
            let new_context = context.clone()
                .push_var_type(var, typ.clone(), context);
            let (fn_typ, new_context2) = new_context.get_embeddings(typ);
            let new_context3 = fn_typ.iter()
                .fold(new_context2, |ctx, var_typfun| ctx.push_var_type(var_typfun.0.clone(), var_typfun.1.clone(), context));
            new_context3.push_alias(name.get_name(), typ.to_owned())
        },
        Lang::Assign(var, expr, _h) => {
            let variable_assigned = Var::from_language((**var).clone()).unwrap();
            let variable = context.get_true_variable(&variable_assigned);
            let var_type = context.get_type_from_variable(variable.clone());
            let var_type_reduced = reduce_type(context, &var_type);
            let expr_type = typing(&context, expr).0;
            let expr_type_reduced = reduce_type(context, &expr_type);
            if !(expr_type_reduced == var_type_reduced || expr_type_reduced.is_subtype(&var_type_reduced)) {
                panic!("{}", TypeError::Param(expr_type, var_type).display());
            } else if !variable.is_mutable() {
                panic!("{}", TypeError::ImmutableVariable(variable_assigned, variable).display());
            } else {
                context.clone()
            }
        }
        Lang::Library(name, _h) => {
            install_package(name);
            install_header(name, context)
        },
        Lang::ModuleDecl(_name, _h) 
            => context.clone().add_module_declarations(&[expr.clone()]),
        Lang::Signature(var, typ, _h) => {
            if var.is_variable(){
                context.clone().push_var_type(var.to_owned(), typ.to_owned(), context)
            } else {
                context.clone()
                    .push_alias(var.get_name(), var.get_type())
                    .push_var_type(var.to_owned(), typ.to_owned(), context)
            }
        },
        _ => context.clone()
    }
}

fn get_gen_type(type1: &Type, type2: &Type) -> Option<Vec<(Type, Type)>> {
        match (type1, type2) {
            (Type::Integer(i, _), Type::Integer(j, _)) => {
                (j.gen_of(i)).then(|| vec![])
            },
            (Type::Char(c, _), Type::Char(d, _)) => {
                (d.gen_of(c)).then(|| vec![])
            },
            (_, Type::Generic(_, _)) | (_, Type::IndexGen(_, _)) | (_, Type::LabelGen(_, _))
                => Some(vec![(type1.clone(), type2.clone())]),
            (Type::Function(_, args1, ret_typ1, _), Type::Function(_, args2, ret_typ2, _)) => {
                let res = args1.iter()
                    .zip(args2.iter())
                    .chain([(&(**ret_typ1), &(**ret_typ2))].iter().cloned())
                    .flat_map(|(typ1, typ2)| get_gen_type(typ1, typ2))
                    .flat_map(|x| x)
                    .collect::<Vec<_>>();
                //if res.len() > 0 { Some(res) } else { None }
                Some(res)
            }
            (Type::Array(ind1, typ1, _), Type::Array(ind2, typ2, _)) => {
               let gen1 = get_gen_type(ind1, ind2)
                   .unwrap_or(vec![]);
               let gen2 = get_gen_type(typ1, typ2)
                   .unwrap_or(vec![]);
               Some(gen1.iter().chain(gen2.iter()).cloned().collect())
            },
            (Type::Record(v1, _), Type::Record(v2, _)) => {
                   let res = v1.iter() 
                       .zip(v2.iter())
                       .flat_map(|(argt1, argt2)| {
                            let gen1 = get_gen_type(&argt1.get_argument(), &argt2.get_argument()).unwrap_or(vec![]);
                            let gen2 = get_gen_type(&argt1.get_type(), &argt2.get_type()).unwrap_or(vec![]);
                       gen1.iter().chain(gen2.iter()).cloned().collect::<Vec<_>>()
                       })
                       .collect::<HashSet<_>>()
                       .into_iter().collect::<Vec<_>>();
                    Some(res)
                },
                (Type::Union(types1, _), Type::Union(types2, _)) => {
                   Some(types1.iter() 
                       .zip(types2.iter())
                       .flat_map(|(typ1, typ2)| get_gen_type(&typ1.to_type(), &typ2.to_type()))
                       .fold(vec![], |acc: Vec<(Type, Type)>, vec| acc.iter().chain(vec.iter()).cloned().collect::<Vec<_>>()))
                },
                (Type::Tag(_name1, typ1, _h1), Type::Tag(_name2, typ2, _h2)) => {
                    get_gen_type(typ1, typ2)
                }
            (t1, t2) if t1 == t2 => Some(vec![]),
            _ => None
        }
}

pub fn match_types(ctx: &Context, type1: &Type, type2: &Type) 
    -> Option<Vec<(Type, Type)>> {
    let type1 = reduce_type(ctx, type1);
    let type2 = reduce_type(ctx, type2);
    let res = get_gen_type(&type1, &type2)
        .expect(&TypeError::Param(type2, type1).display());
    let unif_map = res.iter()
        .flat_map(|(arg, par)| unification::unify(ctx, &arg, &par))
        .collect::<Vec<_>>();
    Some(unif_map)
}

fn get_variable_type(lang: &Lang, tags: &[Tag]) -> Option<(Var, Type)> {
    if let Lang::Tag(name, variable, _h) = lang {
        let res = tags.iter()
            .find(|&tag| name == &tag.get_name());
        match res {
            Some(typ) => {
                let variable = Var::from_language((**variable).clone())
                    .unwrap_or(Var::from_name("_"));
                Some((variable, typ.get_type()))
            },
            _ => None
        }
        
    } else { panic!("The element in the left hand side of the match statement is not a tag") }
}

pub fn typing(context: &Context, expr: &Lang) -> (Type, Context) {
    match expr {
        Lang::Number(_, h) => (Type::Number(h.clone()), context.clone()),
        Lang::Integer(i, h) => (Type::Integer((*i).into(), h.clone()), context.clone()),
        Lang::Bool(_, h) => (Type::Boolean(h.clone()), context.clone()),
        Lang::Char(s, h) => (Type::Char(s.to_owned().into(), h.clone()), context.clone()),
        Lang::Empty(h) => (Type::Empty(h.clone()), context.clone()),
        Lang::And(e1, e2, _) | Lang::Or(e1, e2, _) => {
            if typing(context, e1).0.is_boolean() && typing(context, e2).0.is_boolean() {
                (Type::Boolean(HelpData::default()), context.clone())
            } else {
                panic!("Type error");
            }
        }
        Lang::Eq(e1, e2, _) | Lang::LesserOrEqual(e1, e2, _) | Lang::GreaterOrEqual(e1, e2, _) | Lang::GreaterThan(e1, e2, _) | Lang::LesserThan(e1, e2, _) => {
            let ty1 = typing(context, e1).0;
            let ty2 = typing(context, e2).0;
            if ty1 == ty2 {
                (Type::Boolean(HelpData::default()), context.clone())
            } else {
                panic!("Type error");
            }
        }
        Lang::Chain(e1, e2, _) => {
            let ty2 = typing(context, e2).0;
            match (reduce_type(context, &ty2), *e1.clone()) {
                (Type::Record(fields, _), Lang::Variable(name, _, _, _, _, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect("Field not found")
                },
                (_, Lang::FunctionApp(name, args, h)) 
                    if Var::from_language((*name).clone()).unwrap().get_name() == "map"
                        => {
                            let new_args = [(**e2).clone()].into_iter()
                                .chain(args.into_iter())
                                .collect::<Vec<_>>();
                            typing(context, &Lang::FunctionApp(name, new_args, h))
                        },
                (Type::Record(fields1, h), Lang::Record(fields2, _)) => {
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
                    (Type::Record(fields3, h.clone()), context.clone())
                },
                (a, b) => panic!("Type error we can't combine {} and {:?}", a, b)
            }
        },
        Lang::Function(kinds, params, ret_ty, body, h) => {
            let list_of_types = params.iter()
                .map(ArgumentType::get_type)
                .collect::<Vec<_>>();
            if body.is_empty_scope() && context.is_in_header_mode() {
                (Type::Function(kinds.clone(), list_of_types, Box::new(ret_ty.clone()), h.clone()), context.to_owned())
            } else {
                let sub_context = params.into_iter()
                    .map(|arg_typ| {
                        let new_type = reduce_type(context, &arg_typ.get_type())
                            .for_var();
                        Var::from_type(arg_typ.get_argument())
                            .expect("The arg_typ should have been label function")
                            .set_type(new_type)
                    })
                    .zip(list_of_types.clone().into_iter().map(|typ| reduce_type(context, &typ)))
                    .fold(context.clone(), |cont, (var, typ)| cont.clone().push_var_type(var, typ, &cont));
                let res = typing(&sub_context, body);
                let reduced_body_type = reduce_type(&sub_context, &res.0);
                let reduced_expected_ty = reduce_type(&context, &ret_ty);
                    if !reduced_body_type.is_subtype(&reduced_expected_ty) {
                        None.expect(
                        &TypeError::UnmatchingReturnType(reduced_expected_ty, reduced_body_type).display()
                                   )
                    }
                let new_context = res.1.unifications.into_iter()
                    .fold(context.clone(), |cont, uni_vec| cont.push_unifications(uni_vec));
                (Type::Function(kinds.clone(), list_of_types, Box::new(ret_ty.clone()), h.clone()), new_context)
            }
            }
        Lang::Sequence(exprs, _h) => {
            if exprs.len() == 1 {
                let res = exprs.clone().pop().unwrap();
                match context.compile_mode {
                    CompileMode::Body => {
                        typing(context, &res)
                    },
                    _ => {
                        (Type::Empty(HelpData::default()), eval(context, &res))
                    }
                }
            } else if exprs.len() == 0 {
                (Type::Empty(HelpData::default()), context.clone()) 
            } else {
                let context2 = context.clone();
                let mut exprs2 = exprs.clone();
                let exp = exprs2.pop().unwrap();
                let new_context = exprs.iter()
                    .fold(context2, |ctx, expr| eval(&ctx, expr));
                typing(&new_context, &exp)
            }
        },
        Lang::FunctionApp(fn_var_name, values, _h) => {
            let func = fn_var_name.clone()
                .get_related_function(values, context)
                .expect(&TypeError::UndefinedFunction((**fn_var_name).clone()).display());
            let param_types = func.get_param_types();
            context
                .get_unification_map(values, &param_types)
                .unwrap_or(UnificationMap::new(vec![]))
                .apply_unification_type(context, &func.get_ret_type())
        }
        Lang::Tag(name, expr, h) => {
            let ty = typing(context, expr).0;
            (Type::Tag(name.clone(), Box::new(ty), h.clone()), context.clone())
        }
        Lang::If(cond, true_branch, false_branch, _h) => {
            if typing(context, cond).0.is_boolean() {
                let true_ty = typing(context, true_branch).0;
                let false_ty = typing(context, false_branch).0;
                if true_ty.is_tag_or_union() && false_ty.is_tag_or_union() {
                    let res = unify_type(&true_ty, &false_ty);
                    (res, context.clone())
                } else {
                    panic!("Error: {} is not matching {}", true_ty, false_ty);
                }
            } else {
                panic!("Type error: {:?} isn't a boolean expression", cond);
            }
        }
        Lang::Array(exprs, h) => {
            let types = exprs.iter().map(|expr| typing(context, expr).0).collect::<Vec<_>>();
            if exprs.len() == 0 {
                let new_type = Type::Array(
                    Box::new(builder::integer_type(0)),
                    Box::new(builder::any_type()),
                    h.clone());
                (new_type, context.clone())
            } else if types.windows(2).all(|w| w[0] == w[1]) {
                let new_type = Type::Array(
                    Box::new(builder::integer_type(exprs.len() as i32)),
                    Box::new(types[0].clone()),
                    h.clone());
                (new_type, context.clone())
            } else {
                panic!("Type error");
            }
        }
        Lang::Record(fields, h) => {
            let field_types = fields.iter()
                .map(|arg_val| {
                    (arg_val.get_argument(),
                    typing(context, &arg_val.get_value()).0).into()
                }).collect();
            (Type::Record(field_types, h.clone()), context.clone())
        }
        Lang::Match(val, branches, _h) => {
            let val_ty = reduce_type(context, &typing(context, val).0);
            match val_ty {
                Type::Union(union_types, _) => {
                    let branch_types = branches.iter()
                        .map(|(tag, exp)| (get_variable_type(tag, &union_types)
                             .expect("The tag branch is not part of the union type"),
                             exp))
                        .map(|((var, typ), exp)| {
                             typing(&context.clone().push_var_type(var, typ, context), exp).0
                        })
                        .collect::<Vec<_>>();
                        (unify_types(&branch_types), context.clone())
                    },
                _ => panic!("Type error"),
            }
        }
        Lang::ArrayIndexing(expr, index, _h) => {
            let ty = typing(context, expr).0;
            match ty {
                Type::Array(len, elem_ty, _) => {
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
        Lang::Variable(_, _, _, _, _, _) => {
            let old_var = Var::from_language(expr.clone()).unwrap();
            let var = context.get_true_variable(&old_var);
            if var.is_private() && var.is_foreign() {
               panic!("{}", TypeError::PrivateVariable(old_var, var).display())
            } else {
                (context.get_type_from_variable(var), context.clone())
            }
        },
        Lang::Scope(expr, _) if expr.len() == 1 => {
            typing(context, &expr[0])
        },
        Lang::Scope(expr, h) => typing(context, &Lang::Sequence(expr.to_vec(), h.clone())),
        Lang::Tuple(elements, h) => {
            (Type::Tuple(elements.iter()
                .map(|x| typing(context, x).0)
                .collect(), h.clone()),
                context.clone())
        },
        Lang::VecBloc(_, h) => (Type::Empty(h.clone()), context.to_owned()),
        _ => (Type::Any(HelpData::default()), context.clone()),
    }
}

fn unify_type(ty1: &Type, ty2: &Type) -> Type {
    match (ty1, ty2) {
        (Type::Tag(name1, params1, h1), Type::Tag(name2, params2, _h2)) => {
            if name1 == name2 && params1 == params2 {
                ty1.clone()
            } else {
                Type::Union(vec![
                            Tag::from_type(ty1.clone()).unwrap(),
                            Tag::from_type(ty2.clone()).unwrap()], h1.clone())
            }
        }
        (Type::Union(union1, h1), Type::Tag(name, params, h2)) => {
            let mut union2 = union1.clone();
            union2.push(Tag::new(name.clone(), *params.clone(), h2.clone()));
            Type::Union(union2, h1.clone())
        }
        (Type::Tag(name, params, h), Type::Union(union1, _)) => {
            let mut union2 = union1.clone();
            union2.push(Tag::new(name.clone(), *params.clone(), h.clone()));
            Type::Union(union2, h.clone())
        }
        (Type::Any(_), _) | (_, Type::Any(_)) => Type::Any(HelpData::default()),
        (Type::Empty(_), ty) | (ty, Type::Empty(_)) => ty.clone(),
        (ty1, ty2) if ty1 == ty2 => ty1.clone(),
        _ => Type::Empty(HelpData::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_equality(){
        let a = builder::integer_type(2);
        let b = builder::integer_type(17);
        assert!(a == b)
    }
}
