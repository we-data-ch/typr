#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
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
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;
use crate::builder;
use crate::TypeError;
use crate::help_message::ErrorMsg;
use std::error::Error;
use crate::argument_value::ArgumentValue;
use crate::typer::Typer;
use crate::type_comparison::is_matching;
use crate::function_type::FunctionType;

fn execute_r_function(function_code: &str) -> Result<String, Box<dyn Error>> {
    // Créer un script R temporaire avec la fonction à exécuter
    let r_script = format!("{}\n", function_code);

    // Exécuter la commande Rscript avec le script
    let output = Command::new("Rscript")
        .arg("-e") // -e permet d'exécuter une expression R directement
        .arg(&r_script)
        .output()?; // Exécute la commande et capture la sortie

    // Vérifier si l'exécution a réussi
    if output.status.success() {
        // Convertir la sortie standard (stdout) en chaîne de caractères
        let stdout = String::from_utf8(output.stdout)?;
        Ok(stdout.trim().to_string())
    } else {
        // En cas d'erreur, retourner stderr comme erreur
        let stderr = String::from_utf8(output.stderr)?;
        Err(format!("Erreur lors de l'exécution de R: {}", stderr).into())
    }
}

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
    let full_path = if context.in_a_project() {
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
            let expr_ty = exp.typing(&context.deep_clone()).0;
            if ty.is_empty() {
                let res = if exp.is_function() && (exp.nb_params() > 0) {
                    let first_param = expr_ty.to_function_type()
                        .unwrap()
                        .get_param_types()[0].clone();
                    let new_name = name.to_owned().set_type(first_param, context);
                    let res = context.to_owned()
                            .push_var_type(new_name, expr_ty.to_owned(), context)
                            .add_generic_function(&[build_generic_function(&name.get_name())]);
                            // TODO: check for already existing generic function upthere
                    res
                } else if exp.is_r_function() {
                    let new_name = name.to_owned().set_type(builder::any_type(), context);
                    context.clone().push_var_type(new_name, builder::r_function_type(), context)
                } else {
                    let new_name = name.to_owned()
                        .set_type(expr_ty.clone(), context);
                    context.to_owned()
                            .push_var_type(new_name, expr_ty.to_owned(), context)
                };
                res
            } else {
                let new_context = expr_ty.is_subtype(&ty, context).then(|| {
                    if !ty.is_any() {
                        context.to_owned()
                            .push_var_type(name.to_owned().into(), ty.to_owned(), context)
                    } else {
                        context.to_owned()
                            .push_var_type(name.to_owned().into(), expr_ty.to_owned(), context)
                    }
                }).expect(&TypeError::Let(ty.clone(), expr_ty).display());
                if exp.is_function() && !exp.is_undefined() {
                    new_context.add_generic_function(&[build_generic_function(&name.get_name())])
                } else {
                    new_context
                }
            }
        },
        Lang::Alias(name, params, typ, h) => {
            let var = name.clone()
                .set_type(Type::Params(params.to_vec(), h.clone()), context);
            let new_context = context.clone()
                .push_var_type(var, typ.clone(), context);
            let (fn_typ, new_context2) = new_context.get_embeddings(typ);
            let new_context3 = fn_typ.iter()
                .fold(new_context2, |ctx, var_typfun| ctx.push_var_type(var_typfun.0.clone(), var_typfun.1.clone(), context));
            new_context3.push_alias(name.get_name(), typ.to_owned())
        },
        Lang::Assign(var, expr, _h) => {
            let variable_assigned = Var::try_from(var.clone()).unwrap();
            let expr_type = typing(&context, expr).0;
            let expr_type_reduced = reduce_type(context, &expr_type);
            if !context.we_check_mutability() {
               variable_assigned.exist(context) 
                   .map(|var| {
                        let res = is_matching(context, &expr_type_reduced, &var.get_type())
                            .then_some(context.clone()
                                       .update_variable(var.clone()
                                        .set_type(expr_type_reduced.clone(), context)));
                        match res {
                            Some(val) => val,
                            None => panic!("{}", TypeError::Param(var.get_type(), expr_type_reduced.clone()).display())
                        }
                   })
                   .unwrap_or(context.clone().push_var_type(
                                        variable_assigned.set_type(expr_type_reduced.clone(), context),
                                        expr_type_reduced, &context))
            } else {
            println!("We don't check");
                let variable = context.get_true_variable(&variable_assigned);
                let var_type = context.get_type_from_existing_variable(variable.clone());
                let var_type_reduced = reduce_type(context, &var_type);
                if (expr_type_reduced != var_type_reduced) && !expr_type_reduced.is_subtype(&var_type_reduced, context) {
                    panic!("{}", TypeError::Param(expr_type, var_type).display());
                } else if !variable.is_mutable() && context.we_check_mutability() {
                    panic!("{}", TypeError::ImmutableVariable(variable_assigned, variable).display());
                } else {
                    context.clone()
                }
            }
        }
        Lang::Library(name, _h) => {
            install_package(name);
            let function_list = execute_r_function(&format!("library({})\n\nls('package:{}')", name, name))
                .expect("The R command didn't work");
            let new_context = context.append_function_list(&function_list);
            install_header(name, &new_context)
        },
        Lang::ModuleDecl(_name, _h) 
            => context.clone().add_module_declarations(&[expr.clone()]),
        Lang::Signature(var, typ, _h) => {
            if var.is_variable(){
                let new_var = FunctionType::try_from(typ.clone())
                            .map(|ft| var.clone().set_type(ft.get_first_param().unwrap_or(builder::empty_type()), context))
                            .unwrap_or(var.clone());
                context.clone().push_var_type(new_var, typ.to_owned(), context)
            } else { // is alias
                context.clone()
                    .push_var_type(var.to_owned(), typ.to_owned(), context)
            }
        },
        _ => context.clone()
    }
}

fn get_gen_type(type1: &Type, type2: &Type) -> Option<Vec<(Type, Type)>> {
        match (type1, type2) {
            (_, Type::Any(_)) => Some(vec![]),
            (Type::Integer(i, _), Type::Integer(j, _)) => {
                if j.gen_of(i) || i == j {
                    Some(vec![])
                } else {
                    None
                }
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
                Some(res)
            }
            (Type::Array(ind1, typ1, _), Type::Array(ind2, typ2, _)) => {
               let gen1 = get_gen_type(ind1, ind2);
               let gen2 = get_gen_type(typ1, typ2);
                match (gen1, gen2) {
                    (None, _) | (_, None) => None,
                    (Some(g1), Some(g2)) 
                        => Some(g1.iter().chain(g2.iter()).cloned().collect())
                }
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
                (Type::StrictUnion(types1, _), Type::StrictUnion(types2, _)) => {
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
            (typing(context, e1).0.is_boolean() && typing(context, e2).0.is_boolean())
                .then_some((builder::boolean_type(), context.clone()))
                .expect("Type error")
        }
        Lang::Eq(e1, e2, _) | Lang::LesserOrEqual(e1, e2, _) | Lang::GreaterOrEqual(e1, e2, _) | Lang::GreaterThan(e1, e2, _) | Lang::LesserThan(e1, e2, _) => {
            (typing(context, e1).0 == typing(context, e2).0)
                .then_some((builder::boolean_type(), context.clone()))
                .expect("Type error")
        }
        Lang::Chain(e1, e2, _) => {
            let ty2 = typing(context, e2).0;
            match (ty2.reduce(context), *e1.clone()) {
                (Type::Record(fields, _), Lang::Variable(name, _, _, _, _, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect("Field not found")
                },
                (Type::Record(fields, _), Lang::Char(name, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect("Field not found")
                },
                (Type::Tuple(vals, _), Lang::Integer(i, _)) => {
                    vals.iter()
                        .nth((i-1) as usize)
                        .map(|typ| (typ.clone(), context.clone()))
                        .expect(&format!("no value at the position {}", i))
                },
                (Type::Record(fields1, h), Lang::Record(fields2, _)) => {
                    let at = fields2[0].clone();
                    let fields3 = fields1.iter()
                        .map(replace_fields_type_if_needed(context, at)).collect::<Vec<_>>();
                    (Type::Record(fields3, h.clone()), context.clone())
                },
                (a, b) => panic!("Type error we can't combine {} and {:?}", a, b)
            }
        },
        Lang::Function(kinds, params, ret_ty, body, h) => {
            let list_of_types = params.iter()
                .map(ArgumentType::get_type)
                .collect::<Vec<_>>();
            let sub_context = params.into_iter()
                .map(|arg_typ| arg_typ.clone().to_var(context))
                .zip(list_of_types.clone().into_iter().map(|typ| typ.reduce(context)))
                .fold(context.clone(), |cont, (var, typ)| cont.clone().push_var_type(var, typ, &cont));
            let res = body.typing(&sub_context);
            let reduced_body_type = res.0.reduce(&sub_context);
            let reduced_expected_ty = ret_ty.reduce(&context);
            if !reduced_body_type.is_subtype(&reduced_expected_ty, context) {
                None.expect(
                    &TypeError::UnmatchingReturnType(reduced_expected_ty, reduced_body_type).display())
            }
            (Type::Function(kinds.clone(), list_of_types, Box::new(ret_ty.clone()), h.clone()), res.1)
            }
        Lang::Sequence(exprs, _h) => {
            if exprs.len() == 1 {
                let res = exprs.clone().pop().unwrap();
                let (typ, cont) = typing(context, &res);
                (typ.clone(), context.clone().push_var_type(Var::from("_out"), typ, &context))
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
        Lang::FunctionApp(fn_var_name, values, _, h) => {
            let var = Var::try_from(fn_var_name.clone()).unwrap();
            if context.is_an_untyped_function(&var.get_name()) {
                (Type::Empty(h.clone()), context.clone())
            } else {
                let func = fn_var_name.clone()
                    .get_related_function(values, context)
                    .expect(&TypeError::UndefinedFunction((**fn_var_name).clone()).display());
                let param_types = func.get_param_types();
                let unification_map = context
                        .get_unification_map(values, &param_types)
                        .unwrap_or(UnificationMap::new(vec![]));
                let (new_ret_typ, new_context) = unification_map
                        .apply_unification_type(context, &func.get_ret_type());
                let params = func.get_param_types().iter()
                            .map(|p| unification_map.apply_unification_type(context, p).0)
                            .collect::<Vec<_>>();
                (new_ret_typ.clone(), new_context.push(expr.clone(), func.set_params(params).set_ret_type(new_ret_typ)))
            }
        }
        Lang::Tag(name, expr, h) => {
            let ty = typing(context, expr).0;
            (Type::Tag(name.clone(), Box::new(ty), h.clone()), context.clone())
        }
        Lang::If(cond, true_branch, false_branch, _h) => {
            if typing(context, cond).0.is_boolean() {
                let true_ty = typing(context, true_branch).0;
                let false_ty = typing(context, false_branch).0;
                let set = if let Type::Union(v, h) = false_ty {
                    let mut set = v; set.insert(true_ty);
                    set
                    //(Type::Union(set.clone(), h), context.clone())
                } else if false_ty.is_empty() {
                    let mut set = HashSet::new(); set.insert(true_ty);
                    set
                    //(Type::Union(set.clone(), HelpData::default()), context.clone())
                } else {
                    let mut set = HashSet::new(); 
                    set.insert(false_ty);
                    set.insert(true_ty);
                    set
                    //(Type::Union(set.clone(), HelpData::default()), context.clone())
                };
                if set.len() == 1 {
                    (set.iter().cloned().next().unwrap(), context.clone())
                } else {
                    (Type::Union(set.clone(), HelpData::default()), context.clone())
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
                (new_type.clone(), context.clone().push_types(&[new_type]))
            } else if types.windows(2).all(|w| w[0] == w[1]) {
                let new_type = Type::Array(
                    Box::new(builder::integer_type(exprs.len() as i32)),
                    Box::new(types[0].clone()),
                    h.clone());
                (new_type.clone(), context.clone().push_types(&[new_type]))
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
        Lang::Match(exp, var, branches, _h) => {
            let var_ty = reduce_type(context, &typing(context, &**exp).0);
            match var_ty {
                Type::Union(union_types, h) => {
                    let set = branches.iter().map(|(t, _)| t).cloned().collect::<HashSet<Type>>();
                    if union_types != set {
                        panic!("Some types are missing");
                    }
                    let types = branches.iter()
                        .map(|(typ, bexp)| {
                            let new_context = context.clone().push_var_type(var.clone(), typ.clone(), context);
                            typing(&new_context, bexp).0
                        }).collect::<HashSet<_>>();
                    let output_type = if types.len() == 1 {
                        types.iter().next().unwrap().clone()
                    } else {Type::Union(types, h)};
                    (output_type, context.clone())
                },
                _ => panic!("Type error"),
            }
        }
        Lang::ArrayIndexing(expr, index, _h) => {
            let ty = typing(context, expr).0;
            match ty {
                Type::Array(len, elem_ty, _) => {
                    match Index::from_type(&(*len)) {
                        Some(n)  => {
                            let index2 = n.get_value();
                            if (*index as u32) <  index2 {
                                (*elem_ty, context.clone())
                            } else {
                                panic!("Index out of bounds");
                            }
                        },
                        // TODO: check condition in Index::from_type() if
                        // we get generics (#M), Any or Empty as an index for 
                        // array type: [id, type]
                       None => (*elem_ty, context.clone())
                    }
                },
                Type::Any(h) => {
                    (builder::empty_type().set_help_data(h), context.clone())
                },
                _ => panic!("Indexing error: {:?} can't be indexable by {}",
                        expr, 
                        index),
            }
        },
        Lang::Variable(_, _, _, _, _, _) => {
            let old_var = Var::try_from(expr.clone()).unwrap();
            let var = context.get_true_variable(&old_var);
            if var.is_private() && var.is_from_other_module() {
               panic!("{}", TypeError::PrivateVariable(old_var, var).display())
            } else {
                (context.get_type_from_existing_variable(var), context.clone())
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
        Lang::RFunction(_, _, h) => (Type::RFunction(h.clone()), context.to_owned()),
        Lang::ForLoop(var, iter, body, _h) => {
            let base_type = typing(context, iter).0.to_array()
                .expect(&format!("The iterator is not an array {:?}", iter))
                .base_type;
            let var = var.clone().set_type(base_type.clone(), context);
            Typer::from(context.clone())
                .set_type(base_type)
                .set_var(var)
                .push_var_type()
                .typing((**body).clone());
            (builder::empty_type(), context.clone())
        },
        _ => (Type::Any(HelpData::default()), context.clone()),
    }
}

fn replace_fields_type_if_needed(context: &Context, at: ArgumentValue) -> impl FnMut(&ArgumentType) -> ArgumentType + use<'_> {
    move |arg_typ2| {
        (arg_typ2.get_argument_str() == at.get_argument())
            .then_some(ArgumentType::new(&at.get_argument(),
                        &typing(context, &at.get_value()).0))
            .unwrap_or(arg_typ2.clone())
    }
}

fn unify_type(ty1: &Type, ty2: &Type) -> Type {
    match (ty1, ty2) {
        (Type::Tag(name1, params1, h1), Type::Tag(name2, params2, _h2)) => {
            if name1 == name2 && params1 == params2 {
                ty1.clone()
            } else {
                Type::StrictUnion(vec![
                            Tag::from_type(ty1.clone()).unwrap(),
                            Tag::from_type(ty2.clone()).unwrap()], h1.clone())
            }
        }
        (Type::StrictUnion(union1, h1), Type::Tag(name, params, h2)) => {
            let mut union2 = union1.clone();
            union2.push(Tag::new(name.clone(), *params.clone(), h2.clone()));
            Type::StrictUnion(union2, h1.clone())
        }
        (Type::Tag(name, params, h), Type::StrictUnion(union1, _)) => {
            let mut union2 = union1.clone();
            union2.push(Tag::new(name.clone(), *params.clone(), h.clone()));
            Type::StrictUnion(union2, h.clone())
        }
        (Type::Any(_), _) | (_, Type::Any(_)) => Type::Any(HelpData::default()),
        (Type::Empty(_), ty) | (ty, Type::Empty(_)) => ty.clone(),
        (ty1, ty2) if ty1 == ty2 => ty1.clone(),
        _ => Type::Empty(HelpData::default())
    }
}

fn if_strict_mode(){
    todo!();
        //if typing(context, cond).0.is_boolean() {
            //let true_ty = typing(context, true_branch).0;
            //let false_ty = typing(context, false_branch).0;
            //if true_ty.is_tag_or_union() && false_ty.is_tag_or_union() {
                //let res = unify_type(&true_ty, &false_ty);
                //(res, context.clone())
            //} else if true_ty == false_ty {
                //(true_ty, context.clone())
            //} else if false_ty == builder::empty_type() {
                //(true_ty, context.clone())
            //} else {
                //panic!("Error: {} is not matching {}", true_ty, false_ty);
            //}
        //} else {
            //panic!("Type error: {:?} isn't a boolean expression", cond);
        //}
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
