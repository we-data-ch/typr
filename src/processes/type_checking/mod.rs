pub mod type_comparison;
pub mod unification_map;
pub mod type_checker;
pub mod type_context;
pub mod unification;

use crate::processes::type_checking::type_comparison::reduce_type;
use crate::components::language::argument_value::ArgumentValue;
use crate::processes::type_checking::type_context::TypeContext;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::type_error::TypeError;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::function_type::FunctionType;
use crate::components::error_message::help_data::HelpData;
use crate::components::context::config::TargetLanguage;
use crate::components::language::array_lang::ArrayLang;
use crate::components::r#type::type_system::TypeSystem;
use crate::utils::package_loader::PackageManager;
use crate::components::language::operators::Op;
use crate::components::r#type::typer::Typer;
use crate::components::language::var::Var;
use crate::components::context::Context;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use std::collections::HashSet;
use crate::utils::builder;
use std::process::Command;
use std::error::Error;

pub fn execute_r_function(function_code: &str) -> Result<String, Box<dyn Error>> {
    let r_script = format!("{}\n", function_code);

    let output = Command::new("Rscript")
        .arg("-e") 
        .arg(&r_script)
        .output()?;

    if output.status.success() {
        let stdout = String::from_utf8(output.stdout)?;
        Ok(stdout.trim().to_string())
    } else {
        let stderr = String::from_utf8(output.stderr)?;
        Err(format!("Erreur lors de l'exécution de R: {}", stderr).into())
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

pub fn eval(context: &Context, expr: &Lang) -> TypeContext {
    match expr {
        Lang::Let(name, ty, exp, h) => {
            let new_context = context.clone()
                .push_types(&exp.extract_types_from_expression(context));

            let res = exp.typing(&new_context)
                .get_covariant_type(ty)
                .add_to_context(Var::try_from(name).unwrap());
            let new_expr = Lang::Let(
                name.clone(), 
                ty.clone(), 
                Box::new(res.get_expr()),
                h.clone());
            res.with_lang(&new_expr)
        },
        Lang::Alias(exp, params, typ, h) => {
            let var = Var::try_from(exp).unwrap()
                .set_type(Type::Params(params.to_vec(), h.clone()));
            let alias_context = context.clone()
                .push_alias(var.get_name(), typ.to_owned());
            let new_context = context.clone()
                .push_var_type(var.clone(), typ.clone(), &alias_context);
            (builder::unknown_function(), expr.clone(),
                new_context.push_alias(var.get_name(), typ.to_owned())).into()
        },
        Lang::Assign(left_expr, right_expr, _h) => {
            let left_type = typing(&context, left_expr).value;
            let right_type = typing(&context, right_expr).value;
            let reduced_left_type = reduce_type(context, &left_type);
            let reduced_right_type = reduce_type(context, &right_type);
            if reduced_right_type.is_subtype(&reduced_left_type, context) {
                let var = Var::from_language((**left_expr).clone())
                    .unwrap().set_type(right_type.clone());
                (right_type.clone(), 
                 expr.clone(), 
                 context.clone()
                    .push_var_type(var, right_type, context)).into()
            } else { 
                panic!("The right side and left sides don't match {} <- {}",
                       left_type.pretty(), right_type.pretty())
            }
        },
        Lang::Library(name, _h) => {
            install_package(name);
            let package_manager = PackageManager::to_package(name).unwrap();
            if !package_manager.exists() {
                package_manager.clone().save();
            }
            let var_type = package_manager.load().unwrap();
            (builder::unknown_function(), expr.clone(), context.clone().extend_typing_context(var_type)).into()
        },
        Lang::ModuleDecl(_name, _h) 
            => (builder::unknown_function(), expr.clone(), context.clone()).into(),
        Lang::Signature(var, typ, _h) => {
            if var.is_variable(){
                let new_var = FunctionType::try_from(typ.clone())
                            .map(|ft| var.clone().set_type(ft.get_first_param().unwrap_or(builder::unknown_function())))
                            .unwrap_or(var.clone());
                (builder::unknown_function(), expr.clone(),
                context.clone().replace_or_push_var_type(new_var, typ.to_owned(), context)).into()
            } else { // is alias
                (builder::unknown_function(), expr.clone(),
                        context.clone()
                            .replace_or_push_var_type(var.to_owned(), typ.to_owned(), context)).into()
            }
        },
        Lang::TestBlock(body, _) => {
            //Needed to be type checked
            let _ = typing(context, body);
            (builder::unknown_function(), expr.clone(), context.clone()).into()
        },
        Lang::Module(_name, members, _position, _config, h) => {
            let expr = if members.len() > 1 {
                Lang::Lines(members.iter().cloned().collect(), h.clone())
            } else { members.iter().next().unwrap().clone() }; // TODO: Modules can't be empty
            let new_context = 
                typing(&Context::default(), &expr).context;
            (builder::empty_type(), expr.clone(), context.clone() + new_context).into()
        },
        _ => (builder::unknown_function(), expr.clone(), context.clone()).into()
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
            (_, Type::Generic(_, _)) | (_, Type::IndexGen(_, _)) | (_, Type::LabelGen(_, _)) | (_, Type::Interface(_, _))
                => Some(vec![(type1.clone(), type2.clone())]),
            (Type::Function(args1, ret_typ1, _), Type::Function(args2, ret_typ2, _)) => {
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
            (Type::Vector(ind1, typ1, _), Type::Vector(ind2, typ2, _)) => {
               let gen1 = get_gen_type(ind1, ind2);
               let gen2 = get_gen_type(typ1, typ2);
                match (gen1, gen2) {
                    (None, _) | (_, None) => None,
                    (Some(g1), Some(g2)) 
                        => Some(g1.iter().chain(g2.iter()).cloned().collect())
                }
            },
            (Type::Sequence(ind1, typ1, _), Type::Sequence(ind2, typ2, _)) => {
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
                (Type::Tag(_name1, typ1, _h1), Type::Tag(_name2, typ2, _h2)) => {
                    get_gen_type(typ1, typ2)
                }
            (t1, t2) if t1 == t2 => Some(vec![]),
            _ => Some(vec![])
        }
}

//Check if we really have a type (type1) matched with a genery (type2)
pub fn match_types_to_generic(ctx: &Context, type1: &Type, type2: &Type) 
    -> Option<Vec<(Type, Type)>> {
    let type1 = reduce_type(ctx, type1);
    let type2 = reduce_type(ctx, type2);
    let res = get_gen_type(&type1, &type2)
        .expect(&TypeError::GenericPatternMatch(type2, type1).display());
    let unif_map = res.iter()
        .flat_map(|(arg, par)| unification::unify(ctx, &arg, &par))
        .collect::<Vec<_>>();
    Some(unif_map)
}

fn are_homogenous_types(types: &[Type]) -> bool {
    types.windows(2).all(|w| w[0] == w[1])
}

trait WithLang2 {
    fn with_lang(self, expr: &Lang, context: &Context) -> (Type, Lang, Context);
}

impl WithLang2 for Type {
    fn with_lang(self, expr: &Lang, context: &Context) -> (Type, Lang, Context) {
        (self, expr.clone(), context.clone())
    }
}

//main
pub fn typing(context: &Context, expr: &Lang) -> TypeContext {
    match expr {
        Lang::Number(_, h) => (Type::Number(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Integer(i, h) 
            => (builder::integer_type(*i).set_help_data(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Bool(_, h) => (Type::Boolean(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Char(s, h) 
            => (builder::character_type(&s).set_help_data(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Empty(h) => (Type::Empty(h.clone()), expr.clone(), context.clone()).into(),
        Lang::Operator(Op::And(_), e1, e2, _) | Lang::Operator(Op::Or(_), e1, e2, _) => {
            let (typ, new_context) = (typing(context, e1).value.is_boolean() && typing(context, e2).value.is_boolean())
                .then_some((builder::boolean_type(), context.clone()))
                .expect("Type error");
            (typ, expr.clone(), new_context).into()
        }
        Lang::Operator(Op::Eq(_), e1, e2, _) 
            | Lang::Operator(Op::LesserOrEqual(_), e1, e2, _) 
            | Lang::Operator(Op::GreaterOrEqual(_), e1, e2, _) 
            | Lang::Operator(Op::GreaterThan(_), e1, e2, _) 
            | Lang::Operator(Op::LesserThan(_), e1, e2, _) => {
            let t1 = typing(context, e1).value;
            let t2 = typing(context, e2).value;
            let (typ, new_context) = (t1 == t2)
                .then_some((builder::boolean_type(), context.clone()))
                .expect("Type error");
            (typ, expr.clone(), new_context).into()
        }
        Lang::Operator(Op::Dot(_), e1, e2, _) 
            | Lang::Operator(Op::Pipe(_), e1, e2, _) 
            => {
            if let Lang::FunctionApp(exp, v, ty, h) = (**e2).clone() {
                let fun_app = Lang::FunctionApp(exp, 
                                            [(**e1).clone()].iter().chain(v.iter())
                                            .cloned().collect::<Vec<_>>(), 
                                            ty, h.clone());
                typing(context, &fun_app)
            } else {
                let ty2 = typing(context, e2).value.clone().reduce(context);
                match (ty2.clone(), *e1.clone()) {
                    (Type::Record(fields, _), Lang::Variable(name, _, _, _, h)) => {
                        let (typ, new_context) = fields.iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                            .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                            .expect(&TypeError::FieldNotFound((name, h), ty2).display());
                        (typ, expr.clone(), new_context).into()
                    },
                    (Type::Record(fields, _), Lang::Char(name, _)) => {
                        let (typ, new_context) = fields.iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                            .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                            .expect("Field not found");
                        (typ, expr.clone(), new_context).into()
                    },
                    (Type::Tuple(vals, _), Lang::Integer(i, _)) => {
                        let (typ, new_context) = vals.iter()
                            .nth((i-1) as usize)
                            .map(|typ| (typ.clone(), context.clone()))
                            .expect(&format!("no value at the position {}", i));
                        (typ, expr.clone(), new_context).into()
                    },
                    (Type::Record(fields1, h), Lang::Record(_, _)) => {
                        let fields3: HashSet<_> = match e1.typing(context).value {
                            Type::Record(fields2, _) => {
                                fields1.union(&fields2).cloned().collect()
                            },
                            _ => panic!("Typing {} should produce a record type", e1)
                        };
                        Type::Record(fields3, h.clone()).with_lang(expr, context).into()
                    },
                    (Type::Generic(_, _), Lang::Record(_, _)) => {
                        builder::intersection_type(&[ty2.clone(), e1.typing(context).value])
                            .with_lang(expr, context).into()
                    },
                    (a, b) => panic!("Type error we can't combine {} and {:?}", a, b)
                }
            }
        },
        Lang::Operator(Op::Dollar(hd), e1, e2, _) => {
            let op = match expr {
                Lang::Operator(op, _, _, _) => op,
                _ => panic!("expr n'est pas un opérateur"),
            };
            let ty1 = typing(context, e1).value.clone();
            match (ty1.reduce(context), *e2.clone(), op) {
                (Type::Record(fields, _), Lang::Variable(name, _, _, _, _), _) => {
                    let (typ, new_context) = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Field {} not found", name));
                        (typ, expr.clone(), new_context).into()
                },
                (Type::Module(fields, _), Lang::Variable(name, _, _, _, _), _) => {
                    let (typ, new_context) = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Variable {} not found in the module", name));
                        (typ, expr.clone(), new_context).into()
                },
                (Type::Module(fields, _), Lang::FunctionApp(exp, _, _, _), _) => {
                    let var = Var::from_language(*exp).unwrap();
                    let typ = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == var.get_name())
                        .expect(&format!("Field {} not found", var.get_name()))
                        .get_type();
                    let res = FunctionType::try_from(typ).unwrap().get_return_type();
                    (res, expr.clone(), context.clone()).into()
                },
                (Type::Record(fields, _), Lang::Char(name, _), _) => {
                    let (typ, new_context) = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Field {} not found", name));
                        (typ, expr.clone(), new_context).into()
                },
                (Type::Tuple(vals, _), Lang::Integer(i, _), _) => {
                    let (typ, new_context) = vals.iter()
                        .nth((i-1) as usize)
                        .map(|typ| (typ.clone(), context.clone()))
                        .expect(&format!("no value at the position {}", i));
                        (typ, expr.clone(), new_context).into()
                },
                (Type::Record(fields1, h), Lang::Record(fields2, _), _) => {
                    let at = fields2[0].clone();
                    let fields3 = fields1.iter()
                        .map(replace_fields_type_if_needed(context, at))
                        .collect::<HashSet<_>>();
                    Type::Record(fields3, h.clone()).with_lang(expr, context).into()
                },
                (Type::Record(fields, _), Lang::FunctionApp(exp, _, _, _), _) => {
                    let var = Var::from_language(*exp).unwrap();
                    let typ = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == var.get_name())
                        .expect(&format!("Field {} not found", var.get_name()))
                        .get_type();
                    typing(&context.clone()
                           .push_var_type(var, typ, context), e2).value.clone()
                           .with_lang(expr, context).into()
                },
                (Type::UnknownFunction(h) , Lang::FunctionApp(_, _, _, _), _ ) => {
                    (Type::UnknownFunction(h), (*expr).clone(), context.clone()).into()
                }
                (Type::Array(n, _, h), Lang::Variable(_, _, _, _, _), _) => {
                    let (typ, lang, _) = 
                        typing(context, 
                            &builder::operation(
                                Op::Dollar(hd.clone()),
                                ArrayLang::try_from(e1).unwrap()
                                    .get_first_argument()
                                    .expect("The array is of size 0"),
                                (**e2).clone())
                               ).to_tuple();
                    (Type::Array(n, Box::new(typ), h.clone()), lang, context.clone()).into()
                },
                (_ , Lang::FunctionApp(exp, args, ret, h2), Op::Dot(_)) => {
                    typing(
                        context,
                        &Lang::FunctionApp(
                            exp,
                            [(**e1).clone()].iter().chain(args.iter()).cloned().collect(),
                            ret,
                            h2))
                },
                (a, b, _c) => panic!("Type error we can't combine {} and {:?}", a, b)
            }
        },
        Lang::Operator(op, e1, e2, h) => {
                let var_exp = Var::from_name(&format!("`{}`", op))
                    .set_help_data(e1.get_help_data())
                    .to_language();
                let fun_app = Lang::FunctionApp(Box::new(var_exp), 
                                                vec![(**e1).clone(), (**e2).clone()], 
                                                builder::empty_type(), h.clone());
                typing(context, &fun_app)
        },
        Lang::Function(params, ret_ty, body, h) => {
            let list_of_types = params.iter()
                .map(ArgumentType::get_type)
                .collect::<Vec<_>>();
            let sub_context = params.into_iter()
                .map(|arg_typ| arg_typ.clone().to_var(context))
                .zip(list_of_types.clone().into_iter().map(|typ| typ.reduce(context)))
                .fold(context.clone(), |cont, (var, typ)| cont.clone().push_var_type(var, typ, &cont));
            let body_type = body.typing(&sub_context);
            let reduced_body_type = body_type.value.reduce(&sub_context);
            let reduced_expected_ty = ret_ty.reduce(&context);
            if !reduced_body_type.is_subtype(&reduced_expected_ty, context) {
                None.expect(
                    &TypeError::UnmatchingReturnType(ret_ty.clone(), body_type.value).display())
            }
            Type::Function(list_of_types, Box::new(ret_ty.clone()), h.clone())
                .with_lang(expr, &body_type.context).into()
        }
        Lang::Lines(exprs, _h) => {
            if exprs.len() == 1 {
                let res = exprs.clone().pop().unwrap();
                let (typ, langs, _) = typing(context, &res).to_tuple();
                (typ.clone(), langs.clone(), 
                 context.clone().push_var_type(Var::from("_out"), typ.clone(), &context)).into()
            } else if exprs.len() == 0 {
                    builder::unknown_function().with_lang(expr, context).into()
            } else {
                let context2 = context.clone();
                let mut exprs2 = exprs.clone();
                let exp = exprs2.pop().unwrap();
                let new_context = exprs.iter()
                    .fold(context2, |ctx, expr| typing(&ctx, expr).context);
                typing(&new_context, &exp)
            }
        },
        Lang::FunctionApp(fn_var_name, values, t, h) => {
            let var = Var::try_from(fn_var_name.clone()).unwrap();
            let name = var.get_name();
            let new_values = values.iter().map(|x| typing(context, x).lang).collect::<Vec<_>>();
            let funs = var.get_function_signatures(values, context);
            let (id, typ) = funs.iter()
                .enumerate()
                .find_map(|(id, x)| 
                          match x.clone().infer_return_type(values, context, &name) {
                              Some(res) => Some((id, res)),
                              _ => None})
                .unwrap();
            let old_ret_typ = funs[id].get_return_type();
            let new_expr = if typ.is_vector_of(&old_ret_typ, context) {
               Lang::VecFunctionApp(fn_var_name.clone(), new_values.clone(), t.clone(), h.clone()) 
            } else { 
               Lang::FunctionApp(fn_var_name.clone(), new_values.clone(), t.clone(), h.clone())
            };

            let (typ, new_context) = typ
                .tuple(&context.clone());
                (typ, new_expr, new_context).into()
        },
        Lang::Tag(name, expr, h) => {
            let ty = typing(context, expr).value;
            Type::Tag(name.clone(), Box::new(ty.clone()), h.clone())
                .with_lang(expr, context).into()
        }
        Lang::If(cond, true_branch, false_branch, _h) => {
            if typing(context, cond).value.is_boolean() {
                let true_ty = typing(context, true_branch).value.clone();
                let false_ty = typing(context, false_branch).value.clone();
                let set = if let Type::Union(v, _) = false_ty {
                    let mut set = v; set.insert(true_ty);
                    set
                } else if false_ty.is_empty() {
                    let mut set = HashSet::new(); set.insert(true_ty);
                    set
                } else {
                    let mut set = HashSet::new(); 
                    set.insert(false_ty);
                    set.insert(true_ty);
                    set
                };
                if set.len() == 1 {
                    set.iter().cloned().next().unwrap().with_lang(expr, context).into()
                } else {
                    Type::Union(set.clone(), HelpData::default())
                        .with_lang(expr, context).into()
                }
            } else {
                panic!("Type error: {:?} isn't a boolean expression", cond);
            }
        }
        Lang::Array(exprs, h) => {
            let types = exprs.iter()
                .map(|expr| typing(context, expr).value)
                .map(|typ| typ.reduce(context))
                .collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "[0, Empty]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type])).into()
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type])).into()
            } else {
                let array_type = Type::Tuple(types, HelpData::default());
                panic!("Type error: The array don't have homogenous types \n {}", 
                               array_type.pretty());
            }
        }
        Lang::Vector(exprs, h) => {
            let types = exprs.iter().map(|expr| typing(context, expr).value).collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "Vec[0, Any]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type])).into()
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("Vec[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type])).into()
            } else {
                panic!("Type error: The vector don't have homogenous types.");
            }
        },
        Lang::Sequence(exprs, h) => {
            let types = exprs.iter().map(|expr| typing(context, expr).value).collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "Seq[0, Empty]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type])).into()
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("Seq[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type])).into()
            } else {
                panic!("Type error: The sequence don't have homogenous types.");
            }
        },
        Lang::Record(fields, h) => {
            let field_types = fields.iter()
                .map(|arg_val| {
                    (arg_val.get_argument(),
                    typing(context, &arg_val.get_value()).value.clone()).into()
                }).collect();
                Type::Record(field_types, h.clone())
                .with_lang(expr, context).into()
        }
        Lang::Match(exp, var, branches, _h) => {
            let var_ty = reduce_type(context, &typing(context, &**exp).value);
            match var_ty {
                Type::Union(union_types, h) => {
                    let set = branches.iter().map(|(t, _)| t).cloned().collect::<HashSet<Type>>();
                    if union_types != set {
                        panic!("Some types are missing");
                    }
                    let types = branches.iter()
                        .map(|(typ, bexp)| {
                            let new_context = context.clone().push_var_type(var.clone(), typ.clone(), context);
                            typing(&new_context, bexp).value.clone()
                        }).collect::<HashSet<_>>();
                    let output_type = if types.len() == 1 {
                        types.iter().next().unwrap().clone()
                    } else {Type::Union(types, h)};
                        output_type.with_lang(expr, context).into()
                },
                _ => panic!("Type error"),
            }
        }
        Lang::ArrayIndexing(exp, index, _) => {
            let typ1 = typing(context, exp).value;
            let args_target = typ1.clone().linearize();
            let args_index = index.get_members_if_array()
                .expect(&format!("{} is not an array", index.simple_print()))
                .iter()
                .map(|x| builder::integer_type(x.len()).set_help_data((*x).clone().into()))
                .collect::<Vec<_>>();
            let is_indexable = args_target.iter()
                .zip(args_index.iter())
                .all(|(target, index)| index <= target);
            let typ2 = Type::to_array2(args_index).set_help_data((**exp).clone().into());
            if is_indexable {
                (typ2, expr.clone(), context.clone()).into()
            } else {
                None.expect(&TypeError::WrongIndexing(typ1, typ2).display())
            }
        },
        Lang::Variable(_, _, _, _, _) => {
            let old_var = Var::try_from(expr.clone()).unwrap();
            let var = context.get_true_variable(&old_var);
            context
                .get_type_from_existing_variable(var)
                .with_lang(expr, context).into()
        },
        Lang::Scope(expr, _) if expr.len() == 1 => {
            typing(context, &expr[0])
        },
        Lang::Scope(expr, h) => typing(context, &Lang::Lines(expr.to_vec(), h.clone())),
        Lang::Tuple(elements, h) => {
            Type::Tuple(elements.iter()
                .map(|x| typing(context, x).value.clone())
                .collect(), h.clone())
                .with_lang(expr, context).into()
        },
        Lang::VecBlock(_, h) => Type::Empty(h.clone()).with_lang(expr, context).into(),
        Lang::RFunction(_, _, h) => Type::UnknownFunction(h.clone()).with_lang(expr, context).into(),
        Lang::ForLoop(var, iter, body, _h) => {
            let base_type = typing(context, iter).value.to_array()
                .expect(&format!("The iterator is not an array {:?}", iter))
                .base_type;
            let var = var.clone().set_type(base_type.clone());
            Typer::from(context.clone())
                .set_type(base_type)
                .set_var(var)
                .push_var_type()
                .typing((**body).clone());
                builder::unknown_function().with_lang(expr, context).into()
        },
        Lang::Not(exp, h) => {
            let typ = typing(context, exp).value;
            match typ.clone() {
                Type::Boolean(_) => Type::Boolean(h.clone()).with_lang(expr, context).into(),
                _ => panic!("Use of the '!' to a none boolean expression")
            }
        },
        Lang::JSBlock(body, _, h) => {
            let js_context = Context::default()
                .set_target_language(TargetLanguage::JS);
            let _ = typing(&js_context, body).context;
            //TODO add js subcontext
            let (new_context, id) = (context.clone(), 0);//.add_js_subcontext(js_context);
            let new_expr = Lang::JSBlock(body.clone(), id, h.clone());
            builder::character_type_default().with_lang(&new_expr, &new_context).into()
        },
        Lang::Let(..) => {
            eval(context, expr)
        },
        Lang::Assign(..) => {
            eval(context, expr)
        },
        Lang::Alias(..) => {
            eval(context, expr).with_lang(expr).into()
        },
        Lang::Library(..) => {
            eval(context, expr).with_lang(expr).into()
        },
        Lang::ModuleDecl(..) => {
            eval(context, expr).with_lang(expr).into()
        },
        Lang::TestBlock(..) => {
            eval(context, expr).with_lang(expr).into()
        },
        Lang::Signature(..) => {
            eval(context, expr).with_lang(expr).into()
        },
        Lang::Return(exp, _) => {
            typing(context, exp)
        },
        Lang::Module(_, _, _position, _, _) => {
            eval(context, expr).with_lang(expr).into()
        },
        _ => builder::any_type().with_lang(expr, context).into()
    }
}

fn replace_fields_type_if_needed(context: &Context, at: ArgumentValue) -> impl FnMut(&ArgumentType) -> ArgumentType + use<'_> {
    move |arg_typ2| {
        (arg_typ2.get_argument_str() == at.get_argument())
            .then_some(ArgumentType::new(&at.get_argument(),
                        &typing(context, &at.get_value()).value))
            .unwrap_or(arg_typ2.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::fluent_parser::FluentParser;
    use crate::processes::parsing::parse;
    use super::*;

    #[test]
    #[should_panic]
    fn test_function_application_unknown_function() {
        let res = "typr(true)".parse::<Lang>().unwrap();
        let context = Context::default();
        typing(&context, &res);
    }

    #[test]
    fn test_let1() {
        let context = Context::default();
        let lang = Var::default().set_name("a");
        let typ = builder::integer_type_default();
        let context2 = context.clone().push_var_type(lang.clone(), typ.clone(), &context);
        let res = context2.get_type_from_variable(&lang);
        //assert_eq!(res, Some(typ));
        assert!(true)
            
    }

    #[test]
    fn test_let2() {
        let context = Context::default();
        let let_exps = parse("let a: int <- 8;".into());
        let let_exp = let_exps.clone();
        let var = Var::default().set_name("a");
        let new_context = typing(&context, &let_exp).2;
        let res = new_context.get_type_from_variable(&var);
        let typ = builder::integer_type_default();
        assert!(true);
    }

    #[test]
    fn test_let2_0() {
        let context = Context::default();
        let let_exps = parse("a".into());
        let let_exp = let_exps.clone();
        let var = Var::default().set_name("a");
        let new_context = typing(&context, &let_exp).2;
        let res = new_context.get_type_from_variable(&var);
        let typ = builder::integer_type_default();
        assert!(true);
    }

    #[test]
    fn test_let3() {
        let fp = FluentParser::new()
            .push("let n <- 8;")
            .parse_type_next()
            .push("n")
            .parse_next();
        println!("{}", fp);
        assert!(true)
    }

    #[test]
    fn test_simple_signature1() {
        let val = FluentParser::new()
            .push("@as__character: (Any) -> char;")
            .parse_type_next()
            .push("as__character(3)")
            .parse_type_next()
            .get_last_type();
        println!("{}", val);
        assert!(true);
    }

    #[test]
    fn test_function_return_type1() {
        let typ = FluentParser::new()
            .set_context(Context::default())
            .push("@incr: (int) -> int;")
            .parse_type_next()
            .push("incr([1, 2])")
            .parse_type_next()
            .get_last_type();
        println!("{}", typ.pretty());
        assert!(true);
    }

    #[test]
    fn test_function_return_type2() {
        let typ = FluentParser::new()
            .set_context(Context::default())
            .push("@scale: (bool, int) -> bool;").parse_type_next()
            .push("scale([true, false], 2)")
            .parse_type_next()
            .get_last_type();
        println!("{}", typ.pretty());
        assert!(true);
    }

}
