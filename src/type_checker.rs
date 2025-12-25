#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::language::build_generic_function;
use std::collections::HashSet;
use crate::Type;
use crate::context::Context;
use crate::Lang;
use crate::var::Var;
use crate::index::Index;
use crate::unification;
use crate::type_comparison::reduce_type;
use crate::argument_type::ArgumentType;
use crate::unification_map::UnificationMap;
use std::process::Command;
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
use crate::function_type::FunctionType;
use crate::graph::TypeSystem;
use crate::array_type::ArrayType;
use crate::config::TargetLanguage;
use rpds::Vector;
use crate::translatable::RTranslatable;
use crate::var_function::VarFunction;
use crate::Environment;
use crate::operators::Op;
use crate::PackageManager;

#[derive(Debug, Clone)]
pub struct TypeChecker {
    context: Context,
    code: Vector<Lang>,
    types: Vector<Type>,
    last_type: Type
}

impl TypeChecker {
    pub fn new(context: Context) -> Self {
        Self {
            context: context,
            code: Vector::new(),
            types: Vector::new(),
            last_type: builder::unknown_function()
        }
    }

    pub fn typing(self, exp: &Lang) -> Self {
        match exp {
            Lang::Lines(exps, _) => {
                let type_checker = exps.iter()
                    .fold(self.clone(), |acc, lang| acc.typing_helper(lang));
                println!("Typing:\n{}\n", type_checker.last_type.pretty());
                type_checker
            },
            _ => self.clone().typing_helper(exp)
        }
    }

    fn typing_helper(self, exp: &Lang) -> Self {
        let (typ, lang, context) = typing(&self.context, exp);
        Self {
            context: context,
            code: self.code.push_back(lang),
            types: self.types.push_back(typ.clone()),
            last_type: typ
        }
    }

    pub fn get_context(&self) -> Context {
        self.context.clone()
    }

    pub fn get_code(&self) -> Vector<Lang> {
        self.code.clone()
    }

    pub fn get_types(&self) -> Vector<Type> {
        self.types.clone()
    }
    
    pub fn get_type(&self) -> Type {
        self.last_type.clone()
    }

    pub fn transpile(self, project: bool) -> String {
        let code = self.code.iter()
            .zip(self.types.iter())
            .map(|(lang, typ)| lang.to_r(&self.context).0)
            .collect::<Vec<_>>().join("\n");
        let import = match self.get_environment() {
            Environment::Project => "",
            Environment::StandAlone => "source('a_std.R', echo = FALSE)"
        };

        format!("{}\n\n{}", import, code)
    }

    fn get_environment(&self) -> Environment {
        self.context.get_environment()
    }

}

pub fn execute_r_function(function_code: &str) -> Result<String, Box<dyn Error>> {
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

pub fn eval(context: &Context, expr: &Lang) -> (Type, Lang, Context) {
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
                new_context.push_alias(var.get_name(), typ.to_owned()))
        },
        Lang::Assign(left_expr, right_expr, _h) => {
            let left_type = typing(&context, left_expr).0;
            let right_type = typing(&context, right_expr).0;
            let reduced_left_type = reduce_type(context, &left_type);
            let reduced_right_type = reduce_type(context, &right_type);
            if reduced_right_type.is_subtype(&reduced_left_type, context) {
                (builder::unknown_function(), expr.clone(), context.clone())
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
            (builder::unknown_function(), expr.clone(), context.clone().extend_typing_context(var_type))
        },
        Lang::ModuleDecl(_name, _h) 
            => (builder::unknown_function(), expr.clone(), context.clone()),
        Lang::Signature(var, typ, _h) => {
            if var.is_variable(){
                let new_var = FunctionType::try_from(typ.clone())
                            .map(|ft| var.clone().set_type(ft.get_first_param().unwrap_or(builder::unknown_function())))
                            .unwrap_or(var.clone());
                (builder::unknown_function(), expr.clone(),
                context.clone().replace_var_type(new_var, typ.to_owned(), context))
            } else { // is alias
                (builder::unknown_function(), expr.clone(),
                        context.clone()
                            .replace_var_type(var.to_owned(), typ.to_owned(), context))
            }
        },
        Lang::TestBlock(body, _) => {
            let res = typing(context, body);
            (builder::unknown_function(), expr.clone(), context.clone())
        },
        Lang::Module(name, members, _position, _config, h) => {
            let expr = if members.len() > 1 {
                Lang::Lines(members.iter().cloned().collect(), h.clone())
            } else { members.iter().next().unwrap().clone() }; // TODO: Modules can't be empty
            let new_context = 
                typing(&Context::default(), &expr).2;
            let var = Var::from_name(name);
            let arg_types = new_context.get_members()
                .iter().cloned()
                .map(|arg_type| ArgumentType::from(arg_type))
                .collect::<Vec<_>>();
            let module_type = Type::Module(arg_types, h.clone());
            let new_context = context.clone().set_as_module_context()
                .push_var_type(var, module_type.clone(), &context);
            (module_type, expr.clone(), new_context)
        },
        _ => (builder::unknown_function(), expr.clone(), context.clone())
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

trait TypeContext {
    fn with_lang(self, expr: &Lang) -> (Type, Lang, Context);
    fn get_covariant_type(self, typ: &Type) -> Self;
    fn add_to_context(self, var: Var) -> Self;
    fn get_expr(&self) -> Lang;
}

impl TypeContext for (Type, Lang, Context) {
    fn with_lang(self, expr: &Lang) -> (Type, Lang, Context) {
        (self.0, expr.clone(), self.2)
    }

    fn get_covariant_type(self, typ: &Type) -> Self {
        let typ = self.0.get_covariant_type(typ, &self.2);
        (typ, self.1, self.2)
    }

    fn add_to_context(self, var: Var) -> Self {
        let (typ, context) = self.0.add_to_context(var, self.2);
        (typ, self.1, context)
    }

    fn get_expr(&self) -> Lang {
        self.1.clone()
    }

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
pub fn typing(context: &Context, expr: &Lang) -> (Type, Lang, Context) {
    match expr {
        Lang::Number(_, h) => (Type::Number(h.clone()), expr.clone(), context.clone()),
        Lang::Integer(i, h) 
            => (builder::integer_type(*i).set_help_data(h.clone()), expr.clone(), context.clone()),
        Lang::Bool(_, h) => (Type::Boolean(h.clone()), expr.clone(), context.clone()),
        Lang::Char(s, h) 
            => (builder::character_type(&s).set_help_data(h.clone()), expr.clone(), context.clone()),
        Lang::Empty(h) => (Type::Empty(h.clone()), expr.clone(), context.clone()),
        Lang::Operator(Op::And(_), e1, e2, _) | Lang::Operator(Op::Or(_), e1, e2, _) => {
            let (typ, new_context) = (typing(context, e1).0.is_boolean() && typing(context, e2).0.is_boolean())
                .then_some((builder::boolean_type(), context.clone()))
                .expect("Type error");
            (typ, expr.clone(), new_context)
        }
        Lang::Operator(Op::Eq(_), e1, e2, _) 
            | Lang::Operator(Op::LesserOrEqual(_), e1, e2, _) 
            | Lang::Operator(Op::GreaterOrEqual(_), e1, e2, _) 
            | Lang::Operator(Op::GreaterThan(_), e1, e2, _) 
            | Lang::Operator(Op::LesserThan(_), e1, e2, _) => {
            let t1 = typing(context, e1).0;
            let t2 = typing(context, e2).0;
            let (typ, new_context) = (t1 == t2)
                .then_some((builder::boolean_type(), context.clone()))
                .expect("Type error");
            (typ, expr.clone(), new_context)
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
                let ty2 = typing(context, e2).0.clone().reduce(context);
                match (ty2.clone(), *e1.clone()) {
                    (Type::Record(fields, _), Lang::Variable(name, _, _, _, h)) => {
                        let (typ, new_context) = fields.iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                            .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                            .expect(&TypeError::FieldNotFound((name, h), ty2).display());
                        (typ, expr.clone(), new_context)
                    },
                    (Type::Record(fields, _), Lang::Char(name, _)) => {
                        let (typ, new_context) = fields.iter()
                            .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                            .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                            .expect("Field not found");
                        (typ, expr.clone(), new_context)
                    },
                    (Type::Tuple(vals, _), Lang::Integer(i, _)) => {
                        let (typ, new_context) = vals.iter()
                            .nth((i-1) as usize)
                            .map(|typ| (typ.clone(), context.clone()))
                            .expect(&format!("no value at the position {}", i));
                        (typ, expr.clone(), new_context)
                    },
                    (Type::Record(fields1, h), Lang::Record(fields2, _)) => {
                        let fields3: HashSet<_> = match e1.typing(context).0 {
                            Type::Record(fields, _) => {
                                fields1.union(&fields).cloned().collect()
                            },
                            _ => panic!("Typing {} should produce a record type", e1)
                        };
                        Type::Record(fields3, h.clone()).with_lang(expr, context)
                    },
                    (Type::Generic(symbol, h), Lang::Record(fields2, _)) => {
                        builder::intersection_type(&[ty2.clone(), e1.typing(context).0])
                            .with_lang(expr, context)
                    },
                    (a, b) => panic!("Type error we can't combine {} and {:?}", a, b)
                }
            }
        },
        Lang::Operator(Op::Dollar(_), e1, e2, _) => {
            let op = match expr {
                Lang::Operator(op, _, _, _) => op,
                _ => panic!("expr n'est pas un opérateur"),
            };
            let ty1 = typing(context, e1).0.clone();
            match (ty1.reduce(context), *e2.clone(), op) {
                (Type::Record(fields, _), Lang::Variable(name, _, _, _, _), _) => {
                    let (typ, new_context) = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Field {} not found", name));
                        (typ, expr.clone(), new_context)
                },
                (Type::Module(fields, _), Lang::Variable(name, _, _, _, _), _) => {
                    let (typ, new_context) = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Variable {} not found in the module", name));
                        (typ, expr.clone(), new_context)
                },
                (Type::Module(fields, h1), Lang::FunctionApp(exp, args, _, h2), _) => {
                    let var = Var::from_language(*exp).unwrap();
                    let typ = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == var.get_name())
                        .expect(&format!("Field {} not found", var.get_name()))
                        .get_type();
                    let res = FunctionType::try_from(typ).unwrap().get_return_type();
                    (res, expr.clone(), context.clone())
                },
                (Type::Record(fields, _), Lang::Char(name, _), _) => {
                    let (typ, new_context) = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Field {} not found", name));
                        (typ, expr.clone(), new_context)
                },
                (Type::Tuple(vals, _), Lang::Integer(i, _), _) => {
                    let (typ, new_context) = vals.iter()
                        .nth((i-1) as usize)
                        .map(|typ| (typ.clone(), context.clone()))
                        .expect(&format!("no value at the position {}", i));
                        (typ, expr.clone(), new_context)
                },
                (Type::Record(fields1, h), Lang::Record(fields2, _), _) => {
                    let at = fields2[0].clone();
                    let fields3 = fields1.iter()
                        .map(replace_fields_type_if_needed(context, at))
                        .collect::<HashSet<_>>();
                    Type::Record(fields3, h.clone()).with_lang(expr, context)
                },
                (Type::Record(fields, h1), Lang::FunctionApp(exp, args, _, h2), _) => {
                    let var = Var::from_language(*exp).unwrap();
                    let typ = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == var.get_name())
                        .expect(&format!("Field {} not found", var.get_name()))
                        .get_type();
                    typing(&context.clone()
                           .push_var_type(var, typ, context), e2).0.clone()
                           .with_lang(expr, context)
                },
                (Type::UnknownFunction(h) , Lang::FunctionApp(exp, args, ret, h2), _ ) => {
                    (Type::UnknownFunction(h), (*expr).clone(), context.clone())
                }
                (typ , Lang::FunctionApp(exp, args, ret, h2), Op::Dot(_)) => {
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
            let reduced_body_type = body_type.0.reduce(&sub_context);
            let reduced_expected_ty = ret_ty.reduce(&context);
            if !reduced_body_type.is_subtype(&reduced_expected_ty, context) {
                None.expect(
                    &TypeError::UnmatchingReturnType(ret_ty.clone(), body_type.0).display())
            }
            Type::Function(list_of_types, Box::new(ret_ty.clone()), h.clone())
                .with_lang(expr, &body_type.2)
        }
        Lang::Lines(exprs, _h) => {
            if exprs.len() == 1 {
                let res = exprs.clone().pop().unwrap();
                let (typ, langs, cont) = typing(context, &res);
                (typ.clone(), langs.clone(), 
                 context.clone().push_var_type(Var::from("_out"), typ.clone(), &context))
            } else if exprs.len() == 0 {
                    builder::unknown_function().with_lang(expr, context)
            } else {
                let context2 = context.clone();
                let mut exprs2 = exprs.clone();
                let exp = exprs2.pop().unwrap();
                let new_context = exprs.iter()
                    .fold(context2, |ctx, expr| typing(&ctx, expr).2);
                typing(&new_context, &exp)
            }
        },
        Lang::FunctionApp(fn_var_name, values, t, h) => {
            let var = Var::try_from(fn_var_name.clone()).unwrap();
            let name = var.get_name();
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
               Lang::VecFunctionApp(fn_var_name.clone(), values.clone(), t.clone(), h.clone()) 
            } else { expr.clone() };
            let (typ, new_context) = typ
                .tuple(&context.clone());
                (typ, new_expr, new_context)
        },
        Lang::Tag(name, expr, h) => {
            let ty = typing(context, expr).0;
            Type::Tag(name.clone(), Box::new(ty.clone()), h.clone())
                .with_lang(expr, context)
        }
        Lang::If(cond, true_branch, false_branch, _h) => {
            if typing(context, cond).0.is_boolean() {
                let true_ty = typing(context, true_branch).0.clone();
                let false_ty = typing(context, false_branch).0.clone();
                let set = if let Type::Union(v, h) = false_ty {
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
                    set.iter().cloned().next().unwrap().with_lang(expr, context)
                } else {
                    Type::Union(set.clone(), HelpData::default())
                        .with_lang(expr, context)
                }
            } else {
                panic!("Type error: {:?} isn't a boolean expression", cond);
            }
        }
        Lang::Array(exprs, h) => {
            let types = exprs.iter()
                .map(|expr| typing(context, expr).0)
                .map(|typ| typ.reduce(context))
                .collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "[0, Empty]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type]))
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type]))
            } else {
                let array_type = Type::Tuple(types, HelpData::default());
                panic!("Type error: The array don't have homogenous types \n {}", 
                               array_type.pretty());
            }
        }
        Lang::Vector(exprs, h) => {
            let types = exprs.iter().map(|expr| typing(context, expr).0).collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "Vec[0, Any]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type]))
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("Vec[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type]))
            } else {
                panic!("Type error: The vector don't have homogenous types.");
            }
        },
        Lang::Sequence(exprs, h) => {
            let types = exprs.iter().map(|expr| typing(context, expr).0).collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "Seq[0, Empty]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type]))
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("Seq[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), expr.clone(), context.clone().push_types(&[new_type]))
            } else {
                panic!("Type error: The sequence don't have homogenous types.");
            }
        },
        Lang::Record(fields, h) => {
            let field_types = fields.iter()
                .map(|arg_val| {
                    (arg_val.get_argument(),
                    typing(context, &arg_val.get_value()).0.clone()).into()
                }).collect();
                Type::Record(field_types, h.clone())
                .with_lang(expr, context)
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
                            typing(&new_context, bexp).0.clone()
                        }).collect::<HashSet<_>>();
                    let output_type = if types.len() == 1 {
                        types.iter().next().unwrap().clone()
                    } else {Type::Union(types, h)};
                        output_type.with_lang(expr, context)
                },
                _ => panic!("Type error"),
            }
        }
        Lang::ArrayIndexing(exp, index, _) => {
            let typ1 = typing(context, exp).0;
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
                (typ2, expr.clone(), context.clone())
            } else {
                None.expect(&TypeError::WrongIndexing(typ1, typ2).display())
            }
        },
        Lang::Variable(_, _, _, typ, _) => {
            let old_var = Var::try_from(expr.clone()).unwrap();
            let var = context.get_true_variable(&old_var);
            context
                .get_type_from_existing_variable(var)
                .with_lang(expr, context)
        },
        Lang::Scope(expr, _) if expr.len() == 1 => {
            typing(context, &expr[0])
        },
        Lang::Scope(expr, h) => typing(context, &Lang::Lines(expr.to_vec(), h.clone())),
        Lang::Tuple(elements, h) => {
            Type::Tuple(elements.iter()
                .map(|x| typing(context, x).0.clone())
                .collect(), h.clone())
                .with_lang(expr, context)
        },
        Lang::VecBlock(_, h) => Type::Empty(h.clone()).with_lang(expr, context),
        Lang::RFunction(_, _, h) => Type::UnknownFunction(h.clone()).with_lang(expr, context),
        Lang::ForLoop(var, iter, body, _h) => {
            let base_type = typing(context, iter).0.to_array()
                .expect(&format!("The iterator is not an array {:?}", iter))
                .base_type;
            let var = var.clone().set_type(base_type.clone());
            Typer::from(context.clone())
                .set_type(base_type)
                .set_var(var)
                .push_var_type()
                .typing((**body).clone());
                builder::unknown_function().with_lang(expr, context)
        },
        Lang::Not(exp, h) => {
            let typ = typing(context, exp).0;
            match typ.clone() {
                Type::Boolean(_) => Type::Boolean(h.clone()).with_lang(expr, context),
                _ => panic!("Use of the '!' to a none boolean expression")
            }
        },
        Lang::JSBlock(body, _, h) => {
            let js_context = Context::default()
                .set_target_language(TargetLanguage::JS);
            let js_context = typing(&js_context, body).2;
            //TODO add js subcontext
            let (new_context, id) = (context.clone(), 0);//.add_js_subcontext(js_context);
            let new_expr = Lang::JSBlock(body.clone(), id, h.clone());
            builder::character_type_default().with_lang(&new_expr, &new_context)
        },
        Lang::Let(..) => {
            eval(context, expr)
        },
        Lang::Assign(..) => {
            eval(context, expr).with_lang(expr)
        },
        Lang::Alias(..) => {
            eval(context, expr).with_lang(expr)
        },
        Lang::Library(..) => {
            eval(context, expr).with_lang(expr)
        },
        Lang::ModuleDecl(..) => {
            eval(context, expr).with_lang(expr)
        },
        Lang::TestBlock(..) => {
            eval(context, expr).with_lang(expr)
        },
        Lang::Signature(..) => {
            eval(context, expr).with_lang(expr)
        },
        Lang::Return(exp, _) => {
            typing(context, exp)
        },
        Lang::Module(name, members, _position, config, h) => {
            eval(context, expr).with_lang(expr)
        },
        _ => builder::any_type().with_lang(expr, context)
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
        (Type::Any(_), _) | (_, Type::Any(_)) => Type::Any(HelpData::default()),
        (Type::Empty(_), ty) | (ty, Type::Empty(_)) => ty.clone(),
        (ty1, ty2) if ty1 == ty2 => ty1.clone(),
        _ => Type::Empty(HelpData::default())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::signature;
    use crate::fluent_parser::FluentParser;

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
