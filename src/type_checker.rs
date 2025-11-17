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
use crate::function_type::FunctionType;
use crate::graph::TypeSystem;
use crate::array_type::ArrayType;
use crate::config::TargetLanguage;
use rpds::Vector;
use crate::translatable::RTranslatable;
use crate::var_function::VarFunction;

#[derive(Debug)]
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
            last_type: builder::empty_type()
        }
    }

    pub fn typing(self, exp: &Lang) -> Self {
        match exp {
            Lang::Lines(exps, _) => {
                exps.iter()
                    .fold(self, |acc, lang| acc.typing_helper(lang))
            },
            _ => self.typing_helper(exp)
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

    pub fn transpile(self, project: bool, functions: &VarFunction) -> String {
        let code = self.code.iter()
            .zip(self.types.iter())
            .map(|(lang, typ)| lang.to_r(typ.clone(), &self.context).0)
            .collect::<Vec<_>>().join("\n");
        let type_converters = self.context.get_type_definition(functions);
        let headers = self.context.get_adt().to_r(&self.context);
        let import = if project {
            "source('R/std.R', echo = FALSE)"
        } else {
            "source('std.R', echo = FALSE)"
        };

        format!("{}\n\n#Existing types\n{}\n\n{}{}", import, type_converters, headers, code)
    }

}

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
            .fold(context2, |ctx, expr| eval(&ctx, expr).1)
}

pub fn eval(context: &Context, expr: &Lang) -> (Type, Context){
    match expr {
        Lang::Let(name, ty, exp, _h) => {
            let new_context = context.clone()
                .push_types(&exp.extract_types_from_expression(context));

            exp.typing(&new_context)
                .get_covariant_type(ty)
                .add_to_context(name.clone())
        },
        Lang::Alias(name, params, typ, h) => {
            let var = name.clone()
                .set_type(Type::Params(params.to_vec(), h.clone()));
            let alias_context = context.clone()
                .push_alias(name.get_name(), typ.to_owned());
            let new_context = context.clone().push_var_type(var, typ.clone(), &alias_context);
            (builder::empty_type(),
                new_context.push_alias(name.get_name(), typ.to_owned()))
        },
        Lang::Assign(left_expr, right_expr, _h) => {
            let left_type = typing(&context, left_expr).0;
            let right_type = typing(&context, right_expr).0;
            let reduced_left_type = reduce_type(context, &left_type);
            let reduced_right_type = reduce_type(context, &right_type);
            if reduced_right_type.is_subtype(&reduced_left_type, context) {
                (builder::empty_type(), context.clone())
            } else { 
                panic!("The right side and left sides don't match {} <- {}",
                       left_type.pretty(), right_type.pretty())
            }
        },
        Lang::Library(name, _h) => {
            install_package(name);
            let function_list = execute_r_function(&format!("library({})\n\nls('package:{}')", name, name))
                .expect("The R command didn't work");
            let new_context = context.append_function_list(&function_list);
            (builder::empty_type(), install_header(name, &new_context))
        },
        Lang::ModuleDecl(_name, _h) 
            => (builder::empty_type(), context.clone().add_module_declarations(&[expr.clone()])),
        Lang::Signature(var, typ, _h) => {
            if var.is_variable(){
                let new_var = FunctionType::try_from(typ.clone())
                            .map(|ft| var.clone().set_type(ft.get_first_param().unwrap_or(builder::empty_type())))
                            .unwrap_or(var.clone());
                (builder::empty_type(),
                context.clone().push_var_type(new_var, typ.to_owned(), context))
            } else { // is alias
                (builder::empty_type(),
                        context.clone()
                            .push_var_type(var.to_owned(), typ.to_owned(), context))
            }
        },
        Lang::TestBlock(body, _) => {
            let res = typing(context, body);
            builder::empty_type().tuple(context)
        },
        _ => builder::empty_type().tuple(context)
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

fn are_homogenous_types(types: &[Type]) -> bool {
    types.windows(2).all(|w| w[0] == w[1])
}

trait TypeContext {
    fn with_lang(self, expr: &Lang) -> (Type, Lang, Context);
    fn get_covariant_type(self, typ: &Type) -> Self;
    fn add_to_context(self, var: Var) -> Self;
}

impl TypeContext for (Type, Context) {
    fn with_lang(self, expr: &Lang) -> (Type, Lang, Context) {
        (self.0, expr.clone(), self.1)
    }

    fn get_covariant_type(self, typ: &Type) -> Self {
        let typ = self.0.get_covariant_type(typ, &self.1);
        (typ, self.1)
    }

    fn add_to_context(self, var: Var) -> Self {
        self.0.add_to_context(var, self.1)
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
        Lang::Number(_, h) => (Type::Number(h.clone()), context.clone()).with_lang(expr),
        Lang::Integer(i, h) => Type::Integer((*i).into(), h.clone()).with_lang(expr, context),
        Lang::Bool(_, h) => Type::Boolean(h.clone()).with_lang(expr, context),
        Lang::Char(s, h) => Type::Char(s.to_owned().into(), h.clone()).with_lang(expr, context),
        Lang::Empty(h) => Type::Empty(h.clone()).with_lang(expr, context),
        Lang::And(e1, e2, _) | Lang::Or(e1, e2, _) => {
            (typing(context, e1).0.is_boolean() && typing(context, e2).0.is_boolean())
                .then_some((builder::boolean_type(), context.clone()))
                .expect("Type error").with_lang(expr)
        }
        Lang::Eq(e1, e2, _) | Lang::LesserOrEqual(e1, e2, _) | Lang::GreaterOrEqual(e1, e2, _) | Lang::GreaterThan(e1, e2, _) | Lang::LesserThan(e1, e2, _) => {
            (typing(context, e1).0 == typing(context, e2).0)
                .then_some((builder::boolean_type(), context.clone()))
                .expect("Type error").with_lang(expr)
        }
        Lang::Chain(e1, e2, _) => {
            let ty2 = typing(context, e2).0.clone();
            match (ty2.reduce(context), *e1.clone()) {
                (Type::Record(fields, _), Lang::Variable(name, _, _, _, _, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Field {} not found", name))
                        .with_lang(expr)
                },
                (Type::Record(fields, _), Lang::Char(name, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect("Field not found").with_lang(expr)
                },
                (Type::Tuple(vals, _), Lang::Integer(i, _)) => {
                    vals.iter()
                        .nth((i-1) as usize)
                        .map(|typ| (typ.clone(), context.clone()))
                        .expect(&format!("no value at the position {}", i))
                        .with_lang(expr)
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
        },
        Lang::Dollar(e1, e2, _) => {
            let ty2 = typing(context, e2).0.clone();
            match (ty2.reduce(context), *e1.clone()) {
                (Type::Record(fields, _), Lang::Variable(name, _, _, _, _, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Field {} not found", name))
                        .with_lang(expr)
                },
                (Type::Record(fields, _), Lang::Char(name, _)) => {
                    fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == name)
                        .map(|arg_typ| (arg_typ.1.clone(), context.clone()))
                        .expect(&format!("Field {} not found", name))
                        .with_lang(expr)
                },
                (Type::Tuple(vals, _), Lang::Integer(i, _)) => {
                    vals.iter()
                        .nth((i-1) as usize)
                        .map(|typ| (typ.clone(), context.clone()))
                        .expect(&format!("no value at the position {}", i))
                        .with_lang(expr)
                },
                (Type::Record(fields1, h), Lang::Record(fields2, _)) => {
                    let at = fields2[0].clone();
                    let fields3 = fields1.iter()
                        .map(replace_fields_type_if_needed(context, at))
                        .collect::<HashSet<_>>();
                    Type::Record(fields3, h.clone()).with_lang(expr, context)
                },
                (Type::Record(fields, h1), Lang::FunctionApp(exp, args, _, h2)) => {
                    let var = Var::from_language(*exp).unwrap();
                    let typ = fields.iter()
                        .find(|arg_typ2| arg_typ2.get_argument_str() == var.get_name())
                        .expect(&format!("Field {} not found", var.get_name()))
                        .get_type();
                    typing(&context.clone()
                           .push_var_type(var, typ, context), e1).0.clone()
                           .with_lang(expr, context)
                },
                (a, b) => panic!("Type error we can't combine {} and {:?}", a, b)
            }
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
                    &TypeError::UnmatchingReturnType(reduced_expected_ty, reduced_body_type).display())
            }
            Type::Function(list_of_types, Box::new(ret_ty.clone()), h.clone())
                .with_lang(expr, &body_type.1)
        }
        Lang::Lines(exprs, _h) => {
            if exprs.len() == 1 {
                let res = exprs.clone().pop().unwrap();
                let (typ, langs, cont) = typing(context, &res);
                (typ.clone(), langs.clone(), 
                 context.clone().push_var_type(Var::from("_out"), typ.clone(), &context))
            } else if exprs.len() == 0 {
                    builder::empty_type().with_lang(expr, context)
            } else {
                let context2 = context.clone();
                let mut exprs2 = exprs.clone();
                let exp = exprs2.pop().unwrap();
                let new_context = exprs.iter()
                    .fold(context2, |ctx, expr| typing(&ctx, expr).2);
                typing(&new_context, &exp)
            }
        },
        Lang::FunctionApp(fn_var_name, values, _, h) => {
            let var = Var::try_from(fn_var_name.clone()).unwrap();
            let func = var.get_function_signature(values, context)
                        .infer_return_type(values, context);
            func.get_return_type().clone()
                .tuple(&context.clone().push(expr.clone(), func))
                .with_lang(expr)
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
                .collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "[0, Empty]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), context.clone().push_types(&[new_type]))
                    .with_lang(expr)
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), context.clone().push_types(&[new_type]))
                    .with_lang(expr)
            } else {
                panic!("Type error: The array don't have homogenous types.");
            }
        }
        Lang::Vector(exprs, h) => {
            let types = exprs.iter().map(|expr| typing(context, expr).0).collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "Vec[0, Any]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), context.clone().push_types(&[new_type]))
                    .with_lang(expr)
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("Vec[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), context.clone().push_types(&[new_type]))
                    .with_lang(expr)
            } else {
                panic!("Type error: The vector don't have homogenous types.");
            }
        },
        Lang::Sequence(exprs, h) => {
            let types = exprs.iter().map(|expr| typing(context, expr).0).collect::<Vec<_>>();
            if exprs.is_empty() {
                let new_type = "Seq[0, Empty]".parse::<Type>()
                    .unwrap().set_help_data(h.clone());
                (new_type.clone(), context.clone().push_types(&[new_type]))
                    .with_lang(expr)
            } else if are_homogenous_types(&types.iter().map(|x| x.clone()).collect::<Vec<_>>()) {
                let new_type = format!("Seq[{}, {}]", exprs.len(), types[0].clone().pretty())
                    .parse::<Type>().unwrap().set_help_data(h.clone());
                (new_type.clone(), context.clone().push_types(&[new_type]))
                    .with_lang(expr)
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
        Lang::ArrayIndexing(expr, index, _) => {
            let ty = typing(context, expr).0.reduce(context);
            let index_type = typing(context, index).0.reduce(context);
            match ty.clone() {
                Type::Array(len, elem_ty, _) => {
                    ArrayType::try_from(ty).unwrap()
                      .respect_the_bound(&index_type)
                      .then_some((*elem_ty, context.clone()))
                      .expect("Index out of bounds")
                      .with_lang(expr)
                },
                Type::Sequence(len, elem_ty, _) => {
                    ArrayType::try_from(ty).unwrap()
                      .respect_the_bound(&index_type)
                      .then_some((*elem_ty, context.clone()))
                      .expect("Index out of bounds")
                      .with_lang(expr)
                },
                Type::Vector(len, elem_ty, _) => {
                    ArrayType::try_from(ty).unwrap()
                      .respect_the_bound(&index_type)
                      .then_some((*elem_ty, context.clone()))
                      .expect("Index out of bounds")
                      .with_lang(expr)
                },
                Type::Any(h) => {
                    builder::empty_type().set_help_data(h).with_lang(expr, context)
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
                    context
                        .get_type_from_existing_variable(var)
                        .with_lang(expr, context)
            }
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
        Lang::RFunction(_, _, h) => Type::RFunction(h.clone()).with_lang(expr, context),
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
                builder::empty_type().with_lang(expr, context)
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
            let (new_context, id) = context.clone().add_js_subcontext(js_context);
            let new_expr = Lang::JSBlock(body.clone(), id, h.clone());
            builder::character_type_default().with_lang(&new_expr, &new_context)
        },
        Lang::Let(..) => {
            eval(context, expr).with_lang(expr)
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


#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::signature;

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
        assert_eq!(res, Some(typ));
    }

    #[test]
    fn test_let2() {
        let context = Context::default();
        let let_exps = parse("let a: int <- 8;".into()).unwrap().1.0;
        let let_exp = let_exps[0].clone();
        let var = Var::default().set_name("a");
        let new_context = typing(&context, &let_exp).2;
        let res = new_context.get_type_from_variable(&var);
        let typ = builder::integer_type_default();
        assert_eq!(res, Some(typ));
        //assert!(true);
    }

    #[test]
    fn test_interface() {
        //créer l'interface Animal
        //créer une fonction cri pour les booléens
        assert!(true);
    }

}
