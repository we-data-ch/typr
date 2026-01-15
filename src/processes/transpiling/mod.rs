pub mod translatable;

use crate::components::language::set_related_type_if_variable;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::processes::transpiling::translatable::Translatable;
use crate::components::r#type::function_type::FunctionType;
use crate::processes::type_checking::typing;
use crate::components::error_message::help_data::HelpData;
use crate::components::language::format_backtick;
use crate::components::language::ModulePosition;
use crate::components::language::function_lang::Function;
use crate::components::r#type::array_type::ArrayType;
use crate::components::context::config::Environment;
use crate::components::context::Context;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use crate::components::language::operators::Op;
use crate::components::language::var::Var;
use translatable::RTranslatable;
use std::path::PathBuf;
use std::io::Write;
use std::fs::File;
use std::fs;

pub trait ToSome {
    fn to_some(self) -> Option<Self> where Self: Sized;
}

impl<T: Sized> ToSome for T {
    fn to_some(self) -> Option<Self> {
        Some(self)
    }
}

trait AndIf {
    fn and_if<F>(self, condition: F) -> Option<Self>
    where
        F: Fn(Self) -> bool,
        Self: Sized;
}

impl<T: Clone> AndIf for T {
    fn and_if<F>(self, condition: F) -> Option<Self>
    where
        F: Fn(Self) -> bool,
    {
        if condition(self.clone()) {
            Some(self)
        } else {
            None
        }
    }
}

const JS_HEADER: &str = "";

fn condition_to_if(var: &Var, typ: &Type, context: &Context) -> String {
    format!("any(class({}) == c({}))", var.get_name(), context.get_class(typ))
}

fn to_if_statement(var: Var, exp: Lang, branches: &[(Type, Box<Lang>)], context: &Context) -> String {
    let res = branches.iter()
        .map(|(typ, body)| { (condition_to_if(&var, typ, context), body)})
        .enumerate()
        .map(|(id, (cond, body))| if id == 0 {
            format!("if ({}) {{ \n {} \n }}", cond, body.to_r(context).0)
        } else {
            format!("else if ({}) {{ \n {} \n }}", cond, body.to_r(context).0)
        }).collect::<Vec<_>>().join(" ");
    format!("{{\n {} <- {} \n {}\n}}", var.get_name(), exp.to_r(context).0, res)
}

impl RTranslatable<(String, Context)> for Lang {
    fn to_r(&self, cont: &Context) -> (String, Context) {
        let result = match self {
            Lang::Bool(b, _) => {
                let (typ, _, _) = typing(cont, self);
                let anotation = cont.get_type_anotation(&typ);
                (format!("{} |> {}", b.to_string().to_uppercase(), anotation), cont.clone())
            },
            Lang::Number(n, _) => {
                let (typ, _, _) = typing(cont, self);
                let anotation = cont.get_type_anotation(&typ);
                (format!("{} |> {}", n, anotation), cont.clone())
            },
            Lang::Integer(i, _) => {
                let (typ, _, _) = typing(cont, self);
                let anotation = cont.get_type_anotation(&typ);
                (format!("{}L |> {}", i, anotation), cont.clone())
            },
            Lang::Char(s, _) => {
                let (typ, _, _) = typing(cont, self);
                let anotation = cont.get_type_anotation(&typ);
                (format!("'{}' |> {}", s, anotation), cont.clone())
            },
            Lang::Operator(Op::Dot(_), e1, e2, _) | Lang::Operator(Op::Pipe(_), e1, e2, _) 
                => {
                let e1 = (**e1).clone();
                let e2 = (**e2).clone();
                match e2.clone() {
                    Lang::Variable(_, _, _, _, _) => {
                        Translatable::from(cont.clone())
                            .to_r(&e2)
                            .add("[['").to_r(&e1).add("']]").into()
                    },
                    Lang::Record(fields, _) => {
                        let at = fields[0].clone();
                        Translatable::from(cont.clone())
                            .add("within(").to_r(&e2)
                            .add(", { ").add(&at.get_argument())
                            .add(" <- ")
                            .to_r(&at.get_value()).add(" })")
                            .into()
                    }
                    Lang::FunctionApp(var, v, typ, h) => {
                        let v = [e1].iter().chain(v.iter()).cloned().collect();
                        Lang::FunctionApp(var, v, typ, h).to_r(cont)
                    }
                    _ => {
                        Translatable::from(cont.clone())
                            .to_r(&e2).add("[[")
                            .add("]]").to_r(&e1)
                            .into()
                    }
                }
            },
            Lang::Operator(Op::Dollar(_), e1, e2, _) => {
                let e1 = (**e1).clone();
                let e2 = (**e2).clone();
                let t1 = typing(cont, &e1).0;
                let val = match (t1.clone(), e2.clone()) {
                    (Type::Array(_, _, _), Lang::Variable(name, _, _, _, _))
                        => format!("vec_apply(get, {}, typed_vec('{}'))", e1.to_r(cont).0, name),
                    (_, Lang::Variable(name, _, _, _, _))
                        => format!("{}${}", e1.to_r(cont).0, name),
                    _ => format!("{}${}", e1.to_r(cont).0, e2.to_r(cont).0),
                    //_ => panic!("Dollar operation not yet implemented for {:?}", e2)
                };
                (val, cont.clone())
            },
            Lang::Operator(op, e1, e2, _) => {
                let op_str = format!(" {} ", op.to_string());
                Translatable::from(cont.clone())
                    .to_r(e1).add(&op_str).to_r(e2).into()
            },
            Lang::Scope(exps, _) => {
                Translatable::from(cont.clone())
                    .add("{\n")
                    .join(exps, "\n")
                    .add("\n}").into()
            },
            Lang::Function(args, _, body, _) => {
                let fn_type = FunctionType::try_from(typing(cont, self).0.clone()).unwrap();
                let output_conversion = cont.get_type_anotation(&fn_type.get_return_type());
                let res = (output_conversion == "")
                    .then_some("".to_string())
                    .unwrap_or(" |> ".to_owned() + &output_conversion);
                (format!("(function({}) {}{}) |> {}", 
                        args.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", "),
                        body.to_r(cont).0, 
                        res,
                        cont.get_type_anotation(&fn_type.into())),
                cont.clone())
            },
            Lang::Variable(_, _, _, _, _) => {
                //Here we only keep the variable name, the path and the type
                let var = Var::from_language(self.clone()).unwrap();
                let name = if var.contains("__") {
                    var.replace("__", ".").get_name()
                } else {
                    var.display_type(cont).get_name()
                };
                ((&name).to_string(), cont.clone())
            },
            Lang::FunctionApp(exp, vals, _, _) => {
                let var = Var::try_from(exp.clone()).unwrap();
                let name = var.get_name();
                let str_vals = vals.iter()
                                .map(|x| x.to_r(cont).0)
                                .collect::<Vec<_>>().join(", ");
                if cont.is_an_untyped_function(&name) {
                    let name = name.replace("__", ".");
                    let new_name = if &name[0..1] == "%" {
                        format!("`{}`", name)
                    } else { name.to_string() };
                    let s = format!("{}({})", new_name, str_vals); 
                    (s, cont.clone())
                } else {
                    let (exp_str, cont1) = exp.to_r(cont);
                    let fn_t =  FunctionType::try_from(cont1.get_type_from_variable(&var)
                                    .expect(&format!("variable {} don't have a related type", var)))
                                    .unwrap();
                    let new_args = fn_t.get_param_types().into_iter()
                            .map(|arg| reduce_type(&cont1, &arg))
                            .collect::<Vec<_>>();
                    let new_vals = vals.into_iter().zip(new_args.iter())
                        .map(set_related_type_if_variable)
                        .collect::<Vec<_>>();
                    let (args, current_cont) = Translatable::from(cont1)
                            .join(&new_vals, ", ").into();
                    Var::from_language(*exp.clone())
                        .map(|var| {
                            let name = var.get_name();
                            let new_name = if &name[0..1] == "%" {
                                format!("`{}`", name.replace("__", "."))
                            } else { name.replace("__", ".") };
                            (format!("{}({})", new_name, args), current_cont.clone())
                        }).unwrap_or((format!("{}({})", exp_str, args), current_cont))
                }
            },
            Lang::VecFunctionApp(exp, vals, _, _) => {
                let var = Var::try_from(exp.clone()).unwrap();
                let name = var.get_name();
                let str_vals = vals.iter()
                                .map(|x| x.to_r(cont).0)
                                .collect::<Vec<_>>().join(", ");
                if name == "reduce" {
                    (format!("vec_reduce({})", str_vals), cont.clone())
                } else if cont.is_an_untyped_function(&name) {
                    let name = name.replace("__", ".");
                    let new_name = if &name[0..1] == "%" {
                        format!("`{}`", name)
                    } else { name.to_string() };
                    let s = format!("{}({})", new_name, str_vals); 
                    (s, cont.clone())
                } else {
                    let (exp_str, cont1) = exp.to_r(cont);
                    let fn_t =  FunctionType::try_from(cont1.get_type_from_variable(&var)
                                    .expect(&format!("variable {} don't have a related type", var)))
                                    .unwrap();
                    let new_args = fn_t.get_param_types().into_iter()
                            .map(|arg| reduce_type(&cont1, &arg))
                            .collect::<Vec<_>>();
                    let new_vals = vals.into_iter().zip(new_args.iter())
                        .map(set_related_type_if_variable)
                        .collect::<Vec<_>>();
                    let (args, current_cont) = Translatable::from(cont1)
                            .join(&new_vals, ", ").into();
                    Var::from_language(*exp.clone())
                        .map(|var| {
                            let name = var.get_name();
                            let new_name = if &name[0..1] == "%" {
                                format!("`{}`", name.replace("__", "."))
                            } else { name.replace("__", ".") };
                            (format!("vec_apply({}, {})", new_name, args), current_cont.clone())
                        }).unwrap_or((format!("vec_apply({}, {})", exp_str, args), current_cont))
                }
            },
            Lang::ArrayIndexing(exp, val, _) => {
                let (exp_str, _) = exp.to_r(cont);
                let (val_str, _) = val.to_simple_r(cont);
                let (typ, _, _) = typing(&cont, exp);
                let res = match typ {
                    Type::Array(_, _, _) | Type::Vector(_, _, _) | Type::Sequence(_, _, _)
                        => format!("{}[[{}]]", exp_str, val_str), 
                    _ => "".to_string()
                };
                (res, cont.clone())
            },
            Lang::GenFunc(func, _, _) => 
                (format!("function(x, ...) UseMethod('{}')", func.to_string()), cont.clone()),
            Lang::Let(expr, ttype, body, _) => {
                let (body_str, new_cont) = body.to_r(cont);
                let new_name = format_backtick(expr.clone().to_r(cont).0);

                let (r_code, _new_name2) =
                Function::try_from((**body).clone())
                    .map(|_| {
                        //let related_type = Var::try_from(expr).unwrap().get_type();
                        let related_type = typing(cont, expr).0;
                        let method = match cont.get_environment() {
                            Environment::Project => 
                                format!("#' @method {}\n", new_name.replace(".", " ").replace("`", "")),
                            _ => "".to_string()
                        };
                        match related_type {
                            Type::Empty(_) 
                                => (format!("{} <- {}", new_name, body_str), new_name.clone()),
                            Type::Any(_) | Type::Generic(_, _) 
                                => (format!("{}.default <- {}", new_name, body_str), new_name.clone()),
                            _ => {
                                (format!("{}{} <- {}", method, new_name, body_str), new_name.clone())
                            }
                        }
                    }).unwrap_or((format!("{} <- {}", new_name, body_str), new_name));
                let code = if !ttype.is_empty() {
                    let anotation = new_cont.get_type_anotation(ttype);
                    if anotation == "Generic()" || anotation == "" {
                        format!("{}\n", r_code)
                    } else {
                        format!("{} |> \n\t{} #let type\n", r_code, anotation)
                    }
                } else {
                    r_code + "\n"
                };
                (code, new_cont)
            },
            Lang::Array(_v, _h) => {
                let typ = self.typing(cont).0;

                let _dimension = ArrayType::try_from(typ.clone()).unwrap().get_shape()
                    .map(|sha| format!("c({})", sha))
                    .unwrap_or(format!("c(0)"));

                let array = &self.linearize_array()
                    .iter().map(|lang| lang.to_r(&cont).0)
                    .collect::<Vec<_>>().join(", ")
                    .and_if(|lin_array| lin_array != "")
                    //.map(|lin_array| format!("concat({}, dim = {})", lin_array, dimension))
                    .map(|lin_array| format!("typed_vec({})", lin_array))
                    .unwrap_or("logical(0)".to_string());

                (format!("{} |> {}", array, cont.get_type_anotation(&typ)) ,cont.to_owned())
            },
            Lang::Record(args, _) => {
                let (body, current_cont) = 
                Translatable::from(cont.clone())
                    .join_arg_val(args, ",\n ").into();
               let (typ, _, _) = typing(cont, self);
               let anotation = cont.get_type_anotation(&typ);
                cont.get_classes(&typ)
                    .map(|_| format!("list({}) |> {}", 
                                body, anotation))
                    .unwrap_or(format!("list({}) |> {}",
                                body, anotation))
                    .to_some().map(|s| (s, current_cont)).unwrap()
            },
            Lang::If(cond, exp, els, _) if els == &Box::new(Lang::Empty(HelpData::default())) => {
                Translatable::from(cont.clone())
                    .add("if(").to_r(cond).add(") {\n")
                    .to_r(exp).add(" \n}").into()
            },
            Lang::If(cond, exp, els, _) => {
                Translatable::from(cont.clone())
                    .add("if(").to_r(cond).add(") {\n")
                    .to_r(exp).add(" \n} else ")
                    .to_r(els).into()
            },
            Lang::Tuple(vals, _) => {
                Translatable::from(cont.clone())
                    .add("struct(list(")
                    .join(vals, ", ")
                    .add("), 'Tuple')").into()
            },
            Lang::Assign(var, exp, _) => {
                Translatable::from(cont.clone())
                    .to_r(var).add(" <- ").to_r(exp).into()
            },
            Lang::Comment(txt, _) => 
                ("#".to_string() + txt, cont.clone()),
            Lang::Tag(s, t, _) => {
                let (t_str, new_cont) = t.to_r(cont);
                let (typ, _, _) = typing(cont, self);
                let class = cont.get_class(&typ);
                cont.get_classes(&typ)
                    .map(|res| format!("struct(list('{}', {}), c('Tag', {}, {}))",
                                s, t_str, class, res))
                    .unwrap_or(format!("struct(list('{}', {}), c('Tag', {}))",
                                s, t_str, class))
                    .to_some().map(|s| (s, new_cont)).unwrap()
            },
            Lang::Empty(_) => 
                ("NA".to_string(), cont.clone()),
            Lang::ModuleDecl(name, _) => 
                (format!("{} <- new.env()", name), cont.clone()),
            Lang::Lines(exps, _) => {
                Translatable::from(cont.clone())
                    .join(exps, "\n").into()
            },
            Lang::Return(exp, _) => {
                Translatable::from(cont.clone())
                    .add("return ").to_r(exp).into()
            },
            Lang::Lambda(bloc, _) 
                =>  {
                    (format!("function(x) {{ {} }}", bloc.to_r(cont).0), cont.clone())
                },
            Lang::VecBlock(bloc, _) => (bloc.to_string(), cont.clone()),
            Lang::Library(name, _) => (format!("library({})", name), cont.clone()),
            Lang::Match(exp, var, branches, _) 
                => (to_if_statement(var.clone(), (**exp).clone(), branches, cont), cont.clone()),
            Lang::Exp(exp, _) => (exp.clone(), cont.clone()),
            Lang::ForLoop(var, iterator, body, _) => {
                Translatable::from(cont.clone())
                    .add("for (").to_r_safe(var)
                    .add(" in ").to_r_safe(iterator).add(") {\n")
                    .to_r_safe(body).add("\n}").into()
            },
            Lang::RFunction(vars, body, _) => {
                Translatable::from(cont.clone())
                    .add("function (").join(vars, ", ")
                    .add(") \n").add(&body).add("\n")
                    .into()
            }
            Lang::Signature(_, _, _) => {
                ("".to_string(), cont.clone())
            }
            Lang::Alias(_, _, _, _) => ("".to_string(), cont.clone()),
            Lang::KeyValue(k, v, _) => {
                (format!("{} = {}", k, v.to_r(cont).0), cont.clone())
            },
            Lang::Vector(vals, _) => {
               let res = "c(".to_string() + 
                   &vals.iter().map(|x| x.to_r(cont).0)
                   .collect::<Vec<_>>().join(", ")
                + ")";
               (res, cont.to_owned())
            },
            Lang::Not(exp, _) => {
                (format!("!{}", exp.to_r(cont).0),
                    cont.clone())
            },
            Lang::Sequence(vals, _) => {
                let res = if vals.len() > 0 {
                    "c(".to_string() + 
                       &vals.iter().map(|x| "list(".to_string() + &x.to_r(cont).0 + ")")
                       .collect::<Vec<_>>().join(", ")
                    + ")"
                } else {
                    "c(list())".to_string()
                };
               (res, cont.to_owned())
            },
            Lang::TestBlock(body, h) => {
                let current_dir = match std::env::current_dir() {
                    Ok(dir) => dir,
                    Err(e) => {
                        eprintln!("Erreur lors de l'obtention du répertoire courant: {}", e);
                        std::process::exit(1);
                    }
                };
                // Définir le chemin du dossier
                let dir = current_dir.join("tests/testthat");

                let file_name = format!("test-{}", h.get_file_data().unwrap().0)
                    .replace("TypR/", "").replace(".ty", ".R");

                // Définir le chemin complet du fichier
                let file_path = dir.join(&file_name);

                // Écrire le contenu
                let mut file = fs::File::create(&file_path).unwrap();
                file.write_all(body.to_r(cont).0.as_bytes()).unwrap();
                ("".to_string(), cont.clone())
            },
            Lang::JSBlock(exp, _id, _h) => {
                let js_cont = Context::default(); //TODO get js context from memory
                let res = exp.to_js(&js_cont).0;
                (format!("'{}{}'", JS_HEADER, res), cont.clone())
            },
            Lang::WhileLoop(condition, body, _) => {
                (format!("while ({}) {{\n{}\n}}", 
                        condition.to_r(cont).0,
                        body.to_r(cont).0), cont.clone())
            },
            Lang::Break(_) => {
                ("break".to_string(), cont.clone())
            },
            Lang::Module(name, body, position, config, _) => {
                let name = if (name == "main") && (config.environment == Environment::Project) {
                    "a_main"
                } else { name };
                let content = body.iter()
                    .map(|lang| lang.to_r(cont).0)
                    .collect::<Vec<_>>().join("\n");
                match (position, config.environment) {
                    (ModulePosition::Internal, _) => {
                        (content, cont.clone())
                    },
                    (ModulePosition::External, Environment::StandAlone) |
                     (ModulePosition::External, Environment::Repl) => {
                        let output_dir: PathBuf = ".".into();
                        let std_path = output_dir.join(format!("{}.R", name));
                        let mut module_file = File::create(std_path.clone()).unwrap();
                        module_file.write_all(content.as_bytes()).unwrap();
                        (format!("source('{}')", std_path.display()), cont.clone())
                    },
                    (ModulePosition::External, Environment::Project) => {
                        let output_dir: PathBuf = ".".into();
                        let std_path = output_dir.join(format!("R/{}.R", name));
                        let mut module_file = File::create(std_path.clone()).unwrap();
                        module_file.write_all(content.as_bytes()).unwrap();
                        ("".to_string(), cont.clone())
                    },
                }
            },
            _ => {
                println!("This language structure won't transpile: {:?}", self);
                ("".to_string(), cont.clone())
            },
        };
        
        result
    }
}
