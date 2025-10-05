use crate::r#type::Type;
use crate::var::Var;
use crate::var::Permission;
use serde::Serialize;
use crate::argument_type::ArgumentType;
use crate::argument_value::ArgumentValue;
use crate::argument_kind::ArgumentKind;
use crate::type_checker;
use crate::Context;
use crate::typing;
use crate::help_data::HelpData;
use crate::path::Path;
use crate::function_type::FunctionType;
use crate::type_comparison::reduce_type;
use crate::translatable::Translatable;
use crate::function_lang::Function;
use crate::array_type::ArrayType;
use crate::translatable::RTranslatable;
use crate::builder;
use std::str::FromStr;
use crate::elements::parse_elements;
use crate::fs;
use std::io::Write;

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

pub trait ToSome {
    fn to_some(self) -> Option<Self> where Self: Sized;
}

impl<T: Sized> ToSome for T {
    fn to_some(self) -> Option<Self> {
        Some(self)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Lang {
    Number(f32, HelpData),
    Integer(i32, HelpData),
    Bool(bool, HelpData),
    Char(String, HelpData),
    And(Box<Lang>, Box<Lang>, HelpData),
    Or(Box<Lang>, Box<Lang>, HelpData),
    Union(Box<Lang>, Box<Lang>, HelpData),
    In(Box<Lang>, Box<Lang>, HelpData),
    Eq(Box<Lang>, Box<Lang>, HelpData),
    Eq2(Box<Lang>, Box<Lang>, HelpData),
    NotEq(Box<Lang>, Box<Lang>, HelpData),
    Modu(Box<Lang>, Box<Lang>, HelpData), // modulus
    Modu2(Box<Lang>, Box<Lang>, HelpData), // modulus2
    LesserThan(Box<Lang>, Box<Lang>, HelpData),
    GreaterThan(Box<Lang>, Box<Lang>, HelpData),
    LesserOrEqual(Box<Lang>, Box<Lang>, HelpData),
    GreaterOrEqual(Box<Lang>, Box<Lang>, HelpData),
    Chain(Box<Lang>, Box<Lang>, HelpData),
    Dollar(Box<Lang>, Box<Lang>, HelpData),
    Scope(Vec<Lang>, HelpData),
    Function(Vec<ArgumentKind>, Vec<ArgumentType>, Type, Box<Lang>, HelpData),
    Module(String, Vec<Lang>, HelpData), // module name { lines }
    ModuleDecl(String, HelpData), // to create an env
    Variable(String, Path, Permission, bool, Type, HelpData),
    FunctionApp(Box<Lang>, Vec<Lang>, Type, HelpData),
    ArrayIndexing(Box<Lang>, Box<Lang>, HelpData),
    Let(Var, Type, Box<Lang>, HelpData),
    Array(Vec<Lang>, HelpData),
    Record(Vec<ArgumentValue>, HelpData),
    Alias(Var, Vec<Type>, Type, HelpData),
    Tag(String, Box<Lang>, HelpData),
    If(Box::<Lang>, Box<Lang>, Box<Lang>, HelpData),
    Match(Box<Lang>, Var, Vec<(Type, Box<Lang>)>, HelpData),
    Tuple(Vec<Lang>, HelpData),
    Lines(Vec<Lang>, HelpData),
    Assign(Box<Lang>, Box<Lang>, HelpData),
    Comment(String, HelpData),
    ModImp(String, HelpData), // mod name;
    Import(Type, HelpData), // type alias
    GenFunc(String, String, HelpData), //body, name, helpdata
    Test(Vec<Lang>, HelpData),
    Return(Box<Lang>, HelpData),
    VecBlock(String, HelpData),
    Lambda(Box<Lang>, HelpData),
    Library(String, HelpData),
    Exp(String, HelpData),
    Signature(Var, Type, HelpData),
    ForLoop(Var, Box<Lang>, Box<Lang>, HelpData), // variable, iterator, body
    RFunction(Vec<Lang>, String, HelpData), // variable, iterator, body
    KeyValue(String, Box<Lang>, HelpData),
    Vector(Vec<Lang>, HelpData),
    Sequence(Vec<Lang>, HelpData),
    Not(Box<Lang>, HelpData),
    TestBlock(Box<Lang>, HelpData),
    JSBlock(Box<Lang>, HelpData),
    Empty(HelpData)
}

impl From<Var> for Lang {
   fn from(val: Var) -> Self {
       Lang::Variable(val.0, val.1.into(), val.2, val.3, val.4, val.5)
   } 
}

pub fn build_generic_function(s: &str) -> Lang {
    builder::generic_function(
        &format!("{} <- function(x, ...) {{\n\tUseMethod('{}')\n}}\n", s, s)
                                                         )
}

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


fn set_related_type_if_variable((val, arg): (&Lang, &Type)) -> Lang {
    let oargs = FunctionType::try_from(arg.clone())
        .map(|fn_t| fn_t.get_param_types());

    match oargs {
        Ok(args) => (args.len() > 0)
                    .then_some(val.set_type_if_variable(&args[0]))
                    .unwrap_or(val.clone()),
        Err(_) => val.clone()
    }
    
}

//main
impl Lang {
    fn set_type_if_variable(&self, typ: &Type) -> Lang {
        match self {
            Lang::Variable(name, path, perm, spec, _, h) 
                => Lang::Variable(name.clone(), path.clone(), perm.clone(), spec.clone(), typ.clone(), h.clone()),
            _ => self.clone()
        }
    }

    pub fn extract_types_from_expression(&self, context: &Context) -> Vec<Type> {
        if self.is_value() {
            vec![typing(context, self).0[0].clone()]
        } else {
            match self {
                Lang::FunctionApp(exp, arg_typs, _, _) => {
                    let typs = exp.extract_types_from_expression(context);
                    let typs2 = arg_typs.iter()
                        .flat_map(|x| x.extract_types_from_expression(context))
                        .collect::<Vec<_>>();
                    typs.iter().chain(typs2.iter()).cloned().collect()
                },
                _ => vec![]
            }
        }
    }

    pub fn is_value(&self) -> bool {
        match self {
            Lang::Number(_, _) | Lang::Integer(_, _) | Lang::Bool(_, _) | Lang::Char(_, _) => true,
            Lang::Array(_, _) => true,
            _ => false
        }
    }

    pub fn is_undefined(&self) -> bool {
        if let Lang::Function(_, _, _, body, _h) = self.clone() {
            if let Lang::Scope(v, _) = *body.clone() {
                   let ele = v.first().unwrap();
                   if let Lang::Empty(_) = ele {true} else {false}
            } else {false}
        } else {false}
    }

    pub fn is_function(&self) -> bool {
        match self {
            Lang::Function(_, _, _, _, _) => true,
            Lang::RFunction(_, _, _) => true,
            _ => false
        }
    }

    pub fn infer_var_name(&self, args: &Vec<Lang>, context: &Context) -> Var {
        if args.len() > 0 {
                        let first = typing(context, &args.iter().nth(0).unwrap().clone()).0;
                        Var::from_language(self.clone())
                            .unwrap().set_type(first[0].clone())
                    } else {
                        Var::from_language(self.clone()).unwrap()
            }
    }

    pub fn get_related_function(self, args: &Vec<Lang>, context: &Context) 
        -> Option<FunctionType> {
        let var_name = self.infer_var_name(args, context);
        let fn_ty = typing(context, &var_name.to_language()).0;
        fn_ty[0].clone().to_function_type()
    }

    pub fn lang_substitution(&self, sub_var: &Lang, var: &Lang, context: &Context) -> String {
        if let Lang::Variable(name, _, _, _, _, _) = var {
            let res = match self {
                Lang::Variable(_, _, _, _, _, h) if self == sub_var 
                    => Lang::Exp(format!("{}[[2]]", name.to_string()), h.clone()),
                lang => lang.clone()
            };
            res.to_r(context).0
        } else { panic!("var is not a variable") }
    }

    pub fn get_help_data(&self) -> HelpData {
        match self {
            Lang::Number(_, h) => h,
            Lang::Integer(_, h) => h,
            Lang::Char(_, h) => h,
            Lang::Bool(_, h) => h,
            Lang::And(_, _, h) => h,
            Lang::Or(_, _, h) => h,
            Lang::Union(_, _, h) => h,
            Lang::In(_, _, h) => h,
            Lang::Eq(_, _, h) => h,
            Lang::Eq2(_, _, h) => h,
            Lang::NotEq(_, _, h) => h,
            Lang::Modu(_, _, h) => h,
            Lang::Modu2(_, _, h) => h,
            Lang::LesserThan(_, _, h) => h,
            Lang::GreaterThan(_, _, h) => h,
            Lang::LesserOrEqual(_, _, h) => h,
            Lang::GreaterOrEqual(_, _, h) => h,
            Lang::Chain(_, _, h) => h,
            Lang::Scope(_, h) => h,
            Lang::Function(_, _, _, _, h) => h,
            Lang::Module(_, _, h) => h,
            Lang::ModuleDecl(_, h) => h,
            Lang::Variable(_, _, _, _, _, h) => h,
            Lang::FunctionApp(_, _, _, h) => h,
            Lang::ArrayIndexing(_, _, h) => h,
            Lang::Let(_, _, _, h) => h,
            Lang::Array(_, h) => h,
            Lang::Record(_, h) => h,
            Lang::Alias(_, _, _, h) => h,
            Lang::Tag(_, _, h) => h,
            Lang::If(_, _, _, h) => h,
            Lang::Match(_, _, _, h) => h,
            Lang::Tuple(_, h) => h,
            Lang::Lines(_, h) => h,
            Lang::Assign(_, _, h) => h,
            Lang::Comment(_, h) => h,
            Lang::ModImp(_, h) => h,
            Lang::Import(_, h) => h,
            Lang::GenFunc(_, _, h) => h,
            Lang::Test(_, h) => h,
            Lang::Return(_, h) => h,
            Lang::VecBlock(_, h) => h,
            Lang::Lambda(_, h) => h,
            Lang::Library(_, h) => h,
            Lang::Exp(_, h) => h,
            Lang::Empty(h) => h,
            Lang::Signature(_, _, h) => h,
            Lang::ForLoop(_, _, _, h) => h,
            Lang::RFunction(_, _, h) => h,
            Lang::KeyValue(_, _, h) => h,
            Lang::Vector(_, h) => h,
            Lang::Dollar(_, _, h) => h,
            Lang::Not(_, h) => h,
            Lang::Sequence(_, h) => h,
            Lang::TestBlock(_, h) => h,
            Lang::JSBlock(_, h) => h,
        }.clone()
    }

    pub fn linearize_array(&self) -> Vec<Lang> {
        match self {
            Lang::Array(v, _) 
                => v.iter()
                .fold(vec![], |acc, x| 
                      acc.iter()
                      .chain(x.linearize_array().iter())
                      .cloned().collect()),
            _ => vec![self.to_owned()]
        }
    }

    pub fn is_r_function(&self) -> bool {
        match self {
            Lang::RFunction(_, _, _) => true,
            _ => false
        }
    }

    pub fn nb_params(&self) -> usize {
        self.simple_print();
        match self {
            Lang::Function(_, params, _, _, _) => params.len(),
            _ => 0 as usize
        }
    }

    pub fn simple_print(&self) -> String {
        match self {
            Lang::Number(_, _) => "Number".to_string(),
            Lang::Integer(_, _) => "Integer".to_string(),
            Lang::Char(_, _) => "Char".to_string(),
            Lang::Bool(_, _) => "Bool".to_string(),
            Lang::And(_, _, _) => "And".to_string(),
            Lang::Or(_, _, _) => "Or".to_string(),
            Lang::Union(_, _, _) => "Union".to_string(),
            Lang::In(_, _, _) => "In".to_string(),
            Lang::Eq(_, _, _) => "Eq".to_string(),
            Lang::Eq2(_, _, _) => "Eq2".to_string(),
            Lang::NotEq(_, _, _) => "NotEq".to_string(),
            Lang::Modu(_, _, _) => "Modu".to_string(),
            Lang::Modu2(_, _, _) => "Modu2".to_string(),
            Lang::LesserThan(_, _, _) => "LesserThan".to_string(),
            Lang::GreaterThan(_, _, _) => "GreaterThan".to_string(),
            Lang::LesserOrEqual(_, _, _) => "LesserOrEqual".to_string(),
            Lang::GreaterOrEqual(_, _, _) => "GreatOrEqual".to_string(),
            Lang::Chain(_, _, _) => "Chain".to_string(),
            Lang::Scope(_, _) => "Scope".to_string(),
            Lang::Function(_, _, _, _, _) => "Function".to_string(),
            Lang::Module(_, _, _) => "Module".to_string(),
            Lang::ModuleDecl(_, _) => "ModuleDecl".to_string(),
            Lang::Variable(name, _, _, _, _, _) => format!("Variable({})", name),
            Lang::FunctionApp(var, _, _, _) => 
                format!("FunctionApp({})", Var::from_language(*(var.clone())).unwrap().get_name()),
            Lang::ArrayIndexing(_, _, _) => "ArrayIndexing".to_string(),
            Lang::Let(var, _, _, _) => format!("let {}", var.get_name()),
            Lang::Array(_, _) => "Array".to_string(),
            Lang::Record(_, _) => "Record".to_string(),
            Lang::Alias(_, _, _, _) => "Alias".to_string(),
            Lang::Tag(_, _, _) => "Tag".to_string(),
            Lang::If(_, _, _, _) => "If".to_string(),
            Lang::Match(_, _, _, _) => "Match".to_string(),
            Lang::Tuple(_, _) => "Tuple".to_string(),
            Lang::Lines(_, _) => "Sequence".to_string(),
            Lang::Assign(_, _, _) => "Addign".to_string(),
            Lang::Comment(_, _) => "Comment".to_string(),
            Lang::ModImp(_, _) => "ModImp".to_string(),
            Lang::Import(_, _) => "Import".to_string(),
            Lang::GenFunc(_, _, _) => "GenFunc".to_string(),
            Lang::Test(_, _) => "Test".to_string(),
            Lang::Return(_, _) => "Return".to_string(),
            Lang::VecBlock(_, _) => "VecBloc".to_string(),
            Lang::Lambda(_, _) => "Lambda".to_string(),
            Lang::Library(_, _) => "Library".to_string(),
            Lang::Exp(_, _) => "Exp".to_string(),
            Lang::Empty(_) => "Empty".to_string(),
            Lang::Signature(_, _, _) => "Signature".to_string(),
            Lang::ForLoop(_, _, _, _) => "ForLoop".to_string(),
            Lang::RFunction(_, _, _) => "RFunction".to_string(),
            Lang::KeyValue(_, _, _) => "KeyValue".to_string(),
            Lang::Vector(_, _) => "Vector".to_string(),
            Lang::Dollar(_, _, _) => "Dollar".to_string(),
            Lang::Not(_, _) => "Not".to_string(),
            Lang::Sequence(_, _) => "Sequence".to_string(),
            Lang::TestBlock(_, _) => "TestBlock".to_string(),
            Lang::JSBlock(_, _) => "JSBlock".to_string(),
        }
    }

    pub fn typing(&self, context: &Context) -> (Type, Context) {
        let res = typing(context, self);
        (res.0[0].clone(), res.2)
    }

    pub fn to_js(&self, context: &Context) -> (String, Context) {
        match self {
            Lang::Let(var, _, body, _) => {
                (format!("let {} = {};", var.get_name(), body.to_js(context).0),
                 context.clone())
            },
            Lang::Assign(var, body, _) => {
                (format!("let {} = {};", var.to_js(context).0, body.to_js(context).0),
                 context.clone())
            },
            Lang::Scope(langs, _) => {
                let res = langs.iter()
                    .map(|x| x.to_js(context).0)
                    .collect::<Vec<_>>().join("\n");
                (res, context.clone())
            },
            Lang::Integer(i, _) => {
                (i.to_string(), context.clone())
            },
            Lang::FunctionApp(exp, params, _, _) => {
                let var = Var::try_from(exp.clone()).unwrap();
                let res = format!("{}({})", var.get_name(),
                        params.iter()
                        .map(|x| x.to_js(context).0)
                        .collect::<Vec<_>>().join(", "));
                (res, context.clone())
            },
            _ => {
                self.to_r(context)
            }
        }
    }
}

impl From<Lang> for HelpData {
   fn from(val: Lang) -> Self {
       match val {
           Lang::Number(_, h) => h,
           Lang::Integer(_, h) => h,
           Lang::Bool(_, h) => h,
           Lang::Char(_, h) => h,
           Lang::Variable(_, _, _, _, _, h) => h,
           Lang::Match(_, _, _, h) => h,
           Lang::FunctionApp(_, _, _, h) => h,
           Lang::Empty(h) => h,
           Lang::Array(_, h) => h,
           Lang::Eq(_, _, h) => h,
           Lang::Eq2(_, _, h) => h,
           Lang::NotEq(_, _, h) => h,
           Lang::Chain(_, _, h) => h,
           Lang::Record(_, h) => h,
           Lang::Scope(_, h) => h,
           Lang::Let(_, _, _, h) => h,
           Lang::Alias(_, _, _, h) => h,
           Lang::Lambda(_, h) => h,
           Lang::Function(_, _, _, _, h) => h,
           Lang::VecBlock(_, h) => h,
           Lang::If(_, _, _, h) => h,
           Lang::Assign(_, _, h) => h,
           Lang::And(_, _, h) => h,
           Lang::Or(_, _, h) => h,
           Lang::Union(_, _, h) => h,
           Lang::In(_, _, h) => h,
           Lang::Modu(_, _, h) => h,
           Lang::Modu2(_, _, h) => h,
           Lang::Module(_, _, h) => h,
           Lang::ModuleDecl(_, h) => h,
           Lang::ModImp(_, h) => h,
           Lang::Import(_, h) => h,
           Lang::GreaterThan(_, _, h) => h,
           Lang::GreaterOrEqual(_, _, h) => h,
           Lang::LesserThan(_, _, h) => h,
           Lang::LesserOrEqual(_, _, h) => h,
           Lang::ArrayIndexing(_, _, h) => h,
           Lang::Tag(_, _, h) => h,
           Lang::Tuple(_, h) => h,
           Lang::Lines(_, h) => h,
           Lang::Comment(_, h) => h,
           Lang::GenFunc(_, _, h) => h,
           Lang::Test(_, h) => h,
           Lang::Return(_, h) => h,
           Lang::Library(_, h) => h,
           Lang::Exp(_, h) => h,
           Lang::Signature(_, _, h) => h,
           Lang::ForLoop(_, _, _, h) => h,
           Lang::RFunction(_, _, h) => h,
           Lang::KeyValue(_, _, h) => h,
           Lang::Vector(_, h) => h,
           Lang::Dollar(_, _, h) => h,
           Lang::Not(_, h) => h,
           Lang::Sequence(_, h) => h,
           Lang::TestBlock(_, h) => h,
           Lang::JSBlock(_, h) => h,
       }.clone()
   } 
}

impl From<Vec<Lang>> for HelpData {
   fn from(val: Vec<Lang>) -> Self {
       if val.len() > 0 {
            val[0].clone().into()
       } else { HelpData::default() }
   } 
}

use std::fmt;
impl fmt::Display for Lang {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Lang::Variable(name, path, _permision, _bo, typ, _h) 
                => format!("{}{} -> {}", path, name, typ),
            _ => format!("{:?}", self)
        };
        write!(f, "{}", res)       
    }
}

//main
impl RTranslatable<(String, Context)> for Lang {
    fn to_r(&self, cont: &Context) -> (String, Context) {
        let result = match self {
            Lang::Bool(b, _) => 
                (format!("{}", b.to_string().to_uppercase()), cont.clone()),
            Lang::In(b1, b2, _) => {
                Translatable::from(cont.clone())
                    .to_r(b1).add(" %in% ").to_r(b2).into()
            },
            Lang::And(b1, b2, _) => {
                Translatable::from(cont.clone())
                    .to_r(b1).add(" & ").to_r(b2).into()
            },
            Lang::Or(b1, b2, _) => {
                Translatable::from(cont.clone())
                    .to_r(b1).add(" | ").to_r(b2).into()
            },
            Lang::Modu(e1, e2, _) => {
                Translatable::from(cont.clone())
                    .to_r(e1).add(" % ").to_r(e2).into()
            },
            Lang::Modu2(e1, e2, _) => {
                Translatable::from(cont.clone())
                    .to_r(e1).add(" %% ").to_r(e2).into()
            },
            Lang::Number(n, _) => 
                (format!("{}", n), cont.clone()),
            Lang::Eq(e1, e2, _) => {
                Translatable::from(cont.clone())
                    .to_r(e2).add(" == ").to_r(e1).into()
            },
            Lang::NotEq(e1, e2, _) => {
                Translatable::from(cont.clone())
                    .to_r(e2).add(" != ").to_r(e1).into()
            },
            Lang::LesserThan(e1, e2, _) => {
                Translatable::from(cont.clone())
                    .to_r(e2).add(" < ").to_r(e1).into()
            },
            Lang::GreaterThan(e1, e2, _) => {
                Translatable::from(cont.clone())
                    .to_r(e2).add(" > ").to_r(e1).into()
            },
            Lang::LesserOrEqual(e1, e2, _) => {
                Translatable::from(cont.clone())
                    .to_r(e2).add(" <= ").to_r(e1).into()
            },
            Lang::GreaterOrEqual(e1, e2, _) => {
                Translatable::from(cont.clone())
                    .to_r(e2).add(" >= ").to_r(e1).into()
            },
            Lang::Chain(e1, e2, _) => {
                match *e1.clone() {
                    Lang::Variable(_, _, _, _, _, _) => {
                        Translatable::from(cont.clone())
                            .to_r(e2)
                            .add("[['").to_r(e1).add("']]").into()
                    },
                    Lang::Record(fields, _) => {
                        let at = fields[0].clone();
                        Translatable::from(cont.clone())
                            .add("within(").to_r(e2)
                            .add(", { ").add(&at.get_argument())
                            .add(" <- ")
                            .to_r(&at.get_value()).add(" })")
                            .into()
                    }
                    Lang::FunctionApp(_, _, _, _) => {
                        Translatable::from(cont.clone())
                            .to_r(e1).add(" |> ").to_r(e2)
                            .into()
                    }
                    _ => {
                        Translatable::from(cont.clone())
                            .to_r(e2).add("[[")
                            .add("]]").to_r(e1)
                            .into()
                    }
                }
            },
            Lang::Scope(exps, _) => {
                Translatable::from(cont.clone())
                    .add("{\n")
                    .join(exps, "\n")
                    .add("\n}\n").into()
            },
            Lang::Function(_, args, return_type, body, _) => {
                //Wasn't able to use Translatable
                let sub_cont = cont.add_arg_types(args);
                let (body_str, new_cont) = body.to_r(&sub_cont);
                let fn_type = typing(&sub_cont, self).0;
                let output_conversion = cont.get_type_anotation(return_type);
                let res = (output_conversion == "")
                    .then_some("".to_string())
                    .unwrap_or(" |> ".to_owned() + &output_conversion);
                (format!("(function({}) {{\n {}{}\n}}) |> {}", 
                        args.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", "),
                        body_str, 
                        res,
                        cont.get_type_anotation(&fn_type[0])),
                new_cont)
            },
            Lang::Variable(v, path, _, _, ty, _) => {
                //Here we only keep the variable name, the path and the type
                let name = if v.contains("__") {
                    v.replace("__", ".")
                } else {
                    match ty {
                        Type::Empty(_) | Type::Any(_) => v.clone(),
                        _ => v.clone() + "." + &cont.get_class(ty)
                    }
                };
                ((path.clone().to_r() + &name).to_string(), cont.clone())
            }
            Lang::FunctionApp(exp, vals, _, _) => {
                let var = Var::try_from(exp.clone()).unwrap();
                if cont.is_an_untyped_function(&var.get_name()) {
                    let name = var.get_name().replace("__", ".");
                    let new_name = if &name[0..1] == "%" {
                        format!("`{}`", name)
                    } else { name.to_string() };
                    let s = format!("{}({})", new_name, 
                            vals.iter().map(|x| x.to_r(cont).0).collect::<Vec<_>>().join(", "));
                    (s, cont.clone())
                } else {
                    let (exp_str, cont1) = exp.to_r(cont);
                    let fn_t = cont1.get_true_fn_type(self)
                                    .unwrap_or(
                                        FunctionType::try_from(cont1.get_type_from_variable(&var).expect(&format!("variable {} don't have a related type", var))).unwrap());

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
                            let (name, path) = (var.get_name(), Path::new(&var.get_path()));
                            let new_name = if &name[0..1] == "%" {
                                format!("`{}`", name.replace("__", "."))
                            } else { name.replace("__", ".") };
                            (path != Path::default())
                                .then_some((format!("eval(quote({}({})), envir = {})",
                                    new_name, args, path.get_value()), current_cont.clone()))
                                .unwrap_or((format!("{}({})", new_name, args), current_cont.clone()))
                        }).unwrap_or((format!("{}({})", exp_str, args), current_cont))
                }
            },
            Lang::ArrayIndexing(exp, val, _) => {
                let (exp_str, _) = exp.to_r(cont);
                let (val_str, _) = val.to_r(cont);
                let res = match typing(cont, exp).0[0] {
                    Type::Array(_, _, _) | Type::Vector(_, _, _)
                        => format!("{}[{}]", exp_str, val_str), 
                    Type::Sequence(_, _, _) 
                        => format!("{}[[{}]]", exp_str, val_str), 
                    _ => "".to_string()
                };
                (res, cont.clone())
            },
            Lang::GenFunc(func, _, _) => 
                (func.to_string(), cont.clone()),
            Lang::Let(var, ttype, body, _) => {
                let (body_str, new_cont) = body.to_r(cont);
                let new_name = var.clone().to_r(cont);

                let (r_code, _new_name2) =
                Function::try_from((**body).clone())
                    .map(|_| {
                        let related_type = var.get_type();
                        match related_type {
                            Type::Empty(_) 
                                => (format!("{} <- {}", new_name, body_str), new_name.clone()),
                            Type::Any(_) | Type::Generic(_, _) 
                                => (format!("{}.default <- {}", new_name, body_str), new_name.clone()),
                            _ => {
                                //let class = cont.get_class_unquoted(&reduce_type(cont, &related_type));
                                let class = cont.get_type_anotation_no_parentheses(&related_type);
                                let new_name2 = format!("{}.{}", new_name.clone(), class);
                                (format!("{} <- {}", new_name2, body_str), new_name2)
                            }
                        }
                    }).unwrap_or((format!("{} <- {}", new_name, body_str), new_name));
                let code = if !ttype.is_empty() {
                    let anotation = new_cont.get_type_anotation(ttype);
                    if anotation == "Generic()" {
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
                let vector = &self.linearize_array()
                    .iter().map(|lang| lang.to_r(&cont).0)
                    .collect::<Vec<_>>().join(", ")
                    .and_if(|lin_array| lin_array != "")
                    .map(|lin_array| format!("c({})", lin_array))
                    .unwrap_or("logical(0)".to_string());

                let dim = typing(&cont, &self).0;

                let array = ArrayType::try_from(dim[0].clone()).unwrap().get_shape()
                    .map(|sha| format!("array({}, dim = c({}))", vector, sha))
                    .unwrap_or(format!("array({}, dim = c(0))", vector));

                (format!("{} |> {}", array, cont.get_type_anotation(&dim[0])) ,cont.to_owned())
            },
            Lang::Record(args, _) => {
                let (body, current_cont) = 
                Translatable::from(cont.clone())
                    .join_arg_val(args, ", ").into();
                let typ = type_checker::typing(cont, self).0;
                //let class = cont.get_class(&typ);
               let anotation = cont.get_type_anotation(&typ[0]);
                cont.get_classes(&typ[0])
                    .map(|_| format!("list({}) |> {}", 
                                body, anotation))
                    .unwrap_or(format!("list({}) |> {}",
                                body, anotation))
                    .to_some().map(|s| (s, current_cont)).unwrap()
            },
            Lang::Char(s, _) => 
                ("'".to_string() + s + "'", cont.clone()),
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
            Lang::Integer(i, _) => 
                (format!("{}L", i), cont.clone()),
            Lang::Tag(s, t, _) => {
                let (t_str, new_cont) = t.to_r(cont);
                let typ = type_checker::typing(cont, self).0;
                let class = cont.get_class(&typ[0]);
                cont.get_classes(&typ[0])
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
                => (format!("function(x) {{ {} }}", bloc.to_r(cont).0), cont.clone()),
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
                    .add(") {\n ").add(&body).add(" \n}")
                    .into()
            }
            Lang::Eq2(right, left, _) => {
                let res = match &**left {
                    Lang::Tag(n, _, _) => n.to_string(),
                    Lang::Variable(n, _, _, _, _, _) => n.to_string(),
                    _ => format!("{}", left) 
                };
                (format!("{} = {}", res, right.to_r(cont).0), cont.clone())
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
            Lang::Dollar(e1, e2, _) => {
                let name = e2.to_r(cont).0;
                match (**e1).clone() {
                    Lang::FunctionApp(exp, params, _, _) => {
                        let var = Var::from_language(*exp).unwrap();
                        let new_params = params.iter().map(|x| x.to_r(cont).0)
                                .collect::<Vec<_>>().join(", ");
                        (format!("{}${}({})", name, var.get_name(), new_params),
                            cont.to_owned())
                    },
                    _ => (format!("{}${}", name, e1.to_r(cont).0),
                            cont.to_owned())
                }
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
            Lang::JSBlock(exp, _h) => {
                let res = exp.to_js(cont).0;
                (format!("'{}'", res), cont.clone())
            },
            _ =>  {
                println!("This language structure won't transpile: {:?}", self);
                ("".to_string(), cont.clone())
            },
        };
        
        result
    }
}

#[derive(Debug)]
pub struct ErrorStruct;

impl FromStr for Lang {
    type Err = ErrorStruct;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let val = parse_elements(s.into())
            .map(|x| x.1).unwrap_or(builder::empty_lang());
        Ok(val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector1(){
        let res = "c(1, 2)".parse::<Lang>().unwrap();
        assert_eq!(res, Lang::Vector(vec![], HelpData::default()));
    }
}
