use crate::argument_value::ArgumentValue;
use crate::operation_priority::TokenKind;
use crate::type_comparison::reduce_type;
use crate::argument_type::ArgumentType;
use crate::translatable::RTranslatable;
use crate::function_type::FunctionType;
use crate::translatable::Translatable;
use serde::{Serialize, Deserialize};
use crate::function_lang::Function;
use crate::array_type::ArrayType;
use crate::lang_token::LangToken;
use crate::help_data::HelpData;
use crate::elements::elements;
use crate::var::Permission;
use crate::operators::Op;
use crate::r#type::Type;
use crate::Environment;
use std::path::PathBuf;
use std::str::FromStr;
use crate::var::Var;
use crate::Context;
use crate::builder;
use std::io::Write;
use crate::typing;
use crate::Config;
use std::fs::File;
use crate::fs;

const JS_HEADER: &str = "let add = (a, b) => a+b;\nlet mul = (a, b) => a*b;\nlet minus = (a, b) => a - b;\nlet div = (a, b) => a/b;";

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

#[derive(Debug, Serialize, Deserialize, PartialEq, Clone)]
pub enum ModulePosition {
    Internal,
    External
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Lang {
    Number(f32, HelpData),
    Integer(i32, HelpData),
    Bool(bool, HelpData),
    Char(String, HelpData),
    Union(Box<Lang>, Box<Lang>, HelpData),
    Scope(Vec<Lang>, HelpData),
    Function(Vec<ArgumentType>, Type, Box<Lang>, HelpData),
    Module(String, Vec<Lang>, ModulePosition, Config, HelpData), // module name { lines }
    ModuleDecl(String, HelpData), // to create an env
    Variable(String, Permission, bool, Type, HelpData),
    FunctionApp(Box<Lang>, Vec<Lang>, Type, HelpData),
    VecFunctionApp(Box<Lang>, Vec<Lang>, Type, HelpData),
    MethodCall(Box<Lang>, Vec<Lang>, Type, HelpData),
    ArrayIndexing(Box<Lang>, Box<Lang>, HelpData),
    Let(Box<Lang>, Type, Box<Lang>, HelpData),
    Alias(Box<Lang>, Vec<Type>, Type, HelpData),
    Array(Vec<Lang>, HelpData),
    Record(Vec<ArgumentValue>, HelpData),
    Tag(String, Box<Lang>, HelpData),
    If(Box::<Lang>, Box<Lang>, Box<Lang>, HelpData),
    Match(Box<Lang>, Var, Vec<(Type, Box<Lang>)>, HelpData),
    Tuple(Vec<Lang>, HelpData),
    Lines(Vec<Lang>, HelpData),
    Assign(Box<Lang>, Box<Lang>, HelpData),
    Comment(String, HelpData),
    ModuleImport(String, HelpData), // mod name;
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
    JSBlock(Box<Lang>, u32, HelpData), // = (js_code, context_id, help_data)
    Use(Box<Lang>, Box<Lang>, HelpData),
    Empty(HelpData),
    WhileLoop(Box<Lang>, Box<Lang>, HelpData),
    Break(HelpData),
    Operator(Op, Box<Lang>, Box<Lang>, HelpData),
}

impl Default for Lang {
    fn default() -> Lang {
        builder::empty_lang()
    }
}

impl From<Var> for Lang {
   fn from(val: Var) -> Self {
       Lang::Variable(val.0, val.1, val.2, val.3, val.4)
   } 
}

impl From<LangToken> for  Lang {
   fn from(val: LangToken) -> Self {
        match val {
            LangToken::Expression(exp) => exp,
            LangToken::Operator(op) 
                => panic!("Shouldn't convert the token to lang {}", op),
            LangToken::EmptyOperator => panic!("Shouldn't be empty ")
        }
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
    pub fn to_module(self, name: &str, environment: Environment) -> Self {
        match self {
            Lang::Lines(v, h) => Lang::Module(name.to_string(), v, ModulePosition::External, Config::default().set_environment(environment), h),
            s => s 
        }
    }

    fn set_type_if_variable(&self, typ: &Type) -> Lang {
        match self {
            Lang::Variable(name, perm, spec, _, h) 
                => Lang::Variable(name.clone(), perm.clone(), spec.clone(), typ.clone(), h.clone()),
            _ => self.clone()
        }
    }

    pub fn to_arg_type(&self) -> Option<ArgumentType> {
        match self {
            Lang::Let(var, ty, _, _) => {
                Some(ArgumentType::new(&Var::from_language((**var).clone())
                                       .unwrap().get_name(), &ty))
            },
            Lang::Alias(var, _types, ty, _) => {
                Some(ArgumentType::new(&Var::from_language((**var).clone())
                                       .unwrap().get_name(), &ty))
            },
            _ => None
        }
    }

    pub fn extract_types_from_expression(&self, context: &Context) -> Vec<Type> {
        if self.is_value() {
            vec![typing(context, self).0.clone()]
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
        if let Lang::Function(_, _, body, _h) = self.clone() {
            if let Lang::Scope(v, _) = *body.clone() {
                   let ele = v.first().unwrap();
                   if let Lang::Empty(_) = ele {true} else {false}
            } else {false}
        } else {false}
    }

    pub fn is_function(&self) -> bool {
        match self {
            Lang::Function(_, _, _, _) => true,
            Lang::RFunction(_, _, _) => true,
            _ => false
        }
    }

    pub fn infer_var_name(&self, args: &Vec<Lang>, context: &Context) -> Var {
        if args.len() > 0 {
                        let first = typing(context, &args.iter().nth(0).unwrap().clone()).0;
                        Var::from_language(self.clone())
                            .unwrap().set_type(first.clone())
                    } else {
                        Var::from_language(self.clone()).unwrap()
            }
    }

    pub fn get_related_function(self, args: &Vec<Lang>, context: &Context) 
        -> Option<FunctionType> {
        let var_name = self.infer_var_name(args, context);
        let fn_ty = typing(context, &var_name.to_language()).0;
        fn_ty.clone().to_function_type()
    }

    pub fn lang_substitution(&self, sub_var: &Lang, var: &Lang, context: &Context) -> String {
        if let Lang::Variable(name, _, _, _, _) = var {
            let res = match self {
                Lang::Variable(_, _, _, _, h) if self == sub_var 
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
            Lang::Union(_, _, h) => h,
            Lang::Scope(_, h) => h,
            Lang::Function(_, _, _, h) => h,
            Lang::Module(_, _, _, _, h) => h,
            Lang::ModuleDecl(_, h) => h,
            Lang::Variable(_, _, _, _, h) => h,
            Lang::FunctionApp(_, _, _, h) => h,
            Lang::VecFunctionApp(_, _, _, h) => h,
            Lang::MethodCall(_, _, _, h) => h,
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
            Lang::ModuleImport(_, h) => h,
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
            Lang::Not(_, h) => h,
            Lang::Sequence(_, h) => h,
            Lang::TestBlock(_, h) => h,
            Lang::JSBlock(_, _, h) => h,
            Lang::Use(_, _, h) => h,
            Lang::WhileLoop(_, _, h) => h,
            Lang::Break(h) => h,
            Lang::Operator(_, _, _, h) => h,
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
            Lang::Function(params, _, _, _) => params.len(),
            _ => 0 as usize
        }
    }

    pub fn simple_print(&self) -> String {
        match self {
            Lang::Number(_, _) => "Number".to_string(),
            Lang::Integer(_, _) => "Integer".to_string(),
            Lang::Char(_, _) => "Char".to_string(),
            Lang::Bool(_, _) => "Bool".to_string(),
            Lang::Union(_, _, _) => "Union".to_string(),
            Lang::Scope(_, _) => "Scope".to_string(),
            Lang::Function(_, _, _, _) => "Function".to_string(),
            Lang::Module(_, _, _, _, _) => "Module".to_string(),
            Lang::ModuleDecl(_, _) => "ModuleDecl".to_string(),
            Lang::Variable(name, _, _, _, _) => format!("Variable({})", name),
            Lang::FunctionApp(var, _, _, _) => 
                format!("FunctionApp({})", Var::from_language(*(var.clone())).unwrap().get_name()),
            Lang::VecFunctionApp(var, _, _, _) => 
                format!("VecFunctionApp({})", Var::from_language(*(var.clone())).unwrap().get_name()),
            Lang::MethodCall(var, _, _, _) => 
                format!("MethodCall({})", Var::from_language(*(var.clone())).unwrap().get_name()),
            Lang::ArrayIndexing(_, _, _) => "ArrayIndexing".to_string(),
            Lang::Let(var, _, _, _) 
                => format!("let {}", Var::from_language((**var).clone()).unwrap().get_name()),
            Lang::Array(_, _) => "Array".to_string(),
            Lang::Record(_, _) => "Record".to_string(),
            Lang::Alias(_, _, _, _) => "Alias".to_string(),
            Lang::Tag(_, _, _) => "Tag".to_string(),
            Lang::If(_, _, _, _) => "If".to_string(),
            Lang::Match(_, _, _, _) => "Match".to_string(),
            Lang::Tuple(_, _) => "Tuple".to_string(),
            Lang::Lines(_, _) => "Sequence".to_string(),
            Lang::Assign(_, _, _) => "Assign".to_string(),
            Lang::Comment(_, _) => "Comment".to_string(),
            Lang::ModuleImport(_, _) => "ModImp".to_string(),
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
            Lang::Not(_, _) => "Not".to_string(),
            Lang::Sequence(_, _) => "Sequence".to_string(),
            Lang::TestBlock(_, _) => "TestBlock".to_string(),
            Lang::JSBlock(_, _, _) => "JSBlock".to_string(),
            Lang::Use(_, _, _) => "Use".to_string(),
            Lang::WhileLoop(_, _, _) => "WhileLoop".to_string(),
            Lang::Break(_) => "Break".to_string(),
            Lang::Operator(_, _, _, _) => "Operator".to_string(),
        }
    }

    pub fn typing(&self, context: &Context) -> (Type, Lang, Context) {
        let res = typing(context, self);
        (res.0.clone(), res.1, res.2)
    }

    //main
    pub fn to_js(&self, context: &Context) -> (String, Context) {
        match self {
            Lang::Char(val, _) => {
                (format!("\\'{}\\'", val),
                 context.clone())
            },
            Lang::Let(var, _, body, _) => {
                (format!("let {} = {};", Var::from_language(*(var.clone())).unwrap().get_name(), body.to_js(context).0),
                 context.clone())
            },
            Lang::Assign(var, body, _) => {
                (format!("{} = {};", var.to_js(context).0, body.to_js(context).0),
                 context.clone())
            },
            Lang::Scope(langs, _) => {
                let res = langs.iter()
                    .map(|x| x.to_js(context).0)
                    .collect::<Vec<_>>().join("\n");
                (res, context.clone())
            },
            Lang::Return(exp, _) => {
                (format!("return {};", exp.to_js(context).0), context.clone())
            },
            Lang::Integer(i, _) => {
                (i.to_string(), context.clone())
            },
            Lang::FunctionApp(exp, params, _, _) => {
                let var = Var::try_from(exp.clone()).unwrap();
                let res = format!("{}({})", var.get_name().replace("__", "."),
                        params.iter()
                        .map(|x| x.to_js(context).0)
                        .collect::<Vec<_>>().join(", "));
                (res, context.clone())
            },
            Lang::Function(params, _, body, _) => {
                let parameters = &params.iter()
                    .map(|x| x.get_argument_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                (format!("({}) => {{\n{}\n}}", parameters, body.to_js(context).0),
                 context.clone())
            },
            Lang::Use(lib, members, _) => {
                let body = match (**members).clone() {
                    Lang::Vector(v, _) 
                        => v.iter()
                            .map(|val| val.to_js(context).0.replace("\\'", ""))
                            .collect::<Vec<_>>().join(", "),
                    Lang::Char(val, _) => val.clone(),
                    lang => lang.simple_print()
                };
                (format!("import {{ {} }} from {};", 
                         body, lib.to_js(context).0), context.clone())
            },
            Lang::Sequence(v, _) => {
                let res = "[".to_string() + &v.iter()
                    .map(|lang| lang.to_js(context).0)
                    .collect::<Vec<_>>().join(", ") + "]";
                (res, context.clone())
            },
            Lang::Array(v, _) => {
                let res = "[".to_string() + &v.iter()
                    .map(|lang| lang.to_js(context).0)
                    .collect::<Vec<_>>().join(", ") + "]";
                (res, context.clone())
            },
            Lang::Vector(v, _) => {
                let res = "[".to_string() + &v.iter()
                    .map(|lang| lang.to_js(context).0)
                    .collect::<Vec<_>>().join(", ") + "]";
                (res, context.clone())
            },
            Lang::Record(arg_vals, _) => {
                let res = "{".to_string() + &arg_vals.iter()
                    .map(|arg_val| 
                         arg_val.get_argument().replace("'", "") + ": " 
                         + &arg_val.get_value().to_js(context).0)
                    .collect::<Vec<_>>().join(", ") + "}";
                (res, context.clone())
            },
            Lang::Lambda(body, _) => {
                (format!("x => {}", body.to_js(context).0), context.clone())
            },
            Lang::Bool(b, _) => {
                (b.to_string(), context.clone())
            },
            _ => {
                self.to_r(context)
            }
        }
    }

    //main
    pub fn to_simple_r(&self, context: &Context) -> (String, Context) {
        match self {
            Lang::Number(n, _) => (n.to_string(), context.clone()),
            Lang::Array(v, _) => {
                if v.len() == 1 {
                    v[0].to_simple_r(context)
                } else {
                    panic!("Not yet implemented for indexing of multiple elements")
                }
            }
            _ => self.to_r(context)
        }
    }

    pub fn to_module_member(self) -> Lang {
        match self {
            Lang::Module(name, body, _, _, h) => {
                Lang::Lines(body.clone(), h).to_module_helper(&name)
            },
            res => res
        }
    }

    pub fn to_module_helper(self, name: &str) -> Lang{
        match self.clone() {
            Lang::Variable(_, _, _, _, h) => {
                Lang::Operator(Op::Dollar(h.clone()), 
                               Box::new(Var::from_name(name).to_language()),
                               Box::new(self), h)
            },
            Lang::Let(var, typ, lang, h) => {
                let expr = Lang::Operator(Op::Dollar(h.clone()), var,
                    Box::new(Var::from_name(name).to_language()), h.clone());
                Lang::Let(Box::new(expr), typ, lang, h)
            },
            Lang::Alias(var, types, typ, h) => {
                let expr = Lang::Operator(Op::Dollar(h.clone()),
                    Box::new(Var::from_name(name).to_language()), var, h.clone());
                Lang::Alias(Box::new(expr), types, typ, h)
            },
            Lang::Function(args, typ, body, h) => {
                Lang::Function(args, typ, Box::new(body.to_module_helper(name)), h)
            },
            Lang::Lines(exprs, h) => {
                Lang::Lines(exprs.iter()
                    .cloned()
                    .map(|expr| expr.to_module_helper(name))
                    .collect::<Vec<_>>(), h)
            },
            rest => rest
        }
    }

    pub fn to_arg_value(self, type_module: &Type, context: &Context) 
        -> Option<Vec<ArgumentValue>> {
        match self {
            Lang::Let(lang, _, body, h) 
                if Var::from_language(*lang.clone()).is_some() => {
                    let var = Var::from_language(*lang).unwrap();
                    type_module.get_first_function_parameter_type(&var.get_name())
                        .map(|typ_par| 
                             var.clone().set_name(&format!("{}.{}", 
                                                  var.get_name(),
                                                  context.get_type_anotation_no_parentheses(&typ_par))))
                        .map(|var2| Some(vec![
                                        ArgumentValue(var.get_name(), 
                                                      Lang::GenFunc(var.get_name(), var.get_name(), h)),
                                        ArgumentValue(var2.get_name(), *body.clone())
                        ]))
                        .unwrap_or(Some(vec![ArgumentValue(var.get_name(), *body)]))
                },
            _ => None
        }
    }

    pub fn get_token_type(&self) -> TokenKind {
        TokenKind::Expression
    }

    pub fn get_binding_power(&self) -> i32 {
        1
    }

    pub fn get_members_if_array(&self) -> Option<Vec<Lang>> {
        match self {
            Lang::Array(members, _) => Some(members.clone()),
            _ => None
        }
    }

    pub fn len(&self) -> i32 {
        match self {
            Lang::Integer(i, _) => *i,
            n => panic!("not implemented for language {}", n.simple_print())
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
           Lang::Variable(_, _, _, _, h) => h,
           Lang::Match(_, _, _, h) => h,
           Lang::FunctionApp(_, _, _, h) => h,
           Lang::VecFunctionApp(_, _, _, h) => h,
           Lang::MethodCall(_, _, _, h) => h,
           Lang::Empty(h) => h,
           Lang::Array(_, h) => h,
           Lang::Record(_, h) => h,
           Lang::Scope(_, h) => h,
           Lang::Let(_, _, _, h) => h,
           Lang::Alias(_, _, _, h) => h,
           Lang::Lambda(_, h) => h,
           Lang::Function(_, _, _, h) => h,
           Lang::VecBlock(_, h) => h,
           Lang::If(_, _, _, h) => h,
           Lang::Assign(_, _, h) => h,
           Lang::Union(_, _, h) => h,
           Lang::Module(_, _, _, _, h) => h,
           Lang::ModuleDecl(_, h) => h,
           Lang::ModuleImport(_, h) => h,
           Lang::Import(_, h) => h,
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
           Lang::Not(_, h) => h,
           Lang::Sequence(_, h) => h,
           Lang::TestBlock(_, h) => h,
           Lang::JSBlock(_, _, h) => h,
           Lang::Use(_, _, h) => h,
           Lang::WhileLoop(_, _, h) => h,
           Lang::Break(h) => h,
           Lang::Operator(_, _, _, h) => h,
       }.clone()
   } 
}


use std::fmt;
impl fmt::Display for Lang {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Lang::Variable(name, _permision, _bo, typ, _h) 
                => format!("{} -> {}", name, typ),
            _ => format!("{:?}", self)
        };
        write!(f, "{}", res)       
    }
}

fn format_backtick(s: String) -> String {
    "`".to_string() + &s.replace("`", "") + "`"
}

//main
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
               let res = "concat(".to_string() + 
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
                (format!("'{}\n\n{}'", JS_HEADER, res), cont.clone())
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
                let content = body.iter()
                    .map(|lang| lang.to_r(cont).0)
                    .collect::<Vec<_>>().join("\n");
                match (position, config.environment) {
                    (ModulePosition::Internal, _) => {
                        (content, cont.clone())
                    },
                    (ModulePosition::External, Environment::StandAlone) => {
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

#[derive(Debug)]
pub struct ErrorStruct;

impl FromStr for Lang {
    type Err = ErrorStruct;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let val = elements(s.into())
            .map(|x| x.1).unwrap_or(builder::empty_lang());
        Ok(val)
    }
}

