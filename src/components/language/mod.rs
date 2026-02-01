pub mod function_lang;
pub mod module_lang;
pub mod operators;
pub mod var;
pub mod var_function;
pub mod argument_value;
pub mod array_lang;

use crate::components::language::argument_value::ArgumentValue;
use crate::processes::transpiling::translatable::RTranslatable;
use crate::processes::type_checking::type_context::TypeContext;
use crate::processes::parsing::operation_priority::TokenKind;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::error_message::help_data::HelpData;
use crate::processes::parsing::lang_token::LangToken;
use crate::components::context::config::Environment;
use crate::processes::parsing::elements::elements;
use crate::components::context::config::Config;
use crate::components::language::operators::Op;
use crate::processes::type_checking::typing;
use crate::components::language::var::Var;
use crate::components::context::Context;
use crate::components::r#type::Type;
use serde::{Serialize, Deserialize};
use crate::utils::builder;
use std::str::FromStr;

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
    Module(String, Vec<Lang>, ModulePosition, Config, HelpData),
    ModuleDecl(String, HelpData),
    Variable(String, bool, Type, HelpData),
    FunctionApp(Box<Lang>, Vec<Lang>, HelpData),
    VecFunctionApp(Box<Lang>, Vec<Lang>, HelpData),
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
    ModuleImport(String, HelpData),
    Import(Type, HelpData),
    GenFunc(String, String, HelpData),
    Test(Vec<Lang>, HelpData),
    Return(Box<Lang>, HelpData),
    VecBlock(String, HelpData),
    Lambda(Box<Lang>, HelpData),
    Library(String, HelpData),
    Exp(String, HelpData),
    Signature(Var, Type, HelpData),
    ForLoop(Var, Box<Lang>, Box<Lang>, HelpData),
    RFunction(Vec<Lang>, String, HelpData),
    KeyValue(String, Box<Lang>, HelpData),
    Vector(Vec<Lang>, HelpData),
    Sequence(Vec<Lang>, HelpData),
    Not(Box<Lang>, HelpData),
    TestBlock(Box<Lang>, HelpData),
    JSBlock(Box<Lang>, u32, HelpData),
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
       Lang::Variable(val.0, val.1, val.2, val.3)
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

pub fn set_related_type_if_variable((val, arg): (&Lang, &Type)) -> Lang {
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
    pub fn save_in_memory(&self) -> bool {
        match self {
            Lang::Let(_, _, _, _) => true,
            Lang::Assign(_, _, _) => true,
            _ => false
        }
    }


    pub fn to_module(self, name: &str, environment: Environment) -> Self {
        match self {
            Lang::Lines(v, h) => Lang::Module(name.to_string(), v, ModulePosition::External, Config::default().set_environment(environment), h),
            s => s 
        }
    }

    fn set_type_if_variable(&self, typ: &Type) -> Lang {
        match self {
            Lang::Variable(name, spec, _, h) 
                => Lang::Variable(name.clone(), spec.clone(), typ.clone(), h.clone()),
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
            vec![typing(context, self).value.clone()]
        } else {
            match self {
                Lang::FunctionApp(exp, arg_typs, _) => {
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
                        let first = typing(context, &args.iter().nth(0).unwrap().clone()).value;
                        Var::from_language(self.clone())
                            .unwrap().set_type(first.clone())
                    } else {
                        Var::from_language(self.clone()).unwrap()
            }
    }

    pub fn get_related_function(self, args: &Vec<Lang>, context: &Context) 
        -> Option<FunctionType> {
        let var_name = self.infer_var_name(args, context);
        let fn_ty = typing(context, &var_name.to_language()).value;
        fn_ty.clone().to_function_type()
    }

    pub fn lang_substitution(&self, sub_var: &Lang, var: &Lang, context: &Context) -> String {
        if let Lang::Variable(name, _, _, _) = var {
            let res = match self {
                Lang::Variable(_, _, _, h) if self == sub_var 
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
            Lang::Variable(_, _, _, h) => h,
            Lang::FunctionApp(_, _, h) => h,
            Lang::VecFunctionApp(_, _, h) => h,
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
            Lang::Variable(name, _, _, _) => format!("Variable({})", name),
            Lang::FunctionApp(var, _, _) => 
                format!("FunctionApp({})", Var::from_language(*(var.clone())).unwrap().get_name()),
            Lang::VecFunctionApp(var, _, _) => 
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

    pub fn typing(&self, context: &Context) -> TypeContext {
        typing(context, self)
    }

    pub fn to_js(&self, context: &Context) -> (String, Context) {
        match self {
            Lang::Char(val, _) => {
                (format!("\\'{}\\'", val),
                 context.clone())
            },
            Lang::Bool(b, _) => {
                (format!("{}", b.to_string().to_uppercase()), context.clone())
            },
            Lang::Number(n, _) => {
                (format!("{}", n), context.clone())
            },
            Lang::Integer(i, _) => {
                (format!("{}", i), context.clone())
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
            Lang::FunctionApp(exp, params, _) => {
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
            Lang::Operator(op, e1, e2, _) => {
                (format!("{} {} {}", e1.to_js(context).0, op.to_string(), e2.to_js(context).0),
                context.clone())
            },
            _ => {
                self.to_r(context)
            }
        }
    }

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
            Lang::Variable(_, _, _, h) => {
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

    pub fn to_vec(self) -> Vec<Lang> {
        match self {
            Lang::Lines(v, _) => v,
            l => vec![l]
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
           Lang::Variable(_, _, _, h) => h,
           Lang::Match(_, _, _, h) => h,
           Lang::FunctionApp(_, _, h) => h,
           Lang::VecFunctionApp(_, _, h) => h,
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
            Lang::Variable(name, _bo, typ, _h) 
                => format!("{} -> {}", name, typ),
            _ => format!("{:?}", self)
        };
        write!(f, "{}", res)       
    }
}

pub fn format_backtick(s: String) -> String {
    "`".to_string() + &s.replace("`", "") + "`"
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

