#![allow(dead_code)]
use crate::r#type::Type;
use crate::var::Var;
use crate::var::Permission;
use serde::Serialize;
use crate::argument_type::ArgumentType;
use crate::argument_value::ArgumentValue;
use crate::argument_kind::ArgumentKind;
use crate::type_checker;
use crate::Context;
use crate::nominal_context::TypeCategory;
use crate::typing;
use crate::unification;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Lang {
    Bool(bool),
    Char(String),
    And(Box<Lang>, Box<Lang>),
    Or(Box<Lang>, Box<Lang>),
    Union(Box<Lang>, Box<Lang>),
    Number(f32),
    Integer(i32),
    In(Box<Lang>, Box<Lang>),
    Add(Box<Lang>, Box<Lang>),
    Eq(Box<Lang>, Box<Lang>),
    Modu(Box<Lang>, Box<Lang>),
    Modu2(Box<Lang>, Box<Lang>),
    LesserThan(Box<Lang>, Box<Lang>),
    GreaterThan(Box<Lang>, Box<Lang>),
    LesserOrEqual(Box<Lang>, Box<Lang>),
    GreaterOrEqual(Box<Lang>, Box<Lang>),
    Pipe(Box<Lang>, Box<Lang>),
    Dot(Box<Lang>, Box<Lang>),
    Scope(Vec<Lang>),
    Function(Vec<ArgumentKind>, Vec<ArgumentType>, Type, Box<Lang>),
    Module(String, Vec<Lang>),
    ModuleDecl(String),
    Variable(String, String, Permission, bool, Type),
    FunctionApp(Box<Lang>, Vec<Lang>),
    ArrayIndexing(Box<Lang>, f32),
    Let(Var, Type, Box<Lang>),
    Array(Vec<Lang>),
    Record(Vec<ArgumentValue>),
    Alias(Var, Vec<Type>, Type),
    Tag(String, Box<Lang>),
    If(Box::<Lang>, Box<Lang>, Box<Lang>),
    Match(Box<Lang>, Vec<(Box<Lang>, Box<Lang>)>),
    Tuple(Vec<Lang>),
    Sequence(Vec<Lang>),
    Assign(Box<Lang>, Box<Lang>),
    Comment(String),
    Range(i32, i32, i32),
    ModImp(String),
    Import(Type), // type alias
    Header(Box<Lang>),
    GenFunc(String),
    Test(Vec<Lang>),
    Return(Box<Lang>),
    Any,
    Empty
}

impl From<Var> for Lang {
   fn from(val: Var) -> Self {
       Lang::Variable(val.0, val.1, val.2, val.3, val.4)
   } 
}

fn my_to_str<T: ToString>(v: &[T]) -> String {
    let res = v.iter()
        .map(|x| x.to_string())
        .reduce(|acc, x| format!("{}, {}", acc, x))
        .unwrap_or("".to_string());
    format!("[{}]", res)
}

pub fn build_generic_function(s: &str) -> String {
    format!("{} <- function(x, ...) {{\n\tUseMethod('{}')\n}}", s, s)
}

impl Lang {
    fn set_type(&self, typ: &Type) -> Lang {
        match self {
            Lang::Variable(name, path, perm, spec, _) 
                => Lang::Variable(name.clone(), path.clone(), perm.clone(), spec.clone(), typ.clone()),
            _ => self.clone()
        }
    }
    pub fn shape(&self) -> Vec<usize> {
        match self {
            Lang::Array(vec) => {
                let dimensions = vec.len(); // Taille actuelle de ce niveau
                if let Some(first) = vec.get(0) {
                    if let Lang::Array(_) = first {
                        // Descend récursivement dans la première sous-structure
                        let mut sub_shape = first.shape();
                        sub_shape.insert(0, dimensions);
                        sub_shape
                    } else {
                        // Si ce niveau contient des valeurs uniquement
                        vec![dimensions]
                    }
                } else {
                    vec![0] // Array vide
                }
            }
            _ => vec![], // Retourne une forme vide si ce n'est pas un tableau
        }
    }

    fn format_path(path: &str) -> String {
        match path {
            "" => "".to_string(),
            pat => pat.replace("/", "$") + "$"
        }
    }

    pub fn to_r(&self, cont: &Context) -> (String, Context) {
        let result = match self {
            Lang::Bool(b) => 
                (format!("{}", b.to_string().to_uppercase()), cont.clone()),
            Lang::In(b1, b2) => {
                let (b1_str, cont1) = b1.to_r(cont);
                let (b2_str, cont2) = b2.to_r(&cont1);
                (format!("{} %in% {}", b2_str, b1_str), cont2)
            },
            Lang::And(b1, b2) => {
                let (b1_str, cont1) = b1.to_r(cont);
                let (b2_str, cont2) = b2.to_r(&cont1);
                (format!("{} & {}", b1_str, b2_str), cont2)
            },
            Lang::Or(b1, b2) => {
                let (b1_str, cont1) = b1.to_r(cont);
                let (b2_str, cont2) = b2.to_r(&cont1);
                (format!("{} | {}", b1_str, b2_str), cont2)
            },
            Lang::Modu(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                (format!("{} % {}", e2_str, e1_str), cont2)
            },
            Lang::Modu2(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                (format!("{} %% {}", e2_str, e1_str), cont2)
            },
            Lang::Number(n) => 
                (format!("{}", n), cont.clone()),
            Lang::Add(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                (format!("add({}, {})", e1_str, e2_str), cont2)
            },
            Lang::Eq(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                (format!("{} == {}", e2_str, e1_str), cont2)
            },
            Lang::LesserThan(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                (format!("{} < {}", e2_str, e1_str), cont2)
            },
            Lang::GreaterThan(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                (format!("{} > {}", e2_str, e1_str), cont2)
            },
            Lang::LesserOrEqual(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                (format!("{} <= {}", e2_str, e1_str), cont2)
            },
            Lang::GreaterOrEqual(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                (format!("{} >= {}", e2_str, e1_str), cont2)
            },
            Lang::Dot(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                match *e1.clone() {
                    Lang::Variable(_, _, _, _, _) => 
                        (format!("{}${}", e2_str, e1_str), cont2),
                    _ => (format!("{} |> {}", e2_str, e1_str), cont2),
                }
            },
            Lang::Pipe(e1, e2) => {
                let (e1_str, cont1) = e1.to_r(cont);
                let (e2_str, cont2) = e2.to_r(&cont1);
                match *e1.clone() {
                    Lang::Variable(_, _, _, _, _) => 
                        (format!("{}${}", e2_str, e1_str), cont2),
                    _ => (format!("{} |> {}", e2_str, e1_str), cont2),
                }
            },
            Lang::Scope(exps) => {
                let mut current_cont = cont.clone();
                let mut results = Vec::new();
                
                for exp in exps {
                    let (exp_str, new_cont) = exp.to_r(&current_cont);
                    results.push(exp_str);
                    current_cont = new_cont;
                }
                
                (results.join("\n"), current_cont)
            },
            Lang::Function(_args_kind, args, _typ, body) => {
                let (body_str, new_cont) = body.to_r(cont);
                (format!("function({}) {{ {} }}", 
                        args.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", "),
                        body_str), 
                new_cont)
            },
            Lang::Variable(v, path, _perm, _muta, _ty) => 
                match _ty {
                    Type::Empty | Type::Any => 
                        (Self::format_path(path) + v, cont.clone()),
                    _ => {
                        let class = cont.get_class(_ty);
                        (Self::format_path(path) + v + "." + &class, cont.clone())
                    }
                }
            Lang::FunctionApp(exp, vals) => {
                let (exp_str, cont1) = exp.to_r(cont);
                let (unification_map, cont2) = cont1.pop_unifications();
                let exp_typ = typing(cont, exp).0;
                let new_exp_typ = unification::type_substitution(&exp_typ, &unification_map.unwrap_or(vec![]));

                let new_vals = match new_exp_typ {
                    Type::Function(_, args, _) => {
                        vals.into_iter().zip(args.into_iter())
                            .map(|(val, arg)| {
                                match arg {
                                    Type::Function(_, args2, _) 
                                        if args2.len() > 0
                                        => val.set_type(&args2[0]),
                                    _ => val.clone()
                                }
                            }).collect::<Vec<_>>()
                        },
                    _ => vals.clone()
                };
                
                let mut current_cont = cont1;
                let mut val_strs = Vec::new();

                for val in new_vals {
                    let (val_str, new_cont) = val.to_r(&current_cont);
                    val_strs.push(val_str);
                    current_cont = new_cont;
                }
                
                let args = val_strs.join(", ");
                
                match *exp.clone() {
                    Lang::Variable(var, _path, _perm, _spec, _typ) => {
                        (format!("{}({})", var.replace("__", "."), args), current_cont)
                    },
                    _ => (format!("{}({})", exp_str, args), current_cont)
                }
            },
            Lang::ArrayIndexing(exp, val) => {
                let (exp_str, new_cont) = exp.to_r(cont);
                (format!("{}[{}]", exp_str, val), new_cont)
            },
            Lang::GenFunc(func) => 
                (func.to_string(), cont.clone()),
            Lang::Let(var, _s2, body) => {
                let new_path = Self::format_path(&var.get_path());
                let (body_str, new_cont) = body.to_r(cont);
                let new_name = new_path + &var.get_name();
                
                match **body {
                    Lang::Function(_, _, _, _) => {
                        let related_type = var.get_type();
                        let class = cont.get_class(&related_type);
                        if class != "Empty" {
                            (format!("{}.{} <- {}", new_name, class, body_str), new_cont)
                        } else {
                            (format!("{} <- {}", new_name, body_str), new_cont)
                        }
                    }
                    _ => (format!("{} <- {}", new_name, body_str), new_cont)
                }
            },
            Lang::Array(v) => {
                let mut current_cont = cont.clone();
                let mut val_strs = Vec::new();
                
                for val in v {
                    let (val_str, new_cont) = val.to_r(&current_cont);
                    val_strs.push(val_str);
                    current_cont = new_cont;
                }
                
                let vector = format!("c({})", val_strs.join(","));
                let shape = Lang::Array(v.to_vec()).shape();
                
                (format!("array({}, dim = c({}))",
                    vector,
                    shape.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")),
                current_cont)
            },
            Lang::Record(args) => {
                let mut current_cont = cont.clone();
                let mut arg_strs = Vec::new();
                
                for arg in args {
                    let (arg_str, new_cont) = (arg.to_r(&current_cont), cont.clone());
                    arg_strs.push(arg_str);
                    current_cont = new_cont;
                }
                
                let body = arg_strs.join(", ");
                let typ = type_checker::typing(cont, self).0;
                let class = cont.get_class(&typ);
                
                match cont.get_classes(&typ) {
                    Some(res) => (format!("structure(list({}), class = c('Record', '{}', {}))", body, class, res), current_cont),
                    _ => (format!("structure(list({}), class = c('Record', '{}'))", body, class), current_cont)
                }
            },
            Lang::Char(s) => 
                ("'".to_string() + s + "'", cont.clone()),
            Lang::If(cond, exp, els) if els == &Box::new(Lang::Empty) => {
                let (cond_str, cont1) = cond.to_r(cont);
                let (exp_str, cont2) = exp.to_r(&cont1);
                
                (format!("if({}) {{\n {} \n}}", cond_str, exp_str), cont2)
            },
            Lang::If(cond, exp, els) => {
                let (cond_str, cont1) = cond.to_r(cont);
                let (exp_str, cont2) = exp.to_r(&cont1);
                let (els_str, cont3) = els.to_r(&cont2);
                
                (format!("if ({}) {{\n {} \n}} else {}", cond_str, exp_str, els_str), cont3)
            },
            Lang::Tuple(vals) => {
                let mut current_cont = cont.clone();
                let mut val_entries = Vec::new();
                
                for (i, val) in vals.iter().enumerate() {
                    let (val_str, new_cont) = val.to_r(&current_cont);
                    val_entries.push(format!("'{}' = {}", i.to_string(), val_str));
                    current_cont = new_cont;
                }
                
                (format!("structure(list({}), class = 'Tuple')", val_entries.join(", ")), current_cont)
            },
            Lang::Assign(var, exp) => {
                let (var_str, cont1) = var.to_r(cont);
                let (exp_str, cont2) = exp.to_r(&cont1);
                
                (format!("{} <- {}", var_str, exp_str), cont2)
            },
            Lang::Comment(txt) => 
                ("# ".to_string() + txt, cont.clone()),
            Lang::Range(i1, i2, i0) => 
                (format!("array(seq({},{},{}), dim = c({}))", i1, i2, i0, i2-i1/i0), cont.clone()),
            Lang::Integer(i) => 
                (i.to_string(), cont.clone()),
            Lang::Tag(s, t) => {
                let (t_str, new_cont) = t.to_r(cont);
                let typ = type_checker::typing(cont, self).0;
                let class = cont.get_class(&typ);
                
                match cont.get_classes(&typ) {
                    Some(res) => 
                        (format!("structure(list('{}', {}), class = c('Tag', '{}', {}))", s, t_str, class, res), new_cont),
                    _ => (format!("structure(list('{}', {}), class = c('Tag', '{}'))", s, t_str, class), new_cont)
                }
            },
            Lang::Empty => 
                ("NA".to_string(), cont.clone()),
            Lang::ModuleDecl(name) => 
                (format!("{} <- new.env()", name), cont.clone()),
            Lang::Sequence(exps) => {
                let mut current_cont = cont.clone();
                let mut results = Vec::new();
                
                for exp in exps {
                    let (exp_str, new_cont) = exp.to_r(&current_cont);
                    results.push(exp_str);
                    current_cont = new_cont;
                }
                
                (results.join("\n\n"), current_cont)
            },
            Lang::Return(exp) => {
                let (exp_str, new_cont) = exp.to_r(cont);
                (format!("return ({})", exp_str), new_cont)
            },
            _ => ("".to_string(), cont.clone())
        };
        
        result
    }

    pub fn to_typescript(&self, cont: &Context) -> String {
        match self {
            Lang::Bool(b) => format!("{}", b),
            Lang::Integer(i) => format!("{}", i),
            Lang::Number(n) => format!("{}", n),
            Lang::Char(c) => format!("\"{}\"", c),
            Lang::Empty => "null".to_string(),
            Lang::Array(v) => {
                let res = v.iter().map(|lang| lang.to_typescript(cont)).collect::<Vec<_>>();
                format!("[{}]", res.join(", "))
            },
            Lang::Record(v) => {
                let res = v.iter().map(|arg_val| format!("{}: {}", 
                                               arg_val.get_argument(), 
                                               arg_val.get_value().to_typescript(cont)))
                    .collect::<Vec<_>>();
                format!("{{ {} }}", res.join(", "))
            },
            Lang::Tuple(vals) => format!("{{ {} }}", vals.iter()
                                         .enumerate()
                                         .map(|(i, x)| format!("'{}': {}", i.to_string(), x.to_typescript(cont)))
                                         .collect::<Vec<_>>().join(", ")),
            Lang::Tag(s, t) => {
                let typ = type_checker::typing(cont, self).0;
                format!("{{ _type: '{}', _body: {} }}", s, t.to_typescript(cont))
            },
            Lang::Variable(v, path, _perm, _muta, _ty) => Self::format_path(path) + v,
            Lang::Let(var, typ, body) => {
                if var.get_name() == "main" {
                    match *body.clone() {
                        Lang::Function(_kinds, params, ret, body2) => {
                            format!("export function main(): void {{\n{}\n}}", body2.to_typescript(cont))
                        },
                        _ => todo!()
                    }
                } else {
                   match *body.clone() {
                       Lang::Function(_kinds, params, ret, body2) => {
                           let first = params.iter().nth(0).unwrap().get_type();
                           let class = cont.get_class(&first);
                           let res = params.iter()
                            .map(|at| format!("{}: {}",
                                              at.get_argument(),
                                              at.get_type().to_typescript()))
                            .collect::<Vec<_>>();
                            format!("function {}_{}({}): {} {{\n{}\n}}",
                            class,
                            var.get_name(), res.join(", "),
                            ret.to_typescript(), body2.to_typescript(cont))
                       },
                       _ => format!("let {} = {}", var.get_name(), body.to_typescript(cont))
                   } 
                }
            },
            Lang::FunctionApp(var, params) => {
                let first = params.iter().nth(0).unwrap();
                let typ = typing(cont, first).0;
                let res = params.iter().map(|x| x.to_typescript(cont)).collect::<Vec<_>>();
                let name = var.get_name();
                match &name[..] {
                    "parseInt" | "parseFloat"
                        => format!("{}({})", var.get_name(), res.join(", ")),
                    n if (name.len() > 6) && (&name[0..6] == "math__") => {
                            format!("Math.{}({})", name[6..].to_string(), res.join(", "))
                        }
                    n if name.contains("__") =>
                        format!("{}({})", name.replace("__", "."), res.join(", ")),
                    _ => format!("{}_{}({})", cont.get_class(&typ), var.get_name(), res.join(", "))
                }
            },
            Lang::Scope(langs) => {
                let res = langs.iter().map(|lang| lang.to_typescript(cont)).collect::<Vec<_>>();
                res.join("\n")
            },
            Lang::Sequence(exps) 
                => exps.iter().map(|x| x.to_typescript(cont)).collect::<Vec<String>>().join("\n\n"),
            Lang::Return(exp) => format!("return {};", exp.to_typescript(cont)),
            Lang::Dot(e1, e2) => {
                match *e1.clone() {
                    Lang::Variable(_, _, _, _, _) => 
                        format!("{}.{}", e2.to_typescript(cont), e1.to_typescript(cont)),
                    _ => format!("{} |> {}", e2.to_typescript(cont), e1.to_typescript(cont)),
                }
            }
            Lang::Pipe(e1, e2) => {
                match *e1.clone() {
                    Lang::Variable(_, _, _, _, _) => 
                        format!("{}.{}", e2.to_typescript(cont), e1.to_typescript(cont)),
                    _ => format!("{} |> {}", e2.to_typescript(cont), e1.to_typescript(cont)),
                }
            }
            Lang::Alias(var, args, typ) => {
                if args.len() > 0 {
                    let res = args.iter()
                        .map(|typ| typ.to_typescript())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("type {}<{}> = {};", var.get_name(), res, typ.to_typescript())
                } else {
                    format!("type {} = {};", var.get_name(), typ.to_typescript())
                }
            }
            _ => "".to_string()
        }
    } 

    pub fn get_number(&self) -> i32 {
        if let Lang::Integer(number) = self {
            number.clone()
        } else { 0 }
    }

    pub fn get_name(&self) -> String {
        if let Lang::Variable(name, _, _, _, _) = self {
            name.to_string()
        } else { "".to_string() }
    }
}

fn typescript_type(s: &str, cont: &Context) -> String {
    match s {
        "integer" => "number".to_string(),
        "number" => "number".to_string(),
        "bool" => "boolean".to_string(),
        x => {
            let typ = cont.get_type_from_class(x);
            match typ {
                Type::Record(body) => {
                    let res = body.iter()
                        .map(|at| at.to_string())
                        .collect::<Vec<_>>()
                        .join(",");
                    format!("{{ {} }}", res)
                },
                _ => format!("check typescript_type for: {}", typ)
            }
        }
    }
}

fn wasm_type(s: &str) -> String {
    match s {
        "integer" => "i32".to_string(),
        x => format!("Check `wasm_type` function for: {}", x)
    }
}
