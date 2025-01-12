#![allow(dead_code)]
use std::fmt;
use crate::types::Type;
use crate::var::Var;
use crate::var::Permission;
use serde::Serialize;
use crate::argument_type::ArgumentType;
use crate::argument_value::ArgumentValue;
use crate::argument_kind::ArgumentKind;

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
    Function(Vec<ArgumentKind>, Vec<ArgumentType>, String, Box<Lang>),
    Module(String, Vec<Lang>),
    ModuleDecl(String),
    Variable(String, String, Permission, bool, Type),
    FunctionApp(Box<Lang>, Vec<Lang>),
    ArrayIndexing(Box<Lang>, f32),
    Let(Var, String, Box<Lang>),
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
    Empty
}

fn my_to_str<T: ToString>(v: &[T]) -> String {
    let res = v.iter()
        .map(|x| x.to_string())
        .reduce(|acc, x| format!("{}, {}", acc, x))
        .unwrap_or("".to_string());
    format!("[{}]", res)
}

//for match exp branches
fn to_string(v: &[(Box<Lang>, Box<Lang>)]) -> String {
    "[".to_string() +
        &v.iter()
        .map(|(p1, p2)| format!("[{}, {}]", p1.to_string(), p2.to_string()))
        .collect::<Vec<_>>()
        .join(",")
    + "]"
}

fn to_records(v: &[Lang]) -> String {
    "[".to_string() + 
    &v.iter()
        .enumerate()
        .map(|(i, x)| format!("[var('{}'), {}]", i, x))
        .collect::<Vec<_>>()
        .join(",")
    + "]"
}

impl fmt::Display for Lang {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Lang::Bool(b) => format!("{}", b),
            Lang::And(e1, e2) => format!("and({}, {})", e1.to_string(), e2.to_string()),
            Lang::Or(e1, e2) => format!("or({}, {})", e1.to_string(), e2.to_string()),
            Lang::Union(e1, e2) => format!("rec_union({}, {})", e1.to_string(), e2.to_string()),
            Lang::Number(n) => format!("{:?}", n),
            Lang::Modu(e1, e2) => format!("modu({}, {})", e1.to_string(), e2.to_string()),
            Lang::Modu2(e1, e2) => format!("modu({}, {})", e1.to_string(), e2.to_string()),
            Lang::Add(e1, e2) => format!("add({}, {})", e1.to_string(), e2.to_string()),
            Lang::Eq(e1, e2) => format!("eq({}, {})", e1.to_string(), e2.to_string()),
            Lang::LesserThan(e1, e2) => format!("lesser_than({}, {})", e1.to_r(), e2.to_r()),
            Lang::GreaterThan(e1, e2) => format!("{} > {}", e1.to_r(), e2.to_r()),
            Lang::LesserOrEqual(e1, e2) => format!("lesser_or_equal({}, {})", e1.to_r(), e2.to_r()),
            Lang::GreaterOrEqual(e1, e2) => format!("{} >= {}", e1.to_r(), e2.to_r()),
            Lang::Pipe(e1, e2) => format!("pipe({}, {})", e1.to_string(), e2.to_string()),
            Lang::Dot(e1, e2) => format!("dot({}, {})", e1.to_string(), e2.to_string()),
            Lang::Scope(exps)
                => {
                let res = exps.iter()
                    .map(|x| x.to_string()).collect::<Vec<String>>();
                format!("sequence([{}])", res.join(","))
                },
            Lang::Module(name, body) => {
               "mod('".to_string() + name + "', [" + 
                   &body.iter()
                   .map(|x| x.to_string())
                   .collect::<Vec<_>>().join(",")
                + "])"
            },
            Lang::Function(args_kind, args, typ, body) => 
                format!("fn({},{},{},{})", my_to_str(args_kind), my_to_str(args), typ, body),
            Lang::Variable(v, path, perm, muta, ty) 
                => Var(v.clone(), path.clone(), perm.clone(), muta.clone(), ty.clone()).to_string(),
            Lang::FunctionApp(exp, vals) => format!("fn_app({}, values({}))", exp, my_to_str(vals)),
            Lang::ArrayIndexing(exp, val) => format!("array_indexing({}, values({}))", exp, val),
            Lang::Let(var, s2, body) 
                => format!("decl({}, {}, {})", var.clone().to_string(), s2, body),
            Lang::Array(v) => format!("array({})", my_to_str(v)),
            Lang::Record(args) => format!("record({})",my_to_str(args)),
            Lang::Alias(var, params, typ) 
                => format!("decl({}, params({}), {})", var.clone().set_permission(true), my_to_str(params), typ.to_string()),
            Lang::Tag(s, t) => format!("tag('{}', {})", s, t),
            Lang::Char(s) => format!("chars('{}')", s),
            Lang::Match(val, branches) => format!("match({}, {})", val, to_string(branches)),
            Lang::If(cond, exp, els) 
                => format!("if({}, {}, {})", cond, exp, els),
            Lang::Tuple(vals) => format!("record({})", to_records(vals)),
            Lang::Assign(var, exp) => format!("assign({}, {})", var, exp),
            Lang::Comment(_) => "comment".to_string(),
            Lang::Range(i1, i2, i0) => format!("seq({},{},{})", i1, i2, i0),
            Lang::Integer(i) => i.to_string(),
            Lang::Import(_typ) => "import".to_string(),
            Lang::Sequence(exps) 
                => {
                let res = exps.iter()
                    .map(|x| x.to_string()).collect::<Vec<String>>();
                format!("{}", res.join(","))
                },
            _ => "empty".to_string()
        };
        write!(f, "{}", res)
    }
}

impl Lang {
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

    pub fn to_r(&self) -> String {
        match self {
            Lang::Bool(b) => format!("structure({}, class = 'bool')", b.to_string().to_uppercase()),
            Lang::In(b1, b2) => format!("{} %in% {}", b2.to_r(), b1.to_r()),
            Lang::And(b1, b2) => format!("{} & {}", b1.to_r(), b2.to_r()),
            Lang::Or(b1, b2) => format!("{} | {}", b1.to_r(), b2.to_r()),
            Lang::Modu(e1, e2) => format!("{} % {}", e2.to_r(), e1.to_r()),
            Lang::Modu2(e1, e2) => format!("{} %% {}", e2.to_r(), e1.to_r()),
            Lang::Number(n) => format!("{}", n),
            Lang::Add(e1, e2) => format!("add({}, {})", e1.to_r(), e2.to_r()),
            Lang::Eq(e1, e2) => format!("{} == {}", e2.to_r(), e1.to_r()),
            Lang::LesserThan(e1, e2) => format!("{} < {}", e2.to_r(), e1.to_r()),
            Lang::GreaterThan(e1, e2) => format!("{} > {}", e2.to_r(), e1.to_r()),
            Lang::LesserOrEqual(e1, e2) => format!("{} <= {}", e2.to_r(), e1.to_r()),
            Lang::GreaterOrEqual(e1, e2) => format!("{} >= {}", e2.to_r(), e1.to_r()),
            Lang::Pipe(e1, e2) => format!("{} |> {}", e2.to_r(), e1.to_r()),
            Lang::Dot(e1, e2) => format!("{} |> {}", e2.to_r(), e1.to_r()),
            Lang::Scope(exps) 
                => exps.iter().map(|x| x.to_r()).collect::<Vec<String>>().join("\n"),
            Lang::Function(_args_kind, args, _typ, body) => 
                format!("function({}) {{ {} }}", 
                        args.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", "),
                        body.to_r()),
            Lang::Variable(v, path, _perm, _muta, _ty) => Self::format_path(path) + v,
            Lang::FunctionApp(exp, vals) if exp.get_name() == "seq"
                => format!("array({}({}), dim = c({}))", exp.to_r(),
                vals.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", "),
                (vals[1].get_number()-vals[0].get_number())/vals[2].get_number()),
            Lang::FunctionApp(exp, vals) 
                => format!("{}({})", exp.to_r(),
                vals.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", ")),
            Lang::ArrayIndexing(exp, val) => format!("{}[{}]", exp, val),
            Lang::Let(var, _s2, body) 
                => format!("{} <- {}", Self::format_path(&var.get_path()) + &var.get_name(), body.to_r()),
            Lang::Array(v) 
                => {
                    let vector = format!("c({})", v.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(","));
                       let shape = Lang::Array(v.to_vec()).shape();
                       format!("array({}, dim = c({}))",
                            vector,
                            shape.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")) 
                } 
            Lang::Record(args) 
                => format!("list({})", args.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", ")),
            Lang::Char(s) => "'".to_string() + s + "'",
            Lang::If(cond, exp, els) 
                if els == &Box::new(Lang::Empty)
                => format!("if({}) {{\n {} \n}}", cond.to_r(), exp.to_r()),
            Lang::If(cond, exp, els) 
                => format!("if ({}) {{\n {} \n}} else {}", cond.to_r(), exp.to_r(), els.to_r()),
            Lang::Tuple(vals) => format!("list({})", vals.iter()
                                         .enumerate()
                                         .map(|(i, x)| format!("'{}' = {}", i.to_string(), x.to_r()))
                                         .collect::<Vec<_>>().join(", ")),
            Lang::Assign(var, exp) => format!("{} <- {}", var.to_r(), exp.to_r()),
            Lang::Comment(txt) => "# ".to_string() + txt,
            Lang::Range(i1, i2, i0) 
                => format!("array(seq({},{},{}), dim = c({}))", i1, i2, i0, i2-i1/i0),
            Lang::Integer(i) => i.to_string(),
            Lang::Tag(s, t) => format!("list('{}', {})", s, t.to_r()),
            Lang::Empty => "NA".to_string(),
            Lang::ModuleDecl(name) => format!("{} <- new.env()", name),
            Lang::Sequence(exps) 
                => exps.iter().map(|x| x.to_r()).collect::<Vec<String>>().join("\n\n"),
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

