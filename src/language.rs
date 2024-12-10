use std::fmt;
use crate::types::Type;
use crate::var::Var;
use crate::var::Permission;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)] // 3 argument is for the embedding
pub struct ArgumentType(pub String, pub Type, pub bool);

impl ArgumentType {
    fn to_r(&self) -> String {
        self.0.clone()
    }
}

impl fmt::Display for ArgumentType {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[var('{}'),{}]", self.0, self.1)       
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ArgumentValue(pub String, pub String);

impl ArgumentValue {
    fn to_r(&self) -> String {
        format!("{} = {}", self.0, self.1)
    }
}

impl fmt::Display for ArgumentValue {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[var('{}'),{}]", self.0, self.1)       
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ArgumentKind(pub String, pub String);

impl fmt::Display for ArgumentKind {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[gen('{}'),{}]", self.0, self.1)       
    }
}


#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Lang {
    Bool(bool),
    Char(String),
    And(Box<Lang>, Box<Lang>),
    Or(Box<Lang>, Box<Lang>),
    Union(Box<Lang>, Box<Lang>),
    Number(f32),
    Integer(i32),
    Add(Box<Lang>, Box<Lang>),
    Eq(Box<Lang>, Box<Lang>),
    Pipe(Box<Lang>, Box<Lang>),
    Dot(Box<Lang>, Box<Lang>),
    Scope(Vec<Lang>),
    Function(Vec<ArgumentKind>, Vec<ArgumentType>, String, Box<Lang>),
    Module(String, Vec<Lang>),
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
            Lang::Add(e1, e2) => format!("add({}, {})", e1.to_string(), e2.to_string()),
            Lang::Eq(e1, e2) => format!("eq({}, {})", e1.to_string(), e2.to_string()),
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
            Lang::Comment(txt) => "comment".to_string(),
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
    pub fn to_r(&self) -> String {
        match self {
            Lang::Bool(b) => b.to_string().to_uppercase(),
            Lang::And(b1, b2) => format!("{} & {}", b1.to_r(), b2.to_r()),
            Lang::Or(b1, b2) => format!("{} | {}", b1.to_r(), b2.to_r()),
            Lang::Number(n) => format!("{}", n),
            Lang::Add(e1, e2) => format!("add({}, {})", e1.to_r(), e2.to_r()),
            Lang::Eq(e1, e2) => format!("{} == {}", e1.to_string(), e2.to_string()),
            Lang::Pipe(e1, e2) => format!("{} |> {}", e1.to_string(), e2.to_string()),
            Lang::Dot(e1, e2) => format!("{} |> {}", e1.to_string(), e2.to_string()),
            Lang::Scope(exps) 
                => exps.iter() .map(|x| x.to_r()).collect::<Vec<String>>().join("\n"),
            Lang::Function(_args_kind, args, _typ, body) => 
                format!("function({}) {{ {} }}", 
                        args.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", "),
                        body.to_r()),
            Lang::Variable(v, _path, _perm, _muta, _ty) => v.to_string(),
            Lang::FunctionApp(exp, vals) 
                => format!("{}({})", exp.to_r(),
                vals.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", ")),
            Lang::ArrayIndexing(exp, val) => format!("{}[{}]", exp, val),
            Lang::Let(var, _s2, body) 
                => format!("{} <- {}", var.get_name(), body.to_r()),
            Lang::Array(v) => format!("c({})", my_to_str(v)),
            Lang::Record(args) 
                => format!("list({})", args.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", ")),
            Lang::Char(s) => format!("\"{}\"", s),
            Lang::If(cond, exp, els) 
                => format!("if({}, {}, {})", cond.to_r(), exp.to_r(), els.to_r()),
            Lang::Tuple(vals) => format!("list({})", vals.iter().map(|x| x.to_r()).collect::<Vec<_>>().join(", ")),
            Lang::Assign(var, exp) => format!("{} <- {}", var, exp),
            Lang::Comment(txt) => "# ".to_string() + txt,
            Lang::Range(i1, i2, i0) => format!("seq({},{},{})", i1, i2, i0),
            Lang::Integer(i) => i.to_string(),
            Lang::Sequence(exps) 
                => exps.iter().map(|x| x.to_r()).collect::<Vec<String>>().join("\n\n"),
            _ => "".to_string()
        }
    }
}
