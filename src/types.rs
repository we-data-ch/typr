use nom::IResult;
use nom::character::complete::multispace0;
use nom::sequence::tuple;
use nom::sequence::terminated;
use nom::bytes::complete::tag;
use nom::multi::many0;
use nom::branch::alt;
use nom::combinator::opt;
use nom::character::complete::alpha1;
use crate::argument_type::ArgumentType;
use crate::elements::variable;
use nom::character::complete::one_of;
use nom::sequence::delimited;
use nom::multi::many1;
use nom::sequence::preceded;
use nom::character::complete::digit1;

use crate::elements::scope;
use crate::elements::function_symbol;
use nom::combinator::recognize;
use serde::Serialize;
use crate::operators::op;
use crate::operators::Op;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Type {
    Number,
    Integer,
    Boolean,
    Char,
    Embedded(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Generic(String),
    IndexGen(String),
    Array(Box<Type>, Box<Type>),
    Record(Vec<ArgumentType>),
    Index(u32),
    Alias(String, Vec<Type>, String),
    Tag(String, Box<Type>),
    Union(Vec<Type>),
    Interface(Vec<ArgumentType>),
    Params(Vec<Type>),
    Add(Box<Type>, Box<Type>),
    Minus(Box<Type>, Box<Type>),
    Div(Box<Type>, Box<Type>),
    Mul(Box<Type>, Box<Type>),
    Failed(String),
    Empty
}

impl Type {
    pub fn get_name(self) -> String {
        match self {
            Type::Alias(name, _args, _path) => name.to_string(),
            _ => todo!()
        }
    }
}

fn to_string<T: ToString>(v: &[T]) -> String {
    let res = v.iter()
        .map(|x| x.to_string())
        .reduce(|acc, x| format!("{}, {}", acc, x))
        .unwrap_or("".to_string());
    format!("[{}]", res)
}

use std::fmt;
impl fmt::Display for Type {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Type::Embedded(t) => format!("tembedded({})", t),
            Type::Alias(name, params, path) => format!("var('{}', '{}', public, false, params({}))", name, path, to_string(params)),
            Type::Function(v, t) => format!("tfn([], {}, {})", to_string(v), t) ,
            Type::Generic(g) => format!("gen('{}')", g.to_lowercase()),
            Type::IndexGen(g) => format!("ind('{}')", g.to_lowercase()),
            Type::Array(n, t) => format!("tarray({}, {})", n, t),
            Type::Record(r) => format!("trecord({})", to_string(r)),
            Type::Index(i) => i.to_string(),
            Type::Number => "num".to_string(),
            Type::Integer => "int".to_string(),
            Type::Boolean => "bool".to_string(),
            Type::Char => "chars".to_string(),
            Type::Tag(s, t) => format!("ttag('{}', {})", s, t),
            Type::Union(v) => format!("union({})", to_string(v)),
            Type::Interface(v) => format!("interface({})", to_string(v)),
            Type::Params(v) => format!("params({})", to_string(v)),
            Type::Add(id1, id2) => format!("add({}, {})", id1, id2),
            Type::Minus(id1, id2) => format!("minus({}, {})", id1, id2),
            Type::Mul(id1, id2) => format!("mul({}, {})", id1, id2),
            Type::Div(id1, id2) => format!("division({}, {})", id1, id2),
            Type::Empty => "any".to_string(),
            _ => "".to_string()
        };
        write!(f, "{}", res)       
    }
}


fn ltype_arg(s: &str) -> IResult<&str, Type> {
    let res = tuple((ltype, terminated(opt(tag(",")), multispace0)))(s);
    match res {
        Ok((s, (t, _))) => Ok((s, t)),
        Err(r) => Err(r)
    }
}

fn function_type(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            terminated(tag("("), multispace0),
            many0(ltype_arg),
            terminated(tag(")"), multispace0),
            terminated(tag("->"), multispace0),
            terminated(ltype, multispace0)
          ))(s);
    match res {
        Ok((s, (_, v, _, _, t))) => Ok((s, Type::Function(v.clone(), Box::new(t)))),
        Err(r) => Err(r)
    }
}


fn upper_case_generic(s: &str) -> IResult<&str, String> {
    let res = terminated(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), multispace0)(s);
    match res {
        Ok((s, g)) => Ok((s, g.to_string())),
        Err(r) => Err(r)
    }
}

fn self_tag(s: &str) -> IResult<&str, String> {
        let res = terminated(tag("Self"), multispace0)(s);
        match res {
            Ok((s, st)) => Ok((s, st.to_string())),
            Err(r) => Err(r)
        }
}

fn generic(s: &str) -> IResult<&str, Type> {
    let res = alt((
            upper_case_generic,
            self_tag
                  ))(s);
    match res {
        Ok((s, g)) => Ok((s, Type::Generic(g))),
        Err(r) => Err(r)
    }
}

fn index(s: &str) -> IResult<&str, Type> {
    let res = terminated(digit1, multispace0)(s);
    match res {
        Ok((s, fl)) => Ok((s, Type::Index(fl.parse::<u32>().unwrap()))),
        Err(r) => Err(r)
    }
}

fn array_type(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            terminated(tag("["), multispace0),
            indices,
            terminated(tag(","), multispace0),
            ltype,
            terminated(tag("]"), multispace0),
                  ))(s);

    match res {
        Ok((s, (_, num, _, typ, _))) => Ok((s, Type::Array(Box::new(num), Box::new(typ)))),
        Err(r) => Err(r)
    }
}

fn embedded_ltype(s: &str) -> IResult<&str, Type> {
    let res = preceded(tag("*"), ltype)(s);
    match res {
        Ok((s, ty)) => Ok((s, Type::Embedded(Box::new(ty)))),
        Err(r) => Err(r)
    }
}


pub fn argument(s: &str) -> IResult<&str, ArgumentType> {
    let res = tuple((
        terminated(alpha1, multispace0),
        terminated(tag(":"), multispace0),
        alt((ltype, embedded_ltype)),
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (e1, _, Type::Embedded(ty), _))) => Ok((s, ArgumentType(e1.to_string(), *ty.clone(), true))),
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentType(e1.to_string(), e2, false))),
        Err(r) => Err(r)
    }
}

pub fn argument2(s: &str) -> IResult<&str, Type> {
    let res = tuple((
        terminated(alpha1, multispace0),
        terminated(tag(":"), multispace0),
        alt((ltype, embedded_ltype)),
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (_, _, e, _))) => Ok((s, e)),
        Err(r) => Err(r)
    }
}

fn record_type(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            terminated(tag("{"), multispace0),
            many0(argument),
            terminated(tag("}"), multispace0)
                    ))(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, Type::Record(v.clone()))),
        Err(r) => Err(r)
    }
}

fn number(s: &str) -> IResult<&str, Type> {
    let res = terminated(tag("num"), multispace0)(s);
    match res {
        Ok((s, _)) => Ok((s, Type::Number)),
        Err(r) => Err(r)
    }
}

fn boolean(s: &str) -> IResult<&str, Type> {
    let res = terminated(tag("bool"), multispace0)(s);
    match res {
        Ok((s, _)) => Ok((s, Type::Boolean)),
        Err(r) => Err(r)
    }
}

fn ltype_parameter(s: &str) -> IResult<&str, Type> {
    alt((
    terminated(terminated(ltype, tag(",")), multispace0),
    ltype
        ))(s)
}

fn type_params(s: &str) -> IResult<&str, Vec<Type>> {
    let res = tuple((
        terminated(tag("<"), multispace0),
        many0(ltype_parameter),
        terminated(tag(">"), multispace0)
                    ))(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, v.clone())),
        Err(r) => Err(r)
    }
}

pub fn pascal_case(s: &str) -> IResult<&str, String> {
    let res = terminated(tuple((
            one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
            alpha1)), multispace0)(s);
    match res {
        Ok((s, (t1, t2))) => Ok((s, format!("{}{}", t1, t2))),
        Err(r) => Err(r)
    }
}

fn module_path(s: &str) -> IResult<&str, String> {
    let res = many1(terminated(pascal_case, tag("::")))(s);
    match res {
        Ok((s, v)) => Ok((s, v.join("/"))),
        Err(r) => Err(r)
    }
}

pub fn type_alias(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            opt(module_path),
            pascal_case,
            opt(type_params)
          ))(s);
    match res {
        Ok((s, (Some(p), name, Some(v)))) => Ok((s, Type::Alias(name, v.clone(), p))),
        Ok((s, (None, name, Some(v)))) => Ok((s, Type::Alias(name, v.clone(), "empty".to_string()))),
        Ok((s, (Some(p), name, None))) => Ok((s, Type::Alias(name, vec![], p))),
        Ok((s, (None, name, None))) => Ok((s, Type::Alias(name, vec![], "empty".to_string()))),
        Err(r) => Err(r),
    }
}

fn parenthese_value(s: &str) -> IResult<&str, Type> {
    delimited(
            terminated(tag("("), multispace0),
            alt((embedded_ltype, ltype)),
            terminated(tag(")"), multispace0)
          )(s)
}

fn tag_default(s: &str) -> IResult<&str, Type> {
    let res = terminated(tuple((
            tag("."),
            pascal_case,
            opt(parenthese_value))), multispace0)(s);
    match res {
        Ok((s, (_, n, Some(val)))) => Ok((s, Type::Tag(n, Box::new(val)))),
        Ok((s, (_, n, None))) => Ok((s, Type::Tag(n, Box::new(Type::Empty)))),
        Err(r) => Err(r),
    }
}

fn tag_from_primitive(s: &str) -> IResult<&str, Type> {
    let res = alt((
                tag("bool"),
                tag("num"),
                tag("char"),
                  ))(s);
    match res {
        Ok((s, "bool")) => Ok((s, Type::Tag("Bool".to_string(), Box::new(Type::Boolean)))),
        Ok((s, "num")) => Ok((s, Type::Tag("Num".to_string(), Box::new(Type::Number)))),
        Ok((s, "char")) => Ok((s, Type::Tag("Char".to_string(), Box::new(Type::Char)))),
        Ok((_, _)) => todo!(),
        Err(r) => Err(r)
    }
}

fn tag_exp(s: &str) -> IResult<&str, Type> {
    alt((tag_default, tag_from_primitive))(s)
}

fn tags(s: &str) -> IResult<&str, Type> {
    let res = tuple((
        tag_exp,
        terminated(tag("|"), multispace0)
          ))(s);
    match res {
        Ok((s, (t, _))) => Ok((s, t)),
        Err(r) => Err(r)
    }
}


fn union(s: &str) -> IResult<&str, Type> {
    let res = tuple((
           alt((tags, tag_exp)),
           many0(alt((tags, tag_exp)))))(s);
   match res {
       Ok((s, (t, v))) if v.len() == 0 => Ok((s, t.clone())),
       Ok((s, (t, v))) => Ok((s, Type::Union([t].iter().chain(v.iter()).cloned().collect()))),
       Err(r) => Err(r)
   }
}

fn chars(s: &str) -> IResult<&str, Type> {
    let res = tag("chars")(s);
    match res {
        Ok((s, _st)) => Ok((s, Type::Char)),
        Err(r) => Err(r)
    }
}

fn pseudo_function_signature(s: &str) -> IResult<&str, Type> {
    let res = tuple((
        terminated(function_symbol, multispace0),
        terminated(tag("("), multispace0),
        many0(argument2),
        terminated(tag(")"), multispace0),
        terminated(tag(":"), multispace0),
        terminated(ltype, multispace0)
          ))(s);
    match res {
        Ok((s, (_, _, args, _, _, typ))) => 
            Ok((s, Type::Function(args, Box::new(typ)))),
        Err(r) => Err(r)
    }
}

fn interface_simple_function(s: &str) -> IResult<&str, Type> {
    let res = tuple((
        terminated(function_symbol, multispace0),
        terminated(tag("("), multispace0),
        many0(argument2),
        terminated(tag(")"), multispace0),
        terminated(tag(":"), multispace0),
        terminated(ltype, multispace0),
        scope
          ))(s);
    match res {
        Ok((s, (_fn, _par1, vt, _par2, _dp, ty, _body))) 
            => Ok((s, Type::Function(vt, Box::new(ty)))),
        Err(r) => Err(r)
    }
}

fn interface_function(s: &str) -> IResult<&str, ArgumentType> {
    let res = tuple((
        terminated(recognize(variable), multispace0),
        terminated(tag(":"), multispace0),
        alt((pseudo_function_signature, interface_simple_function)),
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (e, _, f, _))) => 
            Ok((s, ArgumentType(e.to_string(), f, false))),
        Err(r) => Err(r)
    }
}

fn interface(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            terminated(tag("interface"), multispace0),
            terminated(tag("{"), multispace0),
            terminated(many1(interface_function), multispace0),
            terminated(tag("}"), multispace0)
                    ))(s);
    match res {
        Ok((s, (_, _, v, _))) => Ok((s, Type::Interface(v))),
        Err(r) => Err(r)
    }
}

fn tuple_type(s: &str) -> IResult<&str, Type> {
    let res = tuple((
                tag("("),
                many0(ltype_parameter),
                tag(")")))(s);
    match res {
        Ok((s, (_op, v, _cl))) => {
            let v_final = v.iter().enumerate()
                    .map(|(id, x)| ArgumentType(id.to_string(), x.clone(), false))
                    .collect::<Vec<_>>();
            Ok((s, Type::Record(v_final)))
        },
        Err(r) => Err(r)
    }
}

fn index_generic(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            tag("#"),
            generic))(s);
    match res {
        Ok((s, (_tag, Type::Generic(gen)))) 
            => Ok((s, Type::IndexGen(gen))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn integer(s: &str) -> IResult<&str, Type> {
    let res = terminated(tag("int"), multispace0)(s);
    match res {
        Ok((s, _)) => Ok((s, Type::Integer)),
        Err(r) => Err(r)
    }
}

fn reverse_type_operators(v: &mut Vec<(Option<Op>, Type)>) -> Type {
    let first = v.pop().unwrap();
    match first {
        (Some(Op::Add), t) 
            => Type::Add(Box::new(reverse_type_operators(v)), Box::new(t)),
        (Some(Op::Minus), t)
            => Type::Minus(Box::new(reverse_type_operators(v)) , Box::new(t)),
        (Some(Op::Mul), t)
            => Type::Mul(Box::new(reverse_type_operators(v)) , Box::new(t)),
        (Some(Op::Div), t)
            => Type::Div(Box::new(reverse_type_operators(v)), Box::new(t)),
        (None, t) => t,
        _ => todo!()
    }
}

fn indices_chain(s: &str) -> IResult<&str, Type> {
    let res = many1(tuple((opt(op), single_index)))(s);
    match res {
        Ok((s, v)) => Ok((s, reverse_type_operators(&mut v.clone()))),
        Err(r) => Err(r)
    }
}

fn single_index(s: &str) -> IResult<&str, Type> {
    alt((
            index,
            index_generic
        ))(s)
}

fn indices(s: &str) -> IResult<&str, Type> {
    alt((
            indices_chain,
            single_index
                  ))(s)
}

//ltype to not use the reserved symbol "type"
// main
pub fn ltype(s: &str) -> IResult<&str, Type> {
    terminated(alt((
            interface,
            indices,
            number,
            integer,
            boolean,
            chars,
            union,
            type_alias,
            generic,
            array_type,
            function_type,
            tuple_type,
            record_type,
            )), multispace0)(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_type() {
        let res = function_type("(): number").unwrap().1;
        assert_eq!(res.to_string(), "tfn([], var(number))");
    }

    #[test]
    fn test_function_type2() {
        let res = function_type("(number) -> number").unwrap().1;
        assert_eq!(res.to_string(), "tfn([var('number')], var('number'))");
    }

    #[test]
    fn test_function_type3() {
        let res = function_type("(number, number) -> number").unwrap().1;
        assert_eq!(res.to_string(), "tfn([var('number'), var('number')], var('number'))");
    }

    #[test]
    fn test_generic() {
        let res = generic("A").unwrap().1;
        assert_eq!(res.to_string(), "gen('a')");
    }

    #[test]
    fn test_ltype_gen() {
        let res = ltype("A").unwrap().1;
        assert_eq!(res.to_string(), "gen('a')");
    }

    #[test]
    fn test_array_gen() {
        let res = array_type("[N, T]").unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), gen('t'))");
    }

    #[test]
    fn test_ltype_array_gen() {
        let res = ltype("[N, T]").unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), gen('t'))");
    }

    #[test]
    fn test_ltype_array() {
        let res = ltype("[3, num]").unwrap().1;
        assert_eq!(res.to_string(), "tarray(3, var('num'))");
    }

    #[test]
    fn test_type_alias1() {
        let res = type_alias("One").unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('One'), [])");
    }

    #[test]
    fn test_type_alias2() {
        let res = type_alias("Val<num>").unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Val'), [num])");
    }

    #[test]
    fn test_type_alias3() {
        let res = type_alias("Complex<num, num>").unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Complex'), [num, num])");
    }

    #[test]
    fn test_type_alias4() {
        let res = ltype("Complex<num, num>").unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Complex'), [num, num])");
    }

    #[test]
    fn test_union1() {
        let res = union("Rouge | Vert | Orange").unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty), ttag('Orange', empty)])");
    }

    #[test]
    fn test_union2() {
        let res = union("Rouge | Vert").unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty)])");
    }

    #[test]
    fn test_union3() {
        let res = ltype("Rouge | Vert").unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty)])");
    }

    #[test]
    fn test_union4() {
        let res = ltype("Rouge(num)").unwrap().1;
        assert_eq!(res.to_string(), "ttag('Rouge', num)");
    }

    #[test]
    fn test_embedded1() {
        let res = embedded_ltype("*num").unwrap().1;
        assert_eq!(res.to_string(), "tembedded(num)");
    }

    #[test]
    fn test_record1() {
        let res = record_type("{x: *num, y: num}").unwrap().1;
        assert_eq!(res.to_string(), "trecord([[var('x'),tembedded(num)], [var('y'),num]])");
    }

    #[test]
    fn test_interface1() {
        let res = interface("interface { hey: fn(a: num, b: num): num }").unwrap().1;
        assert_eq!(res.to_string(), "interface([[var('hey'),tfn([], [num, num], num)]])");
    }

    #[test]
    fn test_tuple_type1() {
        let res = tuple_type("(num, num, num)").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_alias_type1() {
        let res = ltype("Option<T>").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_alias_type2() {
        let res = ltype("Option").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_index_generic() {
        let res = ltype("#N").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op1() {
        let res = ltype("3+4+5").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op2() {
        let res = reverse_type_operators(&mut vec![
                                         (None, Type::Index(93)),
                                         (Some(Op::Add), Type::Index(8)),
                                         (Some(Op::Div), Type::Index(100))
        ]);
        assert_eq!(res, Type::Empty);
    }

    #[test]
    fn test_type_op3() {
        let res = ltype("#I + #J").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op4() {
        let res = ltype("[#I + #I, int]").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

}
