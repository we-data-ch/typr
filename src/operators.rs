use nom::IResult;
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::character::complete::multispace0;
use nom::sequence::terminated;
use crate::Type;
use nom::Parser;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;
use nom::character::complete::char;
use nom::bytes::complete::take_until;
use nom::combinator::recognize;

type Span<'a> = LocatedSpan<&'a str, String>;

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    And(HelpData),
    Or(HelpData),
    Eq(HelpData),
    Eq2(HelpData),
    NotEq(HelpData),
    Add(HelpData),
    Add2(HelpData),
    Pipe(HelpData),
    Dot(HelpData),
    Pipe2(HelpData),
    Dot2(HelpData),
    Union(HelpData),
    Minus(HelpData),
    Minus2(HelpData),
    Mul(HelpData),
    Mul2(HelpData),
    In(HelpData),
    At(HelpData),
    At2(HelpData),
    Div(HelpData),
    Div2(HelpData),
    LesserThan(HelpData),
    GreaterThan(HelpData),
    LesserOrEqual(HelpData),
    GreaterOrEqual(HelpData),
    Modu(HelpData),
    Modu2(HelpData),
    Empty(HelpData),
    Dollar(HelpData),
    Dollar2(HelpData),
    Custom(String, HelpData)
}

impl Op {
    pub fn to_type(&self) -> Option<Type> {
        match self {
            Op::In(h) => Some(Type::In(h.clone())),
            _ => None
        }
    }
}


fn bool_op(s: Span) -> IResult<Span, Span> {
    terminated(
        alt((
            tag("<="),
            tag(">="),
            tag("=="),
            tag("!="),
            tag("<"),
            tag(">"),
            tag("and"),
            tag("or"),
            tag("="),
            )), multispace0).parse(s)
}

fn get_op(ls: LocatedSpan<&str, String>) -> Op {
    match ls.clone().into_fragment() {
        "in " => Op::In(ls.into()),
        "and" => Op::And(ls.into()),
        "or" => Op::Or(ls.into()),
        "+" => Op::Add(ls.into()),
        "++" => Op::Add2(ls.into()),
        "-" => Op::Minus(ls.into()),
        "--" => Op::Minus2(ls.into()),
        "*" => Op::Mul(ls.into()),
        "**" => Op::Mul2(ls.into()),
        "/" => Op::Div(ls.into()),
        "//" => Op::Div2(ls.into()),
        "@" => Op::At(ls.into()),
        "@@" => Op::At2(ls.into()),
        "%%" => Op::Modu2(ls.into()),
        "%" => Op::Modu(ls.into()),
        "|>" => Op::Pipe(ls.into()),
        "|>>" => Op::Pipe2(ls.into()),
        "=" => Op::Eq2(ls.into()),
        "." => Op::Dot(ls.into()),
        ".." => Op::Dot2(ls.into()),
        "$" => Op::Dollar(ls.into()),
        "$$" => Op::Dollar2(ls.into()),
        "|" => Op::Union(ls.into()),
        "==" => Op::Eq(ls.into()),
        "!=" => Op::NotEq(ls.into()),
        "<=" => Op::LesserOrEqual(ls.into()),
        ">=" => Op::GreaterOrEqual(ls.into()),
        "<" => Op::LesserThan(ls.into()),
        ">" => Op::GreaterThan(ls.into()),
        n => Op::Custom(n.to_string(), ls.into())
    }

}

pub fn custom_op(s: Span) -> IResult<Span,Span> {
    recognize((char('%'), take_until("%"), char('%'))).parse(s)
}

fn pipe_op(s: Span) -> IResult<Span, Span> {
        alt(( 
            tag("|>>"),
            tag("|>"),
            tag(".."),
            tag("."),
            tag("$$"),
            tag("$"))).parse(s)
}

pub fn op(s: Span) -> IResult<Span, Op> {
    let res = terminated(
        alt((
            custom_op,
            bool_op,
            pipe_op,
            tag("in "),
            tag("++"),
            tag("+"),
            tag("--"),
            tag("-"),
            tag("@@"),
            tag("@"),
            tag("**"),
            tag("*"),
            tag("//"),
            tag("/"),
            tag("%%"),
            tag("%"),
            tag("|"),
            )),
        multispace0).parse(s);
    match res {
        Ok((s, ls)) => Ok((s, get_op(ls))),
        Err(r) => Err(r),
    }
}

fn get_string(op: &Op) -> String {
    match op {
        Op::In(_) => "in".to_string(),
        Op::And(_) => "and".to_string(),
        Op::Or(_) => "or".to_string(),
        Op::Add(_) => "+".to_string(),
        Op::Add2(_) => "++".to_string(),
        Op::Minus(_) => "-".to_string(),
        Op::Minus2(_) => "--".to_string(),
        Op::Mul(_) => "*".to_string(),
        Op::Mul2(_) => "**".to_string(),
        Op::Div(_) => "/".to_string(),
        Op::Div2(_) => "//".to_string(),
        Op::At(_) => "@".to_string(),
        Op::At2(_) => "@@".to_string(),
        Op::Eq(_) => "==".to_string(),
        Op::Pipe(_) => "|>".to_string(),
        Op::Pipe2(_) => "|>>".to_string(),
        Op::Dot(_) => ".".to_string(),
        Op::Dot2(_) => "..".to_string(),
        Op::Union(_) => "|".to_string(),
        Op::LesserThan(_) => "<".to_string(),
        Op::GreaterThan(_) => ">".to_string(),
        Op::LesserOrEqual(_) => "<=".to_string(),
        Op::GreaterOrEqual(_) => ">=".to_string(),
        Op::Modu2(_) => "%%".to_string(),
        Op::Modu(_) => "%".to_string(),
        Op::Dollar(_) => "$".to_string(),
        Op::Dollar2(_) => "$$".to_string(),
        _ => todo!()
    }
}

use std::fmt;
impl fmt::Display for Op {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = get_string(self);
        write!(f, "{}", res)       
    }
}

