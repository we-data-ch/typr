use nom::IResult;
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::character::complete::multispace0;
use nom::sequence::terminated;
use crate::Type;
use nom::Parser;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;

type Span<'a> = LocatedSpan<&'a str, String>;

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    And,
    Or,
    Eq,
    Add,
    Add2,
    Pipe,
    Dot,
    Pipe2,
    Dot2,
    Union,
    Minus,
    Minus2,
    Mul,
    Mul2,
    In(HelpData),
    At,
    At2,
    Div,
    Div2,
    LesserThan,
    GreaterThan,
    LesserOrEqual,
    GreaterOrEqual,
    Modu,
    Modu2,
    Empty
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
            tag("<"),
            tag(">"),
            tag("and"),
            tag("or"),
            )), multispace0).parse(s)
}

fn get_op(ls: LocatedSpan<&str, String>) -> Op {
    match ls.clone().into_fragment() {
        "in " => Op::In(ls.into()),
        "and" => Op::And,
        "or" => Op::Or,
        "+" => Op::Add,
        "++" => Op::Add2,
        "-" => Op::Minus,
        "--" => Op::Minus2,
        "*" => Op::Mul,
        "**" => Op::Mul2,
        "/" => Op::Div,
        "//" => Op::Div2,
        "@" => Op::At,
        "@@" => Op::At2,
        "%%" => Op::Modu2,
        "%" => Op::Modu,
        "|>" => Op::Pipe,
        "|>>" => Op::Pipe2,
        "." => Op::Dot,
        ".." => Op::Dot2,
        "$" => Op::Dot,
        "$$" => Op::Dot2,
        "|" => Op::Union,
        "==" => Op::Eq,
        "<=" => Op::LesserOrEqual,
        ">=" => Op::GreaterOrEqual,
        "<" => Op::LesserThan,
        ">" => Op::GreaterThan,
        _ => todo!()
    }

}

pub fn op(s: Span) -> IResult<Span, Op> {
    let res = terminated(
        alt((
            bool_op,
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
            tag("|>>"),
            tag("|>"),
            tag(".."),
            tag("."),
            tag("$$"),
            tag("$"),
            tag("|")
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
        Op::And => "and".to_string(),
        Op::Or => "or".to_string(),
        Op::Add => "+".to_string(),
        Op::Add2 => "++".to_string(),
        Op::Minus => "-".to_string(),
        Op::Minus2 => "--".to_string(),
        Op::Mul => "*".to_string(),
        Op::Mul2 => "**".to_string(),
        Op::Div => "/".to_string(),
        Op::Div2 => "//".to_string(),
        Op::At => "@".to_string(),
        Op::At2 => "@@".to_string(),
        Op::Eq => "==".to_string(),
        Op::Pipe => "|>".to_string(),
        Op::Pipe2 => "|>>".to_string(),
        Op::Dot => ".".to_string(),
        Op::Dot2 => "..".to_string(),
        Op::Union => "|".to_string(),
        Op::LesserThan => "<".to_string(),
        Op::GreaterThan => ">".to_string(),
        Op::LesserOrEqual => "<=".to_string(),
        Op::GreaterOrEqual => ">=".to_string(),
        Op::Modu2 => "%%".to_string(),
        Op::Modu => "%".to_string(),
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

