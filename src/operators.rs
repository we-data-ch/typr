use nom::IResult;
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::character::complete::multispace0;
use nom::sequence::terminated;

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
    At,
    At2,
    Div,
    Div2,
    Empty
}

pub fn op(s: &str) -> IResult<&str, Op> {
    let res = terminated(
        alt((
            tag("and"),
            tag("or"),
            tag("++"),
            tag("+"),
            tag("--"),
            tag("-"),
            tag("@"),
            tag("@@"),
            tag("**"),
            tag("*"),
            tag("//"),
            tag("/"),
            tag("=="),
            tag("|>>"),
            tag("|>"),
            tag(".."),
            tag("."),
            tag("|")
            )),
        multispace0)(s);
    match res {
        Ok((s, "and")) => Ok((s, Op::And)),
        Ok((s, "or")) => Ok((s, Op::Or)),
        Ok((s, "+")) => Ok((s, Op::Add)),
        Ok((s, "++")) => Ok((s, Op::Add2)),
        Ok((s, "-")) => Ok((s, Op::Minus)),
        Ok((s, "--")) => Ok((s, Op::Minus2)),
        Ok((s, "*")) => Ok((s, Op::Mul)),
        Ok((s, "**")) => Ok((s, Op::Mul2)),
        Ok((s, "/")) => Ok((s, Op::Div)),
        Ok((s, "//")) => Ok((s, Op::Div2)),
        Ok((s, "@")) => Ok((s, Op::At)),
        Ok((s, "@@")) => Ok((s, Op::At2)),
        Ok((s, "==")) => Ok((s, Op::Eq)),
        Ok((s, "|>")) => Ok((s, Op::Pipe)),
        Ok((s, "|>>")) => Ok((s, Op::Pipe2)),
        Ok((s, ".")) => Ok((s, Op::Dot)),
        Ok((s, "..")) => Ok((s, Op::Dot2)),
        Ok((s, "|")) => Ok((s, Op::Union)),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn get_string(op: &Op) -> String {
    match op {
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

