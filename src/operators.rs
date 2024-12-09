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
    Pipe,
    Dot,
    Pipe2,
    Dot2,
    Union,
    Minus,
    Mul,
    Div,
    Empty
}

pub fn op(s: &str) -> IResult<&str, Op> {
    let res = terminated(
        alt((
            tag("and"),
            tag("or"),
            tag("+"),
            tag("-"),
            tag("*"),
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
        Ok((s, "-")) => Ok((s, Op::Minus)),
        Ok((s, "*")) => Ok((s, Op::Mul)),
        Ok((s, "/")) => Ok((s, Op::Div)),
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
