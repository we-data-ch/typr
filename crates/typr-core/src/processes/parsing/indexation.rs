#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use nom::Parser;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, multispace0},
    combinator::{map, opt, recognize},
    multi::separated_list0,
    sequence::{delimited, pair, preceded},
    IResult,
};

#[derive(Debug, Clone, PartialEq)]
pub enum RIndex {
    /// Indexation simple: x[5]
    Single(i32),

    /// Indexation multiple: x[c(1, 3, 5)]
    Multiple(Vec<i32>),

    /// Séquence: x[1:10]
    Range { start: i32, end: i32 },

    /// Indexation négative: x[-2]
    Negative(i32),

    /// Exclusion multiple: x[-c(1, 3)]
    NegativeMultiple(Vec<i32>),

    /// Tous les éléments: x[]
    All,

    /// Indexation logique simulée: x[TRUE] ou x[FALSE]
    Logical(bool),
}

/// Parse un nombre entier (positif ou négatif)
fn parse_integer(s: &str) -> IResult<&str, i32> {
    let res = recognize(pair(opt(char('-')), digit1)).parse(s);

    match res {
        Ok((s, int)) => Ok((s, int.parse::<i32>().unwrap())),
        Err(r) => Err(r),
    }
}

/// Parse un nombre positif
fn parse_positive_integer(s: &str) -> IResult<&str, i32> {
    match digit1.parse(s) {
        Ok((s, int)) => Ok((s, int.parse::<i32>().unwrap())),
        Err(r) => Err(r),
    }
}

/// Parse une séquence: 1:10
fn parse_range(s: &str) -> IResult<&str, RIndex> {
    let res = (
        parse_integer,
        delimited(multispace0, char(':'), multispace0),
        parse_integer,
    )
        .parse(s);

    match res {
        Ok((s, (start, _, end))) => Ok((s, RIndex::Range { start, end })),
        Err(r) => Err(r),
    }
}

/// Parse un vecteur avec c(): c(1, 2, 3)
fn parse_c_vector(input: &str) -> IResult<&str, Vec<i32>> {
    delimited(
        (char('c'), multispace0, char('('), multispace0),
        separated_list0(
            delimited(multispace0, char(','), multispace0),
            parse_integer,
        ),
        (multispace0, char(')')),
    )
    .parse(input)
}

/// Parse un vecteur négatif: -c(1, 2, 3)
fn parse_negative_vector(input: &str) -> IResult<&str, RIndex> {
    map(
        preceded(char('-'), parse_c_vector),
        RIndex::NegativeMultiple,
    )
    .parse(input)
}

/// Parse un vecteur positif: c(1, 2, 3)
fn parse_positive_vector(input: &str) -> IResult<&str, RIndex> {
    map(parse_c_vector, RIndex::Multiple).parse(input)
}

/// Parse un index négatif simple: -5
fn parse_negative_single(input: &str) -> IResult<&str, RIndex> {
    map(
        preceded(char('-'), parse_positive_integer),
        RIndex::Negative,
    )
    .parse(input)
}

/// Parse un index positif simple: 5
fn parse_positive_single(input: &str) -> IResult<&str, RIndex> {
    map(parse_positive_integer, RIndex::Single).parse(input)
}

/// Parse TRUE ou FALSE
fn parse_logical(input: &str) -> IResult<&str, RIndex> {
    alt((
        map(tag("TRUE"), |_| RIndex::Logical(true)),
        map(tag("FALSE"), |_| RIndex::Logical(false)),
    ))
    .parse(input)
}

/// Parse tous les éléments (crochets vides)
fn parse_all(input: &str) -> IResult<&str, RIndex> {
    map(multispace0, |_| RIndex::All).parse(input)
}

/// Parse le contenu entre crochets
fn parse_bracket_content(input: &str) -> IResult<&str, RIndex> {
    alt((
        parse_logical,
        parse_range,
        parse_negative_vector,
        parse_positive_vector,
        parse_negative_single,
        parse_positive_single,
        parse_all,
    ))
    .parse(input)
}

/// Parse une indexation complète: x[...]
pub fn parse_r_index(input: &str) -> IResult<&str, RIndex> {
    delimited(
        char('['),
        delimited(multispace0, parse_bracket_content, multispace0),
        char(']'),
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_index() {
        assert_eq!(parse_r_index("[5]"), Ok(("", RIndex::Single(5))));
        assert_eq!(parse_r_index("[ 5 ]"), Ok(("", RIndex::Single(5))));
    }

    #[test]
    fn test_multiple_index() {
        assert_eq!(
            parse_r_index("[c(1, 3, 5)]"),
            Ok(("", RIndex::Multiple(vec![1, 3, 5])))
        );
    }

    #[test]
    fn test_range() {
        assert_eq!(
            parse_r_index("[1:10]"),
            Ok(("", RIndex::Range { start: 1, end: 10 }))
        );
        assert_eq!(
            parse_r_index("[-5:5]"),
            Ok(("", RIndex::Range { start: -5, end: 5 }))
        );
    }

    #[test]
    fn test_negative() {
        assert_eq!(parse_r_index("[-2]"), Ok(("", RIndex::Negative(2))));
        assert_eq!(
            parse_r_index("[-c(1, 3)]"),
            Ok(("", RIndex::NegativeMultiple(vec![1, 3])))
        );
    }

    #[test]
    fn test_all() {
        assert_eq!(parse_r_index("[]"), Ok(("", RIndex::All)));
        assert_eq!(parse_r_index("[ ]"), Ok(("", RIndex::All)));
    }

    #[test]
    fn test_logical() {
        assert_eq!(parse_r_index("[TRUE]"), Ok(("", RIndex::Logical(true))));
        assert_eq!(parse_r_index("[FALSE]"), Ok(("", RIndex::Logical(false))));
    }
}
