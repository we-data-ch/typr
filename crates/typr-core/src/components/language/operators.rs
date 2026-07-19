use crate::components::error_message::help_data::HelpData;
use crate::components::language::Lang;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::parsing::operation_priority::TokenKind;
use crate::utils::builder;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_until;
use nom::character::complete::alphanumeric1;
use nom::character::complete::char;
use nom::character::complete::multispace0;
use nom::combinator::not;
use nom::combinator::recognize;
use nom::sequence::terminated;
use nom::IResult;
use nom::Parser;
use nom_locate::LocatedSpan;
use serde::{Deserialize, Serialize};

type Span<'a> = LocatedSpan<&'a str, String>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Op {
    And(HelpData),
    And2(HelpData),
    Eq(HelpData),
    NotEq(HelpData),
    Add(HelpData),
    Pipe(HelpData),
    Dot(HelpData),
    Or(HelpData),
    Or2(HelpData),
    Minus(HelpData),
    Mul(HelpData),
    In(HelpData),
    Div(HelpData),
    LesserThan(HelpData),
    GreaterThan(HelpData),
    LesserOrEqual(HelpData),
    GreaterOrEqual(HelpData),
    Modulo(HelpData),
    Dollar(HelpData),
    Custom(String, HelpData),
    Empty(HelpData),
    AsExcl(HelpData),
}

/// Reinterprets the value-level AST produced for a bracket type expression
/// (`[Any, int]`, `Vec[N, T]`, `Array[N, T]`) appearing as the right-hand side
/// of `as!` as the `Type` it denotes. These shapes parse as plain `Lang`
/// values (`Array`/`ArrayIndexing`/`Variable`) since `as!`'s right operand
/// goes through the ordinary expression grammar, not the type grammar — see
/// `Op::combine`. The `Vec`/`Array` keyword prefix is not preserved in the
/// resulting `VecType` since `Type`'s `PartialEq` ignores it anyway (it only
/// matters for named aliases, which inline literals are not).
fn lang_to_cast_type(lang: &Lang) -> Option<Type> {
    match lang {
        Lang::Variable {
            name, help_data, ..
        } => Some(match name.as_str() {
            "int" => builder::integer_type_default(),
            "num" => builder::number_type(),
            "bool" | "logic" => builder::boolean_type(),
            "char" => builder::character_type_default(),
            "Any" => builder::any_type(),
            _ => Type::Alias(name.clone(), vec![], false, help_data.clone()),
        }),
        Lang::Integer { value, help_data } => {
            Some(Type::Integer(Tint::Val(*value), help_data.clone()))
        }
        Lang::Array { value, help_data } => {
            let (idx, elem) = match value.as_slice() {
                [elem] => (builder::any_type(), elem),
                [idx, elem] => (lang_to_cast_type(idx)?, elem),
                _ => return None,
            };
            Some(Type::Vec(
                VecType::S3,
                Box::new(idx),
                Box::new(lang_to_cast_type(elem)?),
                help_data.clone(),
            ))
        }
        Lang::ArrayIndexing {
            identifier,
            indexing,
            help_data,
        } => {
            match identifier.as_ref() {
                Lang::Variable { name, .. } if name == "Vec" || name == "Array" => {}
                _ => return None,
            }
            let value = match indexing.as_ref() {
                Lang::Array { value, .. } => value,
                _ => return None,
            };
            let (idx, elem) = match value.as_slice() {
                [elem] => (builder::any_type(), elem),
                [idx, elem] => (lang_to_cast_type(idx)?, elem),
                _ => return None,
            };
            Some(Type::Vec(
                VecType::S3,
                Box::new(idx),
                Box::new(lang_to_cast_type(elem)?),
                help_data.clone(),
            ))
        }
        _ => None,
    }
}

/// `int`/`num`/`char`/`bool` as the RHS of `as!`: these are TypR primitive-
/// type keywords, not aliases resolvable through the context — see the call
/// site in `combine()` for why leaving them as a bare `Lang::Variable` name
/// used to reduce to a broken `Type::Alias`.
fn primitive_cast_type(name: &str) -> Option<Type> {
    match name {
        "int" => Some(builder::integer_type_default()),
        "num" => Some(builder::number_type()),
        "char" => Some(builder::character_type_default()),
        "bool" | "logic" => Some(builder::boolean_type()),
        _ => None,
    }
}

impl Op {
    pub fn to_type(&self) -> Option<Type> {
        match self {
            Op::In(h) => Some(Type::In(h.clone())),
            _ => None,
        }
    }

    pub fn get_token_type(&self) -> TokenKind {
        TokenKind::Operator
    }

    pub fn get_binding_power(&self) -> i32 {
        match self {
            Op::AsExcl(_) => 4,
            Op::Dot(_) | Op::Pipe(_) | Op::Dollar(_) | Op::In(_) => 4,
            Op::Mul(_) | Op::Div(_) | Op::Modulo(_) => 3,
            Op::Add(_) | Op::Minus(_) => 2,
            _ => 1,
        }
    }

    pub fn combine(self, left: Lang, right: Lang) -> Lang {
        if let Op::AsExcl(_) = self {
            let (type_name, literal_type) = match &right {
                // `x as! int` (and num/char/bool): the RHS parses as an
                // ordinary `Lang::Variable` identifier, same as a real alias
                // name — but these four are TypR primitive-type keywords,
                // not context-resolvable aliases. Left as `(name, None)`,
                // the type-checker's `ValidatingCast` arm used to wrap them
                // in `Type::Alias("int", ...)`, which nothing downstream can
                // reduce (the stdlib alias fallback only knows the
                // capitalized long-form names "Integer"/"Number"/etc., not
                // "int"/"num") — silently `Any` at best, and a hard panic at
                // worst (`HelpData::from(Type)` has no arm for `Type::Alias`,
                // reached e.g. via a record field's ArgumentType conversion).
                Lang::Variable { name, .. } => match primitive_cast_type(name) {
                    Some(t) => (t.pretty(), Some(t)),
                    None => (name.clone(), None),
                },
                _ => match lang_to_cast_type(&right) {
                    Some(t) => (t.pretty(), Some(t)),
                    None => ("Unknown".to_string(), None),
                },
            };
            return Lang::ValidatingCast {
                expression: Box::new(left.clone()),
                type_name,
                literal_type,
                help_data: left.get_help_data(),
            };
        }
        Lang::Operator {
            operator: self,
            rhs: Box::new(left.clone()),
            lhs: Box::new(right),
            help_data: left.get_help_data(),
        }
    }

    pub fn get_help_data(&self) -> HelpData {
        match self {
            Op::AsExcl(h) => h.clone(),
            Op::Empty(h) => h.clone(),
            Op::Custom(_, h) => h.clone(),
            Op::Dollar(h) => h.clone(),
            Op::Modulo(h) => h.clone(),
            Op::LesserOrEqual(h) => h.clone(),
            Op::GreaterOrEqual(h) => h.clone(),
            Op::Add(h) => h.clone(),
            Op::And(h) => h.clone(),
            Op::And2(h) => h.clone(),
            Op::Or(h) => h.clone(),
            Op::Or2(h) => h.clone(),
            Op::Eq(h) => h.clone(),
            Op::NotEq(h) => h.clone(),
            Op::Pipe(h) => h.clone(),
            Op::Dot(h) => h.clone(),
            Op::Minus(h) => h.clone(),
            Op::Mul(h) => h.clone(),
            Op::In(h) => h.clone(),
            Op::Div(h) => h.clone(),
            Op::LesserThan(h) => h.clone(),
            Op::GreaterThan(h) => h.clone(),
        }
    }
}

fn bool_op(s: Span) -> IResult<Span, Span> {
    // Deliberately no bare `tag("=")` alternative here: a single `=` is never
    // a binary operator in TypR — there is no `Op` variant for it at all
    // (see the tokenizer purge note on `op()` below). `=` is reserved for
    // dedicated grammar positions parsed directly elsewhere: named record
    // fields (`x = 1`), `assign()`'s `x = expr;`, and `fn(...)` default
    // parameter values.
    terminated(
        alt((
            tag("<="),
            tag(">="),
            tag("=="),
            tag("!="),
            tag("<"),
            tag(">"),
            tag("and"),
            tag("&&"),
            tag("&"),
            tag("or"),
            tag("||"),
            tag("|"),
        )),
        multispace0,
    )
    .parse(s)
}

fn get_op(ls: LocatedSpan<&str, String>) -> Op {
    match ls.clone().into_fragment() {
        "+" => Op::Add(ls.into()),
        "-" => Op::Minus(ls.into()),
        "*" => Op::Mul(ls.into()),
        "/" => Op::Div(ls.into()),
        "%" => Op::Modulo(ls.into()),
        "|>" => Op::Pipe(ls.into()),
        "::" => Op::Dollar(ls.into()),
        "." => Op::Dot(ls.into()),
        "$" => Op::Dollar(ls.into()),
        "==" => Op::Eq(ls.into()),
        "!=" => Op::NotEq(ls.into()),
        "<=" => Op::LesserOrEqual(ls.into()),
        ">=" => Op::GreaterOrEqual(ls.into()),
        "<" => Op::LesserThan(ls.into()),
        ">" => Op::GreaterThan(ls.into()),
        "in" => Op::In(ls.into()),
        "and" => Op::And2(ls.into()),
        "&&" => Op::And2(ls.into()),
        "&" => Op::And(ls.into()),
        "or" => Op::Or2(ls.into()),
        "||" => Op::Or2(ls.into()),
        "|" => Op::Or(ls.into()),
        n => Op::Custom(n.to_string(), ls.into()),
    }
}

pub fn custom_op(s: Span) -> IResult<Span, Span> {
    recognize((char('%'), take_until("%"), char('%'))).parse(s)
}

fn pipe_op(s: Span) -> IResult<Span, Span> {
    // `..` and `$$`/`|>>` are deliberately NOT recognized here: they have no
    // type-checking/transpiling arm and no stdlib signature anywhere, so as
    // runtime infix operators they're dead syntax — see the tokenizer purge
    // note on `op()` below. `..` is only alive as the dedicated *nominal
    // spread* field prefix (`Point:{ ..source }`), parsed by
    // `spread_field` in `parsing/elements.rs`, never through this combinator.
    alt((tag("|>"), tag("::"), tag("."), tag("$"))).parse(s)
}

pub fn op(s: Span) -> IResult<Span, Op> {
    // The doubled/duplicated-character variants (`++ -- ** // %% @@ ..  $$
    // |>>`) and bare `@`/`=` as infix operators are deliberately NOT
    // recognized here: none of them has a type-checking/transpiling arm or a
    // stdlib `` `op` `` signature anywhere (unlike `+ - * / % && || ...`,
    // which fall through to a real stdlib-backed function call — see the
    // catch-all `Lang::Operator` arm in `type_checking/mod.rs`). Left in the
    // tokenizer they silently swallowed typos as a "successfully parsed"
    // operator that only failed much later, as a confusing "function not
    // found" type error far from the real mistake — e.g. a stray `//`
    // comment (TypR comments are `#`, never `//`) was accepted as a division
    // operator instead of surfacing near the actual typo (now caught earlier,
    // as a `#`-comment lookalike, by `wrong_comment` in `parsing/mod.rs`).
    // `@` in particular is also the *prefix* of
    // `@pub`/`@export`/`@testable`/`@extern`/`@name: ...` annotations, parsed
    // by dedicated statement-level parsers tried before general expressions;
    // see cases/0012-new-scene-object-arg-not-found for the bug this specific
    // overlap caused before `@`/`@@` were dropped from this tokenizer.
    //
    // A bare `=` was deliberately tried as an `op()`-level recovery (falling
    // back to `Op::Eq` with a `SingleEqualsComparison` warning, so `if (a = b)`
    // degrades to `==` instead of losing the rest of the statement) but had
    // to be reverted: `op()` is also called from the *type* grammar
    // (`types.rs::index_operator`/`compute_operators`, for type-level
    // arithmetic like `type Combined <- A + B;`), which sits directly in
    // front of a default parameter's `= value` separator
    // (`greeting: char = "Hello"`). There, `=` right after a type is a
    // syntactic separator with no operator meaning at all — recovering it as
    // `Op::Eq` made `compute_operators` panic on the unhandled combination.
    // A future fix needs to special-case `=` in `elements.rs`'s
    // expression-only tokenizer instead of this shared primitive.
    let res = terminated(
        alt((
            custom_op,
            pipe_op,
            bool_op,
            // `in` is a word, not a symbol, so a bare `tag("in")` would also
            // match the first two characters of `index`/`input`/`in_valid`/
            // ... — guard with a trailing word-boundary check (no
            // alphanumeric-or-underscore right after, matching identifier
            // body chars in `elements.rs::body_char`), same idiom as
            // `single_letter_type_name` in `parsing/types.rs`. `not()` only
            // asserts, it consumes nothing, so identifiers keep matching as a
            // whole via `variable_exp` elsewhere; this only rules out this
            // tokenizer swallowing a prefix of one.
            terminated(tag("in"), not(alt((alphanumeric1, tag("_"))))),
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("%"),
        )),
        multispace0,
    )
    .parse(s);
    match res {
        Ok((s, ls)) => Ok((s, get_op(ls))),
        Err(r) => Err(r),
    }
}

pub fn get_string(op: &Op) -> String {
    match op {
        Op::AsExcl(_) => "as!".to_string(),
        Op::In(_) => "in".to_string(),
        Op::And(_) => "&".to_string(),
        Op::And2(_) => "&&".to_string(),
        Op::Or(_) => "|".to_string(),
        Op::Or2(_) => "||".to_string(),
        Op::Add(_) => "+".to_string(),
        Op::Minus(_) => "-".to_string(),
        Op::Mul(_) => "*".to_string(),
        Op::Div(_) => "/".to_string(),
        Op::Pipe(_) => "|>".to_string(),
        Op::Dot(_) => ".".to_string(),
        Op::LesserThan(_) => "<".to_string(),
        Op::GreaterThan(_) => ">".to_string(),
        Op::LesserOrEqual(_) => "<=".to_string(),
        Op::GreaterOrEqual(_) => ">=".to_string(),
        Op::Modulo(_) => "%%".to_string(),
        Op::Dollar(_) => "$".to_string(),
        Op::Eq(_) => "==".to_string(),
        Op::NotEq(_) => "!=".to_string(),
        Op::Custom(s, _) => s.clone(),
        n => todo!("operator to_string not implemented for {:?}", n),
    }
}

use std::fmt;
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = get_string(self);
        write!(f, "{}", res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span(s: &str) -> Span<'_> {
        LocatedSpan::new_extra(s, "test".to_string())
    }

    #[test]
    fn live_operators_still_tokenize() {
        for (input, expected) in [
            ("+ ", Op::Add(HelpData::default())),
            ("- ", Op::Minus(HelpData::default())),
            ("* ", Op::Mul(HelpData::default())),
            ("/ ", Op::Div(HelpData::default())),
            ("% ", Op::Modulo(HelpData::default())),
            ("&& ", Op::And2(HelpData::default())),
            ("|| ", Op::Or2(HelpData::default())),
        ] {
            let (_, got) = op(span(input)).unwrap_or_else(|_| panic!("{input:?} should tokenize"));
            assert_eq!(
                std::mem::discriminant(&got),
                std::mem::discriminant(&expected),
                "unexpected op for {input:?}: {got:?}"
            );
        }
    }

    #[test]
    fn dead_doubled_operators_no_longer_tokenize_as_one_token() {
        // ++ -- ** // %% .. $$ |>> and a bare @/= never had a stdlib `op`
        // signature or a typing/transpiling arm backing them (see the
        // tokenizer-purge note on `op()`), so they must no longer be
        // consumed whole as a single operator token — either the parse
        // fails outright, or (for tags that overlap a live single-char
        // prefix, e.g. "++"'s leading "+") only the live prefix is consumed,
        // leaving a leftover character that breaks the surrounding
        // expression grammar instead of silently forming a bogus operator.
        for (input, leftover) in [
            ("++", Some("+")),
            ("--", Some("-")),
            ("**", Some("*")),
            ("//", Some("/")),
            // "%%" is fully consumed by `custom_op` as an empty-name `%...%`
            // custom operator (`Op::Custom("", _)`), not by the purged
            // `Modulo2`/tag("%%") arm — a pre-existing, unrelated overlap.
            ("%%", Some("")),
            ("..", Some(".")),
            ("$$", Some("$")),
            ("|>>", Some(">")),
            ("@", None),
            ("=", None),
        ] {
            match (op(span(input)), leftover) {
                (Ok((rest, _)), Some(expected)) => {
                    assert_eq!(
                        *rest.fragment(),
                        expected,
                        "leftover mismatch for {input:?}"
                    )
                }
                (Err(_), None) => {}
                (result, expected) => {
                    panic!("{input:?}: expected leftover {expected:?}, got {result:?}")
                }
            }
        }
    }

    #[test]
    fn in_operator_word_boundary() {
        // `in` followed by anything that isn't a plain space used to fail to
        // tokenize at all (old pattern was the literal `tag("in ")`); now any
        // non-alphanumeric boundary works, matching how every other keyword
        // in the grammar behaves.
        for input in ["in ", "in(", "in\t", "in\n", "in)"] {
            let (_, got) = op(span(input)).unwrap_or_else(|_| panic!("{input:?} should tokenize"));
            assert_eq!(
                std::mem::discriminant(&got),
                std::mem::discriminant(&Op::In(HelpData::default())),
                "unexpected op for {input:?}: {got:?}"
            );
        }
    }

    #[test]
    fn in_operator_does_not_swallow_identifier_prefix() {
        // `index`/`input`/`instance` must never be tokenized as `Op::In` plus
        // a leftover suffix — `in` is only an operator at a word boundary.
        for input in ["index", "input", "instance", "in_valid"] {
            assert!(
                op(span(input)).is_err(),
                "{input:?} should not tokenize as an operator"
            );
        }
    }
}
