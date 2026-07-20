use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::syntax_error::SyntaxError;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::operators::op;
use crate::components::language::operators::Op;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::Type;
use crate::processes::parsing::base_parse;
use crate::processes::parsing::lang_token::LangToken;
use crate::processes::parsing::operation_priority::PriorityTokens;
use crate::processes::parsing::push_parse_error;
use crate::processes::parsing::types::if_type;
use crate::processes::parsing::types::label;
use crate::processes::parsing::types::ltype;
use crate::processes::parsing::types::pascal_case_no_space;
use crate::processes::parsing::types::primitive_types;
use crate::processes::parsing::types::single_type;

use crate::processes::parsing::vector_priority::VectorPriority;
use crate::utils::builder;
use nom::branch::alt;
use nom::bytes::complete::escaped;
use nom::bytes::complete::is_not;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_until;
use nom::bytes::complete::take_while1;
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::character::complete::anychar;
use nom::character::complete::char;
use nom::character::complete::digit1;
use nom::character::complete::line_ending;
use nom::character::complete::multispace0;
use nom::character::complete::multispace1;
use nom::character::complete::not_line_ending;
use nom::character::complete::one_of;
use nom::combinator::map;
use nom::combinator::not;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::multi::many0;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::IResult;
use nom::Parser;
use nom_locate::LocatedSpan;
use std::process::exit;

type Span<'a> = LocatedSpan<&'a str, String>;

pub fn is_pascal_case(name: &str) -> bool {
    let res = recognize(pascal_case_no_space).parse(name.into());
    match res {
        Ok((_, _)) => true,
        Err(_) => false,
    }
}

fn number_helper(s: Span) -> IResult<Span, Lang> {
    let res = (opt(tag("-")), digit1, tag("."), digit1).parse(s);
    match res {
        Ok((s, (sign, d1, _dot, d2))) => {
            let sign2 = sign.unwrap_or(LocatedSpan::new_extra("", d1.clone().extra));
            let n = format!("{}{}.{}", sign2, d1, d2).parse::<f32>().unwrap();
            Ok((
                s,
                Lang::Number {
                    value: n,
                    help_data: sign2.into(),
                },
            ))
        }
        Err(r) => Err(r),
    }
}

pub fn number(s: Span) -> IResult<Span, Lang> {
    terminated(number_helper, multispace0).parse(s)
}

fn integer(s: Span) -> IResult<Span, Lang> {
    let res = terminated((opt(tag("-")), digit1), multispace0).parse(s);
    match res {
        Ok((s, (minus, d))) => {
            let symbol = match minus {
                Some(_) => "-",
                None => "",
            }
            .to_string()
                + d.as_ref();
            Ok((
                s,
                Lang::Integer {
                    value: symbol.parse::<i32>().unwrap(),
                    help_data: d.into(),
                },
            ))
        }
        Err(r) => Err(r),
    }
}

fn get_value(l: LocatedSpan<&str, String>) -> Lang {
    match l.clone().into_fragment() {
        "true" | "TRUE" => Lang::Bool {
            value: true,
            help_data: l.into(),
        },
        "false" | "FALSE" => Lang::Bool {
            value: false,
            help_data: l.into(),
        },
        _ => panic!("No other boolean notation alolwed"),
    }
}

fn null_value(s: Span) -> IResult<Span, Lang> {
    let res = alt((
        terminated(terminated(tag("NULL"), not(body_char)), multispace0),
        terminated(terminated(tag("null"), not(body_char)), multispace0),
    ))
    .parse(s);
    match res {
        Ok((s, n)) => Ok((s, Lang::Null(n.into()))),
        Err(r) => Err(r),
    }
}

fn na_value(s: Span) -> IResult<Span, Lang> {
    let res = alt((
        terminated(terminated(tag("NA"), not(body_char)), multispace0),
        terminated(terminated(tag("na"), not(body_char)), multispace0),
    ))
    .parse(s);
    match res {
        Ok((s, n)) => Ok((s, Lang::NA(n.into()))),
        Err(r) => Err(r),
    }
}

fn boolean(s: Span) -> IResult<Span, Lang> {
    let res = alt((
        terminated(terminated(tag("true"), not(body_char)), multispace0),
        terminated(terminated(tag("TRUE"), not(body_char)), multispace0),
        terminated(terminated(tag("false"), not(body_char)), multispace0),
        terminated(terminated(tag("FALSE"), not(body_char)), multispace0),
    ))
    .parse(s);
    match res {
        Ok((s, ls)) => Ok((s, get_value(ls))),
        Err(r) => Err(r),
    }
}

pub fn chars(s: Span) -> IResult<Span, Lang> {
    terminated(alt((double_quotes, single_quotes)), multispace0).parse(s)
}

/// Decode the backslash escape sequences kept verbatim by `escaped(...)` into
/// their literal characters, so that `Lang::Char.value` holds the true semantic
/// value of the string, independent of the source quoting style. Re-encoding for
/// a given target (R, JS, ...) is the transpiler's responsibility.
pub fn decode_escapes(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('"') => out.push('"'),
                Some('\'') => out.push('\''),
                Some('\\') => out.push('\\'),
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

pub fn double_quotes(input: Span) -> IResult<Span, Lang> {
    let res = delimited(char('"'), opt(escaped(is_not("\\\""), '\\', anychar)), char('"')).parse(input);
    match res {
        Ok((s, st)) => {
            let content = st.clone().map(|span| decode_escapes(span.as_ref())).unwrap_or_default();
            let location = st.map(|span| span.into()).unwrap_or_else(|| s.clone().into());
            Ok((
                s,
                Lang::Char {
                    value: content,
                    help_data: location,
                },
            ))
        }
        Err(r) => Err(r),
    }
}

pub fn single_quotes(input: Span) -> IResult<Span, Lang> {
    let res = delimited(char('\''), opt(escaped(is_not("\\'"), '\\', anychar)), char('\'')).parse(input);
    match res {
        Ok((s, st)) => {
            let content = st.clone().map(|span| decode_escapes(span.as_ref())).unwrap_or_default();
            let location = st.map(|span| span.into()).unwrap_or_else(|| s.clone().into());
            Ok((
                s,
                Lang::Char {
                    value: content,
                    help_data: location,
                },
            ))
        }
        Err(r) => Err(r),
    }
}

fn starting_char(s: Span) -> IResult<Span, (char, HelpData)> {
    let res = one_of("abcdefghijklmnopqrstuvwxyz_")(s);
    match res {
        Ok((s, val)) => Ok((s.clone(), (val, s.into()))),
        Err(r) => Err(r),
    }
}

fn body_char(s: Span) -> IResult<Span, (char, HelpData)> {
    let res = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")(s);
    match res {
        Ok((s, val)) => Ok((s.clone(), (val, s.into()))),
        Err(r) => Err(r),
    }
}

pub fn variable_exp(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = (starting_char, many0(body_char)).parse(s);
    match res {
        Ok((s, ((s1, h), v))) => {
            let res2 = v.iter().map(|(val, _h)| *val).collect::<String>();
            Ok((s, (format!("{}{}", s1, res2), h.clone())))
        }
        Err(r) => Err(r),
    }
}

fn type_annotation(s: Span) -> IResult<Span, Type> {
    delimited(tag("<"), ltype, tag(">")).parse(s)
}

pub enum Case {
    Maj,
    Min,
}

fn variable_exp_2(s: Span) -> IResult<Span, (String, Case, HelpData)> {
    let res = variable_exp.parse(s);
    match res {
        Ok((s, (name, h))) => Ok((s, (name, Case::Min, h))),
        Err(r) => Err(r),
    }
}

fn pascal_case_2(s: Span) -> IResult<Span, (String, Case, HelpData)> {
    let res = pascal_case.parse(s);
    match res {
        Ok((s, (name, h))) => Ok((s, (name, Case::Maj, h))),
        Err(r) => Err(r),
    }
}

fn quoted_variable(s: Span) -> IResult<Span, (String, Case, HelpData)> {
    let res = delimited(char('`'), is_not("`"), char('`')).parse(s);

    match res {
        Ok((s, st)) => Ok((s, (format!("`{}`", st.clone()), Case::Min, st.into()))),
        Err(r) => Err(r),
    }
}

pub fn variable_recognizer(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = alt((quoted_variable, pascal_case_2, variable_exp_2)).parse(s);
    match res {
        Ok((s, (s1, _case, h))) => Ok((s, (s1, h))),
        Err(r) => Err(r),
    }
}

fn variable_helper(s: Span) -> IResult<Span, (Lang, Case)> {
    let res = (
        alt((quoted_variable, pascal_case_2, variable_exp_2)),
        opt(type_annotation),
    )
        .parse(s);
    match res {
        Ok((s, ((v, case, h), typ))) => {
            let res = Var::from_name(&v)
                .set_type(typ.unwrap_or(builder::empty_type()))
                .set_help_data(h);
            Ok((s, (res.into(), case)))
        }
        Err(r) => Err(r),
    }
}

pub fn variable(s: Span) -> IResult<Span, (Lang, Case)> {
    terminated(variable_helper, multispace0).parse(s)
}

pub fn argument(s: Span) -> IResult<Span, ArgumentType> {
    // Try variadic: `...name: T[,]`
    let variadic = (
        terminated(tag("..."), multispace0),
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        ltype,
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s.clone());
    if let Ok((s2, (_, e1, _, e2, _))) = variadic {
        return Ok((s2, ArgumentType(e1, e2, false, true, None)));
    }

    // Regular: `name: T[ = default][,]`
    let res = (
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        ltype,
        opt(preceded(terminated(tag("="), multispace0), parse_elements)),
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (e1, _, e2, default, _))) => Ok((s, ArgumentType(e1, e2, false, false, default.map(Box::new)))),
        Err(r) => Err(r),
    }
}

fn equality_params(s: Span) -> IResult<Span, Span> {
    terminated(alt((tag("="), tag(":"))), multispace0).parse(s)
}

fn argument_val(s: Span) -> IResult<Span, ArgumentValue> {
    let res = (
        terminated(alphanumeric1, multispace0),
        equality_params,
        parse_elements,
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentValue(e1.to_string(), e2))),
        Err(r) => Err(r),
    }
}

pub fn parse_block(input: Span) -> IResult<Span, Span> {
    recognize(parse_nested_braces).parse(input)
}

fn parse_nested_braces(input: Span) -> IResult<Span, Span> {
    recognize(delimited(
        tag("{"),
        many0(alt((
            parse_nested_braces,
            recognize(take_while1(|c| c != '{' && c != '}')),
        ))),
        tag("}"),
    ))
    .parse(input)
}

pub fn r_function(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(alt((tag("function"), tag("\\"))), multispace0),
        terminated(tag("("), multispace0),
        many0(terminated(terminated(variable, opt(tag(","))), multispace0)),
        terminated(tag(")"), multispace0),
        terminated(parse_block, multispace0),
    )
        .parse(s);
    match res {
        Ok((_s, (id, _op, _args, _cl, _exp))) if *id.fragment() == "fn" => {
            std::panic::panic_any(SyntaxError::FunctionWithoutType(id.into()))
        }
        Ok((s, (id, _op, args, _cl, exp))) => {
            let args = args.iter().map(|(arg, _)| arg).cloned().collect::<Vec<_>>();
            Ok((
                s,
                Lang::RFunction {
                    parameters: args,
                    body: exp.to_string(),
                    help_data: id.into(),
                },
            ))
        }
        Err(r) => Err(r),
    }
}

/// `R { ... }` — an untyped raw-R value block. The body is captured
/// verbatim, brace-balanced, via `parse_block` (the same technique as
/// `r_function` above) and never re-parsed as TypR, unlike `@{...}@`
/// (`vectorial_bloc` below), which re-lexes its contents as a sequence of
/// TypR elements and so can't hold real R-only syntax (pipes, formulas,
/// NSE). `parse_block`'s output already includes the delimiting `{`/`}`,
/// and R's own `{ ... }` is itself a value-producing expression (it
/// evaluates to its last statement), so the captured text is emitted
/// straight through at transpile time with no wrapper/call — see
/// `Lang::RBlock` in `transpiling/mod.rs`.
pub fn r_block(s: Span) -> IResult<Span, Lang> {
    let res = (terminated(tag("R"), multispace0), terminated(parse_block, multispace0)).parse(s);
    match res {
        Ok((s, (kw, body))) => Ok((
            s,
            Lang::RBlock {
                value: body.to_string(),
                help_data: kw.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

// Parses r#"..."# raw R string literals (exactly one hash).
// The body is returned verbatim — no escape processing.
fn raw_r_string(s: Span) -> IResult<Span, String> {
    let (s, _) = tag("r#\"")(s)?;
    let (s, body) = take_until("\"#")(s)?;
    let (s, _) = tag("\"#")(s)?;
    Ok((s, body.fragment().to_string()))
}

// `extern (name: Type, ...) -> RetType r#"...R code..."#`
// Typed raw R block: parameters and return type are checked by TypR;
// the body is emitted verbatim into the transpiled output.
// Return type must be a single type token (not a union/function type inline) —
// use a type alias for complex return types.
pub fn extern_block(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("extern"), multispace1),
        terminated(tag("("), multispace0),
        many0(argument),
        terminated(tag(")"), multispace0),
        terminated(alt((tag("->"), tag(":"))), multispace0),
        single_type,
        raw_r_string,
    )
        .parse(s);
    match res {
        Ok((s, (kw, _op, params, _cl, _arrow, ret_ty, body))) => Ok((
            s,
            Lang::ExternBlock {
                parameters: params,
                return_type: ret_ty,
                body,
                help_data: kw.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

pub fn simple_function(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("fn"), multispace0),
        terminated(tag("("), multispace0),
        many0(argument),
        terminated(tag(")"), multispace0),
        opt(terminated(alt((tag("->"), tag(":"))), multispace0)),
        opt(terminated(alt((if_type, ltype)), multispace0)),
        //alt((scope, parse_elements))
        scope,
    )
        .parse(s);
    match res {
        Ok((s, (_, _, args, _, Some(_), Some(typ), exp))) => Ok((
            s,
            Lang::Function {
                parameters: args,
                return_type: typ,
                body: Box::new(exp),
                help_data: HelpData::default(),
            },
        )),
        Ok((_s, (_, _, _args, _cp, None, None, _exp))) => {
            panic!("You forgot to specify the function return type: 'fn(...): Type'");
        }
        Ok((_s, (_, _, _args, _, Some(tag), None, _exp))) => {
            std::panic::panic_any(SyntaxError::FunctionWithoutReturnType(tag.into()));
        }
        Ok((_s, (_, _, _args, _, None, Some(typ), _exp))) => {
            eprintln!(
                "The type '{}' should be preceded by a ':' :\n 'fn(...): {}'",
                typ.clone(),
                typ.clone()
            );
            exit(1)
        }
        Err(r) => Err(r),
    }
}

fn function(s: Span) -> IResult<Span, Lang> {
    simple_function.parse(s)
}

fn key_value(s: Span) -> IResult<Span, Lang> {
    let res = (recognize(variable), terminated(tag("="), multispace0), single_element).parse(s);
    match res {
        Ok((s, (v, _eq, el))) => Ok((
            s,
            Lang::KeyValue {
                key: (*v).into(),
                value: Box::new(el),
                help_data: v.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn values(s: Span) -> IResult<Span, Vec<Lang>> {
    many0(terminated(
        alt((key_value, parse_elements)),
        terminated(opt(tag(",")), multispace0),
    ))
    .parse(s)
}

pub fn variable2(s: Span) -> IResult<Span, Lang> {
    let res = variable.parse(s);
    match res {
        Ok((s, (lang, _))) => Ok((s, lang)),
        Err(r) => Err(r),
    }
}

fn array_indexing(s: Span) -> IResult<Span, Lang> {
    let res = (alt((scope, variable2)), array).parse(s);

    match res {
        Ok((s, (lang1, lang2))) => Ok((
            s,
            Lang::ArrayIndexing {
                identifier: Box::new(lang1.clone()),
                indexing: Box::new(lang2),
                help_data: lang1.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn dataframe_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        alt((tag("data__frame"), tag("data.frame"))),
        terminated(tag("("), multispace0),
        many0(argument_val),
        terminated(tag(")"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (start, _, args, _))) => Ok((
            s,
            Lang::DataFrame {
                value: args.clone(),
                help_data: start.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn function_application(s: Span) -> IResult<Span, Lang> {
    let res = (
        alt((scope, variable2)),
        terminated(tag("("), multispace0),
        values,
        terminated(tag(")"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (exp, _, v, _))) => Ok((
            s,
            Lang::FunctionApp {
                identifier: Box::new(exp.clone()),
                arguments: v.clone(),
                help_data: exp.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn array(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("["), multispace0),
        values,
        terminated(tag("]"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_, v, _))) => Ok((
            s,
            Lang::Array {
                value: v.clone(),
                help_data: v.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

pub fn vector(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("c("), multispace0),
        values,
        terminated(tag(")"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_, v, _))) => Ok((
            s,
            Lang::Vector {
                value: v.clone(),
                help_data: v.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn sequence(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("seq["), multispace0),
        values,
        terminated(tag("]"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_, v, _))) => Ok((
            s,
            Lang::Sequence {
                body: v.clone(),
                help_data: v.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

/// Skips whitespace and `#`-line-comments. Unlike `multispace0`, this lets a
/// stray `#comment` line survive inside a `{ ... }` field list (record or
/// constructor literal), where fields aren't part of the statement-level
/// `many0` that normally absorbs `Lang::Comment` lines.
fn ws0(s: Span) -> IResult<Span, ()> {
    many0(alt((
        map(multispace1, |_| ()),
        map((char('#'), not_line_ending, opt(line_ending)), |_| ()),
    )))
    .parse(s)
    .map(|(s, _)| (s, ()))
}

/// One element inside a `TypeName:{ ... }` field list: either a regular
/// `name = value` field, a `..source` static spread (RFC-TR-033), or a
/// `...source` runtime spread (spread_operator2.md).
enum ConstructorElement {
    Field(Box<ArgumentValue>),
    Spread(Vec<String>, String, HelpData),
    RuntimeSpread(Box<Lang>),
}

/// Parses `... <expr> ,?` inside `TypeName:{ ... }` — the runtime-merge
/// spread (spread_operator2.md), as opposed to `spread_field`'s static `..name`.
fn runtime_spread_field(s: Span) -> IResult<Span, ConstructorElement> {
    let res = (
        terminated(tag("..."), multispace0),
        single_element,
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (_, e, _))) => Ok((s, ConstructorElement::RuntimeSpread(Box::new(e)))),
        Err(r) => Err(r),
    }
}

/// Parses `.. <module_path>$<variable> ,?` (the spread element of RFC-TR-033).
/// `..` is deliberately not allowed to be followed by another `.` so it can't
/// be confused with the variadic `...` token used in parameter lists.
fn spread_field(s: Span) -> IResult<Span, ConstructorElement> {
    let res = (
        terminated(terminated(tag(".."), not(char('.'))), multispace0),
        many0(terminated(variable_exp, tag("$"))),
        terminated(variable_exp, multispace0),
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (_, path, (name, h), _))) => Ok((
            s,
            ConstructorElement::Spread(path.into_iter().map(|(seg, _)| seg).collect(), name, h),
        )),
        Err(r) => Err(r),
    }
}

fn constructor_field(s: Span) -> IResult<Span, ConstructorElement> {
    if let Ok((s2, spread)) = spread_field(s.clone()) {
        return Ok((s2, spread));
    }
    if let Ok((s2, spread)) = runtime_spread_field(s.clone()) {
        return Ok((s2, spread));
    }
    let (s2, field) = argument_val(s)?;
    Ok((s2, ConstructorElement::Field(Box::new(field))))
}

fn constructor_call(s: Span) -> IResult<Span, Lang> {
    let res = (
        many0(terminated(variable_exp, tag("$"))),
        pascal_case,
        terminated(tag(":"), multispace0),
        terminated(tag("{"), multispace0),
        many0(preceded(ws0, constructor_field)),
        preceded(ws0, terminated(tag("}"), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (path, (name, h), _, _, elements, _))) => {
            let mut fields = Vec::new();
            let mut spreads = Vec::new();
            let mut runtime_spreads = Vec::new();
            for el in elements {
                match el {
                    ConstructorElement::Field(f) => fields.push(*f),
                    ConstructorElement::Spread(p, n, sh) => spreads.push((p, n, sh)),
                    ConstructorElement::RuntimeSpread(e) => runtime_spreads.push(*e),
                }
            }
            // Only a single static `..` spread per constructor call (RFC-TR-033
            // §2). Only a single runtime `...` spread per constructor call
            // (spread_operator3.md §2.2): it maps to the constructor's one
            // `.spread` parameter, unlike record literals which allow several.
            if spreads.len() > 1 || runtime_spreads.len() > 1 {
                return Err(nom::Err::Error(nom::error::Error::new(s, nom::error::ErrorKind::Many1)));
            }
            Ok((
                s,
                Lang::ConstructorCall {
                    module_path: path.into_iter().map(|(seg, _)| seg).collect(),
                    type_name: name,
                    fields,
                    spread: spreads.into_iter().next(),
                    spreads: runtime_spreads,
                    help_data: h,
                },
            ))
        }
        Err(r) => Err(r),
    }
}

fn array_constructor_call(s: Span) -> IResult<Span, Lang> {
    let res = (
        pascal_case,
        tag(":["),
        multispace0,
        values,
        terminated(tag("]"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, ((name, h), _, _, elems, _))) => Ok((
            s,
            Lang::ArrayConstructorCall {
                type_name: name,
                elements: elems,
                help_data: h,
            },
        )),
        Err(r) => Err(r),
    }
}

fn record_identifier(s: Span) -> IResult<Span, Span> {
    alt((tag("record"), tag("object"), tag("list"), tag(":"))).parse(s)
}

/// One element inside a record literal `{ ... }`: either a regular `name =
/// value` field or a `...source` spread (see spread_operator2.md).
enum RecordElement {
    Field(Box<ArgumentValue>),
    Spread(Box<Lang>),
}

/// Parses `... <expr> ,?`, the spread element of a record literal.
fn record_spread_field(s: Span) -> IResult<Span, RecordElement> {
    let res = (
        terminated(tag("..."), multispace0),
        single_element,
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (_, e, _))) => Ok((s, RecordElement::Spread(Box::new(e)))),
        Err(r) => Err(r),
    }
}

fn record_field(s: Span) -> IResult<Span, RecordElement> {
    if let Ok((s2, spread)) = record_spread_field(s.clone()) {
        return Ok((s2, spread));
    }
    let (s2, field) = argument_val(s)?;
    Ok((s2, RecordElement::Field(Box::new(field))))
}

pub fn record(s: Span) -> IResult<Span, Lang> {
    let res = (
        opt(terminated(record_identifier, multispace0)),
        terminated(alt((tag("{"), tag("("))), multispace0),
        many0(preceded(ws0, record_field)),
        preceded(ws0, terminated(alt((tag("}"), tag(")"))), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (Some(start), _, elements, _))) => {
            let mut fields = Vec::new();
            let mut spreads = Vec::new();
            for el in elements {
                match el {
                    RecordElement::Field(f) => fields.push(*f),
                    RecordElement::Spread(e) => spreads.push(*e),
                }
            }
            Ok((
                s,
                Lang::List {
                    value: fields,
                    spreads,
                    help_data: start.into(),
                },
            ))
        }
        Ok((_s, (None, _ob, _elements, _))) => Err(nom::Err::Error(nom::error::Error::new(
            _s,
            nom::error::ErrorKind::Many1,
        ))),
        Err(r) => Err(r),
    }
}

/// `list{...}` / `record{...}` / `object{...}` with at least one positional (unnamed)
/// element — e.g. `list{1, 2, 3}`. These three keywords are reserved for named-field
/// record literals (`list{ x = 1, y = 2 }`, handled by `record()` above, which is always
/// tried first in every `alt()` this parser shares a spot with and wins whenever every
/// element is `name = value`). By nom's `alt()` semantics, reaching this parser at all
/// means `record()` already failed to match here — so at least one element is positional.
/// Before this fix `list{1, 2, 3}` silently became a `Lang::Tuple` (via `tuple_exp`, which
/// also accepts the bare `"list"` keyword) while `record{1, 2, 3}`/`object{1, 2, 3}` fell
/// all the way through to `variable2`, misparsing as a bare identifier with a dangling,
/// unconsumed `{...}` block. This parser intercepts all three uniformly, still recovers a
/// full `Lang::Tuple` (nothing lost from the AST — same shape `tuple_exp` would have
/// produced), but flags it as a fatal `KeywordRecordPositionalElements` syntax error
/// pointing at the neutral `:{...}` tuple syntax instead.
///
/// Deliberately brace-only (`{`/`}`): `list(1, 2, 3)` (parens) is the long-established,
/// heavily-used way to build positional tuples (see `tuple_exp`, which keeps handling it
/// unchanged) and must not be affected.
fn keyword_positional_record_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(alt((tag("list"), tag("record"), tag("object"))), multispace0),
        terminated(tag("{"), multispace0),
        values,
        preceded(ws0, terminated(tag("}"), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (kw, _ob, vals, _cb))) => {
            let keyword = kw.to_string();
            let h: HelpData = kw.into();
            push_parse_error(SyntaxError::KeywordRecordPositionalElements {
                keyword,
                help_data: h.clone(),
            });
            Ok((
                s,
                Lang::Tuple {
                    value: vals,
                    help_data: h,
                },
            ))
        }
        Err(r) => Err(r),
    }
}

fn pascal_case_helper(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = (one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), opt(alpha1)).parse(s);
    match res {
        Ok((s, (t1, Some(t2)))) => Ok((s.clone(), (format!("{}{}", t1, t2), s.into()))),
        Ok((s, (t1, None))) => Ok((s.clone(), (t1.to_string(), s.into()))),
        Err(r) => Err(r),
    }
}

fn pascal_case(s: Span) -> IResult<Span, (String, HelpData)> {
    pascal_case_helper.parse(s)
}

fn union_constructor(s: Span) -> IResult<Span, Lang> {
    let res = (
        pascal_case,
        terminated(tag("."), multispace0),
        pascal_case,
        opt((
            terminated(tag(":"), multispace0),
            terminated(tag("{"), multispace0),
            many0(argument_val),
            terminated(tag("}"), multispace0),
        )),
    )
        .parse(s);
    match res {
        Ok((s, ((union_name, h), _, (variant_name, _), None))) => Ok((
            s,
            Lang::UnionConstructor {
                union_name,
                variant_name,
                fields: vec![],
                help_data: h,
            },
        )),
        Ok((s, ((union_name, h), _, (variant_name, _), Some((_, _, fields, _))))) => Ok((
            s,
            Lang::UnionConstructor {
                union_name,
                variant_name,
                fields,
                help_data: h,
            },
        )),
        Err(r) => Err(r),
    }
}

fn parenthese_value(s: Span) -> IResult<Span, Lang> {
    delimited(
        terminated(tag("("), multispace0),
        parse_elements,
        terminated(tag(")"), multispace0),
    )
    .parse(s)
}

pub fn tag_exp(s: Span) -> IResult<Span, Lang> {
    let res = terminated((tag("."), pascal_case, opt(parenthese_value)), multispace0).parse(s);
    match res {
        Ok((s, (dot, (n, _h), None))) => Ok((
            s,
            Lang::Tag {
                name: n,
                value: Box::new(Lang::Empty(dot.clone().into())),
                help_data: dot.into(),
            },
        )),
        Ok((s, (dot, (n, _h), Some(val)))) => Ok((
            s,
            Lang::Tag {
                name: n,
                value: Box::new(val),
                help_data: dot.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn dotdotdot(s: Span) -> IResult<Span, Lang> {
    let res = terminated(tag("..."), multispace0).parse(s);
    match res {
        Ok((s, d)) => Ok((s, Lang::Empty(d.into()))),
        Err(r) => Err(r),
    }
}

fn else_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("else"), multispace0),
        terminated(tag("{"), multispace0),
        parse_elements,
        terminated(tag("}"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_else, _o, exp, _c))) => Ok((s, exp)),
        Err(r) => Err(r),
    }
}

fn else_if_exp(s: Span) -> IResult<Span, Lang> {
    preceded(terminated(tag("else"), multispace1), if_exp).parse(s)
}

fn if_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("if"), multispace0),
        terminated(tag("("), multispace0),
        parse_elements,
        terminated(tag(")"), multispace0),
        terminated(tag("{"), multispace0),
        parse_elements,
        terminated(tag("}"), multispace0),
        opt(alt((else_if_exp, else_exp))),
    )
        .parse(s);
    match res {
        Ok((s, (_if, _op, cond, _cp, _o, exp, _c, els))) => Ok((
            s,
            Lang::If {
                condition: Box::new(cond),
                if_block: Box::new(exp),
                else_block: Box::new(els.unwrap_or(Lang::Empty(HelpData::default()))),
                help_data: _if.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

/// Parse a tag pattern with a variable binding in parentheses: `.Some(a)`
fn tag_pattern_with_var(s: Span) -> IResult<Span, Lang> {
    let res = (
        tag("."),
        pascal_case,
        delimited(
            terminated(tag("("), multispace0),
            variable2,
            terminated(tag(")"), multispace0),
        ),
    )
        .parse(s);
    match res {
        Ok((s, (dot, (n, _h), var))) => Ok((
            s,
            Lang::Tag {
                name: n,
                value: Box::new(var),
                help_data: dot.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

/// Parse a tag pattern without binding: `.None`
fn tag_pattern_no_var(s: Span) -> IResult<Span, Lang> {
    let res = (tag("."), pascal_case).parse(s);
    match res {
        Ok((s, (dot, (n, _h)))) => Ok((
            s,
            Lang::Tag {
                name: n,
                value: Box::new(Lang::Empty(dot.clone().into())),
                help_data: dot.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

/// Parse a wildcard pattern: `_`
fn wildcard_pattern(s: Span) -> IResult<Span, Lang> {
    let res = terminated(tag("_"), multispace0).parse(s);
    match res {
        Ok((s, underscore)) => Ok((
            s,
            Lang::Variable {
                name: "_".to_string(),
                is_opaque: false,
                related_type: builder::empty_type(),
                help_data: underscore.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

/// Parse a type pattern with a variable binding: `x as int`, `y as bool`, etc.
fn type_pattern(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(variable_exp, multispace0),
        terminated(tag("as"), multispace1),
        terminated(primitive_types, multispace0),
    )
        .parse(s);
    match res {
        Ok((s, ((name, h), _as, typ))) => Ok((
            s,
            Lang::TypePattern {
                variable_name: name,
                matched_type: typ,
                help_data: h,
            },
        )),
        Err(r) => Err(r),
    }
}

/// Parse a match pattern: `.Some(a)`, `.None`, `x as int`, `:{nom: n}`, `:{a, b}`, `_`, or a variable
fn match_pattern(s: Span) -> IResult<Span, Lang> {
    terminated(
        alt((
            tag_pattern_with_var,
            tag_pattern_no_var,
            record,
            keyword_positional_record_exp,
            tuple_exp,
            type_pattern,
            wildcard_pattern,
            variable2,
        )),
        multispace0,
    )
    .parse(s)
}

/// Parse a pattern branch: `pattern => expression,`
fn pattern_branch(s: Span) -> IResult<Span, (Lang, Box<Lang>)> {
    let res = (
        terminated(match_pattern, multispace0),
        terminated(tag("=>"), multispace0),
        terminated(parse_elements, multispace0),
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (pat, _arr, lang, _vir))) => Ok((s, (pat, Box::new(lang)))),
        Err(r) => Err(r),
    }
}

/// Parse a match expression with pattern matching:
/// `match expr { .Some(a) => a, .None => 0, _ => default }`
fn match_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("match"), multispace1),
        terminated(alt((scope, variable2)), multispace0),
        terminated(tag("{"), multispace0),
        many1(pattern_branch),
        terminated(tag("}"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_m, exp, _o, bs, _c))) => Ok((
            s,
            Lang::Match {
                target: Box::new(exp),
                branches: bs,
                help_data: _m.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

pub fn tuple_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(alt((tag("list"), tag(":"))), multispace0),
        terminated(alt((tag("{"), tag("("))), multispace0),
        values,
        terminated(alt((tag("}"), tag(")"))), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (id, _op, vals, _cl))) => Ok((
            s,
            Lang::Tuple {
                value: vals,
                help_data: id.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn int_or_var(s: Span) -> IResult<Span, Lang> {
    alt((integer, variable2)).parse(s)
}

fn create_range(params: &[Lang]) -> Lang {
    if params.len() == 2 {
        Lang::FunctionApp {
            identifier: Box::new(Var::from_name("seq").to_language()),
            arguments: vec![
                params[0].clone(),
                params[1].clone(),
                Lang::Integer {
                    value: 1,
                    help_data: HelpData::default(),
                },
            ],
            help_data: params.to_vec().into(),
        }
    } else {
        Lang::FunctionApp {
            identifier: Box::new(Var::from_name("seq").to_language()),
            arguments: vec![params[0].clone(), params[1].clone(), params[2].clone()],
            help_data: params.to_vec().into(),
        }
    }
}

fn range(s: Span) -> IResult<Span, Lang> {
    let res = (int_or_var, tag(":"), opt(terminated(int_or_var, tag(":"))), int_or_var).parse(s);
    //from_name().to_language()
    match res {
        Ok((s, (iv1, _sep, None, iv2))) => Ok((s, create_range(&[iv1.clone(), iv2.clone()]))),
        Ok((s, (iv1, _sep, Some(iv0), iv2))) => Ok((s, create_range(&[iv1.clone(), iv2.clone(), iv0.clone()]))),
        Err(r) => Err(r),
    }
}

fn function_application2(s: Span) -> IResult<Span, Lang> {
    let res = recognize(function_application).parse(s);
    match res {
        Ok((s, fun_app)) => Ok((
            s,
            Lang::Exp {
                value: fun_app.to_string(),
                help_data: fun_app.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn dot_variable(s: Span) -> IResult<Span, Lang> {
    let res = preceded(tag("."), variable2).parse(s);
    match res {
        Ok((
            s,
            Lang::Variable {
                name: n,
                is_opaque: b,
                related_type: c,
                help_data: d,
            },
        )) => Ok((
            s,
            Lang::Variable {
                name: format!(".{}", n),
                is_opaque: b,
                related_type: c,
                help_data: d,
            },
        )),
        Ok((_s, _)) => todo!(),
        Err(r) => Err(r),
    }
}

fn element_operator2(s: Span) -> IResult<Span, (Lang, Op)> {
    let res = (
        opt(op),
        alt((
            function_application2,
            null_value,
            number,
            integer,
            chars,
            boolean,
            variable2,
            dot_variable,
        )),
    )
        .parse(s);
    match res {
        Ok((s, (Some(ope), ele))) => Ok((s, (ele, ope))),
        Ok((s, (None, ele))) => Ok((s.clone(), (ele, Op::Empty(s.into())))),
        Err(r) => Err(r),
    }
}

fn vectorial_bloc(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("@{"), multispace0),
        recognize(many1(element_operator2)),
        terminated(tag("}@"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_start, bloc, _end))) => Ok((
            s,
            Lang::VecBlock {
                value: bloc.fragment().to_string(),
                help_data: bloc.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

/// Partial application: `\f(arg1 = val1, ...)` (RFC partial_application.md).
/// Distinguished from `lambda` below by what follows the `\`: `\(` is a
/// lambda's parameter list, `\identifier(` is a partial application — so
/// trying this combinator first and falling back to `lambda` on failure
/// (via the `alt()` in `single_element`) disambiguates the two unambiguously.
fn partial_application(s: Span) -> IResult<Span, Lang> {
    let res = (
        tag("\\"),
        variable2,
        terminated(tag("("), multispace0),
        many0(terminated(key_value, terminated(opt(tag(",")), multispace0))),
        terminated(tag(")"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (start, ident, _, args, _))) => Ok((
            s,
            Lang::PartialApp {
                function: Box::new(ident),
                arguments: args,
                help_data: start.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

/// Partial application over a record constructor: `\TypeName:{ field = val, ... }`
/// (records-only — see `partial_application` above for the function-call form).
/// Reuses the same `Lang::PartialApp` node: `function` holds a bare
/// `Lang::Variable` for the type name and `arguments` holds the fixed fields as
/// `Lang::KeyValue`, so the type-checker (`partial_application` in
/// `type_checking/partial_application.rs`) handles both forms with one
/// dispatch — it just resolves the target as a record alias instead of a
/// `Type::Function` when no function of that name exists. Field syntax reuses
/// `argument_val`'s grammar (same as `constructor_call`), so `:` or `=` both
/// work as the field separator.
fn partial_constructor_application(s: Span) -> IResult<Span, Lang> {
    let res = (
        tag("\\"),
        pascal_case,
        terminated(tag(":"), multispace0),
        terminated(tag("{"), multispace0),
        many0(preceded(ws0, argument_val)),
        preceded(ws0, terminated(tag("}"), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (start, (name, h), _, _, fields, _))) => {
            let arguments = fields
                .into_iter()
                .map(|ArgumentValue(key, value)| Lang::KeyValue {
                    key,
                    value: Box::new(value),
                    help_data: h.clone(),
                })
                .collect();
            Ok((
                s,
                Lang::PartialApp {
                    function: Box::new(Lang::Variable {
                        name,
                        is_opaque: false,
                        related_type: Type::Empty(h.clone()),
                        help_data: h,
                    }),
                    arguments,
                    help_data: start.into(),
                },
            ))
        }
        Err(r) => Err(r),
    }
}

fn lambda(s: Span) -> IResult<Span, Lang> {
    let res = (
        tag("\\"),
        terminated(tag("("), multispace0),
        many0(terminated(variable, opt((tag(","), multispace0)))),
        terminated(tag(")"), multispace0),
        parse_elements,
    )
        .parse(s);
    match res {
        Ok((s, (start, _, v, _, body))) => Ok((
            s,
            Lang::Lambda {
                parameters: v.iter().map(|(var, _)| var).cloned().collect(),
                body: Box::new(body.clone()),
                help_data: start.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn not_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        tag("!"),
        alt((
            tag_exp,
            range,
            lambda,
            boolean,
            number,
            integer,
            chars,
            match_exp,
            if_exp,
            dotdotdot,
            vector,
            record,
            keyword_positional_record_exp,
            r_function,
            function,
            tuple_exp,
            function_application,
            array_indexing,
            variable2,
            scope,
            array,
        )),
    )
        .parse(s);
    match res {
        Ok((s, (not_op, lang))) => Ok((
            s,
            Lang::Not {
                value: Box::new(lang),
                help_data: not_op.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn array_variant(s: Span) -> IResult<Span, Lang> {
    alt((vector, sequence)).parse(s)
}

fn js_block(s: Span) -> IResult<Span, Lang> {
    let res = (terminated(tag("JS"), multispace0), scope).parse(s);

    match res {
        Ok((s, (js, body))) => Ok((s, Lang::JSBlock(Box::new(body), 0, js.into()))),
        Err(r) => Err(r),
    }
}

fn primitive(s: Span) -> IResult<Span, Lang> {
    alt((null_value, na_value, boolean, number, integer, chars)).parse(s)
}

pub fn return_exp(s: Span) -> IResult<Span, Lang> {
    let res = terminated(delimited(tag("return "), parse_elements, tag(";")), multispace0).parse(s);
    match res {
        Ok((s, el)) => Ok((
            s,
            Lang::Return {
                value: Box::new(el.clone()),
                help_data: el.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

pub fn break_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = tag("break;").parse(s);
    match res {
        Ok((s, el)) => Ok((s, vec![Lang::Break(el.into())])),
        Err(r) => Err(r),
    }
}

pub fn next_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = tag("next;").parse(s);
    match res {
        Ok((s, el)) => Ok((s, vec![Lang::Next(el.into())])),
        Err(r) => Err(r),
    }
}

// main
pub fn single_element(s: Span) -> IResult<Span, Lang> {
    alt((
        alt((
            not_exp,
            tag_exp,
            union_constructor,
            range,
            partial_application,
            partial_constructor_application,
            lambda,
            primitive,
            js_block,
            return_exp,
            match_exp,
            if_exp,
            dotdotdot,
            array_variant,
        )),
        alt((
            dataframe_exp,
            array_constructor_call,
            constructor_call,
            record,
            keyword_positional_record_exp,
            r_function,
            r_block,
            extern_block,
            function,
            tuple_exp,
            function_application,
            array_indexing,
            variable2,
            scope,
            array,
        )),
    ))
    .parse(s)
}

pub fn scope(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(alt((tag("("), tag("{"))), multispace0),
        opt(base_parse),
        terminated(preceded(multispace0, alt((tag(")"), tag("}")))), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (open, Some(v), _))) if v.is_empty() => Ok((
            s,
            Lang::Scope {
                body: vec![],
                help_data: open.into(),
            },
        )),
        Ok((s, (_, Some(v), _))) => Ok((
            s,
            Lang::Scope {
                body: v.clone(),
                help_data: v.into(),
            },
        )),
        Ok((s, (open, None, _))) => Ok((
            s,
            Lang::Scope {
                body: vec![],
                help_data: open.into(),
            },
        )),
        Err(r) => Err(r),
    }
}

fn element_operator_token(s: Span) -> IResult<Span, LangToken> {
    match op.parse(s) {
        Ok((s, op)) => Ok((s, LangToken::Operator(op))),
        Err(r) => Err(r),
    }
}

fn single_element_token(s: Span) -> IResult<Span, LangToken> {
    match single_element.parse(s) {
        Ok((s, op)) => Ok((s, LangToken::Expression(op))),
        Err(r) => Err(r),
    }
}

fn as_excl_operator_token(s: Span) -> IResult<Span, LangToken> {
    let res = terminated(tag("as!"), multispace0).parse(s);
    match res {
        Ok((s, tok)) => Ok((s, LangToken::Operator(Op::AsExcl(tok.into())))),
        Err(r) => Err(r),
    }
}

/// Recovers a stray `=` used where `==` was meant (`if (a = b)`). Only tried
/// after `element_operator_token` has already failed at this position —
/// `op()`'s `bool_op` matches the two-char `==`/`!=`/`<=`/`>=` tags first, so
/// this never fires on a genuine comparison operator, only on a lone `=`.
/// `not(char('>'))` keeps it from swallowing the `=` of a match arm's `=>`
/// separator (`pattern_branch` in this file).
///
/// Deliberately NOT added to the shared `op()` primitive in `operators.rs`:
/// `op()` is also called by the *type* grammar
/// (`types.rs::index_operator`/`compute_operators`, for `type Combined <- A +
/// B;`), which sits directly in front of a default parameter's `= value`
/// separator (`greeting: char = "Hello"`) — recovering `=` there panics
/// `compute_operators` on the unhandled `Op::Eq` combination (see the long
/// comment on `op()`). Living here instead means the recovery only applies
/// inside `elements()`'s expression-continuation loop, which the type
/// grammar never calls.
fn single_equals_recovery_token(s: Span) -> IResult<Span, LangToken> {
    let res = terminated(terminated(recognize(char('=')), not(char('>'))), multispace0).parse(s);
    match res {
        Ok((s, eq)) => {
            push_parse_error(SyntaxError::SingleEqualsComparison(eq.clone().into()));
            Ok((s, LangToken::Operator(Op::Eq(eq.into()))))
        }
        Err(r) => Err(r),
    }
}

// `many1(alt((as_excl_operator_token, single_element_token, element_operator_token)))`
// used to gather tokens with no alternation constraint: two `single_element_token`s in a
// row (no operator between them) were silently accepted into the token vec. When the source
// omits a `;` between two statements (`let a <- fn(...){...}` directly followed by
// `let b <- ...`), the next statement's `let`/identifier tokens got vacuumed up as bogus
// trailing `Expression` tokens here (nothing in `variable_exp` excludes keywords), advancing
// the parse position across the whole next statement. `VectorPriority::run_helper`
// (operation_priority.rs) then silently drops any token vec tail that isn't a well-formed
// `Op Expression` pair — so the swallowed statement vanished from the AST with no parse
// error anywhere (see cases/0012). Requiring strict `E (Op E)*` alternation here means a
// stray non-operator after a complete expression makes `many0` stop *without* consuming
// input, leaving the next statement for the caller to parse normally instead of eating it.
fn operator_like_token(s: Span) -> IResult<Span, LangToken> {
    alt((
        as_excl_operator_token,
        element_operator_token,
        single_equals_recovery_token,
    ))
    .parse(s)
}

pub fn elements(s: Span) -> IResult<Span, Lang> {
    let res = (
        single_element_token,
        many0(pair(operator_like_token, single_element_token)),
    )
        .parse(s);
    match res {
        Ok((s, (first, rest))) => {
            if rest.is_empty() {
                Ok((s, first.into()))
            } else {
                let mut v = vec![first];
                for (op, ex) in rest {
                    v.push(op);
                    v.push(ex);
                }
                Ok((s, VectorPriority::from(v).run()))
            }
        }
        Err(r) => Err(r),
    }
}

// main
pub fn parse_elements(s: Span) -> IResult<Span, Lang> {
    alt((vectorial_bloc, elements)).parse(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::fluent_parser::FluentParser;

    #[test]
    fn test_extern_block_parse() {
        let res = r##"extern (x: int, y: char) -> char r#"paste0(x, y)"#"##.parse::<Lang>();
        println!("extern_block parse: {:?}", res.as_ref().map(|l| l.simple_print()));
        assert!(res.is_ok(), "extern_block should parse successfully");
        assert!(
            matches!(res.unwrap(), Lang::ExternBlock { .. }),
            "should be ExternBlock"
        );
    }

    #[test]
    fn test_extern_block_no_params_parse() {
        let res = r##"extern () -> int r#"42L"#"##.parse::<Lang>();
        println!("extern_block no-params: {:?}", res.as_ref().map(|l| l.simple_print()));
        assert!(res.is_ok(), "extern_block no-params should parse");
        assert!(matches!(res.unwrap(), Lang::ExternBlock { .. }));
    }

    #[test]
    fn test_r_block_parse() {
        let res = "R { sum(c(1,2,3)) }".parse::<Lang>();
        assert!(res.is_ok(), "r_block should parse successfully");
        match res.unwrap() {
            Lang::RBlock { value, .. } => assert_eq!(value, "{ sum(c(1,2,3)) }"),
            other => panic!("expected RBlock, got {}", other.simple_print()),
        }
    }

    #[test]
    fn test_r_block_multiline_with_pipe() {
        // Real R-only syntax (`%>%`) has no TypR equivalent and would break
        // `@{...}@` (which re-lexes as TypR elements) — `R { ... }` must
        // accept it verbatim, brace-balanced, across multiple lines.
        let src = "R {\n  df %>%\n    filter(x > 1) %>%\n    mutate(z = if (y) { 1 } else { 2 })\n}";
        let res = src.parse::<Lang>();
        assert!(res.is_ok(), "multiline r_block with nested braces should parse");
        assert!(matches!(res.unwrap(), Lang::RBlock { .. }));
    }

    #[test]
    fn test_r_block_transpiles_to_bare_r_block() {
        let fp = FluentParser::new().push("R { 1 + 2 }").run();
        let r_code = fp.get_r_code().iter().cloned().collect::<Vec<_>>().join("\n");
        assert_eq!(r_code.trim(), "{ 1 + 2 }");
    }

    #[test]
    fn test_empty_scope() {
        let res = "{  }".parse::<Lang>().unwrap();
        assert!(matches!(res, Lang::Scope { body, .. } if body.is_empty()));
    }

    #[test]
    fn test_decode_escapes() {
        assert_eq!(decode_escapes("hello"), "hello");
        assert_eq!(decode_escapes(r#"say \"hi\""#), r#"say "hi""#);
        assert_eq!(decode_escapes(r"it\'s"), "it's");
        assert_eq!(decode_escapes(r"a\\b"), r"a\b");
        assert_eq!(decode_escapes(r"line1\nline2"), "line1\nline2");
    }

    #[test]
    fn test_backslash_escapes_parse_in_string_literals() {
        // `"\n"`/`"\t"`/`"\\"` must parse as string literals, not fail the
        // whole statement (historically only `\"`/`\'` were accepted after a
        // backslash, so `cat("a", "\n")` broke the surrounding parse).
        for (src, expected) in [
            (r#""\n""#, "\n"),
            (r#""\t""#, "\t"),
            (r#""a\\b""#, r"a\b"),
            (r#"'\n'"#, "\n"),
            (r#""line1\nline2""#, "line1\nline2"),
        ] {
            match src.parse::<Lang>() {
                Ok(Lang::Char { value, .. }) => assert_eq!(value, expected, "wrong decoded value for {src}"),
                other => panic!("expected Lang::Char for {src}, got {:?}", other),
            }
        }
    }

    #[test]
    fn test_char_value_is_decoded() {
        // Both quoting styles store the same decoded semantic value.
        let from_double = r#""say \"hi\"""#.parse::<Lang>().unwrap();
        let from_single = r#"'say "hi"'"#.parse::<Lang>().unwrap();
        match (from_double, from_single) {
            (Lang::Char { value: d, .. }, Lang::Char { value: s, .. }) => {
                assert_eq!(d, r#"say "hi""#);
                assert_eq!(s, r#"say "hi""#);
            }
            other => panic!("expected two Lang::Char, got {:?}", other),
        }
    }

    #[test]
    fn test_function_with_empty_scope3() {
        let res = simple_function("fn(): int { 5 }".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Function");
    }

    #[test]
    fn test_variable1() {
        let res = variable_exp("hello".into()).unwrap().1 .0;
        assert_eq!(res, "hello", "Should return the variable name 'hello'");
    }

    #[test]
    fn test_simple_variable1() {
        let res = variable_exp("hello".into()).unwrap().1 .0;
        assert_eq!(res, "hello", "Should return the variable name 'hello'");
    }

    #[test]
    fn test_addition1() {
        let res = "1 + 2".parse::<Lang>().unwrap();
        assert_eq!(res.simple_print(), "Operator", "Should parse 1 + 2");
    }

    #[test]
    fn test_addition2() {
        let res = "1 + 2 + 3".parse::<Lang>().unwrap();
        assert_eq!(res.simple_print(), "Operator", "Should parse 1 + 2 + 3");
    }

    #[test]
    fn test_multiplication1() {
        let res = "1 + 2 * 3".parse::<Lang>().unwrap();
        assert_eq!(
            res.simple_print(),
            "Operator",
            "Should put multiplication first 1 + 2 * 3"
        );
    }

    #[test]
    fn test_multiplication2() {
        let res = "1 * 2 + 3".parse::<Lang>().unwrap();
        assert_eq!(
            res.simple_print(),
            "Operator",
            "Should put multiplication first 1 * 2 + 3"
        );
    }

    #[test]
    fn test_multiplication3() {
        let res = "1 * 2 + 3 * 4".parse::<Lang>().unwrap();
        assert_eq!(
            res.simple_print(),
            "Operator",
            "Should put multiplication first 1 * 2 + 3 * 4"
        );
    }

    #[test]
    fn test_accessor1() {
        let res = "3 + personne$age ".parse::<Lang>().unwrap();
        assert_eq!(
            res.simple_print(),
            "Operator",
            "Should put multiplication first 1 * 2 + 3 * 4"
        );
    }

    #[test]
    fn test_and1() {
        let res = "true & true".parse::<Lang>().unwrap();
        assert_eq!(res.simple_print(), "Operator", "Should accept '&&'");
    }

    #[test]
    fn test_array_indexing0() {
        let res = array_indexing("name[1, 2, 3]".into()).unwrap().1;
        assert_eq!(res.simple_print(), "ArrayIndexing");
    }

    #[test]
    fn test_array_indexing() {
        let fp = FluentParser::new().push("name[1, 2, 3]").parse_next();
        assert_eq!(fp.get_last_log(), "The logs are empty");
    }

    #[test]
    fn test_quoted_variable() {
        let res = quoted_variable("`+`".into()).unwrap().1;
        assert_eq!(res.0, "`+`");
    }

    #[test]
    fn test_uniform_function_call() {
        let fp = FluentParser::new().push("true.not()").parse_next();
        assert_eq!(fp.get_last_log(), "The logs are empty");
    }

    #[test]
    fn test_key_value1() {
        let res = key_value("sep = '3'".into()).unwrap().1;
        assert_eq!(res.simple_print(), "KeyValue");
    }

    #[test]
    fn test_empty_char0() {
        let res = single_element("''".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Char");
    }

    #[test]
    fn test_empty_char1() {
        let res = primitive("''".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Char");
    }

    #[test]
    fn test_empty_char2() {
        let res = chars("''".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Char");
    }

    // ==================== Null Tests ====================

    #[test]
    fn test_null_value_lowercase() {
        let res = null_value("null ".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Null");
    }

    #[test]
    fn test_null_value_uppercase() {
        let res = null_value("NULL ".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Null");
    }

    #[test]
    fn test_null_via_primitive() {
        let res = primitive("null ".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Null");
    }

    #[test]
    fn test_null_via_single_element() {
        let res = single_element("null ".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Null");
    }

    #[test]
    fn test_null_parse_lang() {
        let res = "null".parse::<Lang>().unwrap();
        assert_eq!(res.simple_print(), "Null");
    }

    #[test]
    fn test_null_type_check() {
        let fp = FluentParser::new()
            .push("let x: null <- null;")
            .parse_type_next()
            .push("x")
            .parse_next();
        assert_eq!(fp.get_last_type(), crate::utils::builder::null_type());
    }

    // ==================== Match Pattern Tests ====================

    #[test]
    fn test_match_pattern_tag_with_binding() {
        let input = "match x { .Some(a) => a, .None => 0 }";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
    }

    #[test]
    fn test_match_pattern_with_wildcard() {
        let input = "match x { .Some(a) => a, _ => 0 }";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
    }

    #[test]
    fn test_match_pattern_tag_without_binding() {
        let input = "match x { .None => 7 }";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
    }

    #[test]
    fn test_match_pattern_multiple_branches() {
        let input = "match value { .Some(a) => a + 1, .None => 0, _ => 9 }";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        // Verify we have 3 branches
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 3, "Should have 3 branches");
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_match_pattern_via_single_element() {
        let input = "match x { .Some(a) => a, .None => 0 } ";
        let res = single_element(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
    }

    #[test]
    fn test_match_pattern_branch_tag_with_var() {
        let input = ".Some(a) => a + 1, ";
        let res = pattern_branch(input.into()).unwrap().1;
        let (pattern, _body) = res;
        assert_eq!(pattern.simple_print(), "Tag");
    }

    #[test]
    fn test_match_pattern_branch_wildcard() {
        let input = "_ => 42 ";
        let res = pattern_branch(input.into()).unwrap().1;
        let (pattern, _body) = res;
        assert_eq!(pattern.simple_print(), "Variable(_)");
    }

    #[test]
    fn test_match_pattern_branch_tag_no_binding() {
        let input = ".None => 7, ";
        let res = pattern_branch(input.into()).unwrap().1;
        let (pattern, body) = res;
        assert_eq!(pattern.simple_print(), "Tag");
        assert_eq!(body.simple_print(), "Integer");
    }

    #[test]
    fn test_wildcard_pattern() {
        let input = "_ ";
        let res = wildcard_pattern(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Variable(_)");
    }

    #[test]
    fn test_tag_pattern_with_var() {
        let input = ".Some(a)";
        let res = tag_pattern_with_var(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Tag");
        if let Lang::Tag { name, value: inner, .. } = &res {
            assert_eq!(name, "Some");
            assert_eq!(inner.simple_print(), "Variable(a)");
        } else {
            panic!("Expected Tag variant");
        }
    }

    #[test]
    fn test_tag_pattern_no_var() {
        let input = ".None ";
        let res = tag_pattern_no_var(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Tag");
        if let Lang::Tag { name, value: inner, .. } = &res {
            assert_eq!(name, "None");
            assert_eq!(inner.simple_print(), "Empty");
        } else {
            panic!("Expected Tag variant");
        }
    }

    #[test]
    fn test_match_pattern_multiline() {
        let input = "match result {
            .Some(value) => value + 1,
            .None => 0,
            _ => 99
        } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 3);
        } else {
            panic!("Expected Match variant");
        }
    }

    // ==================== Type Pattern Tests ====================

    #[test]
    fn test_type_pattern_int() {
        let input = "x as int ";
        let res = type_pattern(input.into()).unwrap().1;
        assert!(
            res.simple_print().starts_with("TypePattern"),
            "Should parse 'x as int' as TypePattern"
        );
        if let Lang::TypePattern {
            variable_name: name, ..
        } = &res
        {
            assert_eq!(name, "x");
        } else {
            panic!("Expected TypePattern variant");
        }
    }

    #[test]
    fn test_type_pattern_bool() {
        let input = "y as bool ";
        let res = type_pattern(input.into()).unwrap().1;
        if let Lang::TypePattern {
            variable_name: name, ..
        } = &res
        {
            assert_eq!(name, "y");
        } else {
            panic!("Expected TypePattern variant");
        }
    }

    #[test]
    fn test_type_pattern_num() {
        let input = "val as num ";
        let res = type_pattern(input.into()).unwrap().1;
        if let Lang::TypePattern {
            variable_name: name, ..
        } = &res
        {
            assert_eq!(name, "val");
        } else {
            panic!("Expected TypePattern variant");
        }
    }

    #[test]
    fn test_type_pattern_char() {
        let input = "s as char ";
        let res = type_pattern(input.into()).unwrap().1;
        if let Lang::TypePattern {
            variable_name: name, ..
        } = &res
        {
            assert_eq!(name, "s");
        } else {
            panic!("Expected TypePattern variant");
        }
    }

    #[test]
    fn test_match_with_type_patterns() {
        let input = "match x { y as int => y + 1, z as bool => 0 } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 2, "Should have 2 branches");
            assert!(
                branches[0].0.simple_print().starts_with("TypePattern"),
                "First branch should be a TypePattern"
            );
            assert!(
                branches[1].0.simple_print().starts_with("TypePattern"),
                "Second branch should be a TypePattern"
            );
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_match_mixed_tag_and_type_patterns() {
        let input = "match value {
            .Some(a) => a,
            x as int => x + 1,
            _ => 0
        } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 3, "Should have 3 branches");
            assert_eq!(branches[0].0.simple_print(), "Tag");
            assert!(branches[1].0.simple_print().starts_with("TypePattern"));
            assert_eq!(branches[2].0.simple_print(), "Variable(_)");
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_type_pattern_in_match_pattern() {
        let input = "x as int ";
        let res = match_pattern(input.into()).unwrap().1;
        assert!(
            res.simple_print().starts_with("TypePattern"),
            "match_pattern should accept type patterns"
        );
    }

    // ==================== List/Record Pattern Tests ====================

    #[test]
    fn test_record_pattern_colon_syntax() {
        let input = ":{nom: n, age: a} ";
        let res = match_pattern(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Record", "Should parse record pattern as Record");
        if let Lang::List { value: fields, .. } = &res {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].get_argument(), "nom");
            assert_eq!(fields[1].get_argument(), "age");
        } else {
            panic!("Expected List variant");
        }
    }

    #[test]
    fn test_record_pattern_list_syntax() {
        let input = "list(nom = n, age = a) ";
        let res = match_pattern(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Record");
        if let Lang::List { value: fields, .. } = &res {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].get_argument(), "nom");
            assert_eq!(fields[1].get_argument(), "age");
        } else {
            panic!("Expected List variant");
        }
    }

    #[test]
    fn test_match_with_record_pattern() {
        let input = "match x { :{nom: n, age: a} => a, _ => 0 } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 2, "Should have 2 branches");
            assert_eq!(
                branches[0].0.simple_print(),
                "Record",
                "First branch should be a Record pattern"
            );
            assert_eq!(branches[1].0.simple_print(), "Variable(_)");
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_match_with_list_pattern() {
        let input = "match x { list(nom = n, age = a) => a, _ => 0 } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 2);
            assert_eq!(branches[0].0.simple_print(), "Record");
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_match_mixed_record_tag_type_patterns() {
        let input = "match value {
            .Some(a) => a,
            :{nom: n, age: a} => a,
            x as int => x + 1,
            _ => 0
        } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 4);
            assert_eq!(branches[0].0.simple_print(), "Tag");
            assert_eq!(branches[1].0.simple_print(), "Record");
            assert!(branches[2].0.simple_print().starts_with("TypePattern"));
            assert_eq!(branches[3].0.simple_print(), "Variable(_)");
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_record_pattern_single_field() {
        let input = ":{nom: n} ";
        let res = match_pattern(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Record");
        if let Lang::List { value: fields, .. } = &res {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].get_argument(), "nom");
        } else {
            panic!("Expected List variant");
        }
    }

    // ==================== Tuple Pattern Tests ====================

    #[test]
    fn test_tuple_pattern_colon_syntax() {
        let input = ":{a, b, c} ";
        let res = match_pattern(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Tuple");
        if let Lang::Tuple { value: elements, .. } = &res {
            assert_eq!(elements.len(), 3);
        } else {
            panic!("Expected Tuple variant");
        }
    }

    #[test]
    fn test_tuple_pattern_list_syntax() {
        let input = "list(a, b, c) ";
        let res = match_pattern(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Tuple");
        if let Lang::Tuple { value: elements, .. } = &res {
            assert_eq!(elements.len(), 3);
        } else {
            panic!("Expected Tuple variant");
        }
    }

    #[test]
    fn test_tuple_pattern_two_elements() {
        let input = ":{x, y} ";
        let res = match_pattern(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Tuple");
        if let Lang::Tuple { value: elements, .. } = &res {
            assert_eq!(elements.len(), 2);
        } else {
            panic!("Expected Tuple variant");
        }
    }

    #[test]
    fn test_match_with_tuple_pattern() {
        let input = "match x { :{a, b, c} => a + c, _ => 0 } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 2);
            assert_eq!(branches[0].0.simple_print(), "Tuple");
            assert_eq!(branches[1].0.simple_print(), "Variable(_)");
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_match_with_list_tuple_pattern() {
        let input = "match x { list(a, b, c) => a + c, _ => 0 } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 2);
            assert_eq!(branches[0].0.simple_print(), "Tuple");
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_match_mixed_all_pattern_types() {
        let input = "match value {
            .Some(a) => a,
            :{nom: n, age: a} => a,
            :{x, y} => x + y,
            z as int => z + 1,
            _ => 0
        } ";
        let res = match_exp(input.into()).unwrap().1;
        assert_eq!(res.simple_print(), "Match");
        if let Lang::Match { branches, .. } = &res {
            assert_eq!(branches.len(), 5);
            assert_eq!(branches[0].0.simple_print(), "Tag");
            assert_eq!(branches[1].0.simple_print(), "Record");
            assert_eq!(branches[2].0.simple_print(), "Tuple");
            assert!(branches[3].0.simple_print().starts_with("TypePattern"));
            assert_eq!(branches[4].0.simple_print(), "Variable(_)");
        } else {
            panic!("Expected Match variant");
        }
    }

    #[test]
    fn test_character_constructor_fn() {
        let input = "fn(name: char, attack: int, health: int): Character {\n    :{ name: name, attack: attack, health: health }\n}";
        let res = simple_function(input.into());
        match &res {
            Ok((remaining, _)) => {
                println!("SUCCESS, remaining: {:?}", **remaining);
                assert!(
                    remaining.is_empty(),
                    "Should consume entire input, remaining: {:?}",
                    **remaining
                );
            }
            Err(e) => panic!("Parse failed: {:?}", e),
        }
    }

    #[test]
    fn test_scope_with_record_body() {
        let input = "{\n    :{ name: name, attack: attack, health: health }\n}";
        let res = scope(input.into());
        match &res {
            Ok((remaining, _)) => {
                println!("scope SUCCESS, remaining: {:?}", **remaining);
            }
            Err(e) => println!("scope FAILED: {:?}", e),
        }
        assert!(res.is_ok(), "scope should succeed");
    }

    #[test]
    fn test_record_parse_directly() {
        use crate::processes::parsing::base_parse;
        let input = ":{ name: name, attack: attack, health: health }";
        let res = base_parse(input.into());
        println!(
            "base_parse result: {:?}",
            res.as_ref().map(|(r, v): &(_, Vec<_>)| (*r.fragment(), v.len()))
        );
        assert!(res.is_ok());
        let (remaining, elems) = res.unwrap();
        println!("  remaining: {:?}", *remaining.fragment());
        println!("  elements count: {}", elems.len());
        for (i, el) in elems.iter().enumerate() {
            println!("  elem[{}]: {}", i, el.simple_print());
        }
    }

    #[test]
    fn test_parse_elements_record() {
        let input = ":{ name: name, attack: attack, health: health }";
        let res = parse_elements(input.into());
        match &res {
            Ok((remaining, lang)) => println!(
                "parse_elements OK: {}, remaining: {:?}",
                lang.simple_print(),
                **remaining
            ),
            Err(e) => println!("parse_elements FAILED: {:?}", e),
        }
        assert!(res.is_ok(), "parse_elements should succeed on record");
    }

    #[test]
    fn test_single_element_record() {
        let input = ":{ name: name, attack: attack, health: health }";
        let res = single_element(input.into());
        match &res {
            Ok((remaining, lang)) => println!(
                "single_element OK: {}, remaining: {:?}",
                lang.simple_print(),
                **remaining
            ),
            Err(e) => println!("single_element FAILED: {:?}", e),
        }
        assert!(res.is_ok(), "single_element should succeed on record");
    }

    #[test]
    fn test_record_logic_inline() {
        let input = ":{ name: name, attack: attack, health: health }";
        let res = record(input.into());
        match &res {
            Ok((remaining, lang)) => println!("record OK: {}, remaining: {:?}", lang.simple_print(), **remaining),
            Err(e) => println!("record FAILED: {:?}", e),
        }
        assert!(res.is_ok(), "record should succeed");
    }

    #[test]
    fn test_module_constructor_parsing() {
        let (_, lang) = constructor_call("person$Person:{ age = 12, name = \"Bob\" }".into())
            .expect("Should parse module constructor call");
        match lang {
            Lang::ConstructorCall {
                module_path, type_name, ..
            } => {
                assert_eq!(module_path, vec!["person".to_string()]);
                assert_eq!(type_name, "Person");
            }
            other => panic!("Expected ConstructorCall, got: {}", other.simple_print()),
        }

        let fp = FluentParser::new()
            .push("module person { @pub type Person <- list { age: int, name: char }; };")
            .run()
            .push("let p <- person$Person:{ age = 12, name = \"Bob\" };")
            .run();
        assert_eq!(fp.get_last_log(), "The logs are empty");
    }

    #[test]
    fn test_constructor_call_spread_parsing() {
        let (_, lang) = constructor_call("Person:{ name = \"Alice\", ..bob }".into())
            .expect("Should parse constructor call with spread");
        match lang {
            Lang::ConstructorCall {
                type_name,
                fields,
                spread,
                ..
            } => {
                assert_eq!(type_name, "Person");
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].get_argument(), "name");
                let (path, name, _) = spread.expect("Should have a spread");
                assert!(path.is_empty());
                assert_eq!(name, "bob");
            }
            other => panic!("Expected ConstructorCall, got: {}", other.simple_print()),
        }
    }

    #[test]
    fn test_constructor_call_runtime_spread_parsing() {
        let (_, lang) = constructor_call("Person:{ name = \"Alice\", ...bob }".into())
            .expect("Should parse constructor call with runtime spread");
        match lang {
            Lang::ConstructorCall {
                type_name,
                fields,
                spread,
                spreads,
                ..
            } => {
                assert_eq!(type_name, "Person");
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].get_argument(), "name");
                assert!(spread.is_none());
                assert_eq!(spreads.len(), 1);
            }
            other => panic!("Expected ConstructorCall, got: {}", other.simple_print()),
        }
    }

    #[test]
    fn test_record_literal_spread_parsing() {
        let (_, lang) = record(":{ ...x, a = 1 }".into()).expect("Should parse record literal with spread");
        match lang {
            Lang::List { value, spreads, .. } => {
                assert_eq!(value.len(), 1);
                assert_eq!(value[0].get_argument(), "a");
                assert_eq!(spreads.len(), 1);
                assert!(matches!(&spreads[0], Lang::Variable { name, .. } if name == "x"));
            }
            other => panic!("Expected Lang::List, got: {}", other.simple_print()),
        }
    }

    #[test]
    fn test_record_literal_multiple_spreads_parsing() {
        let (_, lang) =
            record(":{ ...x, ...y, a = 1 }".into()).expect("Should parse record literal with multiple spreads");
        match lang {
            Lang::List { value, spreads, .. } => {
                assert_eq!(value.len(), 1);
                assert_eq!(spreads.len(), 2);
            }
            other => panic!("Expected Lang::List, got: {}", other.simple_print()),
        }
    }

    #[test]
    fn test_record_literal_bare_spread_parsing() {
        let (_, lang) = record(":{ ...x }".into()).expect("Should parse record literal with bare spread");
        match lang {
            Lang::List { value, spreads, .. } => {
                assert!(value.is_empty());
                assert_eq!(spreads.len(), 1);
            }
            other => panic!("Expected Lang::List, got: {}", other.simple_print()),
        }
    }
}
