use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::help_message::ErrorMsg;
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
use crate::processes::parsing::types::if_type;
use crate::processes::parsing::types::label;
use crate::processes::parsing::types::ltype;
use crate::processes::parsing::types::pascal_case_no_space;
use crate::processes::parsing::types::primitive_types;

use crate::processes::parsing::vector_priority::VectorPriority;
use crate::utils::builder;
use nom::branch::alt;
use nom::bytes::complete::escaped;
use nom::bytes::complete::is_not;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while1;
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::character::complete::char;
use nom::character::complete::digit1;
use nom::character::complete::multispace0;
use nom::character::complete::multispace1;
use nom::character::complete::one_of;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::multi::many0;
use nom::multi::many1;
use nom::sequence::delimited;
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
            Ok((s, Lang::Number(n, sign2.into())))
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
                + &d.to_string();
            Ok((s, Lang::Integer(symbol.parse::<i32>().unwrap(), d.into())))
        }
        Err(r) => Err(r),
    }
}

fn get_value(l: LocatedSpan<&str, String>) -> Lang {
    match l.clone().into_fragment() {
        "true" | "TRUE" => Lang::Bool(true, l.into()),
        "false" | "FALSE" => Lang::Bool(false, l.into()),
        _ => panic!("No other boolean notation alolwed"),
    }
}

fn null_value(s: Span) -> IResult<Span, Lang> {
    let res = alt((
        terminated(tag("NULL"), multispace0),
        terminated(tag("null"), multispace0),
    ))
    .parse(s);
    match res {
        Ok((s, n)) => Ok((s, Lang::Null(n.into()))),
        Err(r) => Err(r),
    }
}

fn na_value(s: Span) -> IResult<Span, Lang> {
    let res = alt((
        terminated(tag("NA"), multispace0),
        terminated(tag("na"), multispace0),
    ))
    .parse(s);
    match res {
        Ok((s, n)) => Ok((s, Lang::NA(n.into()))),
        Err(r) => Err(r),
    }
}

fn boolean(s: Span) -> IResult<Span, Lang> {
    let res = alt((
        terminated(tag("true"), multispace0),
        terminated(tag("TRUE"), multispace0),
        terminated(tag("false"), multispace0),
        terminated(tag("FALSE"), multispace0),
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

pub fn double_quotes(input: Span) -> IResult<Span, Lang> {
    let res = delimited(
        char('"'),
        opt(escaped(is_not("\\\""), '\\', alt((char('"'), char('\''))))),
        char('"'),
    )
    .parse(input);
    match res {
        Ok((s, st)) => {
            let content = st.clone().map(|span| span.to_string()).unwrap_or_default();
            let location = st
                .map(|span| span.into())
                .unwrap_or_else(|| s.clone().into());
            Ok((s, Lang::Char(content, location)))
        }
        Err(r) => Err(r),
    }
}

pub fn single_quotes(input: Span) -> IResult<Span, Lang> {
    let res = delimited(
        char('\''),
        opt(escaped(is_not("\\'"), '\\', alt((char('"'), char('\''))))),
        char('\''),
    )
    .parse(input);
    match res {
        Ok((s, st)) => {
            let content = st.clone().map(|span| span.to_string()).unwrap_or_default();
            let location = st
                .map(|span| span.into())
                .unwrap_or_else(|| s.clone().into());
            Ok((s, Lang::Char(content, location)))
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
            let res2 = v.iter().map(|(val, _h)| val.clone()).collect::<String>();
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
    let res = (
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        ltype,
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentType(e1, e2, false))),
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
        single_element,
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
            panic!("{}", SyntaxError::FunctionWithoutType(id.into()).display())
        }
        Ok((s, (id, _op, args, _cl, exp))) => {
            let args = args.iter().map(|(arg, _)| arg).cloned().collect::<Vec<_>>();
            Ok((s, Lang::RFunction(args, exp.to_string(), id.into())))
        }
        Err(r) => Err(r),
    }
}

pub fn simple_function(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("fn"), multispace0),
        terminated(tag("("), multispace0),
        many0(argument),
        terminated(tag(")"), multispace0),
        opt(terminated(tag(":"), multispace0)),
        opt(terminated(alt((if_type, ltype)), multispace0)),
        //alt((scope, parse_elements))
        scope,
    )
        .parse(s);
    match res {
        Ok((s, (_, _, args, _, Some(_), Some(typ), exp))) => Ok((
            s,
            Lang::Function(args, typ, Box::new(exp), HelpData::default()),
        )),
        Ok((_s, (_, _, _args, _cp, None, None, _exp))) => {
            panic!("You forgot to specify the function return type: 'fn(...): Type'");
        }
        Ok((_s, (_, _, _args, _, Some(tag), None, _exp))) => {
            None::<bool>.expect(&SyntaxError::FunctionWithoutReturnType(tag.into()).display());
            exit(1)
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
    let res = (
        recognize(variable),
        terminated(tag("="), multispace0),
        single_element,
    )
        .parse(s);
    match res {
        Ok((s, (v, _eq, el))) => Ok((s, Lang::KeyValue((*v).into(), Box::new(el), v.into()))),
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
            Lang::ArrayIndexing(Box::new(lang1.clone()), Box::new(lang2), lang1.into()),
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
            Lang::FunctionApp(Box::new(exp.clone()), v.clone(), exp.into()),
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
        Ok((s, (_, v, _))) => Ok((s, Lang::Array(v.clone(), v.into()))),
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
        Ok((s, (_, v, _))) => Ok((s, Lang::Vector(v.clone(), v.into()))),
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
        Ok((s, (_, v, _))) => Ok((s, Lang::Sequence(v.clone(), v.into()))),
        Err(r) => Err(r),
    }
}

fn record_identifier(s: Span) -> IResult<Span, Span> {
    alt((tag("record"), tag("object"), tag("list"), tag(":"))).parse(s)
}

fn record(s: Span) -> IResult<Span, Lang> {
    let res = (
        opt(record_identifier),
        terminated(alt((tag("{"), tag("("))), multispace0),
        many0(argument_val),
        terminated(alt((tag("}"), tag(")"))), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (Some(start), _, args, _))) => Ok((s, Lang::List(args.clone(), start.into()))),
        Ok((_s, (None, _ob, args, _))) => {
            if args.len() == 0 {
                panic!("Error: the scope shouldn't be empty")
            } else {
                eprintln!("{}", _s);
                panic!("You forgot to put a record identifier before the bracket: ':{{...}}'");
            }
        }
        Err(r) => Err(r),
    }
}

fn pascal_case_helper(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = (one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alpha1).parse(s);
    match res {
        Ok((s, (t1, t2))) => Ok((s.clone(), (format!("{}{}", t1, t2), s.into()))),
        Err(r) => Err(r),
    }
}

fn pascal_case(s: Span) -> IResult<Span, (String, HelpData)> {
    pascal_case_helper.parse(s)
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
    let res = (tag("."), pascal_case, opt(parenthese_value)).parse(s);
    match res {
        Ok((s, (dot, (n, _h), None))) => Ok((
            s,
            Lang::Tag(n, Box::new(Lang::Empty(dot.clone().into())), dot.into()),
        )),
        Ok((s, (dot, (n, _h), Some(val)))) => Ok((s, Lang::Tag(n, Box::new(val), dot.into()))),
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
            Lang::If(
                Box::new(cond),
                Box::new(exp),
                Box::new(els.unwrap_or(Lang::Empty(HelpData::default()))),
                _if.into(),
            ),
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
        Ok((s, (dot, (n, _h), var))) => Ok((s, Lang::Tag(n, Box::new(var), dot.into()))),
        Err(r) => Err(r),
    }
}

/// Parse a tag pattern without binding: `.None`
fn tag_pattern_no_var(s: Span) -> IResult<Span, Lang> {
    let res = (tag("."), pascal_case).parse(s);
    match res {
        Ok((s, (dot, (n, _h)))) => Ok((
            s,
            Lang::Tag(n, Box::new(Lang::Empty(dot.clone().into())), dot.into()),
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
            Lang::Variable(
                "_".to_string(),
                false,
                builder::empty_type(),
                underscore.into(),
            ),
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
        Ok((s, ((name, h), _as, typ))) => Ok((s, Lang::TypePattern(name, typ, h))),
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
        Ok((s, (_m, exp, _o, bs, _c))) => Ok((s, Lang::Match(Box::new(exp), bs, _m.into()))),
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
        Ok((s, (id, _op, vals, _cl))) => Ok((s, Lang::Tuple(vals, id.into()))),
        Err(r) => Err(r),
    }
}

fn int_or_var(s: Span) -> IResult<Span, Lang> {
    alt((integer, variable2)).parse(s)
}

fn create_range(params: &[Lang]) -> Lang {
    if params.len() == 2 {
        Lang::FunctionApp(
            Box::new(Var::from_name("seq").to_language()),
            vec![
                params[0].clone(),
                params[1].clone(),
                Lang::Integer(1, HelpData::default()),
            ],
            params.to_vec().into(),
        )
    } else {
        Lang::FunctionApp(
            Box::new(Var::from_name("seq").to_language()),
            vec![params[0].clone(), params[1].clone(), params[2].clone()],
            params.to_vec().into(),
        )
    }
}

fn range(s: Span) -> IResult<Span, Lang> {
    let res = (
        int_or_var,
        tag(":"),
        opt(terminated(int_or_var, tag(":"))),
        int_or_var,
    )
        .parse(s);
    //from_name().to_language()
    match res {
        Ok((s, (iv1, _sep, None, iv2))) => Ok((s, create_range(&[iv1.clone(), iv2.clone()]))),
        Ok((s, (iv1, _sep, Some(iv0), iv2))) => {
            Ok((s, create_range(&[iv1.clone(), iv2.clone(), iv0.clone()])))
        }
        Err(r) => Err(r),
    }
}

fn function_application2(s: Span) -> IResult<Span, Lang> {
    let res = recognize(function_application).parse(s);
    match res {
        Ok((s, fun_app)) => Ok((s, Lang::Exp(fun_app.to_string(), fun_app.into()))),
        Err(r) => Err(r),
    }
}

fn dot_variable(s: Span) -> IResult<Span, Lang> {
    let res = preceded(tag("."), variable2).parse(s);
    match res {
        Ok((s, Lang::Variable(n, b, c, d))) => Ok((s, Lang::Variable(format!(".{}", n), b, c, d))),
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
        Ok((s, (_start, bloc, _end))) => {
            Ok((s, Lang::VecBlock(bloc.fragment().to_string(), bloc.into())))
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
            Lang::Lambda(
                v.iter().map(|(var, _)| var).cloned().collect(),
                Box::new(body.clone()),
                start.into(),
            ),
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
        Ok((s, (not_op, lang))) => Ok((s, Lang::Not(Box::new(lang), not_op.into()))),
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
    let res = terminated(
        delimited(tag("return "), parse_elements, tag(";")),
        multispace0,
    )
    .parse(s);
    match res {
        Ok((s, el)) => Ok((s, Lang::Return(Box::new(el.clone()), el.into()))),
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

// main
pub fn single_element(s: Span) -> IResult<Span, Lang> {
    alt((
        not_exp,
        tag_exp,
        range,
        lambda,
        primitive,
        js_block,
        return_exp,
        match_exp,
        if_exp,
        dotdotdot,
        array_variant,
        record,
        r_function,
        function,
        tuple_exp,
        function_application,
        array_indexing,
        variable2,
        scope,
        array,
    ))
    .parse(s)
}

pub fn scope(s: Span) -> IResult<Span, Lang> {
    let res = delimited(
        terminated(alt((tag("("), tag("{"))), multispace0),
        opt(base_parse),
        terminated(alt((tag(")"), tag("}"))), multispace0),
    )
    .parse(s);
    match res {
        Ok((s, Some(v))) => Ok((s, Lang::Scope(v.clone(), v.into()))),
        Ok((_s, None)) => panic!("Error: the scope shouldn't be empty"),
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

pub fn elements(s: Span) -> IResult<Span, Lang> {
    let res = many1(alt((single_element_token, element_operator_token))).parse(s);
    match res {
        Ok((s, v)) => {
            if v.len() == 1 {
                Ok((s, v[0].clone().into()))
            } else {
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
    #[should_panic]
    fn test_empty_scope() {
        let _ = "{  }".parse::<Lang>();
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
        dbg!(&res);
        assert_eq!(res.simple_print(), "Operator", "Should parse 1 + 2");
    }

    #[test]
    fn test_addition2() {
        let res = "1 + 2 + 3".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(res.simple_print(), "Operator", "Should parse 1 + 2 + 3");
    }

    #[test]
    fn test_multiplication1() {
        let res = "1 + 2 * 3".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(
            res.simple_print(),
            "Operator",
            "Should put multiplication first 1 + 2 * 3"
        );
    }

    #[test]
    fn test_multiplication2() {
        let res = "1 * 2 + 3".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(
            res.simple_print(),
            "Operator",
            "Should put multiplication first 1 * 2 + 3"
        );
    }

    #[test]
    fn test_multiplication3() {
        let res = "1 * 2 + 3 * 4".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(
            res.simple_print(),
            "Operator",
            "Should put multiplication first 1 * 2 + 3 * 4"
        );
    }

    #[test]
    fn test_accessor1() {
        let res = "3 + personne$age ".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(
            res.simple_print(),
            "Operator",
            "Should put multiplication first 1 * 2 + 3 * 4"
        );
    }

    #[test]
    fn test_and1() {
        let res = "true & true".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(res.simple_print(), "Operator", "Should accept '&&'");
    }

    #[test]
    fn test_array_indexing0() {
        let res = array_indexing("name[1, 2, 3]".into()).unwrap().1;
        dbg!(&res);
        assert!(true);
    }

    #[test]
    fn test_array_indexing() {
        let fp = FluentParser::new().push("name[1, 2, 3]").parse_next();
        println!("fp: {}", fp);
        assert!(true);
    }

    #[test]
    fn test_quoted_variable() {
        let res = quoted_variable("`+`".into()).unwrap().1;
        assert_eq!(res.0, "`+`");
    }

    #[test]
    fn test_uniform_function_call() {
        let res = FluentParser::new().push("true.not()").parse_next();
        dbg!(&res.next_code());
        assert!(true);
    }

    #[test]
    fn test_key_value1() {
        let res = key_value("sep = '3'".into()).unwrap().1;
        dbg!(&res);
        assert!(true);
    }

    #[test]
    fn test_empty_char0() {
        let res = single_element("''".into()).unwrap().1;
        dbg!(&res);
        assert!(true);
    }

    #[test]
    fn test_empty_char1() {
        let res = primitive("''".into()).unwrap().1;
        dbg!(&res);
        assert!(true);
    }

    #[test]
    fn test_empty_char2() {
        let res = chars("''".into()).unwrap().1;
        dbg!(&res);
        assert!(true);
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
        dbg!(&res);
        assert_eq!(res.simple_print(), "Match");
    }

    #[test]
    fn test_match_pattern_with_wildcard() {
        let input = "match x { .Some(a) => a, _ => 0 }";
        let res = match_exp(input.into()).unwrap().1;
        dbg!(&res);
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
        if let Lang::Match(_, branches, _) = &res {
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
        if let Lang::Tag(name, inner, _) = &res {
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
        if let Lang::Tag(name, inner, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
        if let Lang::TypePattern(name, _, _) = &res {
            assert_eq!(name, "x");
        } else {
            panic!("Expected TypePattern variant");
        }
    }

    #[test]
    fn test_type_pattern_bool() {
        let input = "y as bool ";
        let res = type_pattern(input.into()).unwrap().1;
        if let Lang::TypePattern(name, _, _) = &res {
            assert_eq!(name, "y");
        } else {
            panic!("Expected TypePattern variant");
        }
    }

    #[test]
    fn test_type_pattern_num() {
        let input = "val as num ";
        let res = type_pattern(input.into()).unwrap().1;
        if let Lang::TypePattern(name, _, _) = &res {
            assert_eq!(name, "val");
        } else {
            panic!("Expected TypePattern variant");
        }
    }

    #[test]
    fn test_type_pattern_char() {
        let input = "s as char ";
        let res = type_pattern(input.into()).unwrap().1;
        if let Lang::TypePattern(name, _, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
        assert_eq!(
            res.simple_print(),
            "Record",
            "Should parse record pattern as Record"
        );
        if let Lang::List(fields, _) = &res {
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
        if let Lang::List(fields, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
        if let Lang::List(fields, _) = &res {
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
        if let Lang::Tuple(elements, _) = &res {
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
        if let Lang::Tuple(elements, _) = &res {
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
        if let Lang::Tuple(elements, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
        if let Lang::Match(_, branches, _) = &res {
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
}
