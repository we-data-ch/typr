use std::process::exit;
use nom::IResult;
use nom::character::complete::multispace0;
use crate::language::Lang;
use crate::var::Var;
use nom::bytes::complete::tag;
use crate::operators::{Op, op};
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::combinator::opt;
use crate::argument_type::ArgumentType;
use crate::argument_value::ArgumentValue;
use crate::argument_kind::ArgumentKind;
use crate::types::ltype;
use nom::character::complete::one_of;
use nom::character::complete::none_of;
use crate::r#type::Type;
use nom::character::complete::multispace1;
use crate::parser::parse_exp;
use crate::var::Permission;
use nom::character::complete::digit1;
use crate::kind::Kind;
use std::collections::HashSet;
use crate::Context;
use crate::types::label;
use crate::types::if_type;
use nom::sequence::terminated;
use nom::branch::alt;
use nom::sequence::delimited;
use nom::multi::many0;
use nom::multi::many1;
use nom::sequence::preceded;
use nom::Parser;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;
use nom::combinator::recognize;
use crate::help_message::SyntaxError;
use crate::help_message::ErrorMsg;

type Span<'a> = LocatedSpan<&'a str, String>;

fn number_helper(s: Span) -> IResult<Span, Lang> {
    let res = (opt(tag("-")), digit1, tag("."), digit1).parse(s);
    match res {
        Ok((s, (sign, d1, _dot, d2))) => {
            let sign2 = sign.unwrap_or(LocatedSpan::new_extra("", d1.clone().extra));
            let n = format!("{}{}.{}", sign2, d1, d2).parse::<f32>().unwrap();
            Ok((s, Lang::Number(n, sign2.into())))
        },
        Err(r) => Err(r),
    }
}

pub fn number(s: Span) -> IResult<Span,Lang> {
    terminated(number_helper, multispace0).parse(s)
}

fn integer(s: Span) -> IResult<Span, Lang> {
    let res = terminated(digit1, multispace0).parse(s);
    match res {
        Ok((s, d)) => Ok((s, Lang::Integer(d.parse::<i32>().unwrap(), d.into()))),
        Err(r) => Err(r)
    }
}

fn get_value(l: LocatedSpan<&str, String>) -> Lang {
    match l.clone().into_fragment() {
        "true" | "TRUE" => Lang::Bool(true, l.into()),
        "false" | "FALSE" => Lang::Bool(false, l.into()),
        _ => panic!("No other boolean notation alolwed")
    }
}

fn boolean(s: Span) -> IResult<Span,Lang> {
    let res = alt((
                terminated(tag("true"), multispace0),
                terminated(tag("TRUE"), multispace0),
                terminated(tag("false"), multispace0),
                terminated(tag("FALSE"), multispace0),
                  )).parse(s);
    match res {
        Ok((s, ls)) => Ok((s, get_value(ls))),
        Err(r) => Err(r),
    }
}

fn chars(s: Span) -> IResult<Span, Lang> {
    let res = terminated(alt((
            (tag("\""), many0(none_of("\"")), tag("\"")),
            (tag("'"), many0(none_of("'")), tag("'")),
                  )), multispace0).parse(s);
    match res {
        Ok((s, (start, st, _end))) 
            => Ok((s, Lang::Char(st.clone().iter().collect(), start.into()))),
        Err(r) => Err(r)
    }
}

fn starting_char(s: Span) -> IResult<Span, (char, HelpData)> {
    let res = one_of("abcdefghijklmnopqrstuvwxyz_")(s);
    match res {
        Ok((s, val)) => Ok((s.clone(), (val, s.into()))),
        Err(r) => Err(r)
    }
}

fn body_char(s: Span) -> IResult<Span, (char, HelpData)> {
    let res = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")(s);
    match res {
        Ok((s, val)) => Ok((s.clone(), (val, s.into()))),
        Err(r) => Err(r)
    }
}

pub fn variable_exp(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = (starting_char, many0(body_char)).parse(s);
    match res {
        Ok((s, ((s1, h), v))) => {
            let res2 =  v.iter()
                .map(|(val, _h)| val.clone())
                .collect::<String>();
            Ok((s, (format!("{}{}", s1, res2), h.clone())))
        },
        Err(r) => Err(r)
    }
}

fn type_annotation(s: Span) -> IResult<Span, Type> {
    delimited(tag("<"), ltype, tag(">")).parse(s)
}

fn module_path(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = many1(terminated(variable_exp, tag("::"))).parse(s);
    match res {
        Ok((s, v)) => {
            let res = v.iter()
                .map(|(name, _)| name.clone())
                .collect::<Vec<_>>()
                .join("/");
            Ok((s, (res, v[0].1.clone())))
        },
        Err(r) => Err(r)
    }
}

fn variable_helper(s: Span) -> IResult<Span, Lang> {
    let res = (opt(module_path), variable_exp, opt(type_annotation)).parse(s);
    match res {
        Ok((s, (Some(mp), (v, h), Some(ty)))) 
            => Ok((s, Var::from_name(&v)
                   .set_path(mp.0.into())
                   .set_type(ty)
                   .set_help_data(h.clone()).into())),
        Ok((s, (None, (v, h), Some(ty)))) 
            => Ok((s, Var::from_name(&v)
                        .set_type(ty)
                        .set_help_data(h).into())),
        Ok((s, (Some(mp), (v, h), None))) 
            => Ok((s.clone(), Var::from_name(&v)
                        .set_path(mp.0.into())
                        .set_help_data(h)
                        .into())),
        Ok((s, (None, (v, h), None))) 
            => Ok((s.clone(), Var::from_name(&v)
                        .set_help_data(h).into())),
        Err(r) => Err(r)
    }
}

pub fn variable(s: Span) -> IResult<Span, Lang> {
    terminated(variable_helper, multispace0).parse(s)
}


pub fn argument(s: Span) -> IResult<Span, ArgumentType> {
    let res = (
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        ltype,
        opt(terminated(tag(","), multispace0))
                ).parse(s);
    match res {
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentType(e1, e2, false))),
        Err(r) => Err(r)
    }
}

fn equality_params(s: Span) -> IResult<Span, Span> {
    terminated(alt((tag(":"), tag("="))), multispace0).parse(s)
}

fn argument_val(s: Span) -> IResult<Span, ArgumentValue> {
    let res = (
        terminated(alphanumeric1, multispace0),
        equality_params,
        single_element,
        opt(terminated(tag(","), multispace0))
                ).parse(s);
    match res {
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentValue(e1.to_string(), e2))),
        Err(r) => Err(r)
    }
}

fn get_kind(ls: LocatedSpan<&str, String>) -> Kind {
    match ls.into_fragment() {
        "Type" => Kind::Type,
        "Kind" => Kind::Dim,
        _ => panic!("No other string for Kinds allowed")
    }
}

fn lkind(s: Span) -> IResult<Span, Kind> {
    let res = alt((tag("Type"), tag("Dim"))).parse(s);
    match res {
        Ok((s, ls)) => Ok((s, get_kind(ls))),
        Err(r) => Err(r),
    }
}

pub fn argument_kind(s: Span) -> IResult<Span, ArgumentKind> {
    let res = (
        ltype,
        terminated(tag(":"), multispace0),
        lkind,
        opt(terminated(tag(","), multispace0))
                ).parse(s);
    match res {
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentKind(e1, e2))),
        Err(r) => Err(r)
    }
}

pub fn function_symbol(s: Span) -> IResult<Span, Span> {
    alt((tag("function"), tag("func"), tag("fn"))).parse(s)
}

fn extract_generics(args: &[ArgumentType], ret_typ: &Type) -> Vec<ArgumentKind> {
    args.iter()
        .map(|at| at.get_type())
        .chain([ret_typ.clone()].iter().cloned())
        .flat_map(|typ| typ.extract_generics())
        .collect::<HashSet<_>>()
        .into_iter()
        .map(|typ| ArgumentKind::from((typ.clone(), typ.get_kind())))
        .collect::<Vec<_>>()
}

pub fn r_function(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(function_symbol, multispace0),
        terminated(tag("("), multispace0),
        many0(terminated(terminated(variable, opt(tag(","))), multispace0)),
        terminated(tag(")"), multispace0),
        recognize(scope)
          ).parse(s);
    match res {
        Ok((_s, (id, _op, _args, _cl, _exp))) 
            if *id.fragment() == "fn" => {
                 panic!("{}", SyntaxError::FunctionWithoutType(id.into()).display())
        },
        Ok((s, (id, _op, args, _cl, exp))) =>{
            Ok((s, Lang::RFunction(args, exp.to_string(), id.into())))
        },
        Err(r) => Err(r)
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
        scope
          ).parse(s);
    match res {
        Ok((s, (_, _, args, _, Some(_), Some(typ), exp))) =>{
            let gen_vec = extract_generics(&args, &typ);
            Ok((s, Lang::Function(gen_vec, args, typ, Box::new(exp), HelpData::default())))
        },
        Ok((_s, (_, _, _args, _cp, None, None, _exp))) 
            => {
                panic!("You forgot to specify the function return type: 'fn(...): Type'");
            }, 
        Ok((_s, (_, _, _args, _, Some(_), None, _exp))) 
            => {
            println!("Hey You forgot to specify the function return type after the ':' : 'fn(...): Type'");
            exit(1)
            },
        Ok((_s, (_, _, _args, _, None, Some(typ), _exp))) 
            => {
                println!(
                    "The type '{}' should be preceded by a ':' :\n 'fn(...): {}'", 
                    typ.clone(),
                    typ.clone());
                exit(1)
            }
        Err(r) => Err(r)
    }
}

fn complex_function(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("fn"), multispace0),
        terminated(tag("<"), multispace0),
        many0(argument_kind),
        terminated(tag(">"), multispace0),
        terminated(tag("("), multispace0),
        many0(argument),
        terminated(tag(")"), multispace0),
        terminated(tag(":"), multispace0),
        terminated(ltype, multispace0),
        scope
          ).parse(s);
    match res {
        Ok((s, (start, _, arg_kinds, _, _, args, _, _, typ, exp))) => 
            Ok((s, Lang::Function(arg_kinds, args, typ, Box::new(exp), start.into()))),
        Err(r) => Err(r)
    }
}

fn function(s: Span) -> IResult<Span, Lang> {
    alt((
        simple_function,
        complex_function
    )).parse(s)
}

fn values(s: Span) -> IResult<Span, Vec<Lang>> {
    many0(
        terminated(
            parse_elements,
            terminated(opt(tag(",")), multispace0))).parse(s)
}

fn array_indexing(s: Span) -> IResult<Span, Lang> {
    let res = (
            alt((scope, variable)),
            terminated(tag("["), multispace0),
            number,
            terminated(tag("]"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (exp, _, Lang::Number(n, _), _))) 
            => Ok((s, Lang::ArrayIndexing(Box::new(exp.clone()), n, exp.into()))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn function_application(s: Span) -> IResult<Span, Lang> {
    let res = (
            alt((scope, variable)),
            terminated(tag("("), multispace0),
            values,
            terminated(tag(")"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (exp, _, v, _))) 
            => Ok((s, Lang::FunctionApp(Box::new(exp.clone()), v.clone(), exp.into()))),
        Err(r) => Err(r)
    }
}

fn array(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("["), multispace0),
            values,
            terminated(tag("]"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, Lang::Array(v.clone(), v.into()))),
        Err(r) => Err(r)
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
        terminated(alt((tag("}"), tag(")"))), multispace0)).parse(s);
    match res {
        Ok((s, (Some(start), _, args, _))) 
            => Ok((s, Lang::Record(args.clone(), start.into()))),
        Ok((_s, (None, _ob, _args, _))) => {
            panic!("You forgot to put a record identifier before the bracket: ':{{...}}'");
        } 
        Err(r) => Err(r)
    }

}

fn pascal_case_helper(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = (one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alpha1).parse(s);
    match res {
        Ok((s, (t1, t2))) 
            => Ok((s.clone(), (format!("{}{}", t1, t2), s.into()))),
        Err(r) => Err(r)
    }
}

fn pascal_case(s: Span) -> IResult<Span, (String, HelpData)> {
    terminated(pascal_case_helper, multispace0).parse(s)
}

fn parenthese_value(s: Span) -> IResult<Span, Lang> {
    delimited(
            terminated(tag("("), multispace0),
            parse_elements,
            terminated(tag(")"), multispace0)
          ).parse(s)
}

pub fn tag_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
            pascal_case,
            opt(parenthese_value)).parse(s);
    match res {
        Ok((s, ((n, h), None))) 
            => Ok((s, Lang::Tag(n, Box::new(Lang::Empty(h.clone())), h))),
        Ok((s, ((n, h), Some(val)))) 
            => Ok((s, Lang::Tag(n, Box::new(val), h))),
        Err(r) => Err(r)
    }
}


fn dotdotdot(s: Span) -> IResult<Span, Lang> {
    let res = terminated(tag("..."), multispace0).parse(s);
    match res {
        Ok((s, d)) => Ok((s, Lang::Empty(d.into()))),
        Err(r) => Err(r)
    }
}

fn else_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("else"), multispace0),
            terminated(tag("{"), multispace0),
            parse_elements,
            terminated(tag("}"), multispace0),
                    ).parse(s);
    match res {
        Ok((s, (_else, _o, exp, _c))) 
            => Ok((s, exp)),
        Err(r) => Err(r)
    }
}

fn else_if_exp(s: Span) -> IResult<Span, Lang> {
    preceded(
            terminated(tag("else"), multispace1),
            if_exp).parse(s)
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
            opt(alt((else_if_exp, else_exp)))
                    ).parse(s);
    match res {
        Ok((s, (_if, _op, cond, _cp, _o, exp, _c, els))) 
            => Ok((s, 
                   Lang::If(
                       Box::new(cond),
                       Box::new(exp),
                       Box::new(els.unwrap_or(Lang::Empty(HelpData::default()))),
                       _if.into()))),
        Err(r) => Err(r)
    }
}


fn branch(s: Span) -> IResult<Span, (Box<Lang>, Box<Lang>)> {
    let res = (
            terminated(tag_exp, multispace0),
            terminated(tag("=>"), multispace0),
            terminated(single_element, multispace0),
            opt(terminated(tag(","), multispace0)),
                    ).parse(s);
    match res {
        Ok((s, (part1, _arr, part2, _vir)))
            => Ok((s, (Box::new(part1), Box::new(part2)))),
        Err(r) => Err(r)
    }
}

fn match_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("match"), multispace0),
            variable,
            terminated(tag("{"), multispace0),
            many0(branch),
            terminated(tag("}"), multispace0),
                    ).parse(s);
    match res {
        Ok((s, (_m, val, _o, bs, _c))) 
            => Ok((s, Lang::Match(Box::new(val), bs, _m.into()))),
        Err(r) => Err(r)
    }
}

fn tuple_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(alt((tag("list"), tag(":"))), multispace0),
            terminated(alt((tag("{"), tag("("))), multispace0),
            values,
            terminated(alt((tag("}"), tag(")"))), multispace0),
                    ).parse(s);
    match res {
        Ok((s, (id, _op, vals, _cl))) => 
            Ok((s, Lang::Tuple(vals, id.into()))),
        Err(r) => Err(r)
    }
}

fn int_or_var(s: Span) -> IResult<Span, Lang> {
    alt((integer, variable)).parse(s)
}

fn create_range(params: &[Lang]) -> Lang {
    if params.len() == 2 {
        Lang::FunctionApp(
           Box::new(Var::from_name("seq").to_language()),
           vec![params[0].clone(), params[1].clone(), Lang::Integer(1, HelpData::default())],
           params.to_vec().into())
    } else {
        Lang::FunctionApp(
           Box::new(Var::from_name("seq").to_language()),
           vec![params[0].clone(), params[1].clone(), params[2].clone()],
           params.to_vec().into())
    }
}

fn range(s: Span) -> IResult<Span, Lang> {
    let res = (
            int_or_var,
            tag(":"),
            opt(terminated(int_or_var, tag(":"))),
            int_or_var).parse(s);
    //from_name().to_language()
    match res {
        Ok((s, (iv1, _sep, None, iv2))) 
            => Ok((s, create_range(&[iv1.clone(), iv2.clone()]))),
        Ok((s, (iv1, _sep, Some(iv0), iv2)))
            => Ok((s, create_range(&[iv1.clone(), iv2.clone(), iv0.clone()]))),
        Err(r) => Err(r),
    }
}

fn pure_string((lang, op): (Lang, Op)) -> String {
    let cont = Context::new(vec![], vec![]);
    match (lang, op) {
        (lang, Op::Empty(_)) => lang.to_r(&cont).0,
        (lang, Op::Add(_)) => format!(" + {}", lang.to_r(&cont).0),
        (lang, Op::Add2(_)) => format!(" ++ {}", lang.to_r(&cont).0),
        (lang, Op::Minus(_)) => format!(" - {}", lang.to_r(&cont).0),
        (lang, Op::Minus2(_)) => format!(" -- {}", lang.to_r(&cont).0),
        (lang, Op::Mul(_)) => format!(" * {}", lang.to_r(&cont).0),
        (lang, Op::Mul2(_)) => format!(" ** {}", lang.to_r(&cont).0),
        (lang, Op::Div(_)) => format!(" / {}", lang.to_r(&cont).0),
        (lang, Op::Div2(_)) => format!(" // {}", lang.to_r(&cont).0),
        (lang, Op::Modu(_)) => format!(" % {}", lang.to_r(&cont).0),
        (lang, Op::Modu2(_)) => format!(" %% {}", lang.to_r(&cont).0),
        (lang, Op::Eq(_)) => format!(" == {}", lang.to_r(&cont).0),
        (lang, Op::Eq2(_)) => format!(" = {}", lang.to_r(&cont).0),
        (lang, Op::NotEq(_)) => format!(" != {}", lang.to_r(&cont).0),
        (lang, Op::And(_)) => format!(" && {}", lang.to_r(&cont).0),
        (lang, Op::Or(_)) => format!(" || {}", lang.to_r(&cont).0),
        (lang, Op::Pipe(_)) => format!(" |> {}", lang.to_r(&cont).0),
        (lang, Op::Pipe2(_)) => format!(" |>> {}", lang.to_r(&cont).0),
        (lang, Op::Dot(_)) => format!(" . {}", lang.to_r(&cont).0),
        (lang, Op::Dot2(_)) => format!(" .. {}", lang.to_r(&cont).0),
        (lang, Op::Union(_)) => format!(" | {}", lang.to_r(&cont).0),
        (lang, Op::In(_)) => format!(" in {}", lang.to_r(&cont).0),
        (lang, Op::At(_)) => format!(" @ {}", lang.to_r(&cont).0),
        (lang, Op::At2(_)) => format!(" @@ {}", lang.to_r(&cont).0),
        (lang, Op::LesserThan(_)) => format!(" < {}", lang.to_r(&cont).0),
        (lang, Op::GreaterThan(_)) => format!(" > {}", lang.to_r(&cont).0),
        (lang, Op::LesserOrEqual(_)) => format!(" <= {}", lang.to_r(&cont).0),
        (lang, Op::GreaterOrEqual(_)) => format!(" >= {}", lang.to_r(&cont).0)
    }
}

fn function_application2(s: Span) -> IResult<Span, Lang> {
    let res = recognize(function_application).parse(s);
    match res {
        Ok((s, fun_app)) => Ok((s, Lang::Exp(fun_app.to_string(), fun_app.into()))),
        Err(r) => Err(r)
    }
}

fn element_operator2(s: Span) -> IResult<Span, (Lang, Op)> {
    let res = (opt(op),
                alt((function_application2, number, integer, chars, boolean, variable))
                ).parse(s);
    match res {
        Ok((s, (Some(ope), ele))) => Ok((s, (ele, ope))),
        Ok((s, (None, ele))) => Ok((s.clone(), (ele, Op::Empty(s.into())))),
        Err(r) => Err(r)
    }
}

fn vectorial_bloc(s: Span) -> IResult<Span, Lang> {
    let res =   (
                    terminated(tag("@{"), multispace0),
                    many1(element_operator2),
                    terminated(tag("}@"), multispace0),
                    ).parse(s);
    match res {
        Ok((s, (start, v, _end))) => {
            let new_v = v.clone().into_iter()
                .map(|e| pure_string(e))
                .collect::<Vec<_>>().join("");
            Ok((s, Lang::VecBloc(new_v.clone(), start.into())))
        },
        Err(r) => Err(r)
    }
}

fn lambda(s: Span) -> IResult<Span, Lang> {
    let res = preceded(tag("~"), element_chain).parse(s);
    match res {
        Ok((s, e)) 
            => Ok((s, Lang::Lambda(Box::new(e.clone()), e.into()))),
        Err(r) => Err(r)
    }
}


// main
pub fn single_element(s: Span) -> IResult<Span,Lang> {
    alt((
            range,
            lambda,
            boolean,
            number,
            integer,
            chars,
            match_exp,
            if_exp,
            dotdotdot,
            r_function,
            function,
            tuple_exp,
            record,
            function_application,
            array_indexing,
            variable,
            tag_exp,
            scope,
            array
        )).parse(s)
}

pub fn scope(s: Span) -> IResult<Span, Lang> {
    let res = delimited(
        terminated(alt((tag("("), tag("{"))), multispace0),
        parse_exp,
        terminated(alt((tag(")"), tag("}"))), multispace0)).parse(s);
    match res {
        Ok((s, Lang::Empty(h))) => Ok((s, Lang::Scope(vec![], h.clone()))),
        Ok((s, Lang::Sequence(v, _h))) 
            => Ok((s, Lang::Scope(v.clone(), v.into()))),
        Ok((s, rest)) => Ok((s, Lang::Scope(vec![rest.clone()], rest.into()))),
        Err(r) => Err(r),
    }
}



fn op_reverse(v: &mut Vec<(Lang, Op)>) -> Lang {
    // (params, op)
    let first = v.pop().expect(&format!("The vector v is empty {:?}", v));
    match first {
        (p, Op::In(_)) 
			=> Lang::In(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::And(_)) 
			=> Lang::And(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Or(_)) 
			=> Lang::Or(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Union(_)) 
			=> Lang::Union(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Eq(_)) 
			=> Lang::Eq(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Eq2(_)) 
			=> Lang::Eq2(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::NotEq(_)) 
			=> Lang::NotEq(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::LesserThan(_)) 
			=> Lang::LesserThan(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::GreaterThan(_)) 
			=> Lang::GreaterThan(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::LesserOrEqual(_)) 
			=> Lang::LesserOrEqual(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::GreaterOrEqual(_)) 
			=> Lang::GreaterOrEqual(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Modu(_)) 
            => Lang::Modu(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Modu2(_)) 
            => Lang::Modu2(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (Lang::FunctionApp(name, params, h1), Op::Pipe(_h2)) 
            => { // (UFC) add the "object" as te first parameter of the function call
                let res = [op_reverse(v)].iter().chain(params.iter()).cloned().collect::<Vec<_>>();
                Lang::FunctionApp(name, res, h1.clone())
            }
        (p, Op::Pipe(_)) => Lang::Chain(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Pipe2(_)) => {
            let res = match p.clone() {
                Lang::FunctionApp(name, _, _) => *name.clone(),
                rest => rest.clone()
            };
            let func = Lang::FunctionApp(
                Box::new(Lang::Variable("map".to_string(), "".into(), Permission::Private, false, Type::Empty(HelpData::default()), HelpData::default())),
                vec![res.clone()], res.into());
            Lang::Chain(Box::new(func), Box::new(op_reverse(v)), p.into())
        },
        (p, Op::Add(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("add")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::Add2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("add2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::Minus(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("minus")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::Minus2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("minus2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::Mul(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("mul")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::Mul2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("mul2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::Div(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("div")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::Div2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("div2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::At(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("at")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (p, Op::At2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("at2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], res.into()) },
        (Lang::FunctionApp(name, params, h1), Op::Dot(_h2)) 
            => { // (UFC) add the "object" as te first parameter of the function call
                let res = [op_reverse(v)].iter().chain(params.iter()).cloned().collect::<Vec<_>>();
                Lang::FunctionApp(name, res, h1.clone())
            }
        (p, Op::Dot(_)) => Lang::Chain(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Dot2(_)) => {
            let res = match p.clone() {
                Lang::FunctionApp(name, _, _) => *name.clone(),
                rest => rest.clone()
            };
            let func = Lang::FunctionApp(
                Box::new(Var::from_name("map").to_language()),
                vec![res.clone()], res.into());
            Lang::Chain(Box::new(func), Box::new(op_reverse(v)), p.into())
        },
        (p, Op::Empty(_)) => p
    }
}

fn element_operator(s: Span) -> IResult<Span, (Lang, Op)> {
    let res = (opt(op),
                single_element
                ).parse(s);
    match res {
        Ok((s, (Some(ope), ele))) => Ok((s, (ele, ope))),
        Ok((s, (None, ele))) => Ok((s.clone(), (ele, Op::Empty(s.into())))),
        Err(r) => Err(r)
    }
}

pub fn bang_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        many1(element_operator),
        terminated(tag("!;"), multispace0)
                    ).parse(s);
    match res {
        Ok((s, (v, _bang))) => {
            let base = v[0].0.clone();
            Ok((s, 
                Lang::Assign(
                    Box::new(base),
                    Box::new(op_reverse(&mut v.clone())),
                    _bang.into())))
        },
        Err(r) => Err(r)
    }
}

fn check_minus_sign(v: Vec<(Lang, Op)>) -> Vec<(Lang, Op)> {
    if v.len() > 0 {
        let mut v2 = v.clone();
        let (lang, op) = v2.first().unwrap();
        let first = match (op.clone(), lang.clone()) {
            (Op::Minus(h1), Lang::Integer(l, h)) => (Lang::Integer(-l, h), Op::Empty(h1)),
            (Op::Minus(h1), Lang::Number(l, h)) => (Lang::Number(-l, h), Op::Empty(h1)),
            _ => (lang.clone(), op.clone())
        };
        v2.insert(0, first); v2
    } else { v.clone() }
}

fn accessor(s: Span) -> IResult<Span, (Lang, Op)> {
    let res = (
                terminated(tag("[["), multispace0),
                alt((integer, chars, variable)),
                terminated(tag("]]"), multispace0)
              ).parse(s);
    match res {
        Ok((s, (op, content, _cl))) => Ok((s, (content, Op::Dot(op.into())))),
        Err(r) => Err(r)
    }
}

fn element_chain(s: Span) -> IResult<Span, Lang> {
    let res = many1(alt((accessor, element_operator))).parse(s);
    match res {
        Ok((s, v)) => {
            let v2 = check_minus_sign(v);
            Ok((s, op_reverse(&mut v2.clone())))
        },
        Err(r) => Err(r)
    }
}

// main
pub fn parse_elements(s: Span) -> IResult<Span, Lang> {
    alt((
        vectorial_bloc,
        element_chain,
        single_element
        )).parse(s)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder;
    use std::backtrace::Backtrace;

    #[test]
    fn test_single_element() {
        let res = single_element("4;".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_sop0() {
        let res = op("+".into()).unwrap().1;
        assert_eq!(res, Op::Add(HelpData::default()));
    }

    #[test]
    fn test_el_op0() {
        let res = element_operator("+ 2".into()).unwrap().1;
        assert_eq!(res, (builder::empty_lang(), Op::And(HelpData::default())));
    }

    #[test]
    fn test_el_op1() {
        let res = element_operator("2".into()).unwrap().1;
        assert_eq!(res, (builder::empty_lang(), Op::And(HelpData::default())));
    }

    #[test]
    fn test_chain0() {
        let res = element_chain("2".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_chain1() {
        let res = element_chain("2 + 9".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_chain2() {
        let res = element_chain("3 + 2 + 4 + 7 + 6".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_chain3() {
        let res = element_chain("3 .add(2).add(4).add(7).add(6)".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_chain4() {
        let res = element_chain("7 .combine(2, 3).okay(true)".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }
    
    #[test]
    fn test_chain5() {
        let res = single_element("combine(3, 2, 3)".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_function_no_parameters(){
        let res = function("fn () : Number { 7 }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_function_with_parameters(){
        let res = function("fn (a: number, b: bool) : Number { 7 }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_parse_function1() {
        let res = single_element("fn (a: num, b: bool) : num {...}".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }
    #[test]
    fn test_parse_function2() {
        let res = single_element("fn (a: int): .Some({char, char}) { Some(:{'un', 'hello'}) };".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_parse_elements_function() {
        let res = parse_elements("fn (a: number, b: bool) : Number { 7 }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_function_application1() {
        let res = function_application("incr()".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_function_application2() {
        let res = single_element("incr()".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_function_application3() {
        let res = parse_elements("incr()".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_function_application4() {
        let res = parse_elements("{7}()".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_function_application5() {
        let res = parse_elements("{7}(4)".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_function_application6() {
        let res = function_application("{7}(4)".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_scope1() {
        let res = scope("{7}".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_scope2() {
        let res = scope("{let a <- 7;}".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_scope3() {
        let res = scope("{let a = 7; a;}".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_array() {
        let res = array("[1, 2, 3]".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_char1() {
        let res = chars("\"Hello world\"".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_char2() {
        let res = single_element("'Hello world'".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_if1() {
        let res = if_exp("if (true) { 7 }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_if2() {
        let res = parse_elements("if (true) { 7 }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_if3() {
        let res = parse_elements("if num < 18 { true } else { false }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_branch1() {
        let res = branch("True => 3".into()).unwrap().1;
        assert_eq!(res, (Box::new(builder::empty_lang()), Box::new(builder::empty_lang())));
    }

    #[test]
    fn test_branch2() {
        let res = branch("Int(i) => 3".into()).unwrap().1;
        assert_eq!(res, (Box::new(builder::empty_lang()), Box::new(builder::empty_lang())));
    }

    #[test]
    fn test_match1() {
        let res = match_exp("match a { True => 3, False => 4, }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_match2() {
        let res = match_exp("match res { None => true, Some(t) => false }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_variable1() {
        let res = variable("hey".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_variable2() {
        let res = variable("Person::hey".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_variable3() {
        let res = variable("Person::Course::hey".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_var_tag1() {
        let res = single_element("Some(s.substr(a.len() + 1, s.len()))".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_var_tag2() {
        let res = tag_exp("Some(s.substr(a.len() + 1, s.len()))".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_my_element1() {
        let res = single_element("s.substr(a.len() + 1, s.len())".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_func_appli1() {
        let res = parse_elements("Mod::new(7)".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_func_appli2() {
        let res = parse_elements("Count::new(7)".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_index_function() {
        let res = single_element("fn(a: #N): [#N, int] { ... }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_range1() {
        let res = range("1:3".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }
    
    #[test]
    fn test_range2() {
        let res = single_element("1:3".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_range3() {
        let res = parse_elements("1:3".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_range4() {
        let res = parse_elements("1:a".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_range5() {
        let res = element_chain("1:a".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_shape0() {
        let res = single_element("[[1, 2], [3, 4]]".into()).unwrap().1;
        assert_eq!(res.shape(), vec![0 as usize]);
    }

    #[test]
    fn test_fn_op1() {
        let res = single_element("fn(n: int): bool { 3 >= n }".into()).unwrap().1;
        assert_eq!(res.shape(), vec![0 as usize]);
    }

    #[test]
    fn test_parse_greater_than1() {
        let res = element_chain("3 <= n".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_parse_greater_than2() {
        let res = element_operator("<= 3".into()).unwrap().1;
        assert_eq!(res, (builder::empty_lang(), Op::Empty(HelpData::default())));
    }

    #[test]
    fn test_chain_string0() {
        let res = element_chain("'wow' + 'hey'".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    } 

    #[test]
    fn test_chain_struct() {
        let res = element_chain("5.:{x: 0}".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    } 

    #[test]
    fn test_element_operator2() {
        let res = element_operator2("3".into()).unwrap().1;
        assert_eq!(res, (builder::empty_lang(), Op::Empty(HelpData::default())));
    }

    #[test]
    fn test_element_operator2_2() {
        let res = element_operator2("+ 4".into()).unwrap().1;
        assert_eq!(res, (builder::empty_lang(), Op::Empty(HelpData::default())));
    }

    #[test]
    fn test_vectorial_bloc1() {
        let res = vectorial_bloc("@{ 3 }@".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_vectorial_bloc2() {
        let res = vectorial_bloc("@{ 3 + 4 }@".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_vectorial_bloc3() {
        let res = vectorial_bloc("@{ 4*v + 12 }@".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_vectorial_bloc4() {
        let res = single_element("@{ 4*v + 12 }@".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_vectorial_bloc5() {
        let res = element_chain("@{ 4*v + 12 }@".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_vectorial_bloc6() {
        let res = vectorial_bloc("@{sep = ' '}@".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_vectorial_bloc7() {
        let res = vectorial_bloc("@{dates = paste(dates, '3')}@".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_variable_field() {
        let res = element_chain("p.x".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_lang_alias0() {
        let res = element_chain("x + 1".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_lang_alias1() {
        let res = lambda("$x + 1".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_r_function0() {
        let res = r_function("fn(a, b) { a + b }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }



}
