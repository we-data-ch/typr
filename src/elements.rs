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
use std::collections::HashSet;
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
use nom::bytes::complete::take_while1;
use crate::builder;
use crate::types::single_type;

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

pub fn chars(s: Span) -> IResult<Span, Lang> {
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
    let res = (opt(module_path), alt((pascal_case, variable_exp)), opt(type_annotation)).parse(s);
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
    terminated(alt((tag("="), tag(":"))), multispace0).parse(s)
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
    )).parse(input)
}

pub fn r_function(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(alt((tag("function"), tag("\\"))), multispace0),
        terminated(tag("("), multispace0),
        many0(terminated(terminated(variable, opt(tag(","))), multispace0)),
        terminated(tag(")"), multispace0),
        terminated(parse_block, multispace0)
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
        alt((scope, parse_elements))
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

fn function(s: Span) -> IResult<Span, Lang> {
        simple_function.parse(s)
}

fn key_value(s: Span) -> IResult<Span, Lang> {
    let res = (recognize(variable),
                terminated(tag("="), multispace0),
                single_element).parse(s);
    match res {
        Ok((s, (v, _eq, el))) 
            => Ok((s, Lang::KeyValue((*v).into(), Box::new(el), v.into()))),
        Err(r) => Err(r)
    }
}

fn values(s: Span) -> IResult<Span, Vec<Lang>> {
    many0(
        terminated(
            alt((key_value, parse_elements)),
            terminated(opt(tag(",")), multispace0))).parse(s)
}

fn array_indexing(s: Span) -> IResult<Span, Lang> {
    let res = (
            alt((scope, variable)),
            terminated(tag("["), multispace0),
            alt((integer, variable)),
            terminated(tag("]"), multispace0),
            multispace0
          ).parse(s);
    match res {
        Ok((s, (exp, _, int_var, _, _))) 
            => Ok((s, Lang::ArrayIndexing(Box::new(exp.clone()), Box::new(int_var), exp.into()))),
        Err(r) => Err(r),
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
            => Ok((s, Lang::FunctionApp(Box::new(exp.clone()), v.clone(), builder::empty_type(), exp.into()))),
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

pub fn vector(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("c("), multispace0),
            values,
            terminated(tag(")"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, Lang::Vector(v.clone(), v.into()))),
        Err(r) => Err(r)
    }
}

fn sequence(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("seq["), multispace0),
            values,
            terminated(tag("]"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, Lang::Sequence(v.clone(), v.into()))),
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
            println!("{}", _s);
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
    //terminated(pascal_case_helper, multispace0).parse(s)
    pascal_case_helper.parse(s)
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
            tag("."),
            pascal_case,
            opt(parenthese_value)).parse(s);
    match res {
        Ok((s, (dot, (n, _h), None))) 
            => Ok((s, Lang::Tag(n, Box::new(Lang::Empty(dot.clone().into())), dot.into()))),
        Ok((s, (dot, (n, _h), Some(val)))) 
            => Ok((s, Lang::Tag(n, Box::new(val), dot.into()))),
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


fn branch(s: Span) -> IResult<Span, (Type, Box<Lang>)> {
    let res = (
            terminated(single_type, multispace0),
            terminated(tag("=>"), multispace0),
            terminated(parse_elements, multispace0),
            opt(terminated(tag(","), multispace0)),
                    ).parse(s);
    match res {
        Ok((s, (typ, _arr, lang, _vir)))
            => Ok((s, (typ, Box::new(lang)))),
        Err(r) => Err(r)
    }
}

fn match_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("match"), multispace0),
            alt((variable, element_chain)),
            terminated(tag("as"), multispace0),
            variable,
            terminated(tag("{"), multispace0),
            many1(branch),
            terminated(tag("}"), multispace0),
                    ).parse(s);
    match res {
        Ok((s, (_m, exp, _as, var, _o, bs, _c))) 
            => Ok((s, Lang::Match(Box::new(exp), Var::try_from(var).unwrap(), bs, _m.into()))),
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
           builder::empty_type(),
           params.to_vec().into())
    } else {
        Lang::FunctionApp(
           Box::new(Var::from_name("seq").to_language()),
           vec![params[0].clone(), params[1].clone(), params[2].clone()],
           builder::empty_type(),
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

fn function_application2(s: Span) -> IResult<Span, Lang> {
    let res = recognize(function_application).parse(s);
    match res {
        Ok((s, fun_app)) => Ok((s, Lang::Exp(fun_app.to_string(), fun_app.into()))),
        Err(r) => Err(r)
    }
}

fn dot_variable(s: Span) -> IResult<Span, Lang> {
    let res = preceded(tag("."), variable).parse(s);
    match res {
        Ok((s, Lang::Variable(n, a, b, c, d, e))) 
            => Ok((s, Lang::Variable(format!(".{}", n), a, b, c, d, e))),
        Ok((_s, _)) => todo!(),
        Err(r) => Err(r)
    }
}

fn element_operator2(s: Span) -> IResult<Span, (Lang, Op)> {
    let res = (opt(op),
                alt((function_application2, number, integer, chars, boolean, variable, dot_variable))
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
                    recognize(many1(element_operator2)),
                    terminated(tag("}@"), multispace0),
                    ).parse(s);
    match res {
        Ok((s, (_start, bloc, _end))) => {
            Ok((s, Lang::VecBlock(bloc.fragment().to_string(), bloc.into())))
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

fn not_exp(s: Span) -> IResult<Span, Lang> {
    let res = (tag("!"),
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
            variable,
            scope,
            array
        ))).parse(s);
    match res {
        Ok((s, (not_op, lang))) => Ok((s, Lang::Not(Box::new(lang), not_op.into()))),
        Err(r) => Err(r)
    }
}

fn array_variant(s: Span) -> IResult<Span, Lang> {
    alt((vector, sequence)).parse(s)
}

fn js_block(s: Span) -> IResult<Span, Lang> {
    let res = (terminated(tag("JS"), multispace0),
     scope).parse(s);

    match res {
        Ok((s, (js, body))) 
            => Ok((s, Lang::JSBlock(Box::new(body), 0, js.into()))),
        Err(r) => Err(r)
    }
}

fn primitive(s: Span) -> IResult<Span, Lang> {
    alt((
        boolean,
        number,
        integer,
        chars,
        )).parse(s)
}

pub fn return_exp(s: Span) -> IResult<Span, Lang> {
    let res = terminated(delimited(tag("return "), parse_elements, tag(";")), multispace0).parse(s);
    match res {
        Ok((s, el)) 
            => Ok((s, Lang::Return(Box::new(el.clone()), el.into()))),
        Err(r) => Err(r)
    }
}

pub fn break_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = tag("break;").parse(s);
    match res {
        Ok((s, el)) 
            => Ok((s, vec![Lang::Break(el.into())])),
        Err(r) => Err(r)
    }
}

// main
pub fn single_element(s: Span) -> IResult<Span,Lang> {
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
            variable,
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
        Ok((s, Lang::Lines(v, _h))) 
            => Ok((s, Lang::Scope(v.clone(), v.into()))),
        Ok((s, rest)) => Ok((s, Lang::Scope(vec![rest.clone()], rest.into()))),
        Err(r) => Err(r),
    }
}



fn op_reverse(v: &mut Vec<(Lang, Op)>) -> Lang {
    // (params, op)
    let first = v.pop()
        .unwrap_or((Lang::Empty(HelpData::default()), Op::Empty(HelpData::default())));
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
        (Lang::FunctionApp(name, params, fn_typ, h1), Op::Pipe(_h2)) 
            => { // (UFC) add the "object" as te first parameter of the function call
                let res = [op_reverse(v)].iter().chain(params.iter()).cloned().collect::<Vec<_>>();
                Lang::FunctionApp(name, res, fn_typ, h1.clone())
            }
        (p, Op::Pipe(_)) => Lang::Chain(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Pipe2(_)) => {
            let res = match p.clone() {
                Lang::FunctionApp(name, _, _, _) => *name.clone(),
                rest => rest.clone()
            };
            let func = Lang::FunctionApp(
                Box::new(Lang::Variable("map".to_string(), "".into(), Permission::Private, false, Type::Empty(HelpData::default()), HelpData::default())),
                vec![res.clone()], builder::empty_type(), res.into());
            Lang::Chain(Box::new(func), Box::new(op_reverse(v)), p.into())
        },
        (p, Op::Add(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("add")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::Add2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("add2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::Minus(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("minus")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::Minus2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("minus2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::Mul(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("mul")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::Mul2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("mul2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::Div(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("div")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::Div2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("div2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::At(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("at")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (p, Op::At2(h)) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name("at2")
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
        (Lang::FunctionApp(name, params, fn_typ, h1), Op::Dot(_h2)) 
            => { // (UFC) add the "object" as te first parameter of the function call
                let res = [op_reverse(v)].iter().chain(params.iter()).cloned().collect::<Vec<_>>();
                Lang::FunctionApp(name, res, fn_typ, h1.clone())
            }
        (p, Op::Dot(_)) => Lang::Chain(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Dot2(_)) => {
            let res = match p.clone() {
                Lang::FunctionApp(name, _, _, _) => *name.clone(),
                rest => rest.clone()
            };
            let func = Lang::FunctionApp(
                Box::new(Var::from_name("map").to_language()),
                vec![res.clone()], builder::empty_type(), res.into());
            Lang::Chain(Box::new(func), Box::new(op_reverse(v)), p.into())
        },
        (p, Op::Dollar(_)) => Lang::Dollar(Box::new(p.clone()), Box::new(op_reverse(v)), p.into()),
        (p, Op::Dollar2(_)) => {
            let res = match p.clone() {
                Lang::FunctionApp(name, _, _, _) => *name.clone(),
                rest => rest.clone()
            };
            let func = Lang::FunctionApp(
                Box::new(Var::from_name("map").to_language()),
                vec![res.clone()], builder::empty_type(), res.into());
            Lang::Chain(Box::new(func), Box::new(op_reverse(v)), p.into())
        },
        (p, Op::Custom(s, h)) 
            => {
                let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::from(Var::from_name(&s)
                                              .set_help_data(h.clone().into())));
                Lang::FunctionApp(var, vec![res.clone(), pp], builder::empty_type(), res.into()) },
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
