use crate::components::r#type::argument_type::ArgumentType;
use crate::components::error_message::help_message::ErrorMsg;
use crate::processes::parsing::operation_priority::PriorityTokens;
use std::process::exit;
use crate::utils::builder;
use crate::processes::parsing::types::label;
use crate::components::language::operators::Op;
use crate::processes::parsing::base_parse;
use crate::components::language::operators::op;
use nom::IResult;
use crate::components::language::Lang;
use nom::character::complete::multispace0;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use crate::processes::parsing::vector_priority::VectorPriority;
use crate::processes::parsing::lang_token::LangToken;
use nom::character::complete::alphanumeric1;
use nom::combinator::opt;
use nom::character::complete::one_of;
use nom::character::complete::multispace1;
use nom::character::complete::digit1;
use nom::sequence::terminated;
use nom::branch::alt;
use nom::sequence::delimited;
use nom::multi::many0;
use nom::multi::many1;
use nom::sequence::preceded;
use nom::Parser;
use nom_locate::LocatedSpan;
use nom::combinator::recognize;
use nom::bytes::complete::take_while1;
use nom::character::complete::char;
use nom::bytes::complete::escaped;
use nom::bytes::complete::is_not;
use crate::components::language::var::Var;
use crate::components::error_message::help_data::HelpData;
use crate::processes::parsing::types::single_type;
use crate::components::r#type::Type;
use crate::components::error_message::help_message::SyntaxError;
use crate::processes::parsing::types::ltype;
use crate::processes::parsing::types::if_type;
use crate::components::language::argument_value::ArgumentValue;
use crate::processes::parsing::types::pascal_case_no_space;

type Span<'a> = LocatedSpan<&'a str, String>;

pub fn is_pascal_case(name: &str) -> bool {
    let res = recognize(pascal_case_no_space).parse(name.into());
    match res {
        Ok((_, _)) => true,
        Err(_) => false
    }
}

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
    let res = terminated((opt(tag("-")), digit1), multispace0).parse(s);
    match res {
        Ok((s, (minus, d))) => {
            let symbol = match minus {
                Some(_) => "-",
                None => ""
            }.to_string() + &d.to_string();
            Ok((s, Lang::Integer(symbol.parse::<i32>().unwrap(), d.into())))
        },
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
    terminated(alt((
                double_quotes,
                single_quotes
                  )), multispace0).parse(s)
}

pub fn double_quotes(input: Span) -> IResult<Span, Lang> {
    let res = delimited(
        char('"'),
        opt(escaped(
            is_not("\\\""),    
            '\\',              
            alt((char('"'), char('\''))),
        )),
        char('"'),
    ).parse(input);
    match res {
        Ok((s, st)) => {
            let content = st.clone().map(|span| span.to_string()).unwrap_or_default();
            let location = st.map(|span| span.into()).unwrap_or_else(|| s.clone().into());
            Ok((s, Lang::Char(content, location)))
        },
        Err(r) => Err(r)
    }
}

pub fn single_quotes(input: Span) -> IResult<Span, Lang> {
    let res = delimited(
        char('\''),
        opt(escaped(
            is_not("\\'"),    
            '\\',              
            alt((char('"'), char('\''))),
        )),
        char('\''),
    ).parse(input);
    match res {
        Ok((s, st)) => {
            let content = st.clone().map(|span| span.to_string()).unwrap_or_default();
            let location = st.map(|span| span.into()).unwrap_or_else(|| s.clone().into());
            Ok((s, Lang::Char(content, location)))
        },
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

pub enum Case {
    Maj,
    Min,
}

fn variable_exp_2(s: Span) -> IResult<Span, (String, Case, HelpData)> {
    let res = variable_exp.parse(s);
    match res {
        Ok((s, (name, h))) => Ok((s, (name, Case::Min, h))),
        Err(r) => Err(r)
    }
}

fn pascal_case_2(s: Span) -> IResult<Span, (String, Case, HelpData)> {
    let res = pascal_case.parse(s);
    match res {
        Ok((s, (name, h))) => Ok((s, (name, Case::Maj, h))),
        Err(r) => Err(r)
    }
}

fn quoted_variable(s: Span) -> IResult<Span, (String, Case, HelpData)> {
    let res = delimited(
        char('`'),
        is_not("`"),    
        char('`')
    ).parse(s);

    match res {
        Ok((s, st)) 
            => Ok((s, (format!("`{}`", st.clone()), Case::Min, st.into()))),
        Err(r) => Err(r)
    }
}

pub fn variable_recognizer(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = alt((quoted_variable, pascal_case_2, variable_exp_2)).parse(s);
    match res {
        Ok((s, (s1, _case, h))) => {
            Ok((s, (s1, h)))
        },
        Err(r) => Err(r)
    }
}

fn variable_helper(s: Span) -> IResult<Span, (Lang, Case)> {
    let res = (alt((quoted_variable, pascal_case_2, variable_exp_2)), opt(type_annotation)).parse(s);
    match res {
        Ok((s, ((v, case, h), typ))) => {
            let res = Var::from_name(&v)
                .set_type(typ.unwrap_or(builder::empty_type()))
                .set_help_data(h);
            Ok((s, (res.into(), case)))
        },
        Err(r) => Err(r)
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
            let args = args.iter().map(|(arg, _)| arg).cloned().collect::<Vec<_>>();
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
        //alt((scope, parse_elements))
        scope
          ).parse(s);
    match res {
        Ok((s, (_, _, args, _, Some(_), Some(typ), exp))) =>{
            Ok((s, Lang::Function(args, typ, Box::new(exp), HelpData::default())))
        },
        Ok((_s, (_, _, _args, _cp, None, None, _exp))) 
            => {
                panic!("You forgot to specify the function return type: 'fn(...): Type'");
            }, 
        Ok((_s, (_, _, _args, _, Some(tag), None, _exp))) 
            => {
                None::<bool>.expect(
                    &SyntaxError::FunctionWithoutReturnType(tag.into()).display()
                           );
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

pub fn variable2(s: Span) -> IResult<Span, Lang> {
    let res = variable.parse(s);
    match res {
        Ok((s, (lang, _))) => Ok((s, lang)),
        Err(r) => Err(r)
    }
}

fn array_indexing(s: Span) -> IResult<Span, Lang> {
    let res = (alt((scope, variable2)), array).parse(s);
    
    match res {
        Ok((s, (lang1, lang2))) 
            => Ok((s, Lang::ArrayIndexing(Box::new(lang1.clone()), Box::new(lang2), lang1.into()))),
        Err(r) => Err(r)
    }
}

fn function_application(s: Span) -> IResult<Span, Lang> {
    let res = (
            alt((scope, variable2)),
            terminated(tag("("), multispace0),
            values,
            terminated(tag(")"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (exp, _, v, _))) 
            => Ok((s, Lang::FunctionApp(Box::new(exp.clone()), v.clone(), builder::unknown_function(), exp.into()))),
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
        Ok((_s, (None, _ob, args, _))) => {
            if args.len() == 0 {
                panic!("Error: the scope shouldn't be empty")
            } else {
                println!("{}", _s);
                panic!("You forgot to put a record identifier before the bracket: ':{{...}}'");
            }
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
            alt((variable2, elements)),
            terminated(tag("as"), multispace0),
            variable2,
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
    alt((integer, variable2)).parse(s)
}

fn create_range(params: &[Lang]) -> Lang {
    if params.len() == 2 {
        Lang::FunctionApp(
           Box::new(Var::from_name("seq").to_language()),
           vec![params[0].clone(), params[1].clone(), Lang::Integer(1, HelpData::default())],
           builder::unknown_function(),
           params.to_vec().into())
    } else {
        Lang::FunctionApp(
           Box::new(Var::from_name("seq").to_language()),
           vec![params[0].clone(), params[1].clone(), params[2].clone()],
           builder::unknown_function(),
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
    let res = preceded(tag("."), variable2).parse(s);
    match res {
        Ok((s, Lang::Variable(n, a, b, c, d))) 
            => Ok((s, Lang::Variable(format!(".{}", n), a, b, c, d))),
        Ok((_s, _)) => todo!(),
        Err(r) => Err(r)
    }
}

fn element_operator2(s: Span) -> IResult<Span, (Lang, Op)> {
    let res = (opt(op),
                alt((function_application2, number, integer, chars, boolean, variable2, dot_variable))
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
    let res = preceded(tag("~"), elements).parse(s);
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
            variable2,
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
            variable2,
            scope,
            array
        )).parse(s)
}

pub fn scope(s: Span) -> IResult<Span, Lang> {
    let res = delimited(
        terminated(alt((tag("("), tag("{"))), multispace0),
        opt(base_parse),
        terminated(alt((tag(")"), tag("}"))), multispace0)).parse(s);
    match res {
        Ok((s, Some(v)))
            => {
                Ok((s, Lang::Scope(v.clone(), v.into())))
            },
        Ok((_s, None)) => panic!("Error: the scope shouldn't be empty"),
        Err(r) => Err(r),
    }
}


fn element_operator_token(s: Span) -> IResult<Span, LangToken> {
    match op.parse(s) {
        Ok((s, op)) => Ok((s, LangToken::Operator(op))),
        Err(r) => Err(r)
    }
}

fn single_element_token(s: Span) -> IResult<Span, LangToken> {
    match single_element.parse(s) {
        Ok((s, op)) => Ok((s, LangToken::Expression(op))),
        Err(r) => Err(r)
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
        },
        Err(r) => Err(r)
    }
}

// main
pub fn parse_elements(s: Span) -> IResult<Span, Lang> {
    alt((
        vectorial_bloc,
        elements,
        )).parse(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fluent_parser::FluentParser;

    #[test]
    #[should_panic]
    fn test_empty_scope(){
        let _ = "{  }".parse::<Lang>();
    }

    #[test]
    #[should_panic]
    fn test_function_with_empty_scope1() {
        let _ = "fn(): int {}".parse::<Lang>();
    }

    #[test]
    #[should_panic]
    fn test_function_with_empty_scope2() {
        let _ = simple_function("fn(): int {}".into());
    }

    #[test]
    fn test_function_with_empty_scope3() {
        let res = simple_function("fn(): int { 5 }".into()).unwrap().1;
        assert_eq!(res.simple_print(), "Function");
    }

    #[test]
    fn test_variable1() {
        let res = variable_exp("hello".into()).unwrap().1.0;
        assert_eq!(res, "hello", "Should return the variable name 'hello'");
    }

    #[test]
    fn test_simple_variable1() {
        let res = variable_exp("hello".into()).unwrap().1.0;
        assert_eq!(res, "hello", "Should return the variable name 'hello'");
    }
    
    #[test]
    fn test_simple_variable2() {
        let res = variable_exp("module::hello".into()).unwrap().1.0;
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
        assert_eq!(res.simple_print(), "Operator", "Should put multiplication first 1 + 2 * 3");
    }

    #[test]
    fn test_multiplication2() {
        let res = "1 * 2 + 3".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(res.simple_print(), "Operator", "Should put multiplication first 1 * 2 + 3");
    }

    #[test]
    fn test_multiplication3() {
        let res = "1 * 2 + 3 * 4".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(res.simple_print(), "Operator", "Should put multiplication first 1 * 2 + 3 * 4");
    }

    #[test]
    fn test_accessor1() {
        let res = "3 + personne$age ".parse::<Lang>().unwrap();
        dbg!(&res);
        assert_eq!(res.simple_print(), "Operator", "Should put multiplication first 1 * 2 + 3 * 4");
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
        let fp = FluentParser::new()
            .push("name[1, 2, 3]")
            .parse_next();
        println!("fp: {}", fp);
        assert!(true);
    }

    #[test]
    fn test_function_application1() {
        let res = elements("hey . add()".into()).unwrap().1;
        dbg!(&res);
        assert!(true);
    }

    #[test]
    fn test_quoted_variable() {
        let res = quoted_variable("`+`".into()).unwrap().1;
        assert_eq!(res.0, "`+`");
    }

    #[test]
    fn test_uniform_function_call() {
        let res = FluentParser::new()
            .push("true.not()")
            .parse_next();
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

}
