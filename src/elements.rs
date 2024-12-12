use std::process::exit;
use nom::IResult;
use nom::branch::alt;
use nom::sequence::terminated;
use nom::character::complete::multispace0;
use crate::language::Lang;
use nom::bytes::complete::tag;
use nom::sequence::tuple;
use crate::operators::{Op, op};
use nom::sequence::delimited;
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::combinator::opt;
use nom::multi::many0;
use nom::multi::many1;
use crate::language::ArgumentType;
use crate::language::ArgumentValue;
use crate::language::ArgumentKind;
use crate::types::ltype;
use nom::character::complete::one_of;
use nom::character::complete::none_of;
use crate::types::Type;
use nom::sequence::preceded;
use nom::character::complete::multispace1;
use crate::parser::parse_exp;
use crate::var::Permission;
use nom::character::complete::digit1;

pub fn number(s: &str) -> IResult<&str,Lang> {
    let res = terminated(tuple((digit1, tag("."), digit1)), multispace0)(s);
    match res {
        Ok((s, (d1, _dot, d2))) => {
            let n = format!("{}.{}", d1, d2).parse::<f32>().unwrap();
            Ok((s, Lang::Number(n)))
        },
        Err(r) => Err(r),
    }
}

fn integer(s: &str) -> IResult<&str, Lang> {
    let res = terminated(digit1, multispace0)(s);
    match res {
        Ok((s, d)) => Ok((s, Lang::Integer(d.parse::<i32>().unwrap()))),
        Err(r) => Err(r)
    }
}

fn boolean(s: &str) -> IResult<&str,Lang> {
    let res = alt((
                terminated(tag("true"), multispace0),
                terminated(tag("TRUE"), multispace0),
                terminated(tag("false"), multispace0),
                terminated(tag("FALSE"), multispace0),
                  ))(s);
    match res {
        Ok((s, "true")) => Ok((s, Lang::Bool(true))),
        Ok((s, "TRUE")) => Ok((s, Lang::Bool(true))),
        Ok((s, "false")) => Ok((s, Lang::Bool(false))),
        Ok((s, "FALSE")) => Ok((s, Lang::Bool(false))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn chars(s: &str) -> IResult<&str, Lang> {
    let res = alt((
            delimited(tag("\""), many0(none_of("\"")), tag("\"")),
            delimited(tag("'"), many0(none_of("'")), tag("'")),
                  ))(s);
    match res {
        Ok((s, st)) => Ok((s, Lang::Char(st.iter().collect()))),
        Err(r) => Err(r)
    }
}

fn starting_char(s: &str) -> IResult<&str, char> {
    one_of("abcdefghijklmnopqrstuvwxyz_")(s)
}

fn body_char(s: &str) -> IResult<&str, char> {
    one_of("abcdefghijklmnopqrstuvwxyz_0123456789")(s)
}

fn variable_exp(s: &str) -> IResult<&str, String> {
    let res = tuple((starting_char, many0(body_char)))(s);
    match res {
        Ok((s, (s1, v))) => Ok((s, format!("{}{}", s1, v.iter().collect::<String>()))),
        Err(r) => Err(r)
    }
}

fn type_annotation(s: &str) -> IResult<&str, Type> {
    delimited(tag("<"), ltype, tag(">"))(s)
}

fn module_path(s: &str) -> IResult<&str, String> {
    let res = many1(terminated(pascal_case, tag("::")))(s);
    match res {
        Ok((s, v)) => Ok((s, v.join("/"))),
        Err(r) => Err(r)
    }
}

pub fn variable(s: &str) -> IResult<&str, Lang> {
    let res = terminated(
        tuple((opt(module_path), variable_exp, opt(type_annotation))),
        multispace0)(s);
    match res {
        Ok((s, (Some(mp), v, Some(ty)))) 
            => Ok((s, Lang::Variable(
                        v.to_string(),
                        mp,
                        Permission::Private,
                        false,
                        ty))),
        Ok((s, (None, v, Some(ty)))) 
            => Ok((s, Lang::Variable(
                        v.to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        ty))),
        Ok((s, (Some(mp), v, None))) 
            => Ok((s, Lang::Variable(
                        v.to_string(),
                        mp,
                        Permission::Private,
                        false,
                        Type::Empty))),
        Ok((s, (None, v, None))) 
            => Ok((s, Lang::Variable(
                        v.to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty))),
        Err(r) => Err(r)
    }
}


pub fn argument(s: &str) -> IResult<&str, ArgumentType> {
    let res = tuple((
        terminated(alpha1, multispace0),
        terminated(tag(":"), multispace0),
        ltype,
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentType(e1.to_string(), e2, false))),
        Err(r) => Err(r)
    }
}

fn equality_params(s: &str) -> IResult<&str, &str> {
    terminated(alt((tag(":"), tag("="))), multispace0)(s)
}

fn argument_val(s: &str) -> IResult<&str, ArgumentValue> {
    let res = tuple((
        terminated(alphanumeric1, multispace0),
        equality_params,
        single_element,
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentValue(e1.to_string(), e2))),
        Err(r) => Err(r)
    }
}

pub fn argument_kind(s: &str) -> IResult<&str, ArgumentKind> {
    let res = tuple((
        terminated(alpha1, multispace0),
        terminated(tag(":"), multispace0),
        ltype,
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentKind(e1.to_string(), e2.to_string()))),
        Err(r) => Err(r)
    }
}

pub fn function_symbol(s: &str) -> IResult<&str, &str> {
    alt((tag("function"), tag("func"), tag("fn")))(s)
}

pub fn simple_function(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
        terminated(function_symbol, multispace0),
        terminated(tag("("), multispace0),
        many0(argument),
        terminated(tag(")"), multispace0),
        opt(terminated(tag(":"), multispace0)),
        opt(terminated(ltype, multispace0)),
        scope
          ))(s);
    match res {
        Ok((s, (_, _, args, _, Some(_), Some(typ), exp))) =>
            Ok((s, Lang::Function(vec![], args, typ.to_string(), Box::new(exp)))),
        Ok((_s, (_, _, _args, _, None, None, _exp))) 
            => {
                println!("You forgot to specify the function return type: 'fn(...): Type'");
                exit(1)
            }, 
        Ok((_s, (_, _, _args, _, Some(_), None, _exp))) 
            => {
            println!("You forgot to specify the function return type after the ':' : 'fn(...): Type'");
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

fn complex_function(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
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
          ))(s);
    match res {
        Ok((s, (_, _, arg_kinds, _, _, args, _, _, typ, exp))) => 
            Ok((s, Lang::Function(arg_kinds, args, typ.to_string(), Box::new(exp)))),
        Err(r) => Err(r)
    }
}

fn function(s: &str) -> IResult<&str, Lang> {
    alt((
        simple_function,
        complex_function
    ))(s)
}

fn values(s: &str) -> IResult<&str, Vec<Lang>> {
    many0(
        terminated(
            parse_elements,
            terminated(opt(tag(",")), multispace0)))(s)
}

fn array_indexing(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            alt((scope, variable)),
            terminated(tag("["), multispace0),
            number,
            terminated(tag("]"), multispace0)
          ))(s);
    match res {
        Ok((s, (exp, _, Lang::Number(n), _))) 
            => Ok((s, Lang::ArrayIndexing(Box::new(exp), n))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn function_application(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            alt((scope, variable)),
            terminated(tag("("), multispace0),
            values,
            terminated(tag(")"), multispace0)
          ))(s);
    match res {
        Ok((s, (exp, _, v, _))) => Ok((s, Lang::FunctionApp(Box::new(exp), v.clone()))),
        Err(r) => Err(r)
    }
}

fn array(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            terminated(tag("["), multispace0),
            values,
            terminated(tag("]"), multispace0)
          ))(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, Lang::Array(v.clone()))),
        Err(r) => Err(r)
    }
}

fn record_identifier(s: &str) -> IResult<&str, &str> {
    alt((tag("record"), tag("object"), tag("list"), tag(":")))(s)
}

fn opening_separator(s: &str) -> IResult<&str, &str> {
    terminated(alt((tag("{"), tag("("))), multispace0)(s)
}

fn closing_separator(s: &str) -> IResult<&str, &str> {
    terminated(alt((tag("}"), tag(")"))), multispace0)(s)
}

fn record(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
        opt(record_identifier),
        opening_separator,
        many0(argument_val),
        closing_separator))(s);
    match res {
        Ok((s, (Some(_), _, args, _))) => Ok((s, Lang::Record(args.clone()))),
        Ok((_s, (None, _, args, _))) => {
            println!("You forgot to put a record identifier before the bracket: 'record {{...}}'");
            println!("{}", args.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "));
            exit(1)
        } 
        Err(r) => Err(r)
    }

}

fn pascal_case(s: &str) -> IResult<&str, String> {
    let res = terminated(tuple((
            one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
            alpha1)), multispace0)(s);
    match res {
        Ok((s, (t1, t2))) => Ok((s, format!("{}{}", t1, t2))),
        Err(r) => Err(r)
    }
}

fn parenthese_value(s: &str) -> IResult<&str, Lang> {
    delimited(
            terminated(tag("("), multispace0),
            single_element,
            terminated(tag(")"), multispace0)
          )(s)
}

pub fn tag_exp(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            pascal_case,
            opt(parenthese_value)))(s);
    match res {
        Ok((s, (n, None))) => Ok((s, Lang::Tag(n, Box::new(Lang::Empty)))),
        Ok((s, (n, Some(val)))) => Ok((s, Lang::Tag(n, Box::new(val)))),
        Err(r) => Err(r)
    }
}


fn dotdotdot(s: &str) -> IResult<&str, Lang> {
    let res = terminated(tag("..."), multispace0)(s);
    match res {
        Ok((s, _)) => Ok((s, Lang::Empty)),
        Err(r) => Err(r)
    }
}

fn else_exp(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            terminated(tag("else"), multispace0),
            terminated(tag("{"), multispace0),
            parse_elements,
            terminated(tag("}"), multispace0),
                    ))(s);
    match res {
        Ok((s, (_else, _o, exp, _c))) 
            => Ok((s, exp)),
        Err(r) => Err(r)
    }
}

fn else_if_exp(s: &str) -> IResult<&str, Lang> {
    preceded(
            terminated(tag("else"), multispace1),
            if_exp
                    )(s)
}

fn if_exp(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            terminated(tag("if"), multispace0),
            terminated(tag("("), multispace0),
            parse_elements,
            terminated(tag(")"), multispace0),
            terminated(tag("{"), multispace0),
            parse_elements,
            terminated(tag("}"), multispace0),
            opt(alt((else_if_exp, else_exp)))
                    ))(s);
    match res {
        Ok((s, (_if, _op, cond, _cp, _o, exp, _c, els))) 
            => Ok((s, 
                   Lang::If(
                       Box::new(cond),
                       Box::new(exp),
                       Box::new(els.unwrap_or(Lang::Empty))))),
        Err(r) => Err(r)
    }
}

fn branch(s: &str) -> IResult<&str, (Box<Lang>, Box<Lang>)> {
    let res = tuple((
            tag_exp,
            terminated(tag("=>"), multispace0),
            single_element,
            opt(terminated(tag(","), multispace0)),
                    ))(s);
    match res {
        Ok((s, (part1, _arr, part2, _vir)))
            => Ok((s, (Box::new(part1), Box::new(part2)))),
        Err(r) => Err(r)
    }
}

fn match_exp(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            terminated(tag("match"), multispace0),
            variable,
            terminated(tag("{"), multispace0),
            many0(branch),
            terminated(tag("}"), multispace0),
                    ))(s);
    match res {
        Ok((s, (_m, val, _o, bs, _c))) 
            => Ok((s, Lang::Match(Box::new(val), bs))),
        Err(r) => Err(r)
    }
}

fn tuple_exp(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            terminated(tag("("), multispace0),
            values,
            terminated(tag(")"), multispace0),
                    ))(s);
    match res {
        Ok((s, (_o, vals, _c))) => 
            Ok((s, Lang::Tuple(vals))),
        Err(r) => Err(r)
    }
}

fn int_or_var(s: &str) -> IResult<&str, Lang> {
    alt((integer, variable))(s)
}

fn range(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
            int_or_var,
            tag(":"),
            opt(tuple((int_or_var, tag(":")))),
            int_or_var))(s);
    match res {
        Ok((s, (iv1, _sep, None, iv2))) 
            => Ok((s, 
                   Lang::FunctionApp(
                       Box::new(Lang::Variable("seq".to_string(), "".to_string(), Permission::Private, false, Type::Empty)),
                       vec![iv1, iv2, Lang::Integer(1)]))),
        Ok((s, (iv1, _sep, Some((iv0, _sep2)), iv2)))
            => Ok((s, 
                   Lang::FunctionApp(
                       Box::new(Lang::Variable("seq".to_string(), "".to_string(), Permission::Private, false, Type::Empty)),
                       vec![iv1, iv2, iv0]))),
        Err(r) => Err(r),
    }
}

// main
fn single_element(s: &str) -> IResult<&str,Lang> {
    alt((
            boolean,
            range,
            number,
            integer,
            chars,
            match_exp,
            if_exp,
            dotdotdot,
            function,
            record,
            function_application,
            array_indexing,
            variable,
            tag_exp,
            scope,
            tuple_exp,
            array,
        ))(s)
}

pub fn scope(s: &str) -> IResult<&str, Lang> {
    let res = delimited(
        terminated(alt((tag("("), tag("{"))), multispace0),
        alt((dotdotdot, parse_elements, parse_exp)),
        terminated(alt((tag(")"), tag("}"))), multispace0))(s);
    match res {
        Ok((s, Lang::Empty)) => Ok((s, Lang::Scope(vec![]))),
        Ok((s, Lang::Sequence(v))) => Ok((s, Lang::Scope(v.clone()))),
        Ok((s, rest)) => Ok((s, Lang::Scope(vec![rest]))),
        Err(r) => Err(r),
    }
}

fn op_reverse(v: &mut Vec<(Lang, Op)>) -> Lang {
    // (params, op)
    let first = v.pop().unwrap();
    match first {
        (p, Op::And) => Lang::And(Box::new(p), Box::new(op_reverse(v))),
        (p, Op::Or) => Lang::Or(Box::new(p), Box::new(op_reverse(v))),
        (p, Op::Union) => Lang::Union(Box::new(p), Box::new(op_reverse(v))),
        (p, Op::Eq) => Lang::Eq(Box::new(p), Box::new(op_reverse(v))),
        (Lang::FunctionApp(name, params), Op::Pipe) 
            => { // (UFC) add the "object" as te first parameter of the function call
                let res = [op_reverse(v)].iter().chain(params.iter()).cloned().collect::<Vec<_>>();
                Lang::FunctionApp(name, res)
            }
        (p, Op::Pipe) => Lang::Pipe(Box::new(p), Box::new(op_reverse(v))),
        (p, Op::Pipe2) => {
            let res = match p.clone() {
                Lang::FunctionApp(name, params) => *name.clone(),
                rest => rest.clone()
            };
            let func = Lang::FunctionApp(
                Box::new(Lang::Variable("map".to_string(), "".to_string(), Permission::Private, false, Type::Empty)),
                vec![res.clone()]);
            Lang::Pipe(Box::new(func), Box::new(op_reverse(v)))
        },
        (p, Op::Add) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "add".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::Add2) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "add2".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::Minus) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "minus".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::Minus2) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "minus2".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::Mul) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "mul".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::Mul2) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "mul2".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::Div) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "div".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::Div2) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "div2".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::At) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "at".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (p, Op::At2) 
            => {let res = op_reverse(v); let pp = p;
                let var = Box::new(Lang::Variable(
                        "at2".to_string(),
                        "".to_string(),
                        Permission::Private,
                        false,
                        Type::Empty));
                Lang::FunctionApp(var, vec![res, pp]) },
        (Lang::FunctionApp(name, params), Op::Dot) 
            => { // (UFC) add the "object" as te first parameter of the function call
                let res = [op_reverse(v)].iter().chain(params.iter()).cloned().collect::<Vec<_>>();
                Lang::FunctionApp(name, res)
            }
        //a..f() -> [a, (f, dot2)] -> dot(f, a)
        //dot(map(f), a)
        (p, Op::Dot) => Lang::Dot(Box::new(p), Box::new(op_reverse(v))),
        (p, Op::Dot2) => {
            let res = match p.clone() {
                Lang::FunctionApp(name, params) => *name.clone(),
                rest => rest.clone()
            };
            let func = Lang::FunctionApp(
                Box::new(Lang::Variable("map".to_string(), "".to_string(), Permission::Private, false, Type::Empty)),
                vec![res.clone()]);
            Lang::Dot(Box::new(func), Box::new(op_reverse(v)))
        },
        (p, Op::Empty) => p,
        (p, rest) => panic!("{} shouldn't be applied in index operations", rest)
    }
}

fn element_operator(s: &str) -> IResult<&str, (Lang, Op)> {
    let res = tuple((
                opt(op),
                single_element
                ))(s);
    match res {
        Ok((s, (Some(ope), ele))) => Ok((s, (ele, ope))),
        Ok((s, (None, ele))) => Ok((s, (ele, Op::Empty))),
        Err(r) => Err(r)
    }
}

pub fn bang_exp(s: &str) -> IResult<&str, Lang> {
    let res = tuple((
        many1(element_operator),
        terminated(tag("!;"), multispace0)
                    ))(s);
    match res {
        Ok((s, (v, _bang))) => {
            let base = v[0].0.clone();
            Ok((s, 
                Lang::Assign(
                    Box::new(base),
                    Box::new(op_reverse(&mut v.clone())))))
        },
        Err(r) => Err(r)
    }
}

fn element_chain(s: &str) -> IResult<&str, Lang> {
    let res = many1(element_operator)(s);
    match res {
        Ok((s, v)) => Ok((s, op_reverse(&mut v.clone()))),
        Err(r) => Err(r)
    }
}

// main
pub fn parse_elements(s: &str) -> IResult<&str, Lang> {
    alt((
        element_chain,
        single_element
        ))(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_element() {
        let res = single_element("4;").unwrap().1;
        assert_eq!(res.to_string(), "4");
    }

    #[test]
    fn test_sop0() {
        let res = op("+").unwrap().1;
        assert_eq!(res, Op::Add);
    }

    #[test]
    fn test_el_op0() {
        let res = element_operator("+ 2").unwrap().1;
        assert_eq!(res, (Lang::Empty, Op::And));
    }

    #[test]
    fn test_el_op1() {
        let res = element_operator("2").unwrap().1;
        assert_eq!(res, (Lang::Empty, Op::And));
    }

    #[test]
    fn test_chain0() {
        let res = element_chain("2").unwrap().1;
        assert_eq!(res.to_string(), "...");
    }

    #[test]
    fn test_chain1() {
        let res = element_chain("2 + 9").unwrap().1;
        assert_eq!(res.to_string(), "...");
    }

    #[test]
    fn test_chain2() {
        let res = element_chain("3 + 2 + 4 + 7 + 6").unwrap().1;
        assert_eq!(res.to_string(), "...");
    }

    #[test]
    fn test_chain3() {
        let res = element_chain("3 .add(2).add(4).add(7).add(6)").unwrap().1;
        assert_eq!(res.to_string(), "...");
    }

    #[test]
    fn test_chain4() {
        let res = element_chain("7 .combine(2, 3).okay(true)").unwrap().1;
        assert_eq!(res.to_string(), "...");
    }
    
    #[test]
    fn test_chain5() {
        let res = single_element("combine(3, 2, 3)").unwrap().1;
        assert_eq!(res.to_string(), "...");
    }

    #[test]
    fn test_funcion_no_parameters(){
        let res = function("fn () : Number { 7 }").unwrap().1;
        assert_eq!(res.to_string(), "fn([],Number,(7))");
    }

    #[test]
    fn test_function_with_parameters(){
        let res = function("fn (a: number, b: bool) : Number { 7 }").unwrap().1;
        assert_eq!(res.to_string(), "fn([[var(a),number], [var(b),bool]],Number,(7))");
    }

    #[test]
    fn test_parse_function1() {
        let res = single_element("fn (a: num, b: bool) : num {...}").unwrap().1;
        assert_eq!(res.to_string(), "fn([], [[var(a),number], [var(b),bool]],Number,(7))");
    }

    #[test]
    fn test_parse_elements_function() {
        let res = parse_elements("fn (a: number, b: bool) : Number { 7 }").unwrap().1;
        assert_eq!(res.to_string(), "fn([[var(a),number], [var(b),bool]],Number,(7))");
    }

    #[test]
    fn test_function_application1() {
        let res = function_application("incr()").unwrap().1;
        assert_eq!(res.to_string(), "fn_app(var(incr), values([]))");
    }

    #[test]
    fn test_function_application2() {
        let res = single_element("incr()").unwrap().1;
        assert_eq!(res.to_string(), "fn_app(var(incr), values([]))");
    }

    #[test]
    fn test_function_application3() {
        let res = parse_elements("incr()").unwrap().1;
        assert_eq!(res.to_string(), "fn_app(var(incr), values([]))");
    }

    #[test]
    fn test_function_application4() {
        let res = parse_elements("{7}()").unwrap().1;
        assert_eq!(res.to_string(), "fn_app((7), values([]))");
    }

    #[test]
    fn test_function_application5() {
        let res = parse_elements("{7}(4)").unwrap().1;
        assert_eq!(res.to_string(), "fn_app((7), values([4]))");
    }

    #[test]
    fn test_function_application6() {
        let res = function_application("{7}(4)").unwrap().1;
        assert_eq!(res.to_string(), "fn_app((7), values([4]))");
    }

    #[test]
    fn test_scope1() {
        let res = scope("{7;}").unwrap().1;
        assert_eq!(res.to_string(), "(7)");
    }

    #[test]
    fn test_scope2() {
        let res = scope("{let a = 7;}").unwrap().1;
        assert_eq!(res.to_string(), "(7)");
    }

    #[test]
    fn test_scope3() {
        let res = scope("{let a = 7; a;}").unwrap().1;
        assert_eq!(res.to_string(), "(7)");
    }

    #[test]
    fn test_array() {
        let res = array("[1, 2, 3]").unwrap().1;
        assert_eq!(res.to_string(), "array([1, 2, 3])");
    }

    #[test]
    fn test_char1() {
        let res = chars("\"Hello world\"").unwrap().1;
        assert_eq!(res.to_string(), "char");
    }

    #[test]
    fn test_char2() {
        let res = single_element("'Hello world'").unwrap().1;
        assert_eq!(res.to_string(), "'Hello world'");
    }

    #[test]
    fn test_if1() {
        let res = if_exp("if (true) { 7 }").unwrap().1;
        assert_eq!(res.to_string(), "if(true, 7, [], empty)");
    }

    #[test]
    fn test_if2() {
        let res = parse_elements("if (true) { 7 }").unwrap().1;
        assert_eq!(res.to_string(), "if(true, 7, [], empty)");
    }

    #[test]
    fn test_branch1() {
        let res = branch("True => 3").unwrap().1;
        assert_eq!(res, (Box::new(Lang::Empty), Box::new(Lang::Empty)));
    }

    #[test]
    fn test_branch2() {
        let res = branch("Int(i) => 3").unwrap().1;
        assert_eq!(res, (Box::new(Lang::Empty), Box::new(Lang::Empty)));
    }

    #[test]
    fn test_match1() {
        let res = match_exp("match a { True => 3, False => 4, }").unwrap().1;
        assert_eq!(res.to_string(), "match(7, [3, 3],[var('n'), var('n')])");
    }

    #[test]
    fn test_variable1() {
        let res = variable("hey").unwrap().1;
        assert_eq!(res.to_string(), "var()");
    }

    #[test]
    fn test_variable2() {
        let res = variable("Person::hey").unwrap().1;
        assert_eq!(res.to_string(), "var()");
    }

    #[test]
    fn test_variable3() {
        let res = variable("Person::Course::hey").unwrap().1;
        assert_eq!(res.to_string(), "var()");
    }

    #[test]
    fn test_var_tag1() {
        let res = single_element("Mod::a").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_var_tag2() {
        let res = parse_elements("Mod::a").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_func_appli1() {
        let res = parse_elements("Mod::new(7)").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_func_appli2() {
        let res = parse_elements("Count::new(7)").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_index_function() {
        let res = single_element("fn(a: #N): [#N, int] { ... }").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_range1() {
        let res = range("1:3").unwrap().1;
        assert_eq!(res.to_string(), "");
    }
    
    #[test]
    fn test_range2() {
        let res = single_element("1:3").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_range3() {
        let res = parse_elements("1:3").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_range4() {
        let res = parse_elements("1:a").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_shape0() {
        let res = single_element("[[1, 2], [3, 4]]").unwrap().1;
        assert_eq!(res.shape(), vec![0 as usize]);
    }

}
