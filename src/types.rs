use nom::IResult;
use nom::character::complete::multispace0;
use nom::sequence::tuple;
use nom::sequence::terminated;
use nom::bytes::complete::tag;
use nom::multi::many0;
use nom::branch::alt;
use nom::combinator::opt;
use nom::character::complete::alpha1;
use crate::argument_type::ArgumentType;
use nom::character::complete::one_of;
use nom::sequence::delimited;
use nom::multi::many1;
use nom::sequence::preceded;
use nom::character::complete::digit1;
use crate::tag::Tag;

use crate::elements::scope;
use crate::elements::function_symbol;
use crate::r#type::Type;
use std::collections::HashSet;
use crate::argument_kind::ArgumentKind;

fn ltype_arg(s: &str) -> IResult<&str, Type> {
    let res = tuple((ltype, terminated(opt(tag(",")), multispace0)))(s);
    match res {
        Ok((s, (t, _))) => Ok((s, t)),
        Err(r) => Err(r)
    }
}

fn function_type(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            terminated(tag("("), multispace0),
            many0(ltype_arg),
            terminated(tag(")"), multispace0),
            terminated(tag("->"), multispace0),
            terminated(ltype, multispace0)
          ))(s);
    match res {
        Ok((s, (_, v, _, _, t))) => {
            let kind_vec = v.iter()
                .flat_map(|typ| typ.extract_generics())
                .collect::<HashSet<_>>()
                .into_iter()
                .map(|typ| ArgumentKind::from((typ.clone(), typ.get_kind())))
                .collect::<Vec<_>>();
            Ok((s, Type::Function(kind_vec, v.clone(), Box::new(t))))
        },
        Err(r) => Err(r)
    }
}


fn upper_case_generic(s: &str) -> IResult<&str, String> {
    let res = terminated(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), multispace0)(s);
    match res {
        Ok((s, g)) => Ok((s, g.to_string())),
        Err(r) => Err(r)
    }
}

fn self_tag(s: &str) -> IResult<&str, String> {
        let res = terminated(tag("Self"), multispace0)(s);
        match res {
            Ok((s, st)) => Ok((s, st.to_string())),
            Err(r) => Err(r)
        }
}

fn generic(s: &str) -> IResult<&str, Type> {
    let res = alt((
            upper_case_generic,
            self_tag
                  ))(s);
    match res {
        Ok((s, g)) => Ok((s, Type::Generic(g))),
        Err(r) => Err(r)
    }
}

fn simple_index(s: &str) -> IResult<&str, Type> {
    let res = terminated(digit1, multispace0)(s);
    match res {
        Ok((s, fl)) => Ok((s, Type::Index(fl.parse::<u32>().unwrap()))),
        Err(r) => Err(r)
    }
}

fn index(s: &str) -> IResult<&str,Type> {
    alt((index_generic, simple_index))(s)
}

fn array_type(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            terminated(tag("["), multispace0),
            index,
            terminated(tag(","), multispace0),
            ltype,
            terminated(tag("]"), multispace0),
                  ))(s);

    match res {
        Ok((s, (_, num, _, typ, _))) => Ok((s, Type::Array(Box::new(num), Box::new(typ)))),
        Err(r) => Err(r)
    }
}

fn embedded_ltype(s: &str) -> IResult<&str, Type> {
    let res = preceded(tag("*"), ltype)(s);
    match res {
        Ok((s, ty)) => Ok((s, Type::Embedded(Box::new(ty)))),
        Err(r) => Err(r)
    }
}

fn simple_label(s: &str) -> IResult<&str, Type> {
    let res = alpha1(s);
    match res {
        Ok((s, lab)) => Ok((s, Type::Label(lab.to_string()))),
        Err(r) => Err(r)
    }
}

pub fn label(s: &str) -> IResult<&str, Type> {
    alt((label_generic, simple_label))(s)
}

pub fn argument(s: &str) -> IResult<&str, ArgumentType> {
    let res = tuple((
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        alt((ltype, embedded_ltype)),
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (e1, _, Type::Embedded(ty), _))) => Ok((s, ArgumentType(e1, *ty.clone(), true))),
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentType(e1, e2, false))),
        Err(r) => Err(r)
    }
}

pub fn argument2(s: &str) -> IResult<&str, Type> {
    let res = tuple((
        terminated(alpha1, multispace0),
        terminated(tag(":"), multispace0),
        alt((ltype, embedded_ltype)),
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (_, _, e, _))) => Ok((s, e)),
        Err(r) => Err(r)
    }
}

fn record_type(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            terminated(tag("{"), multispace0),
            many0(argument),
            terminated(tag("}"), multispace0)
                    ))(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, Type::Record(v.clone()))),
        Err(r) => Err(r)
    }
}

fn number(s: &str) -> IResult<&str, Type> {
    let res = terminated(tag("num"), multispace0)(s);
    match res {
        Ok((s, _)) => Ok((s, Type::Number)),
        Err(r) => Err(r)
    }
}

fn boolean(s: &str) -> IResult<&str, Type> {
    let res = terminated(tag("bool"), multispace0)(s);
    match res {
        Ok((s, _)) => Ok((s, Type::Boolean)),
        Err(r) => Err(r)
    }
}

fn ltype_parameter(s: &str) -> IResult<&str, Type> {
    alt((
    terminated(terminated(ltype, tag(",")), multispace0),
    ltype
        ))(s)
}

fn type_params(s: &str) -> IResult<&str, Vec<Type>> {
    let res = tuple((
        terminated(tag("<"), multispace0),
        many0(ltype_parameter),
        terminated(tag(">"), multispace0)
                    ))(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, v.clone())),
        Err(r) => Err(r)
    }
}

pub fn pascal_case(s: &str) -> IResult<&str, String> {
    let res = terminated(tuple((
            one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
            alpha1)), multispace0)(s);
    match res {
        Ok((s, (t1, t2))) => Ok((s, format!("{}{}", t1, t2))),
        Err(r) => Err(r)
    }
}

fn module_path(s: &str) -> IResult<&str, String> {
    let res = many1(terminated(pascal_case, tag("::")))(s);
    match res {
        Ok((s, v)) => Ok((s, v.join("/"))),
        Err(r) => Err(r)
    }
}

pub fn type_alias(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            opt(module_path),
            pascal_case,
            opt(type_params)
          ))(s);
    match res {
        Ok((s, (Some(p), name, Some(v)))) => Ok((s, Type::Alias(name, v.clone(), p))),
        Ok((s, (None, name, Some(v)))) => Ok((s, Type::Alias(name, v.clone(), "".to_string()))),
        Ok((s, (Some(p), name, None))) => Ok((s, Type::Alias(name, vec![], p))),
        Ok((s, (None, name, None))) => Ok((s, Type::Alias(name, vec![], "".to_string()))),
        Err(r) => Err(r),
    }
}

fn parenthese_value(s: &str) -> IResult<&str, Type> {
    delimited(
            terminated(tag("("), multispace0),
            alt((embedded_ltype, ltype)),
            terminated(tag(")"), multispace0)
          )(s)
}

fn tag_default(s: &str) -> IResult<&str, Type> {
    let res = terminated(tuple((
            tag("."),
            pascal_case,
            opt(parenthese_value))), multispace0)(s);
    match res {
        Ok((s, (_, n, Some(val)))) => Ok((s, Type::Tag(n, Box::new(val)))),
        Ok((s, (_, n, None))) => Ok((s, Type::Tag(n, Box::new(Type::Empty)))),
        Err(r) => Err(r)
    }
}

fn tag_from_primitive(s: &str) -> IResult<&str, Type> {
    let res = alt((
                tag("bool"),
                tag("num"),
                tag("char"),
                  ))(s);
    match res {
        Ok((s, "bool")) => Ok((s, Type::Tag("Bool".to_string(), Box::new(Type::Boolean)))),
        Ok((s, "num")) => Ok((s, Type::Tag("Num".to_string(), Box::new(Type::Number)))),
        Ok((s, "char")) => Ok((s, Type::Tag("Char".to_string(), Box::new(Type::Char)))),
        Ok((_, _)) => todo!(),
        Err(r) => Err(r)
    }
}

fn tag_exp(s: &str) -> IResult<&str, Type> {
    alt((tag_default, tag_from_primitive))(s)
}

fn tags(s: &str) -> IResult<&str, Type> {
    let res = tuple((
        tag_exp,
        terminated(tag("|"), multispace0)
          ))(s);
    match res {
        Ok((s, (t, _))) => Ok((s, t)),
        Err(r) => Err(r)
    }
}


fn union(s: &str) -> IResult<&str, Type> {
    let res = tuple((
           alt((tags, tag_exp)),
           many0(alt((tags, tag_exp)))))(s);
   match res {
       Ok((s, (t, v))) if v.len() == 0 => Ok((s, t.clone())),
       Ok((s, (t, v))) => {
           let res = [t].iter()
               .chain(v.iter()).cloned()
               .flat_map(Tag::from_type).collect();
           Ok((s, Type::Union(res)))
       },
       Err(r) => Err(r)
   }
}

fn chars(s: &str) -> IResult<&str, Type> {
    let res = tag("char")(s);
    match res {
        Ok((s, _st)) => Ok((s, Type::Char)),
        Err(r) => Err(r)
    }
}

fn pseudo_function_signature(s: &str) -> IResult<&str, Type> {
    let res = tuple((
        terminated(function_symbol, multispace0),
        terminated(tag("("), multispace0),
        many0(argument2),
        terminated(tag(")"), multispace0),
        terminated(tag(":"), multispace0),
        terminated(ltype, multispace0)
          ))(s);
    match res {
        Ok((s, (_, _, args, _, _, typ))) => 
            Ok((s, Type::Function(vec![], args, Box::new(typ)))),
        Err(r) => Err(r)
    }
}

fn interface_simple_function(s: &str) -> IResult<&str, Type> {
    let res = tuple((
        terminated(function_symbol, multispace0),
        terminated(tag("("), multispace0),
        many0(argument2),
        terminated(tag(")"), multispace0),
        terminated(tag(":"), multispace0),
        terminated(ltype, multispace0),
        scope
          ))(s);
    match res {
        Ok((s, (_fn, _par1, vt, _par2, _dp, ty, _body))) 
            => Ok((s, Type::Function(vec![], vt, Box::new(ty)))),
        Err(r) => Err(r)
    }
}

fn interface_function(s: &str) -> IResult<&str, ArgumentType> {
    let res = tuple((
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        alt((pseudo_function_signature, interface_simple_function)),
        opt(terminated(tag(","), multispace0))
                ))(s);
    match res {
        Ok((s, (e, _, f, _))) => 
            Ok((s, ArgumentType(e, f, false))),
        Err(r) => Err(r)
    }
}

fn interface(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            terminated(tag("interface"), multispace0),
            terminated(tag("{"), multispace0),
            terminated(many1(interface_function), multispace0),
            terminated(tag("}"), multispace0)
                    ))(s);
    match res {
        Ok((s, (_, _, v, _))) => Ok((s, Type::Interface(v))),
        Err(r) => Err(r)
    }
}

fn tuple_type(s: &str) -> IResult<&str, Type> {
    let res = tuple((
                tag("("),
                many0(ltype_parameter),
                tag(")")))(s);
    match res {
        Ok((s, (_op, v, _cl))) => {
            let v_final = v.iter().enumerate()
                    .map(|(id, x)| ArgumentType::new(&id.to_string(), &x))
                    .collect::<Vec<_>>();
            Ok((s, Type::Record(v_final)))
        },
        Err(r) => Err(r)
    }
}

fn index_generic(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            tag("#"),
            generic))(s);
    match res {
        Ok((s, (_tag, Type::Generic(gen)))) 
            => Ok((s, Type::IndexGen(gen))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn label_generic(s: &str) -> IResult<&str, Type> {
    let res = tuple((
            tag("%"),
            generic))(s);
    match res {
        Ok((s, (_tag, Type::Generic(gen)))) 
            => Ok((s, Type::LabelGen(gen))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn integer(s: &str) -> IResult<&str, Type> {
    let res = terminated(tag("int"), multispace0)(s);
    match res {
        Ok((s, _)) => Ok((s, Type::Integer)),
        Err(r) => Err(r)
    }
}

fn empty(s: &str) -> IResult<&str, Type> {
    match tag("Empty")(s) {
        Ok((s, _)) => Ok((s, Type::Empty)),
        Err(r) => Err(r)
    }
}


//ltype to not use the reserved symbol "type"
// main
pub fn ltype(s: &str) -> IResult<&str, Type> {
    terminated(alt((
            empty,
            interface,
            index_generic,
            label_generic,
            simple_index,
            number,
            integer,
            boolean,
            chars,
            union,
            type_alias,
            generic,
            array_type,
            function_type,
            tuple_type,
            record_type,
            )), multispace0)(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_type() {
        let res = function_type("(): number").unwrap().1;
        assert_eq!(res.to_string(), "tfn([], var(number))");
    }

    #[test]
    fn test_function_type2() {
        let res = function_type("(number) -> number").unwrap().1;
        assert_eq!(res.to_string(), "tfn([var('number')], var('number'))");
    }

    #[test]
    fn test_function_type3() {
        let res = function_type("(number, number) -> number").unwrap().1;
        assert_eq!(res.to_string(), "tfn([var('number'), var('number')], var('number'))");
    }

    #[test]
    fn test_generic() {
        let res = generic("A").unwrap().1;
        assert_eq!(res.to_string(), "gen('a')");
    }

    #[test]
    fn test_ltype_gen() {
        let res = ltype("A").unwrap().1;
        assert_eq!(res.to_string(), "gen('a')");
    }

    #[test]
    fn test_array_gen1() {
        let res = array_type("[3, int]").unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), gen('t'))");
    }

    #[test]
    fn test_array_gen2() {
        let res = array_type("[#N, T]").unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), gen('t'))");
    }

    #[test]
    fn test_array_gen3() {
        let res = array_type("[#N, int]").unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), int)");
    }

    #[test]
    fn test_ltype_array_gen() {
        let res = ltype("[#N, T]").unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), gen('t'))");
    }

    #[test]
    fn test_ltype_array() {
        let res = ltype("[3, num]").unwrap().1;
        assert_eq!(res.to_string(), "tarray(3, var('num'))");
    }

    #[test]
    fn test_type_alias1() {
        let res = type_alias("One").unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('One'), [])");
    }

    #[test]
    fn test_type_alias2() {
        let res = type_alias("Val<num>").unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Val'), [num])");
    }

    #[test]
    fn test_type_alias3() {
        let res = type_alias("Complex<num, num>").unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Complex'), [num, num])");
    }

    #[test]
    fn test_type_alias4() {
        let res = ltype("Complex<num, num>").unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Complex'), [num, num])");
    }

    #[test]
    fn test_union1() {
        let res = union("Rouge | Vert | Orange").unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty), ttag('Orange', empty)])");
    }

    #[test]
    fn test_union2() {
        let res = union("Rouge | Vert").unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty)])");
    }

    #[test]
    fn test_union3() {
        let res = ltype("Rouge | Vert").unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty)])");
    }

    #[test]
    fn test_union4() {
        let res = ltype("Rouge(num)").unwrap().1;
        assert_eq!(res.to_string(), "ttag('Rouge', num)");
    }

    #[test]
    fn test_embedded1() {
        let res = embedded_ltype("*num").unwrap().1;
        assert_eq!(res.to_string(), "tembedded(num)");
    }

    #[test]
    fn test_record1() {
        let res = record_type("{x: *num, y: num}").unwrap().1;
        assert_eq!(res.to_string(), "trecord([[var('x'),tembedded(num)], [var('y'),num]])");
    }

    #[test]
    fn test_interface1() {
        let res = interface("interface { hey: fn(a: num, b: num): num }").unwrap().1;
        assert_eq!(res.to_string(), "interface([[var('hey'),tfn([], [num, num], num)]])");
    }

    #[test]
    fn test_tuple_type1() {
        let res = tuple_type("(num, num, num)").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_alias_type1() {
        let res = ltype("Option<T>").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_alias_type2() {
        let res = ltype("Option").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_index_generic() {
        let res = ltype("#N").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op1() {
        let res = ltype("3+4+5").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op2() {
        let res = reverse_type_operators(&mut vec![
                                         (None, Type::Index(93)),
                                         (Some(Op::Add), Type::Index(8)),
                                         (Some(Op::Div), Type::Index(100))
        ]);
        assert_eq!(res, Type::Empty);
    }

    #[test]
    fn test_type_op3() {
        let res = ltype("#I + #J").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op4() {
        let res = ltype("[#I + #I, int]").unwrap().1;
        assert_eq!(res.to_string(), "");
    }

}
