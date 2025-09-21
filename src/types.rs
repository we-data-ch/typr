use nom::IResult;
use nom::character::complete::multispace0;
use nom::bytes::complete::tag;
use nom::combinator::opt;
use nom::character::complete::alpha1;
use crate::argument_type::ArgumentType;
use nom::character::complete::one_of;
use nom::character::complete::digit1;
use crate::tag::Tag;
use nom::character::complete::alphanumeric1;
use crate::elements::scope;
use crate::elements::function_symbol;
use crate::r#type::Type;
use std::collections::HashSet;
use crate::argument_kind::ArgumentKind;
use crate::operators::{Op, op};
use nom::sequence::terminated;
use nom::multi::many0;
use nom::branch::alt;
use nom::sequence::preceded;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::Parser;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;
use crate::elements::variable;
use crate::Lang;
use crate::tint::Tint;
use crate::tchar::Tchar;
use crate::elements::variable_exp;
use nom::combinator::recognize;
use crate::elements;
use crate::graph::TypeSystem;

type Span<'a> = LocatedSpan<&'a str, String>;

fn ltype_arg(s: Span) -> IResult<Span, Type> {
    let res = (ltype, terminated(opt(tag(",")), multispace0)).parse(s);
    match res {
        Ok((s, (t, _))) => Ok((s, t)),
        Err(r) => Err(r)
    }
}

fn function_type(s: Span) -> IResult<Span, Type> {
    let res = (
            terminated(tag("("), multispace0),
            many0(ltype_arg),
            terminated(tag(")"), multispace0),
            terminated(tag("->"), multispace0),
            terminated(alt((if_type, ltype)), multispace0)
          ).parse(s);
    match res {
        Ok((s, (start, v, _, _, t))) => {
            let kind_vec = v.iter()
                .flat_map(|typ| typ.extract_generics())
                .collect::<HashSet<_>>()
                .into_iter()
                .map(|typ| ArgumentKind::from((typ.clone(), typ.get_kind())))
                .collect::<Vec<_>>();
            Ok((s, Type::Function(kind_vec, v.clone(), Box::new(t), start.into())))
        },
        Err(r) => Err(r)
    }
}


fn upper_case_generic(s: Span) -> IResult<Span, String> {
    let res = terminated(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), multispace0).parse(s);
    match res {
        Ok((s, g)) => Ok((s, g.to_string())),
        Err(r) => Err(r)
    }
}

fn self_tag(s: Span) -> IResult<Span, String> {
        let res = terminated(tag("Self"), multispace0).parse(s);
        match res {
            Ok((s, st)) => Ok((s, st.to_string())),
            Err(r) => Err(r)
        }
}

fn generic(s: Span) -> IResult<Span, Type> {
    let res = alt((
            upper_case_generic,
            self_tag
                  )).parse(s);
    match res {
        Ok((s, g)) => Ok((s, Type::Generic(g, HelpData::default()))),
        Err(r) => Err(r)
    }
}

fn simple_index(s: Span) -> IResult<Span, Type> {
    let res = terminated(digit1, multispace0).parse(s);
    match res {
        Ok((s, fl)) 
            => Ok((s, Type::Integer(fl.parse::<i32>().unwrap().into(), fl.into()))),
        Err(r) => Err(r)
    }
}

fn index(s: Span) -> IResult<Span,Type> {
    alt((index_generic, simple_index)).parse(s)
}

fn array_type(s: Span) -> IResult<Span, Type> {
    let res = (
            terminated(tag("["), multispace0),
            index_algebra,
            terminated(tag(","), multispace0),
            ltype,
            terminated(tag("]"), multispace0),
                  ).parse(s);

    match res {
        Ok((s, (start, num, _, typ, _))) => Ok((s, Type::Array(Box::new(num), Box::new(typ), start.into()))),
        Err(r) => Err(r)
    }
}

fn embedded_ltype(s: Span) -> IResult<Span, Type> {
    let res = (tag("@"), ltype).parse(s);
    match res {
        Ok((s, (at, ty))) 
            => Ok((s, Type::Embedded(Box::new(ty), at.into()))),
        Err(r) => Err(r)
    }
}

fn simple_label(s: Span) -> IResult<Span, Type> {
    let res = variable(s);
    match res.clone() {
        Ok((s, Lang::Variable(name, _, _, _, _, h))) 
            => Ok((s, Type::Char(name.to_owned().into(), h.clone()))),
        Ok((_s, _)) 
            => panic!("Error: {:?} shouldn't be something different from a variable", res),
        Err(r) => Err(r)
    }
}

pub fn label(s: Span) -> IResult<Span, Type> {
    alt((label_generic, simple_label)).parse(s)
}

pub fn argument(s: Span) -> IResult<Span, ArgumentType> {
    let res = (
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        alt((ltype, embedded_ltype)),
        opt(terminated(tag(","), multispace0))
                ).parse(s);
    match res {
        Ok((s, (e1, _, Type::Embedded(ty, _), _))) 
            => Ok((s, ArgumentType(e1, *ty.clone(), true))),
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentType(e1, e2, false))),
        Err(r) => Err(r)
    }
}

pub fn argument2(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(alpha1, multispace0),
        terminated(tag(":"), multispace0),
        alt((ltype, embedded_ltype)),
        opt(terminated(tag(","), multispace0))
                ).parse(s);
    match res {
        Ok((s, (_, _, e, _))) => Ok((s, e)),
        Err(r) => Err(r)
    }
}

fn record_type(s: Span) -> IResult<Span, Type> {
    let res = (
            terminated(tag("{"), multispace0),
            many0(argument),
            terminated(tag("}"), multispace0)
                    ).parse(s);
    match res {
        Ok((s, (start, v, _))) 
            => Ok((s, Type::Record(v.iter().cloned().collect(), start.into()))),
        Err(r) => Err(r)
    }
}

fn number(s: Span) -> IResult<Span, Type> {
    let res = (tag("num"), multispace0).parse(s);
    match res {
        Ok((s, (numtype, _))) 
            => Ok((s, Type::Number(numtype.into()))),
        Err(r) => Err(r)
    }
}

fn boolean(s: Span) -> IResult<Span, Type> {
    let res = terminated(tag("bool"), multispace0).parse(s);
    match res {
        Ok((s, b)) => Ok((s, Type::Boolean(b.into()))),
        Err(r) => Err(r)
    }
}

fn ltype_parameter(s: Span) -> IResult<Span, Type> {
    alt((
    terminated(terminated(ltype, tag(",")), multispace0),
    ltype)).parse(s)
}

fn type_params(s: Span) -> IResult<Span, Vec<Type>> {
    let res = (
        terminated(tag("<"), multispace0),
        many0(ltype_parameter),
        terminated(tag(">"), multispace0)).parse(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, v.clone())),
        Err(r) => Err(r)
    }
}


pub fn pascal_case(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = terminated((one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alphanumeric1), multispace0).parse(s);
    match res {
        Ok((s, (t1, t2))) => Ok((s.clone(), (format!("{}{}", t1, t2.clone()), t2.into()))),
        Err(r) => Err(r)
    }
}

fn module_path(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = many1(terminated(pascal_case, tag("::"))).parse(s);
    match res {
        Ok((s, v)) => {
            let res =  v.iter()
                .map(|(string, _)| string.clone())
                .collect::<Vec<_>>()
                .join("/");
            Ok((s, (res, v[0].1.clone())))
        },
        Err(r) => Err(r)
    }
}


pub fn type_alias(s: Span) -> IResult<Span, Type> {
    let res = (
            opt(module_path),
            alt((pascal_case, variable_exp)),
            terminated(opt(type_params), multispace0)
          ).parse(s);
    match res {
        Ok((s, (Some(p), (name, h), Some(v)))) 
            => Ok((s, Type::Alias(name, v.clone(), p.0.into(), false, h))),
        Ok((s, (None, (name, h), Some(v)))) 
            => Ok((s, Type::Alias(name, v.clone(), "".into(), false, h))),
        Ok((s, (Some(p), (name, h), None))) 
            => Ok((s, Type::Alias(name, vec![], p.0.into(), false, h))),
        Ok((s, (None, (name, h), None))) 
            => Ok((s, Type::Alias(name, vec![], "".into(), false, h))),
        Err(r) => Err(r),
    }
}

fn parenthese_value(s: Span) -> IResult<Span, Type> {
    delimited(
            terminated(tag("("), multispace0),
            alt((embedded_ltype, ltype)),
            terminated(tag(")"), multispace0)
          ).parse(s)
}

fn tag_default_helper(s: Span) -> IResult<Span, Type> {
        let res = (tag("."),
            pascal_case,
            opt(parenthese_value)).parse(s);
    match res {
        Ok((s, (_, (n, h), Some(val)))) 
            => Ok((s, Type::Tag(n, Box::new(val), h))),
        Ok((s, (_, (n, h), None))) 
            => Ok((s, Type::Tag(n, Box::new(Type::Empty(HelpData::default())), h))),
        Err(r) => Err(r)
    }
}

fn tag_default(s: Span) -> IResult<Span, Type> {
    terminated(tag_default_helper, multispace0).parse(s)
}

fn get_primitive(ls: LocatedSpan<&str, String>) -> Type {
    match ls.clone().into_fragment() {
        "bool" 
            => Type::Tag("Bool".to_string(), Box::new(Type::Boolean(ls.clone().into())), ls.into()),
        "num" 
            => Type::Tag("Num".to_string(), Box::new(Type::Number(ls.clone().into())), ls.into()),
        "char" 
            => Type::Tag("Char".to_string(), Box::new(Type::Char(Tchar::Unknown, ls.clone().into())), ls.into()),
        _ => todo!()
    }
}

fn tag_from_primitive(s: Span) -> IResult<Span, Type> {
    let res = alt((
                tag("bool"),
                tag("num"),
                tag("char"),
                  )).parse(s);
    match res {
        Ok((s, ls)) => Ok((s, get_primitive(ls))),
        Err(r) => Err(r)
    }
}

fn tag_exp(s: Span) -> IResult<Span, Type> {
    alt((tag_default, tag_from_primitive)).parse(s)
}

fn tags(s: Span) -> IResult<Span, Type> {
    let res = (
        tag_exp,
        terminated(tag("||"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (t, _))) => Ok((s, t)),
        Err(r) => Err(r)
    }
}


fn strict_union(s: Span) -> IResult<Span, Type> {
    let res = (
           alt((tags, tag_exp)),
           many0(alt((tags, tag_exp)))).parse(s);
   match res {
       Ok((s, (t, v))) if v.len() == 0 => Ok((s, t.clone())),
       Ok((s, (t, v))) => {
           let res = [t].iter()
               .chain(v.iter()).cloned()
               .flat_map(Tag::from_type).collect();
           Ok((s.clone(), Type::StrictUnion(res, s.into())))
       },
       Err(r) => Err(r)
   }
}

fn union_helper(s: Span) -> IResult<Span, Type> {
    terminated(terminated(utype, opt(tag("|"))), multispace0).parse(s)
}

fn union(s: Span) -> IResult<Span, Type> {
    let res = (union_helper, many1(union_helper)).parse(s);

    match res {
        Ok((s, (first, v))) => Ok((s, Type::Union(v.iter().chain([first].iter())
                                                  .cloned().collect(),
                                            v[0].clone().into()))),
        Err(r) => Err(r)
    }
}



fn chars(s: Span) -> IResult<Span, Type> {
    let res = tag("char")(s);
    match res {
        Ok((s, st)) => Ok((s, Type::Char(Tchar::Unknown, st.into()))),
        Err(r) => Err(r)
    }
}

fn pseudo_function_signature(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(function_symbol, multispace0),
        terminated(tag("("), multispace0),
        many0(argument2),
        terminated(tag(")"), multispace0),
        terminated(tag(":"), multispace0),
        terminated(ltype, multispace0)
          ).parse(s);
    match res {
        Ok((s, (start, _, args, _, _, typ))) => 
            Ok((s, Type::Function(vec![], args, Box::new(typ), start.into()))),
        Err(r) => Err(r)
    }
}

fn interface_simple_function(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(function_symbol, multispace0),
        terminated(tag("("), multispace0),
        many0(argument2),
        terminated(tag(")"), multispace0),
        terminated(tag(":"), multispace0),
        terminated(ltype, multispace0),
        scope
          ).parse(s);
    match res {
        Ok((s, (start, _par1, vt, _par2, _dp, ty, _body))) 
            => Ok((s, Type::Function(vec![], vt, Box::new(ty), start.into()))),
        Err(r) => Err(r)
    }
}

fn interface_function(s: Span) -> IResult<Span, ArgumentType> {
    let res = (
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        alt((pseudo_function_signature, interface_simple_function)),
        opt(terminated(tag(","), multispace0))
                ).parse(s);
    match res {
        Ok((s, (e, _, f, _))) => 
            Ok((s, ArgumentType(e, f, false))),
        Err(r) => Err(r)
    }
}

fn interface(s: Span) -> IResult<Span, Type> {
    let res = (
            terminated(tag("interface"), multispace0),
            terminated(tag("{"), multispace0),
            terminated(many1(interface_function), multispace0),
            terminated(tag("}"), multispace0)
                    ).parse(s);
    match res {
        Ok((s, (i, _, v, _))) 
            => Ok((s, Type::Interface(v, i.into()))),
        Err(r) => Err(r)
    }
}

fn tuple_type(s: Span) -> IResult<Span, Type> {
    let res = (
                tag("{"),
                many0(ltype_parameter),
                tag("}")).parse(s);
    match res {
        Ok((s, (ope, v, _cl))) => {
            Ok((s, Type::Tuple(v, ope.into())))
        },
        Err(r) => Err(r)
    }
}

fn index_generic(s: Span) -> IResult<Span, Type> {
    let res = (
            tag("#"),
            generic).parse(s);
    match res {
        Ok((s, (tag, Type::Generic(gen, _)))) 
            => Ok((s, Type::IndexGen(gen, tag.into()))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn label_generic(s: Span) -> IResult<Span, Type> {
    let res = (
            tag("$"),
            generic).parse(s);
    match res {
        Ok((s, (tag, Type::Generic(gen, _)))) 
            => Ok((s, Type::LabelGen(gen, tag.into()))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn integer(s: Span) -> IResult<Span, Type> {
    let res = (tag("int"), multispace0).parse(s);
    match res {
        Ok((s, (inttype, _))) 
            => Ok((s.clone(), Type::Integer(Tint::Unknown, inttype.into()))),
        Err(r) => Err(r)
    }
}

fn any(s: Span) -> IResult<Span, Type> {
    match tag("Any")(s) {
        Ok((s, e)) => Ok((s, Type::Any(e.into()))),
        Err(r) => Err(r)
    }
}

fn empty(s: Span) -> IResult<Span, Type> {
    match tag("Empty")(s) {
        Ok((s, e)) => Ok((s, Type::Empty(e.into()))),
        Err(r) => Err(r)
    }
}

fn compute_operators(v: &mut Vec<(Type, Op)>) -> Type {
    // (params, op)
    let first = v.pop().unwrap();
    match first {
        (p, Op::Add(_)) => {
            let res = compute_operators(v); let pp = p;
            Type::Add(Box::new(res.clone()), Box::new(pp), res.into())
        },
        (p, Op::Minus(_)) => { 
            let res = compute_operators(v); let pp = p;
            Type::Minus(Box::new(res.clone()), Box::new(pp), res.into())
        },
        (p, Op::Mul(_)) => {
            let res = compute_operators(v); let pp = p;
            Type::Mul(Box::new(res.clone()), Box::new(pp), res.into())
        },
        (p, Op::Div(_)) => {
            let res = compute_operators(v); let pp = p;
            Type::Div(Box::new(res.clone()), Box::new(pp), res.into())
        },
        (p, Op::Empty(_)) => p,
        _ => panic!()
    }
}


fn index_operator(s: Span) -> IResult<Span, (Type, Op)> {
    let res = (
                opt(op),
                index
                ).parse(s);
    match res {
        Ok((s, (Some(ope), ele))) => Ok((s, (ele, ope))),
        Ok((s, (None, ele))) => Ok((s.clone(), (ele, Op::Empty(s.into())))),
        Err(r) => Err(r)
    }
}

fn index_chain(s: Span) -> IResult<Span, Type> {
    let res = many1(index_operator).parse(s);
    match res {
        Ok((s, v)) => Ok((s, compute_operators(&mut v.clone()))),
        Err(r) => Err(r)
    }
}

fn index_algebra(s: Span) -> IResult<Span, Type> {
    alt((index_chain, index)).parse(s)
}

fn propagate_multiplicity(t: &Type) -> Option<Type> {
    match t {
        Type::Record(body, h) => {
            if body.len() == 1 {
                let argt = body.iter().next().unwrap().clone();
                Some(Type::Record([ArgumentType(
                    Type::Multi(Box::new(argt.get_argument()), HelpData::default()),
                    Type::Multi(Box::new(argt.get_type()), HelpData::default()),
                    false)].iter().cloned().collect(), h.clone()))
            } else { panic!("The Record {} should have only one couple 'label: type'", t) }
        },
        _ => None 
    }
}

fn multitype(s: Span) -> IResult<Span, Type> {
    let res = preceded(tag("*"), ltype).parse(s);
    match res {
        Ok((s, t)) 
            => {
                if let Some(new_t) = propagate_multiplicity(&t) {
                    Ok((s, new_t))
                } else {
                    Ok((s, Type::Multi(Box::new(t.clone()), t.into())))
                }
            }
        Err(r) => Err(r)
    }
}

fn type_condition(s: Span) -> IResult<Span, Type> {
    let res = (ltype, op, ltype).parse(s);
    match res {
        Ok((s, (t1, ope, t2))) => {
            let new_ope = ope.to_type()
                .expect("This is not a valid type operator");
            Ok((s, Type::Condition(
                        Box::new(t1),
                        Box::new(new_ope.clone()),
                        Box::new(t2), 
                        new_ope.into())))
        },
        Err(r) => Err(r)
    }
}

pub fn if_type(s: Span) -> IResult<Span,Type> {
    // if conditions
    let res = (ltype, tag("if "), many1(type_condition)).parse(s);
    match res {
        Ok((s, (typ, _if, t_conds))) 
            => Ok((s, Type::If(Box::new(typ.clone()), t_conds, typ.into()))),
        Err(r) => Err(r)
    }
}

fn r_class(s: Span) -> IResult<Span, Type> {
    let res = (terminated(tag("Class("), multispace0),
    many1(terminated(terminated(recognize(elements::chars), opt(tag(","))), multispace0)),
    terminated(tag(")"), multispace0)).parse(s);
    match res {
        Ok((s, (class, elems, _close))) 
            => Ok((s, Type::RClass(
                        elems.iter().map(|x| x.to_string()).collect()
                        , class.into()))),
        Err(r) => Err(r)
    }
}

pub fn utype(s: Span) -> IResult<Span, Type> {
    terminated(alt((
            r_class,
            any,
            empty,
            interface,
            label_generic,
            index_algebra,
            number,
            integer,
            boolean,
            chars,
            type_alias,
            generic,
            array_type,
            function_type,
            tuple_type,
            record_type,
            )), multispace0).parse(s)
}


//ltype to not use the reserved symbol "type"
// main
pub fn ltype(s: Span) -> IResult<Span, Type> {
    terminated(alt((
            union,
            multitype,
            r_class,
            any,
            empty,
            interface,
            label_generic,
            index_algebra,
            number,
            integer,
            boolean,
            chars,
            strict_union,
            type_alias,
            generic,
            array_type,
            function_type,
            tuple_type,
            record_type,
            )), multispace0).parse(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder;
    use crate::Adt;
    use crate::Context;

    #[test]
    fn test_function_type() {
        let res = function_type("(): number".into()).unwrap().1;
        assert_eq!(res.to_string(), "tfn([], var(number))");
    }

    #[test]
    fn test_function_type2() {
        let res = function_type("(number) -> number".into()).unwrap().1;
        assert_eq!(res.to_string(), "tfn([var('number')], var('number'))");
    }

    #[test]
    fn test_function_type3() {
        let res = function_type("(number, number) -> number".into()).unwrap().1;
        assert_eq!(res.to_string(), "tfn([var('number'), var('number')], var('number'))");
    }

    #[test]
    fn test_generic() {
        let res = generic("A".into()).unwrap().1;
        assert_eq!(res.to_string(), "gen('a')");
    }

    #[test]
    fn test_ltype_gen() {
        let res = ltype("A".into()).unwrap().1;
        assert_eq!(res.to_string(), "gen('a')");
    }

    #[test]
    fn test_array_gen1() {
        let res = array_type("[3, int]".into()).unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), gen('t'))");
    }

    #[test]
    fn test_array_gen2() {
        let res = array_type("[#N, T]".into()).unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), gen('t'))");
    }

    #[test]
    fn test_array_gen3() {
        let res = array_type("[#N, int]".into()).unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), int)");
    }

    #[test]
    fn test_ltype_array_gen() {
        let res = ltype("[#N, T]".into()).unwrap().1;
        assert_eq!(res.to_string(), "tarray(gen('n'), gen('t'))");
    }

    #[test]
    fn test_ltype_array() {
        let res = ltype("[3, num]".into()).unwrap().1;
        assert_eq!(res.to_string(), "tarray(3, var('num'))");
    }

    #[test]
    fn test_type_alias1() {
        let res = type_alias("One".into()).unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('One'), [])");
    }

    #[test]
    fn test_type_alias2() {
        let res = type_alias("Val<num>".into()).unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Val'), [num])");
    }

    #[test]
    fn test_type_alias3() {
        let res = type_alias("Complex<num, num>".into()).unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Complex'), [num, num])");
    }

    #[test]
    fn test_type_alias4() {
        let res = ltype("Complex<num, num>".into()).unwrap().1;
        assert_eq!(res.to_string(), "talias([], var('Complex'), [num, num])");
    }

    #[test]
    fn test_type_alias5() {
        let res = ltype("Option<{char, char}>".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_union1() {
        let res = strict_union("Rouge | Vert | Orange".into()).unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty), ttag('Orange', empty)])");
    }

    #[test]
    fn test_union2() {
        let res = strict_union("Rouge | Vert".into()).unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty)])");
    }

    #[test]
    fn test_union3() {
        let res = ltype("Rouge | Vert".into()).unwrap().1;
        assert_eq!(res.to_string(), "union([ttag('Rouge', empty), ttag('Vert', empty)])");
    }

    #[test]
    fn test_union4() {
        let res = ltype("Rouge(num)".into()).unwrap().1;
        assert_eq!(res.to_string(), "ttag('Rouge', num)");
    }

    #[test]
    fn test_u0() {
        let res = union("int | bool".into()).unwrap().1;
        assert_eq!(res.to_string(), "int, bool");
    }

    #[test]
    fn test_embedded1() {
        let res = embedded_ltype("*num".into()).unwrap().1;
        assert_eq!(res.to_string(), "tembedded(num)");
    }

    #[test]
    fn test_record1() {
        let res = record_type("{x: *num, y: num}".into()).unwrap().1;
        assert_eq!(res.to_string(), "trecord([[var('x'),tembedded(num)], [var('y'),num]])");
    }

    #[test]
    fn test_interface1() {
        let res = interface("interface { hey: fn(a: num, b: num): num }".into()).unwrap().1;
        assert_eq!(res.to_string(), "interface([[var('hey'),tfn([], [num, num], num)]])");
    }

    #[test]
    fn test_tuple_type1() {
        let res = tuple_type("{num, num, num}".into()).unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_alias_type1() {
        let res = ltype("Option<T>".into()).unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_alias_type2() {
        let res = ltype("Option".into()).unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_alias_type3() {
        let res = ltype("tbl".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_index_generic() {
        let res = ltype("#N".into()).unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op1() {
        let res = ltype("3+4+5".into()).unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op2() {
        let res = index_algebra("3+4+5".into()).unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op3() {
        let res = ltype("#I + #J".into()).unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_type_op4() {
        let res = ltype("[#I + #I, int]".into()).unwrap().1;
        assert_eq!(res.to_string(), "");
    }

    #[test]
    fn test_multitype0() {
        let res = ltype("+bool".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_multitype1() {
        let res = ltype("+{age: int}".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_multitype2() {
        let res = ltype("+{@L: T}".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_if_type() {
        let res = if_type("bool if $B in $L".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_type_condition() {
        let res = type_condition("$B in $L".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_r_class0() {
        let res = r_class("Class('data.frame', 'tbl')".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_type_alias0() {
        let res = type_alias("data__frame".into()).unwrap().1;
        assert_eq!(res, builder::empty_type());
    }

    #[test]
    fn test_fabrice0(){
        let arr1 = ltype("[1, T]".into()).unwrap().1;
        let arr2 = ltype("[1, 1]".into()).unwrap().1;
        assert_eq!(
            arr2.is_subtype(&arr1),
            true);
    }

}
