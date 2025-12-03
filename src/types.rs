use nom::IResult;
use nom::character::complete::multispace0;
use nom::bytes::complete::tag;
use nom::combinator::opt;
use crate::argument_type::ArgumentType;
use nom::character::complete::one_of;
use nom::character::complete::digit1;
use crate::tag::Tag;
use nom::character::complete::alphanumeric1;
use crate::r#type::Type;
use std::collections::HashSet;
use crate::operators::{Op, op};
use nom::sequence::terminated;
use nom::multi::many0;
use nom::branch::alt;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::Parser;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;
use crate::Lang;
use crate::tint::Tint;
use crate::tchar::Tchar;
use crate::elements::variable_exp;
use nom::combinator::recognize;
use crate::elements;
use nom::character::complete::none_of;
use crate::builder;
use crate::elements::variable2;
use crate::type_operator::TypeOperator;
use crate::type_token::TypeToken;
use crate::vector_priority::VectorPriority;
use crate::operation_priority::PriorityTokens;

type Span<'a> = LocatedSpan<&'a str, String>;

fn ltype_arg(s: Span) -> IResult<Span, Type> {
    let res = (ltype, terminated(opt(terminated(tag(","), multispace0)), multispace0)).parse(s);
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
            Ok((s, Type::Function(v.clone(), Box::new(t), start.into())))
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

fn vector_type(s: Span) -> IResult<Span, Type> {
    let res = (
            terminated(tag("Vec["), multispace0),
            index_algebra,
            terminated(tag(","), multispace0),
            ltype,
            terminated(tag("]"), multispace0),
                  ).parse(s);

    match res {
        Ok((s, (start, num, _, typ, _))) => Ok((s, Type::Vector(Box::new(num), Box::new(typ), start.into()))),
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
    let res = variable2(s);
    match res.clone() {
        Ok((s, Lang::Variable(name, _, _, _, h))) 
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

pub fn pascal_case_no_space(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = (one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alphanumeric1).parse(s);
    match res {
        Ok((s, (t1, t2))) => Ok((s.clone(), (format!("{}{}", t1, t2.clone()), t2.into()))),
        Err(r) => Err(r)
    }
}

pub fn type_alias(s: Span) -> IResult<Span, Type> {
    let res = (
            pascal_case_no_space,
            terminated(opt(type_params), multispace0)
          ).parse(s);
    match res {
        Ok((s, ((name, h), Some(v)))) 
            => Ok((s, Type::Alias(name, v.clone(), false, h))),
        Ok((s, ((name, h), None))) 
            => Ok((s, Type::Alias(name, vec![], false, h))),
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


fn chars(s: Span) -> IResult<Span, Type> {
    let res = tag("char")(s);
    match res {
        Ok((s, st)) => Ok((s, Type::Char(Tchar::Unknown, st.into()))),
        Err(r) => Err(r)
    }
}

fn interface_function(s: Span) -> IResult<Span, ArgumentType> {
    let res = (
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        function_type,
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
        Ok((s, (i, _, v, _))) => {
            let set = v.iter().cloned().collect::<HashSet<_>>();
            Ok((s, Type::Interface(set, i.into())))
        },
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
        Ok((s, (tag, Type::Generic(r#gen, _)))) 
            => Ok((s, Type::IndexGen(r#gen, tag.into()))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn label_generic(s: Span) -> IResult<Span, Type> {
    let res = (
            tag("$"),
            generic).parse(s);
    match res {
        Ok((s, (tag, Type::Generic(r#gen, _)))) 
            => Ok((s, Type::LabelGen(r#gen, tag.into()))),
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

fn self_type(s: Span) -> IResult<Span, Type> {
    match tag("Self")(s) {
        Ok((s, e)) 
            => Ok((s, builder::self_generic_type().set_help_data(e.into()))),
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
        Ok((s, v)) => {
            Ok((s, compute_operators(&mut v.clone())))
        },
        Err(r) => Err(r)
    }
}

fn index_algebra(s: Span) -> IResult<Span, Type> {
    alt((index_chain, index)).parse(s)
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

fn sequence_type(s: Span) -> IResult<Span, Type> {
    let res = (
            terminated(tag("Seq["), multispace0),
            index_algebra,
            terminated(tag(","), multispace0),
            ltype,
            terminated(tag("]"), multispace0),
                  ).parse(s);

    match res {
        Ok((s, (start, num, _, typ, _))) 
            => Ok((s, Type::Sequence(Box::new(num), Box::new(typ), start.into()))),
        Err(r) => Err(r)
    }
}

pub fn primitive_types(s: Span) -> IResult<Span, Type> {
    alt((
            number,
            integer,
            boolean,
            chars)).parse(s)
}

fn type_operator(s: Span) -> IResult<Span, TypeToken> {
    let res = terminated(alt((
            tag("|"),
            tag("&"),
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("$")
        )), multispace0).parse(s);

    match res {
        Ok((s, op)) => {
            let operator = match op.into_fragment() {
                "|" => TypeOperator::Union,
                "&" => TypeOperator::Intersection,
                "+" => TypeOperator::Addition,
                "-" => TypeOperator::Substraction,
                "*" => TypeOperator::Multiplication,
                "/" => TypeOperator::Division,
                "$" => TypeOperator::Access,
                _ => TypeOperator::Unknown,
            };
            Ok((s, operator.into()))
        },
        Err(r) => Err(r)
    }
}


// Named "ltype" to not use the reserved symbol "type"
// Will work on union and intersection types that use infix operators
// main
pub fn ltype(s: Span) -> IResult<Span, Type> {
    let res = many1(alt((type_operator, single_type_token))).parse(s);
    match res {
        Ok((s, v)) => {
            Ok((s, VectorPriority::from(v).run()))
        },
        Err(r) => Err(r)
    }
}

pub fn char_litteral(s: Span) -> IResult<Span, Type> {
    let res = terminated(alt((
            (tag("\""), many0(none_of("\"")), tag("\"")),
            (tag("'"), many0(none_of("'")), tag("'")),
                  )), multispace0).parse(s);
    match res {
        Ok((s, (start, st, _end))) 
            => {
                let val: String = st.clone().iter().collect();
                Ok((s, Type::Char(val.into(), start.into())))
            },
        Err(r) => Err(r)
    }
}

fn type_variable(s: Span) -> IResult<Span, Type> {
    let res = variable_exp.parse(s);
    match res {
        Ok((s, (name, help_data))) 
            => Ok((s, Type::Variable(name, help_data))),
        Err(r) => Err(r)
    }
}


// main
pub fn single_type(s: Span) -> IResult<Span, Type> {
    terminated(alt((
            self_type,
            r_class,
            vector_type,
            sequence_type,
            any,
            empty,
            interface,
            label_generic,
            char_litteral,
            index_algebra,
            primitive_types,
            strict_union,
            type_alias,
            type_variable,
            generic,
            array_type,
            function_type,
            tuple_type,
            record_type,
            )), multispace0).parse(s)
}

fn single_type_token(s: Span) -> IResult<Span,TypeToken> {
    let res = single_type.parse(s);
    match res {
        Ok((s, typ)) => Ok((s, typ.into())),
        Err(r) => Err(r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder;
    use crate::graph::TypeSystem;
    use crate::type_category::TypeCategory;
    use crate::Context;

    #[test]
    fn test_alias_type1() {
        //Test if alias can be parsed
        let res = ltype("Option<T>".into()).unwrap().1;
        assert_eq!(res.to_category(), TypeCategory::Alias,
        "Parsing 'Option<T>' should give an Alias type");
    }

    #[test]
    fn test_alias_type2() {
        //Test if alias can be reduced
        let record_type = builder::record_type(&[("content".to_string(), builder::generic_type())]);
        let context = Context::default()
                        .set_new_aliase_signature("Triplet<T>", record_type);
        let type_ = ltype("Triplet<int>".into()).unwrap().1;
        let reduced_type = type_.reduce(&context);
        assert_eq!(reduced_type.pretty(), "{content: int}");
    }

    #[test]
    fn test_alias_type3() {
        //Test if alias can be parsed
        let res = ltype("Model".into()).unwrap().1;
        assert_eq!(res.to_category(), TypeCategory::Alias,
        "Parsing 'Model' should give an Alias type");
    }

    #[test]
    fn test_variable_type1(){
        let var = ltype("my_var".into()).unwrap().1;
        assert_eq!(var, Type::Variable("my_var".to_string(), HelpData::default()));
    }

    #[test]
    fn test_fabrice0(){
        let arr1 = ltype("[1, T]".into()).unwrap().1;
        let arr2 = ltype("[1, 1]".into()).unwrap().1;
        assert_eq!(
            arr2.is_subtype(&arr1, &Context::default()),
            true);
    }

    #[test]
    fn test_intersection_parsing() {
        let res = "int & char & bool".parse::<Type>().unwrap();
        assert_eq!(res.pretty(), "(& (& int char) bool)".to_string(),
                   "Parsing 'int & char & bool' should give an intersection");
    }

    #[test]
    fn test_union_parsing() {
        let res = "int | char | bool".parse::<Type>().unwrap();
        assert_eq!(res.pretty(), "(| (| int char) bool)".to_string(),
                   "Parsing 'int & char & bool' should give an intersection");
    }

    #[test]
    fn test_union_intersection_parsing() {
        let res = "int | char & bool".parse::<Type>().unwrap();
        assert_eq!(res.pretty(), "(& (| int char) bool)".to_string(),
                   "Parsing 'int & char & bool' should give an intersection");
    }

    #[test]
    fn test_interface_parsing1() {
        let res = interface("interface { hey: (num, num) -> num }".into()).unwrap().1;
        let num = builder::number_type();
        let inter = builder::interface_type(&[
                        ("hey", builder::function_type(&[num.clone(), num.clone()], num))
        ]);
        assert_eq!(res, inter);
    }

    #[test]
    fn test_interface_parsing2() {
        let res = interface("interface { hey: (Self, num) -> num }".into()).unwrap().1;
        let num = builder::number_type();
        let self_t = builder::self_generic_type();
        let inter = builder::interface_type(&[
                        ("hey", builder::function_type(&[self_t, num.clone()], num))
        ]);
        assert_eq!(res, inter);
    }

    #[test]
    fn test_interface_parsing3() {
        let res = interface("interface { hey: (Self) -> num }".into()).unwrap().1;
        let num = builder::number_type();
        let self_t = builder::self_generic_type();
        let inter = builder::interface_type(&[
                        ("hey", builder::function_type(&[self_t], num))
        ]);
        assert_eq!(res, inter);
    }

    #[test]
    fn test_char_litteral() {
        let typ = "\"char\"".parse::<Type>().unwrap();
        assert_eq!(typ.to_category(), TypeCategory::Char,
                    "Char litterals should be parsable");
    }

    #[test]
    fn test_self_ltype() {
        let typ = ltype("Self".into()).unwrap().1;
        assert_eq!(typ.to_category(), TypeCategory::Generic);
    }

}
