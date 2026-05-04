use crate::components::error_message::help_data::HelpData;
use crate::components::language::operators::{op, Op};
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::parsing::elements;
use crate::processes::parsing::elements::variable2;
use crate::processes::parsing::elements::variable_exp;
use crate::processes::parsing::operation_priority::PriorityTokens;
use crate::processes::parsing::type_token::TypeToken;
use crate::processes::parsing::vector_priority::VectorPriority;
use crate::utils::builder;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alphanumeric1;
use nom::character::complete::digit1;
use nom::character::complete::multispace0;
use nom::character::complete::none_of;
use nom::character::complete::one_of;
use nom::combinator::opt;
use nom::combinator::recognize;
use nom::multi::many0;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::sequence::terminated;
use nom::IResult;
use nom::Parser;
use nom_locate::LocatedSpan;
use std::collections::HashSet;

type Span<'a> = LocatedSpan<&'a str, String>;

// Handles the `name: type` form in function type signatures, e.g. `(a: char) -> .None`.
// The parameter name is discarded — only the type matters for type checking.
fn labeled_ltype(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        alt((embedded_ltype, ltype)),
    )
    .parse(s);
    match res {
        Ok((s, (_, _, typ))) => Ok((s, typ)),
        Err(r) => Err(r),
    }
}

fn ltype_arg(s: Span) -> IResult<Span, Type> {
    let res = terminated(
        terminated(alt((labeled_ltype, ltype)), multispace0),
        opt(terminated(tag(","), multispace0)),
    )
    .parse(s);
    match res {
        Ok((s, t)) => Ok((s, t)),
        Err(r) => Err(r),
    }
}

fn function_type(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("("), multispace0),
        many0(ltype_arg),
        terminated(tag(")"), multispace0),
        terminated(tag("->"), multispace0),
        terminated(alt((if_type, ltype)), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (start, v, _, _, t))) => {
            let args: Vec<ArgumentType> = v
                .iter()
                .enumerate()
                .map(|(i, typ)| {
                    let arg_name = crate::components::r#type::generate_arg(i);
                    ArgumentType::new(&arg_name, typ)
                })
                .collect();
            Ok((s, Type::Function(args, Box::new(t), start.into())))
        }
        Err(r) => Err(r),
    }
}

fn upper_case_generic(s: Span) -> IResult<Span, String> {
    let res = terminated(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), multispace0).parse(s);
    match res {
        Ok((s, g)) => Ok((s, g.to_string())),
        Err(r) => Err(r),
    }
}

fn self_tag(s: Span) -> IResult<Span, String> {
    let res = terminated(tag("Self"), multispace0).parse(s);
    match res {
        Ok((s, st)) => Ok((s, st.to_string())),
        Err(r) => Err(r),
    }
}

fn generic(s: Span) -> IResult<Span, Type> {
    let res = alt((upper_case_generic, self_tag)).parse(s);
    match res {
        Ok((s, g)) => Ok((s, Type::Generic(g, HelpData::default()))),
        Err(r) => Err(r),
    }
}

fn simple_index(s: Span) -> IResult<Span, Type> {
    let res = terminated(digit1, multispace0).parse(s);
    match res {
        Ok((s, fl)) => Ok((
            s,
            Type::Integer(fl.parse::<i32>().unwrap().into(), fl.into()),
        )),
        Err(r) => Err(r),
    }
}

fn index(s: Span) -> IResult<Span, Type> {
    alt((index_generic, simple_index, any)).parse(s)
}

fn array_type_full(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("["), multispace0),
        index_algebra,
        terminated(tag(","), multispace0),
        ltype,
        terminated(tag("]"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, num, _, typ, _))) => Ok((
            s,
            Type::Vec(VecType::S3, Box::new(num), Box::new(typ), start.into()),
        )),
        Err(r) => Err(r),
    }
}

fn array_type_short(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("["), multispace0),
        single_type,
        terminated(tag("]"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, typ, _))) => Ok((
            s,
            Type::Vec(
                VecType::S3,
                Box::new(Type::Any(start.clone().into())),
                Box::new(typ),
                start.into(),
            ),
        )),
        Err(r) => Err(r),
    }
}

fn array_type(s: Span) -> IResult<Span, Type> {
    alt((
        named_array_type_full,
        named_array_type_short,
        array_type_full,
        array_type_short,
    ))
    .parse(s)
}

fn named_array_type_full(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("Array["), multispace0),
        index_algebra,
        terminated(tag(","), multispace0),
        ltype,
        terminated(tag("]"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, num, _, typ, _))) => Ok((
            s,
            Type::Vec(VecType::Array, Box::new(num), Box::new(typ), start.into()),
        )),
        Err(r) => Err(r),
    }
}

fn named_array_type_short(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("Array["), multispace0),
        single_type,
        terminated(tag("]"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, typ, _))) => Ok((
            s,
            Type::Vec(
                VecType::Array,
                Box::new(Type::Any(start.clone().into())),
                Box::new(typ),
                start.into(),
            ),
        )),
        Err(r) => Err(r),
    }
}

fn named_array_type(s: Span) -> IResult<Span, Type> {
    alt((named_array_type_full, named_array_type_short)).parse(s)
}

fn vector_type_full(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("Vec["), multispace0),
        index_algebra,
        terminated(tag(","), multispace0),
        ltype,
        terminated(tag("]"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, num, _, typ, _))) => Ok((
            s,
            Type::Vec(VecType::Vector, Box::new(num), Box::new(typ), start.into()),
        )),
        Err(r) => Err(r),
    }
}

fn vector_type_short(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("Vec["), multispace0),
        single_type,
        terminated(tag("]"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, typ, _))) => Ok((
            s,
            Type::Vec(
                VecType::Vector,
                Box::new(Type::Any(start.clone().into())),
                Box::new(typ),
                start.into(),
            ),
        )),
        Err(r) => Err(r),
    }
}

fn vector_type(s: Span) -> IResult<Span, Type> {
    alt((vector_type_full, vector_type_short)).parse(s)
}

fn dataframe_type_full(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(alt((tag("dataframe["), tag("df["))), multispace0),
        index_algebra,
        terminated(tag("]"), multispace0),
        terminated(tag("{"), multispace0),
        many0(argument),
        terminated(tag("}"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, num, _, _, columns, _))) => Ok((
            s,
            Type::Vec(
                VecType::DataFrame,
                Box::new(num),
                Box::new(Type::Record(
                    columns.iter().cloned().collect(),
                    start.clone().into(),
                )),
                start.into(),
            ),
        )),
        Err(r) => Err(r),
    }
}

fn dataframe_type_short(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(alt((tag("dataframe"), tag("df"))), multispace0),
        terminated(tag("{"), multispace0),
        many0(argument),
        terminated(tag("}"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, _, columns, _))) => Ok((
            s,
            Type::Vec(
                VecType::DataFrame,
                Box::new(Type::Any(start.clone().into())),
                Box::new(Type::Record(
                    columns.iter().cloned().collect(),
                    start.clone().into(),
                )),
                start.into(),
            ),
        )),
        Err(r) => Err(r),
    }
}

fn dataframe_type(s: Span) -> IResult<Span, Type> {
    alt((dataframe_type_full, dataframe_type_short)).parse(s)
}

fn embedded_ltype(s: Span) -> IResult<Span, Type> {
    let res = (tag("@"), ltype).parse(s);
    match res {
        Ok((s, (at, ty))) => Ok((s, Type::Embedded(Box::new(ty), at.into()))),
        Err(r) => Err(r),
    }
}

fn simple_label(s: Span) -> IResult<Span, Type> {
    let res = variable2(s);
    match res.clone() {
        Ok((
            s,
            Lang::Variable {
                name, help_data: h, ..
            },
        )) => Ok((s, Type::Char(name.to_owned().into(), h.clone()))),
        Ok((_s, _)) => panic!(
            "Error: {:?} shouldn't be something different from a variable",
            res
        ),
        Err(r) => Err(r),
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
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (e1, _, Type::Embedded(ty, _), _))) => Ok((s, ArgumentType(e1, *ty.clone(), true))),
        Ok((s, (e1, _, e2, _))) => Ok((s, ArgumentType(e1, e2, false))),
        Err(r) => Err(r),
    }
}

fn record_type(s: Span) -> IResult<Span, Type> {
    let res = (
        opt(terminated(tag("list"), multispace0)),
        terminated(tag("{"), multispace0),
        many1(argument),
        terminated(tag("}"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_, start, v, _))) => {
            Ok((s, Type::Record(v.iter().cloned().collect(), start.into())))
        }
        Err(r) => Err(r),
    }
}

fn number(s: Span) -> IResult<Span, Type> {
    let res = (tag("num"), multispace0).parse(s);
    match res {
        Ok((s, (numtype, _))) => Ok((s, Type::Number(numtype.into()))),
        Err(r) => Err(r),
    }
}

fn boolean(s: Span) -> IResult<Span, Type> {
    let res = terminated(tag("bool"), multispace0).parse(s);
    match res {
        Ok((s, b)) => Ok((s, Type::Boolean(b.into()))),
        Err(r) => Err(r),
    }
}

fn null_type(s: Span) -> IResult<Span, Type> {
    let res = terminated(tag("null"), multispace0).parse(s);
    match res {
        Ok((s, n)) => Ok((s, Type::Null(n.into()))),
        Err(r) => Err(r),
    }
}

fn na_type(s: Span) -> IResult<Span, Type> {
    let res = terminated(tag("na"), multispace0).parse(s);
    match res {
        Ok((s, n)) => Ok((s, Type::NA(n.into()))),
        Err(r) => Err(r),
    }
}

fn ltype_parameter(s: Span) -> IResult<Span, Type> {
    alt((terminated(terminated(ltype, tag(",")), multispace0), ltype)).parse(s)
}

fn type_params(s: Span) -> IResult<Span, Vec<Type>> {
    let res = (
        terminated(tag("<"), multispace0),
        many0(ltype_parameter),
        terminated(tag(">"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_, v, _))) => Ok((s, v.clone())),
        Err(r) => Err(r),
    }
}

pub fn pascal_case_no_space(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = (one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), alphanumeric1).parse(s);
    match res {
        Ok((s, (t1, t2))) => Ok((s.clone(), (format!("{}{}", t1, t2.clone()), t2.into()))),
        Err(r) => Err(r),
    }
}

pub fn type_alias(s: Span) -> IResult<Span, Type> {
    let res = (
        pascal_case_no_space,
        terminated(opt(type_params), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, ((name, h), Some(v)))) => Ok((s, Type::Alias(name, v.clone(), false, h))),
        Ok((s, ((name, h), None))) => Ok((s, Type::Alias(name, vec![], false, h))),
        Err(r) => Err(r),
    }
}

fn parenthese_value(s: Span) -> IResult<Span, Type> {
    delimited(
        terminated(tag("("), multispace0),
        alt((embedded_ltype, ltype)),
        terminated(tag(")"), multispace0),
    )
    .parse(s)
}

fn chars(s: Span) -> IResult<Span, Type> {
    let res = tag("char")(s);
    match res {
        Ok((s, st)) => Ok((s, Type::Char(Tchar::Unknown, st.into()))),
        Err(r) => Err(r),
    }
}

fn interface_function(s: Span) -> IResult<Span, ArgumentType> {
    let res = (
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        function_type,
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (e, _, f, _))) => Ok((s, ArgumentType(e, f, false))),
        Err(r) => Err(r),
    }
}

fn interface(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("interface"), multispace0),
        terminated(tag("{"), multispace0),
        terminated(many1(interface_function), multispace0),
        terminated(tag("}"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (i, _, v, _))) => {
            let set = v.iter().cloned().collect::<HashSet<_>>();
            Ok((s, Type::Interface(set, i.into())))
        }
        Err(r) => Err(r),
    }
}

fn tuple_type(s: Span) -> IResult<Span, Type> {
    let res = (
        tag("("),
        many1(ltype_parameter),
        tag(")"),
    )
        .parse(s);
    match res {
        Ok((s, (ope, v, _cl))) => Ok((s, Type::Tuple(v, ope.into()))),
        Err(r) => Err(r),
    }
}

fn index_generic(s: Span) -> IResult<Span, Type> {
    let res = (tag("#"), generic).parse(s);
    match res {
        Ok((s, (tag, Type::Generic(r#gen, _)))) => Ok((s, Type::IndexGen(r#gen, tag.into()))),
        Err(r) => Err(r),
        _ => todo!(),
    }
}

fn label_generic(s: Span) -> IResult<Span, Type> {
    let res = (tag("$"), generic).parse(s);
    match res {
        Ok((s, (tag, Type::Generic(r#gen, _)))) => Ok((s, Type::LabelGen(r#gen, tag.into()))),
        Err(r) => Err(r),
        _ => todo!(),
    }
}

fn integer(s: Span) -> IResult<Span, Type> {
    let res = (tag("int"), multispace0).parse(s);
    match res {
        Ok((s, (inttype, _))) => Ok((s.clone(), Type::Integer(Tint::Unknown, inttype.into()))),
        Err(r) => Err(r),
    }
}

fn any(s: Span) -> IResult<Span, Type> {
    match tag("Any")(s) {
        Ok((s, e)) => Ok((s, Type::Any(e.into()))),
        Err(r) => Err(r),
    }
}

fn self_type(s: Span) -> IResult<Span, Type> {
    match tag("Self")(s) {
        Ok((s, e)) => Ok((s, builder::self_generic_type().set_help_data(e.into()))),
        Err(r) => Err(r),
    }
}

fn empty(s: Span) -> IResult<Span, Type> {
    match tag("Empty")(s) {
        Ok((s, e)) => Ok((s, Type::Empty(e.into()))),
        Err(r) => Err(r),
    }
}

fn compute_operators(v: &mut Vec<(Type, Op)>) -> Type {
    // (params, op)
    let first = v.pop().unwrap();
    match first {
        (p, Op::Add(_)) => {
            let res = compute_operators(v);
            let pp = p;
            Type::Add(Box::new(res.clone()), Box::new(pp), res.into())
        }
        (p, Op::Minus(_)) => {
            let res = compute_operators(v);
            let pp = p;
            Type::Minus(Box::new(res.clone()), Box::new(pp), res.into())
        }
        (p, Op::Mul(_)) => {
            let res = compute_operators(v);
            let pp = p;
            Type::Mul(Box::new(res.clone()), Box::new(pp), res.into())
        }
        (p, Op::Div(_)) => {
            let res = compute_operators(v);
            let pp = p;
            Type::Div(Box::new(res.clone()), Box::new(pp), res.into())
        }
        (p, Op::Empty(_)) => p,
        _ => panic!(),
    }
}

fn index_operator(s: Span) -> IResult<Span, (Type, Op)> {
    let res = (opt(op), index).parse(s);
    match res {
        Ok((s, (Some(ope), ele))) => Ok((s, (ele, ope))),
        Ok((s, (None, ele))) => Ok((s.clone(), (ele, Op::Empty(s.into())))),
        Err(r) => Err(r),
    }
}

fn index_chain(s: Span) -> IResult<Span, Type> {
    let res = many1(index_operator).parse(s);
    match res {
        Ok((s, v)) => Ok((s, compute_operators(&mut v.clone()))),
        Err(r) => Err(r),
    }
}

fn index_algebra(s: Span) -> IResult<Span, Type> {
    alt((index_chain, index)).parse(s)
}

fn type_condition(s: Span) -> IResult<Span, Type> {
    let res = (ltype, op, ltype).parse(s);
    match res {
        Ok((s, (t1, ope, t2))) => {
            let new_ope = ope.to_type().expect("This is not a valid type operator");
            Ok((
                s,
                Type::Condition(
                    Box::new(t1),
                    Box::new(new_ope.clone()),
                    Box::new(t2),
                    new_ope.into(),
                ),
            ))
        }
        Err(r) => Err(r),
    }
}

pub fn if_type(s: Span) -> IResult<Span, Type> {
    // if conditions
    let res = (ltype, tag("if "), many1(type_condition)).parse(s);
    match res {
        Ok((s, (typ, _if, t_conds))) => {
            Ok((s, Type::If(Box::new(typ.clone()), t_conds, typ.into())))
        }
        Err(r) => Err(r),
    }
}

fn r_class(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("Class("), multispace0),
        many1(terminated(
            terminated(recognize(elements::chars), opt(tag(","))),
            multispace0,
        )),
        terminated(tag(")"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (class, elems, _close))) => Ok((
            s,
            Type::RClass(elems.iter().map(|x| x.to_string()).collect(), class.into()),
        )),
        Err(r) => Err(r),
    }
}

pub fn primitive_types(s: Span) -> IResult<Span, Type> {
    alt((number, integer, boolean, null_type, na_type, chars)).parse(s)
}

fn type_operator(s: Span) -> IResult<Span, TypeToken> {
    let res = terminated(
        alt((
            tag("->"),
            tag("|"),
            tag("&"),
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("$"),
        )),
        multispace0,
    )
    .parse(s);

    match res {
        Ok((s, op)) => {
            let operator = match op.into_fragment() {
                "->" => TypeOperator::Arrow,
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
        }
        Err(r) => Err(r),
    }
}

// Named "ltype" to not use the reserved symbol "type"
// Will work on union and intersection types that use infix operators
// main
pub fn ltype(s: Span) -> IResult<Span, Type> {
    let res = many1(alt((type_operator, single_type_token))).parse(s);
    match res {
        Ok((s, v)) => Ok((s, VectorPriority::from(v).run())),
        Err(r) => Err(r),
    }
}

pub fn char_litteral(s: Span) -> IResult<Span, Type> {
    let res = terminated(
        alt((
            (tag("\""), many0(none_of("\"")), tag("\"")),
            (tag("'"), many0(none_of("'")), tag("'")),
        )),
        multispace0,
    )
    .parse(s);
    match res {
        Ok((s, (start, st, _end))) => {
            let val: String = st.clone().iter().collect();
            Ok((s, Type::Char(val.into(), start.into())))
        }
        Err(r) => Err(r),
    }
}

fn type_variable(s: Span) -> IResult<Span, Type> {
    let res = variable_exp.parse(s);
    match res {
        Ok((s, (name, help_data))) => Ok((s, Type::Variable(name, help_data))),
        Err(r) => Err(r),
    }
}

fn unknown_function(s: Span) -> IResult<Span, Type> {
    let res = tag("UnknownFunction").parse(s);
    match res {
        Ok((s, rfunc)) => Ok((s, Type::UnknownFunction(rfunc.into()))),
        Err(r) => Err(r),
    }
}

fn tag_type(s: Span) -> IResult<Span, Type> {
    let res = (
        tag("."),
        pascal_case_no_space,
        opt((tag("("), ltype, tag(")"))),
    )
        .parse(s);
    match res {
        Ok((s, (dot, (name, _), Some(body)))) => {
            Ok((s, Type::Tag(name, Box::new(body.1), dot.into())))
        }
        Ok((s, (dot, (name, h), None))) => {
            let empty = builder::empty_type().set_help_data(h);
            Ok((s, Type::Tag(name, Box::new(empty), dot.into())))
        }
        Err(r) => Err(r),
    }
}

// main
pub fn single_type(s: Span) -> IResult<Span, Type> {
    terminated(
        alt((
            function_type,
            self_type,
            r_class,
            unknown_function,
            vector_type,
            dataframe_type,
            record_type,
            parenthese_value,
            tag_type,
            any,
            empty,
            interface,
            label_generic,
            char_litteral,
            index_algebra,
            primitive_types,
            type_alias,
            type_variable,
            generic,
            array_type,
            tuple_type,
        )),
        multispace0,
    )
    .parse(s)
}

fn single_type_token(s: Span) -> IResult<Span, TypeToken> {
    let res = single_type.parse(s);
    match res {
        Ok((s, typ)) => Ok((s, typ.into())),
        Err(r) => Err(r),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::context::Context;
    use crate::components::r#type::type_category::TypeCategory;
    use crate::components::r#type::type_system::TypeSystem;
    use crate::utils::builder;

    #[test]
    fn test_fabrice0() {
        let arr1 = ltype("[1, T]".into()).unwrap().1;
        let arr2 = ltype("[1, 1]".into()).unwrap().1;
        assert_eq!(arr2.is_subtype(&arr1, &Context::default()).0, true);
    }

    #[test]
    fn test_interface_parsing1() {
        let res = interface("interface { hey: (num, num) -> num }".into())
            .unwrap()
            .1;
        let num = builder::number_type();
        let inter = builder::interface_type(&[(
            "hey",
            builder::function_type(&[num.clone(), num.clone()], num),
        )]);
        assert_eq!(res, inter);
    }

    #[test]
    fn test_interface_parsing2() {
        let res = interface("interface { hey: (Self, num) -> num }".into())
            .unwrap()
            .1;
        let num = builder::number_type();
        let self_t = builder::self_generic_type();
        let inter = builder::interface_type(&[(
            "hey",
            builder::function_type(&[self_t, num.clone()], num),
        )]);
        assert_eq!(res, inter);
    }

    #[test]
    fn test_interface_parsing3() {
        let res = interface("interface { hey: (Self) -> num }".into())
            .unwrap()
            .1;
        let num = builder::number_type();
        let self_t = builder::self_generic_type();
        let inter = builder::interface_type(&[("hey", builder::function_type(&[self_t], num))]);
        assert_eq!(res, inter);
    }

    #[test]
    fn test_char_litteral() {
        let typ = "\"char\"".parse::<Type>().unwrap();
        assert_eq!(
            typ.to_category(),
            TypeCategory::Char,
            "Char litterals should be parsable"
        );
    }

    #[test]
    fn test_self_ltype() {
        let typ = ltype("Self".into()).unwrap().1;
        assert_eq!(typ.to_category(), TypeCategory::Generic);
    }

    #[test]
    fn test_function_signature1() {
        let typ = ltype("() -> Empty".into()).unwrap().1;
        assert_eq!(typ.pretty(), "fn() -> Empty".to_string());
    }

    #[test]
    fn test_char_litteral1() {
        let typ = char_litteral("'hello'".into()).unwrap().1;
        assert_eq!(typ.pretty(), "hello".to_string());
    }

    #[test]
    fn test_dataframe_parsing1() {
        let res = dataframe_type("dataframe[#N]{ name: char, age: int }".into());
        assert!(res.is_ok(), "dataframe type should parse successfully");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::DataFrame, _, body, _) => match body.as_ref() {
                Type::Record(fields, _) => assert_eq!(fields.len(), 2),
                other => panic!("Expected Record body, got {:?}", other),
            },
            other => panic!("Expected Vec(DataFrame, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_dataframe_parsing2() {
        let res = dataframe_type("dataframe[3]{ x: num }".into());
        assert!(
            res.is_ok(),
            "dataframe type with numeric index should parse"
        );
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::DataFrame, idx, body, _) => {
                match idx.as_ref() {
                    Type::Integer(_, _) => {}
                    other => panic!("Expected Integer index, got {:?}", other),
                }
                match body.as_ref() {
                    Type::Record(fields, _) => assert_eq!(fields.len(), 1),
                    other => panic!("Expected Record body, got {:?}", other),
                }
            }
            other => panic!("Expected Vec(DataFrame, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_dataframe_via_ltype() {
        let res = ltype("dataframe[#N]{ col1: int, col2: bool }".into());
        assert!(res.is_ok(), "dataframe should be parseable via ltype");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::DataFrame, _, _, _) => {}
            other => panic!("Expected Vec(DataFrame, ...) via ltype, got {:?}", other),
        }
    }

    #[test]
    fn test_dataframe_df_shorthand() {
        let res = dataframe_type("df[#N]{ name: char, age: int }".into());
        assert!(res.is_ok(), "df shorthand should parse successfully");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::DataFrame, _, body, _) => match body.as_ref() {
                Type::Record(fields, _) => assert_eq!(fields.len(), 2),
                other => panic!("Expected Record body, got {:?}", other),
            },
            other => panic!("Expected Vec(DataFrame, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_dataframe_any_index() {
        let res = dataframe_type("dataframe[Any]{ name: char }".into());
        assert!(res.is_ok(), "dataframe with Any index should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::DataFrame, idx, _, _) => match idx.as_ref() {
                Type::Any(_) => {}
                other => panic!("Expected Any index, got {:?}", other),
            },
            other => panic!("Expected Vec(DataFrame, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_vector_any_index() {
        let res = vector_type("Vec[Any, num]".into());
        assert!(res.is_ok(), "Vec with Any index should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::Vector, idx, _, _) => match idx.as_ref() {
                Type::Any(_) => {}
                other => panic!("Expected Any index, got {:?}", other),
            },
            other => panic!("Expected Vec(Vector, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_array_any_index() {
        let res = array_type("[Any, int]".into());
        assert!(res.is_ok(), "Array with Any index should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::S3, idx, _, _) => match idx.as_ref() {
                Type::Any(_) => {}
                other => panic!("Expected Any index, got {:?}", other),
            },
            other => panic!("Expected Vec(S3, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_dataframe_short_form() {
        let res = dataframe_type("dataframe{ name: char, age: int }".into());
        assert!(res.is_ok(), "dataframe without index should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::DataFrame, idx, body, _) => {
                match idx.as_ref() {
                    Type::Any(_) => {}
                    other => panic!("Expected Any index, got {:?}", other),
                }
                match body.as_ref() {
                    Type::Record(fields, _) => assert_eq!(fields.len(), 2),
                    other => panic!("Expected Record body, got {:?}", other),
                }
            }
            other => panic!("Expected Vec(DataFrame, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_df_short_form() {
        let res = dataframe_type("df{ x: num }".into());
        assert!(res.is_ok(), "df without index should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::DataFrame, idx, _, _) => match idx.as_ref() {
                Type::Any(_) => {}
                other => panic!("Expected Any index, got {:?}", other),
            },
            other => panic!("Expected Vec(DataFrame, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_vector_short_form() {
        let res = vector_type("Vec[num]".into());
        assert!(res.is_ok(), "Vec[type] short form should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::Vector, idx, body, _) => {
                match idx.as_ref() {
                    Type::Any(_) => {}
                    other => panic!("Expected Any index, got {:?}", other),
                }
                match body.as_ref() {
                    Type::Number(_) => {}
                    other => panic!("Expected Number body, got {:?}", other),
                }
            }
            other => panic!("Expected Vec(Vector, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_array_short_form() {
        let res = array_type("[int]".into());
        assert!(res.is_ok(), "[type] short form should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::S3, idx, body, _) => {
                match idx.as_ref() {
                    Type::Any(_) => {}
                    other => panic!("Expected Any index, got {:?}", other),
                }
                match body.as_ref() {
                    Type::Integer(_, _) => {}
                    other => panic!("Expected Integer body, got {:?}", other),
                }
            }
            other => panic!("Expected Vec(S3, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_named_array_full() {
        let res = array_type("Array[3, int]".into());
        assert!(res.is_ok(), "Array[index, type] should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::Array, idx, body, _) => {
                match idx.as_ref() {
                    Type::Integer(_, _) => {}
                    other => panic!("Expected Integer index, got {:?}", other),
                }
                match body.as_ref() {
                    Type::Integer(_, _) => {}
                    other => panic!("Expected Integer body, got {:?}", other),
                }
            }
            other => panic!("Expected Vec(Array, ...), got {:?}", other),
        }
    }

    #[test]
    fn test_named_array_short() {
        let res = array_type("Array[num]".into());
        assert!(res.is_ok(), "Array[type] short form should parse");
        let typ = res.unwrap().1;
        match &typ {
            Type::Vec(VecType::Array, idx, body, _) => {
                match idx.as_ref() {
                    Type::Any(_) => {}
                    other => panic!("Expected Any index, got {:?}", other),
                }
                match body.as_ref() {
                    Type::Number(_) => {}
                    other => panic!("Expected Number body, got {:?}", other),
                }
            }
            other => panic!("Expected Vec(Array, ...), got {:?}", other),
        }
    }

    /// Regression: `ltype` must NOT consume `{ .Rouge }` as a type token after `Feux`.
    /// Previously `tuple_type` accepted `{...}` which caused `fn(i: int): Feux { .Rouge }`
    /// to be misparsed (body consumed as part of return type).
    #[test]
    fn test_ltype_stops_before_brace_body() {
        use crate::processes::parsing::elements::simple_function;
        let res = simple_function("fn(i: int): Feux { .Rouge }".into());
        assert!(res.is_ok(), "Should parse fn with tag-alias return type");
        let (remaining, _lang) = res.unwrap();
        assert!(
            remaining.is_empty(),
            "Should consume the whole input, remaining: {:?}",
            *remaining
        );
    }

    // ==================== Named parameter in function type ====================

    #[test]
    fn test_named_param_function_type_parses() {
        let res = ltype("(a: char) -> .None".into());
        assert!(res.is_ok(), "Should parse (a: char) -> .None as a function type");
    }

    #[test]
    fn test_named_param_signature_type_checks() {
        use crate::utils::fluent_parser::FluentParser;
        // The return type is .None = Tag("None", Empty)
        let res = FluentParser::new()
            .push("@print: (a: char) -> .None;")
            .run()
            .check_typing("print(\"Hello world\")");
        assert_eq!(
            res,
            Type::Tag(
                "None".to_string(),
                Box::new(Type::Empty(HelpData::default())),
                HelpData::default()
            )
        );
    }

    #[test]
    fn test_named_param_multi_args() {
        let res = ltype("(a: int, b: int) -> int".into());
        assert!(res.is_ok(), "Should parse (a: int, b: int) -> int as a function type");
    }
}
