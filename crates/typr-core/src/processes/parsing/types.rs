use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::syntax_error::SyntaxError;
use crate::components::language::operators::{op, Op};
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::tbool::Tbool;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::tnumber::Tnum;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::parsing::elements;
use crate::processes::parsing::elements::variable2;
use crate::processes::parsing::elements::variable_exp;
use crate::processes::parsing::operation_priority::PriorityTokens;
use crate::processes::parsing::push_parse_error;
use crate::processes::parsing::type_token::TypeToken;
use crate::processes::parsing::vector_priority::VectorPriority;
use crate::utils::builder;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alphanumeric1;
use nom::character::complete::digit1;
use nom::character::complete::multispace0;
use nom::character::complete::multispace1;
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
        ltype,
    )
        .parse(s);
    match res {
        Ok((s, (_, _, typ))) => Ok((s, typ)),
        Err(r) => Err(r),
    }
}

fn ltype_arg(s: Span) -> IResult<Span, (bool, Type)> {
    // Try variadic: `...Type` or `...name: Type`
    let variadic = (
        terminated(tag("..."), multispace0),
        terminated(alt((labeled_ltype, ltype)), multispace0),
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s.clone());
    if let Ok((s2, (_, typ, _))) = variadic {
        return Ok((s2, (true, typ)));
    }
    // Regular: `Type` or `name: Type`
    let res = terminated(
        terminated(alt((labeled_ltype, ltype)), multispace0),
        opt(terminated(tag(","), multispace0)),
    )
    .parse(s);
    match res {
        Ok((s, t)) => Ok((s, (false, t))),
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
                .map(|(i, (is_variadic, typ))| {
                    let arg_name = crate::components::r#type::generate_arg(i);
                    ArgumentType::new(&arg_name, typ).set_variadic(*is_variadic)
                })
                .collect();
            Ok((s, Type::Function(args, Box::new(t), start.into())))
        }
        Err(r) => Err(r),
    }
}

/// Catches `fn(args) -> Type` in type context and produces a helpful error.
/// This is identical to `function_type` but with an extra leading `fn` tag.
fn fn_function_type_error(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("fn"), multispace0),
        terminated(tag("("), multispace0),
        many0(ltype_arg),
        terminated(tag(")"), multispace0),
        terminated(tag("->"), multispace0),
        terminated(alt((if_type, ltype)), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (fn_span, _, v, _, _, t))) => {
            let args: Vec<ArgumentType> = v
                .iter()
                .enumerate()
                .map(|(i, (is_variadic, typ))| {
                    let arg_name = crate::components::r#type::generate_arg(i);
                    ArgumentType::new(&arg_name, typ).set_variadic(*is_variadic)
                })
                .collect();
            let help_data: HelpData = fn_span.clone().into();
            push_parse_error(SyntaxError::FunctionTypeSyntax(help_data));
            Ok((s, Type::Function(args, Box::new(t), fn_span.into())))
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

/// Generic record constructor usage following the golden rule:
/// `Name[N]{ field: Type, ... }` — a `{ ... }` block makes it a record type.
/// Produces `Type::Vec(VecType::Named(name), index, Record(fields), h)`.
fn named_record_type(s: Span) -> IResult<Span, Type> {
    let res = (
        pascal_case_no_space,
        terminated(tag("["), multispace0),
        index_algebra,
        terminated(tag("]"), multispace0),
        terminated(tag("{"), multispace0),
        many1(argument),
        terminated(tag("}"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, ((name, h), _, num, _, _, columns, _))) => Ok((
            s,
            Type::Vec(
                VecType::Named(name),
                Box::new(num),
                Box::new(Type::Record(columns.iter().cloned().collect(), h.clone())),
                h,
            ),
        )),
        Err(r) => Err(r),
    }
}

/// Golden-rule violation: a record constructor with more than one index parameter,
/// e.g. `Df[8, int]{ ... }`. Emits a syntax error and recovers using the first index.
fn named_record_bad_index(s: Span) -> IResult<Span, Type> {
    let res = (
        pascal_case_no_space,
        terminated(tag("["), multispace0),
        index_algebra,
        many1((terminated(tag(","), multispace0), ltype)),
        terminated(tag("]"), multispace0),
        terminated(tag("{"), multispace0),
        many1(argument),
        terminated(tag("}"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, ((name, h), _, first, _extra, _, _, columns, _))) => {
            push_parse_error(SyntaxError::RecordConstructorIndex(h.clone()));
            Ok((
                s,
                Type::Vec(
                    VecType::Named(name),
                    Box::new(first),
                    Box::new(Type::Record(columns.iter().cloned().collect(), h.clone())),
                    h,
                ),
            ))
        }
        Err(r) => Err(r),
    }
}

fn record_constructor_type(s: Span) -> IResult<Span, Type> {
    alt((named_record_type, named_record_bad_index)).parse(s)
}

/// Golden-rule violation: a record block `{ ... }` appearing inside the parameters
/// of a recursive constructor, e.g. `Array[5, { a: int }]`. Emits a syntax error
/// and recovers by treating the block as the element record.
fn recursive_with_record_error(s: Span) -> IResult<Span, Type> {
    let res = (
        alt((tag("Array["), tag("Vec["), tag("["))),
        index_algebra,
        terminated(tag(","), multispace0),
        terminated(tag("{"), multispace0),
        many1(argument),
        terminated(tag("}"), multispace0),
        terminated(tag("]"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (start, num, _, _, columns, _, _))) => {
            let h: HelpData = start.into();
            push_parse_error(SyntaxError::RecordInRecursiveParams(h.clone()));
            Ok((
                s,
                Type::Vec(
                    VecType::S3,
                    Box::new(num),
                    Box::new(Type::Record(columns.iter().cloned().collect(), h.clone())),
                    h,
                ),
            ))
        }
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

/// Named type embedding (RFC: `type_embedding.md`): an `embed` prefix on a
/// record field, e.g. `embed coords: Position`, makes `ArgumentType::is_embedded()`
/// true for that field. `embed` is a soft keyword recognized only in this
/// position; it requires trailing whitespace so a field genuinely named
/// `embed` (immediately followed by `:`) still parses as a plain field name.
fn embed_keyword(s: Span) -> IResult<Span, Span> {
    terminated(tag("embed"), multispace1).parse(s)
}

pub fn argument(s: Span) -> IResult<Span, ArgumentType> {
    let res = (
        opt(embed_keyword),
        terminated(label, multispace0),
        terminated(tag(":"), multispace0),
        ltype,
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (embed, e1, _, e2, _))) => Ok((s, ArgumentType(e1, e2, embed.is_some(), false))),
        Err(r) => Err(r),
    }
}

fn record_type(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("list"), multispace0),
        terminated(tag("{"), multispace0),
        many0(argument),
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
        Ok((s, (numtype, _))) => Ok((s, Type::Number(Tnum::Unknown, numtype.into()))),
        Err(r) => Err(r),
    }
}

fn number_literal(s: Span) -> IResult<Span, Type> {
    use nom::character::complete::char as nchar;
    use nom::combinator::recognize;
    let res = terminated(
        recognize((opt(nchar('-')), digit1, nchar('.'), digit1)),
        multispace0,
    )
    .parse(s);
    match res {
        Ok((s, span)) => {
            let val: f64 = (*span).parse().unwrap_or(0.0);
            Ok((s, Type::Number(Tnum::Val(val), span.into())))
        }
        Err(r) => Err(r),
    }
}

fn boolean(s: Span) -> IResult<Span, Type> {
    let res = terminated(tag("bool"), multispace0).parse(s);
    match res {
        Ok((s, b)) => Ok((s, Type::Boolean(Tbool::Unknown, b.into()))),
        Err(r) => Err(r),
    }
}

fn boolean_literal(s: Span) -> IResult<Span, Type> {
    let res = alt((
        terminated(tag("true"), multispace0),
        terminated(tag("false"), multispace0),
    ))
    .parse(s);
    match res {
        Ok((s, span)) => {
            let val = *span == "true";
            Ok((s, Type::Boolean(Tbool::Val(val), span.into())))
        }
        Err(r) => Err(r),
    }
}

fn integer_literal(s: Span) -> IResult<Span, Type> {
    let res = terminated(digit1, multispace0).parse(s);
    match res {
        Ok((s, span)) => {
            let val: i32 = (*span).parse().unwrap_or(0);
            Ok((s, Type::Integer(Tint::Val(val), span.into())))
        }
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
        ltype,
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
        alt((function_type, fn_function_type_error)),
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (e, _, f, _))) => Ok((s, ArgumentType(e, f, false, false))),
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
            // Normalize function parameter names in interface method signatures
            // so that `fn(s: Self) -> Self` compares equal to `fn(a: Self) -> Self`.
            let set = v
                .iter()
                .cloned()
                .map(|arg| {
                    let normalized_type = arg.get_type().normalize_fn_param_names();
                    arg.set_type(normalized_type)
                })
                .collect::<HashSet<_>>();
            Ok((s, Type::Interface(set, i.into())))
        }
        Err(r) => Err(r),
    }
}

fn tuple_type(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("list"), multispace0),
        terminated(tag("{"), multispace0),
        many1(ltype_parameter),
        terminated(tag("}"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (ope, _, v, _cl))) => Ok((s, Type::Tuple(v, ope.into()))),
        Err(r) => Err(r),
    }
}

/// `tuple{T1, T2, ...}` or `tuple{T..., U}` — explicit tuple type (spec §2.1)
/// Uses `tuple_bracket_param` so that variadic seq-vars (`T...`) are recognised.
fn explicit_tuple_type(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("tuple"), multispace0),
        terminated(tag("{"), multispace0),
        many0(tuple_bracket_param),
        terminated(tag("}"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (start, _, v, _))) => Ok((s, Type::Tuple(v, start.into()))),
        Err(r) => Err(r),
    }
}

/// `record{name: T, ...}` or `record{Fs..., name: T}` — explicit record type (spec §2.2)
/// Uses `variadic_field_seq` so that field seq-vars (`Fs...`) are recognised.
fn explicit_record_type(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("record"), multispace0),
        terminated(tag("{"), multispace0),
        many0(alt((variadic_field_seq, argument))),
        terminated(tag("}"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (start, _, v, _))) => {
            Ok((s, Type::Record(v.iter().cloned().collect(), start.into())))
        }
        Err(r) => Err(r),
    }
}

/// `T...` or `Ts...` — variadic type-sequence variable (spec §1.1, §4.1)
/// Produces `Type::Multi(Generic(name))`.
fn type_seq_var(s: Span) -> IResult<Span, Type> {
    let res = (
        one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        opt(alphanumeric1),
        terminated(tag("..."), multispace0),
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (first, rest, _, _))) => {
            let tail = rest.map(|r| r.to_string()).unwrap_or_default();
            let name = format!("{}{}", first, tail);
            let h = HelpData::default();
            Ok((s, Type::Multi(Box::new(Type::Generic(name, h.clone())), h)))
        }
        Err(r) => Err(r),
    }
}

/// A single element inside `Tuple[...]`: either a variadic seq-var or a regular type.
fn tuple_bracket_param(s: Span) -> IResult<Span, Type> {
    alt((type_seq_var, ltype_parameter)).parse(s)
}

/// `Tuple[T1, T2, ...]` or `Tuple[T..., U]` — bracket tuple type (spec §2.1)
fn tuple_bracket_type(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("Tuple"), multispace0),
        terminated(tag("["), multispace0),
        many0(tuple_bracket_param),
        terminated(tag("]"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (start, _, v, _))) => Ok((s, Type::Tuple(v, start.into()))),
        Err(r) => Err(r),
    }
}

/// `Fs...` inside `Record[...]` — variadic field-sequence variable (spec §1.2, §4.2)
fn variadic_field_seq(s: Span) -> IResult<Span, ArgumentType> {
    let res = (
        one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        opt(alphanumeric1),
        terminated(tag("..."), multispace0),
        opt(terminated(tag(","), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (first, rest, _, _))) => {
            let tail = rest.map(|r| r.to_string()).unwrap_or_default();
            let name = format!("{}{}", first, tail);
            let h = HelpData::default();
            let label = Type::Char(name.clone().into(), h.clone());
            let typ = Type::Generic(name, h.clone());
            Ok((s, ArgumentType(label, typ, false, true)))
        }
        Err(r) => Err(r),
    }
}

/// `Record[name: T, ...]` or `Record[Fs..., name: T]` — bracket record type (spec §2.2)
fn record_bracket_type(s: Span) -> IResult<Span, Type> {
    let res = (
        terminated(tag("Record"), multispace0),
        terminated(tag("["), multispace0),
        many0(alt((variadic_field_seq, argument))),
        terminated(tag("]"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (start, _, v, _))) => {
            Ok((s, Type::Record(v.iter().cloned().collect(), start.into())))
        }
        Err(r) => Err(r),
    }
}

/// Groups all `list`/`tuple`/`record` brace-delimited types (spec §2).
fn list_types(s: Span) -> IResult<Span, Type> {
    alt((
        explicit_tuple_type,
        explicit_record_type,
        record_type,
        tuple_type,
    ))
    .parse(s)
}

/// `Tuple[...]` and `Record[...]` bracket notation — must be tried before `type_alias`
/// so that `Tuple`/`Record` are not swallowed as bare alias names.
fn bracket_tuple_record(s: Span) -> IResult<Span, Type> {
    alt((tuple_bracket_type, record_bracket_type)).parse(s)
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
            Type::Operator(
                TypeOperator::Addition,
                Box::new(res.clone()),
                Box::new(pp),
                res.into(),
            )
        }
        (p, Op::Minus(_)) => {
            let res = compute_operators(v);
            let pp = p;
            Type::Operator(
                TypeOperator::Substraction,
                Box::new(res.clone()),
                Box::new(pp),
                res.into(),
            )
        }
        (p, Op::Mul(_)) => {
            let res = compute_operators(v);
            let pp = p;
            Type::Operator(
                TypeOperator::Multiplication,
                Box::new(res.clone()),
                Box::new(pp),
                res.into(),
            )
        }
        (p, Op::Div(_)) => {
            let res = compute_operators(v);
            let pp = p;
            Type::Operator(
                TypeOperator::Division,
                Box::new(res.clone()),
                Box::new(pp),
                res.into(),
            )
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
    alt((
        number_literal,
        number,
        integer_literal,
        integer,
        boolean_literal,
        boolean,
        null_type,
        na_type,
        chars,
    ))
    .parse(s)
}

fn type_operator(s: Span) -> IResult<Span, TypeToken> {
    let res = terminated(
        alt((
            tag("->"),
            tag("::"),
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
                "::" | "$" => TypeOperator::Access,
                "|" => TypeOperator::Union,
                "&" => TypeOperator::Intersection,
                "+" => TypeOperator::Addition,
                "-" => TypeOperator::Substraction,
                "*" => TypeOperator::Multiplication,
                "/" => TypeOperator::Division,
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

/// All bracketed/record-vec forms, ordered to respect the golden rule
/// (a `{ ... }` block makes a type a record). Grouped into a single parser so
/// `single_type` stays within nom's 21-alternative limit.
fn composite_vec_type(s: Span) -> IResult<Span, Type> {
    alt((
        recursive_with_record_error,
        vector_type,
        dataframe_type,
        record_constructor_type,
        array_type,
    ))
    .parse(s)
}

// main
pub fn single_type(s: Span) -> IResult<Span, Type> {
    terminated(
        alt((
            alt((function_type, fn_function_type_error)),
            self_type,
            r_class,
            unknown_function,
            bracket_tuple_record, // Tuple[...] / Record[...] — before type_alias
            composite_vec_type,
            list_types, // tuple{}/record{}/list{} brace forms
            parenthese_value,
            tag_type,
            any,
            empty,
            interface,
            label_generic,
            char_litteral,
            primitive_types,
            index_algebra,
            type_alias,
            type_variable,
            generic,
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
                    Type::Number(_, _) => {}
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
                    Type::Number(_, _) => {}
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
        assert!(
            res.is_ok(),
            "Should parse (a: char) -> .None as a function type"
        );
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
        assert!(
            res.is_ok(),
            "Should parse (a: int, b: int) -> int as a function type"
        );
    }

    // ==================== Generic record constructors (typeconstructor) ====================

    #[test]
    fn test_named_record_constructor_parses() {
        let typ = ltype("Tibble[3]{ id: int, active: bool }".into())
            .unwrap()
            .1;
        match &typ {
            Type::Vec(VecType::Named(name), idx, body, _) => {
                assert_eq!(name, "Tibble");
                assert!(matches!(idx.as_ref(), Type::Integer(_, _)));
                match body.as_ref() {
                    Type::Record(fields, _) => assert_eq!(fields.len(), 2),
                    other => panic!("Expected Record body, got {:?}", other),
                }
            }
            other => panic!("Expected Vec(Named(\"Tibble\"), ...), got {:?}", other),
        }
    }

    #[test]
    fn test_named_record_without_brace_is_not_record() {
        // Golden rule: no `{ ... }` block ⇒ not a record constructor.
        let typ = ltype("Tibble".into()).unwrap().1;
        assert!(
            matches!(typ, Type::Alias(ref n, _, _, _) if n == "Tibble"),
            "Bare PascalCase should be a type alias, got {:?}",
            typ
        );
    }

    #[test]
    fn test_record_constructor_multi_index_errors() {
        // `Df[8, int]{...}` is invalid: a record constructor takes exactly one index.
        let _ = crate::processes::parsing::take_parse_errors();
        let _ = ltype("Df[8, int]{ name: char }".into());
        let errors = crate::processes::parsing::take_parse_errors();
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SyntaxError::RecordConstructorIndex(_))),
            "Expected RecordConstructorIndex error, got {:?}",
            errors
        );
    }

    #[test]
    fn test_record_in_recursive_params_errors() {
        // `Array[5, { a: int }]` is invalid: record block inside recursive params.
        let _ = crate::processes::parsing::take_parse_errors();
        let _ = ltype("Array[5, { a: int }]".into());
        let errors = crate::processes::parsing::take_parse_errors();
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SyntaxError::RecordInRecursiveParams(_))),
            "Expected RecordInRecursiveParams error, got {:?}",
            errors
        );
    }

    // ── Spec §2 : explicit tuple/record keyword syntax ───────────────────────

    #[test]
    fn test_explicit_tuple_non_empty() {
        let typ = ltype("tuple{int, char}".into()).unwrap().1;
        assert!(matches!(typ, Type::Tuple(ref v, _) if v.len() == 2));
    }

    #[test]
    fn test_explicit_tuple_empty() {
        let typ = ltype("tuple{}".into()).unwrap().1;
        assert!(matches!(typ, Type::Tuple(ref v, _) if v.is_empty()));
    }

    #[test]
    fn test_explicit_record_non_empty() {
        let typ = ltype("record{x: int, y: char}".into()).unwrap().1;
        assert!(matches!(typ, Type::Record(ref f, _) if f.len() == 2));
    }

    #[test]
    fn test_explicit_record_empty() {
        let typ = ltype("record{}".into()).unwrap().1;
        assert!(matches!(typ, Type::Record(ref f, _) if f.is_empty()));
    }

    // ── Spec §2.1 : Tuple[...] bracket notation ──────────────────────────────

    #[test]
    fn test_tuple_bracket_concrete() {
        let typ = ltype("Tuple[int, char]".into()).unwrap().1;
        assert!(matches!(typ, Type::Tuple(ref v, _) if v.len() == 2));
    }

    #[test]
    fn test_tuple_bracket_empty() {
        let typ = ltype("Tuple[]".into()).unwrap().1;
        assert!(matches!(typ, Type::Tuple(ref v, _) if v.is_empty()));
    }

    #[test]
    fn test_tuple_bracket_single_seq_var() {
        // Tuple[T...] — variadic sequence variable
        let typ = ltype("Tuple[T...]".into()).unwrap().1;
        match &typ {
            Type::Tuple(elems, _) => {
                assert_eq!(elems.len(), 1);
                assert!(
                    matches!(&elems[0], Type::Multi(inner, _) if matches!(inner.as_ref(), Type::Generic(n, _) if n == "T"))
                );
            }
            other => panic!("Expected Tuple, got {:?}", other),
        }
    }

    #[test]
    fn test_tuple_bracket_seq_var_then_concrete() {
        // Tuple[T..., U] — variadic prefix then concrete type
        let typ = ltype("Tuple[T..., U]".into()).unwrap().1;
        match &typ {
            Type::Tuple(elems, _) => {
                assert_eq!(elems.len(), 2);
                assert!(matches!(&elems[0], Type::Multi(_, _)));
                assert!(matches!(&elems[1], Type::Generic(n, _) if n == "U"));
            }
            other => panic!("Expected Tuple, got {:?}", other),
        }
    }

    #[test]
    fn test_tuple_bracket_multichar_seq_var() {
        // Tuple[Ts...] — two-char sequence variable
        let typ = ltype("Tuple[Ts...]".into()).unwrap().1;
        match &typ {
            Type::Tuple(elems, _) => {
                assert_eq!(elems.len(), 1);
                assert!(
                    matches!(&elems[0], Type::Multi(inner, _) if matches!(inner.as_ref(), Type::Generic(n, _) if n == "Ts"))
                );
            }
            other => panic!("Expected Tuple, got {:?}", other),
        }
    }

    // ── Spec §2.2 : Record[...] bracket notation ─────────────────────────────

    #[test]
    fn test_record_bracket_concrete() {
        let typ = ltype("Record[x: int, y: char]".into()).unwrap().1;
        assert!(matches!(typ, Type::Record(ref f, _) if f.len() == 2));
    }

    #[test]
    fn test_record_bracket_empty() {
        let typ = ltype("Record[]".into()).unwrap().1;
        assert!(matches!(typ, Type::Record(ref f, _) if f.is_empty()));
    }

    #[test]
    fn test_record_bracket_variadic_seq() {
        // Record[Fs...] — variadic field-sequence variable
        let typ = ltype("Record[Fs...]".into()).unwrap().1;
        match &typ {
            Type::Record(fields, _) => {
                assert_eq!(fields.len(), 1);
                let field = fields.iter().next().unwrap();
                assert!(field.is_variadic());
            }
            other => panic!("Expected Record, got {:?}", other),
        }
    }

    #[test]
    fn test_record_bracket_seq_then_concrete() {
        // Record[Fs..., z: bool]
        let typ = ltype("Record[Fs..., z: bool]".into()).unwrap().1;
        match &typ {
            Type::Record(fields, _) => assert_eq!(fields.len(), 2),
            other => panic!("Expected Record, got {:?}", other),
        }
    }

    // ── Variadic seq-vars inside brace-delimited forms (the reported bug) ────

    #[test]
    fn test_explicit_tuple_with_seq_var() {
        // tuple{T...} must parse — this was the reported bug
        let typ = ltype("tuple{T...}".into()).unwrap().1;
        match &typ {
            Type::Tuple(elems, _) => {
                assert_eq!(elems.len(), 1);
                assert!(matches!(&elems[0], Type::Multi(_, _)));
            }
            other => panic!("Expected Tuple, got {:?}", other),
        }
    }

    #[test]
    fn test_explicit_tuple_seq_var_then_concrete() {
        // tuple{T..., U}
        let typ = ltype("tuple{T..., U}".into()).unwrap().1;
        match &typ {
            Type::Tuple(elems, _) => {
                assert_eq!(elems.len(), 2);
                assert!(matches!(&elems[0], Type::Multi(_, _)));
                assert!(matches!(&elems[1], Type::Generic(n, _) if n == "U"));
            }
            other => panic!("Expected Tuple, got {:?}", other),
        }
    }

    #[test]
    fn test_explicit_record_with_field_seq_var() {
        // record{Fs...} — variadic field sequence inside brace form
        let typ = ltype("record{Fs...}".into()).unwrap().1;
        match &typ {
            Type::Record(fields, _) => {
                assert_eq!(fields.len(), 1);
                let f = fields.iter().next().unwrap();
                assert!(f.is_variadic());
            }
            other => panic!("Expected Record, got {:?}", other),
        }
    }

    #[test]
    fn test_function_type_with_variadic_tuple_params() {
        // @append: (tuple{T...}, U) -> tuple{T..., U}
        let typ = ltype("(tuple{T...}, U) -> tuple{T..., U}".into())
            .unwrap()
            .1;
        assert!(
            matches!(&typ, Type::Function(params, ret, _)
                if params.len() == 2
                && matches!(params[0].get_type(), Type::Tuple(ref v, _) if v.len() == 1)
                && matches!(ret.as_ref(), Type::Tuple(ref v, _) if v.len() == 2)
            ),
            "Unexpected function type: {:?}",
            typ
        );
    }

    // ── Spec §3.2 : subtyping rules ──────────────────────────────────────────

    #[test]
    fn test_empty_tuple_subtype_of_empty_record() {
        // tuple{} <: list{} (where list{} == record{} == Type::Record({}))
        let ctx = Context::default();
        let empty_tuple = Type::Tuple(vec![], HelpData::default());
        let empty_record = Type::Record(std::collections::HashSet::new(), HelpData::default());
        assert!(
            empty_tuple.is_subtype(&empty_record, &ctx).0,
            "empty tuple should be a subtype of empty record"
        );
    }

    #[test]
    fn test_empty_record_subtype_of_empty_tuple() {
        // record{} <: list{} (where list{} == tuple{} == Type::Tuple([]))
        let ctx = Context::default();
        let empty_tuple = Type::Tuple(vec![], HelpData::default());
        let empty_record = Type::Record(std::collections::HashSet::new(), HelpData::default());
        assert!(
            empty_record.is_subtype(&empty_tuple, &ctx).0,
            "empty record should be a subtype of empty tuple"
        );
    }
}
