use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0},
    multi::separated_list0,
    sequence::{delimited, preceded, tuple},
    IResult,
};

use crate::Lang;
use crate::Type;

pub type Context = Vec<(Lang, Type)>;

#[derive(Debug, PartialEq, Clone)]
enum Data {
    Function(String, Vec<Data>),
    Array(Vec<Data>),
    Value(String),
    Dot
}

use std::fmt;
impl fmt::Display for Data {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Data::Value(val) => val,
            _ => "ERR"
        };
        write!(f, "{}", res)
    }
}

impl Data {
    fn to_name(&self) -> String {
        match self {
            Data::Function(name, rest) 
                if name == "var" => rest[0].to_string(),
            _ => "@@@\n".to_string()
        }
    }

    fn string_to_type(val: &str) -> Type {
        match val {
            "int" => Type::Integer,
            "num" => Type::Number,
            "bool" => Type::Boolean,
            "chars" => Type::Char,
            _ => Type::Empty
        }
    }

    fn to_type(&self) -> Type {
        match self {
            Data::Value(val) => Data::string_to_type(val),
            Data::Array(v) => Type::Params(v.iter().map(|val| val.to_type()).collect::<Vec<_>>()),
            Data::Function(name, rest) 
                if name == "tfn" => {
                    if let Data::Array(v) = &rest[1] {
                        let vecs = v.iter().map(|val| val.to_type()).collect::<Vec<_>>();
                        Type::Function(vecs, Box::new(rest[2].to_type()))
                    } else {
                        Type::Empty
                    }
                }
            _ => Type::Empty
        }
    }

    //fn to_couple(&self) -> Option<(String, Type)>{
        //match self {
            //Data::Array(v)
                //if v.len() == 2 => Some((v[0].to_name(), v[1].to_type())),
            //_ => None 
        //}
    //}

    fn to_couple(&self) -> Option<(String, String)>{
        match self {
            Data::Array(v)
                if v.len() == 2 => Some((v[0].to_name(), v[1].to_string())),
            _ => None 
        }
    }

}

fn parse_value(input: &str) -> IResult<&str, Data> {
    let (input, value) = take_while1(|c: char| c.is_alphanumeric() || c == '_')(input)?;
    Ok((input, Data::Value(value.to_string())))
}

fn parse_array(input: &str) -> IResult<&str, Data> {
    let (input, elements) = delimited(
        char('['),
        separated_list0(preceded(multispace0, char(',')), parse_data),
        char(']'),
    )(input)?;
    Ok((input, Data::Array(elements)))
}

fn parse_function(input: &str) -> IResult<&str, Data> {
    let (input, (name, params)) = tuple((
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),
        delimited(
            char('('),
            separated_list0(preceded(multispace0, char(',')), parse_data),
            char(')'),
        ),
    ))(input)?;
    Ok((input, Data::Function(name.to_string(), params)))
}
fn parse_dot(s: &str) -> IResult<&str, Data> {
    let res = tag(".")(s);
    match res {
        Ok((s, dot)) => Ok((s, Data::Dot)),
        Err(r) => Err(r)
    }
}

fn parse_data(input: &str) -> IResult<&str, Data> {
    preceded(
        multispace0,
        alt((parse_function, parse_array, parse_value, parse_dot)),
    )(input)
}

pub fn parse_prolog(input: &str) -> Context {
    let data = parse_data(input).expect("The parsing of the context went wrong").1;
    let res = match data {
        Data::Array(v) => v.iter().flat_map(|name_type| Data::to_couple(name_type)).collect::<Vec<_>>(),
        _ => panic!("The context should be an array of mapping")
    };
    res.iter().for_each(|x| println!("x: {:?}", x));
    vec![]
}
