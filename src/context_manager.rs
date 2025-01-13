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
use crate::var::Var;
use crate::argument_type::ArgumentType;
use crate::var::Permission;

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
            Data::Value(val) if val == "empty" => "".to_string(),
            Data::Value(val) => val.to_string(),
            Data::Function(name, rest) if name == "var" => {
                rest[0].to_string()
            },
            _ => "ERR".to_string()
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

    fn get_array(&self) -> Option<Vec<Data>> {
        match self {
            Data::Array(v) => Some(v.clone()),
            _ => None
        }
    }

    fn get_value(&self) -> Option<String> {
        match self {
            Data::Value(val) => Some(val.to_string()),
            _ => None
        }
    }

    fn to_type(&self) -> Type {
        match self {
            Data::Value(val) => Data::string_to_type(val),
            Data::Array(v) => Type::Params(v.iter().map(|val| val.to_type()).collect::<Vec<_>>()),
            Data::Function(name, rest) if name == "tfn" => {
                    let v = rest[1].get_array().unwrap();
                    let vecs = v.iter().map(|val| val.to_type()).collect::<Vec<_>>();
                    Type::Function(vecs, Box::new(rest[2].to_type()))
            },
            Data::Function(name, rest) if name == "tarray" => {
                    Type::Array(Box::new(rest[0].to_type()), Box::new(rest[1].to_type()))
            },
            Data::Function(name, rest) if name == "ind" => {
                match rest[0].get_value().unwrap().parse::<u32>().ok() {
                   Some(num) => Type::Index(num),
                   _ => Type::IndexGen(rest[0].to_string())
                }
            },
            Data::Function(name, rest) if name == "gen" => {
                let val = rest[0].get_value().unwrap();
                match Data::string_to_type(&val) {
                    Type::Empty => Type::Generic(val),
                    typ => typ
                }
            },
            Data::Function(name, rest) if name == "add" => {
                let val1 = rest[0].to_type();
                let val2 = rest[1].to_type();
                Type::Add(Box::new(val1), Box::new(val2))
            },
            Data::Function(name, rest) if name == "mul" => {
                let val1 = rest[0].to_type();
                let val2 = rest[1].to_type();
                Type::Mul(Box::new(val1), Box::new(val2))
            },
            Data::Function(name, rest) if name == "division" => {
                let val1 = rest[0].to_type();
                let val2 = rest[1].to_type();
                Type::Div(Box::new(val1), Box::new(val2))
            },
            Data::Function(name, rest) if name == "minus" => {
                let val1 = rest[0].to_type();
                let val2 = rest[1].to_type();
                Type::Minus(Box::new(val1), Box::new(val2))
            },
            Data::Function(name, rest) if name == "ttag" => {
                let val1 = rest[0].to_string();
                let val2 = rest[1].to_type();
                Type::Tag(val1, Box::new(val2))
            },
            Data::Function(name, rest) if name == "trecord" => {
                let v = rest[0].get_array().expect("Can't open array in trecord");
                let new_v = v.iter()
                 .map(|key_value| {
                     let val = key_value.get_array().unwrap();
                     ArgumentType(val[0].to_string(), val[1].to_type(), false)
                 }).collect::<Vec<_>>();
                Type::Record(new_v)
            },
            Data::Function(name, rest) if name == "interface" => {
                let v = rest[0].get_array().expect("Can't open array in interface");
                let new_v = v.iter()
                 .map(|key_value| {
                     let val = key_value.get_array().expect("Can't open body (array) in key value interface");
                     ArgumentType(val[0].to_string(), val[1].to_type(), false)
                 }).collect::<Vec<_>>();
                Type::Record(new_v)
            },
            Data::Function(name, rest) if name == "union" => {
                let v = rest[0].get_array().expect("can't extract body (array) from union");
                let new_v = v.iter().map(|ttag| ttag.to_type()).collect::<Vec<_>>();
                Type::Union(new_v)
            },
            Data::Function(name, rest) if name == "var" => {
                let name = rest[0].to_string();
                let path = rest[1].to_string();
                if let Type::Params(params) = rest[4].to_type(){
                    Type::Alias(name, params, path)
                } else { Type::Empty }
            },
            Data::Function(name, rest) if name == "params" => {
                let v = rest[0].get_array().expect("There is a problem with the type parameters");
                let new_v = v.iter().map(|param| param.to_type()).collect::<Vec<_>>();
                Type::Params(new_v)
            },
            Data::Function(name, rest) if name == "talias" => {
                rest[2].to_type()
            },
            _ => Type::Failed(self.to_string())
        }
    }

    fn to_lang(&self) -> Lang {
        match self {
            Data::Function(name, rest) if name == "var" => {
                    let var = Lang::Variable(
                        rest[0].to_string(),
                        rest[1].to_string(),
                        rest[2].to_permission(),
                        rest[3].to_bool(),
                        rest[4].to_type());
                    let var2: Var = Var::from_language(var.clone()).unwrap();
                if let Type::Params(vec) = var2.clone().4 {
                    Lang::Alias(var2, vec.clone(), Type::Empty)
                } else {
                    var.clone()
                }
            }
            _ => Lang::Empty
        }
    }

    fn to_bool(&self) -> bool {
        if self.to_string() == "true"{
            true
        } else { false }
    }

    fn to_permission(&self) -> Permission {
        if self.to_string() == "public"{
            Permission::Public
        } else { Permission::Private }
        
    }

    fn to_couple(&self) -> Option<(Lang, Type)>{
        match self {
            Data::Array(v) if v.len() == 2 
                => {
                  let typ = v[1].to_type();
                  Some((v[0].to_lang(), typ))
                }
            _ => None 
        }
    }

    //fn to_couple(&self) -> Option<(String, Data)>{
        //match self {
            //Data::Array(v)
                //if v.len() == 2 => Some((v[0].to_name(), v[1].clone())),
            //_ => None 
        //}
    //}

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
    }; res
}
