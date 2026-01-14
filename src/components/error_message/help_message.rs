#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use miette::{Diagnostic, NamedSource, SourceSpan, Result};
use thiserror::Error;
use miette::SourceCode;
use crate::components::r#type::r#type::Type;
use crate::components::lang::var::Var;
use crate::components::lang::language::Lang;
use std::fs;
use crate::components::error_message::help_data::HelpData;

pub trait ErrorMsg { 
    fn display(self) -> String;
}

pub trait PrintAndDefault<T: Default> {
    fn error_message(self, msg: impl ErrorMsg) -> T;
}

impl<T: Default> PrintAndDefault<T> for Option<T> {
    fn error_message(self, msg: impl ErrorMsg) -> T {
        println!("{}", msg.display());
        self.unwrap_or_default()
    }
}

impl<T: Default, E> PrintAndDefault<T> for Result<T, E> 
where
    E: std::fmt::Debug,
{
    fn error_message(self, msg: impl ErrorMsg) -> T {
        println!("{}", msg.display());
        self.unwrap_or_default()
    }
}


#[derive(Error, Debug, Diagnostic)]
pub enum MsgTemplate<S: SourceCode + 'static + std::fmt::Debug> {
    #[error("Type error: {text}")]
    Single {
        text: String, 
        #[label("{pos_text}")]
        pos: SourceSpan,
        pos_text: String,
        #[source_code]
        file: NamedSource<S>,
        #[help]
        help: Option<String>,
    },
    #[error("Type error: {text}")]
    Double {
        text: String, 

        #[label("{pos_text1}")]
        pos1: SourceSpan,
        pos_text1: String,
        #[source_code]
        file1: NamedSource<S>,

        #[label("{pos_text2}")]
        pos2: SourceSpan,
        pos_text2: String,
        #[source_code]
        file2: NamedSource<S>,

        #[help]
        help: Option<String>,
    }
}


// Builder pour Single
#[derive(Debug)]
pub struct SingleBuilder<S: SourceCode + 'static + std::fmt::Debug> {
    text: String,
    pos: SourceSpan,
    pos_text: String,
    file: NamedSource<S>,
    help: Option<String>,
}

impl<S: SourceCode + 'static + std::fmt::Debug> SingleBuilder<S> {
    pub fn new(file_name: String, text: S) -> Self {
        Self {
            text: "Default error message".to_string(),
            pos: (0_usize, 0_usize).into(),
            pos_text: "Error here".to_string(),
            file: NamedSource::new(file_name, text),
            help: None,
        }
    }

    pub fn text<T: Into<String>>(mut self, text: T) -> Self {
        self.text = text.into();
        self
    }

    pub fn pos_text<T: Into<String>>(mut self, text: T) -> Self {
        self.pos_text = text.into();
        self
    }

    pub fn pos(mut self, pos: (usize, usize)) -> Self {
        self.pos = pos.into();
        self
    }

    pub fn help<T: Into<String>>(mut self, help: T) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn build(self) -> Result<()> {
        let res = MsgTemplate::Single {
            text: self.text,
            pos: self.pos,
            pos_text: self.pos_text,
            file: self.file,
            help: self.help,
        };
        Err(res.into())
    }
}

// Builder pour Double
pub struct DoubleBuilder<S: SourceCode + 'static + std::fmt::Debug> {
    text: String,
    pos1: SourceSpan,
    pos_text1: String,
    pos2: SourceSpan,
    pos_text2: String,
    file1: NamedSource<S>,
    file2: NamedSource<S>,
    help: Option<String>,
}

impl<S: SourceCode + 'static + std::fmt::Debug + Clone> DoubleBuilder<S> {
    pub fn new(file_name: String, text: S, file_name2: String, text2: S) -> Self {
        Self {
            text: "Default error message".to_string(),
            pos1: (0_usize, 0_usize).into(),
            pos_text1: "First error".to_string(),
            pos2: (0_usize, 0_usize).into(),
            pos_text2: "Second error".to_string(),
            file1: NamedSource::new(file_name, text),
            file2: NamedSource::new(file_name2, text2),
            help: None,
        }
    }

    pub fn text<T: Into<String>>(mut self, text: T) -> Self {
        self.text = text.into();
        self
    }

    pub fn pos_text1<T: Into<String>>(mut self, pos_text1: T) -> Self {
        self.pos_text1 = pos_text1.into();
        self
    }

    pub fn pos1(mut self, pos1: (usize, usize)) -> Self {
        self.pos1 = pos1.into();
        self
    }

    pub fn pos_text2<T: Into<String>>(mut self, pos_text2: T) -> Self {
        self.pos_text2 = pos_text2.into();
        self
    }

    pub fn pos2(mut self, pos2: (usize, usize)) -> Self {
        self.pos2 = pos2.into();
        self
    }

    pub fn help<T: Into<String>>(mut self, help: T) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn build(self) -> Result<()> {
        let res = MsgTemplate::Double {
            text: self.text,
            pos1: self.pos1,
            pos_text1: self.pos_text1,
            file1: self.file1,
            pos2: self.pos2,
            pos_text2: self.pos_text2,
            file2: self.file2,
            help: self.help,
        };
        Err(res.into())
    }
}

pub enum TypeError {
    Let(Type, Type),
    Param(Type, Type),
    UndefinedFunction(Var),
    UndefinedVariable(Lang),
    UnmatchingReturnType(Type, Type),
    ImmutableVariable(Var, Var),
    PrivateVariable(Var, Var),
    GenericPatternMatch(Type, Type),
    FieldNotFound((String, HelpData), Type),
    WrongExpression(HelpData),
    WrongIndexing(Type, Type),
}

// main
impl ErrorMsg for TypeError {
    fn display(self) -> String {
        let msg: Result<()> = match self {
            TypeError::Let(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name, text) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), 
                                fs::read_to_string("std.ty").unwrap_or("".to_string())));
                DoubleBuilder::new(file_name.clone(), text.clone(), file_name, text)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("type {} doesn't match type {}", t1.pretty(), t2.pretty()))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Recieved {}", t2.pretty()))
                    .build()
            },
            TypeError::Param(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                let (file_name2, text2) = help_data2.get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("type {} doesn't match type {}", t1.pretty(), t2.pretty()))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Recieved {}", t2.pretty()))
                    .build()
            },
            TypeError::GenericPatternMatch(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                let (file_name2, text2) = help_data2.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("can't pattern match {{{} => {}}}", t1.pretty2(), t2.pretty2()))
                    .pos_text1(format!("{} should be a generic variable", t1.pretty2()))
                    .pos_text2(format!("Substitution type: {}", t2.pretty2()))
                    .build()
            },
            TypeError::UnmatchingReturnType(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                let (file_name2, text2) = help_data2.get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("The output type of the function don't match it's type annotation\nExpected: {:?}\nFound: {}", t1.pretty(), t2.pretty()))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Recieved {}", t2.pretty()))
                    .build()
            },
            TypeError::UndefinedFunction(fun)
                => {
                    let help_data = fun.get_help_data();
                    let (file_name, text) = help_data.get_file_data()
                        .unwrap_or(("std.ty".to_string(), "".to_string()));
                    let res = SingleBuilder::new(file_name, text)
                        .pos((help_data.get_offset(), 0))
                        .text("The function doesn't exist");

                    res.build()
                },
            TypeError::UndefinedVariable(var)
                => {
                    let help_data = var.get_help_data();
                    let var2 = Var::from_language(var).unwrap();
                    let (file_name, text) = help_data.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                    SingleBuilder::new(file_name, text)
                        .pos((help_data.get_offset(), 0))
                        .pos_text(format!("Undefined variable '{}'", var2.get_name()))
                        .text(format!("Undefined variable '{}'", var2.get_name()))
                        .help(format!("- Check the orthograph \n- if it's a function check if it's defined for the given type {}", var2.get_type()))
                        .build()
                }
            TypeError::ImmutableVariable(var_assign, var)
                => {
                let var = var.clone().set_type(var.get_type().generalize());
                let help_data1 = var_assign.get_help_data();
                let help_data2 = var.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .expect(&format!("The file name of {:?} for {} doesn't exist", 
                                     help_data1, var_assign));
                let (file_name2, text2) = help_data2.get_file_data()
                    .expect(&format!("The file name of {:?} for {} doesn't exist", 
                                     help_data2, var));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("The variable {} is immutable", var))
                    .pos_text1(format!("Forbidden assignation to {}", var))
                    .pos_text2(format!("{} defined with 'let' (=immutable) here", var))
                    .help("Try to replace the 'let' keyword by the 'mut' keyword")
                    .build()
                }
            TypeError::PrivateVariable(var_used, var)
                => {
                let var = var.clone().set_type(var.get_type().generalize());
                let help_data1 = var_used.get_help_data();
                let help_data2 = var.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .expect(&format!("The file name of {:?} for {} doesn't exist", 
                                     help_data1, var_used));
                let (file_name2, text2) = help_data2.get_file_data()
                    .expect(&format!("The file name of {:?} for {} doesn't exist", 
                                     help_data2, var));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("The variable {} is private", var))
                    .pos_text1(format!("Forbidden access to {} which is private", var))
                    .pos_text2(format!("{} defined as private (without 'pub') here", var))
                    .help("Try to add the 'pub' keyword befor the 'let' keyword")
                    .build()
                }
            TypeError::FieldNotFound((name, h), typ) => {
                let help_data1 = h;
                let help_data2 = typ.get_help_data();
                let (file_name, text) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), 
                                fs::read_to_string("std.ty").unwrap_or("".to_string())));
                DoubleBuilder::new(file_name.clone(), text.clone(), file_name, text)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("{} doesn't have a field '{}'", typ.pretty(), name))
                    .pos_text1(format!("Field '{}' doesn't exist", name))
                    .pos_text2(format!("Type {} defined here", typ.pretty()))
                    .build()
            },
            TypeError::WrongExpression(help_data) => {
                let (file_name, text) = help_data.get_file_data()
                    .unwrap_or(("std.ty".to_string(), 
                                fs::read_to_string("std.ty").unwrap_or("".to_string())));
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .build()
            }
            TypeError::WrongIndexing(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                let (file_name2, text2) = help_data2.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("Wrong indexing"))
                    .pos_text1(format!("{} Can't be indexed", t1.pretty2()))
                    .pos_text2(format!("Has a bigger dimension {}", t2.pretty2()))
                    .build()
            },
                
        };
        format!("{:?}", msg)
    }

}

pub enum SyntaxError {
    FunctionWithoutType(HelpData),
    FunctionWithoutReturnType(HelpData),
}

impl ErrorMsg for SyntaxError {
    fn display(self) -> String {
        let msg: Result<()> = match self {
            SyntaxError::FunctionWithoutType(help_data) => {
                    let (file_name, text) = help_data.get_file_data()
                        .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                    SingleBuilder::new(file_name, text)
                        .pos((help_data.get_offset(), 0))
                        .build()
            },
            SyntaxError::FunctionWithoutReturnType(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap();
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text("Hey You forgot to specify the function return type after the ':' : 'fn(...): Type'")
                    .pos_text("Here")
                    .help("Just add the type")
                    .build()
            }
        };
        match msg {
            Err(val) => format!("Syntax error:\n{:?}", val),
            _ => todo!()
        }
    }

}

