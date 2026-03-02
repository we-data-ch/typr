#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::MsgTemplate;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use miette::SourceCode;
use miette::{Diagnostic, NamedSource, Result, SourceSpan};
use std::fs;
use thiserror::Error;

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

impl From<(String, String)> for SingleBuilder<String> {
    fn from((file_name, text): (String, String)) -> Self {
        Self::new(file_name, text)
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
