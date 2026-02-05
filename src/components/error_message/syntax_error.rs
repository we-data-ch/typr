use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::help_message::SingleBuilder;
use crate::components::r#type::Type;
use miette::Result;
use std::fs;

pub enum SyntaxError {
    FunctionWithoutType(HelpData),
    FunctionWithoutReturnType(HelpData),
    ForgottenSemicolon(HelpData),
}

impl ErrorMsg for SyntaxError {
    fn display(self) -> String {
        let msg: Result<()> = match self {
            SyntaxError::FunctionWithoutType(help_data) => {
                let (file_name, text) = help_data
                    .get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .build()
            }
            SyntaxError::FunctionWithoutReturnType(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap();
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text("Hey You forgot to specify the function return type after the ':' : 'fn(...): Type'")
                    .pos_text("Here")
                    .help("Just add the type")
                    .build()
            }
            SyntaxError::ForgottenSemicolon(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap();
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 1))
                    .text("You forgot a semicolon at the end of your statement")
                    .pos_text("Here")
                    .help("Just add a ';'")
                    .build()
            }
        };
        match msg {
            Err(val) => format!("Syntax error:\n{:?}", val),
            _ => todo!(),
        }
    }
}
