use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::help_message::SingleBuilder;
use crate::components::r#type::Type;
use miette::Result;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SyntaxError {
    FunctionWithoutType(HelpData),
    FunctionWithoutReturnType(HelpData),
    ForgottenSemicolon(HelpData),
}

impl SyntaxError {
    /// Get the HelpData containing position information for this error.
    pub fn get_help_data(&self) -> Option<HelpData> {
        match self {
            SyntaxError::FunctionWithoutType(h) => Some(h.clone()),
            SyntaxError::FunctionWithoutReturnType(h) => Some(h.clone()),
            SyntaxError::ForgottenSemicolon(h) => Some(h.clone()),
        }
    }

    /// Get a simple error message without file access (for LSP use).
    pub fn simple_message(&self) -> String {
        match self {
            SyntaxError::FunctionWithoutType(_) => {
                "Function parameter is missing a type annotation".to_string()
            }
            SyntaxError::FunctionWithoutReturnType(_) => {
                "Function is missing a return type annotation after ':'".to_string()
            }
            SyntaxError::ForgottenSemicolon(_) => {
                "Missing semicolon at the end of the statement".to_string()
            }
        }
    }
}

/// Default file data when source is not available
fn default_file_data() -> (String, String) {
    ("std.ty".to_string(), String::new())
}

impl ErrorMsg for SyntaxError {
    fn display(self) -> String {
        let msg: Result<()> = match self {
            SyntaxError::FunctionWithoutType(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .build()
            }
            SyntaxError::FunctionWithoutReturnType(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text("Hey You forgot to specify the function return type after the ':' : 'fn(...): Type'")
                    .pos_text("Here")
                    .help("Just add the type")
                    .build()
            }
            SyntaxError::ForgottenSemicolon(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
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
