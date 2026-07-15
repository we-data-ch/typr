use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::help_message::SingleBuilder;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use miette::Result;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SyntaxError {
    FunctionWithoutType(HelpData),
    FunctionWithoutReturnType(HelpData),
    ForgottenSemicolon(HelpData),
    MissingListPrefix(HelpData),
    EmptyFunctionBody(HelpData),
    FunctionTypeSyntax(HelpData),
    RecordConstructorIndex(HelpData),
    RecordInRecursiveParams(HelpData),
    UnknownElement {
        element: String,
        line: u32,
        help_data: HelpData,
    },
    LetInsteadOfType {
        name: String,
        help_data: HelpData,
    },
    TypeInsteadOfLet {
        name: String,
        help_data: HelpData,
    },
    MutationTargetNotAssignable(HelpData),
    WrongCommentSyntax(HelpData),
    WithNode(Box<Lang>, Box<SyntaxError>),
}

impl SyntaxError {
    /// Get the HelpData containing position information for this error.
    pub fn get_help_data(&self) -> Option<HelpData> {
        match self {
            SyntaxError::FunctionWithoutType(h) => Some(h.clone()),
            SyntaxError::FunctionWithoutReturnType(h) => Some(h.clone()),
            SyntaxError::ForgottenSemicolon(h) => Some(h.clone()),
            SyntaxError::MissingListPrefix(h) => Some(h.clone()),
            SyntaxError::EmptyFunctionBody(h) => Some(h.clone()),
            SyntaxError::FunctionTypeSyntax(h) => Some(h.clone()),
            SyntaxError::RecordConstructorIndex(h) => Some(h.clone()),
            SyntaxError::RecordInRecursiveParams(h) => Some(h.clone()),
            SyntaxError::UnknownElement { help_data, .. } => Some(help_data.clone()),
            SyntaxError::LetInsteadOfType { help_data, .. } => Some(help_data.clone()),
            SyntaxError::TypeInsteadOfLet { help_data, .. } => Some(help_data.clone()),
            SyntaxError::MutationTargetNotAssignable(h) => Some(h.clone()),
            SyntaxError::WrongCommentSyntax(h) => Some(h.clone()),
            SyntaxError::WithNode(_, inner) => inner.get_help_data(),
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
            SyntaxError::MissingListPrefix(_) => {
                "Missing list prefix ('list' or ':') before the braces".to_string()
            }
            SyntaxError::EmptyFunctionBody(_) => {
                "Empty function body is not allowed. Use `...` as a placeholder.".to_string()
            }
            SyntaxError::FunctionTypeSyntax(_) => {
                "Function types use parentheses without 'fn': use `(args) -> Type` instead of `fn(args) -> Type`".to_string()
            }
            SyntaxError::RecordConstructorIndex(_) => {
                "A record constructor takes exactly one integer length: `Name[N]{ ... }`".to_string()
            }
            SyntaxError::RecordInRecursiveParams(_) => {
                "Record blocks `{ ... }` are not allowed inside recursive type parameters".to_string()
            }
            SyntaxError::UnknownElement { element, line, help_data } => {
                format!(
                    "Unknown element `{}` in `{}` at `{}`",
                    element,
                    help_data.get_file_name(),
                    line
                )
            }
            SyntaxError::LetInsteadOfType { name, .. } => {
                format!(
                    "Use `type` instead of `let` to create a type alias: `type {name} <- ...`"
                )
            }
            SyntaxError::TypeInsteadOfLet { name, .. } => {
                format!(
                    "Use `let` instead of `type` to create a variable binding: `let {name} <- ...`"
                )
            }
            SyntaxError::MutationTargetNotAssignable(_) => {
                "Mutation target is not assignable: `!;` requires a variable or pipeline/UFC starting with a variable".to_string()
            }
            SyntaxError::WrongCommentSyntax(_) => {
                "TypR comments use `#`, not `//`".to_string()
            }
            SyntaxError::WithNode(_, inner) => inner.simple_message(),
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
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Function parameter is missing a type annotation")
                    .pos_text("Here")
                    .help("Add a type after the parameter name, e.g. 'fn(x: Integer): ...'")
                    .build()
            }
            SyntaxError::FunctionWithoutReturnType(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Hey You forgot to specify the function return type after the ':' : 'fn(...): Type'")
                    .pos_text("Here")
                    .help("Just add the type")
                    .build()
            }
            SyntaxError::ForgottenSemicolon(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 1))
                    .text("You forgot a semicolon at the end of your statement")
                    .pos_text("Here")
                    .help("Just add a ';'")
                    .build()
            }
            SyntaxError::MissingListPrefix(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("You forgot to add a list prefix ('list' or ':') before the braces")
                    .pos_text("Here")
                    .help("Add 'list' or ':' before the braces, e.g. 'list {1, 2, 3}' or ': {1, 2, 3}'")
                    .build()
            }
            SyntaxError::EmptyFunctionBody(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Empty function body is not allowed")
                    .pos_text("Empty body here")
                    .help("Use `...` as a placeholder: `fn(): Type { ... }`")
                    .build()
            }
            SyntaxError::FunctionTypeSyntax(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Function types use parentheses without 'fn'")
                    .pos_text("Unexpected 'fn' here")
                    .help("Use `(args) -> Type` instead of `fn(args) -> Type`")
                    .build()
            }
            SyntaxError::RecordConstructorIndex(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("A record constructor takes exactly one integer length")
                    .pos_text("Here")
                    .help("Use `Name[N]{ field: Type, ... }` with a single integer N")
                    .build()
            }
            SyntaxError::RecordInRecursiveParams(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text(
                        "Record blocks `{ ... }` are not allowed inside recursive type parameters",
                    )
                    .pos_text("Here")
                    .help("Recursive types only take types or integers, e.g. `Matrix[3, 4, num]`")
                    .build()
            }
            SyntaxError::UnknownElement {
                element,
                line,
                help_data,
            } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name.clone(), text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), element.len()))
                    .text(format!(
                        "Unknown element `{}` in `{}` at `{}`",
                        element, file_name, line
                    ))
                    .pos_text("Here")
                    .build()
            }
            SyntaxError::LetInsteadOfType { name, help_data } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Use `type` instead of `let` to create a type alias")
                    .pos_text("`let` used here")
                    .help(format!("Did you mean `type {name}`?"))
                    .build()
            }
            SyntaxError::TypeInsteadOfLet { name, help_data } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Use `let` instead of `type` to create a variable binding")
                    .pos_text("`type` used here")
                    .help(format!("Did you mean `let {name}`?"))
                    .build()
            }
            SyntaxError::MutationTargetNotAssignable(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Mutation target is not assignable")
                    .pos_text("Here")
                    .help("`!;` can only be applied to a variable, pipeline (`x |> f()!;`), or UFC (`obj.method()!;`) whose head is a variable")
                    .build()
            }
            SyntaxError::WrongCommentSyntax(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 2))
                    .text("TypR comments use `#`, not `//`")
                    .pos_text("Treated as a comment here")
                    .help("Replace `//` with `#` — the rest of the line was ignored as a comment")
                    .build()
            }
            SyntaxError::WithNode(_, inner) => return inner.display(),
        };
        msg.map_or_else(|e| format!("{:?}", e), |_| String::new())
    }
}
