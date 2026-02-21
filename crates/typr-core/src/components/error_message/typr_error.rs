use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::syntax_error::SyntaxError;
use crate::components::error_message::type_error::TypeError;
use std::fmt;

/// Enum unifie pour toutes les erreurs TypR
/// Permet de collecter les erreurs de type et de syntaxe ensemble
#[derive(Debug, Clone)]
pub enum TypRError {
    Type(TypeError),
    Syntax(SyntaxError),
}

impl TypRError {
    pub fn type_error(err: TypeError) -> Self {
        TypRError::Type(err)
    }

    pub fn syntax_error(err: SyntaxError) -> Self {
        TypRError::Syntax(err)
    }

    pub fn display(self) -> String {
        match self {
            TypRError::Type(te) => te.display(),
            TypRError::Syntax(se) => se.display(),
        }
    }

    /// Get the HelpData containing position information for this error.
    /// Returns the primary error location (first position in case of double-location errors).
    pub fn get_help_data(&self) -> Option<crate::components::error_message::help_data::HelpData> {
        match self {
            TypRError::Type(te) => te.get_help_data(),
            TypRError::Syntax(se) => se.get_help_data(),
        }
    }

    /// Get a simple error message without file access (for LSP use).
    pub fn simple_message(&self) -> String {
        match self {
            TypRError::Type(te) => te.simple_message(),
            TypRError::Syntax(se) => se.simple_message(),
        }
    }
}

impl ErrorMsg for TypRError {
    fn display(self) -> String {
        match self {
            TypRError::Type(err) => err.display(),
            TypRError::Syntax(err) => err.display(),
        }
    }
}

impl fmt::Display for TypRError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypRError::Type(err) => write!(f, "{}", err.clone().display()),
            TypRError::Syntax(err) => write!(f, "{}", err.clone().display()),
        }
    }
}

impl From<TypeError> for TypRError {
    fn from(err: TypeError) -> Self {
        TypRError::Type(err)
    }
}

impl From<SyntaxError> for TypRError {
    fn from(err: SyntaxError) -> Self {
        TypRError::Syntax(err)
    }
}

/// Structure pour collecter plusieurs erreurs
#[derive(Debug, Clone, Default)]
pub struct ErrorCollector {
    errors: Vec<TypRError>,
}

impl ErrorCollector {
    pub fn new() -> Self {
        ErrorCollector { errors: Vec::new() }
    }

    pub fn push(&mut self, error: TypRError) {
        self.errors.push(error);
    }

    pub fn push_type_error(&mut self, error: TypeError) {
        self.errors.push(TypRError::Type(error));
    }

    pub fn push_syntax_error(&mut self, error: SyntaxError) {
        self.errors.push(TypRError::Syntax(error));
    }

    pub fn extend(&mut self, other: ErrorCollector) {
        self.errors.extend(other.errors);
    }

    pub fn extend_vec(&mut self, errors: Vec<TypRError>) {
        self.errors.extend(errors);
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn get_errors(&self) -> &Vec<TypRError> {
        &self.errors
    }

    pub fn into_errors(self) -> Vec<TypRError> {
        self.errors
    }

    /// Affiche toutes les erreurs collectees
    pub fn display_all(&self) -> String {
        self.errors
            .iter()
            .enumerate()
            .map(|(i, err)| format!("Error {}: {}", i + 1, err))
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

impl From<Vec<TypRError>> for ErrorCollector {
    fn from(errors: Vec<TypRError>) -> Self {
        ErrorCollector { errors }
    }
}

impl From<TypRError> for ErrorCollector {
    fn from(error: TypRError) -> Self {
        ErrorCollector {
            errors: vec![error],
        }
    }
}
