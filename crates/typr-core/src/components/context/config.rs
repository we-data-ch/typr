use crate::components::context::Context;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Environment {
    StandAlone,
    Project,
    Repl,
    /// WebAssembly environment - all code is inlined, no file I/O
    Wasm,
}

impl Environment {
    pub fn is_project(&self) -> bool {
        matches!(self, Environment::Project)
    }

    pub fn to_base_path(self) -> String {
        self.to_string()
    }

    /// Check if this environment supports file I/O
    pub fn supports_file_io(self) -> bool {
        match self {
            Environment::StandAlone | Environment::Project | Environment::Repl => true,
            Environment::Wasm => false,
        }
    }

    /// Check if external files should be inlined
    pub fn should_inline_files(self) -> bool {
        matches!(self, Environment::Wasm)
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            Environment::Project => "R/",
            Environment::StandAlone | Environment::Repl | Environment::Wasm => "",
        };
        write!(f, "{}", res)
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize, Default)]
pub enum TargetLanguage {
    R,
    #[default]
    JS,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize, Default)]
pub enum FileType {
    Main,
    #[default]
    Module,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Config {
    pub environment: Environment,
    pub file_type: FileType,
    pub target_language: TargetLanguage,
    /// When true, opaque alias declarations are registered as transparent.
    /// Set to true when type-checking a module body so internal functions
    /// can see through opaque types they declare.
    pub in_module_body: bool,
    /// When true, this is a test build (`--test`): `@testable` private members
    /// are additionally exposed as `M$.test_<name>` (see RFC-TR-031).
    #[serde(default)]
    pub test_mode: bool,
}

//main
impl Config {
    pub fn set_environment(&self, e: Environment) -> Config {
        Config {
            environment: e,
            ..self.clone()
        }
    }

    pub fn set_as_module(self) -> Self {
        Self {
            file_type: FileType::Module,
            ..self
        }
    }

    pub fn set_target_language(self, language: TargetLanguage) -> Self {
        Self {
            target_language: language,
            ..self
        }
    }

    pub fn set_in_module_body(self, val: bool) -> Self {
        Self {
            in_module_body: val,
            ..self
        }
    }

    pub fn set_test_mode(self, val: bool) -> Self {
        Self {
            test_mode: val,
            ..self
        }
    }

    pub fn get_target_language(&self) -> TargetLanguage {
        self.target_language
    }

    pub fn to_context(self) -> Context {
        Context::default().set_config(self)
    }
}

impl Default for Config {
    fn default() -> Config {
        Config {
            target_language: TargetLanguage::R,
            environment: Environment::StandAlone,
            file_type: FileType::Main,
            in_module_body: false,
            test_mode: false,
        }
    }
}
