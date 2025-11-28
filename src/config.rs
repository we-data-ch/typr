use crate::Environment;
use crate::Context;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TargetLanguage {
    R,
    JS
}

impl Default for TargetLanguage {
    fn default() -> Self {
        TargetLanguage::R
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FileType {
    Main,
    Module
}


#[derive(Debug, Clone, PartialEq)]
pub struct Config {
   pub environment: Environment,
   pub file_type: FileType,
   pub target_language: TargetLanguage,
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
        }
    }
}

