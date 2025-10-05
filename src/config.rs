use crate::Environment;

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

#[derive(Debug, Clone, PartialEq)]
pub enum CompileMode {
    Header,
    Body,
    Module
}


#[derive(Debug, Clone, PartialEq)]
pub struct Config {
   pub compile_mode: CompileMode,
   pub environment: Environment,
   pub immutability: bool,
   target_language: TargetLanguage,
}

impl Config {
    pub fn set_environment(&self, e: Environment) -> Config {
        Config {
            environment: e,
            ..self.clone()
        }
    }


    pub fn set_compile_mode(self, cm: CompileMode) -> Self {
        Self {
            compile_mode: cm,
            ..self
        }
    }

    pub fn set_target_language(self, language: TargetLanguage) -> Self {
        Self {
            target_language: language,
            ..self
        }
    }

}


impl Default for Config {
    fn default() -> Config {
        Config {
            target_language: TargetLanguage::default(),
            compile_mode: CompileMode::Body,
            environment: Environment::StandAlone,
            immutability: false
        }
    }
}
