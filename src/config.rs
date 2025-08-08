use crate::Environment;

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
   pub immutability: bool
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

}


impl Default for Config {
    fn default() -> Config {
        Config {
            compile_mode: CompileMode::Body,
            environment: Environment::StandAlone,
            immutability: false
        }
    }
}
