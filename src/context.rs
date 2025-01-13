use std::collections::HashMap;
use crate::types::Type;

#[derive(Clone)]
pub struct Context {
    pub kinds: Vec<String>,  // Representing Prolog's Kinds
    pub types: HashMap<String, Type>  // Representing Prolog's Types
}

impl Default for Context {
    fn default() -> Self {
        Context {
            kinds: vec![],
            types: HashMap::default(),
        }
    }
}
