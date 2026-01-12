#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::help_data::HelpData;

#[derive(Debug)]
pub enum Generic {
    Normal(String, HelpData),
    Index(String, HelpData),
    Label(String, HelpData)
}
