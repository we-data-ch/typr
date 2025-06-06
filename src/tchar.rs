use nom_locate::LocatedSpan;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Hash)]
pub enum Tchar {
    Val(String),
    Unknown
}

use std::fmt;
impl fmt::Display for Tchar {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)       
    }
}

impl From<String> for  Tchar {
   fn from(val: String) -> Self {
       Tchar::Val(val.clone()) 
   } 
}

impl From<LocatedSpan<&str, String>> for Tchar {
   fn from(val: LocatedSpan<&str, String>) -> Self {
        let res: String = (*val).into();
        Tchar::Val(res)
   } 
}
