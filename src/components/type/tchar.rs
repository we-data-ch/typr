use nom_locate::LocatedSpan;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Tchar {
    Val(String),
    Unknown,
}

impl Tchar {
    pub fn gen_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Tchar::Unknown, _) => true,
            _ => false,
        }
    }
    pub fn get_val(&self) -> String {
        match self {
            Tchar::Val(s) => s.clone(),
            _ => "Unkown".to_string(),
        }
    }
}

use std::fmt;
impl fmt::Display for Tchar {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<String> for Tchar {
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
