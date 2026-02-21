use serde::Serialize;
use std::ops::Add;

// names separated by a "/"
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Default, Hash)]
pub struct Path(String);

impl Path {
    pub fn new(s: &str) -> Path {
        Path(s.to_string())
    }

    pub fn to_r(self) -> String {
        match &self.0[..] {
            "" => "".to_string(),
            _ => self.0.replace("::", "$") + "$",
        }
    }

    pub fn add_path(self, p: Path) -> Path {
        Path(self.0 + "::" + &p.0)
    }

    pub fn is_empty(&self) -> bool {
        self.0 == ""
    }

    pub fn get_value(&self) -> String {
        self.0.clone()
    }
}

impl Add<Path> for Path {
    type Output = Path;

    fn add(self, rhs: Path) -> Path {
        Path(self.0 + "::" + &rhs.0)
    }
}

use std::fmt;
impl fmt::Display for Path {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == "" {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{}::", self.0)
        }
    }
}

impl From<String> for Path {
    fn from(val: String) -> Self {
        Path(val)
    }
}

impl From<&str> for Path {
    fn from(val: &str) -> Self {
        Path(val.to_string())
    }
}

impl From<Path> for String {
    fn from(val: Path) -> Self {
        val.0
    }
}
