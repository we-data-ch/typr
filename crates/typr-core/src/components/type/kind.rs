use crate::components::r#type::Type;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Kind {
    Record,
    Interface,
    String,
    Boolean,
}

impl Kind {
    pub fn sigil(&self) -> char {
        match self {
            Kind::Record => '%',
            Kind::Interface => '@',
            Kind::String => '^',
            Kind::Boolean => '?',
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.sigil())
    }
}

/// Maps a concrete (already-reduced) `Type` to the `Kind` it satisfies, for
/// compatibility checks against a `Type::KindedGen` sigil. `None` means "no
/// constraint" (generics, Any, not-yet-reducible shapes) — permissive, same
/// philosophy as `type_arithmetic::accepts_number_kind`. Number is
/// deliberately not representable here: Number stays IndexGen-based.
pub fn type_kind(t: &Type) -> Option<Kind> {
    match t {
        Type::Record(_, _) => Some(Kind::Record),
        Type::Interface(_, _) => Some(Kind::Interface),
        Type::Char(_, _) => Some(Kind::String),
        Type::Boolean(_, _) => Some(Kind::Boolean),
        _ => None,
    }
}
