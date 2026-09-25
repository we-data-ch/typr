//! `BlockKey`: the stable semantic identity of a block (spec §6). Grown one segment at a time
//! while the builder walks down into `Lang`.

use serde::{Deserialize, Serialize};
use std::fmt;

/// `val:` / `type:` / `std:` / `r:` — TypR keeps values and aliases in separate namespaces.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Namespace {
    Val,
    Type,
    Std,
    R,
}

impl fmt::Display for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Namespace::Val => "val",
            Namespace::Type => "type",
            Namespace::Std => "std",
            Namespace::R => "r",
        };
        write!(f, "{s}")
    }
}

/// A block's stable identity, e.g. `val:norm2/a`, `val:norm2/#1/rhs/arg0`, `type:Point`.
/// Opaque on purpose (a plain `String` newtype): callers build one path segment at a time via
/// the methods below rather than constructing the string by hand.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct BlockKey(String);

impl BlockKey {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// A top-level declaration's own key (`val:norm2`, `type:Point`) — top-level names live
    /// directly under their namespace, not nested under the synthetic `Program` block.
    pub fn top_level(namespace: Namespace, name: &str) -> Self {
        BlockKey(format!("{namespace}:{name}"))
    }

    /// The synthetic root block representing the whole program. `@` can't start a TypR
    /// identifier, so this can never collide with a real top-level name.
    pub fn program_root() -> Self {
        BlockKey("val:@program".to_string())
    }

    fn extended(&self, segment: &str) -> Self {
        BlockKey(format!("{}/{}", self.0, segment))
    }

    /// A name introduced by a declaration (`let`, parameter, pattern binding).
    pub fn named(&self, name: &str) -> Self {
        self.extended(name)
    }

    /// A fixed structural role (`then`, `else`, `cond`, `lhs`, `rhs`, `arg0`, `value`…).
    pub fn role(&self, role: &str) -> Self {
        self.extended(role)
    }

    /// An anonymous child's position among its siblings.
    pub fn anonymous(&self, index: usize) -> Self {
        self.extended(&format!("#{index}"))
    }

    /// An overloaded name, disambiguated by the type of its first parameter
    /// (`show(Point)`), spec §6.
    pub fn overload(&self, name: &str, first_param_type: &str) -> Self {
        self.extended(&format!("{name}({first_param_type})"))
    }
}

impl fmt::Display for BlockKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
