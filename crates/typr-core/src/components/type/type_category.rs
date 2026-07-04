use crate::components::error_message::help_data::HelpData;
use crate::components::language::var::Var;
use crate::components::r#type::kind::Kind;
use crate::components::r#type::Type;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;

/// The kind a sigil-generic is bounded to. Mirrors `Kind` (the four sigil
/// kinds `%`/`@`/`^`/`?`) plus `Number` (the `#`/`IndexGen` sigil, which has
/// no `Kind` variant because Number stays IndexGen-based). Each `GKind` names
/// a `TypeCategory::GenericKinded(_)` that sits between `Generic` and the
/// concrete monomorphized types of that kind in the subtype lattice.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub enum GKind {
    Record,
    Interface,
    String,
    Boolean,
    Number,
}

impl GKind {
    /// Lift one of the four `Kind` sigils into its `GKind`. `Number` has no
    /// `Kind` counterpart and is produced directly from `Type::IndexGen`.
    pub fn from_kind(k: Kind) -> Self {
        match k {
            Kind::Record => GKind::Record,
            Kind::Interface => GKind::Interface,
            Kind::String => GKind::String,
            Kind::Boolean => GKind::Boolean,
        }
    }
}

impl fmt::Display for GKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            GKind::Record => "GRecord",
            GKind::Interface => "GInterface",
            GKind::String => "GString",
            GKind::Boolean => "GBoolean",
            GKind::Number => "GNumber",
        };
        write!(f, "{}", res)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub enum TypeCategory {
    Array,
    Function,
    Record,
    Tuple,
    Tag,
    Union,
    Interface,
    Boolean,
    Integer,
    Number,
    Char,
    Generic,
    GenericKinded(GKind),
    DataFrame,
    Alias,
    Any,
    Empty,
    RClass,
    RFunction,
    Opaque(String),
    Template,
    Vector,
    Sequence,
    Rest,
    Intersection,
    Module,
    Operator,
    Null,
}

impl TypeCategory {
    pub fn to_variable(self) -> Var {
        Var::from_name(&format!("{}", self)).set_type(Type::Params(vec![], HelpData::default()))
    }

    /// The immediate supercategory in the category lattice, or `None` for a
    /// top-level category. Every `GenericKinded(_)` sits directly below the
    /// bare `Generic` category — so `GRecord <: Generic`, `GNumber <: Generic`,
    /// etc. This is the category-level reflection of the `Type`-level fact that
    /// any `KindedGen`/`IndexGen` is a subtype of `Generic` in `is_subtype_raw`.
    pub fn supercategory(&self) -> Option<TypeCategory> {
        match self {
            TypeCategory::GenericKinded(_) => Some(TypeCategory::Generic),
            _ => None,
        }
    }

    /// Whether `self` is `other` or a (transitive) subcategory of it in the
    /// lattice above.
    pub fn is_subcategory_of(&self, other: &TypeCategory) -> bool {
        let mut cur = Some(self.clone());
        while let Some(cat) = cur {
            if &cat == other {
                return true;
            }
            cur = cat.supercategory();
        }
        false
    }
}

impl fmt::Display for TypeCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            TypeCategory::Array => "Array",
            TypeCategory::Operator => "Operator",
            TypeCategory::Function => "Function",
            TypeCategory::Record => "Record",
            TypeCategory::Tag => "Tag",
            TypeCategory::Union => "Union",
            TypeCategory::Interface => "Interface",
            TypeCategory::Boolean => "logical",
            TypeCategory::Integer => "integer",
            TypeCategory::Number => "numeric",
            TypeCategory::DataFrame => "data.frame",
            TypeCategory::Char => "character",
            TypeCategory::Generic => "Generic",
            TypeCategory::GenericKinded(gk) => return write!(f, "{}", gk),
            TypeCategory::Alias => "Alias",
            TypeCategory::Any => "Any",
            TypeCategory::Empty => "Empty",
            TypeCategory::RClass => "RClass",
            TypeCategory::RFunction => "RFunction",
            TypeCategory::Tuple => "Tuple",
            TypeCategory::Opaque(name) => &name.to_string(),
            TypeCategory::Template => "Template",
            TypeCategory::Vector => "Vector",
            TypeCategory::Sequence => "Sequence",
            TypeCategory::Rest => "Rest",
            TypeCategory::Intersection => "Intersection",
            TypeCategory::Module => "Module",
            TypeCategory::Null => "NULL",
        };
        write!(f, "{}", res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gkind_display() {
        assert_eq!(format!("{}", GKind::Record), "GRecord");
        assert_eq!(format!("{}", GKind::Interface), "GInterface");
        assert_eq!(format!("{}", GKind::String), "GString");
        assert_eq!(format!("{}", GKind::Boolean), "GBoolean");
        assert_eq!(format!("{}", GKind::Number), "GNumber");
    }

    #[test]
    fn test_generic_kinded_category_display() {
        assert_eq!(
            format!("{}", TypeCategory::GenericKinded(GKind::Record)),
            "GRecord"
        );
    }

    #[test]
    fn test_gkind_from_kind() {
        assert_eq!(GKind::from_kind(Kind::Record), GKind::Record);
        assert_eq!(GKind::from_kind(Kind::Interface), GKind::Interface);
        assert_eq!(GKind::from_kind(Kind::String), GKind::String);
        assert_eq!(GKind::from_kind(Kind::Boolean), GKind::Boolean);
    }

    #[test]
    fn test_kinded_supercategory_is_generic() {
        for gk in [
            GKind::Record,
            GKind::Interface,
            GKind::String,
            GKind::Boolean,
            GKind::Number,
        ] {
            assert_eq!(
                TypeCategory::GenericKinded(gk).supercategory(),
                Some(TypeCategory::Generic)
            );
        }
    }

    #[test]
    fn test_generic_has_no_supercategory() {
        assert_eq!(TypeCategory::Generic.supercategory(), None);
    }

    #[test]
    fn test_kinded_is_subcategory_of_generic() {
        let grecord = TypeCategory::GenericKinded(GKind::Record);
        assert!(grecord.is_subcategory_of(&TypeCategory::Generic));
        assert!(grecord.is_subcategory_of(&grecord));
        // but Generic is not a subcategory of GRecord (other direction)
        assert!(!TypeCategory::Generic.is_subcategory_of(&grecord));
        // and unrelated categories don't compare
        assert!(!grecord.is_subcategory_of(&TypeCategory::GenericKinded(GKind::Number)));
    }
}
