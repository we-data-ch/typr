#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use std::collections::HashSet;

/// A linearised (flat) representation of an intersection type.
///
/// Internally an intersection is stored as a nested binary tree of
/// `Type::Operator(TypeOperator::Intersection, left, right, h)`.
/// `IntersectionType` flattens that tree into a `HashSet<Type>` so that
/// set-level operations (equality, …) can be performed without walking
/// the tree every time. Mirrors `UnionType`.
#[derive(Debug, Clone)]
pub struct IntersectionType {
    /// The flat set of member types that make up the intersection.
    types: HashSet<Type>,
    /// Help / location data inherited from the root `Operator` node.
    help_data: HelpData,
}

impl IntersectionType {
    /// Return the set of member types.
    pub fn get_types(&self) -> &HashSet<Type> {
        &self.types
    }

    /// Return the associated help data.
    pub fn get_help_data(&self) -> &HelpData {
        &self.help_data
    }

    /// Recursively walk a `Type` tree and collect every leaf that is
    /// *not* an `Intersection` operator into the provided set.
    fn flatten(typ: &Type, acc: &mut HashSet<Type>) {
        match typ {
            Type::Operator(TypeOperator::Intersection, t1, t2, _) => {
                Self::flatten(t1, acc);
                Self::flatten(t2, acc);
            }
            other => {
                acc.insert(other.clone());
            }
        }
    }

    /// Reconstruct a nested `Type::Operator(TypeOperator::Intersection, …)`
    /// tree from the flat set of member types.
    ///
    /// The resulting tree is a left-associative fold, consistent with
    /// `builder::intersection_type`. If the set is empty the result is
    /// `Type::Empty`.
    pub fn to_type(&self) -> Type {
        self.types
            .iter()
            .cloned()
            .reduce(|acc, t| {
                Type::Operator(
                    TypeOperator::Intersection,
                    Box::new(acc),
                    Box::new(t),
                    self.help_data.clone(),
                )
            })
            .unwrap_or(Type::Empty(self.help_data.clone()))
    }
}

impl TryFrom<Type> for IntersectionType {
    type Error = String;

    /// Convert a `Type` into an `IntersectionType`.
    ///
    /// Succeeds only when the outermost variant is
    /// `Type::Operator(TypeOperator::Intersection, …)`. The binary tree is
    /// recursively flattened so that nested intersections like
    /// `(A & B) & (C & D)` become the flat set `{A, B, C, D}`.
    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match &value {
            Type::Operator(TypeOperator::Intersection, _, _, _) => {
                let mut types = HashSet::new();
                Self::flatten(&value, &mut types);
                let help_data = value.get_help_data();
                Ok(IntersectionType { types, help_data })
            }
            _ => Err(format!(
                "{} is not an Intersection type and cannot be converted to IntersectionType",
                value
            )),
        }
    }
}

// ---------------------------------------------------------------------------
// PartialEq  –  two intersections are equal when they contain the same member set
// ---------------------------------------------------------------------------

impl PartialEq for IntersectionType {
    fn eq(&self, other: &Self) -> bool {
        self.types == other.types
    }
}

impl Eq for IntersectionType {}
