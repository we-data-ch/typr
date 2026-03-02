#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use std::collections::HashSet;

/// A linearised (flat) representation of a union type.
///
/// Internally a union is stored as a nested binary tree of
/// `Type::Operator(TypeOperator::Union, left, right, h)`.
/// `UnionType` flattens that tree into a `HashSet<Type>` so that
/// set-level operations (equality, subtyping, …) can be performed
/// without walking the tree every time.
#[derive(Debug, Clone)]
pub struct UnionType {
    /// The flat set of member types that make up the union.
    types: HashSet<Type>,
    /// Help / location data inherited from the root `Operator` node.
    help_data: HelpData,
}

// ---------------------------------------------------------------------------
// Construction helpers
// ---------------------------------------------------------------------------

impl UnionType {
    /// Return the set of member types.
    pub fn get_types(&self) -> &HashSet<Type> {
        &self.types
    }

    /// Return the associated help data.
    pub fn get_help_data(&self) -> &HelpData {
        &self.help_data
    }

    /// Recursively walk a `Type` tree and collect every leaf that is
    /// *not* a `Union` operator into the provided set.
    fn flatten(typ: &Type, acc: &mut HashSet<Type>) {
        match typ {
            Type::Operator(TypeOperator::Union, t1, t2, _) => {
                Self::flatten(t1, acc);
                Self::flatten(t2, acc);
            }
            other => {
                acc.insert(other.clone());
            }
        }
    }

    /// Check whether every member type of `other` has at least one
    /// corresponding subtype in `self`.
    ///
    /// In other words, `self.is_subtype(other)` returns `true` when
    /// `other` is a subtype of `self`: every type that `other` can
    /// produce is also producible by `self`.
    pub fn is_subtype(&self, other: &UnionType, context: &Context) -> bool {
        other.types.iter().all(|other_member| {
            self.types
                .iter()
                .any(|self_member| other_member.is_subtype(self_member, context).0)
        })
    }

    /// Reconstruct a nested `Type::Operator(TypeOperator::Union, …)` tree
    /// from the flat set of member types.
    ///
    /// The resulting tree is a left-associative fold, consistent with
    /// `builder::union_type` and `TypeOperator::build_type`.
    /// If the set is empty the result is `Type::Empty`.
    pub fn to_type(&self) -> Type {
        self.types
            .iter()
            .cloned()
            .reduce(|acc, t| {
                Type::Operator(
                    TypeOperator::Union,
                    Box::new(acc),
                    Box::new(t),
                    self.help_data.clone(),
                )
            })
            .unwrap_or(Type::Empty(self.help_data.clone()))
    }
}

// ---------------------------------------------------------------------------
// TryFrom<Type>
// ---------------------------------------------------------------------------

impl TryFrom<Type> for UnionType {
    type Error = String;

    /// Convert a `Type` into a `UnionType`.
    ///
    /// Succeeds only when the outermost variant is
    /// `Type::Operator(TypeOperator::Union, …)`.  The binary tree is
    /// recursively flattened so that nested unions like
    /// `(A | B) | (C | D)` become the flat set `{A, B, C, D}`.
    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match &value {
            Type::Operator(TypeOperator::Union, _, _, _) => {
                let mut types = HashSet::new();
                Self::flatten(&value, &mut types);
                let help_data = value.get_help_data();
                Ok(UnionType { types, help_data })
            }
            _ => Err(format!(
                "{} is not a Union type and cannot be converted to UnionType",
                value
            )),
        }
    }
}

// ---------------------------------------------------------------------------
// PartialEq  –  two unions are equal when they contain the same member set
// ---------------------------------------------------------------------------

impl PartialEq for UnionType {
    fn eq(&self, other: &Self) -> bool {
        self.types == other.types
    }
}

impl Eq for UnionType {}
