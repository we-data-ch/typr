//! Shared field/member-lookup helpers for the `Op::Dot`/`Op::Pipe` (see
//! `dot_pipe_access.rs`) and `Op::Dollar` (see `dollar_access.rs`) arms.
//!
//! Both arms repeat the same shape at every branch: find `name` in a field
//! set, and either return its type or push an error and fall back to `Any`.
//! These helpers factor out the lookup + error-push, leaving each call site
//! to decide what to do with the found `Type` (return it directly, unwrap a
//! `Vec` layer for a DataFrame column, etc).
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::Type;
use crate::utils::builder;

/// Looks up `name` in a record/module field set, regardless of the
/// underlying container (`HashSet<ArgumentType>`, `Vec<ArgumentType>`, ...).
pub fn find_field<'a, I>(fields: I, name: &str) -> Option<Type>
where
    I: IntoIterator<Item = &'a ArgumentType>,
{
    fields
        .into_iter()
        .find(|at| at.get_argument_str() == name)
        .map(|at| at.get_type())
}

/// `record.field` / `record$field` lookup: on miss, pushes `FieldNotFound`
/// against `receiver_type` (the type the field was looked up on).
pub fn find_field_or_push_error<'a, I>(
    fields: I,
    name: &str,
    receiver_type: &Type,
    help_data: HelpData,
    errors: &mut Vec<TypRError>,
) -> Option<Type>
where
    I: IntoIterator<Item = &'a ArgumentType>,
{
    match find_field(fields, name) {
        Some(ty) => Some(ty),
        None => {
            errors.push(TypRError::Type(TypeError::FieldNotFound(
                (name.to_string(), help_data),
                receiver_type.clone(),
            )));
            None
        }
    }
}

/// `module.member` / `module$member` lookup: on miss, pushes
/// `UndefinedVariable` (modules track missing members that way, unlike
/// records/DataFrames).
pub fn find_module_field_or_push_error<'a, I>(
    fields: I,
    name: &str,
    help_data: HelpData,
    errors: &mut Vec<TypRError>,
) -> Option<Type>
where
    I: IntoIterator<Item = &'a ArgumentType>,
{
    match find_field(fields, name) {
        Some(ty) => Some(ty),
        None => {
            errors.push(TypRError::Type(TypeError::UndefinedVariable(Lang::Variable {
                name: name.to_string(),
                is_opaque: false,
                related_type: builder::any_type(),
                help_data,
            })));
            None
        }
    }
}
