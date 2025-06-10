use crate::Type;
use crate::help_data::HelpData;

pub fn empty_type() -> Type {
    Type::Empty(HelpData::default())
}

pub fn any_type() -> Type {
    Type::Any(HelpData::default())
}
