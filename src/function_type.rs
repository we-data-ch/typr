use crate::argument_kind::ArgumentKind;
use crate::Type;
use crate::help_data::HelpData;

pub struct FunctionType(Vec<ArgumentKind>, Vec<Type>, Type, HelpData);

impl TryFrom<Type> for FunctionType {
    type Error = String;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        if let Type::Function(kinds, args, ret, h) = value {
            Ok(FunctionType(kinds, args, *ret, h))
        } else { 
            Err(format!("{} is a type not convertible to FunctionType", value))
        }
    }
}
