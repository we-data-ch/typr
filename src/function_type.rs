use crate::argument_kind::ArgumentKind;
use crate::Type;

pub struct FunctionType(Vec<ArgumentKind>, Vec<Type>, Type);

impl TryFrom<Type> for FunctionType {
    type Error = String;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        if let Type::Function(kinds, args, ret) = value {
            Ok(FunctionType(kinds, args, *ret))
        } else { 
            Err(format!("{} is a type not convertible to FunctionType", value))
        }
    }
}
