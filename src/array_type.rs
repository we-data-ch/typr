use crate::help_data::HelpData;
use crate::Type;

pub struct ArrayType {
    index: Type,
    type_: Type,
    help_data: HelpData
}

impl ArrayType {
    //Get the shape of an array type
    //Return None if we encounter a generic index
    pub fn get_shape(&self) -> Option<String> {
        ArrayType::try_from(self.type_.clone())
            .map(|arr_t| arr_t.get_shape()
                 .map(|arr| self.index.to_string() + ", " + &arr))
            .unwrap_or(
                (!self.index.is_generic()) //only return None if is a generic
                    .then_some(self.index.to_string()))
    }
}

impl TryFrom<Type> for ArrayType {
    type Error = ();

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::Array(t1, t2, h) => Ok(ArrayType{index: *t1, type_: *t2, help_data: h}),
            _ => Err(())
        }
    }
}
