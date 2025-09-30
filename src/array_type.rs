#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::help_data::HelpData;
use crate::Type;
use crate::Lang;
use crate::graph::TypeSystem;

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
                 .map(|arr| self.index.pretty2() + ", " + &arr))
            .unwrap_or(
                (!self.index.is_generic()) //only return None if is a generic
                    .then_some(self.index.pretty2()))
    }

    pub fn respect_the_bound(&self, index: &Type) -> bool {
        match (self.index.get_index(), index.get_index()) {
            (Some(0), _) => true,
            (Some(i1), Some(i2)) if i2 <= i1 => true,
            _ => false
        }
    }
}

impl TryFrom<Type> for ArrayType {
    type Error = ();

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::Array(t1, t2, h) 
                => Ok(ArrayType{index: *t1, type_: *t2, help_data: h}),
            Type::Vector(t1, t2, h) 
                => Ok(ArrayType{index: *t1, type_: *t2, help_data: h}),
            Type::Sequence(t1, t2, h) 
                => Ok(ArrayType{index: *t1, type_: *t2, help_data: h}),
            _ => Err(())
        }
    }
}
