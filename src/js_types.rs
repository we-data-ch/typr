use crate::Type;
use crate::builder;

pub fn get_element_by_id() -> Type {
    builder::any_type()
}

pub fn document_type() -> Type {
    builder::record_type(&[
        ("URL".to_string(), builder::character_type_default()),
        ("title".to_string(), builder::character_type_default()),
        ("domain".to_string(), builder::character_type_default()),
        ("getElementById".to_string(), get_element_by_id()),
    ])
}
