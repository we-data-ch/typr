use nom_locate::LocatedSpan;
use serde::{Serialize, Deserialize};
use crate::components::language::Lang;
use std::fs;

#[derive(Debug, PartialEq, Serialize, Deserialize, Eq, Clone, Hash, Default)]
pub struct HelpData {
   offset: usize,
   file_name: String
}

impl HelpData {
    pub fn get_offset(&self) -> usize {
        self.offset
    }
    
    pub fn get_file_name(&self) -> String {
        self.file_name.clone()
    }

    pub fn get_file_data(&self) -> Option<(String, String)> {
        let file_name = self.get_file_name();
        if file_name == "" {
            None
        } else {
            match fs::read_to_string(&file_name).ok() {
                Some(text) => Some((file_name, text)),
                None => None
            }
        }
    }

    pub fn random() -> Self {
        HelpData { offset: 7_usize, file_name: "asfdlwone".to_string() }
            
    }

}


impl From<LocatedSpan<&str, String>> for  HelpData {
   fn from(ls: LocatedSpan<&str, String>) -> Self {
       HelpData {
           offset: ls.location_offset(),
           file_name: ls.extra
       }
   } 
}


impl From<Vec<Lang>> for HelpData {
   fn from(val: Vec<Lang>) -> Self {
       if val.len() > 0 {
            val[0].clone().into()
       } else { HelpData::default() }
   } 
}
