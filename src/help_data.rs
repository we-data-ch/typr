use nom_locate::LocatedSpan;
use serde::Serialize;
use std::fs;

#[derive(Debug, PartialEq, Serialize, Eq, Clone, Hash, Default)]
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

}


impl From<LocatedSpan<&str, String>> for  HelpData {
   fn from(ls: LocatedSpan<&str, String>) -> Self {
       HelpData {
           offset: ls.location_offset(),
           file_name: ls.extra
       }
   } 
}
