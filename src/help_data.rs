use nom_locate::LocatedSpan;
use serde::Serialize;

#[derive(Debug, PartialEq, Serialize, Eq, Clone, Hash)]
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
}

impl From<LocatedSpan<&str, String>> for  HelpData {
   fn from(ls: LocatedSpan<&str, String>) -> Self {
       HelpData {
           offset: ls.location_offset(),
           file_name: ls.extra
       }
   } 
}
