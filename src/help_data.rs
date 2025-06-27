use nom_locate::LocatedSpan;
use serde::Serialize;

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

    pub fn example() -> Self {
        HelpData {
            offset: [1, 2, 3].len(),
            file_name: "Hey.ty".to_string()
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
