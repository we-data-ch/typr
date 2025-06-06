use crate::help_data::HelpData;

#[derive(Debug)]
pub enum Generic {
    Normal(String, HelpData),
    Index(String, HelpData),
    Label(String, HelpData)
}
