use crate::components::error_message::help_data::HelpData;

/// Trait for any type that carries source-location information via a `HelpData`.
///
/// Implementors must provide `get_help_data`.  
/// The bonus method `get_file_name_and_text` is derived automatically:
/// it reads the source file from disk and returns `(file_name, source_text)`,
/// or `None` when the location is unknown / the file can't be read.
pub trait Locatable {
    /// Returns the `HelpData` attached to this node.
    fn get_help_data(&self) -> HelpData;

    /// Returns `Some((file_name, source_text))` when the source file is
    /// available, `None` otherwise.  Delegates to `HelpData::get_file_data`.
    fn get_file_name_and_text(&self) -> Option<(String, String)> {
        self.get_help_data().get_file_data()
    }
}
