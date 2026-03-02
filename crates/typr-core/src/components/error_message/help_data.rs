use crate::components::language::Lang;
use nom_locate::LocatedSpan;
use serde::{Deserialize, Serialize};

#[cfg(not(feature = "wasm"))]
use std::fs;

use std::cell::RefCell;
use std::collections::HashMap;

// Thread-local storage for source content (used in WASM mode)
thread_local! {
    static SOURCE_CACHE: RefCell<HashMap<String, String>> = RefCell::new(HashMap::new());
}

/// Register source content for a file (used for WASM and error display)
pub fn register_source(file_name: &str, content: &str) {
    SOURCE_CACHE.with(|cache| {
        cache
            .borrow_mut()
            .insert(file_name.to_string(), content.to_string());
    });
}

/// Clear all registered sources
pub fn clear_sources() {
    SOURCE_CACHE.with(|cache| {
        cache.borrow_mut().clear();
    });
}

/// Get source from cache
fn get_cached_source(file_name: &str) -> Option<String> {
    SOURCE_CACHE.with(|cache| cache.borrow().get(file_name).cloned())
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Eq, Clone, Hash, Default)]
pub struct HelpData {
    offset: usize,
    file_name: String,
}

impl HelpData {
    pub fn get_offset(&self) -> usize {
        self.offset
    }

    pub fn get_file_name(&self) -> String {
        self.file_name.clone()
    }

    /// Get file data (filename, content) for error display.
    ///
    /// In WASM mode, this uses the registered source cache.
    /// In native mode, this falls back to filesystem if not in cache.
    pub fn get_file_data(&self) -> Option<(String, String)> {
        let file_name = self.get_file_name();
        if file_name.is_empty() {
            return None;
        }

        // First try the cache
        if let Some(text) = get_cached_source(&file_name) {
            return Some((file_name, text));
        }

        // In native mode, fall back to filesystem
        #[cfg(not(feature = "wasm"))]
        {
            match fs::read_to_string(&file_name).ok() {
                Some(text) => {
                    // Cache it for future use
                    register_source(&file_name, &text);
                    Some((file_name, text))
                }
                None => None,
            }
        }

        #[cfg(feature = "wasm")]
        {
            None
        }
    }

    pub fn random() -> Self {
        HelpData {
            offset: 7_usize,
            file_name: "asfdlwone".to_string(),
        }
    }
}

impl From<LocatedSpan<&str, String>> for HelpData {
    fn from(ls: LocatedSpan<&str, String>) -> Self {
        HelpData {
            offset: ls.location_offset(),
            file_name: ls.extra,
        }
    }
}

impl From<Vec<Lang>> for HelpData {
    fn from(val: Vec<Lang>) -> Self {
        if !val.is_empty() {
            val[0].clone().into()
        } else {
            HelpData::default()
        }
    }
}
