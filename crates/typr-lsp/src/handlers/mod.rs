//! LSP feature handlers, one module per capability. `lsp.rs` (the tower-lsp
//! `Backend`) is the only intended consumer; everything is re-exported flat
//! so call sites read `handlers::resolve_hover(...)`.

pub mod code_actions;
pub mod completions;
pub mod document;
pub mod goto_definition;
pub mod hover;
pub mod inlay_hints;
pub mod rename;
pub mod semantic_tokens;
pub mod signature_help;
pub mod utils;

pub use code_actions::*;
pub use completions::*;
pub use document::*;
pub use goto_definition::*;
pub use hover::*;
pub use inlay_hints::*;
pub use rename::*;
pub use semantic_tokens::*;
pub use signature_help::*;
pub use utils::*;
