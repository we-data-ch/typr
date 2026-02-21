//! Standard library utilities for TypR CLI
//!
//! Prints the content of the standard library.

use typr_core::components::context::Context;
use typr_core::components::r#type::type_system::TypeSystem;

pub fn standard_library() {
    let context = Context::default();
    for (var, typ) in &context.typing_context.standard_library() {
        println!("{}: {}", var.get_name(), typ.pretty());
    }
}
