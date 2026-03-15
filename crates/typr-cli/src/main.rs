//! TypR CLI main entry point
//!
//! This is the main executable for the TypR command-line interface.

mod cli;
mod engine;
mod io;
mod lsp;
mod lsp_parser;
mod metaprogramming;
mod project;
mod repl;
mod standard_library;

fn main() {
    cli::start()
}
