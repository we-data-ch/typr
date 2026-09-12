//! TypR CLI main entry point
//!
//! This is the main executable for the TypR command-line interface.

mod cache;
mod cases;
mod cli;
mod engine;
mod fuzz;
mod gen_types;
mod io;
mod md_renderer;
mod metaprogramming;
mod progress;
mod project;
mod r_deps;
mod r_name_cache;
mod r_name_lint;
mod rd_renderer;
mod repl;
mod standard_library;
mod syntax;
mod vignette_renderer;

fn main() {
    cli::start()
}
