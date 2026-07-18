//! TypR CLI main entry point
//!
//! This is the main executable for the TypR command-line interface.

mod cache;
mod cases;
mod cli;
mod engine;
mod fuzz;
mod io;
mod metaprogramming;
mod progress;
mod project;
mod r_name_lint;
mod rd_renderer;
mod repl;
mod standard_library;
mod vignette_renderer;

fn main() {
    cli::start()
}
