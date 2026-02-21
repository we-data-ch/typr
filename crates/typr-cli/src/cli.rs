//! Command-line interface for TypR
//!
//! Provides the main CLI commands:
//! - `typr new <name>`: Create a new TypR project
//! - `typr check [file]`: Type-check a file or project
//! - `typr build [file]`: Transpile to R
//! - `typr run [file]`: Build and execute
//! - `typr test`: Run tests
//! - `typr repl`: Start interactive REPL
//! - `typr lsp`: Start Language Server Protocol server

use crate::project::{
    build_file, build_project, check_file, check_project, clean, cran, document, load, new,
    pkg_install, pkg_uninstall, run_file, run_project, test, use_package,
};
use crate::repl;
use crate::standard_library::standard_library;
use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,

    #[arg(short, long, value_name = "TARGET", default_value = "r")]
    target: Option<String>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    New {
        name: String,
    },
    Check {
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    Build {
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    Run {
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    Test,
    Pkg {
        #[command(subcommand)]
        pkg_command: PkgCommands,
    },
    Document,
    Use {
        package_name: String,
    },
    Load,
    Cran,
    Std,
    Clean,
    Repl,
    Lsp,
}

#[derive(Subcommand, Debug)]
enum PkgCommands {
    Install,
    Uninstall,
}

/// Main entry point for the CLI
pub fn start() {
    let cli = Cli::parse();
    if let Some(path) = cli.file {
        if cli.command.is_none() {
            run_file(&path);
            return;
        }
    }

    match cli.command {
        Some(Commands::New { name }) => new(&name),
        Some(Commands::Check { file }) => match file {
            Some(path) => check_file(&path),
            _ => check_project(),
        },
        Some(Commands::Build { file }) => match file {
            Some(path) => build_file(&path),
            _ => build_project(),
        },
        Some(Commands::Run { file }) => match file {
            Some(path) => run_file(&path),
            _ => run_project(),
        },
        Some(Commands::Test) => test(),
        Some(Commands::Pkg { pkg_command }) => match pkg_command {
            PkgCommands::Install => pkg_install(),
            PkgCommands::Uninstall => pkg_uninstall(),
        },
        Some(Commands::Document) => document(),
        Some(Commands::Use { package_name }) => use_package(&package_name),
        Some(Commands::Load) => load(),
        Some(Commands::Cran) => cran(),
        Some(Commands::Std) => standard_library(),
        Some(Commands::Clean) => clean(),
        Some(Commands::Lsp) => {
            // Use a larger stack size (8MB) to avoid stack overflow
            // during deep recursive parsing/type-checking operations
            let rt = tokio::runtime::Builder::new_multi_thread()
                .thread_stack_size(8 * 1024 * 1024)
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(crate::lsp::run_lsp());
        }
        Some(Commands::Repl) => repl::start(),
        _ => {
            println!("Please specify a subcommand or file to execute");
            std::process::exit(1);
        }
    }
}
