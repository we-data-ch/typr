use clap::{Parser, Subcommand};
use std::path::PathBuf;
use crate::interface::repl;
use crate::utils::standard_library::standard_library;
use crate::utils::project_management::run_file;
use crate::utils::project_management::new;
use crate::utils::project_management::check_file;
use crate::utils::project_management::check_project;
use crate::utils::project_management::build_file;
use crate::utils::project_management::build_project;
use crate::utils::project_management::run_project;
use crate::utils::project_management::test;
use crate::utils::project_management::pkg_install;
use crate::utils::project_management::pkg_uninstall;
use crate::utils::project_management::document;
use crate::utils::project_management::use_package;
use crate::utils::project_management::load;
use crate::utils::project_management::cran;
use crate::utils::project_management::clean;

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
    Repl
}

#[derive(Subcommand, Debug)]
enum PkgCommands {
    Install,
    Uninstall,
}

pub fn start() {
    let cli = Cli::parse();
    if let Some(path) = cli.file {
        if cli.command.is_none() {
            run_file(&path);
            return;
        }
    }

    match cli.command {
        Some(Commands::New { name }) => {
            new(&name)
        },
        Some(Commands::Check { file }) => {
            match file {
                Some(path) => check_file(&path),
                _ => check_project(),
            }
        },
        Some(Commands::Build { file }) => {
            match file {
                Some(path) => build_file(&path),
                _ => build_project(),
            }
        },
        Some(Commands::Run { file }) => {
            match file {
                Some(path) => run_file(&path),
                _ => run_project(),
            }
        },
        Some(Commands::Test) => {
            test()
        },
        Some(Commands::Pkg { pkg_command }) => {
            match pkg_command {
                PkgCommands::Install => pkg_install(),
                PkgCommands::Uninstall => pkg_uninstall(),
            }
        },
        Some(Commands::Document) => {
            document()
        },
        Some(Commands::Use { package_name }) => {
            use_package(&package_name)
        },
        Some(Commands::Load) => {
            load()
        },
        Some(Commands::Cran) => {
            cran()
        },
        Some(Commands::Std) => {
            standard_library()
        },
        Some(Commands::Clean) => {
            clean()
        },
        Some(Commands::Repl) => {
            repl::start()
        },
        _ => {
            println!("Please specify a subcommand or file to execute");
            std::process::exit(1);
        }
    }
}

