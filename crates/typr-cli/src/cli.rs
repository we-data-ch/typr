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
    build_file, build_project, check_file, check_project, clean, cran, debug_file, document,
    generate_spg, load, new, pkg_install, pkg_uninstall, pkgdown, run_file, run_file_keep,
    run_project, test, use_package, DebugOptions,
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
        #[arg(long, default_value = "true")]
        renv: bool,
    },
    Check {
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    Build {
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Test build: expose `@testable` private members as `M$.test_<name>`.
        #[arg(long)]
        test: bool,
        /// Disable the incremental-build cache (.typr_cache) and force a full rebuild.
        #[arg(long)]
        no_incremental: bool,
    },
    Run {
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Profile R execution with Rprof and print a summary.
        #[arg(long)]
        profile: bool,
    },
    Debug {
        #[arg(value_name = "FILE")]
        file: PathBuf,
        #[arg(short, long)]
        ast: bool,
        #[arg(short = 'y', long)]
        types: bool,
        #[arg(short, long)]
        r: bool,
        #[arg(short, long)]
        json: bool,
        #[arg(short, long)]
        files: bool,
    },
    Test {
        /// Profile R execution with Rprof and print a summary.
        #[arg(long)]
        profile: bool,
    },
    Pkg {
        #[command(subcommand)]
        pkg_command: PkgCommands,
    },
    /// Reproducible bug catalog (see cases/ and cases/README.md).
    Case {
        #[command(subcommand)]
        case_command: CaseCommands,
    },
    Document,
    /// Build documentation (.Rd via SPG) then generate a pkgdown website.
    Pkgdown,
    Use {
        package_name: String,
    },
    Load,
    Cran,
    Std,
    Clean,
    Repl,
    Lsp,
    /// Generate a Semantic Package Graph (spg.json) from the current project.
    Spg {
        /// Output path (default: spg.json).
        #[arg(long, short, value_name = "FILE")]
        output: Option<PathBuf>,
    },
}

#[derive(Subcommand, Debug)]
enum CaseCommands {
    /// List the catalog (filterable by --status).
    List {
        #[arg(long)]
        status: Option<String>,
    },
    /// Replay cases: OPEN/READY (open), PASS/REGRESS (fixed). Exits 1 on REGRESS.
    Run {
        filter: Option<String>,
        #[arg(long)]
        status: Option<String>,
        /// Keep the temp sandboxes (print their path) for inspection.
        #[arg(long)]
        keep: bool,
    },
    /// Scaffold a new case from a real project + capture observed output.
    Add {
        slug: String,
        #[arg(long)]
        from: Option<String>,
        #[arg(long, default_value = "build")]
        cmd: String,
    },
    /// Snapshot the current TypR project into a `<slug>.case/` bundle (run from the project root).
    Snapshot { slug: Option<String> },
    /// Capture the current generated R as golden and flip status to `fixed`.
    Freeze { id: String },
    /// Show a case (case.toml + expect.md + expect.toml).
    Show { id: String },
}

#[derive(Subcommand, Debug)]
enum PkgCommands {
    Install {
        #[arg(value_name = "PACKAGE", num_args = 1..)]
        packages: Option<Vec<String>>,
    },
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
        Some(Commands::New { name, renv }) => new(&name, renv),
        Some(Commands::Check { file }) => match file {
            Some(path) => check_file(&path),
            _ => check_project(),
        },
        Some(Commands::Build {
            file,
            test,
            no_incremental,
        }) => match file {
            Some(path) => build_file(&path, test),
            _ => build_project(test, no_incremental),
        },
        Some(Commands::Run { file, profile }) => match file {
            Some(path) => run_file_keep(&path, profile),
            _ => run_project(profile),
        },
        Some(Commands::Debug {
            file,
            ast,
            types,
            r,
            json,
            files,
        }) => debug_file(
            &file,
            DebugOptions {
                show_ast: ast,
                show_types: types,
                show_r: r,
                write_json: json,
                show_files: files,
            },
        ),
        Some(Commands::Test { profile }) => test(profile),
        Some(Commands::Pkg { pkg_command }) => match pkg_command {
            PkgCommands::Install { packages } => pkg_install(packages.as_deref()),
            PkgCommands::Uninstall => pkg_uninstall(),
        },
        Some(Commands::Case { case_command }) => match case_command {
            CaseCommands::List { status } => crate::cases::list(status),
            CaseCommands::Run {
                filter,
                status,
                keep,
            } => crate::cases::run(filter, status, keep),
            CaseCommands::Add { slug, from, cmd } => crate::cases::add(&slug, from, &cmd),
            CaseCommands::Snapshot { slug } => crate::cases::snapshot(slug),
            CaseCommands::Freeze { id } => crate::cases::freeze(&id),
            CaseCommands::Show { id } => crate::cases::show(&id),
        },
        Some(Commands::Document) => document(),
        Some(Commands::Pkgdown) => pkgdown(),
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
            rt.block_on(typr_lsp::run_lsp());
        }
        Some(Commands::Repl) => repl::start(),
        Some(Commands::Spg { output }) => generate_spg(output),
        _ => {
            println!("Please specify a subcommand or file to execute");
            std::process::exit(1);
        }
    }
}
