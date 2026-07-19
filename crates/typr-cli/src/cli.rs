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
    build_file, build_project, check_file, check_project, clean, cran, debug_file, document, generate_spg, load, new,
    pkg_install, pkg_uninstall, pkgdown, run_file, run_file_keep, run_project, test, use_package, DebugOptions,
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
        /// Emit runtime type-boundary assertions (typr_assert_type) in the generated R.
        /// Test-only oracle, never a production mode (soundness_transpilation.md Phase A).
        #[arg(long)]
        checked: bool,
        /// Escalate the base-R/S4 name-collision lint's warnings to build-failing
        /// errors (soundness_transpilation.md Phase C).
        #[arg(long)]
        strict: bool,
    },
    Run {
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
        /// Profile R execution with Rprof and print a summary.
        #[arg(long)]
        profile: bool,
        /// Emit runtime type-boundary assertions (typr_assert_type) in the generated R.
        /// Test-only oracle, never a production mode (soundness_transpilation.md Phase A).
        #[arg(long)]
        checked: bool,
        /// Escalate the base-R/S4 name-collision lint's warnings to build-failing
        /// errors (soundness_transpilation.md Phase C).
        #[arg(long)]
        strict: bool,
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
    /// Type-directed generative fuzzing (soundness_transpilation.md Phase B
    /// Stage 2): generate well-typed programs, run them under `--checked`
    /// through a real R execution, catalog genuine failures.
    Fuzz {
        #[command(subcommand)]
        fuzz_command: FuzzCommands,
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
        /// parse|type|transpile|r-run. Use "r-run" with `--cmd run` to oracle on a real
        /// Rscript execution (rules target "@run") instead of the generated R text.
        #[arg(long, default_value = "transpile")]
        layer: String,
    },
    /// Snapshot the current TypR project into a `<slug>.case/` bundle (run from the project root).
    Snapshot { slug: Option<String> },
    /// Capture the current generated R as golden and flip status to `fixed`.
    Freeze { id: String },
    /// Show a case (case.toml + expect.md + expect.toml).
    Show { id: String },
}

#[derive(Subcommand, Debug)]
enum FuzzCommands {
    /// Generate N programs and build+run each under `--checked` via a real
    /// `typr run` subprocess (needs Rscript on PATH). Not part of `cargo
    /// test` — real wall-clock cost per iteration; run manually or nightly.
    Run {
        #[arg(default_value_t = 50)]
        n: u32,
        /// Base seed (program i uses seed+i). Defaults to a time-based seed
        /// so repeated runs explore different programs.
        #[arg(long)]
        seed: Option<u64>,
        #[arg(long, default_value_t = 4)]
        max_depth: u32,
        /// Keep every sandbox (not just failures), printing its path.
        #[arg(long)]
        keep: bool,
    },
    /// Generation-only production/coverage report — no R execution, no
    /// Rscript required.
    Stats {
        #[arg(default_value_t = 200)]
        n: u32,
        #[arg(long, default_value_t = 4)]
        max_depth: u32,
    },
    /// Promote a persisted failure (fuzz_failures/<hash>/) into cases/, via
    /// the same curated copy `typr case add --from` already does.
    Promote { hash: String },
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
            checked,
            strict,
        }) => match file {
            Some(path) => build_file(&path, test, checked, strict),
            _ => build_project(test, no_incremental, checked, strict),
        },
        Some(Commands::Run {
            file,
            profile,
            checked,
            strict,
        }) => match file {
            Some(path) => run_file_keep(&path, profile, checked, strict),
            _ => run_project(profile, checked, strict),
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
            CaseCommands::Run { filter, status, keep } => crate::cases::run(filter, status, keep),
            CaseCommands::Add { slug, from, cmd, layer } => crate::cases::add(&slug, from, &cmd, &layer),
            CaseCommands::Snapshot { slug } => crate::cases::snapshot(slug),
            CaseCommands::Freeze { id } => crate::cases::freeze(&id),
            CaseCommands::Show { id } => crate::cases::show(&id),
        },
        Some(Commands::Fuzz { fuzz_command }) => match fuzz_command {
            FuzzCommands::Run {
                n,
                seed,
                max_depth,
                keep,
            } => crate::fuzz::run(n, seed, max_depth, keep),
            FuzzCommands::Stats { n, max_depth } => crate::fuzz::stats(n, max_depth),
            FuzzCommands::Promote { hash } => crate::fuzz::promote(&hash),
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
