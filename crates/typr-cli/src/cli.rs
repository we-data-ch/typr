//! Command-line interface for TypR
//!
//! Provides the main CLI commands:
//! - `typr init`: Install the R packages TypR needs (devtools, testthat)
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
    /// Install the R packages TypR needs (devtools, testthat).
    Init,
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
    /// Inspect or invalidate the R-name cache (.typr_cache/r_names.json), the
    /// table of which R functions are S3/S4-generic that `typr build` uses to
    /// decide whether shadowing a name needs a generated `.default` fallback.
    Cache {
        #[command(subcommand)]
        cache_command: CacheCommands,
    },
    Repl,
    Lsp,
    /// Print the syntax manifest — the single source of truth for TypR's
    /// lexemes — or render an editor grammar from it.
    ///
    /// The grammars under `editors/` are generated, never hand-edited: keeping
    /// six hand-written copies in sync is what let `impl`/`trait`/`struct` (Rust
    /// keywords TypR has never had) get colored while `opaque`, `module`,
    /// `record` and the kind sigils got nothing.
    Syntax {
        /// Print the manifest as JSON. This is the default with no `--target`.
        #[arg(long)]
        json: bool,
        /// Render a grammar instead: `tmlanguage` (VSCode/Positron/Shiki/Monaco)
        /// or `vim` (Vim/Neovim).
        #[arg(long, value_name = "TARGET")]
        target: Option<String>,
        /// Write the rendered output here instead of stdout.
        #[arg(long, short, value_name = "FILE")]
        output: Option<PathBuf>,
        /// Regenerate every grammar in place, at its canonical path under `editors/`.
        #[arg(long)]
        write: bool,
        /// CI gate: exit 1 when a generated grammar on disk differs from what
        /// the manifest produces (hand-edited, or stale after a parser change).
        #[arg(long)]
        check: bool,
    },
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
enum CacheCommands {
    /// Delete the cached table. The next build re-seeds it from the embedded
    /// base-R snapshot and re-introspects the project's packages.
    Clear,
    /// Re-introspect installed packages, discarding what was known about them
    /// first. Use after updating or reinstalling a package. With no argument,
    /// refreshes every package the cache learned beyond the base-R seed.
    Refresh {
        /// Packages to re-introspect.
        packages: Vec<String>,
    },
    /// Show what the cache knows about an R name, or a summary when no name
    /// is given.
    Show {
        /// The R function name to look up.
        name: Option<String>,
    },
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

/// Commands that must not run the R dependency check: `init` does its own
/// (and is the fix being advertised), `lsp` speaks a protocol over stdio and
/// is driven by an editor rather than a human, and `std` only touches TypR's
/// own `.bin` files — no R involved.
fn skips_r_deps_check(command: &Option<Commands>) -> bool {
    matches!(
        command,
        // `cache` maintains typr's own R-name table; `refresh` needs Rscript
        // and says so itself when it is missing, but none of these need the
        // package set (devtools/roxygen2) `warn_if_missing` checks for.
        // `syntax` renders grammars out of typr's own manifest — it never
        // touches a project, let alone R.
        Some(Commands::Init)
            | Some(Commands::Lsp)
            | Some(Commands::Std)
            | Some(Commands::Cache { .. })
            | Some(Commands::Syntax { .. })
    )
}

/// Main entry point for the CLI
pub fn start() {
    let cli = Cli::parse();

    if !skips_r_deps_check(&cli.command) {
        crate::r_deps::warn_if_missing();
    }

    if let Some(path) = cli.file {
        if cli.command.is_none() {
            run_file(&path);
            return;
        }
    }

    match cli.command {
        Some(Commands::Init) => crate::r_deps::init(),
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
        Some(Commands::Cache { cache_command }) => run_cache_command(cache_command),
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
        Some(Commands::Syntax {
            json,
            target,
            output,
            write,
            check,
        }) => run_syntax_command(json, target, output, write, check),
        Some(Commands::Spg { output }) => generate_spg(output),
        _ => {
            println!("Please specify a subcommand or file to execute");
            std::process::exit(1);
        }
    }
}

/// `typr syntax [--json] [--target tmlanguage] [--write|--check]`
fn run_syntax_command(json: bool, target: Option<String>, output: Option<PathBuf>, write: bool, check: bool) {
    use crate::syntax::Target;

    if json && target.is_some() {
        eprintln!("error: `--json` prints the manifest; drop it to render a `--target` grammar.");
        std::process::exit(1);
    }
    let target = match target.as_deref().map(Target::parse) {
        Some(Ok(t)) => Some(t),
        Some(Err(msg)) => {
            eprintln!("error: {msg}");
            std::process::exit(1);
        }
        None => None,
    };
    crate::syntax::run(target, output, write, check);
}

/// `typr cache <clear|refresh|show>` — maintenance for the R-name cache.
///
/// The cache is not something a build ever needs the user to touch: it fills
/// itself in and `typr clean` removes it with the rest of `.typr_cache`. These
/// commands exist for the one case it cannot detect on its own — a package
/// that changed on disk since it was introspected.
fn run_cache_command(command: CacheCommands) {
    use crate::r_name_cache::RNameCache;
    let root = std::path::Path::new(".");

    match command {
        CacheCommands::Clear => match RNameCache::clear(root) {
            Ok(()) => println!("R-name cache cleared ({}).", RNameCache::path(root).display()),
            Err(e) => {
                eprintln!("error: could not clear the R-name cache: {e}");
                std::process::exit(1);
            }
        },
        CacheCommands::Refresh { packages } => {
            let mut cache = RNameCache::load(root);
            let notes = cache.refresh(&packages);
            for note in &notes {
                eprintln!("\x1b[33mwarning\x1b[0m: {note}");
            }
            if let Err(e) = cache.save(root) {
                eprintln!("error: could not write the R-name cache: {e}");
                std::process::exit(1);
            }
            if packages.is_empty() {
                println!("R-name cache refreshed ({} names known).", cache.names.len());
            } else {
                println!(
                    "R-name cache refreshed for {} ({} names known).",
                    packages.join(", "),
                    cache.names.len()
                );
            }
        }
        CacheCommands::Show { name: None } => {
            let cache = RNameCache::load(root);
            println!("file:       {}", RNameCache::path(root).display());
            println!("R version:  {}", cache.r_version);
            println!("names:      {}", cache.names.len());
            println!("S4 classes: {}", cache.s4_classes.len());
            println!(
                "packages:   {}",
                cache.packages.iter().cloned().collect::<Vec<_>>().join(", ")
            );
            if !cache.failed_packages.is_empty() {
                println!(
                    "not loadable: {}",
                    cache.failed_packages.iter().cloned().collect::<Vec<_>>().join(", ")
                );
            }
        }
        CacheCommands::Show { name: Some(name) } => {
            let cache = RNameCache::load(root);
            match cache.lookup(&name) {
                None => {
                    println!("`{name}` is not known to the R-name cache.");
                    println!(
                        "  → typr would emit a bare `UseMethod` stub for it and generate no \
                         `.default` fallback."
                    );
                }
                Some(entry) => {
                    println!("`{name}` (package `{}`)", entry.pkg);
                    println!("  S3 generic:  {}", entry.s3_generic);
                    println!("  S4 generic:  {}", entry.s4_generic);
                    println!("  has default: {}", entry.has_default);
                    if entry.needs_generated_default() {
                        println!(
                            "  → typr generates `{name}.default <- function(...) {}::{name}(...)` \
                             so its stub does not strand the original.",
                            entry.pkg
                        );
                    } else {
                        println!("  → nothing to generate: the name already dispatches.");
                    }
                }
            }
        }
    }
}
