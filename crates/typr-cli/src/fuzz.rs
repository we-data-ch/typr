//! `typr fuzz` — Phase B Stage 2 (soundness_transpilation.md): generate
//! well-typed-by-construction TypR programs via typr-core's `program_gen`
//! (Stage 1's generator), build+run each through a *real* `typr` subprocess
//! under `--checked`, and persist genuine failures to `fuzz_failures/<hash>/`
//! in a shape `typr case add --from` can curate-copy straight into `cases/`.
//!
//! Deliberately **not** a `cargo test` target: it shells out to `Rscript`,
//! which can be absent, and each iteration costs real wall-clock time. Run
//! manually (`typr fuzz run`) or from a nightly job, per the plan doc.
//!
//! No shrinking in this v1 (unlike the plan doc's "shrink → cas minimal"
//! step): the generator's own declaration counts are already small (≤ 2
//! aliases/fns/vals, depth ≤ 4), so failing programs are usually small
//! enough to read as-is. A structural shrinker is a natural follow-up if
//! that stops being true.

use rand::rngs::StdRng;
use rand::SeedableRng;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use typr_core::utils::program_gen;
use typr_core::utils::program_gen::Coverage;

const FUZZ_FAILURES_DIR: &str = "fuzz_failures";

/// The exact prefix `typr_assert_type` (configs/src/std.R) writes into R's
/// `stop()` message on a real soundness violation — the one bug class this
/// whole pipeline exists to catch. Any other non-zero exit is still worth
/// capturing (a crash, a build failure the in-process Stage 1 oracle
/// missed, …) but isn't the target, so it's bucketed separately.
const CHECKED_MARKER: &str = "[typr --checked]";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Outcome {
    Pass,
    CheckedAssertionFailure,
    OtherFailure,
}

impl Outcome {
    fn label(self) -> &'static str {
        match self {
            Outcome::Pass => "pass",
            Outcome::CheckedAssertionFailure => "checked-assertion-failure",
            Outcome::OtherFailure => "other-failure",
        }
    }
}

struct FuzzCase {
    seed: u64,
    src: String,
    outcome: Outcome,
    combined_output: String,
}

fn default_base_seed() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0)
}

fn rscript_available() -> bool {
    Command::new("Rscript")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Minimal buildable project: `typr build`/`typr run` synthesize
/// `R/std.R`/`load_module.R`/etc. themselves — a bare `TypR/main.ty` plus
/// `DESCRIPTION`/`NAMESPACE` (mirroring `cases::copy_repro_curated`'s
/// baseline) is enough.
fn scaffold_project(src: &str) -> PathBuf {
    let tmp = crate::cases::unique_tmp();
    let work = tmp.join("work");
    std::fs::create_dir_all(work.join("TypR")).expect("create TypR/ dir");
    std::fs::create_dir_all(work.join("R")).expect("create R/ dir");
    std::fs::write(work.join("TypR/main.ty"), src).expect("write TypR/main.ty");
    std::fs::write(work.join("DESCRIPTION"), crate::cases::MINIMAL_DESCRIPTION)
        .expect("write DESCRIPTION");
    std::fs::write(work.join("NAMESPACE"), crate::cases::MINIMAL_NAMESPACE)
        .expect("write NAMESPACE");
    work
}

/// A single `typr run --checked` subprocess builds *and* runs the project
/// (`run_project` calls `build_project_impl` internally) — one invocation
/// covers both the "doesn't even type-check in project mode" bucket and the
/// "type-checks but the generated R misbehaves at runtime" bucket. Run as a
/// subprocess (not in-process) because `run_project`/`build_project_impl`
/// call `std::process::exit` on failure, which would kill this whole loop
/// after the very first rejection.
fn execute(work: &Path) -> (String, Outcome) {
    let exe = std::env::current_exe().expect("current exe path");
    match Command::new(&exe)
        .arg("run")
        .arg("--checked")
        .current_dir(work)
        .output()
    {
        Ok(out) => {
            let stdout = String::from_utf8_lossy(&out.stdout);
            let stderr = String::from_utf8_lossy(&out.stderr);
            let combined = format!("{stdout}\n{stderr}");
            let outcome = if out.status.success() {
                Outcome::Pass
            } else if combined.contains(CHECKED_MARKER) {
                Outcome::CheckedAssertionFailure
            } else {
                Outcome::OtherFailure
            };
            (combined, outcome)
        }
        Err(e) => (
            format!("failed to spawn `typr run --checked`: {e}"),
            Outcome::OtherFailure,
        ),
    }
}

fn content_hash(src: &str) -> String {
    let mut hasher = DefaultHasher::new();
    src.hash(&mut hasher);
    format!("{:08x}", hasher.finish() as u32)
}

/// `TypR/main.ty` only — matches what `cases::copy_repro_curated` expects
/// under `--from`; it synthesizes `DESCRIPTION`/`NAMESPACE` itself when
/// absent. `seed.txt`/`outcome.txt` are fuzz-only bookkeeping, ignored by
/// the curated copy.
fn persist_failure(case: &FuzzCase) -> String {
    let hash = content_hash(&case.src);
    let dir = PathBuf::from(FUZZ_FAILURES_DIR).join(&hash);
    let _ = std::fs::remove_dir_all(&dir);
    let _ = std::fs::create_dir_all(dir.join("TypR"));
    let _ = std::fs::write(dir.join("TypR/main.ty"), &case.src);
    let _ = std::fs::write(dir.join("seed.txt"), case.seed.to_string());
    let _ = std::fs::write(
        dir.join("outcome.txt"),
        format!("{}\n\n{}", case.outcome.label(), case.combined_output),
    );
    hash
}

pub fn run(n: u32, seed: Option<u64>, max_depth: u32, keep: bool) {
    if !rscript_available() {
        eprintln!(
            "Warning: Rscript not found on PATH — every iteration will fail as \
             other-failure, not because of a real bug. Install R or add Rscript to PATH."
        );
    }

    let base_seed = seed.unwrap_or_else(default_base_seed);
    let mut cov = Coverage::new();
    let mut pass = 0u32;
    let mut failures: Vec<FuzzCase> = Vec::new();

    for i in 0..n {
        let seed = base_seed.wrapping_add(i as u64);
        let mut rng = StdRng::seed_from_u64(seed);
        let prog = program_gen::gen_program(&mut rng, max_depth, &mut cov);
        let src = prog.to_source();

        let work = scaffold_project(&src);
        let (combined_output, outcome) = execute(&work);

        if keep {
            println!("  seed {seed}: sandbox kept at {}", work.display());
        } else if let Some(parent) = work.parent() {
            let _ = std::fs::remove_dir_all(parent);
        }

        match outcome {
            Outcome::Pass => pass += 1,
            _ => failures.push(FuzzCase {
                seed,
                src,
                outcome,
                combined_output,
            }),
        }
    }

    println!("{}", cov.summary());
    println!(
        "pass={pass} checked-assertion-failures={} other-failures={}",
        failures
            .iter()
            .filter(|f| f.outcome == Outcome::CheckedAssertionFailure)
            .count(),
        failures
            .iter()
            .filter(|f| f.outcome == Outcome::OtherFailure)
            .count(),
    );

    for case in &failures {
        let hash = persist_failure(case);
        println!(
            "  [{}] seed {} -> {FUZZ_FAILURES_DIR}/{hash}/  (promote: `typr fuzz promote {hash}`)",
            case.outcome.label(),
            case.seed
        );
    }
}

pub fn stats(n: u32, max_depth: u32) {
    let base_seed = default_base_seed();
    let mut cov = Coverage::new();
    for i in 0..n {
        let mut rng = StdRng::seed_from_u64(base_seed.wrapping_add(i as u64));
        let _ = program_gen::gen_program(&mut rng, max_depth, &mut cov);
    }
    println!("{}", cov.summary());
}

pub fn promote(hash: &str) {
    let from = format!("{FUZZ_FAILURES_DIR}/{hash}");
    if !PathBuf::from(&from).exists() {
        eprintln!("No such fuzz failure: {from}");
        std::process::exit(1);
    }
    let slug = format!("fuzz-{hash}");
    crate::cases::add(&slug, Some(from), "run", "r-run");
}
