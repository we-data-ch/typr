//! Phase B / Stage 1 (soundness_transpilation.md): generate well-typed-by-
//! construction TypR programs and assert the real pipeline accepts every one
//! of them (parses, type-checks with no errors, transpiles without panic).
//!
//! A failure here is always triaged, never silently allowlisted: either the
//! generator produced a shape it shouldn't have (fix `program_gen.rs`), or
//! the type-checker/transpiler has a real bug (file it via the `cases/`
//! workflow, then still narrow the generator so this test goes back to
//! green).

use rand::rngs::StdRng;
use rand::SeedableRng;
use std::panic::{catch_unwind, AssertUnwindSafe};
use typr_core::components::context::Context;
use typr_core::processes::parsing::parse_from_string;
use typr_core::processes::type_checking::type_checker::TypeChecker;
use typr_core::utils::program_gen::{self, Coverage};

const N_CASES: u64 = 256;
const MAX_DEPTH: u32 = 3;

#[test]
fn generated_programs_typecheck_and_transpile() {
    let mut rejections: Vec<(u64, String, String)> = Vec::new();
    let mut cov = Coverage::new();

    // `Context::default()` deserializes the embedded stdlib bincode
    // (hundreds of typed signatures) on every call — built once and cloned
    // per iteration instead of paying that cost 256 times sequentially
    // (normal `cargo test` amortizes this across parallel test threads;
    // this loop runs in a single test function, so it doesn't get that for
    // free).
    let base_context = Context::default();

    for seed in 0..N_CASES {
        let mut rng = StdRng::seed_from_u64(seed);
        let prog = program_gen::gen_program(&mut rng, MAX_DEPTH, &mut cov);
        let src = prog.to_source();

        let outcome = catch_unwind(AssertUnwindSafe(|| {
            let ast = parse_from_string(&src, "generated");
            let tc = TypeChecker::new(base_context.clone()).typing_no_panic(&ast);
            if tc.has_errors() {
                return Err(format!("{:?}", tc.get_errors()));
            }
            let _r = tc.transpile();
            Ok(())
        }));

        match outcome {
            Ok(Ok(())) => {}
            Ok(Err(msg)) => rejections.push((seed, src, msg)),
            Err(panic) => {
                let msg = if let Some(s) = panic.downcast_ref::<&str>() {
                    s.to_string()
                } else if let Some(s) = panic.downcast_ref::<String>() {
                    s.clone()
                } else {
                    "unknown panic payload".to_string()
                };
                rejections.push((seed, src, format!("PANIC: {msg}")));
            }
        }
    }

    eprintln!("{}", cov.summary());

    if !rejections.is_empty() {
        let mut dump = format!("{} rejected program(s) out of {N_CASES}:\n", rejections.len());
        for (seed, src, msg) in &rejections {
            dump.push_str(&format!("\n--- seed {seed} ---\n{src}\n{msg}\n"));
        }
        panic!("{dump}");
    }
}
