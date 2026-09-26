//! Étape 1 acceptance criterion (spec §12): "tout programme qui passe `typr check` produit un
//! graphe sans panique." Replays every `cases/*/repro/TypR/main.ty` — parsed standalone, so a
//! multi-file case that only type-checks as part of its whole project is simply skipped (not a
//! false negative: we only claim totality over programs that *do* pass type-checking here).

use std::fs;
use std::panic::{catch_unwind, AssertUnwindSafe};
use typr_core::components::context::Context;
use typr_core::processes::parsing::parse_from_string;
use typr_core::processes::type_checking::type_recorder::with_recording;
use typr_core::processes::type_checking::typing_with_errors;

fn cases_dir() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../cases")
}

#[test]
fn builds_every_typechecking_case_without_panicking() {
    let cases_dir = cases_dir();
    let mut checked = 0usize;
    let mut skipped = 0usize;

    for entry in fs::read_dir(&cases_dir).expect("cases/ directory should exist at the workspace root") {
        let entry = entry.expect("readable cases/ entry");
        let main_ty = entry.path().join("repro/TypR/main.ty");
        if !main_ty.is_file() {
            continue;
        }
        let source = fs::read_to_string(&main_ty).expect("readable main.ty");
        let file_name = main_ty.to_string_lossy().to_string();

        let lang = catch_unwind(AssertUnwindSafe(|| parse_from_string(&source, &file_name)));
        let Ok(lang) = lang else {
            skipped += 1;
            continue;
        };

        let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));
        if result.has_errors() {
            skipped += 1;
            continue;
        }

        // Built from the originally parsed tree, not `result.type_context.lang` — `typing()` on
        // a `Lines` node returns only its last statement's rewritten form, not the whole program.
        let outcome = catch_unwind(AssertUnwindSafe(|| typr_graph::build(&lang, &result.type_context.context, &table)));
        assert!(outcome.is_ok(), "typr-graph panicked building {}", main_ty.display());
        checked += 1;
    }

    assert!(checked > 0, "expected at least one cases/ repro to typecheck standalone and be built");
    eprintln!("totality: built {checked} case(s) without panicking, skipped {skipped} (parse/typecheck failure or multi-file dependency)");
}
