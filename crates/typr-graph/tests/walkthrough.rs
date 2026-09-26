//! Snapshot test on the walkthrough example (`visualization_graph_v2.md` §14), the same program
//! already used in `typr-core`'s `type_recorder` tests to validate recording. Run `cargo insta
//! review` after a deliberate change to the builder to re-freeze the snapshot.

use typr_core::components::context::Context;
use typr_core::processes::parsing::parse_from_string;
use typr_core::processes::type_checking::type_recorder::with_recording;
use typr_core::processes::type_checking::typing_with_errors;

const FIL_ROUGE: &str = r#"
type Printable <- interface { show: (Self) -> char };
type Point <- list { x: int, y: int };

let sq <- fn(n: int): int { n * n };

let norm2 <- fn(p: Point): int {
    let a <- sq(p$x);
    a + sq(p$y)
};

let show <- fn(p: Point): char { "Point" };

let p <- Point:{ x = 3, y = 4 };
let d <- Printable(p);
let total <- norm2(p) + 12 + 3;
"#;

#[test]
fn walkthrough_example_builds_without_panicking() {
    let lang = parse_from_string(FIL_ROUGE, "fil_rouge");
    let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));
    assert!(!result.has_errors(), "walkthrough example should typecheck cleanly: {:?}", result.display_errors());

    // `typing()` on a `Lines` node returns the *last statement's* rewritten `Lang` in
    // `type_context.lang` (mirroring a `Scope`'s "value is its last expression"), not the whole
    // program — so the graph is built from the originally parsed tree, whose spans are still
    // what the `TypeTable` was recorded against (spec §7.1's `build(&Lang, &Context, &TypeTable)`
    // takes the parsed AST, not `typing()`'s return value).
    let graph = typr_graph::build(&lang, &result.type_context.context, &table);

    assert!(graph.blocks.contains_key(&typr_graph::BlockKey::top_level(typr_graph::Namespace::Val, "norm2")));
    assert!(graph.blocks.contains_key(&typr_graph::BlockKey::top_level(typr_graph::Namespace::Type, "Point")));

    let json = serde_json::to_string_pretty(&graph).unwrap();
    insta::assert_snapshot!(json);
}
