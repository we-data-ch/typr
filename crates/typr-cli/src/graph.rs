//! `typr graph` — emits the block-graph view of a single TypR file
//! (`visualization_graph_v2.md` §12 étape 2).
//!
//! Deliberately mirrors `typr-graph`'s own tests (spec §7.1) rather than the project-wide
//! machinery `typr check`/`typr build` use: `parse_from_string` → `with_recording(||
//! typing_with_errors(...))` → `typr_graph::build`. Multi-file projects (`module`/`use`/`import`)
//! are out of scope until étape 5 (spec §13 open question F).

use std::path::Path;
use typr_core::components::context::Context;
use typr_core::processes::parsing::parse_from_string;
use typr_core::processes::type_checking::type_recorder::with_recording;
use typr_core::processes::type_checking::typing_with_errors;
use typr_graph::export::{dot, json};
use typr_graph::project::{project, Projection};
use typr_graph::{BlockGraph, BlockKey};

pub fn graph_file(path: &Path, format: &str, focus: Option<&str>, projection: Option<&str>) {
    let mut graph = build_graph_or_exit(path);

    if let Some(name) = projection {
        graph = match name {
            "deps" | "dependencies" => project(&graph, Projection::Dependencies),
            "types" => project(&graph, Projection::Types),
            other => {
                eprintln!("Unknown projection {:?} (expected \"deps\" or \"types\")", other);
                std::process::exit(1);
            }
        };
    }

    match format {
        "json" => print_json(&graph, focus),
        "dot" => print_dot(&graph, focus),
        other => {
            eprintln!("Unknown format {:?} (expected \"json\" or \"dot\")", other);
            std::process::exit(1);
        }
    }
}

fn print_json(graph: &BlockGraph, focus: Option<&str>) {
    let view = match focus {
        Some(key) => match graph.one_level(&BlockKey::from_raw(key)) {
            Some(g) => g,
            None => {
                eprintln!("No block with key {:?} in this graph", key);
                std::process::exit(1);
            }
        },
        None => graph.clone(),
    };
    println!(
        "{}",
        json::to_string_pretty(&view).expect("BlockGraph always serializes")
    );
}

fn print_dot(graph: &BlockGraph, focus: Option<&str>) {
    let focus_key = focus.map(BlockKey::from_raw).unwrap_or_else(BlockKey::program_root);
    match dot::to_dot(graph, &focus_key) {
        Some(rendered) => println!("{}", rendered),
        None => {
            eprintln!("No block with key {:?} in this graph", focus_key.as_str());
            std::process::exit(1);
        }
    }
}

/// `typr graph diff <old> <new>` (spec §12 étape 6): builds each file's block graph
/// independently through the same pipeline as `graph_file`, then pairs blocks up by `BlockKey`.
/// Unlike `typr diff`-style tools this never touches git — the two arguments are just two
/// source files (e.g. two working-tree revisions checked out to temp paths, or simply an
/// old/new copy of the same program), the same "two code excerpts" the spec's playground diff
/// view compares.
pub fn graph_diff(old_file: &Path, new_file: &Path, format: &str) {
    let old_graph = build_graph_or_exit(old_file);
    let new_graph = build_graph_or_exit(new_file);
    let d = typr_graph::diff::diff(&old_graph, &new_graph);

    match format {
        "text" => print!("{}", typr_graph::diff::to_text(&d)),
        "json" => println!(
            "{}",
            serde_json::to_string_pretty(&d).expect("GraphDiff always serializes")
        ),
        other => {
            eprintln!("Unknown format {:?} (expected \"text\" or \"json\")", other);
            std::process::exit(1);
        }
    }
}

fn build_graph_or_exit(path: &Path) -> BlockGraph {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file {:?}: {}", path, e);
            std::process::exit(1);
        }
    };

    let lang = parse_from_string(&source, &path.to_string_lossy());
    let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));
    if result.has_errors() {
        eprintln!("Type errors found in {:?}:", path);
        for err in result.display_errors() {
            eprintln!("  - {}", err);
        }
        std::process::exit(1);
    }

    typr_graph::build(&lang, &result.type_context.context, &table)
}
