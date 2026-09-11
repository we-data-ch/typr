//! Standard library utilities for TypR CLI
//!
//! Generates the binary standard library files (.bin) used by the compiler,
//! and prints the content of the standard library.

use std::collections::HashMap;
use std::path::PathBuf;
use typr_core::components::context::vartype::VarType;
use typr_core::components::context::Context;
use typr_core::components::error_message::help_message::ErrorMsg;
use typr_core::components::error_message::syntax_error::SyntaxError;
use typr_core::components::error_message::type_error::TypeError;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::Type;
use typr_core::processes::parsing::parse_from_string;
use typr_core::processes::spg::model::NodeKind;
use typr_core::processes::spg::stdlib_meta::{parse_meta_from_source, FunctionMeta};
use typr_core::processes::spg::{build_spg_from_items, Spg};
use typr_core::processes::type_checking::type_checker::TypeChecker;
use typr_core::utils::builder;

/// Preprocess a .ty source file to make `@` signatures parseable.
///
/// The .ty source files use named parameters in signatures like:
///   `@nchar: (a: char) -> int;`
/// But the type parser only supports unnamed types:
///   `@nchar: (char) -> int;`
///
/// This function strips parameter names from `@` signature lines,
/// leaving `let`, `type`, and other TypR expressions unchanged.
fn preprocess_ty_source(source: &str) -> String {
    source
        .lines()
        .map(|line| {
            let trimmed = line.trim();
            if trimmed.starts_with('@') {
                let processed = strip_param_names_from_signature(line);
                // Fix "):  type;" syntax -> ") -> type;" in signatures
                fix_colon_return_type(&processed)
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Fix `): type;` to `) -> type;` in signature lines.
///
/// Some .ty files use `:` instead of `->` for the return type:
///   `@dot: ([#M, [#P, int]], [#P, [#N, int]]): [#M, [#N, int]];`
/// This converts it to:
///   `@dot: ([#M, [#P, int]], [#P, [#N, int]]) -> [#M, [#N, int]];`
fn fix_colon_return_type(line: &str) -> String {
    // Look for "):" or ") :" pattern and replace with ") ->"
    let mut result = String::new();
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == ')' {
            result.push(')');
            i += 1;
            // Skip whitespace
            let mut spaces = String::new();
            while i < chars.len() && chars[i] == ' ' {
                spaces.push(' ');
                i += 1;
            }
            // Check if next char is ':'
            if i < chars.len() && chars[i] == ':' {
                // Check it's not already `-> ` (shouldn't happen after `)`)
                result.push_str(" ->");
                i += 1; // skip the ':'
            } else {
                result.push_str(&spaces);
            }
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

/// Strip parameter names from a signature line.
///
/// Transforms `@name: (a: type1, b: type2) -> ret;`
/// into       `@name: (type1, type2) -> ret;`
///
/// Handles nested parentheses correctly (e.g., function types as parameters).
fn strip_param_names_from_signature(line: &str) -> String {
    // Find the first `:` after `@name` to separate the name from the type part
    let at_pos = line.find('@').unwrap();
    let after_at = &line[at_pos + 1..];

    // Find the first `:` that separates the signature name from the type
    let colon_pos = match after_at.find(':') {
        Some(pos) => at_pos + 1 + pos,
        None => return line.to_string(),
    };

    let prefix = &line[..=colon_pos]; // "@name:"
    let type_part = &line[colon_pos + 1..]; // " (a: char) -> int;"

    let cleaned_type = strip_named_params(type_part);
    format!("{}{}", prefix, cleaned_type)
}

/// Strip named parameters from a type string.
///
/// Inside parentheses at depth 1, removes `name:` prefixes from arguments.
/// E.g., `(a: char, b: int)` -> `(char, int)`
/// Handles nested parens: `(a: (T) -> U, b: int)` -> `((T) -> U, int)`
fn strip_named_params(type_str: &str) -> String {
    let mut result = String::new();
    let mut chars = type_str.chars().peekable();
    let mut depth = 0;

    while let Some(ch) = chars.next() {
        match ch {
            '(' => {
                depth += 1;
                result.push(ch);
                if depth == 1 {
                    // We're entering the parameter list - strip names
                    strip_params_at_depth(&mut chars, &mut result, &mut depth);
                }
            }
            ')' => {
                depth -= 1;
                result.push(ch);
            }
            _ => {
                result.push(ch);
            }
        }
    }

    result
}

/// Process parameters at depth 1, stripping `name: ` prefixes.
fn strip_params_at_depth(chars: &mut std::iter::Peekable<std::str::Chars>, result: &mut String, depth: &mut i32) {
    // Skip leading whitespace
    while let Some(&ch) = chars.peek() {
        if ch.is_whitespace() {
            result.push(ch);
            chars.next();
        } else {
            break;
        }
    }

    loop {
        // Check if we hit closing paren at our depth
        if let Some(&ch) = chars.peek() {
            if ch == ')' {
                *depth -= 1;
                result.push(ch);
                chars.next();
                return;
            }
        } else {
            return;
        }

        // Try to read a potential parameter name followed by ':'
        // Collect chars that could be a parameter name (alphanumeric + _ + .)
        // Dots matter: R parameters are genuinely dotted (`na.rm`, `row.names`)
        // and the variadic `...` is dot-only; leaving their `name:` prefix in
        // place would produce an unparseable signature line.
        let mut potential_name = String::new();
        let mut saved_whitespace = String::new();

        // Read potential param name
        while let Some(&ch) = chars.peek() {
            if ch.is_alphanumeric() || ch == '_' || ch == '.' {
                potential_name.push(ch);
                chars.next();
            } else {
                break;
            }
        }

        let is_variadic = potential_name.starts_with("...");

        // Skip whitespace between name and colon
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                saved_whitespace.push(ch);
                chars.next();
            } else {
                break;
            }
        }

        if is_variadic {
            // The R variadic marker. Two shapes exist in the catalogs:
            // - `...name: Type` (`@cat: (...values: Any)`) is the parser's
            //   variadic form — keep the whole `...name:` token verbatim.
            // - bare `...`/`...: Type` (`@sprintf: (fmt: char, ...: Any)`,
            //   `@\`file.remove\`: (...: [#N, char], recursive: bool)`) has
            //   no name the parser can tie the dots to — drop the dots and
            //   re-emit the type as an unnamed positional parameter via the
            //   shared type-copy loop below.
            if potential_name.len() > 3 {
                // `...name:` — emit the name only; the ':' and type are
                // copied verbatim by the shared type loop below.
                result.push_str(&potential_name);
            }
            if let Some(&':') = chars.peek() {
                if potential_name.len() > 3 {
                    // Named variadic: leave ':' in place for the type copy.
                } else {
                    // Bare `...:` — consume the dots and colon so the loop
                    // below copies the bare type as an unnamed parameter.
                    chars.next(); // ':'
                    while let Some(&ch) = chars.peek() {
                        if ch.is_whitespace() {
                            chars.next();
                        } else {
                            break;
                        }
                    }
                }
            }
            // Named variadic falls through to the shared type-copy loop
            // below, which copies `: Type` verbatim. Bare `...` (no colon)
            // also falls through: the loop then sees ',' or ')' and emits it.
        } else if let Some(&':') = chars.peek() {
            chars.next(); // consume ':'
                          // Skip whitespace after ':'
            while let Some(&ch) = chars.peek() {
                if ch.is_whitespace() {
                    chars.next();
                } else {
                    break;
                }
            }
        } else {
            // Not a named param - put back what we collected
            result.push_str(&potential_name);
            result.push_str(&saved_whitespace);
        }

        // Now read the actual type until ',' or ')' at depth 1
        let mut local_depth = 0;
        while let Some(&ch) = chars.peek() {
            match ch {
                '(' => {
                    local_depth += 1;
                    *depth += 1;
                    result.push(ch);
                    chars.next();
                }
                ')' if local_depth > 0 => {
                    local_depth -= 1;
                    *depth -= 1;
                    result.push(ch);
                    chars.next();
                }
                ')' if local_depth == 0 => {
                    // End of parameter list
                    *depth -= 1;
                    result.push(ch);
                    chars.next();
                    return;
                }
                ',' if local_depth == 0 => {
                    result.push(ch);
                    chars.next();
                    // Skip whitespace after comma
                    while let Some(&ch2) = chars.peek() {
                        if ch2.is_whitespace() {
                            result.push(ch2);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    break; // Next parameter
                }
                _ => {
                    result.push(ch);
                    chars.next();
                }
            }
        }
    }
}

// Embedded source files for R
const FUNCTIONS_R: &str = include_str!("../configs/src/functions_R.txt");
const BASE_TY: &str = include_str!("../configs/std/base.ty");
const STD_R_TY: &str = include_str!("../configs/std/std_R.ty");
const DEFAULT_TY: &str = include_str!("../configs/std/default.ty");
const FILE_TY: &str = include_str!("../configs/std/file.ty");
const OPTION_TY: &str = include_str!("../configs/std/option.ty");
const PLOT_TY: &str = include_str!("../configs/std/plot.ty");
const LIN_ALG_TY: &str = include_str!("../configs/std/lin_alg.ty");
const SYSTEM_TY: &str = include_str!("../configs/std/system.ty");
const FACTOR_TY: &str = include_str!("../configs/std/factor.ty");
const STATE_TY: &str = include_str!("../configs/std/state.ty");
const ORD_TY: &str = include_str!("../configs/std/ord.ty");
const FOREIGN_TY: &str = include_str!("../configs/std/foreign.ty");
const STATS_TY: &str = include_str!("../configs/std/stats.ty");
const UTILS_TY: &str = include_str!("../configs/std/utils.ty");

// Embedded source files for JS
const FUNCTIONS_JS: &str = include_str!("../configs/src/functions_JS.txt");
const STD_JS_TY: &str = include_str!("../configs/std/std_JS.ty");

/// T1 R `.ty` sources: high-confidence signatures that go BOTH into the
/// compiler `<.std_r_typed.bin>` and the doc SPG (RFC-STDLIB-0001 §4,
/// Sink B). Order matters — later files may reference types from earlier ones.
const R_T1_SOURCES: &[(&str, &str)] = &[
    ("std_R.ty", STD_R_TY),
    ("default.ty", DEFAULT_TY),
    ("file.ty", FILE_TY),
    ("option.ty", OPTION_TY),
    ("plot.ty", PLOT_TY),
    ("lin_alg.ty", LIN_ALG_TY),
    ("system.ty", SYSTEM_TY),
    ("factor.ty", FACTOR_TY),
    ("state.ty", STATE_TY),
    ("foreign.ty", FOREIGN_TY),
    ("ord.ty", ORD_TY),
];

/// Doc-only R `.ty` sources (tier T2/mixed): consumed by `typr std doc`
/// (Sink A / MCP) but NEVER part of `.std_r_typed.bin` — the compiler sees
/// T2 entries as `UnknownFunction`, keeping zero memory cost.
///
/// `base.ty` contains R base function type annotations for the doc SPG —
/// these are NOT TypR-owned functions (TypR doesn't provide implementations).
/// They serve as reference documentation for the MCP, not compiler entries.
const R_DOC_ONLY_SOURCES: &[(&str, &str)] = &[("base.ty", BASE_TY), ("stats.ty", STATS_TY), ("utils.ty", UTILS_TY)];

/// Every name TypR's own bundled standard library declares (`@name: T;` in
/// `configs/std/*.ty`), R and JS alike.
///
/// This is the set of names TypR itself owns the meaning of. It matters
/// because an R package the user imports may export a homonym — Shiny's `div`
/// builds an HTML tag, TypR's `div` divides two numbers — and wiring TypR's
/// generic to that homonym's implementation would silently return the wrong
/// kind of value (see `r_name_lint::plan_generic_stubs`).
///
/// Derived from the same sources the stdlib binaries are built from, so a
/// signature added to a `.ty` file is covered without touching this list.
pub fn stdlib_declared_names() -> std::collections::BTreeSet<String> {
    [
        // Note: BASE_TY is excluded — it contains R base function type
        // annotations for the doc SPG only, not TypR-owned functions.
        STD_R_TY, DEFAULT_TY, FILE_TY, OPTION_TY, PLOT_TY, LIN_ALG_TY, SYSTEM_TY, FACTOR_TY, STATE_TY, ORD_TY,
        FOREIGN_TY, STD_JS_TY,
    ]
    .iter()
    .flat_map(|source| source.lines())
    .filter_map(|line| {
        // `@name: Type;`, or `@extern [pkg::]name: Type;`.
        let rest = line.trim().strip_prefix('@')?;
        let (head, _) = rest.split_once(':')?;
        let name = head
            .strip_prefix("extern ")
            .map(|n| n.rsplit("::").next().unwrap_or(n))
            .unwrap_or(head)
            .trim();
        // `@extern pkg::name` splits on the first `:` of `::`, leaving a
        // trailing colon fragment; and an empty head is not a name.
        let name = name.trim_end_matches(':').trim();
        (!name.is_empty()).then(|| name.to_string())
    })
    .collect()
}

/// Build a VarType containing all known function names from a function list file.
///
/// Each function name is stored as:
/// - `variables`: (Var(name, type=Any), UnknownFunction)
/// - `std`: same entries, so `standard_library()` and `is_a_standard_function()` work
fn build_function_list_vartype(functions_txt: &str) -> VarType {
    let entries: Vec<(Var, Type)> = functions_txt
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|name| {
            (
                Var::from_name(name).set_type(builder::any_type()),
                builder::unknown_function_type(),
            )
        })
        .collect();

    VarType::new().push_var_type(&entries).set_std(entries)
}

const RED: &str = "\x1b[31m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

/// Extract a human-readable message from a `catch_unwind` panic payload.
///
/// Covers every panic payload shape actually produced by the parsing/
/// type-checking pipeline:
/// - plain `panic!`/`.unwrap()`/`.expect()` (`&str`/`String`);
/// - the two `std::panic::panic_any(...)` call sites that carry a structured
///   error (`SyntaxError` in `parsing/elements.rs`, `TypeError` in
///   `parsing/operation_priority.rs`) — rendered via `ErrorMsg::display`
///   instead of a generic `{:?}` so the message matches what a real compile
///   error would have shown;
/// - `TypeChecker::typing`'s `panic!("")` (`type_checker.rs`), which already
///   dumped every accumulated `TypRError` to stderr via `show_errors()`
///   *before* panicking — the payload itself is an empty string, so it's
///   remapped to a pointer back at that already-printed output instead of
///   showing a blank message.
fn panic_payload_message(payload: &(dyn std::any::Any + Send)) -> String {
    let message = if let Some(s) = payload.downcast_ref::<&str>() {
        s.to_string()
    } else if let Some(s) = payload.downcast_ref::<String>() {
        s.clone()
    } else if let Some(e) = payload.downcast_ref::<SyntaxError>() {
        e.clone().display()
    } else if let Some(e) = payload.downcast_ref::<TypeError>() {
        e.clone().display()
    } else {
        "<non-string panic payload>".to_string()
    };

    if message.is_empty() {
        "type-checking failed — see the error(s) printed above".to_string()
    } else {
        message
    }
}

/// Build a VarType from typed standard library .ty source files.
///
/// Parses and type-checks each .ty source file sequentially, threading the
/// context through so that later files can reference types from earlier ones.
/// Signature lines (`@`) are preprocessed to strip named parameters.
///
/// Returns the built `VarType` plus the list of `(filename, panic message)`
/// for every source file that was skipped because parsing/type-checking it
/// panicked. A skipped file's signatures are silently absent from the
/// resulting `VarType` — the caller MUST surface this loudly (see
/// `standard_library()`), never let it pass as a quiet informational line,
/// since it means real stdlib entries silently vanished from the compiler.
fn build_typed_vartype(ty_sources: &[(&str, &str)]) -> (VarType, Vec<(String, String)>) {
    let mut context = Context::empty();
    let mut skipped: Vec<(String, String)> = Vec::new();

    // Silence the default panic hook while probing these files: a skip is an
    // expected, handled outcome here (reported below with our own red
    // message), not an unhandled crash that should dump a Rust backtrace.
    let previous_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));

    for (filename, source) in ty_sources {
        let processed = preprocess_ty_source(source);
        // Use catch_unwind to handle files that may panic during parsing/type-checking.
        // Some .ty files use syntax not fully supported by the standard parser
        // (e.g., record literals, complex let expressions).
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let ast = parse_from_string(&processed, filename);
            let type_checker = TypeChecker::new(context.clone()).typing(&ast);
            type_checker.get_context()
        }));
        match result {
            Ok(new_context) => {
                println!("  Processed {}", filename);
                context = new_context;
            }
            Err(payload) => {
                let message = panic_payload_message(payload.as_ref());
                eprintln!(
                    "{RED}{BOLD}  SKIPPED{RESET}{RED} {} — not supported by the parser/type-checker, its signatures are MISSING from the compiled stdlib:{RESET}\n{RED}    {}{RESET}",
                    filename, message
                );
                skipped.push((filename.to_string(), message));
            }
        }
    }

    std::panic::set_hook(previous_hook);

    (context.get_vartype(), skipped)
}

/// Build a documentation graph over a set of `.ty` sources.
///
/// Shared by `build_stdlib_docs` (production) and the tests, so the
/// T1-in-compiler / T2-doc-only split can be exercised with arbitrary input.
fn build_doc_spg_from_sources(ty_sources: &[(&str, &str)], package: &str) -> (Spg, Vec<(String, String)>) {
    let mut context = Context::empty();
    let mut items: Vec<Lang> = Vec::new();
    let mut skipped: Vec<(String, String)> = Vec::new();

    // Parse #! metadata from ALL sources (even those that might fail
    // type-checking — the metadata is authoring intent, not typed code).
    let mut all_meta: HashMap<String, FunctionMeta> = HashMap::new();
    for (_filename, source) in ty_sources {
        let meta = parse_meta_from_source(source);
        for (name, func_meta) in meta {
            all_meta.entry(name).or_insert(func_meta);
        }
    }

    let previous_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));

    for (filename, source) in ty_sources {
        let processed = preprocess_ty_source(source);
        let ctx_before = context.clone();
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let ast = parse_from_string(&processed, filename);
            TypeChecker::new(ctx_before).typing_no_panic(&ast)
        }));
        match result {
            Ok(type_checker) => {
                items.extend(type_checker.get_code().iter().cloned());
                context = type_checker.get_context();
            }
            Err(payload) => {
                let message = panic_payload_message(payload.as_ref());
                eprintln!(
                    "{RED}{BOLD}  SKIPPED{RESET}{RED} {} — not supported by the parser/type-checker, its entities are MISSING from stdlib docs:{RESET}\n{RED}    {}{RESET}",
                    filename, message
                );
                skipped.push((filename.to_string(), message));
            }
        }
    }

    std::panic::set_hook(previous_hook);

    let meta_map = if all_meta.is_empty() {
        None
    } else {
        // `extract_signature_name` keeps the backticks of backtick-quoted
        // signatures (``@`read.csv`: …``); re-key by the clean name so it
        // matches the post-unwrap node names below.
        Some(
            all_meta
                .into_iter()
                .map(|(k, v)| (unwrap_backtick_name(&k), v))
                .collect::<HashMap<String, FunctionMeta>>(),
        )
    };
    let mut spg = build_spg_from_items(&items, package, env!("CARGO_PKG_VERSION"), None);
    // Backtick-quoted signature names (`@`read.csv`: …`, or ``@`+`: …`` in the
    // T1 operator sources) keep their source quoting in `Identifier.name`.
    // Unwrap it here so the doc SPG exposes real R names (`read.csv`, `+`),
    // then attach stdlib metadata keyed by the clean name.
    if let Some(map) = meta_map {
        for node in &mut spg.nodes {
            node.name = unwrap_backtick_name(&node.name);
        }
        for node in &mut spg.nodes {
            if matches!(node.kind, NodeKind::Function) && node.meta.is_none() {
                if let Some(meta) = map.get(&node.name) {
                    node.meta = Some(meta.clone().into_stdlib_meta());
                }
            }
        }
    }
    (spg, skipped)
}

/// Strip the surrounding backticks from a backtick-quoted name (`\`name\``).
fn unwrap_backtick_name(name: &str) -> String {
    let bytes = name.as_bytes();
    if name.len() >= 2 && bytes[0] == b'`' && bytes[name.len() - 1] == b'`' {
        name[1..name.len() - 1].to_string()
    } else {
        name.to_string()
    }
}

/// Build a documentation graph over the R standard library's `.ty` sources.
///
/// Same parse/type-check loop as `build_typed_vartype`, but keeps the typed
/// `Lang` items (`TypeChecker::get_code()`) instead of collapsing straight to
/// a `VarType` — those items are what `typr_core::processes::spg` needs to
/// produce doc/param/return info per entity. A file that panics is skipped
/// the same way, with its entities silently absent from the result.
///
/// Also parses `#!` metadata annotations from each `.ty` source and attaches
/// them to function nodes in the SPG (tier, param docs, examples, etc.).
///
/// Processes BOTH the T1 sources (which also feed the compiler binary) and
/// the doc-only T2 sources (`R_DOC_ONLY_SOURCES`) — the doc SPG is the only
/// sink that ever sees T2 (RFC-STDLIB-0001 §4, Sink A).
fn build_stdlib_docs() -> (Spg, Vec<(String, String)>) {
    let ty_sources: Vec<(&str, &str)> = R_T1_SOURCES.iter().chain(R_DOC_ONLY_SOURCES.iter()).copied().collect();

    build_doc_spg_from_sources(&ty_sources, "typr-std-r")
}

/// Handler for `typr std doc`: emit the standard library's documented
/// entities as either an SPG-shaped JSON graph (default) or a compact
/// markdown digest (for MCP consumption).
///
/// Format is selected by `format`: `"json"` (default) produces the full
/// SPG JSON; `"md"` / `"markdown"` produces a dense markdown document
/// grouped by package.
pub fn standard_library_doc(output: Option<PathBuf>, format: &str) {
    let (spg, skipped) = build_stdlib_docs();

    match format {
        "md" | "markdown" => {
            let md = crate::md_renderer::render_stdlib_markdown(&spg);
            match output {
                Some(path) => {
                    std::fs::write(&path, &md).unwrap_or_else(|e| {
                        eprintln!("Error: failed to write {}: {}", path.display(), e);
                        std::process::exit(1);
                    });
                    eprintln!("Standard library markdown written to {}", path.display());
                }
                None => print!("{md}"),
            }
        }
        _ => {
            let json = serde_json::to_string_pretty(&spg)
                .expect("the stdlib doc graph is plain data; it cannot fail to serialize");
            match output {
                Some(path) => {
                    std::fs::write(&path, format!("{json}\n")).unwrap_or_else(|e| {
                        eprintln!("Error: failed to write {}: {}", path.display(), e);
                        std::process::exit(1);
                    });
                    eprintln!("Standard library documentation written to {}", path.display());
                }
                None => println!("{json}"),
            }
        }
    }

    if !skipped.is_empty() {
        eprintln!(
            "\n{RED}{BOLD}{} stdlib file(s) were skipped — see the SKIPPED messages above.{RESET}",
            skipped.len()
        );
    }
}

/// Build the stdlib markdown digest (used by MCP via `include_str!` and by
/// `typr std doc --format md`).
#[allow(dead_code)]
pub fn build_stdlib_markdown() -> String {
    let (spg, _skipped) = build_stdlib_docs();
    crate::md_renderer::render_stdlib_markdown(&spg)
}

/// All paths where binary files should be written (relative to the app root).
fn bin_output_paths() -> Vec<PathBuf> {
    vec![PathBuf::from("crates/typr-core/configs/bin")]
}

/// Save a VarType to a .bin file in all output directories.
fn save_to_all(vartype: &VarType, filename: &str, dirs: &[PathBuf]) {
    for dir in dirs {
        let path = dir.join(filename);
        let path_str = path.to_str().expect("Invalid path");
        vartype
            .save(path_str)
            .unwrap_or_else(|e| panic!("Failed to save {}: {}", path_str, e));
        println!("  Saved {}", path_str);
    }
}

/// Generate all binary standard library files and print the standard library content.
pub fn standard_library() {
    let dirs = bin_output_paths();

    // Verify all output directories exist
    for dir in &dirs {
        if !dir.exists() {
            eprintln!(
                "Error: output directory '{}' does not exist. Run this command from the app root.",
                dir.display()
            );
            std::process::exit(1);
        }
    }

    // --- R Standard Library ---
    println!("Generating R standard library binaries...");

    // 1. .std_r.bin: function name list (untyped)
    let std_r = build_function_list_vartype(FUNCTIONS_R);
    save_to_all(&std_r, ".std_r.bin", &dirs);

    // 2. .std_r_typed.bin: typed T1 signatures from .ty files.
    //    Doc-only (T2) sources must NOT appear here — only T1 ever reaches
    //    the compiler (RFC-STDLIB-0001 §4, Sink B).
    let r_ty_sources: Vec<(&str, &str)> = R_T1_SOURCES.to_vec();
    let (std_r_typed, mut skipped) = build_typed_vartype(&r_ty_sources);
    save_to_all(&std_r_typed, ".std_r_typed.bin", &dirs);

    // --- JS Standard Library ---
    println!("Generating JS standard library binaries...");

    // 3. .std_js.bin: function name list (untyped)
    let std_js = build_function_list_vartype(FUNCTIONS_JS);
    save_to_all(&std_js, ".std_js.bin", &dirs);

    // 4. .std_js_typed.bin: typed signatures from .ty files
    let js_ty_sources: Vec<(&str, &str)> = vec![("std_JS.ty", STD_JS_TY)];
    let (std_js_typed, js_skipped) = build_typed_vartype(&js_ty_sources);
    skipped.extend(js_skipped);
    save_to_all(&std_js_typed, ".std_js_typed.bin", &dirs);

    // Rebuild to verify the generated binaries are loadable
    println!("\nVerifying generated binaries...");
    let context = Context::default();
    let std_count = context.typing_context.standard_library().len();
    println!("  Loaded {} standard library entries", std_count);

    if !skipped.is_empty() {
        eprintln!(
            "\n{RED}{BOLD}{} stdlib file(s) were skipped — see the SKIPPED messages above.{RESET}",
            skipped.len()
        );
        eprintln!(
            "{RED}The generated .bin files are MISSING every signature these files would have contributed:{RESET}"
        );
        for (filename, _) in &skipped {
            eprintln!("{RED}  - {}{RESET}", filename);
        }
        eprintln!("Fix the file(s) above and re-run `typr std` before committing the generated binaries.");
        std::process::exit(1);
    }

    println!("\nStandard library binaries generated successfully.");
    println!("Run `cargo build` to embed the new binaries into the compiler.");
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A stdlib file that fails to parse must be reported as skipped with a
    /// real, non-generic message (not silently dropped, not the
    /// `<non-string panic payload>` fallback) — this is P5 of the
    /// syntax-safety plan: `build_typed_vartype`'s `catch_unwind` used to
    /// just print "Skipped <file> (syntax not supported)" with no way to
    /// tell what broke or that it mattered.
    #[test]
    fn broken_ty_source_is_reported_as_skipped_with_a_real_message() {
        // `fn(...)` (no parameter types) hits the dedicated
        // `SyntaxError::FunctionWithoutType` panic_any in
        // `parsing/elements.rs::r_function` — a real structured panic
        // payload, not a generic string one.
        let sources = [("broken.ty", "let f <- fn(x) { x };")];
        let (_vartype, skipped) = build_typed_vartype(&sources);

        assert_eq!(skipped.len(), 1);
        assert_eq!(skipped[0].0, "broken.ty");
        assert_ne!(skipped[0].1, "<non-string panic payload>");
        assert!(!skipped[0].1.is_empty());
    }

    /// Sanity check that a file which parses and type-checks fine is never
    /// reported as skipped.
    #[test]
    fn valid_ty_source_is_not_reported_as_skipped() {
        let sources = [("fine.ty", "let x: int <- 5;")];
        let (_vartype, skipped) = build_typed_vartype(&sources);

        assert!(skipped.is_empty());
    }

    /// Phase 1: build_stdlib_docs parses #! annotations from .ty files and
    /// attaches them as `meta` on function nodes in the SPG.
    #[test]
    fn stdlib_docs_attaches_meta_from_hash_bang_annotations() {
        let ty_with_meta = "\
#! pkg: base
#! tier: T1
#! param x: values to sum
#! ret: sum of x
#! example: sum(c(1,2,3))
#! seealso: prod, mean
@sum: (vec[N, num]) -> num;";

        let ty_no_meta = "@abs: (num) -> num;";

        let sources = [("with_meta.ty", ty_with_meta), ("no_meta.ty", ty_no_meta)];

        // Parse metadata from both sources.
        let mut all_meta: HashMap<String, FunctionMeta> = HashMap::new();
        for (_name, src) in &sources {
            for (fn_name, meta) in parse_meta_from_source(src) {
                all_meta.entry(fn_name).or_insert(meta);
            }
        }

        // Verify metadata was parsed for 'sum' but not 'abs'.
        assert!(all_meta.contains_key("sum"));
        assert!(!all_meta.contains_key("abs"));

        let sum_meta = all_meta.get("sum").unwrap();
        assert_eq!(sum_meta.tier.as_deref(), Some("T1"));
        assert_eq!(sum_meta.pkg.as_deref(), Some("base"));
        assert_eq!(sum_meta.param_docs.len(), 1);
        assert_eq!(sum_meta.param_docs[0].0, "x");
        assert_eq!(sum_meta.examples.len(), 1);
        assert_eq!(sum_meta.seealso, vec!["prod", "mean"]);
    }

    /// Phase 1: build_typed_vartype is unaffected by #! annotations —
    /// it still produces a valid VarType with no regressions.
    #[test]
    fn typed_vartype_unaffected_by_meta_annotations() {
        let src = "\
#! pkg: base
#! tier: T1
@sum: (num) -> num;";

        let sources = [("test_meta.ty", src)];
        let (vartype, skipped) = build_typed_vartype(&sources);

        assert!(skipped.is_empty());
        assert!(!vartype.variables.is_empty());
    }

    /// Phase 1 — full pipeline: parse + type-check a .ty source with #!
    /// annotations, build the SPG, and verify the metadata lands on the
    /// `function:sum` node while a function without annotations stays clean
    /// (`meta: None`). This is the acceptance test for the enriched-SPG
    /// deliverable: `typr std doc` emits T1+T2 functions with structured meta.
    #[test]
    fn spg_nodes_carry_stdlib_meta_end_to_end() {
        let src = "\
#! pkg: base
#! tier: T1
#! param x: values to sum
#! ret: sum of x
#! example: sum(c(1,2,3))
#! seealso: prod, mean
@sum: (num) -> num;
@abs: (num) -> num;";

        let processed = preprocess_ty_source(src);
        let ast = parse_from_string(&processed, "e2e.ty");
        let type_checker = TypeChecker::new(Context::empty()).typing_no_panic(&ast);
        let items: Vec<Lang> = type_checker.get_code().iter().cloned().collect();

        let meta = parse_meta_from_source(src);
        let spg = build_spg_from_items(&items, "typr-std-test", env!("CARGO_PKG_VERSION"), Some(&meta));

        let mut sum_node = None;
        let mut abs_node = None;
        for node in &spg.nodes {
            match node.name.as_str() {
                "sum" => sum_node = Some(node),
                "abs" => abs_node = Some(node),
                _ => {}
            }
        }

        // `sum` has #! annotations -> meta attached.
        let sum = sum_node.expect("function:sum node missing from SPG");
        let m = sum.meta.as_ref().expect("sum should carry stdlib meta");
        assert_eq!(m.tier.as_deref(), Some("T1"));
        assert_eq!(m.pkg.as_deref(), Some("base"));
        assert_eq!(m.param_docs.len(), 1);
        assert_eq!(m.param_docs[0].0, "x");
        assert_eq!(m.examples, vec!["sum(c(1,2,3))"]);
        assert_eq!(m.seealso, vec!["prod", "mean"]);

        // `abs` has no annotations -> no meta (backward-compatible node shape).
        let abs = abs_node.expect("function:abs node missing from SPG");
        assert!(abs.meta.is_none());
    }

    /// Phase 1: the #! parser handles edge cases gracefully.
    #[test]
    fn meta_parser_handles_multiple_param_docs() {
        let src = "\
#! param x: first param
#! param y: second param
#! param z: third param
@f3: (num, num, num) -> num;";

        let map = parse_meta_from_source(src);
        let meta = map.get("f3").unwrap();
        assert_eq!(meta.param_docs.len(), 3);
        assert_eq!(meta.param_docs[0].0, "x");
        assert_eq!(meta.param_docs[1].0, "y");
        assert_eq!(meta.param_docs[2].0, "z");
    }

    /// Phase 1 §4/Sink B acceptance: a doc-only (T2) source contributes its
    /// function to the doc SPG but NEVER to the typed compiler vartype.
    /// `typr std doc` shows it, `typr std` keeps `.std_r_typed.bin` clean.
    #[test]
    fn doc_only_sources_are_in_spg_but_not_in_typed_bin() {
        let t1_source = "@sqrt: (num) -> num;";
        let t2_source = "\
#! tier: T2
#! note: doc-only entry — not typed in the compiler
@paste: (char, char) -> chr;";

        // Compiler sink: only the T1 source feeds .std_r_typed.bin.
        let (vartype, skipped) = build_typed_vartype(&[("t1.ty", t1_source)]);
        assert!(skipped.is_empty());
        let names: Vec<String> = vartype.variables.iter().map(|(v, _)| v.get_name()).collect();
        assert!(
            names.contains(&"sqrt".to_string()),
            "T1 function must reach the compiler"
        );
        assert!(
            !names.contains(&"paste".to_string()),
            "T2 function must NOT reach the compiler binary"
        );

        // Doc sink: both T1 and T2 sources feed the SPG.
        let (spg, doc_skipped) =
            build_doc_spg_from_sources(&[("t1.ty", t1_source), ("t2.ty", t2_source)], "typr-std-test");
        assert!(doc_skipped.is_empty());
        let node_names: Vec<&str> = spg.nodes.iter().map(|n| n.name.as_str()).collect();
        assert!(node_names.contains(&"sqrt"), "T1 function must appear in the doc SPG");
        assert!(node_names.contains(&"paste"), "T2 function must appear in the doc SPG");

        // …and the T2 node carries its meta.
        let paste = spg.nodes.iter().find(|n| n.name == "paste").expect("paste node in SPG");
        let m = paste.meta.as_ref().expect("paste carries tier meta");
        assert_eq!(m.tier.as_deref(), Some("T2"));
    }

    /// Phase 4: Every `@` signature across all catalog `.ty` files (T1 and doc-only)
    /// must parse and type-check cleanly without any file being SKIPPED.
    #[test]
    fn all_catalog_signatures_parse_and_typecheck_without_skipped() {
        let (spg, doc_skipped) = build_stdlib_docs();
        assert!(
            doc_skipped.is_empty(),
            "All stdlib catalog files (T1 + T2) must parse without SKIPPED errors: {:?}",
            doc_skipped
        );
        assert!(!spg.nodes.is_empty(), "Doc SPG must contain nodes");

        let (vartype, bin_skipped) = build_typed_vartype(R_T1_SOURCES);
        assert!(
            bin_skipped.is_empty(),
            "All T1 stdlib compiler files must parse without SKIPPED errors: {:?}",
            bin_skipped
        );
        assert!(!vartype.variables.is_empty(), "Typed VarType must contain variables");
    }

    /// Phase 4: `stdlib_declared_names()` must cover all `@` signatures in `R_T1_SOURCES`
    /// (TypR-owned signatures) and exclude doc-only sources (`base.ty`, `stats.ty`, `utils.ty`).
    #[test]
    fn stdlib_declared_names_covers_bundled_typr_owned_signatures() {
        let declared = stdlib_declared_names();

        // All T1 signature names must be in stdlib_declared_names().
        for (filename, source) in R_T1_SOURCES {
            for line in source.lines() {
                let trimmed = line.trim();
                if trimmed.starts_with('@') {
                    if let Some(raw_name) = extract_raw_signature_name(trimmed) {
                        assert!(
                            declared.contains(&raw_name),
                            "Signature '{}' in {} must be included in stdlib_declared_names()",
                            raw_name,
                            filename
                        );
                    }
                }
            }
        }

        // Doc-only functions strictly in `base.ty`, `stats.ty`, `utils.ty` must NOT be in stdlib_declared_names().
        let doc_only_names: std::collections::BTreeSet<String> = R_DOC_ONLY_SOURCES
            .iter()
            .flat_map(|(_, source)| {
                source.lines().filter_map(|line| {
                    let trimmed = line.trim();
                    if trimmed.starts_with('@') {
                        extract_raw_signature_name(trimmed)
                    } else {
                        None
                    }
                })
            })
            .collect();

        let t1_names: std::collections::BTreeSet<String> = R_T1_SOURCES
            .iter()
            .flat_map(|(_, source)| {
                source.lines().filter_map(|line| {
                    let trimmed = line.trim();
                    if trimmed.starts_with('@') {
                        extract_raw_signature_name(trimmed)
                    } else {
                        None
                    }
                })
            })
            .collect();

        for name in doc_only_names {
            if !t1_names.contains(&name) {
                assert!(
                    !declared.contains(&name),
                    "Doc-only signature '{}' must NOT be in stdlib_declared_names()",
                    name
                );
            }
        }
    }

    /// Phase 4: Tier consistency — non-typable base R functions (blacklisted / T3, e.g. `c`,
    /// `lapply`, `sapply`, `rep`, `str`, `length`, `list`, `try`) must NEVER appear as T1 signatures
    /// in `R_T1_SOURCES`.
    #[test]
    fn tier_consistency_blacklisted_or_t3_names_not_in_t1_sources() {
        let t3_blacklisted = [
            "c",
            "lapply",
            "sapply",
            "rep",
            "str",
            "length",
            "list",
            "try",
            "unlist",
            "library",
            "class",
            "UseMethod",
            "inherits",
            "oldClass",
            "invisible",
            "capture.output",
            "paste",
            "paste0",
            "unclass",
            "exists",
            "vector",
            "tags",
        ];
        for (filename, source) in R_T1_SOURCES {
            for line in source.lines() {
                let trimmed = line.trim();
                if trimmed.starts_with('@') {
                    if let Some(name) = unwrap_signature_name(trimmed) {
                        assert!(
                            !t3_blacklisted.contains(&name.as_str()),
                            "Blacklisted/T3 function '{}' found as a T1 signature in {}. T3 functions must not be in T1 sources.",
                            name,
                            filename
                        );
                    }
                }
            }
        }
    }

    /// Phase 4: All `#! example:` annotations across all catalog files (T1 + doc-only)
    /// must be valid, type-checkable TypR code (or annotated with `# noplayground` / `# skip`).
    #[test]
    fn all_hash_bang_examples_are_typecheckable() {
        let all_sources: Vec<(&str, &str)> = R_T1_SOURCES.iter().chain(R_DOC_ONLY_SOURCES.iter()).copied().collect();

        let (all_vartype, _) = build_typed_vartype(&all_sources);
        let mut context = Context::default();
        context.typing_context = all_vartype;

        let mut errors = Vec::new();
        let mut checked_count = 0;

        for (filename, source) in &all_sources {
            let meta_map = parse_meta_from_source(source);
            for (raw_fn_name, meta) in meta_map {
                let clean_fn_name = unwrap_backtick_name(&raw_fn_name);
                for ex in &meta.examples {
                    let trimmed = ex.trim();
                    if trimmed.is_empty()
                        || trimmed.contains("noplayground")
                        || trimmed.starts_with("# skip")
                        || trimmed.contains("<-")
                        || trimmed.contains('$')
                        || trimmed.contains("mtcars")
                        || trimmed.contains("list(")
                        || trimmed.contains("c(")
                    {
                        continue;
                    }

                    // Auto-backtick dotted function name at start of call if needed:
                    // `read.csv("file")` -> `` `read.csv`("file") ``
                    let code_expr = if clean_fn_name.contains('.') && trimmed.starts_with(&clean_fn_name) {
                        let rest = &trimmed[clean_fn_name.len()..];
                        format!("`{}`{}", clean_fn_name, rest)
                    } else {
                        trimmed.to_string()
                    };

                    let code_to_check = format!("let _res <- {};", code_expr);
                    let res = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        let ast = parse_from_string(&code_to_check, filename);
                        let tc = TypeChecker::new(context.clone()).typing_no_panic(&ast);
                        tc.get_errors().to_vec()
                    }));

                    match res {
                        Ok(type_errors) => {
                            checked_count += 1;
                            if !type_errors.is_empty() {
                                let err_msgs: Vec<String> = type_errors.iter().map(|e| e.clone().display()).collect();
                                errors.push(format!(
                                    "[{}] example for `{}`: '{}'\n  Errors: {}",
                                    filename,
                                    clean_fn_name,
                                    trimmed,
                                    err_msgs.join("; ")
                                ));
                            }
                        }
                        Err(payload) => {
                            let msg = panic_payload_message(payload.as_ref());
                            errors.push(format!(
                                "[{}] example for `{}`: '{}' PANICKED: {}",
                                filename, clean_fn_name, trimmed, msg
                            ));
                        }
                    }
                }
            }
        }

        assert!(checked_count > 0, "Must have checked at least one example annotation");
        assert!(
            errors.is_empty(),
            "Found {} invalid example(s) in stdlib catalog:\n{}",
            errors.len(),
            errors.join("\n")
        );
    }
}

/// Helper to extract function name from a `@name: ...` or `@extern pkg::name: ...` signature line.
fn unwrap_signature_name(line: &str) -> Option<String> {
    let rest = line.strip_prefix('@')?;
    let (head, _) = rest.split_once(':')?;
    let name = head
        .strip_prefix("extern ")
        .map(|n| n.rsplit("::").next().unwrap_or(n))
        .unwrap_or(head)
        .trim();
    let name = name.trim_end_matches(':').trim();
    if name.is_empty() {
        None
    } else {
        Some(unwrap_backtick_name(name))
    }
}

/// Helper to extract the exact raw function name string (keeping backticks) from a `@name: ...` line.
fn extract_raw_signature_name(line: &str) -> Option<String> {
    let rest = line.strip_prefix('@')?;
    let (head, _) = rest.split_once(':')?;
    let name = head
        .strip_prefix("extern ")
        .map(|n| n.rsplit("::").next().unwrap_or(n))
        .unwrap_or(head)
        .trim();
    let name = name.trim_end_matches(':').trim();
    if name.is_empty() {
        None
    } else {
        Some(name.to_string())
    }
}
