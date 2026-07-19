//! Standard library utilities for TypR CLI
//!
//! Generates the binary standard library files (.bin) used by the compiler,
//! and prints the content of the standard library.

use std::path::PathBuf;
use typr_core::components::context::vartype::VarType;
use typr_core::components::context::Context;
use typr_core::components::error_message::help_message::ErrorMsg;
use typr_core::components::error_message::syntax_error::SyntaxError;
use typr_core::components::error_message::type_error::TypeError;
use typr_core::components::language::var::Var;
use typr_core::components::r#type::Type;
use typr_core::processes::parsing::parse_from_string;
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
        // Collect chars that could be a parameter name (alphanumeric + _)
        let mut potential_name = String::new();
        let mut saved_whitespace = String::new();
        let mut found_name = false;

        // Read potential param name
        while let Some(&ch) = chars.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                potential_name.push(ch);
                chars.next();
            } else {
                break;
            }
        }

        // Skip whitespace between name and colon
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                saved_whitespace.push(ch);
                chars.next();
            } else {
                break;
            }
        }

        // Check if followed by ':'
        if let Some(&':') = chars.peek() {
            chars.next(); // consume ':'
                          // Skip whitespace after ':'
            while let Some(&ch) = chars.peek() {
                if ch.is_whitespace() {
                    chars.next();
                } else {
                    break;
                }
            }
            found_name = true;
        }

        if !found_name {
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
const STD_R_TY: &str = include_str!("../configs/std/std_R.ty");
const DEFAULT_TY: &str = include_str!("../configs/std/default.ty");
const FILE_TY: &str = include_str!("../configs/std/file.ty");
const OPTION_TY: &str = include_str!("../configs/std/option.ty");
const PLOT_TY: &str = include_str!("../configs/std/plot.ty");
const LIN_ALG_TY: &str = include_str!("../configs/std/lin_alg.ty");
const SYSTEM_TY: &str = include_str!("../configs/std/system.ty");
const FACTOR_TY: &str = include_str!("../configs/std/factor.ty");
const STATE_TY: &str = include_str!("../configs/std/state.ty");
const FOREIGN_TY: &str = include_str!("../configs/std/foreign.ty");

// Embedded source files for JS
const FUNCTIONS_JS: &str = include_str!("../configs/src/functions_JS.txt");
const STD_JS_TY: &str = include_str!("../configs/std/std_JS.ty");

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

    // 2. .std_r_typed.bin: typed signatures from .ty files
    let r_ty_sources: Vec<(&str, &str)> = vec![
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
    ];
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
}
