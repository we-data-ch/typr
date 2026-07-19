//! Project management utilities for TypR CLI
//!
//! Provides functions for:
//! - Creating new projects
//! - Building and checking projects
//! - Running tests
//! - Package management

use crate::cache;
use crate::engine::{parse_code, parse_code_from_str, parse_code_with_info, write_std_for_type_checking};
use crate::progress::Step;
use std::fs;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use typr_core::components::context::config::Environment;
use typr_core::components::context::Context;
use typr_core::components::error_message::help_message::ErrorMsg;
use typr_core::components::error_message::syntax_error::SyntaxError;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::processes::spg::build_spg_from_items;
use typr_core::processes::spg::model::{NodePayload, Spg, Visibility};
use typr_core::processes::type_checking::type_checker::TypeChecker;

/// Options for the `typr debug` subcommand
#[derive(Debug, Clone, Default)]
pub struct DebugOptions {
    pub show_ast: bool,
    pub show_types: bool,
    pub show_r: bool,
    pub write_json: bool,
    pub show_files: bool,
}

/// Render a pipeline step heading with consistent formatting
fn print_step(title: &str) {
    println!("\n{}", "─".repeat(60));
    println!("  ▶ {}", title);
    println!("{}", "─".repeat(60));
}

/// Prints every syntax error collected while parsing (entry file + every
/// imported module), and reports whether any of them represents code
/// silently dropped from the AST (`SyntaxError::UnknownElement`) or a
/// forbidden-outright construct (`SyntaxError::SingleLetterTypeName`,
/// `SyntaxError::KeywordRecordPositionalElements`) — the only kinds serious
/// enough to abort the pipeline. Everything else (forgotten semicolons, `//`
/// comments, `let`/`type` mixups, ...) is fully recovered by the parser
/// already and is only printed as a heads-up.
///
/// Previously these were parsed and thrown away entirely by
/// `engine::parse_code`/`parse_code_with_info`/`parse_code_from_str` — a
/// stray `//` comment or any other unparseable trailing statement could
/// silently vanish from a build with zero indication anything was wrong.
///
/// `SingleLetterTypeName` is fatal rather than a warning even though the
/// parser now recovers a full `Lang::Alias` for it: a single-letter alias
/// name (`type A <- ...;`) was never usable before this recovery existed (it
/// silently collapsed to nothing), and it collides with the single-uppercase-
/// letter convention reserved for generics — so there is no working project
/// that failing here could break, and letting it through as a mere warning
/// risks confusing generic-collision bugs downstream.
///
/// `KeywordRecordPositionalElements` (P3 of the syntax-safety plan) is fatal
/// for the same reason, not legalized: `list{1, 2, 3}` used to silently
/// become a positional tuple (indistinguishable from a typo of `list{ x = 1,
/// y = 2 }`) while `record{1, 2, 3}`/`object{1, 2, 3}` misparsed even more
/// confusingly (bare identifier + dangling `{...}`). A repo-wide grep found
/// no existing project relying on the brace-positional form — `list(...)`
/// (parens) remains the working, unaffected way to build positional tuples.
///
/// `TupleDestructureArityMismatch` (T1 of the type-checking edge-case audit)
/// is fatal too: under-binding (`let :{a, b} <- :{1, 2, 3};`) used to drop
/// the extra element with zero diagnostic anywhere, and over-binding fell
/// through to a generic, badly-localized `Unknown element` error pointing at
/// `std.ty` instead of the user's file — neither is a state any working
/// project depends on, and treating it as a mere warning would leave the
/// silent-data-loss case exactly as silent as before.
fn report_syntax_errors(errors: Vec<SyntaxError>) -> bool {
    if errors.is_empty() {
        return false;
    }
    let (fatal, warnings): (Vec<_>, Vec<_>) = errors.into_iter().partition(|e| {
        matches!(
            e,
            SyntaxError::UnknownElement { .. }
                | SyntaxError::SingleLetterTypeName { .. }
                | SyntaxError::KeywordRecordPositionalElements { .. }
                | SyntaxError::TupleDestructureArityMismatch { .. }
        )
    });
    if !warnings.is_empty() {
        eprintln!("Syntax warnings:");
        for err in warnings {
            eprintln!("  - {}", err.display());
        }
    }
    if !fatal.is_empty() {
        eprintln!("Syntax errors found (some source code could not be parsed and was dropped):");
        for err in &fatal {
            eprintln!("  - {}", err.clone().display());
        }
    }
    !fatal.is_empty()
}

/// Pretty-print a Lang AST tree, omitting noise like HelpData
fn print_ast(lang: &Lang, indent: usize) -> String {
    use Lang::*;
    let pad = "  ".repeat(indent);
    let result = match lang {
        Number { value, .. } => format!("{}Number({})", pad, value),
        Integer { value, .. } => format!("{}Integer({})", pad, value),
        Bool { value, .. } => format!("{}Bool({})", pad, value),
        Char { value, .. } => format!("{}Char({:?})", pad, value),
        Variable { name, is_opaque, .. } => {
            let op = if *is_opaque { " (opaque)" } else { "" };
            format!("{}Variable({}{})", pad, name, op)
        }
        Let {
            variable,
            r#type,
            expression,
            is_public,
            ..
        } => {
            let pub_ = if *is_public { "@pub " } else { "" };
            let var = Var::from_language((**variable).clone())
                .map(|v| v.get_name())
                .unwrap_or_default();
            let ty = r#type.pretty();
            let ty_str = if ty == "Empty" {
                String::new()
            } else {
                format!(" : {}", ty)
            };
            format!(
                "{}{}let {}{}\n{}",
                pad,
                pub_,
                var,
                ty_str,
                print_ast(expression, indent + 1)
            )
        }
        Function {
            parameters,
            return_type,
            body,
            ..
        } => {
            let params: Vec<String> = parameters
                .iter()
                .map(|p| format!("{}: {}", p.get_argument_str(), p.get_type().pretty()))
                .collect();
            let ret = return_type.pretty();
            format!(
                "{}fn({}) -> {}\n{}",
                pad,
                params.join(", "),
                ret,
                print_ast(body, indent + 1)
            )
        }
        Lambda { parameters, body, .. } => {
            let params: Vec<String> = parameters.iter().map(|p| p.simple_print()).collect();
            format!(
                "{}fn({}) {{ ... }}\n{}",
                pad,
                params.join(", "),
                print_ast(body, indent + 1)
            )
        }
        FunctionApp {
            identifier, arguments, ..
        } => {
            let name = match identifier.as_ref() {
                Variable { name, .. } => name.clone(),
                other => other.simple_print(),
            };
            let args: Vec<String> = arguments.iter().map(|a| print_ast(a, 0)).collect();
            format!("{}{}({})", pad, name, args.join(", "))
        }
        Alias {
            identifier,
            parameters,
            target_type,
            is_public,
            ..
        } => {
            let pub_ = if *is_public { "@pub " } else { "" };
            let name = Var::from_language((**identifier).clone())
                .map(|v| v.get_name())
                .unwrap_or_default();
            let params: Vec<String> = parameters.iter().map(|p| p.pretty()).collect();
            let params_str = if params.is_empty() {
                String::new()
            } else {
                format!("<{}>", params.join(", "))
            };
            format!("{}{}type {}{} <- {}", pad, pub_, name, params_str, target_type.pretty())
        }
        Lines { value, .. } => {
            let items: Vec<String> = value.iter().map(|l| print_ast(l, indent + 1)).collect();
            format!("{}Sequence\n{}", pad, items.join("\n"))
        }
        Scope { body, .. } => {
            let items: Vec<String> = body.iter().map(|l| print_ast(l, indent + 1)).collect();
            format!("{}Scope\n{}", pad, items.join("\n"))
        }
        Operator { operator, rhs, lhs, .. } => {
            let op_str = format!("{}", operator);
            let rhs_str = print_ast(rhs, indent + 1);
            let lhs_str = print_ast(lhs, indent + 1);
            format!("{}({})\n{}\n{}", pad, op_str, rhs_str, lhs_str)
        }
        If {
            condition,
            if_block,
            else_block,
            ..
        } => {
            format!(
                "{}if\n{}\n{}then:\n{}\n{}else:\n{}",
                pad,
                print_ast(condition, indent + 1),
                pad,
                print_ast(if_block, indent + 1),
                pad,
                print_ast(else_block, indent + 1)
            )
        }
        Match { target, branches, .. } => {
            let branches_str: Vec<String> = branches
                .iter()
                .map(|(pat, body)| format!("{}  {} =>\n{}", pad, print_ast(pat, 0), print_ast(body, indent + 2)))
                .collect();
            format!(
                "{}match\n{}\n{}cases:\n{}",
                pad,
                print_ast(target, indent + 1),
                pad,
                branches_str.join("\n")
            )
        }
        Module { name, body, .. } => {
            let items: Vec<String> = body.iter().map(|l| print_ast(l, indent + 1)).collect();
            format!("{}module {}\n{}", pad, name, items.join("\n"))
        }
        ConstructorCall { type_name, fields, .. } => {
            let args: Vec<String> = fields
                .iter()
                .map(|a| format!("{} = {}", a.0, print_ast(&a.1, 0)))
                .collect();
            format!("{}{}:{{ {} }}", pad, type_name, args.join(", "))
        }
        Tag { name, value, .. } => {
            format!("{}.{}({})", pad, name, print_ast(value, 0))
        }
        List { value, .. } => {
            let fields: Vec<String> = value
                .iter()
                .map(|a| format!("{} = {}", a.0, print_ast(&a.1, 0)))
                .collect();
            format!("{}:{{ {} }}", pad, fields.join(", "))
        }
        Null(_) => format!("{}Null", pad),
        Empty(_) => format!("{}Empty", pad),
        Comment { value, .. } => format!("{}// {}", pad, value),
        Use { .. } => format!("{}use", pad),
        _ => format!("{}{}", pad, lang.simple_print()),
    };
    result
}

/// Add a new Debug subcommand to inspect the pipeline step by step
pub fn debug_file(path: &Path, opts: DebugOptions) {
    let show_all = !opts.show_ast && !opts.show_types && !opts.show_r && !opts.show_files;
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file {:?}: {}", path, e);
            std::process::exit(1);
        }
    };

    println!("{}", "=".repeat(60));
    println!("  TypR Debug: {:?}", path);
    println!("{}", "=".repeat(60));

    // Step 1: Parse
    if show_all || opts.show_ast {
        print_step("Parsing");
        let result = typr_core::processes::parsing::parse_from_string_with_errors(&source, &path.to_string_lossy());
        println!("  AST ({:?} nodes):", path);
        println!("{}", print_ast(&result.ast, 1));

        if result.has_errors() {
            println!("\n  Syntax errors:");
            for err in &result.errors {
                println!("    - {}", err.simple_message());
            }
        }
    }

    // Step 2: Type-check
    if show_all || opts.show_types || opts.show_r || opts.show_files {
        print_step("Type Checking + Transpilation");
        let context = Context::default();
        let ast = typr_core::processes::parsing::parse_from_string(&source, &path.to_string_lossy());
        let type_checker = TypeChecker::new(context).typing_no_panic(&ast);

        if type_checker.has_errors() {
            println!("  Errors:");
            type_checker.show_errors();
        } else {
            let types = type_checker.get_types();
            for (i, ty) in types.iter().enumerate() {
                println!("  expr {} type: {}", i + 1, ty.pretty());
            }
            println!();
        }

        let final_context = type_checker.get_context();

        // Step 3: Transpile — show the full file as it would be executed by Rscript
        if show_all || opts.show_r {
            print_step("Full R File (as executed)");
            let transpiled = type_checker.clone().transpile();
            let preamble = "source('std.R', echo = FALSE)\nsource('generic_functions.R')\nsource('types.R')";
            println!("{}\n{}", preamble, transpiled);
        }

        // Step 4: Show intermediate generated files
        if show_all || opts.show_files {
            let type_annotations = final_context.get_type_anotations();
            let generic_functions = final_context
                .get_all_generic_functions()
                .iter()
                .map(|(var, _)| var.get_name())
                .filter(|x| !x.contains("<-"))
                .map(|fn_name| {
                    format!(
                        "{} <- function(x, ...) UseMethod('{}', x)",
                        fn_name,
                        fn_name.replace('`', "")
                    )
                })
                .collect::<Vec<_>>()
                .join("\n");

            print_step("generic_functions.R");
            println!("{}", generic_functions);

            print_step("types.R");
            println!("{}", type_annotations);
        }

        if opts.write_json {
            write_context_json(&final_context, &PathBuf::from("."));
            println!("  → context.json written");
        }
    }
}

/// Phase C of soundness_transpilation.md: static base-R/S4 name-collision
/// lint, run right before `R/*.R` is written. `generated_r` is the
/// transpiled program body (used to detect a user-supplied `<name>.default`
/// fallback alongside typr's own `std.R`). Returns whether any error-level
/// finding was reported (callers abort the build in that case).
fn lint_r_names(context: &Context, generated_r: &str, strict_mode: bool) -> bool {
    let stub_names = context
        .get_all_generic_functions()
        .iter()
        .map(|(var, _)| var.get_name().replace('`', ""))
        .filter(|x| !x.contains("<-"))
        .collect::<Vec<_>>();
    let ctor_names = context
        .record_aliases
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<Vec<_>>();

    let mut findings = crate::r_name_lint::lint_generic_stub_names(&stub_names, generated_r, strict_mode);
    findings.extend(crate::r_name_lint::lint_record_constructor_names(&ctor_names));
    crate::r_name_lint::report_findings(&findings)
}

pub fn write_header(context: Context, output_dir: &Path, environment: Environment) {
    let type_anotations = context.get_type_anotations();
    let c_types_include = if environment.is_project() {
        "#' @include generic_functions.R\n"
    } else {
        ""
    };
    let types_content = format!("{}{}", c_types_include, type_anotations);
    match environment {
        Environment::Repl => {
            let mut app = OpenOptions::new()
                .append(true)
                .create(true)
                .open(output_dir.join(".repl.R"))
                .unwrap();
            app.write_all(types_content.as_bytes()).unwrap();
        }
        _ => {
            let path = output_dir
                .join(context.get_environment().to_base_path())
                .join("types.R");
            cache::write_if_changed(&path, &types_content).unwrap();
        }
    }

    let include_tag = if environment.is_project() {
        "#' @include std.R\n"
    } else {
        ""
    };
    let generic_functions = context
        .get_all_generic_functions()
        .iter()
        .map(|(var, _)| var.get_name())
        .filter(|x| !x.contains("<-"))
        .map(|fn_name| {
            let export = if environment.is_project() { "#' @export\n" } else { "" }.to_string();
            format!(
                "{}{} <- function(x, ...) UseMethod('{}', x)",
                export,
                fn_name,
                fn_name.replace("`", "")
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let generic_content = format!("{}{}\n", include_tag, generic_functions);
    match environment {
        Environment::Repl => {
            let mut app = OpenOptions::new()
                .append(true)
                .create(true)
                .open(output_dir.join(".repl.R"))
                .unwrap();
            app.write_all(generic_content.as_bytes()).unwrap();
        }
        _ => {
            let path = output_dir
                .join(context.get_environment().to_string())
                .join("generic_functions.R");
            cache::write_if_changed(&path, &generic_content).unwrap();
        }
    }
}

/// Write the TypR project loader (load_module.R) to the project root.
/// Called by build_project so `typr run` can use it without devtools.
pub fn write_loader(project_root: &Path) {
    let loader = include_str!("../configs/src/load_module.R");
    let dest = project_root.join("load_module.R");
    cache::write_if_changed(&dest, loader).expect("Cannot write load_module.R");
}

pub fn write_to_r_lang(content: String, output_dir: &Path, file_name: &str, environment: Environment) {
    let rstd = include_str!("../configs/src/std.R");
    let std_path = output_dir.join("std.R");
    cache::write_if_changed(&std_path, rstd).unwrap();

    let app_path = output_dir.join(file_name);
    let preamble = match environment {
        Environment::Project => {
            // Top-level `mod foo;` deps collected during transpilation surface here
            // as `@include` tags in main.R's header so roxygen2 can order Collate.
            let main_includes = typr_core::processes::transpiling::take_main_includes()
                .iter()
                .map(|f| format!("#' @include {}\n", f))
                .collect::<String>();
            let main_imports = typr_core::processes::transpiling::take_main_import_froms()
                .iter()
                .map(|e| format!("#' @importFrom {}\n", e))
                .collect::<String>();
            format!(
                "#' @include std.R\n#' @include generic_functions.R\n#' @include types.R\n{}{}",
                main_includes, main_imports
            )
        }
        Environment::Repl | Environment::Wasm => String::new(),
        Environment::StandAlone => {
            "source('std.R', echo = FALSE)\nsource('generic_functions.R')\nsource('types.R')\n".to_string()
        }
    };
    let full_content = format!("{}{}", preamble, content);
    match environment {
        Environment::Repl => {
            let mut app = OpenOptions::new().append(true).create(true).open(app_path).unwrap();
            app.write_all(full_content.as_bytes()).unwrap();
        }
        _ => {
            cache::write_if_changed(&app_path, &full_content).unwrap();
        }
    }
}

pub fn new(name: &str, renv: bool) {
    println!("Creating the R package '{}'...", name);

    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("Error obtaining current directory: {}", e);
            std::process::exit(1);
        }
    };

    let project_path = current_dir.join(name);

    if let Err(e) = fs::create_dir(&project_path) {
        eprintln!("Error creating project directory: {}", e);
        std::process::exit(1);
    }

    // Classic architecture of a R package
    let package_folders = vec![
        "R",         // R code
        "TypR",      // TypR code
        "man",       // Documentation
        "tests",     // Tests
        "data",      // Data
        "inst",      // Installed files
        "src",       // Source code (C++, Fortran, etc.)
        "vignettes", // Vignettes/tutorials
    ];

    for folder in package_folders {
        let folder_path = project_path.join(folder);
        if let Err(e) = fs::create_dir(&folder_path) {
            eprintln!("Warning: Unable to create the folder {}: {}", folder_path.display(), e);
        }
    }

    let tests_testthat = project_path.join("tests/testthat");
    if let Err(e) = fs::create_dir(&tests_testthat) {
        eprintln!("Warning: Unable to create the tests/testthat folder: {}", e);
    }

    let package_files = vec![
        (
            "DESCRIPTION",
            include_str!("../configs/DESCRIPTION").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "NAMESPACE",
            include_str!("../configs/NAMESPACE").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            ".Rbuildignore",
            include_str!("../configs/.Rbuildignore").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            ".gitignore",
            include_str!("../configs/.gitignore").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "TypR/main.ty",
            include_str!("../configs/main.ty").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "R/.gitkeep",
            include_str!("../configs/.gitkeep").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "tests/testthat.R",
            include_str!("../configs/testthat.R").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "man/.gitkeep",
            include_str!("../configs/.gitkeep2").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "README.md",
            include_str!("../configs/README.md").replace("{{PACKAGE_NAME}}", name),
        ),
        ("rproj.Rproj", include_str!("../configs/rproj.Rproj").to_string()),
    ];

    for (file_path, content) in package_files {
        let full_path = project_path.join(file_path);
        if let Some(parent) = full_path.parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                eprintln!("Warning: Unable to create parent directory {}: {}", parent.display(), e);
                continue;
            }
        }
        println!("Writing {} in '{:?}'", content.len(), full_path);
        if let Err(e) = fs::write(&full_path, content) {
            eprintln!(
                "Warning: Unable to create parent directory {}: {}",
                full_path.display(),
                e
            );
        }
    }

    println!("Package R '{}' successfully created!", name);

    if renv {
        renv_init(&project_path);
    }

    let package_structure = include_str!("../configs/package_structure.md").replace("{{PACKAGE_NAME}}", name);
    println!("{}", package_structure);

    let instructions = include_str!("../configs/instructions.md").replace("{{PACKAGE_NAME}}", name);
    println!("{}", instructions);
}

pub fn check_project() {
    let context = Context::default().set_environment(Environment::Project);

    let step = Step::new("Parsing");
    let (lang, syntax_errors) = parse_code(&PathBuf::from("TypR/main.ty"), context.get_environment());
    if report_syntax_errors(syntax_errors) {
        step.fail();
        std::process::exit(1);
    }
    step.done();

    let step = Step::new("Type checking");
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    if type_checker.has_errors() {
        step.fail();
        eprintln!("Type errors found:");
        type_checker.show_errors();
        std::process::exit(1);
    }
    step.done();
    println!("Code verification successful!");
}

pub fn check_file(path: &PathBuf) {
    let context = Context::default().set_environment(Environment::Project);
    let dir = PathBuf::from(".");
    write_std_for_type_checking(&dir);

    let step = Step::new("Parsing");
    let (lang, syntax_errors) = parse_code(path, context.get_environment());
    if report_syntax_errors(syntax_errors) {
        step.fail();
        std::process::exit(1);
    }
    step.done();

    let step = Step::new("Type checking");
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    write_context_json(&type_checker.get_context(), &dir);
    if type_checker.has_errors() {
        step.fail();
        eprintln!("Type errors found:");
        type_checker.show_errors();
        std::process::exit(1);
    }
    step.done();
    println!("File verification {:?} successful!", path);
}

/// Build a ROxygen2 comment block for a function node from the SPG.
/// Returns an empty string if there is nothing worth emitting.
fn roxygen_function_block(doc: Option<&str>, params: &[(String, String)], returns: &str) -> String {
    let mut lines: Vec<String> = Vec::new();
    if let Some(d) = doc {
        let mut it = d.lines();
        if let Some(title) = it.next() {
            let t = title.trim();
            if !t.is_empty() {
                lines.push(format!("#' {}", t));
            }
        }
        let rest: Vec<&str> = it.map(str::trim).filter(|l| !l.is_empty()).collect();
        if !rest.is_empty() {
            lines.push("#'".to_string());
            for l in rest {
                lines.push(format!("#' {}", l));
            }
        }
        if !lines.is_empty() {
            lines.push("#'".to_string());
        }
    }
    for (name, ty) in params {
        lines.push(format!("#' @param {} \\code{{{}}}", name, ty));
    }
    if !returns.is_empty() && returns != "Empty" {
        lines.push(format!("#' @return \\code{{{}}}", returns));
    }
    lines.join("\n")
}

/// Build (pattern, replacement) pairs for ROxygen2 injection from the SPG.
///
/// Three patterns per function node:
/// - P2: S3 method     `#' @export\n#' @method name Class`  (typed functions)
/// - P1: plain fn      `#' @export\nname <- function`        (untyped / Empty)
/// - P3: re-export     `#' @export\nname <- Module$name`     (inline module @export)
///
/// P1/P2 match in main.R (top-level) and in external module R files.
/// P3 matches inline-module re-exports in main.R; is a no-op in module files.
fn build_roxygen_entries(spg: &Spg) -> Vec<(String, String)> {
    spg.nodes
        .iter()
        .filter(|n| n.visibility != Visibility::Private)
        .filter_map(|node| {
            let block = match &node.payload {
                NodePayload::Function { params, returns } => {
                    roxygen_function_block(node.doc.as_deref(), params, returns)
                }
                _ => return None,
            };
            if block.is_empty() {
                return None;
            }
            let name = &node.name;
            let mut entries: Vec<(String, String)> = Vec::new();
            entries.push((
                format!("#' @export\n#' @method {} ", name),
                format!("{}\n#' @export\n#' @method {} ", block, name),
            ));
            entries.push((
                format!("#' @export\n{} <- function", name),
                format!("{}\n#' @export\n{} <- function", block, name),
            ));
            if !node.module_path.is_empty() {
                let mod_path = node.module_path.join("$");
                entries.push((
                    format!("#' @export\n{} <- {}${}", name, mod_path, name),
                    format!("{}\n#' @export\n{} <- {}${}", block, name, mod_path, name),
                ));
            }
            Some(entries)
        })
        .flatten()
        .collect()
}

/// Apply (pattern, replacement) pairs to `content`.
fn inject_roxygen_headers(content: String, entries: &[(String, String)]) -> String {
    if entries.is_empty() {
        return content;
    }
    let mut result = content;
    for (pattern, replacement) in entries {
        result = result.replace(pattern.as_str(), replacement.as_str());
    }
    result
}

/// Apply injection to every `R/*.R` file written as a side-effect during transpilation.
/// Skips the fixed generated files (std.R, generic_functions.R, types.R, main.R).
fn inject_roxygen_into_module_files(r_dir: &Path, entries: &[(String, String)]) {
    if entries.is_empty() {
        return;
    }
    const SKIP: &[&str] = &["std.R", "generic_functions.R", "types.R", "main.R"];
    let Ok(read_dir) = fs::read_dir(r_dir) else {
        return;
    };
    for dir_entry in read_dir.flatten() {
        let path = dir_entry.path();
        let Some(file_name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        if !file_name.ends_with(".R") || SKIP.contains(&file_name) {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        let injected = inject_roxygen_headers(content.clone(), entries);
        if injected != content {
            let _ = fs::write(&path, injected);
        }
    }
}

pub fn build_project(test_mode: bool, no_incremental: bool, checked_mode: bool, strict_mode: bool) {
    build_project_impl(test_mode, checked_mode, strict_mode, false, false, !no_incremental);
}

fn build_project_impl(
    test_mode: bool,
    checked_mode: bool,
    strict_mode: bool,
    quiet: bool,
    skip_document: bool,
    incremental: bool,
) {
    let dir = PathBuf::from(".");

    // Whole-project short-circuit: when every source and generated file
    // still matches the manifest of the last successful build, there is
    // nothing to do. For a full build (with document step) we additionally
    // require that devtools::document succeeded at least once, so a project
    // only ever built via `typr run` still gets its NAMESPACE updated.
    if incremental {
        if let Some(manifest) = cache::load_manifest(&dir) {
            let document_done = skip_document || manifest.devtools_input_hash.is_some();
            if manifest.is_compatible(test_mode, checked_mode) && document_done && manifest.is_up_to_date(&dir) {
                if !quiet {
                    println!("Project up to date — nothing to rebuild.");
                }
                return;
            }
        }
    }

    let context = Context::default()
        .set_environment(Environment::Project)
        .set_test_mode(test_mode)
        .set_checked_mode(checked_mode);

    let step = Step::new("Parsing");
    let (lang, mut expansion_info) = parse_code_with_info(&PathBuf::from("TypR/main.ty"), context.get_environment());
    if report_syntax_errors(std::mem::take(&mut expansion_info.syntax_errors)) {
        step.fail();
        std::process::exit(1);
    }
    step.done();

    // Per-module type-check cache: unchanged modules (same source + deps +
    // upstream context) are replayed from .typr_cache/modules/ instead of
    // being re-typed. See typr-core's module_cache for the replay semantics.
    let module_store = if incremental {
        let store = cache::FsModuleStore::new(dir.join(cache::CACHE_DIR).join("modules"));
        typr_core::processes::type_checking::module_cache::install(
            Box::new(store.clone()),
            cache::module_cache_salt(test_mode, checked_mode),
            cache::combined_module_hashes(&expansion_info),
        );
        Some(store)
    } else {
        None
    };

    let step = Step::new("Type checking");
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    if type_checker.has_errors() {
        step.fail();
        type_checker.show_errors();
    } else {
        step.done();
    }

    if let Some(store) = &module_store {
        let (hits, misses) = typr_core::processes::type_checking::module_cache::uninstall();
        if !quiet && hits + misses > 0 {
            println!("  Modules: {} from cache, {} type-checked", hits, misses);
        }
        store.gc();
    }

    // Build the SPG once, with the real package name/version, and reuse it
    // for both roxygen injection and the document step (which used to
    // re-parse and re-type-check the whole project a second time).
    let package = get_package_name().unwrap_or_else(|_| "unknown".to_string());
    let version = get_package_version().unwrap_or_else(|_| "0.0.0".to_string());
    let items: Vec<Lang> = type_checker.get_code().iter().cloned().collect();
    let spg = build_spg_from_items(&items, &package, &version);
    let roxygen_entries = build_roxygen_entries(&spg);

    let step = Step::new("Transpiling");
    typr_core::processes::transpiling::reset_include_stack();
    typr_core::processes::transpiling::reset_import_from_stack();
    let content = type_checker.clone().transpile();
    let content = inject_roxygen_headers(content, &roxygen_entries);
    inject_roxygen_into_module_files(&PathBuf::from("R"), &roxygen_entries);
    step.done();

    if lint_r_names(&type_checker.get_context(), &content, strict_mode) {
        std::process::exit(1);
    }

    let step = Step::new("Writing R files");
    write_header(type_checker.get_context(), &dir, Environment::Project);
    write_to_r_lang(content, &PathBuf::from("R"), "main.R", context.get_environment());
    write_loader(&dir);
    step.done();

    // Carry the last successful devtools run forward so the document step
    // can skip the R subprocess when its inputs did not change.
    let mut manifest = cache::BuildManifest::new(test_mode, checked_mode);
    manifest.source_hashes = expansion_info.files.clone();
    if let Some(previous) = cache::load_manifest(&dir) {
        if previous.is_compatible(test_mode, checked_mode) {
            manifest.devtools_input_hash = previous.devtools_input_hash;
        }
    }

    if !skip_document {
        let manifest_ref = if incremental { Some(&mut manifest) } else { None };
        document_from_spg(&spg, quiet, manifest_ref);
    }

    if incremental && !type_checker.has_errors() {
        manifest.output_hashes = cache::collect_output_hashes(&dir);
        cache::save_manifest(&dir, &manifest);
    }

    if !quiet {
        println!("R code successfully generated in the R/ folder");
    }
}

pub fn build_file(path: &Path, test_mode: bool, checked_mode: bool, strict_mode: bool) {
    let dir = PathBuf::from(".");
    write_std_for_type_checking(&dir);

    let step = Step::new("Parsing");
    let (lang, syntax_errors) = parse_code(path, Environment::StandAlone);
    if report_syntax_errors(syntax_errors) {
        step.fail();
        std::process::exit(1);
    }
    step.done();

    let context = Context::default()
        .set_test_mode(test_mode)
        .set_checked_mode(checked_mode);

    let step = Step::new("Type checking");
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    step.done();

    let r_file_name = path.file_name().unwrap().to_str().unwrap().replace(".ty", ".R");

    let step = Step::new("Transpiling");
    let content = type_checker.clone().transpile();
    step.done();

    if lint_r_names(&type_checker.get_context(), &content, strict_mode) {
        std::process::exit(1);
    }

    let step = Step::new("Writing R files");
    write_header(type_checker.get_context(), &dir, Environment::StandAlone);
    write_to_r_lang(content, &dir, &r_file_name, context.get_environment());
    step.done();

    println!("Generated R code: {:?}", dir.join(&r_file_name));
}

/// Wraps an R expression string with Rprof sampling and prints a summary table.
fn rprof_wrap(r_body: &str) -> String {
    format!(
        concat!(
            "prof_file <- tempfile(fileext = '.Rprof'); ",
            "Rprof(prof_file, interval = 0.01, memory.profiling = FALSE); ",
            "{body}; ",
            "Rprof(NULL); ",
            "p <- tryCatch(summaryRprof(prof_file), error = function(e) NULL); ",
            "if (is.null(p) || nrow(p$by.self) == 0L) {{ ",
            "  cat('\\n[profile] No data collected (program ran too fast).\\n') ",
            "}} else {{ ",
            "  cat('\\n=== TypR Profile: top functions by self time ===\\n'); ",
            "  by_self <- p$by.self[order(-p$by.self$self.time), , drop = FALSE]; ",
            "  print(head(by_self, 30)); ",
            "  cat(sprintf('\\nTotal sampled time: %.3f s  (interval = 10 ms)\\n', ",
            "              p$sampling.interval * nrow(p$by.self))) ",
            "}}"
        ),
        body = r_body
    )
}

pub fn run_project(profile: bool, checked_mode: bool, strict_mode: bool) {
    build_project_impl(false, checked_mode, strict_mode, true, true, true);
    // Use the TypR loader instead of devtools to respect module encapsulation.
    // Top-level code in main.ty already runs as a side effect of sourcing it
    // (sys.source() inside load_module()). The `module Main { @pub let main
    // <- fn() {...} }` convention is optional: if it's present we call it as
    // the entry point, but a plain main.ty with top-level statements works
    // out of the box with no boilerplate required.
    let base_command = concat!(
        "source('load_module.R'); ",
        "modules <- load_module('.'); ",
        "if (exists('Main', envir = modules, inherits = FALSE)) { ",
        "  .typr_main_env <- get('Main', envir = modules); ",
        "  if (is.environment(.typr_main_env) && ",
        "      exists('main', envir = .typr_main_env, inherits = FALSE)) { ",
        "    .typr_main_fn <- get('main', envir = .typr_main_env); ",
        "    if (is.function(.typr_main_fn)) invisible(.typr_main_fn()); ",
        "  } ",
        "}"
    );
    let r_command = if profile {
        rprof_wrap(base_command)
    } else {
        base_command.to_string()
    };
    let step = Step::new("Running");
    match Command::new("Rscript").arg("-e").arg(&r_command).output() {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            if output.status.success() {
                step.clear();
                if !stdout.is_empty() {
                    print!("{}", stdout);
                }
            } else {
                step.fail();
                eprintln!("Error (code {}):\n{}", output.status, stderr);
                if !stdout.is_empty() {
                    print!("{}", stdout);
                }
                std::process::exit(1);
            }
        }
        Err(e) => {
            step.fail();
            eprintln!("Failed to execute command: {}", e);
        }
    }
}

fn strip_shebang(content: &str) -> &str {
    if content.starts_with("#!") {
        content.find('\n').map_or("", |i| &content[i + 1..])
    } else {
        content
    }
}

pub fn run_file(path: &Path) {
    run_file_impl(path, false, false, false, false);
}

pub fn run_file_keep(path: &Path, profile: bool, checked_mode: bool, strict_mode: bool) {
    run_file_impl(path, true, profile, checked_mode, strict_mode);
}

/// RAII guard that removes a temporary directory when dropped.
struct TempDirGuard {
    path: Option<PathBuf>,
}

impl TempDirGuard {
    fn new(path: Option<PathBuf>) -> Self {
        Self { path }
    }

    /// Drop the guard (clean up temp dir) and terminate.
    fn fail(self) -> ! {
        let _ = self;
        std::process::exit(1);
    }
}

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        if let Some(path) = self.path.take() {
            let _ = fs::remove_dir_all(&path);
        }
    }
}

fn run_file_impl(path: &Path, keep_files: bool, profile: bool, checked_mode: bool, strict_mode: bool) {
    let step = Step::new("Parsing");
    let content = get_content(path);
    let file_name = path.to_str().unwrap().to_string();
    let (lang, syntax_errors) = parse_code_from_str(&content, &file_name, Environment::StandAlone);
    if report_syntax_errors(syntax_errors) {
        step.fail();
        std::process::exit(1);
    }
    step.done();

    let step = Step::new("Type checking");
    let work_dir = get_working_directory(keep_files);
    let guard = TempDirGuard::new(if keep_files { None } else { Some(work_dir.clone()) });
    write_std_for_type_checking(&work_dir);
    let context = Context::default().set_checked_mode(checked_mode);
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    if type_checker.has_errors() {
        step.fail();
        type_checker.show_errors();
        guard.fail();
    }
    step.done();

    let step = Step::new("Transpiling");
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("script");
    let r_file_name = format!("{}.R", stem);

    // Change CWD to work_dir before transpiling so that write_output_file
    // (used for external module .R files) writes relative to work_dir,
    // matching where Rscript will look for source() calls.
    if !keep_files {
        if let Err(e) = std::env::set_current_dir(&work_dir) {
            eprintln!("Cannot set working directory: {}", e);
            guard.fail();
        }
    }
    let r_content = type_checker.clone().transpile();
    step.done();

    let step = Step::new("Writing R files");
    if lint_r_names(&type_checker.get_context(), &r_content, strict_mode) {
        guard.fail();
    }
    write_header(type_checker.get_context(), &work_dir, Environment::StandAlone);
    write_to_r_lang(r_content, &work_dir, &r_file_name, context.get_environment());
    step.done();

    let step = Step::new("Running");
    let r_path = work_dir.join(&r_file_name);
    let result = run_script(profile, &work_dir, r_path);

    match result {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            if output.status.success() {
                step.clear();
                if !stdout.is_empty() {
                    print!("{}", stdout);
                }
            } else {
                step.fail();
                eprintln!("Error (code {}):\n{}", output.status, stderr);
                if !stdout.is_empty() {
                    print!("{}", stdout);
                }
                guard.fail();
            }
        }
        Err(e) => {
            step.fail();
            eprintln!("Failed to execute Rscript: {}", e);
        }
    }
}

fn get_working_directory(keep_files: bool) -> PathBuf {
    let work_dir = if keep_files {
        PathBuf::from(".")
    } else {
        let tmp = std::env::temp_dir().join(format!("typr_{}", std::process::id()));
        fs::create_dir_all(&tmp).unwrap();
        tmp
    };
    work_dir
}

fn get_content(path: &Path) -> String {
    let raw = fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Cannot read {:?}: {}", path, e);
        std::process::exit(1);
    });
    let content = strip_shebang(&raw);
    content.to_string()
}

fn run_script(profile: bool, work_dir: &PathBuf, r_path: PathBuf) -> Result<std::process::Output, std::io::Error> {
    if profile {
        let r_path_str = r_path.to_str().unwrap_or("script.R");
        let profile_script = format!(
            r#"
prof_file <- tempfile(fileext = ".Rprof")
Rprof(prof_file, interval = 0.005, memory.profiling = FALSE)
source({r_path_str:?})
Rprof(NULL)
p <- tryCatch(summaryRprof(prof_file), error = function(e) NULL)
if (is.null(p) || nrow(p$by.self) == 0L) {{
  cat("\n[profile] No data collected (program too fast or no R activity sampled).\n")
}} else {{
  cat("\n=== TypR Profile: top functions by self time ===\n")
  by_self <- p$by.self[order(-p$by.self$self.time), , drop = FALSE]
  print(head(by_self, 30))
  total <- p$sampling.interval * sum(p$by.self$self.time / p$sampling.interval)
  cat(sprintf("\nTotal sampled time: %.3f s  (interval = %.0f ms)\n",
              p$sampling.interval * nrow(p$by.self), p$sampling.interval * 1000))
}}
"#,
            r_path_str = r_path_str
        );
        Command::new("Rscript")
            .current_dir(work_dir)
            .arg("-e")
            .arg(&profile_script)
            .output()
    } else {
        Command::new("Rscript").current_dir(work_dir).arg(&r_path).output()
    }
}

fn write_context_json(context: &Context, output_dir: &Path) {
    let json = serde_json::to_string_pretty(context).expect("Failed to serialize context to JSON");
    let path = output_dir.join("context.json");
    let mut file = File::create(&path).expect("Failed to create context.json");
    file.write_all(json.as_bytes()).expect("Failed to write context.json");
}

pub fn test(profile: bool) {
    build_project(true, false, false, false);
    // Load modules in test mode so @testable members are accessible in test files.
    // Then delegate test discovery to devtools::test().
    let base_command = concat!(
        "source('load_module.R'); ",
        "modules <- load_module('.', test = TRUE); ",
        "devtools::test()"
    );
    let r_command = if profile {
        rprof_wrap(base_command)
    } else {
        base_command.to_string()
    };

    println!("Execution of: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(&r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("Error while running tests");
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Error while executing R command: {}", e);
            eprintln!("Make sure devtools is installed");
            std::process::exit(1);
        }
    }
}

pub fn get_package_name() -> Result<String, String> {
    let description_path = PathBuf::from("DESCRIPTION");

    if !description_path.exists() {
        return Err("DESCRIPTION file not found. Are you at the project root?".to_string());
    }

    let content =
        fs::read_to_string(&description_path).map_err(|e| format!("Error reading file DESCRIPTION: {}", e))?;

    for line in content.lines() {
        if line.starts_with("Package:") {
            let package_name = line.replace("Package:", "").trim().to_string();
            return Ok(package_name);
        }
    }

    Err("Package name not found in the DESCRIPTION file".to_string())
}

pub fn get_package_version() -> Result<String, String> {
    let description_path = PathBuf::from("DESCRIPTION");

    if !description_path.exists() {
        return Err("DESCRIPTION file not found. Are you at the project root?".to_string());
    }

    let content =
        fs::read_to_string(&description_path).map_err(|e| format!("Error reading file DESCRIPTION: {}", e))?;

    for line in content.lines() {
        if line.starts_with("Version:") {
            return Ok(line.replace("Version:", "").trim().to_string());
        }
    }

    Err("Version not found in the DESCRIPTION file".to_string())
}

/// Generate a Semantic Package Graph (`spg.json`) from the current project.
pub fn generate_spg(output: Option<PathBuf>) {
    let context = Context::default().set_environment(Environment::Project);

    let step = Step::new("Parsing");
    let (lang, syntax_errors) = parse_code(&PathBuf::from("TypR/main.ty"), context.get_environment());
    if report_syntax_errors(syntax_errors) {
        step.fail();
        std::process::exit(1);
    }
    step.done();

    let step = Step::new("Type checking");
    let type_checker = TypeChecker::new(context).typing_no_panic(&lang);
    if type_checker.has_errors() {
        step.fail();
        type_checker.show_errors();
    } else {
        step.done();
    }

    let package = get_package_name().unwrap_or_else(|_| "unknown".to_string());
    let version = get_package_version().unwrap_or_else(|_| "0.0.0".to_string());

    let step = Step::new("Building semantic graph");
    let typed_items: Vec<Lang> = type_checker.get_code().iter().cloned().collect();
    let spg = build_spg_from_items(&typed_items, &package, &version);
    step.done();

    let out_path = output.unwrap_or_else(|| PathBuf::from("spg.json"));
    let json = serde_json::to_string_pretty(&spg).expect("Failed to serialize SPG");
    let mut file = File::create(&out_path).expect("Failed to create SPG output file");
    file.write_all(json.as_bytes()).expect("Failed to write SPG file");
    println!("Semantic graph written to {:?}", out_path);
}

pub fn renv_init(project_path: &std::path::Path) {
    println!("Initializing renv...");

    let r_command = "renv::init()";
    println!("Executing: R -e \"{}\"", r_command);

    let output = Command::new("R")
        .arg("-e")
        .arg(r_command)
        .current_dir(project_path)
        .output();

    match output {
        Ok(output) => {
            if output.status.success() {
                println!("renv initialized successfully!");

                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("Warning: renv initialization may have failed");
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }
            }
        }
        Err(e) => {
            eprintln!("Warning: Could not execute renv::init(): {}", e);
            eprintln!("Make sure that renv is installed (install.packages('renv')).");
        }
    }
}

pub fn renv_snapshot() {
    println!("Taking renv snapshot...");

    let r_command = "renv::snapshot()";
    println!("Executing: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                println!("renv snapshot saved successfully!");

                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("Warning: renv snapshot may have failed");
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }
            }
        }
        Err(e) => {
            eprintln!("Warning: Could not execute renv::snapshot(): {}", e);
        }
    }
}

fn is_renv_active() -> bool {
    std::env::current_dir()
        .map(|d| d.join("renv.lock").exists())
        .unwrap_or(false)
}

pub fn pkg_install(packages: Option<&[String]>) {
    match packages {
        Some(pkgs) if !pkgs.is_empty() => {
            let renv = is_renv_active();

            let r_command = if renv {
                println!("Installing packages via renv: {:?}", pkgs);
                let pkgs_str = pkgs.iter().map(|p| format!("'{}'", p)).collect::<Vec<_>>().join(", ");
                format!("renv::install(c({}))", pkgs_str)
            } else {
                println!("Installing packages from CRAN: {:?}", pkgs);
                let pkgs_str = pkgs.iter().map(|p| format!("'{}'", p)).collect::<Vec<_>>().join(", ");
                format!("install.packages(c({}))", pkgs_str)
            };
            println!("Executing: R -e \"{}\"", r_command);

            let output = Command::new("R").arg("-e").arg(&r_command).output();

            match output {
                Ok(output) => {
                    if output.status.success() {
                        println!("Package(s) installed successfully!");

                        if !output.stdout.is_empty() {
                            println!("\n{}", String::from_utf8_lossy(&output.stdout));
                        }

                        if !renv {
                            renv_snapshot();
                        }
                    } else {
                        eprintln!("Error during package installation");
                        if !output.stderr.is_empty() {
                            eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                        }
                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    eprintln!("Error executing R command: {}", e);
                    std::process::exit(1);
                }
            }
        }
        _ => {
            println!("Installing the local package...");

            let current_dir = match std::env::current_dir() {
                Ok(dir) => dir,
                Err(e) => {
                    eprintln!("Error obtaining current directory: {}", e);
                    std::process::exit(1);
                }
            };

            let project_path = current_dir.to_str().unwrap();
            let r_command = format!("devtools::install_local('{}')", project_path);
            println!("Executing: R -e \"{}\"", r_command);

            let output = Command::new("R").arg("-e").arg(&r_command).output();

            match output {
                Ok(output) => {
                    if output.status.success() {
                        println!("Package installed successfully!");

                        if !output.stdout.is_empty() {
                            println!("\n{}", String::from_utf8_lossy(&output.stdout));
                        }
                    } else {
                        eprintln!("Error during package installation");
                        if !output.stderr.is_empty() {
                            eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                        }

                        std::process::exit(1);
                    }
                }
                Err(e) => {
                    eprintln!("Error executing command R: {}", e);
                    eprintln!("Make sure that R and devtools are installed.");
                    std::process::exit(1);
                }
            }
        }
    }
}

pub fn pkg_uninstall() {
    println!("Uninstalling the package...");

    let package_name = match get_package_name() {
        Ok(name) => name,
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    };

    println!("Uninstalling the package '{}'...", package_name);
    let r_command = format!("remove.packages('{}')", package_name);
    println!("Executing: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(&r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                println!("Package '{}' successfully uninstalled!", package_name);

                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!(
                    "Note: The package '{}' may not have been installed or an error may have occurred",
                    package_name
                );

                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }
            }
        }
        Err(e) => {
            eprintln!("Error executing command R: {}", e);
            eprintln!("Make sure that R is installed.");
            std::process::exit(1);
        }
    }
}

pub fn document() {
    document_impl(false);
}

pub fn pkgdown() {
    document_impl(true);

    let step = Step::new("Building pkgdown site");
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("Error obtaining current directory: {}", e);
            std::process::exit(1);
        }
    };
    let project_path = current_dir.to_str().unwrap_or(".");
    let r_command = format!("pkgdown::build_site('{}')", project_path);
    let output = Command::new("R").arg("-e").arg(&r_command).output();
    match output {
        Ok(out) if out.status.success() => {
            step.done();
            println!("  pkgdown site written to docs/");
        }
        Ok(out) => {
            step.fail();
            eprintln!("Error building pkgdown site.");
            if !out.stderr.is_empty() {
                eprintln!("{}", String::from_utf8_lossy(&out.stderr));
            }
            std::process::exit(1);
        }
        Err(_) => {
            step.fail();
            eprintln!("R not found — cannot build pkgdown site.");
            std::process::exit(1);
        }
    }
}

fn document_impl(quiet: bool) {
    // Step 1: Parse + type-check the project to build the SPG.
    let context = Context::default().set_environment(Environment::Project);

    let step = Step::new("Parsing");
    let (lang, syntax_errors) = parse_code(&PathBuf::from("TypR/main.ty"), context.get_environment());
    if report_syntax_errors(syntax_errors) {
        step.fail();
        std::process::exit(1);
    }
    step.done();

    let step = Step::new("Type checking");
    let type_checker = TypeChecker::new(context).typing_no_panic(&lang);
    if type_checker.has_errors() {
        step.fail();
        type_checker.show_errors();
        std::process::exit(1);
    }
    step.done();

    let package = get_package_name().unwrap_or_else(|_| "unknown".to_string());
    let version = get_package_version().unwrap_or_else(|_| "0.0.0".to_string());
    let items: Vec<Lang> = type_checker.get_code().iter().cloned().collect();

    let step = Step::new("Building semantic graph");
    let spg = build_spg_from_items(&items, &package, &version);
    step.done();

    // No manifest: an explicit `typr document` always runs devtools.
    document_from_spg(&spg, quiet, None);
}

/// The document pipeline from an already-built SPG: .Rd generation,
/// vignettes, then NAMESPACE/Collate via devtools. `typr build` calls this
/// with its own SPG (avoiding a second parse + type-check of the project)
/// and with the build manifest, which lets the devtools subprocess be
/// skipped when its inputs are unchanged since the last successful run.
fn document_from_spg(spg: &Spg, quiet: bool, manifest: Option<&mut cache::BuildManifest>) {
    // Step 2: Ensure man/ directory exists.
    let man_dir = PathBuf::from("man");
    if let Err(e) = fs::create_dir_all(&man_dir) {
        eprintln!("Error creating man/ directory: {}", e);
        std::process::exit(1);
    }

    // Step 3: Generate .Rd files from the SPG.
    let step = Step::new("Generating .Rd files");
    match crate::rd_renderer::generate_rd_files(spg, &man_dir) {
        Ok(count) => {
            step.done();
            if !quiet {
                println!("  {} .Rd file(s) written to man/", count);
            }
        }
        Err(e) => {
            step.fail();
            eprintln!("Error writing .Rd files: {}", e);
            std::process::exit(1);
        }
    }

    // Step 4: Generate SPG fragments and inject into vignette templates.
    let fragments_dir = PathBuf::from(".vignettes/spg_fragments");
    let vignettes_dir = PathBuf::from("vignettes");
    let compiled_dir = vignettes_dir.join("compiled");

    let step = Step::new("Generating vignette fragments");
    match crate::vignette_renderer::generate_spg_fragments(spg, &fragments_dir) {
        Ok(()) => step.done(),
        Err(e) => {
            step.fail();
            if !quiet {
                eprintln!("Warning: failed to write SPG fragments: {}", e);
            }
        }
    }

    let step = Step::new("Compiling vignette templates");
    match crate::vignette_renderer::process_vignette_templates(&fragments_dir, &vignettes_dir, &compiled_dir) {
        Ok(count) => {
            step.done();
            if !quiet && count > 0 {
                println!("  {} vignette(s) compiled to vignettes/compiled/", count);
            }
        }
        Err(e) => {
            step.fail();
            if !quiet {
                eprintln!("Warning: vignette template processing failed: {}", e);
            }
        }
    }

    // Step 5: Update NAMESPACE and Collate via devtools (skipping Rd generation
    // so our SPG-generated files are not overwritten).
    update_namespace(quiet, manifest);
}

/// Run `devtools::document(roclets = c('collate', 'namespace'))`. When a
/// build manifest is provided and the inputs of that run (`R/*.R` +
/// `DESCRIPTION`) hash to the same value as the last successful run, the R
/// subprocess is skipped entirely.
fn update_namespace(quiet: bool, manifest: Option<&mut cache::BuildManifest>) {
    let step = Step::new("Updating NAMESPACE");
    let input_hash = cache::devtools_input_hash(Path::new("."));
    if let Some(m) = &manifest {
        if input_hash.is_some() && m.devtools_input_hash == input_hash && Path::new("NAMESPACE").exists() {
            step.done();
            if !quiet {
                println!("  NAMESPACE up to date — devtools::document skipped");
            }
            return;
        }
    }
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("Error obtaining current directory: {}", e);
            std::process::exit(1);
        }
    };
    let project_path = current_dir.to_str().unwrap_or(".");
    let r_command = format!(
        "devtools::document('{}', roclets = c('collate', 'namespace'))",
        project_path
    );
    let output = Command::new("R").arg("-e").arg(&r_command).output();
    match output {
        Ok(out) if out.status.success() => {
            step.done();
            if let Some(m) = manifest {
                m.devtools_input_hash = input_hash;
            }
        }
        Ok(out) => {
            // devtools is optional; NAMESPACE may be stale but Rd files are ready.
            step.fail();
            if !quiet {
                eprintln!("Warning: NAMESPACE update failed (devtools/roxygen2 may not be installed).");
                // R routes progress messages to stderr and errors to stdout — show both.
                if !out.stdout.is_empty() {
                    eprintln!("{}", String::from_utf8_lossy(&out.stdout));
                }
                if !out.stderr.is_empty() {
                    eprintln!("{}", String::from_utf8_lossy(&out.stderr));
                }
            }
        }
        Err(_) => {
            step.fail();
            if !quiet {
                eprintln!("Warning: R not found — NAMESPACE not updated.");
            }
        }
    }
}

pub fn use_package(package_name: &str) {
    println!("Adding the package '{}' as a dependency...", package_name);
    let r_command = format!("usethis::use_package('{}')", package_name);
    println!("Execution of: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(&r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                println!("Package '{}' successfully added to dependencies!", package_name);

                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("Error adding package '{}'", package_name);

                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }

                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Error executing command R: {}", e);
            eprintln!("Make sure that R and usethis are installed.");
            std::process::exit(1);
        }
    }
}

pub fn load() {
    let r_command = "devtools::load_all('.')".to_string();

    println!("Execution of: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(&r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                println!("Elements loaded with success!");
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("Error while loading elements");
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }

                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Error while executing R command: {}", e);
            eprintln!("Make sure devtools is installed");
            std::process::exit(1);
        }
    }
}

pub fn cran() {
    let r_command = "devtools::check()".to_string();
    println!("Execution of: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(&r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                println!("Checks passed with success!");
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("Error while checking the project");
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }

                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Error while executing R command: {}", e);
            eprintln!("Make sure devtools is installed");
            std::process::exit(1);
        }
    }
}

pub fn clean() {
    let folder = Path::new(".");
    if folder.is_dir() {
        for entry_result in fs::read_dir(folder).unwrap() {
            let entry = entry_result.unwrap();
            let path = entry.path();
            if let Some(file_name) = path.file_name() {
                if let Some(str_name) = file_name.to_str() {
                    if str_name.starts_with(".") && path.is_file() {
                        let _ = fs::remove_file(&path);
                    }
                }
            }
        }
    };
    let _ = fs::remove_dir_all(cache::CACHE_DIR);
}
