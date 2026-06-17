//! Project management utilities for TypR CLI
//!
//! Provides functions for:
//! - Creating new projects
//! - Building and checking projects
//! - Running tests
//! - Package management

use crate::engine::{parse_code, parse_code_from_str, write_std_for_type_checking};
use std::fs;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use typr_core::components::context::config::Environment;
use typr_core::components::context::Context;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::processes::type_checking::type_checker::TypeChecker;

/// Options for the `typr debug` subcommand
#[derive(Debug, Clone)]
pub struct DebugOptions {
    pub show_ast: bool,
    pub show_types: bool,
    pub show_r: bool,
    pub write_json: bool,
    pub show_files: bool,
}

impl Default for DebugOptions {
    fn default() -> Self {
        DebugOptions {
            show_ast: false,
            show_types: false,
            show_r: false,
            write_json: false,
            show_files: false,
        }
    }
}

/// Render a pipeline step heading with consistent formatting
fn print_step(title: &str) {
    println!("\n{}", "─".repeat(60));
    println!("  {} {}", "▶", title);
    println!("{}", "─".repeat(60));
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
        Variable {
            name, is_opaque, ..
        } => {
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
        Lambda {
            parameters, body, ..
        } => {
            let params: Vec<String> = parameters.iter().map(|p| p.simple_print()).collect();
            format!(
                "{}fn({}) {{ ... }}\n{}",
                pad,
                params.join(", "),
                print_ast(body, indent + 1)
            )
        }
        FunctionApp {
            identifier,
            arguments,
            ..
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
            format!(
                "{}{}type {}{} <- {}",
                pad,
                pub_,
                name,
                params_str,
                target_type.pretty()
            )
        }
        Lines { value, .. } => {
            let items: Vec<String> = value.iter().map(|l| print_ast(l, indent + 1)).collect();
            format!("{}Sequence\n{}", pad, items.join("\n"))
        }
        Scope { body, .. } => {
            let items: Vec<String> = body.iter().map(|l| print_ast(l, indent + 1)).collect();
            format!("{}Scope\n{}", pad, items.join("\n"))
        }
        Operator {
            operator, rhs, lhs, ..
        } => {
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
        Match {
            target, branches, ..
        } => {
            let branches_str: Vec<String> = branches
                .iter()
                .map(|(pat, body)| {
                    format!(
                        "{}  {} =>\n{}",
                        pad,
                        print_ast(pat, 0),
                        print_ast(body, indent + 2)
                    )
                })
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
        ConstructorCall {
            type_name, fields, ..
        } => {
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
        let result = typr_core::processes::parsing::parse_from_string_with_errors(
            &source,
            &path.to_string_lossy(),
        );
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
        let ast =
            typr_core::processes::parsing::parse_from_string(&source, &path.to_string_lossy());
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
            let preamble =
                "source('std.R', echo = FALSE)\nsource('generic_functions.R')\nsource('types.R')";
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

pub fn write_header(context: Context, output_dir: &Path, environment: Environment) {
    let type_anotations = context.get_type_anotations();
    let c_types_include = if environment.is_project() {
        "#' @include generic_functions.R\n"
    } else {
        ""
    };
    let mut app = match environment {
        Environment::Repl => OpenOptions::new()
            .append(true)
            .create(true)
            .open(output_dir.join(".repl.R")),
        _ => OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(
                output_dir
                    .join(context.get_environment().to_base_path())
                    .join("types.R"),
            ),
    }
    .unwrap();

    app.write_all(format!("{}{}", c_types_include, type_anotations).as_bytes())
        .unwrap();

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
            let export = if environment.is_project() {
                "#' @export\n"
            } else {
                ""
            }
            .to_string();
            format!(
                "{}{} <- function(x, ...) UseMethod('{}', x)",
                export,
                fn_name,
                fn_name.replace("`", "")
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let mut app = match environment {
        Environment::Repl => OpenOptions::new()
            .append(true)
            .create(true)
            .open(output_dir.join(".repl.R")),
        _ => OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(
                output_dir
                    .join(context.get_environment().to_string())
                    .join("generic_functions.R"),
            ),
    }
    .unwrap();

    app.write_all(format!("{}{}\n", include_tag, generic_functions).as_bytes())
        .unwrap();
}

/// Write the TypR project loader (load_module.R) to the project root.
/// Called by build_project so `typr run` can use it without devtools.
pub fn write_loader(project_root: &Path) {
    let loader = include_str!("../configs/src/load_module.R");
    let dest = project_root.join("load_module.R");
    let mut f = File::create(&dest).expect("Cannot write load_module.R");
    f.write_all(loader.as_bytes())
        .expect("Cannot write load_module.R contents");
}

pub fn write_to_r_lang(
    content: String,
    output_dir: &Path,
    file_name: &str,
    environment: Environment,
) {
    let rstd = include_str!("../configs/src/std.R");
    let std_path = output_dir.join("std.R");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let app_path = output_dir.join(file_name);
    let mut app = match environment {
        Environment::Repl => OpenOptions::new().append(true).create(true).open(app_path),
        _ => File::create(app_path),
    }
    .unwrap();
    let preamble = match environment {
        Environment::Project => {
            // Top-level `mod foo;` deps collected during transpilation surface here
            // as `@include` tags in main.R's header so roxygen2 can order Collate.
            let main_includes = typr_core::processes::transpiling::take_main_includes()
                .iter()
                .map(|f| format!("#' @include {}\n", f))
                .collect::<String>();
            format!(
                "#' @include std.R\n#' @include generic_functions.R\n#' @include types.R\n{}",
                main_includes
            )
        }
        Environment::Repl | Environment::Wasm => String::new(),
        Environment::StandAlone => {
            "source('std.R', echo = FALSE)\nsource('generic_functions.R')\nsource('types.R')\n"
                .to_string()
        }
    };
    app.write_all(format!("{}{}", preamble, content).as_bytes())
        .unwrap();
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
            eprintln!(
                "Warning: Unable to create the folder {}: {}",
                folder_path.display(),
                e
            );
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
        (
            "rproj.Rproj",
            include_str!("../configs/rproj.Rproj").to_string(),
        ),
    ];

    for (file_path, content) in package_files {
        let full_path = project_path.join(file_path);
        if let Some(parent) = full_path.parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                eprintln!(
                    "Warning: Unable to create parent directory {}: {}",
                    parent.display(),
                    e
                );
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

    let package_structure =
        include_str!("../configs/package_structure.md").replace("{{PACKAGE_NAME}}", name);
    println!("{}", package_structure);

    let instructions = include_str!("../configs/instructions.md").replace("{{PACKAGE_NAME}}", name);
    println!("{}", instructions);
}

pub fn check_project() {
    let context = Context::default().set_environment(Environment::Project);
    let lang = parse_code(&PathBuf::from("TypR/main.ty"), context.get_environment());
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    if type_checker.has_errors() {
        eprintln!("Type errors found:");
        type_checker.show_errors();
        std::process::exit(1);
    }
    println!("Code verification successful!");
}

pub fn check_file(path: &PathBuf) {
    let context = Context::default().set_environment(Environment::Project);
    let lang = parse_code(path, context.get_environment());
    let dir = PathBuf::from(".");
    write_std_for_type_checking(&dir);
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    write_context_json(&type_checker.get_context(), &dir);
    if type_checker.has_errors() {
        eprintln!("Type errors found:");
        type_checker.show_errors();
        std::process::exit(1);
    }
    println!("File verification {:?} successful!", path);
}

pub fn build_project(test_mode: bool) {
    build_project_impl(test_mode, false);
}

fn build_project_impl(test_mode: bool, quiet: bool) {
    let dir = PathBuf::from(".");
    let context = Context::default()
        .set_environment(Environment::Project)
        .set_test_mode(test_mode);
    let lang = parse_code(&PathBuf::from("TypR/main.ty"), context.get_environment());
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    if type_checker.has_errors() {
        type_checker.show_errors();
    }

    typr_core::processes::transpiling::reset_include_stack();
    let content = type_checker.clone().transpile();
    write_header(type_checker.get_context(), &dir, Environment::Project);
    write_to_r_lang(
        content,
        &PathBuf::from("R"),
        "main.R",
        context.get_environment(),
    );
    write_loader(&dir);
    document_impl(quiet);
    if !quiet {
        println!("R code successfully generated in the R/ folder");
    }
}

pub fn build_file(path: &Path, test_mode: bool) {
    let lang = parse_code(path, Environment::StandAlone);
    let dir = PathBuf::from(".");

    write_std_for_type_checking(&dir);
    let context = Context::default().set_test_mode(test_mode);
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    let r_file_name = path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .replace(".ty", ".R");
    let content = type_checker.clone().transpile();
    write_header(type_checker.get_context(), &dir, Environment::StandAlone);
    write_to_r_lang(content, &dir, &r_file_name, context.get_environment());
    println!("Generated R code: {:?}", dir.join(&r_file_name));
}

pub fn run_project() {
    build_project_impl(false, true);
    // Use the TypR loader instead of devtools to respect module encapsulation.
    // Top-level code in main.ty already runs as a side effect of sourcing it
    // (sys.source() inside load_module()). The `module Main { @pub let main
    // <- fn() {...} }` convention is optional: if it's present we call it as
    // the entry point, but a plain main.ty with top-level statements works
    // out of the box with no boilerplate required.
    let r_command = concat!(
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
    match Command::new("Rscript").arg("-e").arg(r_command).output() {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            if output.status.success() {
                if !stdout.is_empty() {
                    print!("{}", stdout);
                }
            } else {
                eprintln!("Error (code {}):\n{}", output.status, stderr);
                if !stdout.is_empty() {
                    print!("{}", stdout);
                }
                std::process::exit(1);
            }
        }
        Err(e) => eprintln!("Failed to execute command: {}", e),
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
    run_file_impl(path, false);
}

pub fn run_file_keep(path: &Path) {
    run_file_impl(path, true);
}

fn run_file_impl(path: &Path, keep_files: bool) {
    let raw = fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Cannot read {:?}: {}", path, e);
        std::process::exit(1);
    });
    let content = strip_shebang(&raw);
    let file_name = path.to_str().unwrap().to_string();
    let lang = parse_code_from_str(content, &file_name, Environment::StandAlone);

    let work_dir = if keep_files {
        PathBuf::from(".")
    } else {
        let tmp = std::env::temp_dir().join(format!("typr_{}", std::process::id()));
        fs::create_dir_all(&tmp).unwrap();
        tmp
    };

    write_std_for_type_checking(&work_dir);
    let context = Context::default();
    let type_checker = TypeChecker::new(context.clone()).typing_no_panic(&lang);
    if type_checker.has_errors() {
        type_checker.show_errors();
        if !keep_files {
            let _ = fs::remove_dir_all(&work_dir);
        }
        std::process::exit(1);
    }

    let stem = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("script");
    let r_file_name = format!("{}.R", stem);

    // Change CWD to work_dir before transpiling so that write_output_file
    // (used for external module .R files) writes relative to work_dir,
    // matching where Rscript will look for source() calls.
    if !keep_files {
        if let Err(e) = std::env::set_current_dir(&work_dir) {
            eprintln!("Cannot set working directory: {}", e);
            let _ = fs::remove_dir_all(&work_dir);
            std::process::exit(1);
        }
    }

    let r_content = type_checker.clone().transpile();
    write_header(
        type_checker.get_context(),
        &work_dir,
        Environment::StandAlone,
    );
    write_to_r_lang(
        r_content,
        &work_dir,
        &r_file_name,
        context.get_environment(),
    );

    let r_path = work_dir.join(&r_file_name);
    let result = Command::new("Rscript")
        .current_dir(&work_dir)
        .arg(&r_path)
        .output();

    if !keep_files {
        let _ = fs::remove_dir_all(&work_dir);
    }

    match result {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            if output.status.success() {
                if !stdout.is_empty() {
                    print!("{}", stdout);
                }
            } else {
                eprintln!("Error (code {}):\n{}", output.status, stderr);
                if !stdout.is_empty() {
                    print!("{}", stdout);
                }
                std::process::exit(1);
            }
        }
        Err(e) => eprintln!("Failed to execute Rscript: {}", e),
    }
}

fn write_context_json(context: &Context, output_dir: &Path) {
    let json = serde_json::to_string_pretty(context).expect("Failed to serialize context to JSON");
    let path = output_dir.join("context.json");
    let mut file = File::create(&path).expect("Failed to create context.json");
    file.write_all(json.as_bytes())
        .expect("Failed to write context.json");
}

pub fn test() {
    build_project(true);
    // Load modules in test mode so @testable members are accessible in test files.
    // Then delegate test discovery to devtools::test().
    let r_command = concat!(
        "source('load_module.R'); ",
        "modules <- load_module('.', test = TRUE); ",
        "devtools::test()"
    );

    println!("Execution of: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(r_command).output();

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

    let content = fs::read_to_string(&description_path)
        .map_err(|e| format!("Error reading file DESCRIPTION: {}", e))?;

    for line in content.lines() {
        if line.starts_with("Package:") {
            let package_name = line.replace("Package:", "").trim().to_string();
            return Ok(package_name);
        }
    }

    Err("Package name not found in the DESCRIPTION file".to_string())
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
                let pkgs_str = pkgs
                    .iter()
                    .map(|p| format!("'{}'", p))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("renv::install(c({}))", pkgs_str)
            } else {
                println!("Installing packages from CRAN: {:?}", pkgs);
                let pkgs_str = pkgs
                    .iter()
                    .map(|p| format!("'{}'", p))
                    .collect::<Vec<_>>()
                    .join(", ");
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

fn document_impl(quiet: bool) {
    if !quiet {
        println!("Generating package documentation...");
    }

    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("Error obtaining current directory: {}", e);
            std::process::exit(1);
        }
    };

    let project_path = current_dir.to_str().unwrap();
    let r_command = format!("devtools::document('{}')", project_path);

    let output = Command::new("R").arg("-e").arg(&r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                if !quiet {
                    println!("Documentation successfully generated!");

                    if !output.stdout.is_empty() {
                        println!()
                    }
                }
            } else {
                eprintln!("Error while generating documentation");

                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }

                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Error while executing the R command : {}", e);
            eprintln!("Be sure that R et devtools are installed.");
            std::process::exit(1);
        }
    }
}

pub fn use_package(package_name: &str) {
    println!("Adding the package '{}' as a dependency...", package_name);
    let r_command = format!("devtools::use_package('{}')", package_name);
    println!("Execution of: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(&r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                println!(
                    "Package '{}' successfully added to dependencies!",
                    package_name
                );

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
            eprintln!("Make sure that R and devtools are installed.");
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
}
