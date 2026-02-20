use crate::components::context::config::Environment;
use crate::components::context::Context;
use crate::processes::type_checking::type_checker::TypeChecker;
use crate::processes::type_checking::typing;
use crate::utils::engine::parse_code;
use crate::utils::engine::write_std_for_type_checking;
use crate::utils::my_io::execute_r_with_path;
use std::fs;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

pub fn write_header(context: Context, output_dir: &PathBuf, environment: Environment) -> () {
    let type_anotations = context.get_type_anotations();
    let mut app = match environment {
        Environment::Repl => OpenOptions::new()
            .append(true)
            .create(true)
            .write(true)
            .open(output_dir.join(".repl.R")),
        _ => OpenOptions::new().create(true).write(true).open(
            output_dir
                .join(context.get_environment().to_base_path())
                .join("c_types.R"),
        ),
    }
    .unwrap();

    app.write_all(type_anotations.as_bytes()).unwrap();

    let generic_functions = context
        .get_all_generic_functions()
        .iter()
        .map(|(var, _)| var.get_name())
        .filter(|x| !x.contains("<-"))
        .map(|fn_name| {
            format!(
                "#' @export\n{} <- function(x, ...) UseMethod('{}', x)",
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
            .write(true)
            .open(output_dir.join(".repl.R")),
        _ => OpenOptions::new().create(true).write(true).open(
            output_dir
                .join(context.get_environment().to_string())
                .join("b_generic_functions.R"),
        ),
    }
    .unwrap();
    app.write_all((generic_functions + "\n").as_bytes())
        .unwrap();
}

pub fn write_to_r_lang(
    content: String,
    output_dir: &PathBuf,
    file_name: &str,
    environment: Environment,
) -> () {
    let rstd = include_str!("../../configs/src/std.R");
    let std_path = output_dir.join("a_std.R");
    let mut rstd_file = File::create(std_path).unwrap();
    rstd_file.write_all(rstd.as_bytes()).unwrap();

    let app_path = output_dir.join(file_name);
    let mut app = match environment {
        Environment::Repl => OpenOptions::new()
            .append(true)
            .write(true)
            .create(true)
            .open(app_path),
        _ => File::create(app_path),
    }
    .unwrap();
    let source = match environment {
        Environment::Project | Environment::Repl => "",
        Environment::StandAlone => "source('b_generic_functions.R')\nsource('c_types.R')",
    };
    app.write_all(format!("{}\n{}", source, content).as_bytes())
        .unwrap();
}

pub fn new(name: &str) {
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
            include_str!("../../configs/DESCRIPTION").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "NAMESPACE",
            include_str!("../../configs/NAMESPACE").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            ".Rbuildignore",
            include_str!("../../configs/.Rbuildignore").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            ".gitignore",
            include_str!("../../configs/.gitignore").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "TypR/main.ty",
            include_str!("../../configs/main.ty").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "R/.gitkeep",
            include_str!("../../configs/.gitkeep").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "tests/testthat.R",
            include_str!("../../configs/testthat.R").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "man/.gitkeep",
            include_str!("../../configs/.gitkeep2").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "README.md",
            include_str!("../../configs/README.md").replace("{{PACKAGE_NAME}}", name),
        ),
        (
            "rproj.Rproj",
            include_str!("../../configs/rproj.Rproj").to_string(),
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

    println!("✓ Package R '{}' successfully created!", name);
    let package_structure =
        include_str!("../../configs/package_structure.md").replace("{{PACKAGE_NAME}}", name);
    println!("{}", package_structure);

    let instructions =
        include_str!("../../configs/instructions.md").replace("{{PACKAGE_NAME}}", name);
    println!("{}", instructions);
}

pub fn check_project() {
    let context = Context::default().set_environment(Environment::Project);
    let lang = parse_code(&PathBuf::from("TypR/main.ty"), context.get_environment());
    let _ = typing(&context, &lang);
    println!("✓ Code verification successful!");
}

pub fn check_file(path: &PathBuf) {
    let context = Context::default().set_environment(Environment::Project);
    let lang = parse_code(path, context.get_environment());
    let dir = PathBuf::from(".");
    write_std_for_type_checking(&dir);
    let type_checker = TypeChecker::new(context.clone()).typing(&lang);
    if type_checker.has_errors() {
        // Les erreurs sont déjà affichées par TypeChecker::typing
        std::process::exit(1);
    }
    println!("✓ File verification {:?} successful!", path);
}

pub fn build_project() {
    let dir = PathBuf::from(".");
    let context = Context::default().set_environment(Environment::Project);
    let lang = parse_code(&PathBuf::from("TypR/main.ty"), context.get_environment());
    let type_checker = TypeChecker::new(context.clone()).typing(&lang);

    let content = type_checker.clone().transpile();
    write_header(type_checker.get_context(), &dir, Environment::Project);
    write_to_r_lang(
        content,
        &PathBuf::from("R"),
        "d_main.R",
        context.get_environment(),
    );
    document();
    println!("✓ R code successfully generated in the R/ folder");
}

pub fn build_file(path: &PathBuf) {
    let lang = parse_code(path, Environment::StandAlone);
    let dir = PathBuf::from(".");

    write_std_for_type_checking(&dir);
    let context = Context::default();
    let type_checker = TypeChecker::new(context.clone()).typing(&lang);
    let r_file_name = path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .replace(".ty", ".R");
    let content = type_checker.clone().transpile();
    write_header(type_checker.get_context(), &dir, Environment::StandAlone);
    write_to_r_lang(content, &dir, &r_file_name, context.get_environment());
    println!("✓ Generated R code: {:?}", dir.join(&r_file_name));
}

pub fn run_project() {
    build_project();
    execute_r_with_path(&PathBuf::from("R"), "main.R");
}

pub fn run_file(path: &PathBuf) {
    let lang = parse_code(path, Environment::StandAlone);
    let dir = PathBuf::from(".");

    write_std_for_type_checking(&dir);
    let context = Context::default();
    let type_checker = TypeChecker::new(context.clone()).typing(&lang);
    let r_file_name = path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .replace(".ty", ".R");
    let content = type_checker.clone().transpile();
    write_header(type_checker.get_context(), &dir, Environment::StandAlone);
    write_to_r_lang(content, &dir, &r_file_name, context.get_environment());
    execute_r_with_path(&dir, &r_file_name);
}

pub fn test() {
    build_project();
    let r_command = "devtools::test()".to_string();

    println!("Execution of: R -e \"{}\"", r_command);

    let output = Command::new("R").arg("-e").arg(&r_command).output();

    match output {
        Ok(output) => {
            if output.status.success() {
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Error while running tests");
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

pub fn pkg_install() {
    println!("Installing the package...");

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
                println!("✓ Package installed successfully!");

                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Error during package installation");
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
                println!("✓ Package '{}' successfully uninstalled!", package_name);

                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("Note: The package '{}' may not have been installed or an error may have occurred", package_name);

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
    println!("Generating package documentation...");

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
                println!("✓ Documentation successfully generated!");

                if !output.stdout.is_empty() {
                    println!("")
                }
            } else {
                eprintln!("✗ Error while generating documentation");

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
                    "✓ Package '{}' successfully added to dependencies!",
                    package_name
                );

                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Error adding package '{}'", package_name);

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
                println!("✓ Elements loaded with success!");
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Error while loading elements");
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
                println!("✓ Checks passed with success!");
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Error while checking the project");
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
                    if str_name.starts_with(".") {
                        if path.is_file() {
                            let _ = fs::remove_file(&path);
                        }
                    }
                }
            }
        }
    };
}
