mod language;
mod elements;
mod parser;
mod types;
mod operators;
mod my_io;
mod var;
mod metaprogramming;
mod argument_type;
mod argument_value;
mod type_comparison;
mod unification;
mod context;
mod r#type;
mod type_checker;
mod type_printer;
mod tag;
mod index;
mod engine;
mod vartype;
mod unification_map;
mod function_type;
mod help_message;
mod help_data;
mod builder;
mod path;
mod generic;
mod tint;
mod tchar;
mod error_message;
mod translatable;
mod function_lang;
mod array_type;
mod config;
mod graph;
mod type_category;
mod typer;
mod var_function;
mod type_stack;
mod js_types;
use crate::config::Config;

use std::io::Write;
use std::fs::File;
use crate::help_message::TypeError;
use parser::parse;
use my_io::read_file;
use crate::r#type::Type;
use crate::language::Lang;
use crate::metaprogramming::metaprogrammation;
use crate::type_checker::typing;
use crate::context::Context;
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::fs;
use crate::engine::parse_code;
use crate::my_io::execute_r_with_path;
use crate::var::Var;
use crate::engine::write_std_for_type_checking;
use std::process::Command;
use crate::type_checker::TypeChecker;
use crate::type_checker::execute_r_function;
use crate::vartype::VarType;

const R_FUNCTIONS: &str = "../configs/src/functions_R.txt";
const JS_FUNCTIONS: &str = "../configs/src/functions_JS.txt";

pub fn write_to_r_lang(content: String, output_dir: &PathBuf, file_name: &str, _project: bool) -> () {
        let rstd = include_str!("../configs/r/std.R");
        let std_path = output_dir.join("std.R");
        let mut rstd_file = File::create(std_path).unwrap();
        rstd_file.write_all(rstd.as_bytes()).unwrap();

        let app_path = output_dir.join(file_name);
        let mut app = File::create(app_path).unwrap();
        app.write_all(content.as_bytes()).unwrap();
}

#[derive(Debug, Parser, Clone, Copy, PartialEq)]
pub enum Environment {
    StandAlone,
    Project
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Fichier à exécuter directement
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,

    /// Langage cible (r, typescript, assemblyscript)
    #[arg(short, long, value_name = "TARGET", default_value = "r")]
    target: Option<String>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Creat a new project
    New {
        /// Project Name
        name: String,
    },
    /// check parsing and typechecking
    Check {
        /// Optional file to check (if not provided, checks project)
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    /// Check and build the targeted code
    Build {
        /// Optional file to build (if not provided, builds project)
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    /// Build and execute the targeted code
    Run {
        /// Optional file to run (if not provided, runs project)
        #[arg(value_name = "FILE")]
        file: Option<PathBuf>,
    },
    /// Run tests
    Test,
    /// Package management commands
    Pkg {
        #[command(subcommand)]
        pkg_command: PkgCommands,
    },
    /// Generate package documentation
    Document,
    /// Add a package dependency
    Use {
        /// Package name to add as dependency
        package_name: String,
    },
    Load,
    Cran,
    /// Update the standard library
    Std,
}

#[derive(Subcommand)]
enum PkgCommands {
    /// Install the package locally
    Install,
    /// Uninstall the package from local machine
    Uninstall,
}


fn new(name: &str) {
    println!("Création du package R '{}'...", name);

    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("Erreur lors de l'obtention du répertoire courant: {}", e);
            std::process::exit(1);
        }
    };
    
    let project_path = current_dir.join(name);
    
    if let Err(e) = fs::create_dir(&project_path) {
        eprintln!("Erreur lors de la création du répertoire du projet: {}", e);
        std::process::exit(1);
    }
    
    // Architecture classique d'un package R
    let package_folders = vec![
        "R",           // Code R
        "TypR",        // Code TypR source
        "man",         // Documentation
        "tests",       // Tests
        "testthat",    // Tests unitaires
        "data",        // Données
        "inst",        // Fichiers installés
        "src",         // Code source (C++, Fortran, etc.)
        "vignettes",   // Vignettes/tutoriels
    ];
    
    for folder in package_folders {
        let folder_path = project_path.join(folder);
        if let Err(e) = fs::create_dir(&folder_path) {
            eprintln!("Avertissement: Impossible de créer le dossier {}: {}", folder_path.display(), e);
        }
    }
    
    // Créer les sous-dossiers spécifiques
    let tests_testthat = project_path.join("tests/testthat");
    if let Err(e) = fs::create_dir(&tests_testthat) {
        eprintln!("Avertissement: Impossible de créer le dossier tests/testthat: {}", e);
    }
    
    // Créer les fichiers du package R
    let package_files = vec![
        ("DESCRIPTION", include_str!("../configs/DESCRIPTION").replace("{{PACKAGE_NAME}}", name)),
        ("NAMESPACE", include_str!("../configs/NAMESPACE").replace("{{PACKAGE_NAME}}", name)),
        (".Rbuildignore", include_str!("../configs/.Rbuildignore").replace("{{PACKAGE_NAME}}", name)),
        (".gitignore", include_str!("../configs/.gitignore").replace("{{PACKAGE_NAME}}", name)),
        ("TypR/main.ty", include_str!("../configs/main.ty").replace("{{PACKAGE_NAME}}", name)),
        ("TypR/std.ty", include_str!("../configs/r/std.ty").to_string()),
        ("std.ty", include_str!("../configs/r/std.ty").to_string()),
        ("R/.gitkeep", include_str!("../configs/.gitkeep").replace("{{PACKAGE_NAME}}", name)),
        ("tests/testthat.R", include_str!("../configs/testthat.R").replace("{{PACKAGE_NAME}}", name)),
        ("tests/testthat/test-basic.R", include_str!("../configs/test-basic.R").replace("{{PACKAGE_NAME}}", name)),
        ("man/.gitkeep", include_str!("../configs/.gitkeep2").replace("{{PACKAGE_NAME}}", name)),
        ("README.md", include_str!("../configs/README.md").replace("{{PACKAGE_NAME}}", name)),
        ("rproj.Rproj", include_str!("../configs/rproj.Rproj").to_string()),
    ];
    
    for (file_path, content) in package_files {
        let full_path = project_path.join(file_path);
        if let Some(parent) = full_path.parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                eprintln!("Warning: Impossible de créer le répertoire parent {}: {}", parent.display(), e);
                continue;
            }
        }
        println!("Writing {} in '{:?}'", content.len(), full_path);
        if let Err(e) = fs::write(&full_path, content) {
            eprintln!("Warning: Impossible de créer le fichier {}: {}", full_path.display(), e);
        }
    }
    
    println!("✓ Package R '{}' successfully created!", name);
    let package_structure = include_str!("../configs/package_structure.md").replace("{{PACKAGE_NAME}}", name);
    println!("{}", package_structure);
    
    let instructions = include_str!("../configs/instructions.md").replace("{{PACKAGE_NAME}}", name);
    println!("{}", instructions);
}

fn check_project() {
    let lang = parse_code(&PathBuf::from("TypR/main.ty"));
    let _ = typing(&Context::default(), &lang);
    println!("✓ Vérification du code réussie!");
}

fn check_file(path: &PathBuf) {
    let lang = parse_code(path);
    let dir = PathBuf::from(".");
    write_std_for_type_checking(&dir);
    let _ = typing(&Context::default(), &lang);
    println!("✓ Vérification du fichier {:?} réussie!", path);
}

fn build_project() {
    let lang = parse_code(&PathBuf::from("TypR/main.ty"));
    let type_checker = TypeChecker::new(Context::default()).typing(&lang);
    let content = type_checker.transpile(true);
    write_to_r_lang(content, &PathBuf::from("R"), "main.R", true);
    println!("✓ Code R généré avec succès dans le dossier R/");
}

fn build_file(path: &PathBuf) {
    let lang = parse_code(path);
    let dir = PathBuf::from(".");
    
    // HEADER
    write_std_for_type_checking(&dir);
    let type_checker = TypeChecker::new(Context::default()).typing(&lang);
    let r_file_name = path.file_name().unwrap().to_str().unwrap().replace(".ty", ".R");
    let content = type_checker.transpile(false);
    write_to_r_lang(content, &dir, &r_file_name, false);
    //adt_manager.get_body().write_to_r(&context, &dir, &r_file_name, false);
    println!("✓ Code R généré: {:?}", dir.join(&r_file_name));
}

fn run_project() {
    build_project();
    execute_r_with_path(&PathBuf::from("R"), "main.R");
}


fn run_file(path: &PathBuf) {
    let lang = parse_code(path);
    let dir = PathBuf::from(".");

    //HEADER
    write_std_for_type_checking(&dir);
    let type_checker = TypeChecker::new(Context::default()).typing(&lang);
    let r_file_name = path.file_name().unwrap().to_str().unwrap().replace(".ty", ".R");
    let content = type_checker.transpile(false);
    write_to_r_lang(content, &dir, &r_file_name, false);
    execute_r_with_path(&dir, &r_file_name);
}

fn test() {
    build_project();
    let r_command = "devtools::test()".to_string();
    
    println!("Execution of: R -e \"{}\"", r_command);
    
    let output = Command::new("R")
        .arg("-e")
        .arg(&r_command)
        .output();
    
    match output {
        Ok(output) => {
            if output.status.success() {
                // Afficher la sortie standard si elle existe
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Error while running tests");
                
                // Afficher les erreurs
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

fn get_package_name() -> Result<String, String> {
    let description_path = PathBuf::from("DESCRIPTION");
    
    if !description_path.exists() {
        return Err("Fichier DESCRIPTION introuvable. Êtes-vous à la racine du projet?".to_string());
    }
    
    let content = fs::read_to_string(&description_path)
        .map_err(|e| format!("Erreur lors de la lecture du fichier DESCRIPTION: {}", e))?;
    
    for line in content.lines() {
        if line.starts_with("Package:") {
            let package_name = line.replace("Package:", "").trim().to_string();
            return Ok(package_name);
        }
    }
    
    Err("Nom du package introuvable dans le fichier DESCRIPTION".to_string())
}

fn pkg_install() {
    println!("Installation du package...");
    
    // Récupérer le répertoire courant (racine du projet)
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("Erreur lors de l'obtention du répertoire courant: {}", e);
            std::process::exit(1);
        }
    };
    
    let project_path = current_dir.to_str().unwrap();
    
    // Construire la commande R
    let r_command = format!("devtools::install_local('{}')", project_path);
    
    println!("Exécution de: R -e \"{}\"", r_command);
    
    let output = Command::new("R")
        .arg("-e")
        .arg(&r_command)
        .output();
    
    match output {
        Ok(output) => {
            if output.status.success() {
                println!("✓ Package installé avec succès!");
                
                // Afficher la sortie standard si elle existe
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Erreur lors de l'installation du package");
                
                // Afficher les erreurs
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }
                
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Erreur lors de l'exécution de la commande R: {}", e);
            eprintln!("Assurez-vous que R et devtools sont installés.");
            std::process::exit(1);
        }
    }
}

fn pkg_uninstall() {
    println!("Désinstallation du package...");
    
    // Récupérer le nom du package depuis le fichier DESCRIPTION
    let package_name = match get_package_name() {
        Ok(name) => name,
        Err(e) => {
            eprintln!("Erreur: {}", e);
            std::process::exit(1);
        }
    };
    
    println!("Désinstallation du package '{}'...", package_name);
    
    // Construire la commande R pour désinstaller
    let r_command = format!("remove.packages('{}')", package_name);
    
    println!("Exécution de: R -e \"{}\"", r_command);
    
    let output = Command::new("R")
        .arg("-e")
        .arg(&r_command)
        .output();
    
    match output {
        Ok(output) => {
            if output.status.success() {
                println!("✓ Package '{}' désinstallé avec succès!", package_name);
                
                // Afficher la sortie standard si elle existe
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                // Note: remove.packages peut retourner un statut d'erreur même si le package n'était pas installé
                eprintln!("Note: Le package '{}' n'était peut-être pas installé ou une erreur s'est produite", package_name);
                
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }
            }
        }
        Err(e) => {
            eprintln!("Erreur lors de l'exécution de la commande R: {}", e);
            eprintln!("Assurez-vous que R est installé.");
            std::process::exit(1);
        }
    }
}

fn document() {
    println!("Génération de la documentation du package...");
    
    // Récupérer le répertoire courant (racine du projet)
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("Erreur lors de l'obtention du répertoire courant: {}", e);
            std::process::exit(1);
        }
    };
    
    let project_path = current_dir.to_str().unwrap();
    
    // Construire la commande R
    let r_command = format!("devtools::document('{}')", project_path);
    
    println!("Exécution de: R -e \"{}\"", r_command);
    
    let output = Command::new("R")
        .arg("-e")
        .arg(&r_command)
        .output();
    
    match output {
        Ok(output) => {
            if output.status.success() {
                println!("✓ Documentation générée avec succès!");
                
                // Afficher la sortie standard si elle existe
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Erreur lors de la génération de la documentation");
                
                // Afficher les erreurs
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }
                
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Erreur lors de l'exécution de la commande R: {}", e);
            eprintln!("Assurez-vous que R et devtools sont installés.");
            std::process::exit(1);
        }
    }
}

fn use_package(package_name: &str) {
    println!("Ajout du package '{}' comme dépendance...", package_name);
    
    // Construire la commande R
    let r_command = format!("devtools::use_package('{}')", package_name);
    
    println!("Exécution de: R -e \"{}\"", r_command);
    
    let output = Command::new("R")
        .arg("-e")
        .arg(&r_command)
        .output();
    
    match output {
        Ok(output) => {
            if output.status.success() {
                println!("✓ Package '{}' ajouté avec succès aux dépendances!", package_name);
                
                // Afficher la sortie standard si elle existe
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Erreur lors de l'ajout du package '{}'", package_name);
                
                // Afficher les erreurs
                if !output.stderr.is_empty() {
                    eprintln!("\n{}", String::from_utf8_lossy(&output.stderr));
                }
                
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Erreur lors de l'exécution de la commande R: {}", e);
            eprintln!("Assurez-vous que R et devtools sont installés.");
            std::process::exit(1);
        }
    }
}

fn load() {
    // Construire la commande R
    let r_command = "devtools::load_all('.')".to_string();
    
    println!("Execution of: R -e \"{}\"", r_command);
    
    let output = Command::new("R")
        .arg("-e")
        .arg(&r_command)
        .output();
    
    match output {
        Ok(output) => {
            if output.status.success() {
                println!("✓ Elements loaded with success!");
                
                // Afficher la sortie standard si elle existe
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Error while loading elements");
                
                // Afficher les erreurs
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

fn cran() {
    // Construire la commande R
    let r_command = "devtools::check()".to_string();
    
    println!("Execution of: R -e \"{}\"", r_command);
    
    let output = Command::new("R")
        .arg("-e")
        .arg(&r_command)
        .output();
    
    match output {
        Ok(output) => {
            if output.status.success() {
                println!("✓ Checks passed with success!");
                
                // Afficher la sortie standard si elle existe
                if !output.stdout.is_empty() {
                    println!("\n{}", String::from_utf8_lossy(&output.stdout));
                }
            } else {
                eprintln!("✗ Error while checking the project");
                
                // Afficher les erreurs
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

fn standard_library() {
    let function_list = execute_r_function("funcs <- ls('package:base', sorted = TRUE)\nfor (element in funcs) {\nprint(element)\n}").unwrap().replace("\"", "").replace("[1] ", "");
    fs::write(R_FUNCTIONS, function_list).unwrap();
    let empty = builder::empty_type();

    //Save R functions
    let std_txt = fs::read_to_string(R_FUNCTIONS).unwrap();
    let std = std_txt.lines()
        .map(|line| (Var::from_name(line), empty.clone()))
        .collect::<Vec<_>>();
    let _ = VarType::default().set_std(std).save("../configs/bin/std_r.bin");

    //Save JS functions
    let std_txt = fs::read_to_string(JS_FUNCTIONS).unwrap();
    let std = std_txt.lines()
        .map(|line| (Var::from_name(line), empty.clone()))
        .collect::<Vec<_>>();
    let _ = VarType::default().set_std(std).save("../configs/bin/std_js.bin");
}


fn main() {
    let cli = Cli::parse();

    // Si un fichier est fourni en argument principal (sans sous-commande)
    if let Some(path) = cli.file {
        if cli.command.is_none() {
            // Comportement original : exécuter le fichier directement
            // run single file
            run_file(&path);
            return;
        }
    }

    // Traitement des sous-commandes
    match cli.command {
        Some(Commands::New { name }) => {
            new(&name)
        },
        Some(Commands::Check { file }) => {
            match file {
                Some(path) => check_file(&path),
                None => check_project(),
            }
        },
        Some(Commands::Build { file }) => {
            match file {
                Some(path) => build_file(&path),
                None => build_project(),
            }
        },
        Some(Commands::Run { file }) => {
            match file {
                Some(path) => run_file(&path),
                None => run_project(),
            }
        },
        Some(Commands::Test) => {
            test()
        },
        Some(Commands::Pkg { pkg_command }) => {
            match pkg_command {
                PkgCommands::Install => pkg_install(),
                PkgCommands::Uninstall => pkg_uninstall(),
            }
        },
        Some(Commands::Document) => {
            document()
        },
        Some(Commands::Use { package_name }) => {
            use_package(&package_name)
        },
        Some(Commands::Load) => {
            load()
        },
        Some(Commands::Cran) => {
            cran()
        },
        Some(Commands::Std) => {
            standard_library()
        },
        None => {
            println!("Veuillez spécifier une sous-commande ou un fichier à exécuter");
            std::process::exit(1);
        }
    }
}
