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
mod argument_kind;
mod type_comparison;
mod unification;
mod context;
mod adt;
mod nominal_context;
mod r#type;
mod kind;
mod type_checker;
mod type_printer;
mod tag;
mod index;
mod adt_manager;
mod engine;
mod vartype;
mod unification_map;
mod function_type;
mod help_message;
mod help_data;
mod adt_header;
mod builder;
mod path;
mod generic;
mod tint;
mod tchar;
mod error_message;
mod type_graph;
mod translatable;
mod function_lang;
mod array_type;
mod header;
mod config;

use crate::help_message::TypeError;
use parser::parse;
use my_io::read_file;
use crate::r#type::Type;
use crate::language::Lang;
use crate::metaprogramming::metaprogrammation;
use crate::adt::Adt;
use crate::type_checker::typing;
use crate::context::Context;
use crate::adt_manager::AdtManager;
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::fs;
use crate::engine::parse_code;
use crate::my_io::execute_r_with_path;
use config::CompileMode;
use crate::var::Var;
use crate::engine::write_std_for_type_checking;

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
    Check,
    /// Check and build the targeted code
    Build,
    /// Build and execute the targeted code
    Run,
    /// Run tests
    Test
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

fn check() {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"));
    let _ = adt_manager.type_check();
    println!("✓ Vérification du code réussie!");
}

fn build() {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"));
    let context = adt_manager.type_check();
    
    adt_manager.get_body().write_to_r(&context, &PathBuf::from("R"), "main.R");
    println!("✓ Code R généré avec succès dans le dossier R/");
}

fn run() {
    build();
    execute_r_with_path(&PathBuf::from("R"), "main.R");
}

fn test() {
    println!("Tests non implémentés pour l'instant.");
}

//main
fn run_single_file(path: &PathBuf) {
    let adt_manager = parse_code(path);
    let dir = PathBuf::from(".");

    //HEADER
    write_std_for_type_checking(&dir);
    let context = adt_manager.type_check();
    let r_file_name = path.file_name().unwrap().to_str().unwrap().replace(".ty", ".R");
    adt_manager.get_body().write_to_r(&context, &dir, &r_file_name);
    execute_r_with_path(&dir, &r_file_name);
}

fn main() {
    let cli = Cli::parse();

    match cli.file {
        Some(path) => run_single_file(&path),
        None => {
            match cli.command {
                Some(Commands::New { name }) => {
                    new(&name)
                },
                Some(Commands::Check) => {
                    check()
                },
                Some(Commands::Build) => {
                    build()
                },
                Some(Commands::Run) => {
                    run()
                },
                Some(Commands::Test) => {
                    test()
                },
                None => {
                    println!("Veuillez spécifier une sous-commande ou un fichier à exécuter");
                    std::process::exit(1);
                }
            }
        }
    }
}
