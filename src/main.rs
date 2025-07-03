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
use std::path::Path;
use crate::engine::parse_code;
use crate::engine::type_check;
use crate::engine::write_adt_to_r_with_path;
use crate::my_io::execute_r_with_path;
use std::process::Command;
use crate::context::CompileMode;
use crate::var::Var;
use crate::engine::write_std_for_type_checking;

pub fn is_subset(v1: &[(Var, Type)], v2: &[(Var, Type)], cont: &Context) -> bool {
    v1.iter().all(|(v1, t1)| {
        v2.iter()
            .any(|(v2, t2)| v1.match_with(v2, cont) && t1 == t2)
    })
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
    Check,
    /// Check and build the targeted code
    Build,
    /// Build and execute the targeted code
    Run,
    /// Run tests
    Test
}

fn get_config_file_content(file_name: &str, name: &str) -> String {
    let description_source = Path::new(file_name);
    match fs::read_to_string(description_source) {
            Ok(content) => {
                content.replace("{{PACKAGE_NAME}}", name)
            },
            Err(e) => {
                "".to_string()
            }
        }
}


fn new(name: &str) {
    println!("Création du package R '{}'...", name);

    // Obtenir le répertoire courant
    let current_dir = match std::env::current_dir() {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("Erreur lors de l'obtention du répertoire courant: {}", e);
            std::process::exit(1);
        }
    };
    
    // Créer le chemin absolu du projet
    let project_path = current_dir.join(name);
    
    // Créer le répertoire principal du package
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
        ("DESCRIPTION", get_config_file_content("configs/DESCRIPTION", name)),
        ("NAMESPACE", get_config_file_content("configs/NAMESPACE", name)),
        (".Rbuildignore", get_config_file_content("configs/.Rbuildignore", name)),
        (".gitignore", get_config_file_content(".gitignore", name)),
        ("TypR/main.ty", get_config_file_content("configs/main.ty", name)),
        ("R/.gitkeep", get_config_file_content("configs/.gitkeep", name)),
        ("tests/testthat.R", get_config_file_content("configs/testthat.R", name)),
        ("tests/testthat/test-basic.R", get_config_file_content("configs/test-basic.R", name)),
        ("man/.gitkeep", get_config_file_content("configs/.gitkeep2", name)),
        ("README.md", get_config_file_content("configs/README.md", name))
    ];
    
    for (file_path, content) in package_files {
        let full_path = project_path.join(file_path);
        if let Some(parent) = full_path.parent() {
            if let Err(e) = fs::create_dir_all(parent) {
                eprintln!("Avertissement: Impossible de créer le répertoire parent {}: {}", parent.display(), e);
                continue;
            }
        }
        if let Err(e) = fs::write(&full_path, content) {
            eprintln!("Avertissement: Impossible de créer le fichier {}: {}", full_path.display(), e);
        }
    }
    
    let rproj_content = get_config_file_content("configs/rproj.Rproj", name);
    // Créer le fichier .Rproj
    let rproj_path = project_path.join(format!("{}.Rproj", name));
    if let Err(e) = fs::write(&rproj_path, rproj_content) {
        eprintln!("Avertissement: Impossible de créer le fichier .Rproj: {}", e);
    }
    
    println!("✓ Package R '{}' créé avec succès!", name);
    let package_structure = get_config_file_content("package_structure.md", name);
    println!("{}", package_structure);
    
    let instructions = get_config_file_content("instructions.md", name);
    println!("{}", instructions);

}

fn check() {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"));
    let _ = type_check(&adt_manager);
    println!("✓ Vérification du code réussie!");
}

fn build() {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"));
    let context = type_check(&adt_manager);
    
    write_adt_to_r_with_path(&adt_manager.get_body(), &context, &PathBuf::from("R"), "main.R");
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
    let file_name = path.file_name().unwrap().to_str().unwrap();
    let adt_manager = parse_code(&PathBuf::from(file_name));
    let dir = PathBuf::from(".");

    //HEADER
    write_std_for_type_checking(&dir);
    let context = type_check(&adt_manager);
    let new_file_name = file_name.replace(".ty", ".R");
    write_adt_to_r_with_path(&adt_manager.get_body(), &context, &dir, &new_file_name);
    execute_r_with_path(&dir, &new_file_name);
}

fn main() {
    let cli = Cli::parse();

    match cli.file {
        Some(path) => run_single_file(&path),
        None => {
            match cli.command {
                Some(Commands::New { name }) => {
                    // Plus besoin de parser le target, on crée toujours un projet R
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
