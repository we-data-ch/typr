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
mod kinds;
mod kind;
mod type_checker;
mod type_context;
mod type_module;
mod type_printer;
mod tag;
mod index;
mod adt_manager;
mod nominals;
mod subtypes;
mod engine;

use parser::parse;
use my_io::{read_file, execute_r};
use crate::r#type::Type;
use crate::language::Lang;
use crate::metaprogramming::metaprogrammation;
use crate::adt::Adt;
use crate::type_checker::typing;
use crate::context::Context;
use crate::adt_manager::AdtManager;
use std::collections::HashSet;
use crate::engine::run_file;
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::fs;
use std::path::Path;
use crate::engine::parse_code;
use crate::engine::type_check;
use crate::engine::execute;
use crate::engine::write_adt_to_r_with_path;
use crate::my_io::execute_r_with_path;
use crate::engine::run_file_to_typescript;

pub fn is_subset<T: Eq + std::hash::Hash>(v1: &[T], v2: &[T]) -> bool {
    let set_v2: HashSet<_> = v2.iter().collect();
    v1.iter().all(|x| set_v2.contains(x))
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Fichier à exécuter directement
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Créer un nouveau projet
    New {
        /// Nom du projet
        name: String,
    },
    /// Vérifier le parsing et le typechecking
    Check,
    /// Faire le check et construire le code cible
    Build,
    /// Faire le build et exécuter le code cible
    Run,
    /// Lancer les tests
    Test,
    /// Exécuter un fichier WebAssembly
    Javascript {
        /// Chemin du fichier WebAssembly
        path: PathBuf,
    },
}

fn new(name: &str) {
    // Créer le dossier principal
    if let Err(e) = fs::create_dir(name) {
        eprintln!("Erreur lors de la création du dossier du projet: {}", e);
        std::process::exit(1);
    }

    // Créer la structure de base du projet
    let directories = vec![
        "R",           // Pour les scripts R
        "data",        // Pour les données
        "man",         // Pour la documentation
        "tests",       // Pour les tests
        "vignettes",   // Pour les tutoriels
        "TypR",        // Pour les scripts TypR
    ];

    // Créer les sous-dossiers
    for dir in directories {
        let path = Path::new(name).join(dir);
        if let Err(e) = fs::create_dir(&path) {
            eprintln!("Erreur lors de la création du dossier {}: {}", path.display(), e);
            std::process::exit(1);
        }
    }

    // Créer les fichiers de base
    let files = vec![
        ("DESCRIPTION".to_string(), format!("Package: {}\nTitle: What the Package Does\nVersion: 0.1.0\nAuthors: [Your Name]\nDescription: Package description.\nLicense: MIT\n", name)),
        ("NAMESPACE".to_string(), "# Exported functions will be listed here\n".to_string()),
        ("README.md".to_string(), format!("# {}\n\nDescription of your package goes here.\n", name)),
        ("R/main.R".to_string(), "# Main package code goes here\n".to_string()),
        ("TypR/main.ty".to_string(), "# Main TypR code goes here\n".to_string()),
        ("tests/test.R".to_string(), "# Test files go here\n".to_string()),
    ];

    for (file, content) in files {
        let path = Path::new(name).join(file);
        if let Err(e) = fs::write(&path, content) {
            eprintln!("Erreur lors de la création du fichier {}: {}", path.display(), e);
            std::process::exit(1);
        }
    }

    println!("✨ Projet '{}' créé avec succès!", name);
    println!("Structure du projet:");
    println!("{}
├── R/
│   └── main.R
├── TypR/
│   └── main.ty
├── data/
├── man/
├── tests/
│   └── test.R
├── vignettes/
├── DESCRIPTION
├── NAMESPACE
└── README.md", name);
}

fn check() {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"));
    let context = type_check(&adt_manager.get_adt_with_header());
}

fn build() {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"));
    let context = type_check(&adt_manager.get_adt_with_header());
    write_adt_to_r_with_path(&adt_manager.get_adt_without_header(), &context, &PathBuf::from("R"));
}

fn run() {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"));
    let context = type_check(&adt_manager.get_adt_with_header());
    write_adt_to_r_with_path(&adt_manager.get_adt_without_header(), &context, &PathBuf::from("R"));
    execute_r_with_path(&PathBuf::from("R"));
}

fn test() {
    todo!("Implémenter les tests");
}

fn main() {
    let cli = Cli::parse();

    match cli.file {
        Some(path) => run_file(&path, &PathBuf::from(".")),
        None => {
            match cli.command {
                Some(Commands::New { name }) => new(&name),
                Some(Commands::Check) => check(),
                Some(Commands::Build) => build(),
                Some(Commands::Run) => run(),
                Some(Commands::Test) => test(),
                Some(Commands::Javascript { path }) => run_file_to_typescript(&path, &PathBuf::from(".")),
                None => {
                    println!("Veuillez spécifier une sous-commande ou un fichier à exécuter");
                    std::process::exit(1);
                }
            }
        }
    }
}
