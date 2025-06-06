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
mod subtypes;
mod engine;
mod vartype;
mod unification_map;
mod function_type;
mod help_message;
mod help_data;
mod adt_header;
mod lang_builder;
mod path;
mod generic;
mod tint;
mod tchar;

use parser::parse;
use my_io::read_file;
use crate::r#type::Type;
use crate::language::Lang;
use crate::metaprogramming::metaprogrammation;
use crate::adt::Adt;
use crate::type_checker::typing;
use crate::context::Context;
use crate::adt_manager::AdtManager;
use std::collections::HashSet;
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::fs;
use std::path::Path;
use crate::engine::parse_code;
use crate::engine::type_check;
use crate::engine::write_adt_to_r_with_path;
use crate::my_io::execute_r_with_path;
use std::process::Command;
use crate::engine::write_adt_to_typescript;
use crate::my_io::execute_typescript;
use crate::engine::write_adt_to_assemblyscript_with_path;
use crate::engine::write_adt_to_typescript_with_path;
use crate::help_message::syntax_error;
use crate::context::CompileMode;
use crate::var::Var;

pub fn is_subset(v1: &[(Var, Type)], v2: &[(Var, Type)], cont: &Context) -> bool {
    v1.iter().all(|(v1, t1)| {
        v2.iter()
            .any(|(v2, t2)| v1.match_with(v2, cont) && t1 == t2)
    })
}

#[derive(Debug, Parser, Clone, Copy, PartialEq)]
enum TargetLanguage {
    R,
    TypeScript,
    AssemblyScript,
}

#[derive(Debug, Parser, Clone, Copy, PartialEq)]
enum Environment {
    StandAlone,
    Project
}

impl std::fmt::Display for TargetLanguage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TargetLanguage::R => write!(f, "R"),
            TargetLanguage::TypeScript => write!(f, "TypeScript"),
            TargetLanguage::AssemblyScript => write!(f, "AssemblyScript"),
        }
    }
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
        
        /// Target languages (r, typescript, assemblyscript)
        #[arg(short, long, value_name = "TARGET", default_value = "r")]
        target: String,
    },
    /// check parsing and typechecking
    Check {
        /// Target language (r, typescript, assemblyscript)
        #[arg(short, long, value_name = "TARGET", default_value = "r")]
        target: String,
    },
    /// Check and build the targeted code
    Build {
        /// Target language (r, typescript, assemblyscript)
        #[arg(short, long, value_name = "TARGET", default_value = "r")]
        target: String,
    },
    /// Build and execute the targeted code
    Run {
        /// Target language (r, typescript, assemblyscript)
        #[arg(short, long, value_name = "TARGET", default_value = "r")]
        target: String,
    },
    /// Run tests
    Test {
        /// Target language (r, typescript, assemblyscript)
        #[arg(short, long, value_name = "TARGET", default_value = "r")]
        target: String,
    },
}

fn parse_target(target_str: &str) -> TargetLanguage {
    match target_str.to_lowercase().as_str() {
        "r" => TargetLanguage::R,
        "typescript" | "ts" | "javascript" | "js" 
            => TargetLanguage::TypeScript,
        "assemblyscript" | "as" | "webassembly" | "wasm" 
            => TargetLanguage::AssemblyScript,
        _ => {
            eprintln!("Langage cible non reconnu: {}. Utilisation de R par défaut.", target_str);
            TargetLanguage::R
        }
    }
}

fn new(name: &str, target: TargetLanguage) {
    // Créer le dossier principal
    if let Err(e) = fs::create_dir(name) {
        eprintln!("Erreur lors de la création du dossier du projet: {}", e);
        std::process::exit(1);
    }

    // Créer la structure de base du projet
    let mut directories = vec![
        "TypR",        // Pour les scripts TypR (commun à tous les projets)
        "tests",       // Pour les tests (commun à tous les projets)
        "data",       // Pour les tests (commun à tous les projets)
    ];
    
    // Ajouter les dossiers spécifiques au langage cible
    match target {
        TargetLanguage::R => {
            directories.extend(vec![
                "R",           // Pour les scripts R
                "data",        // Pour les données
                "man",         // Pour la documentation
                "vignettes",   // Pour les tutoriels
            ]);
        },
        TargetLanguage::TypeScript => {
            directories.extend(vec![
                "src",         // Pour les fichiers TypeScript
                "dist",        // Pour les fichiers JavaScript compilés
                "types",       // Pour les définitions de types
            ]);
        },
        TargetLanguage::AssemblyScript => {
            directories.extend(vec![
                "assembly",    // Pour les fichiers AssemblyScript
                "build",       // Pour les fichiers compilés
            ]);
        },
    }

    // Créer les sous-dossiers
    for dir in directories {
        let path = Path::new(name).join(dir);
        if let Err(e) = fs::create_dir(&path) {
            eprintln!("Erreur lors de la création du dossier {}: {}", path.display(), e);
            std::process::exit(1);
        }
    }

    // Créer les fichiers de base communs
    let mut files = vec![
        ("README.md".to_string(), format!("# {}\n\nDescription of your package goes here.\n", name)),
        ("TypR/main.ty".to_string(), "# Main TypR code goes here\n".to_string()),
        ("tests/test.ty".to_string(), "# Test files go here\n".to_string()),
    ];
    
    // Ajouter les fichiers spécifiques au langage cible
    match target {
        TargetLanguage::R => {
            files.extend(vec![
                ("DESCRIPTION".to_string(), format!("Package: {}\nTitle: What the Package Does\nVersion: 0.1.0\nAuthors: [Your Name]\nDescription: Package description.\nLicense: MIT\n", name)),
                ("NAMESPACE".to_string(), "# Exported functions will be listed here\n".to_string()),
                ("R/main.R".to_string(), "# Main package code goes here\n".to_string()),
            ]);
        },
        TargetLanguage::TypeScript => {
            files.extend(vec![
                ("package.json".to_string(), format!(
                    "{{\n  \"name\": \"{}\",\n  \"version\": \"0.1.0\",\n  \"description\": \"\",\n  \"main\": \"dist/index.js\",\n  \"scripts\": {{\n    \"build\": \"tsc\",\n    \"start\": \"node dist/index.js\",\n    \"test\": \"echo \\\"Error: no test specified\\\" && exit 1\"\n  }},\n  \"keywords\": [],\n  \"author\": \"\",\n  \"license\": \"ISC\"\n}}", 
                    name
                )),
                ("tsconfig.json".to_string(), 
                    "{\n  \"compilerOptions\": {\n    \"target\": \"es2020\",\n    \"module\": \"commonjs\",\n    \"outDir\": \"./dist\",\n    \"strict\": true,\n    \"esModuleInterop\": true,\n    \"skipLibCheck\": true,\n    \"forceConsistentCasingInFileNames\": true\n  },\n  \"include\": [\"src/**/*\"],\n  \"exclude\": [\"node_modules\"]\n}".to_string()
                ),
                ("src/index.ts".to_string(), "// Main TypeScript code goes here\n".to_string()),
            ]);
        },
        TargetLanguage::AssemblyScript => {
            files.extend(vec![
                ("assembly/index.ts".to_string(), "// Main AssemblyScript code goes here\nexport function add(a: i32, b: i32): i32 {\n  return a + b;\n}\n".to_string()),
            ]);
        },
    }

    for (file, content) in files {
        let path = Path::new(name).join(file);
        if let Err(e) = fs::write(&path, content) {
            eprintln!("Erreur lors de la création du fichier {}: {}", path.display(), e);
            std::process::exit(1);
        }
    }

    // Initialisation spécifique pour AssemblyScript
    if target == TargetLanguage::AssemblyScript {
        // Sauvegarder le répertoire courant
        let current_dir = std::env::current_dir().expect("Impossible d'obtenir le répertoire courant");
        
        // Changer vers le répertoire du projet
        if let Err(e) = std::env::set_current_dir(name) {
            eprintln!("Erreur lors du changement vers le répertoire du projet: {}", e);
            std::process::exit(1);
        }
        
        println!("Initialisation du projet AssemblyScript...");
        
        // Exécuter les commandes npm
        let commands = vec![
            "npm init -y",
            "npm install --save-dev assemblyscript",
            "npm install --save @assemblyscript/loader",
            "npm install --save-dev @assemblyscript/wasi-shim",
            "npx asinit . -y"
        ];
        
        for cmd in commands {
            let parts: Vec<&str> = cmd.split_whitespace().collect();
            let command = parts[0];
            let args = &parts[1..];
            
            println!("Exécution de: {}", cmd);
            
            let status = Command::new(command)
                .args(args)
                .status();
                
            match status {
                Ok(exit_status) => {
                    if !exit_status.success() {
                        eprintln!("La commande '{}' a échoué avec le code de sortie: {:?}", cmd, exit_status.code());
                    }
                },
                Err(e) => {
                    eprintln!("Erreur lors de l'exécution de la commande '{}': {}", cmd, e);
                }
            }
        }
        
        // Revenir au répertoire d'origine
        if let Err(e) = std::env::set_current_dir(current_dir) {
            eprintln!("Erreur lors du retour au répertoire d'origine: {}", e);
            std::process::exit(1);
        }
    }

    println!("✨ Projet '{}' pour {} créé avec succès!", name, target);
    println!("Structure du projet:");
    
    // Afficher la structure du projet en fonction du langage cible
    match target {
        TargetLanguage::R => {
            println!("{}
├── R/
│   └── main.R
├── TypR/
│   └── main.ty
├── data/
├── man/
├── tests/
│   └── test.ty
├── vignettes/
├── DESCRIPTION
├── NAMESPACE
└── README.md", name);
        },
        TargetLanguage::TypeScript => {
            println!("{}
├── src/
│   └── index.ts
├── dist/
├── types/
├── TypR/
│   └── main.ty
├── tests/
│   └── test.ty
├── package.json
├── tsconfig.json
└── README.md", name);
        },
        TargetLanguage::AssemblyScript => {
            println!("{}
├── assembly/
│   └── index.ts
├── build/
├── TypR/
│   └── main.ty
├── tests/
│   └── test.ty
├── package.json
├── asconfig.json
├── node_modules/
└── README.md", name);
        },
    }
}

fn check(target: TargetLanguage) {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"), target);
    let _ = type_check(&adt_manager, target, Environment::Project);
    println!("✓ Vérification du code réussie!");
}

fn build(target: TargetLanguage) {
    let adt_manager = parse_code(&PathBuf::from("TypR/main.ty"), target);
    let context = type_check(&adt_manager, target, Environment::Project);
    
    match target {
        TargetLanguage::R => {
            write_adt_to_r_with_path(&adt_manager.get_body(), &context, &PathBuf::from("R"), "main.R");
            println!("✓ Code R généré avec succès dans le dossier R/");
        },
        TargetLanguage::TypeScript => {
            // Fonction à implémenter pour générer du TypeScript
            write_adt_to_typescript_with_path(&adt_manager.get_body(), &context, &PathBuf::from("src"));
            println!("✓ Code TypeScript généré avec succès dans le dossier src/");
        },
        TargetLanguage::AssemblyScript => {
            // Fonction à implémenter pour générer de l'AssemblyScript
            write_adt_to_assemblyscript_with_path(&adt_manager.get_body(), &context, &PathBuf::from("assembly"));
            println!("✓ Code AssemblyScript généré avec succès dans le dossier assembly/");
        },
    }
}

fn run(target: TargetLanguage) {
    build(target);
    
    match target {
        TargetLanguage::R => {
            execute_r_with_path(&PathBuf::from("R"), "main.R");
        },
        TargetLanguage::TypeScript => {
            // Compiler et exécuter le TypeScript
            let original_dir = std::env::current_dir().expect("Impossible d'obtenir le répertoire de travail actuel");
            std::env::set_current_dir("src").expect("Échec lors du changement de répertoire");
            
            println!("Compilation TypeScript...");
            let tsc_output = Command::new("tsc")
                .output()
                .expect("Échec lors de la compilation TypeScript");
                
            if !tsc_output.status.success() {
                let stderr = String::from_utf8_lossy(&tsc_output.stderr);
                println!("Erreur de compilation TypeScript: {}", stderr);
                return;
            }
            
            println!("Exécution JavaScript...");
            let node_output = Command::new("node")
                .arg("../dist/index.js")
                .output()
                .expect("Échec lors de l'exécution de Node.js");
                
            let stdout = String::from_utf8_lossy(&node_output.stdout);
            println!("{}", stdout);
            
            std::env::set_current_dir(original_dir).expect("Échec lors de la restauration du répertoire de travail");
        },
        TargetLanguage::AssemblyScript => {
            // Compiler et exécuter l'AssemblyScript
            let original_dir = std::env::current_dir().expect("Impossible d'obtenir le répertoire de travail actuel");
            
            println!("Compilation AssemblyScript...");
            let asbuild_output = Command::new("npx")
                .arg("asc")
                .arg("assembly/main.ts")
                .arg("-o")
                .arg("build/main.wasm")
                .arg("--optimize")
                .arg("--config")
                .arg("./node_modules/@assemblyscript/wasi-shim/asconfig.json")
                .output()
                .expect("Échec lors de la compilation AssemblyScript");
                
            if !asbuild_output.status.success() {
                let stderr = String::from_utf8_lossy(&asbuild_output.stderr);
                println!("Erreur de compilation AssemblyScript: {}", stderr);
                return;
            }
            
            println!("Exécution WebAssembly...");
            // Ici, vous devriez implémenter une méthode pour exécuter le WebAssembly
            // Cela pourrait nécessiter un environnement Node.js avec un script spécifique
            let asbuild_output = Command::new("wasmtime")
                .arg("build/main.wasm")
                .output()
                .expect("Échec lors de l'execution du wasm");
            
            // Afficher la sortie standard
            let stdout = String::from_utf8_lossy(&asbuild_output.stdout);
            println!("{}", stdout);
            
            // Si vous voulez également afficher les erreurs éventuelles
            let stderr = String::from_utf8_lossy(&asbuild_output.stderr);
            if !stderr.is_empty() {
                eprintln!("Erreurs: {}", stderr);
            }
            
            std::env::set_current_dir(original_dir).expect("Échec lors de la restauration du répertoire de travail");
        },
    }
}


fn test(target: TargetLanguage) {
    println!("Exécution des tests pour {}...", target);
    // Implémenter la logique de test pour chaque langage cible
    match target {
        TargetLanguage::R => {
            // Logique de test pour R
        },
        TargetLanguage::TypeScript => {
            // Logique de test pour TypeScript
        },
        TargetLanguage::AssemblyScript => {
            // Logique de test pour AssemblyScript
        },
    }
    println!("Tests non implémentés pour l'instant.");
}

//main
fn run_single_file(path: &PathBuf, target: TargetLanguage) {
    
    let file_name = path.file_name().unwrap().to_str().unwrap();
    let adt_manager = parse_code(&PathBuf::from(file_name), target);
    //HEADER
    let context = type_check(&adt_manager, target, Environment::StandAlone);

    
    let dir = PathBuf::from(".");
    
    match target {
        TargetLanguage::R => {
            let new_file_name = file_name.replace(".ty", ".R");
            write_adt_to_r_with_path(&adt_manager.get_body(), &context, &dir, &new_file_name);
            execute_r_with_path(&dir, &new_file_name);
        },
        TargetLanguage::TypeScript => {
            // Générer et exécuter le TypeScript
            write_adt_to_typescript(&adt_manager.get_body(), &context);
            execute_typescript();
        },
        TargetLanguage::AssemblyScript => {
            // Générer et exécuter l'AssemblyScript
            // execute_assemblyscript_with_path(&temp_dir);
            println!("Exécution AssemblyScript non implémentée pour l'instant.");
        },
    }
    
}

fn main() {
    let cli = Cli::parse();
    
    // Déterminer le langage cible
    let target = match &cli.target {
        Some(target_str) => parse_target(target_str),
        None => TargetLanguage::R, // Par défaut
    };

    match cli.file {
        Some(path) => run_single_file(&path, target),
        None => {
            match cli.command {
                Some(Commands::New { name, target: target_str }) => {
                    let target_lang = parse_target(&target_str);
                    new(&name, target_lang)
                },
                Some(Commands::Check { target: target_str }) => {
                    let target_lang = parse_target(&target_str);
                    check(target_lang)
                },
                Some(Commands::Build { target: target_str }) => {
                    let target_lang = parse_target(&target_str);
                    build(target_lang)
                },
                Some(Commands::Run { target: target_str }) => {
                    let target_lang = parse_target(&target_str);
                    run(target_lang)
                },
                Some(Commands::Test { target: target_str }) => {
                    let target_lang = parse_target(&target_str);
                    test(target_lang)
                },
                None => {
                    println!("Veuillez spécifier une sous-commande ou un fichier à exécuter");
                    std::process::exit(1);
                }
            }
        }
    }
}
