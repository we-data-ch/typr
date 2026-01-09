use rustyline::error::ReadlineError;
use rustyline::{Config, DefaultEditor, Editor};
use rpds::Vector;
use crate::fluent_parser::FluentParser;
use crate::Context;
use std::path::PathBuf;
use crate::write_to_r_lang;
use crate::Environment;
use crate::my_io::execute_r_with_path2;
use crate::write_header;
use std::fs::OpenOptions;
use std::io::Write;
use std::fs;

/// Résultat de l'exécution d'une commande R
#[derive(Debug, Clone)]
pub struct ExecutionResult {
    pub output: Vec<String>,
}


/// État de la saisie utilisateur
#[derive(Debug, Clone, Copy, PartialEq)]
enum InputState {
    Normal,
    MultiLine,
}

/// Gestionnaire de l'interface CLI avec Rustyline
pub struct CliInterface {
    editor: DefaultEditor,
    input_state: InputState,
    command_buffer: String,
    history_file: String,
}

impl CliInterface {
    /// Crée une nouvelle interface CLI
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let config = Config::builder()
            .auto_add_history(true)
            .build();

        let mut editor = Editor::with_config(config)?;
        
        // Fichier d'historique
        let history_file = std::env::var("HOME")
            .or_else(|_| std::env::var("USERPROFILE"))
            .map(|home| format!("{}/.r_repl_history", home))
            .unwrap_or_else(|_| ".r_repl_history".to_string());

        // Charge l'historique existant
        let _ = editor.load_history(&history_file);

        Ok(CliInterface {
            editor,
            input_state: InputState::Normal,
            command_buffer: String::new(),
            history_file,
        })
    }

    /// Affiche le message de bienvenue
    pub fn show_welcome(&self) {
        println!("TypR REPL: 'exit' to quit");
    }

    /// Lit une ligne avec le prompt approprié
    pub fn read_line(&mut self) -> Result<String, ReadlineError> {
        let prompt = match self.input_state {
            InputState::Normal => "R> ",
            InputState::MultiLine => "... ",
        };
        
        self.editor.readline(prompt)
    }

    /// Traite l'entrée utilisateur et retourne une commande à exécuter si complète
    pub fn process_input(&mut self, input: &str) -> Option<MyCommand> {
        let trimmed = input.trim();

        // Commandes spéciales en mode normal
        if self.input_state == InputState::Normal {
            match trimmed {
                "exit" | "quit" => return Some(MyCommand::Exit),
                "clear" => return Some(MyCommand::Clear),
                "" => return Some(MyCommand::Empty),
                _ => {}
            }
        }

        // Gestion du buffer multi-ligne
        if self.input_state == InputState::MultiLine {
            self.command_buffer.push('\n');
        }
        self.command_buffer.push_str(trimmed);

        // Détecte si la commande est complète
        if self.is_command_complete(&self.command_buffer) {
            let cmd = self.command_buffer.clone();
            self.command_buffer.clear();
            self.input_state = InputState::Normal;
            Some(MyCommand::Execute(cmd))
        } else {
            self.input_state = InputState::MultiLine;
            None
        }
    }

    /// Détermine si une commande est complète (tous les blocs fermés)
    fn is_command_complete(&self, cmd: &str) -> bool {
        let open_braces = cmd.matches('{').count();
        let close_braces = cmd.matches('}').count();
        let open_parens = cmd.matches('(').count();
        let close_parens = cmd.matches(')').count();
        let open_brackets = cmd.matches('[').count();
        let close_brackets = cmd.matches(']').count();

        open_braces == close_braces
            && open_parens == close_parens
            && open_brackets == close_brackets
    }

    /// Affiche un résultat d'exécution
    pub fn display_result(&self, result: &ExecutionResult) {
        for line in &result.output {
            println!("{}", line);
        }
    }

    /// Affiche un message d'erreur
    pub fn display_error(&self, error: &str) {
        eprintln!("❌ Erreur: {}", error);
    }

    /// Efface l'écran
    pub fn clear_screen(&mut self) {
        self.editor.clear_screen().ok();
    }

    /// Sauvegarde l'historique
    pub fn save_history(&mut self) {
        if let Err(e) = self.editor.save_history(&self.history_file) {
            eprintln!("Avertissement: Impossible de sauvegarder l'historique: {}", e);
        }
    }

    /// Réinitialise l'état multi-ligne (utile après Ctrl-C)
    pub fn reset_multiline_state(&mut self) {
        self.input_state = InputState::Normal;
        self.command_buffer.clear();
    }
}

/// Commandes interprétées par le CLI
#[derive(Debug)]
pub enum MyCommand {
    Execute(String),
    Exit,
    Clear,
    Empty,
}

#[derive(Debug, Clone)]
struct TypRExecutor {
    api: FluentParser,
}

impl TypRExecutor {
    pub fn new() -> Self {
        TypRExecutor {
            api: FluentParser::new().set_context(Context::default()),
        }
    }

    fn get_r_code(self, cmd: &str) -> (Self, String) {
        let (r_code, api) = self.api.push(cmd).run().next_r_code().unwrap();
        let res = Self {
            api: api.clone(),
            ..self
        };
        let saved_code = format!("{}\n{}", api.get_saved_r_code(), r_code);
        (res, saved_code)
    }

    fn run_r_code(context: Context, r_code: &str) -> String {
        let dir = PathBuf::from(".");
        let r_file_name = ".repl.R";
        let _ = fs::remove_file(r_file_name);
        let mut file = OpenOptions::new().write(true).create(true).open(r_file_name).unwrap();
        let _ = file.write_all("source('a_std.R')\n".as_bytes());
        write_header(context, &dir, Environment::Repl);
        write_to_r_lang(r_code.to_string(), &dir, &r_file_name, Environment::Repl);
        let res = execute_r_with_path2(&dir, &r_file_name);
        res
    }

    fn execute(self, cmd: &str) -> Result<(Self, ExecutionResult), String> {
        let (new, r_code) = Self::get_r_code(self, cmd);
        let res = Self::run_r_code(new.api.context.clone(), &r_code);
        Ok((new, ExecutionResult { output: vec![res] }))
    }
}

/// REPL principal qui orchestre l'exécuteur et l'interface CLI
pub struct RRepl {
    executor: TypRExecutor,
    cli: CliInterface,
}

impl RRepl {
    /// Crée un nouveau REPL
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let executor = TypRExecutor::new();
        let cli = CliInterface::new()?;

        Ok(RRepl { executor, cli })
    }

    /// Lance la boucle REPL principale
    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        self.cli.show_welcome();

        loop {
            match self.cli.read_line() {
                Ok(line) => {
                    if let Some(command) = self.cli.process_input(&line) {
                        match command {
                            MyCommand::Execute(cmd) => match self.executor.clone().execute(&cmd) {
                                Ok((executor, result)) => {
                                    self.executor = executor;
                                    self.cli.display_result(&result)
                                },
                                Err(e) => {
                                    self.cli.display_error(&format!("Exécution échouée: {}", e))
                                }
                            },
                            MyCommand::Exit => {
                                println!("\nexiting...");
                                break;
                            }
                            MyCommand::Clear => {
                                self.cli.clear_screen();
                            }
                            MyCommand::Empty => {
                                // Ligne vide, ne rien faire
                            }
                        }
                    }

                }
                Err(ReadlineError::Interrupted) => {
                    // Ctrl-C - Quitter proprement
                    println!("\n^C");
                    self.cli.reset_multiline_state();
                    println!("exiting...");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    // Ctrl-D - Quitter proprement
                    println!("\nexiting...");
                    break;
                }
                Err(err) => {
                    self.cli
                        .display_error(&format!("Erreur de lecture: {}", err));
                    break;
                }
            }
        }

        // Sauvegarde l'historique avant de quitter
        self.cli.save_history();

        Ok(())
    }
}

pub fn repl() {
    match RRepl::new() {
        Ok(mut repl) => {
            if let Err(e) = repl.run() {
                eprintln!("❌ Erreur REPL: {}", e);
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("❌ Impossible de démarrer le processus R: {}", e);
            eprintln!("   Vérifiez que R est installé et dans le PATH");
            std::process::exit(1);
        }
    }
}
