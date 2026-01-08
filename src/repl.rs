use rustyline::error::ReadlineError;
use rustyline::{Config, DefaultEditor, Editor};
use std::io::{self, BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;

/// Résultat de l'exécution d'une commande R
#[derive(Debug, Clone)]
pub struct ExecutionResult {
    pub output: Vec<String>,
}

/// Gestionnaire du processus R externe
pub struct RExecutor {
    child: Child,
    stdin: ChildStdin,
    output_receiver: Receiver<String>,
}

impl RExecutor {
    /// Démarre un nouveau processus R
    pub fn new() -> io::Result<Self> {
        let mut child = Command::new("R")
            .args(&["--vanilla", "--quiet", "--no-save", "--no-restore"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdin = child.stdin.take().expect("Failed to open stdin");
        let stdout = child.stdout.take().expect("Failed to open stdout");

        let (tx, rx) = mpsc::channel();

        Self::spawn_output_reader(stdout, tx.clone(), "stdout");

        Ok(RExecutor {
            child,
            stdin,
            output_receiver: rx,
        })
    }

    /// Crée un thread pour lire les sorties du processus R
    fn spawn_output_reader(
        stream: ChildStdout,
        sender: Sender<String>,
        stream_type: &'static str,
    ) {
        thread::spawn(move || {
            let reader = BufReader::new(stream);
            for line in reader.lines() {
                match line {
                    Ok(content) => {
                        if sender.send(content).is_err() {
                            eprintln!("[{}] Channel closed", stream_type);
                            break;
                        }
                    }
                    Err(e) => {
                        eprintln!("[{}] Error reading: {}", stream_type, e);
                        break;
                    }
                }
            }
        });
    }

    /// Exécute une commande R et retourne le résultat
    pub fn execute(&mut self, command: &str) -> io::Result<ExecutionResult> {
        // Envoie la commande
        writeln!(self.stdin, "{}", command)?;
        self.stdin.flush()?;

        // Attend un court instant pour que R traite la commande
        thread::sleep(std::time::Duration::from_millis(50));

        // Collecte les sorties
        let outputs = self.collect_output(500);

        Ok(ExecutionResult { output: outputs })
    }

    /// Récupère les sorties disponibles avec timeout
    fn collect_output(&self, timeout_ms: u64) -> Vec<String> {
        let mut outputs = Vec::new();
        let start = std::time::Instant::now();
        let timeout = std::time::Duration::from_millis(timeout_ms);

        loop {
            let remaining = timeout.saturating_sub(start.elapsed());
            if remaining.is_zero() {
                break;
            }

            match self.output_receiver.recv_timeout(remaining) {
                Ok(line) => {
                    if !line.trim().is_empty() && line != ">" && !line.starts_with("> ") {
                        outputs.push(line);
                    }
                }
                Err(mpsc::RecvTimeoutError::Timeout) => break,
                Err(mpsc::RecvTimeoutError::Disconnected) => {
                    eprintln!("Output channel disconnected");
                    break;
                }
            }
        }

        outputs
    }

    /// Vérifie si le processus R est toujours actif
    pub fn is_alive(&mut self) -> bool {
        self.child.try_wait().map(|s| s.is_none()).unwrap_or(false)
    }

    /// Termine le processus R proprement
    pub fn terminate(&mut self) -> io::Result<()> {
        let _ = writeln!(self.stdin, "q(save='no')");
        let _ = self.stdin.flush();
        thread::sleep(std::time::Duration::from_millis(100));
        self.child.kill()?;
        Ok(())
    }
}

impl Drop for RExecutor {
    fn drop(&mut self) {
        let _ = self.terminate();
    }
}

// ============================================================================
// MODULE: CLI - Gestion de l'interface utilisateur avec Rustyline
// ============================================================================

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
        println!("R REPL");
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

// ============================================================================
// MODULE: REPL - Orchestration de l'application
// ============================================================================

/// REPL principal qui orchestre l'exécuteur et l'interface CLI
pub struct RRepl {
    executor: RExecutor,
    cli: CliInterface,
}

impl RRepl {
    /// Crée un nouveau REPL
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let executor = RExecutor::new()?;
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
                            MyCommand::Execute(cmd) => match self.executor.execute(&cmd) {
                                Ok(result) => self.cli.display_result(&result),
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

                    // Vérifie que le processus R est toujours actif
                    if !self.executor.is_alive() {
                        self.cli
                            .display_error("Le processus R s'est arrêté inopinément");
                        break;
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    // Ctrl-C - Quitter proprement
                    println!("\n^C");
                    self.cli.reset_multiline_state();
                    println!("👋 Au revoir !");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    // Ctrl-D - Quitter proprement
                    println!("\n👋 Au revoir !");
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
