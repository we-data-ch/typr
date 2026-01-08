use std::io::{self, BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;

/// Gestionnaire du processus R externe
struct RProcess {
    child: Child,
    stdin: ChildStdin,
    output_receiver: Receiver<String>,
}

impl RProcess {
    /// Démarre un nouveau processus R
    fn new() -> io::Result<Self> {
        let mut child = Command::new("R")
            .args(&["--vanilla", "--quiet", "--no-save", "--no-restore"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdin = child.stdin.take().expect("Failed to open stdin");
        let stdout = child.stdout.take().expect("Failed to open stdout");
        let stderr = child.stderr.take().expect("Failed to open stderr");

        // Canal pour recevoir les sorties
        let (tx, rx) = mpsc::channel();

        // Thread pour lire stdout
        Self::spawn_output_reader(stdout, tx.clone(), "stdout");
        
        // Thread pour lire stderr
        //Self::spawn_output_reader(stderr, tx, "stderr");

        Ok(RProcess {
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

    /// Envoie une commande au processus R
    fn send_command(&mut self, command: &str) -> io::Result<()> {
        writeln!(self.stdin, "{}", command)?;
        self.stdin.flush()?;
        Ok(())
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
                    // Ignore les lignes vides et les prompts R
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

    /// Termine le processus R
    fn terminate(&mut self) -> io::Result<()> {
        let _ = self.send_command("q(save='no')");
        self.child.kill()?;
        Ok(())
    }
}

impl Drop for RProcess {
    fn drop(&mut self) {
        let _ = self.terminate();
    }
}

/// REPL principal
struct RRepl {
    r_process: RProcess,
}

impl RRepl {
    /// Crée un nouveau REPL
    fn new() -> io::Result<Self> {
        println!("🚀 Démarrage du REPL R...\n");
        let r_process = RProcess::new()?;
        println!("✓ Processus R initialisé");
        println!("  Tapez 'exit' ou 'quit' pour quitter\n");
        
        Ok(RRepl { r_process })
    }

    /// Exécute la boucle REPL
    fn run(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut line_buffer = String::new();
        let mut in_multiline = false;

        loop {
            // Affiche le prompt
            if in_multiline {
                print!("... ");
            } else {
                print!("R> ");
            }
            io::stdout().flush()?;

            // Lit l'entrée utilisateur
            line_buffer.clear();
            stdin.read_line(&mut line_buffer)?;
            let input = line_buffer.trim();

            // Gère les commandes spéciales
            if !in_multiline && (input == "exit" || input == "quit") {
                println!("\n👋 Au revoir !");
                break;
            }

            if input.is_empty() {
                continue;
            }

            // Détecte les commandes multi-lignes (blocs non fermés)
            let open_braces = input.matches('{').count();
            let close_braces = input.matches('}').count();
            let open_parens = input.matches('(').count();
            let close_parens = input.matches(')').count();

            in_multiline = (open_braces > close_braces) || (open_parens > close_parens);

            // Envoie la commande à R
            self.r_process.send_command(input)?;

            if !in_multiline {
                // Attend et collecte les résultats
                std::thread::sleep(std::time::Duration::from_millis(50));
                let outputs = self.r_process.collect_output(500);

                // Affiche les résultats
                for output in outputs {
                    println!("{}", output);
                }
            }
        }

        Ok(())
    }
}

pub fn repl() {
    match RRepl::new() {
        Ok(mut repl) => {
            if let Err(e) = repl.run() {
                eprintln!("Erreur REPL: {}", e);
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
