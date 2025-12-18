#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio, ChildStdin, ChildStdout};

pub struct RSession {
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

impl RSession {
    pub fn new() -> std::io::Result<Self> {
        let mut child = Command::new("R")
            .arg("--vanilla")
            .arg("--quiet")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdin = child.stdin.take().expect("Failed to open stdin");
        let stdout = BufReader::new(child.stdout.take().expect("Failed to open stdout"));

        Ok(RSession { stdin, stdout })
    }

    pub fn execute(&mut self, command: &str) -> std::io::Result<String> {
        // Envoyer la commande à R avec un marqueur de fin immédiatement après
        writeln!(self.stdin, "{}", command)?;
        writeln!(self.stdin, "cat('__END_OUTPUT__\\n')")?;
        self.stdin.flush()?;

        // Lire la réponse
        let mut output = String::new();
        let mut line = String::new();
        let mut started = false;

        loop {
            line.clear();
            self.stdout.read_line(&mut line)?;
            
            // Détecter le marqueur de fin
            if line.contains("__END_OUTPUT__\\n") {
                break;
            }
            
            // Ignorer les lignes de prompt R (qui commencent par >)
            if line.trim().starts_with('>') && !started {
                continue;
            }

            let _ = 3;
            
            started = true;
            output.push_str(&line);
        }

        Ok(output.trim().to_string().replace("__END_OUTPUT__\n", ""))
    }
}

pub fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    
    // Initialiser la session R
    let mut r_session = match RSession::new() {
        Ok(session) => session,
        Err(e) => {
            eprintln!("Erreur lors du démarrage de R: {}", e);
            eprintln!("Assurez-vous que R est installé et accessible dans le PATH");
            return Ok(());
        }
    };

    println!("REPL R connecté. Tapez vos commandes R ci-dessous.");
    println!("Utilisez CTRL-C ou CTRL-D pour quitter.\n");

    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("R> ");
        match readline {
            Ok(line) => {
                let trimmed = line.trim();
                
                // Ignorer les lignes vides
                if trimmed.is_empty() {
                    continue;
                }

                rl.add_history_entry(line.as_str())?;
                
                // Exécuter la commande dans R
                match r_session.execute(&line) {
                    Ok(output) => {
                        if !output.is_empty() {
                            println!("{}", output);
                        }
                    }
                    Err(e) => {
                        eprintln!("Erreur d'exécution: {}", e);
                    }
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("\nCTRL-C - Arrêt du REPL");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("\nCTRL-D - Arrêt du REPL");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }

    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt").ok();
    
    Ok(())
}
