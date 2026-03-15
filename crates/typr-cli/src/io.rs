//! I/O utilities for TypR CLI
//!
//! Provides utilities for:
//! - Reading files
//! - Executing R scripts
//! - Executing TypeScript (for future targets)

#![allow(dead_code)]

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use typr_core::components::context::config::Environment;

pub fn get_os_file(file: &str) -> String {
    if cfg!(windows) {
        file.replace("\\", r"\")
    } else {
        file.to_string()
    }
}

pub fn read_file(path: &PathBuf) -> Option<String> {
    let file = get_os_file(path.to_str().unwrap());
    fs::read_to_string(&file).ok()
}

pub fn read_file_from_name(name: &str, environment: Environment) -> String {
    let base = match environment {
        Environment::StandAlone | Environment::Repl | Environment::Wasm => "",
        Environment::Project => "TypR/",
    };
    let file = get_os_file(&format!("{}{}.ty", base, name));
    fs::read_to_string(&file).unwrap_or_else(|_| panic!("Can't Read file {}", name))
}

pub fn execute_r_with_path(execution_path: &PathBuf, file_name: &str) {
    match Command::new("Rscript")
        .current_dir(execution_path)
        .arg(get_os_file(file_name))
        .output()
    {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);

            if output.status.success() {
                println!("Execution: \n{}", stdout);
            } else {
                println!("Error (code {}): \n{}", output.status, stderr);
                if !stdout.is_empty() {
                    println!("Standard output: \n{}", stdout);
                }
            }
        }
        Err(e) => {
            println!("Failed to execute command: {}", e);
        }
    }
}

pub fn execute_r_with_path2(execution_path: &PathBuf, file_name: &str) -> String {
    match Command::new("Rscript")
        .current_dir(execution_path)
        .arg(get_os_file(file_name))
        .output()
    {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);

            if output.status.success() {
                format!("{}", stdout)
            } else {
                println!("Error (code {}): \n{}", output.status, stderr);
                if !stdout.is_empty() {
                    format!("Standard output: \n{}", stdout)
                } else {
                    "".to_string()
                }
            }
        }
        Err(e) => {
            format!("Failed to execute command: {}", e)
        }
    }
}
