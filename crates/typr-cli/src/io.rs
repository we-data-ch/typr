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
    match fs::read_to_string(&file) {
        Ok(res) => Some(res),
        _ => None,
    }
}

pub fn read_file_from_name(name: &str, environment: Environment) -> String {
    let base = match environment {
        Environment::StandAlone | Environment::Repl | Environment::Wasm => "",
        Environment::Project => "TypR/",
    };
    let file = get_os_file(&format!("{}{}.ty", base, name));
    fs::read_to_string(&file).expect(&format!("Can't Read file {}", name))
}

pub fn execute_r() -> () {
    match Command::new("Rscript").arg(get_os_file("app.R")).output() {
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

pub fn execute_r_with_path(execution_path: &PathBuf, file_name: &str) -> () {
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

pub fn execute_typescript() -> () {
    println!("TypeScript compilation: ");

    // Compile TypeScript to JavaScript
    let tsc_output = Command::new("tsc")
        .arg("app.ts")
        .output()
        .expect("Failed during TypeScript compilation");

    if !tsc_output.status.success() {
        let stderr = String::from_utf8_lossy(&tsc_output.stderr);
        println!("TypeScript compilation error: {}", stderr);
        return;
    }

    println!("JavaScript execution: ");

    // Execute compiled JavaScript file
    let node_output = Command::new("node")
        .arg("app.js")
        .output()
        .expect("Failed during Node.js execution");

    let stdout = String::from_utf8_lossy(&node_output.stdout);
    let stderr = String::from_utf8_lossy(&node_output.stderr);

    if !node_output.status.success() {
        println!("JavaScript execution error: {}", stderr);
    } else {
        println!("{}", stdout);
        if !stderr.is_empty() {
            println!("Warnings: {}", stderr);
        }
    }
}
