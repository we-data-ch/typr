//! Interactive REPL for TypR
//!
//! Provides an interactive Read-Eval-Print-Loop with:
//! - Syntax highlighting
//! - Line editing with rustyline
//! - History support
//! - Type inference display

use crate::io::execute_r_with_path2;
use crate::project::{write_header, write_to_r_lang};
use rustyline::completion::Completer;
use rustyline::error::ReadlineError;
use rustyline::highlight::CmdKind;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::Helper;
use rustyline::{Config, Editor};
use std::borrow::Cow;
use std::fs;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use typr_core::components::context::config::Environment;
use typr_core::components::context::Context;
use typr_core::utils::fluent_parser::FluentParser;

// ANSI color codes
mod colors {
    pub const RESET: &str = "\x1b[0m";
    pub const KEYWORD: &str = "\x1b[35m"; // Magenta for keywords
    pub const FUNCTION: &str = "\x1b[36m"; // Cyan for functions
    pub const STRING: &str = "\x1b[32m"; // Green for strings
    pub const NUMBER: &str = "\x1b[33m"; // Yellow for numbers
    pub const COMMENT: &str = "\x1b[90m"; // Gray for comments
    pub const OPERATOR: &str = "\x1b[37m"; // White for operators
    pub const BRACKET: &str = "\x1b[93m"; // Light yellow for brackets
    pub const ERROR: &str = "\x1b[91m"; // Red for errors
    pub const OUTPUT: &str = "\x1b[34m"; // Blue for output
}

/// Highlighter for R/TypR language
#[derive(Clone)]
struct RHighlighter;

impl RHighlighter {
    fn new() -> Self {
        RHighlighter
    }

    fn is_r_keyword(word: &str) -> bool {
        matches!(
            word,
            "if" | "else"
                | "while"
                | "for"
                | "in"
                | "repeat"
                | "break"
                | "next"
                | "function"
                | "return"
                | "TRUE"
                | "FALSE"
                | "true"
                | "false"
                | "NULL"
                | "NA"
                | "NaN"
                | "Inf"
                | "library"
                | "require"
                | "source"
                | "let"
                | "type"
                | "fn"
        )
    }

    fn is_r_function(word: &str) -> bool {
        matches!(
            word,
            "print"
                | "cat"
                | "paste"
                | "paste0"
                | "length"
                | "sum"
                | "mean"
                | "median"
                | "sd"
                | "var"
                | "min"
                | "max"
                | "range"
                | "c"
                | "list"
                | "data.frame"
                | "matrix"
                | "array"
                | "factor"
                | "as.numeric"
                | "as.character"
                | "as.logical"
                | "str"
                | "summary"
                | "head"
                | "tail"
                | "dim"
                | "nrow"
                | "ncol"
                | "names"
                | "colnames"
                | "rownames"
                | "seq"
                | "rep"
                | "sort"
                | "order"
                | "unique"
                | "table"
                | "subset"
                | "merge"
                | "rbind"
                | "cbind"
                | "apply"
                | "lapply"
                | "sapply"
                | "tapply"
        )
    }

    fn highlight_code(code: &str) -> String {
        let mut result = String::new();
        let mut chars = code.chars().peekable();
        let mut in_string = false;
        let mut string_delim = ' ';
        let mut in_comment = false;
        let mut current_word = String::new();

        while let Some(ch) = chars.next() {
            // Handle comments
            if ch == '#' && !in_string {
                in_comment = true;
                if !current_word.is_empty() {
                    result.push_str(&Self::colorize_word(&current_word));
                    current_word.clear();
                }
                result.push_str(colors::COMMENT);
                result.push(ch);
                continue;
            }

            if in_comment {
                result.push(ch);
                if ch == '\n' {
                    result.push_str(colors::RESET);
                    in_comment = false;
                }
                continue;
            }

            // Handle strings
            if (ch == '"' || ch == '\'') && !in_string {
                if !current_word.is_empty() {
                    result.push_str(&Self::colorize_word(&current_word));
                    current_word.clear();
                }
                in_string = true;
                string_delim = ch;
                result.push_str(colors::STRING);
                result.push(ch);
                continue;
            }

            if in_string {
                result.push(ch);
                if ch == string_delim && chars.peek() != Some(&'\\') {
                    in_string = false;
                    result.push_str(colors::RESET);
                }
                continue;
            }

            // Handle numbers
            if ch.is_numeric() || (ch == '.' && chars.peek().map_or(false, |c| c.is_numeric())) {
                if !current_word.is_empty() {
                    result.push_str(&Self::colorize_word(&current_word));
                    current_word.clear();
                }
                result.push_str(colors::NUMBER);
                result.push(ch);
                while let Some(&next_ch) = chars.peek() {
                    if next_ch.is_numeric() || next_ch == '.' || next_ch == 'e' || next_ch == 'E' {
                        result.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                result.push_str(colors::RESET);
                continue;
            }

            // Handle operators and delimiters
            if "+-*/<>=!&|:".contains(ch) {
                if !current_word.is_empty() {
                    result.push_str(&Self::colorize_word(&current_word));
                    current_word.clear();
                }
                result.push_str(colors::OPERATOR);
                result.push(ch);
                // Handle multi-character operators
                if let Some(&next_ch) = chars.peek() {
                    if matches!(
                        (ch, next_ch),
                        ('<', '-')
                            | ('-', '>')
                            | ('=', '=')
                            | ('!', '=')
                            | ('<', '=')
                            | ('>', '=')
                            | ('&', '&')
                            | ('|', '|')
                    ) {
                        result.push(chars.next().unwrap());
                    }
                }
                result.push_str(colors::RESET);
                continue;
            }

            // Handle parentheses and brackets
            if "()[]{}".contains(ch) {
                if !current_word.is_empty() {
                    result.push_str(&Self::colorize_word(&current_word));
                    current_word.clear();
                }
                result.push_str(colors::BRACKET);
                result.push(ch);
                result.push_str(colors::RESET);
                continue;
            }

            // Accumulate characters to form words
            if ch.is_alphanumeric() || ch == '_' || ch == '.' {
                current_word.push(ch);
            } else {
                if !current_word.is_empty() {
                    result.push_str(&Self::colorize_word(&current_word));
                    current_word.clear();
                }
                result.push(ch);
            }
        }

        // Process the last word
        if !current_word.is_empty() {
            result.push_str(&Self::colorize_word(&current_word));
        }

        if in_comment {
            result.push_str(colors::RESET);
        }

        result
    }

    fn colorize_word(word: &str) -> String {
        if Self::is_r_keyword(word) {
            format!("{}{}{}", colors::KEYWORD, word, colors::RESET)
        } else if Self::is_r_function(word) {
            format!("{}{}{}", colors::FUNCTION, word, colors::RESET)
        } else {
            word.to_string()
        }
    }
}

impl Highlighter for RHighlighter {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        Cow::Owned(Self::highlight_code(line))
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _cmd_kind: CmdKind) -> bool {
        true
    }
}

impl Hinter for RHighlighter {
    type Hint = String;
}

impl Completer for RHighlighter {
    type Candidate = String;
}

impl Validator for RHighlighter {}

impl Helper for RHighlighter {}

/// Result of executing an R command
#[derive(Debug, Clone)]
pub struct ExecutionResult {
    pub output: Vec<String>,
}

/// State of user input
#[derive(Debug, Clone, Copy, PartialEq)]
enum InputState {
    Normal,
    MultiLine,
}

/// CLI interface manager with Rustyline
pub struct CliInterface {
    editor: Editor<RHighlighter, rustyline::history::DefaultHistory>,
    input_state: InputState,
    command_buffer: String,
    history_file: String,
}

impl CliInterface {
    /// Create a new CLI interface
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let config = Config::builder().auto_add_history(true).build();

        let highlighter = RHighlighter::new();
        let mut editor = Editor::with_config(config)?;
        editor.set_helper(Some(highlighter));

        // History file
        let history_file = std::env::var("HOME")
            .or_else(|_| std::env::var("USERPROFILE"))
            .map(|home| format!("{}/.r_repl_history", home))
            .unwrap_or_else(|_| ".r_repl_history".to_string());

        // Load existing history
        let _ = editor.load_history(&history_file);

        Ok(CliInterface {
            editor,
            input_state: InputState::Normal,
            command_buffer: String::new(),
            history_file,
        })
    }

    /// Display welcome message
    pub fn show_welcome(&self) {
        println!(
            "{}TypR REPL{}: 'exit' to quit",
            colors::KEYWORD,
            colors::RESET
        );
    }

    /// Read a line with appropriate prompt
    pub fn read_line(&mut self) -> Result<String, ReadlineError> {
        let prompt = match self.input_state {
            InputState::Normal => format!("{}TypR>{} ", colors::KEYWORD, colors::RESET),
            InputState::MultiLine => format!("{}...{} ", colors::OPERATOR, colors::RESET),
        };

        self.editor.readline(&prompt)
    }

    /// Process user input and return a command if complete
    pub fn process_input(&mut self, input: &str) -> Option<MyCommand> {
        let trimmed = input.trim();

        // Special commands in normal mode
        if self.input_state == InputState::Normal {
            match trimmed {
                "exit" | "quit" => return Some(MyCommand::Exit),
                "clear" => return Some(MyCommand::Clear),
                "" => return Some(MyCommand::Empty),
                _ => {}
            }
        }

        // Multi-line buffer management
        if self.input_state == InputState::MultiLine {
            self.command_buffer.push('\n');
        }
        self.command_buffer.push_str(trimmed);

        // Check if the command is complete
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

    /// Check if a command is complete (all blocks closed)
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

    /// Display an execution result with colors
    pub fn display_result(&self, result: &ExecutionResult) {
        for line in &result.output {
            println!("{}{}{}", colors::OUTPUT, line, colors::RESET);
        }
    }

    /// Display an error message
    pub fn display_error(&self, error: &str) {
        eprintln!("{}Error: {}{}", colors::ERROR, error, colors::RESET);
    }

    /// Clear the screen
    pub fn clear_screen(&mut self) {
        self.editor.clear_screen().ok();
    }

    /// Save history
    pub fn save_history(&mut self) {
        if let Err(e) = self.editor.save_history(&self.history_file) {
            eprintln!("Warning: Unable to save history: {}", e);
        }
    }

    /// Reset multi-line state (useful after Ctrl-C)
    pub fn reset_multiline_state(&mut self) {
        self.input_state = InputState::Normal;
        self.command_buffer.clear();
    }
}

/// Commands interpreted by the CLI
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

    fn get_r_code(self, cmd: &str) -> (Self, String, String) {
        let (r_code, api) = self.api.push(cmd).run().next_r_code().unwrap();
        let r_type = api.get_last_type().pretty2();
        let res = Self {
            api: api.clone(),
            ..self
        };
        let saved_code = format!("{}\n{}", api.get_saved_r_code(), r_code);
        (res, saved_code, r_type)
    }

    fn run_r_code(context: Context, r_code: &str, r_type: &str) -> String {
        let dir = PathBuf::from(".");
        let r_file_name = ".repl.R";
        let _ = fs::remove_file(r_file_name);
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(r_file_name)
            .unwrap();
        let _ = file.write_all("source('a_std.R')\n".as_bytes());
        write_header(context, &dir, Environment::Repl);
        write_to_r_lang(r_code.to_string(), &dir, r_file_name, Environment::Repl);
        println!("{}{}{}", colors::NUMBER, r_type, colors::RESET);
        let res = execute_r_with_path2(&dir, r_file_name);
        res
    }

    fn execute(self, cmd: &str) -> Result<(Self, ExecutionResult), String> {
        let (new, r_code, r_type) = Self::get_r_code(self, cmd);
        let res = Self::run_r_code(new.api.context.clone(), &r_code, &r_type);
        Ok((new, ExecutionResult { output: vec![res] }))
    }
}

/// Main REPL that orchestrates the executor and CLI interface
pub struct RRepl {
    executor: TypRExecutor,
    cli: CliInterface,
}

impl RRepl {
    /// Create a new REPL
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let executor = TypRExecutor::new();
        let cli = CliInterface::new()?;

        Ok(RRepl { executor, cli })
    }

    /// Run the main REPL loop
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
                                }
                                Err(e) => {
                                    self.cli.display_error(&format!("Execution failed: {}", e))
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
                                // Empty line, do nothing
                            }
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    // Ctrl-C - Exit gracefully
                    println!("\n^C");
                    self.cli.reset_multiline_state();
                    println!("exiting...");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    // Ctrl-D - Exit gracefully
                    println!("\nexiting...");
                    break;
                }
                Err(err) => {
                    self.cli.display_error(&format!("Read error: {}", err));
                    break;
                }
            }
        }

        // Save history before exiting
        self.cli.save_history();

        Ok(())
    }
}

/// Start the REPL
pub fn start() {
    match RRepl::new() {
        Ok(mut repl) => {
            if let Err(e) = repl.run() {
                eprintln!("{}REPL error: {}{}", colors::ERROR, e, colors::RESET);
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!(
                "{}Unable to start R process: {}{}",
                colors::ERROR,
                e,
                colors::RESET
            );
            eprintln!("   Check that R is installed and in PATH");
            std::process::exit(1);
        }
    }
}
