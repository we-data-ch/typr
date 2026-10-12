//! `typr tutor`: the terminal front end of the TypR tutor, in the spirit of
//! `vimtutor` — no AI needed.
//!
//! The lessons, their grading and their hints live in `typr_mcp::tutor`,
//! shared with the MCP server's `tutor_*` tools, so a learner gets the same
//! verdict from the terminal and from an AI assistant. This module only owns
//! the workspace: a folder holding one editable `.ty` file per lesson plus a
//! small progress file.

use std::collections::BTreeMap;
use std::fs;
use std::io::IsTerminal;
use std::path::Path;

use serde::{Deserialize, Serialize};
use typr_mcp::tutor::{self, Lesson, LESSONS};

const PROGRESS_FILE: &str = ".tutor-progress.json";

#[derive(Default, Serialize, Deserialize)]
struct Progress {
    /// Lesson numbers passed at least once.
    passed: Vec<usize>,
    /// Hints already shown, per lesson number.
    hints: BTreeMap<usize, usize>,
}

impl Progress {
    fn load(dir: &Path) -> Progress {
        fs::read_to_string(dir.join(PROGRESS_FILE))
            .ok()
            .and_then(|s| serde_json::from_str(&s).ok())
            .unwrap_or_default()
    }

    fn save(&self, dir: &Path) {
        if let Ok(json) = serde_json::to_string_pretty(self) {
            let _ = fs::write(dir.join(PROGRESS_FILE), json);
        }
    }

    /// The first lesson not passed yet, or the last one when all are.
    fn current(&self) -> &'static Lesson {
        LESSONS
            .iter()
            .find(|l| !self.passed.contains(&l.number))
            .unwrap_or_else(|| LESSONS.last().unwrap())
    }
}

struct Style {
    on: bool,
}

impl Style {
    fn new() -> Style {
        Style {
            on: std::io::stdout().is_terminal() && std::env::var_os("NO_COLOR").is_none(),
        }
    }
    fn paint(&self, code: &str, text: &str) -> String {
        if self.on {
            format!("\x1b[{code}m{text}\x1b[0m")
        } else {
            text.to_string()
        }
    }
    fn bold(&self, t: &str) -> String {
        self.paint("1", t)
    }
    fn green(&self, t: &str) -> String {
        self.paint("32", t)
    }
    fn red(&self, t: &str) -> String {
        self.paint("31", t)
    }
    fn yellow(&self, t: &str) -> String {
        self.paint("33", t)
    }
    fn cyan(&self, t: &str) -> String {
        self.paint("36", t)
    }
    fn dim(&self, t: &str) -> String {
        self.paint("2", t)
    }
}

fn lesson_or_exit(number: usize) -> &'static Lesson {
    tutor::find(number).unwrap_or_else(|| {
        eprintln!("error: no lesson {number}: lessons are numbered 1 to {}", LESSONS.len());
        std::process::exit(1);
    })
}

/// Create the workspace if needed. Returns true when it was just created.
fn ensure_workspace(dir: &Path) -> bool {
    if dir.join(PROGRESS_FILE).exists() {
        return false;
    }
    if let Err(e) = fs::create_dir_all(dir) {
        eprintln!("error: cannot create {}: {e}", dir.display());
        std::process::exit(1);
    }
    for lesson in LESSONS {
        let path = dir.join(lesson.file_name());
        // Never overwrite work: an existing folder may hold edited lessons.
        if !path.exists() {
            if let Err(e) = fs::write(&path, lesson.starter) {
                eprintln!("error: cannot write {}: {e}", path.display());
                std::process::exit(1);
            }
        }
    }
    Progress::default().save(dir);
    true
}

fn require_workspace(dir: &Path) {
    if !dir.join(PROGRESS_FILE).exists() {
        eprintln!(
            "error: no tutor workspace in {} — run `typr tutor` first to create it.",
            dir.display()
        );
        std::process::exit(1);
    }
}

fn dir_arg(dir: &Path) -> String {
    if dir == Path::new("typr-tutor") {
        String::new()
    } else {
        format!(" --dir {}", dir.display())
    }
}

fn print_lesson_card(s: &Style, dir: &Path, lesson: &Lesson) {
    println!(
        "  {} {}",
        s.bold(&format!("Lesson {}/{}:", lesson.number, LESSONS.len())),
        s.bold(lesson.title)
    );
    println!("  {}", lesson.goal);
    println!();
    println!(
        "  Open   {}",
        s.cyan(&dir.join(lesson.file_name()).display().to_string())
    );
    println!("  in your editor, follow the comments, save, then run:");
    println!();
    println!("      {}", s.cyan(&format!("typr tutor check{}", dir_arg(dir))));
}

/// `typr tutor`: create the workspace (or resume it) and say what to do next.
pub fn start(dir: &Path) {
    let s = Style::new();
    let created = ensure_workspace(dir);
    let progress = Progress::load(dir);
    println!();
    if created {
        println!("  {}", s.bold("Welcome to the TypR tutor!"));
        println!();
        println!(
            "  {} short lessons, about 30 minutes in all. Each lesson is a TypR file:",
            LESSONS.len()
        );
        println!("  the comments teach, the code is yours to complete. The compiler grades");
        println!("  your answers, and every lesson ends with the documentation pages that");
        println!("  take you further. Nothing can break: `typr tutor reset N` restores a lesson.");
        println!();
        println!("  Lessons written to {}", s.cyan(&dir.display().to_string()));
    } else {
        println!(
            "  {} {} of {} lessons done.",
            s.bold("Welcome back!"),
            progress.passed.len(),
            LESSONS.len()
        );
    }
    println!();
    if progress.passed.len() == LESSONS.len() {
        println!("  {}", s.green("You have completed every lesson."));
        println!("  Revisit any of them with `typr tutor check N`, or keep exploring:");
        print_docs(&s, LESSONS.last().unwrap());
    } else {
        print_lesson_card(&s, dir, progress.current());
    }
    println!();
    println!(
        "  {}",
        s.dim("Other commands: typr tutor list | hint | reset N | solution N")
    );
    println!(
        "  {}",
        s.dim("Prefer learning with an AI assistant? See `typr tutor --help`.")
    );
    println!();
}

fn print_docs(s: &Style, lesson: &Lesson) {
    println!();
    println!("  {}", s.bold("Explore further:"));
    for d in lesson.docs {
        println!("    • {}  {}", d.title, s.cyan(&d.url()));
    }
}

/// `typr tutor check [N]`: grade a lesson file (default: the current lesson).
pub fn check(dir: &Path, number: Option<usize>) {
    require_workspace(dir);
    let s = Style::new();
    let mut progress = Progress::load(dir);
    let lesson = number.map(lesson_or_exit).unwrap_or_else(|| progress.current());
    let path = dir.join(lesson.file_name());
    let source = fs::read_to_string(&path).unwrap_or_else(|e| {
        eprintln!(
            "error: cannot read {}: {e} (restore it with `typr tutor reset {}`)",
            path.display(),
            lesson.number
        );
        std::process::exit(1);
    });

    let grade = tutor::grade(lesson, &source);
    println!();
    println!("  {}", s.bold(&format!("Lesson {}: {}", lesson.number, lesson.title)));
    println!();

    if !grade.diagnostics.is_empty() {
        let label = if grade.passed {
            "The compiler says (as expected):"
        } else {
            "The compiler says:"
        };
        println!("  {label}");
        for d in &grade.diagnostics {
            for (i, line) in d.message.lines().enumerate() {
                if i == 0 {
                    println!("    {} {}", s.yellow(&format!("[{}]", d.code)), line);
                } else {
                    println!("           {line}");
                }
            }
        }
        for e in &grade.explanations {
            println!();
            println!("  {} {}", s.yellow(&e.code), s.bold(&e.title));
            for line in wrap(&e.explanation, 72) {
                println!("    {}", s.dim(&line));
            }
        }
        println!();
    }

    if grade.passed {
        println!("  {} {}", s.green("✓"), s.green(&s.bold("Lesson passed!")));
        if !progress.passed.contains(&lesson.number) {
            progress.passed.push(lesson.number);
            progress.passed.sort_unstable();
            progress.save(dir);
        }
        print_docs(&s, lesson);
        println!();
        if progress.passed.len() < LESSONS.len() {
            println!("  {}", s.bold("Next:"));
            print_lesson_card(&s, dir, progress.current());
        } else {
            {
                println!(
                    "  {}",
                    s.green(&s.bold("You finished the TypR tutor. Congratulations!"))
                );
                println!();
                println!("  Start a real project with  `typr new my_package`,");
                println!("  and keep the reference at hand:");
                println!("  {}", s.cyan("https://we-data-ch.github.io/typr.github.io/docs/"));
            }
        }
    } else {
        println!("  {} {}", s.red("✗"), s.bold("Not yet:"));
        for f in &grade.feedback {
            for (i, line) in wrap(f, 70).iter().enumerate() {
                println!("    {} {line}", if i == 0 { "-" } else { " " });
            }
        }
        println!();
        println!(
            "  {}",
            s.dim(&format!(
                "Stuck? `typr tutor hint{}` gives a hint ({} available).",
                lesson_arg(number, lesson),
                lesson.hints.len()
            ))
        );
    }
    println!();
    if !grade.passed {
        std::process::exit(1);
    }
}

fn lesson_arg(number: Option<usize>, lesson: &Lesson) -> String {
    number.map(|_| format!(" {}", lesson.number)).unwrap_or_default()
}

/// `typr tutor hint [N]`: reveal the next hint for a lesson.
pub fn hint(dir: &Path, number: Option<usize>) {
    require_workspace(dir);
    let s = Style::new();
    let mut progress = Progress::load(dir);
    let lesson = number.map(lesson_or_exit).unwrap_or_else(|| progress.current());
    let shown = progress.hints.get(&lesson.number).copied().unwrap_or(0);
    let level = (shown + 1).min(lesson.hints.len());
    progress.hints.insert(lesson.number, level);
    progress.save(dir);

    println!();
    println!("  {}", s.bold(&format!("Lesson {}: {}", lesson.number, lesson.title)));
    for (i, h) in lesson.hints.iter().take(level).enumerate() {
        println!();
        println!("  {} {h}", s.yellow(&format!("Hint {}/{}:", i + 1, lesson.hints.len())));
    }
    println!();
    if level == lesson.hints.len() {
        println!(
            "  {}",
            s.dim(&format!(
                "That was the last hint. As a last resort: `typr tutor solution {}`.",
                lesson.number
            ))
        );
    } else {
        println!("  {}", s.dim("Run the same command again for the next hint."));
    }
    println!();
}

/// `typr tutor list`: every lesson, with progress.
pub fn list(dir: &Path) {
    let s = Style::new();
    let progress = Progress::load(dir);
    let current = progress.current().number;
    println!();
    for l in LESSONS {
        let mark = if progress.passed.contains(&l.number) {
            s.green("✓")
        } else if l.number == current {
            s.yellow("▸")
        } else {
            " ".to_string()
        };
        println!("  {mark} {:>2}. {:<38} {}", l.number, l.title, s.dim(&l.file_name()));
    }
    println!();
    println!("  {} of {} lessons done.", progress.passed.len(), LESSONS.len());
    println!();
}

/// `typr tutor reset N`: restore a lesson file to its original content.
pub fn reset(dir: &Path, number: usize) {
    require_workspace(dir);
    let lesson = lesson_or_exit(number);
    let path = dir.join(lesson.file_name());
    if let Err(e) = fs::write(&path, lesson.starter) {
        eprintln!("error: cannot write {}: {e}", path.display());
        std::process::exit(1);
    }
    println!("Restored {}", path.display());
}

/// `typr tutor solution N`: print a reference solution.
pub fn solution(number: usize) {
    let s = Style::new();
    let lesson = lesson_or_exit(number);
    println!();
    println!(
        "  {}",
        s.dim("A solution, not the solution: if yours passes `typr tutor check`, it is right too.")
    );
    println!();
    for line in lesson.solution.lines() {
        println!("    {line}");
    }
    println!();
}

fn wrap(text: &str, width: usize) -> Vec<String> {
    let mut lines = Vec::new();
    let mut line = String::new();
    for word in text.split_whitespace() {
        if !line.is_empty() && line.len() + 1 + word.len() > width {
            lines.push(std::mem::take(&mut line));
        }
        if !line.is_empty() {
            line.push(' ');
        }
        line.push_str(word);
    }
    if !line.is_empty() {
        lines.push(line);
    }
    lines
}
