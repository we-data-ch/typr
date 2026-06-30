#![allow(dead_code)]

use std::io::{IsTerminal, Write};
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

const FRAMES: &[char] = &['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];

/// A progress step with an animated spinner on TTY, plain text on non-TTY.
///
/// Call `done()`, `fail()`, or `clear()` to stop — consuming `self`.
/// Dropping without calling one of these calls `clear()` automatically.
pub struct Step {
    label: String,
    start: Instant,
    stop: Arc<AtomicBool>,
    handle: Option<JoinHandle<()>>,
    is_tty: bool,
    finished: bool,
}

impl Step {
    pub fn new(label: &str) -> Self {
        let is_tty = std::io::stderr().is_terminal();
        let stop = Arc::new(AtomicBool::new(false));

        let handle = if is_tty {
            let stop_clone = stop.clone();
            let label_clone = label.to_string();
            Some(thread::spawn(move || {
                let mut frame = 0usize;
                loop {
                    if stop_clone.load(Ordering::Relaxed) {
                        break;
                    }
                    eprint!("\r{} {}...", FRAMES[frame % FRAMES.len()], label_clone);
                    let _ = std::io::stderr().flush();
                    frame += 1;
                    thread::sleep(Duration::from_millis(80));
                }
            }))
        } else {
            eprint!("  {}...", label);
            let _ = std::io::stderr().flush();
            None
        };

        Self {
            label: label.to_string(),
            start: Instant::now(),
            stop,
            handle,
            is_tty,
            finished: false,
        }
    }

    fn stop_thread(&mut self) {
        self.stop.store(true, Ordering::Relaxed);
        if let Some(h) = self.handle.take() {
            let _ = h.join();
        }
        self.finished = true;
    }

    /// Stop the spinner and print a success line with timing.
    pub fn done(mut self) {
        self.stop_thread();
        let ms = self.start.elapsed().as_millis();
        if self.is_tty {
            eprintln!(
                "\r\x1b[K\x1b[32m✓\x1b[0m {}  \x1b[2m({} ms)\x1b[0m",
                self.label, ms
            );
        } else {
            eprintln!(" done ({} ms)", ms);
        }
    }

    /// Stop the spinner and print a failure line.
    pub fn fail(mut self) {
        self.stop_thread();
        if self.is_tty {
            eprintln!("\r\x1b[K\x1b[31m✗\x1b[0m {}", self.label);
        } else {
            eprintln!(" failed");
        }
    }

    /// Stop the spinner and clear the line without printing anything.
    /// Use this before printing program output (e.g. after running Rscript).
    pub fn clear(mut self) {
        self.stop_thread();
        if self.is_tty {
            eprint!("\r\x1b[K");
            let _ = std::io::stderr().flush();
        } else {
            eprintln!();
        }
    }
}

impl Drop for Step {
    fn drop(&mut self) {
        if !self.finished {
            self.stop_thread();
            if self.is_tty {
                eprint!("\r\x1b[K");
                let _ = std::io::stderr().flush();
            }
        }
    }
}
