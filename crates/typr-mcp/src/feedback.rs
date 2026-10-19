//! The feedback loop: a local interaction log, plus `report_issue`, which
//! turns a failure the agent ran into into a pre-filled GitHub issue.
//!
//! **Local log.** Every tool call and resource read is appended, one JSON
//! object per line, to `<log dir>/<unix secs>-<pid>.jsonl` — one file per
//! server process, i.e. per MCP session. The log never leaves the machine:
//! it is there so the user (or the team, when the user chooses to share a
//! file) can replay what an agent tried, which diagnostics it hit, and where
//! it went in circles. Only the [`MAX_SESSIONS`] most recent files are kept.
//!
//! - `TYPR_MCP_LOG=off` (or `0`/`false`) disables it.
//! - `TYPR_MCP_LOG_DIR` overrides the directory; otherwise
//!   `$XDG_DATA_HOME/typr/mcp-logs`, falling back to
//!   `~/.local/share/typr/mcp-logs`.
//!
//! Logging is best-effort: a directory that can't be created or a write that
//! fails silently disables it rather than failing the tool call.
//!
//! **Reports.** `report_issue` never sends anything itself. It re-checks the
//! agent's *anonymized* repro in-process (so the team gets code that still
//! shows the bug, not the user's private code), writes the full report
//! next to the log, and returns a `github.com/.../issues/new?...` URL the
//! user opens, reviews and submits — a human is always the one who publishes.

use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{check_source, Diagnostic};

/// How many session logs to keep; older ones are deleted when a new session
/// starts.
const MAX_SESSIONS: usize = 50;

/// Where the "new issue" link points.
const ISSUES_NEW_URL: &str = "https://github.com/we-data-ch/typr/issues/new";

/// GitHub rejects (414) or truncates pre-filled URLs well before browsers do;
/// past this many encoded bytes the body is cut and points at the local file.
const MAX_URL_LEN: usize = 7000;

fn now_ms() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis())
        .unwrap_or(0)
}

fn logging_enabled() -> bool {
    !matches!(
        std::env::var("TYPR_MCP_LOG").map(|v| v.to_lowercase()).as_deref(),
        Ok("off" | "0" | "false" | "no")
    )
}

fn log_dir() -> Option<PathBuf> {
    if let Some(dir) = std::env::var_os("TYPR_MCP_LOG_DIR") {
        return Some(PathBuf::from(dir));
    }
    let data_home = std::env::var_os("XDG_DATA_HOME")
        .map(PathBuf::from)
        .or_else(|| std::env::var_os("HOME").map(|h| PathBuf::from(h).join(".local/share")))?;
    Some(data_home.join("typr").join("mcp-logs"))
}

/// Delete all but the `keep - 1` newest `*.jsonl` session logs, leaving room
/// for the one about to be created. File names start with a unix timestamp,
/// so lexical order is chronological.
fn prune_old_sessions(dir: &Path, keep: usize) {
    let Ok(entries) = fs::read_dir(dir) else { return };
    let mut logs: Vec<PathBuf> = entries
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|ext| ext == "jsonl"))
        .collect();
    logs.sort();
    let excess = (logs.len() + 1).saturating_sub(keep);
    for path in logs.into_iter().take(excess) {
        let _ = fs::remove_file(path);
    }
}

/// Append-only JSONL log of one MCP session. A disabled log (`file: None`)
/// accepts every call and does nothing.
#[derive(Default)]
pub struct SessionLog {
    session_id: String,
    dir: Option<PathBuf>,
    file: Mutex<Option<File>>,
}

impl SessionLog {
    /// Open the log for a new session, honouring `TYPR_MCP_LOG`/`TYPR_MCP_LOG_DIR`.
    pub fn from_env() -> Self {
        if !logging_enabled() {
            return Self::default();
        }
        match log_dir() {
            Some(dir) => Self::open_in(dir),
            None => Self::default(),
        }
    }

    pub fn open_in(dir: PathBuf) -> Self {
        let session_id = format!("{}-{}", now_ms() / 1000, std::process::id());
        if fs::create_dir_all(&dir).is_err() {
            return Self::default();
        }
        prune_old_sessions(&dir, MAX_SESSIONS);
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(dir.join(format!("{session_id}.jsonl")))
            .ok();
        Self {
            session_id,
            dir: file.as_ref().map(|_| dir),
            file: Mutex::new(file),
        }
    }

    pub fn session_id(&self) -> &str {
        &self.session_id
    }

    #[cfg(test)]
    pub fn path(&self) -> Option<PathBuf> {
        self.dir.as_ref().map(|d| d.join(format!("{}.jsonl", self.session_id)))
    }

    /// Record one event: `kind` is `"tool"`, `"resource"` or `"report"`,
    /// `name` the tool/URI, `input`/`output` whatever the call received and
    /// returned.
    pub fn record(&self, kind: &str, name: &str, input: serde_json::Value, output: serde_json::Value, ms: u128) {
        let Ok(mut guard) = self.file.lock() else { return };
        let Some(file) = guard.as_mut() else { return };
        let line = serde_json::json!({
            "ts_ms": now_ms(),
            "kind": kind,
            "name": name,
            "duration_ms": ms,
            "input": input,
            "output": output,
        });
        if writeln!(file, "{line}").is_err() {
            *guard = None;
        }
    }
}

/// What went wrong, from the agent's point of view — the team triages by it.
#[derive(Debug, Clone, Copy, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum IssueKind {
    /// The compiler rejects code that should be valid TypR.
    FalsePositive,
    /// The compiler accepts code it should reject.
    FalseNegative,
    /// An internal error (`S000`) or a panic instead of a proper diagnostic.
    Crash,
    /// `build` produced R code that is wrong or fails to run.
    WrongOutput,
    /// The diagnostic is correct but its message or `explain` entry didn't
    /// help fix it.
    ConfusingMessage,
    /// A feature, function or doc page the agent needed doesn't exist.
    MissingFeatureOrDocs,
    Other,
}

impl IssueKind {
    fn label(self) -> &'static str {
        match self {
            IssueKind::FalsePositive => "valid code rejected",
            IssueKind::FalseNegative => "invalid code accepted",
            IssueKind::Crash => "internal error / crash",
            IssueKind::WrongOutput => "wrong R output",
            IssueKind::ConfusingMessage => "confusing diagnostic",
            IssueKind::MissingFeatureOrDocs => "missing feature or docs",
            IssueKind::Other => "other",
        }
    }
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ReportParams {
    /// One-line summary, e.g. "T012 on a record spread inside a generic function".
    pub title: String,
    pub kind: IssueKind,
    /// Minimal, ANONYMIZED TypR source that shows the problem. Never paste
    /// the user's code as-is: rename identifiers, replace string/number
    /// literals and strip unrelated lines until it is a small, generic
    /// example that still triggers the same behavior. It is re-checked
    /// before the report is built.
    pub repro: String,
    /// Diagnostic codes the ORIGINAL code produced (e.g. ["T012"]). The
    /// repro is checked against them, so the team knows it reproduces the
    /// same failure. Leave empty when the bug is that no error was raised.
    #[serde(default)]
    pub original_codes: Vec<String>,
    /// What should have happened instead (the expected diagnostic, R output, behavior...).
    pub expected: String,
    /// Optional extra context: what the agent was trying to do, what it already
    /// tried. Same rule as `repro`: nothing identifying about the user's project.
    #[serde(default)]
    pub notes: Option<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct ReportResult {
    /// True when the repro produces every code in `original_codes` (or, with
    /// no original codes, at least compiles to the same "no error" verdict
    /// `kind` implies). False means the repro drifted from the real bug:
    /// adjust it and report again rather than send a misleading issue.
    pub reproduced: bool,
    /// What `check` says about the repro right now.
    pub repro_diagnostics: Vec<Diagnostic>,
    /// Pre-filled "new issue" link. Give it to the user to open, review and
    /// submit — nothing has been sent.
    pub issue_url: String,
    /// Where the full report was saved locally, if logging is enabled.
    pub report_path: Option<String>,
    /// The issue body, in case the user prefers to paste it themselves.
    pub body: String,
}

/// Percent-encode everything but RFC 3986 unreserved characters.
fn url_encode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for b in s.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => out.push(b as char),
            _ => out.push_str(&format!("%{b:02X}")),
        }
    }
    out
}

fn issue_url(title: &str, body: &str) -> String {
    format!(
        "{ISSUES_NEW_URL}?labels=mcp-feedback&title={}&body={}",
        url_encode(title),
        url_encode(body)
    )
}

/// Does the repro behave like the original failure?
fn reproduces(kind: IssueKind, original_codes: &[String], diagnostics: &[Diagnostic]) -> bool {
    if original_codes.is_empty() {
        // "Should have been rejected" bugs reproduce when the compiler still
        // accepts the code; for the rest there is nothing to compare against.
        return match kind {
            IssueKind::FalseNegative | IssueKind::WrongOutput => diagnostics.is_empty(),
            _ => true,
        };
    }
    original_codes
        .iter()
        .all(|want| diagnostics.iter().any(|d| d.code.eq_ignore_ascii_case(want.trim())))
}

fn render_body(p: &ReportParams, reproduced: bool, diagnostics: &[Diagnostic], session_id: &str) -> String {
    let mut body = String::new();
    body.push_str(&format!("**Kind:** {}\n", p.kind.label()));
    body.push_str(&format!("**typr version:** {}\n", env!("CARGO_PKG_VERSION")));
    body.push_str(&format!("**Reported via:** typr mcp (session `{session_id}`)\n\n"));

    body.push_str("### Repro (anonymized)\n\n```typr\n");
    body.push_str(p.repro.trim_end());
    body.push_str("\n```\n\n");

    body.push_str("### Observed\n\n");
    if diagnostics.is_empty() {
        body.push_str("No diagnostics: `check` accepts this code.\n");
    } else {
        for d in diagnostics {
            body.push_str(&format!("- `{}` {}\n", d.code, d.message.trim()));
        }
    }
    if !p.original_codes.is_empty() {
        body.push_str(&format!(
            "\nOriginal code produced: {} — {}.\n",
            p.original_codes.join(", "),
            if reproduced {
                "reproduced by the snippet above"
            } else {
                "**NOT reproduced by the snippet above**"
            }
        ));
    }

    body.push_str("\n### Expected\n\n");
    body.push_str(p.expected.trim());
    body.push('\n');

    if let Some(notes) = p.notes.as_deref().filter(|n| !n.trim().is_empty()) {
        body.push_str("\n### Notes\n\n");
        body.push_str(notes.trim());
        body.push('\n');
    }
    body
}

/// Build (but never send) an issue report — see the module docs.
pub fn build_report(p: &ReportParams, log: &SessionLog) -> ReportResult {
    let repro_diagnostics = check_source(&p.repro).diagnostics;
    let reproduced = reproduces(p.kind, &p.original_codes, &repro_diagnostics);
    let session_id = if log.session_id().is_empty() {
        "unlogged"
    } else {
        log.session_id()
    };
    let body = render_body(p, reproduced, &repro_diagnostics, session_id);
    let title = format!("[mcp] {}", p.title.trim());

    let report_path = log.dir.as_ref().and_then(|dir| {
        let path = dir.join(format!("report-{}.md", now_ms()));
        fs::write(&path, format!("# {title}\n\n{body}")).ok()?;
        Some(path.display().to_string())
    });

    let mut url = issue_url(&title, &body);
    if url.len() > MAX_URL_LEN {
        let note = match &report_path {
            Some(path) => format!("\n\n_(truncated — the full report is in `{path}`)_\n"),
            None => "\n\n_(truncated)_\n".to_string(),
        };
        // Cut on a char boundary, conservatively: encoding grows text ≤ 9x.
        let mut cut = body.len().min(MAX_URL_LEN / 9);
        while !body.is_char_boundary(cut) {
            cut -= 1;
        }
        url = issue_url(&title, &format!("{}{note}", &body[..cut]));
    }

    ReportResult {
        reproduced,
        repro_diagnostics,
        issue_url: url,
        report_path,
        body,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn params(repro: &str, kind: IssueKind, codes: &[&str]) -> ReportParams {
        ReportParams {
            title: "test".into(),
            kind,
            repro: repro.into(),
            original_codes: codes.iter().map(|c| c.to_string()).collect(),
            expected: "should not happen".into(),
            notes: None,
        }
    }

    fn scratch_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("typr-mcp-test-{name}-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        dir
    }

    #[test]
    fn url_encode_keeps_unreserved_and_escapes_the_rest() {
        assert_eq!(url_encode("a-b_c.d~"), "a-b_c.d~");
        assert_eq!(url_encode("a b&c=d\n"), "a%20b%26c%3Dd%0A");
        assert_eq!(url_encode("é"), "%C3%A9");
    }

    #[test]
    fn repro_with_the_original_code_is_reproduced() {
        let p = params("let x: int <- \"oops\";", IssueKind::FalsePositive, &[]);
        let code = check_source(&p.repro).diagnostics[0].code.clone();
        let p = params(&p.repro, IssueKind::FalsePositive, &[&code]);
        let r = build_report(&p, &SessionLog::default());
        assert!(r.reproduced, "{:?}", r.repro_diagnostics);
        assert!(r.issue_url.starts_with(ISSUES_NEW_URL));
        assert!(r.body.contains(&code));
        assert!(r.report_path.is_none());
    }

    #[test]
    fn repro_that_drifted_is_flagged() {
        let p = params("let x: int <- 42;", IssueKind::FalsePositive, &["T001"]);
        let r = build_report(&p, &SessionLog::default());
        assert!(!r.reproduced);
        assert!(r.body.contains("NOT reproduced"));
    }

    #[test]
    fn false_negative_reproduces_when_the_code_is_still_accepted() {
        let p = params("let x: int <- 42;", IssueKind::FalseNegative, &[]);
        assert!(build_report(&p, &SessionLog::default()).reproduced);
    }

    #[test]
    fn oversized_report_still_yields_a_bounded_url() {
        let big = "let x: int <- 42;\n".repeat(2000);
        let p = params(&big, IssueKind::Other, &[]);
        let r = build_report(&p, &SessionLog::default());
        assert!(r.issue_url.len() <= MAX_URL_LEN + 500, "{}", r.issue_url.len());
        assert!(r.issue_url.contains("truncated"));
    }

    #[test]
    fn session_log_writes_one_json_line_per_event_and_saves_reports() {
        let dir = scratch_dir("log");
        let log = SessionLog::open_in(dir.clone());
        log.record(
            "tool",
            "check",
            serde_json::json!({"source": "x"}),
            serde_json::json!({"ok": true}),
            3,
        );
        log.record(
            "tool",
            "explain",
            serde_json::json!({"code": "T001"}),
            serde_json::json!({}),
            1,
        );
        let text = fs::read_to_string(log.path().unwrap()).unwrap();
        let lines: Vec<serde_json::Value> = text.lines().map(|l| serde_json::from_str(l).unwrap()).collect();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0]["name"], "check");
        assert_eq!(lines[1]["input"]["code"], "T001");

        let r = build_report(&params("let x: int <- 42;", IssueKind::Other, &[]), &log);
        assert!(Path::new(r.report_path.as_deref().unwrap()).exists());
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn old_sessions_are_pruned() {
        let dir = scratch_dir("prune");
        fs::create_dir_all(&dir).unwrap();
        for i in 0..5 {
            fs::write(dir.join(format!("{i:03}-1.jsonl")), "").unwrap();
        }
        fs::write(dir.join("report-1.md"), "").unwrap();
        prune_old_sessions(&dir, 3);
        let mut left: Vec<String> = fs::read_dir(&dir)
            .unwrap()
            .map(|e| e.unwrap().file_name().into_string().unwrap())
            .collect();
        left.sort();
        assert_eq!(left, vec!["003-1.jsonl", "004-1.jsonl", "report-1.md"]);
        let _ = fs::remove_dir_all(dir);
    }
}
