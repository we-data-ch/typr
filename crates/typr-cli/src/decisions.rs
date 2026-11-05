//! Architecture decision log (`typr decision …`).
//!
//! One Markdown file per decision under `decisions/` at the repository root:
//!
//! ```text
//! decisions/0007-delegate-packages-to-uvr.md
//!   +++
//!   title = "Delegate package management to uvr"
//!   status = "accepted"     # proposed | accepted | rejected | deprecated | superseded
//!   date = "2026-10-19"
//!   tags = ["toolchain"]
//!   docs = ["rfcs/0000-rig-r-provisioning.md", "../ai_context/uvr.md"]
//!   supersedes = ["0003"]   # optional
//!   superseded_by = "0009"  # optional, written by `typr decision supersede`
//!   +++
//!   ## Context / ## Decision / ## Alternatives considered / ## Consequences / ## Revisions
//! ```
//!
//! `docs` paths are relative to the repository root and are what ties a decision to its
//! technical documentation: `typr decision check` fails on a dead one, and
//! `typr decision list --doc <path>` answers "which decisions rest on this document?".
//!
//! Accepted RFCs are decisions too. `rfcs/` is indexed on the fly and never copied, so there is
//! no second list to keep in sync. RFCs are read-only here: they change through their own process.
//!
//! The directory is git-ignored, so it is resolved from the **main** working tree, which is what
//! makes one log visible from every `git worktree`.

use serde::Deserialize;
use serde::Serialize;
use std::io::IsTerminal;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

const DECISIONS_DIR: &str = "decisions";
const RFCS_DIR: &str = "rfcs";
const REVISIONS_HEADING: &str = "## Revisions";
const STATUSES: [&str; 5] = ["proposed", "accepted", "rejected", "deprecated", "superseded"];
const SLUG_MAX: usize = 60;

const BODY_TEMPLATE: &str = "
## Context

<!-- What forced a choice: the constraint, the bug, the deadline. Link evidence. -->

## Decision

<!-- What we do, in the active voice, stated precisely enough to be checked against the code. -->

## Alternatives considered

<!-- The neighbouring options and why each one lost. -->

## Consequences

<!-- What gets easier, what gets harder, and what would make us reopen this. -->
";

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Meta {
    title: String,
    status: String,
    date: String,
    #[serde(default)]
    tags: Vec<String>,
    #[serde(default)]
    docs: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    supersedes: Vec<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    superseded_by: Option<String>,
}

#[derive(Debug, Clone)]
struct Decision {
    id: String,
    path: PathBuf,
    meta: Meta,
    body: String,
}

/// What `list` and `show` print: a decision file or an indexed RFC, in one shape.
#[derive(Debug, Clone, Serialize)]
struct Entry {
    id: String,
    kind: &'static str,
    title: String,
    status: String,
    date: String,
    tags: Vec<String>,
    docs: Vec<String>,
    path: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    supersedes: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    superseded_by: Option<String>,
    #[serde(skip_serializing_if = "String::is_empty")]
    body: String,
}

#[derive(Debug, Default)]
pub struct ListFilter {
    pub status: Option<String>,
    pub tag: Option<String>,
    pub doc: Option<String>,
    pub grep: Option<String>,
    pub no_rfcs: bool,
}

// ── CLI entry points ───────────────────────────────────────────────────────────

pub fn list(filter: ListFilter, json: bool) {
    let root = repo_root();
    let (entries, problems) = entries(&root);
    report_unreadable(&problems);
    let mut matching: Vec<Entry> = entries.into_iter().filter(|e| matches_filter(e, &filter)).collect();

    if json {
        matching.iter_mut().for_each(|e| e.body.clear());
        print_json(&matching);
        return;
    }
    if matching.is_empty() {
        println!("No matching decision. Record one with `typr decision add \"<title>\"`.");
        return;
    }
    print_table(&matching);
}

pub fn show(id: &str, json: bool) {
    let root = repo_root();
    let entry = or_die(find_entry(&root, id));
    if json {
        print_json(&entry);
        return;
    }
    println!("{}", paint(BOLD, &entry.title));
    println!(
        "{}  {}  {}",
        entry.id,
        paint(status_color(&entry.status), &entry.status),
        entry.date
    );
    if !entry.tags.is_empty() {
        println!("tags: {}", entry.tags.join(", "));
    }
    for doc in &entry.docs {
        let dead = if doc_exists(&root, doc) { "" } else { "  (missing)" };
        println!("doc:  {doc}{dead}");
    }
    if !entry.supersedes.is_empty() {
        println!("supersedes: {}", entry.supersedes.join(", "));
    }
    if let Some(next) = &entry.superseded_by {
        println!(
            "{}",
            paint(YELLOW, &format!("superseded by {next}: read that one instead"))
        );
    }
    println!("file: {}\n{}", entry.path, entry.body.trim_end());
}

pub fn add(title: &str, tags: Vec<String>, docs: Vec<String>, status: &str) {
    let root = repo_root();
    let created = or_die(create(&root, title, tags, docs, status, None));
    print_created(&created);
}

pub fn revise(id: &str, note: &str, docs: Vec<String>) {
    let root = repo_root();
    let decision = or_die(add_revision(&root, id, note, docs));
    println!("Revised {} ({})", decision.id, decision.path.display());
}

pub fn set_status(id: &str, status: &str, note: Option<&str>) {
    let root = repo_root();
    let decision = or_die(change_status(&root, id, status, note));
    println!("{} is now {}", decision.id, decision.meta.status);
}

pub fn supersede(id: &str, title: &str, tags: Vec<String>, docs: Vec<String>) {
    let root = repo_root();
    let created = or_die(replace(&root, id, title, tags, docs));
    print_created(&created);
}

pub fn check() {
    let root = repo_root();
    let errors = check_errors(&root);
    if errors.is_empty() {
        let (decisions, _) = load_decisions(&root);
        println!("{} decisions OK", decisions.len());
        return;
    }
    for error in &errors {
        eprintln!("{} {error}", paint(RED, "error:"));
    }
    eprintln!("{} problem(s) in {DECISIONS_DIR}/", errors.len());
    std::process::exit(1);
}

// ── Mutations ──────────────────────────────────────────────────────────────────

fn create(
    root: &Path,
    title: &str,
    tags: Vec<String>,
    docs: Vec<String>,
    status: &str,
    replaces: Option<&Decision>,
) -> Result<Decision, String> {
    if !STATUSES.contains(&status) || status == "superseded" {
        return Err(format!(
            "cannot create a decision with status `{status}` (use proposed, accepted, rejected or deprecated)"
        ));
    }
    let slug = slugify(title);
    if slug.is_empty() {
        return Err(format!("title `{title}` has nothing to build a file name from"));
    }
    require_docs(root, &docs)?;

    let dir = root.join(DECISIONS_DIR);
    std::fs::create_dir_all(&dir).map_err(|e| format!("cannot create {}: {e}", dir.display()))?;
    let (existing, _) = load_decisions(root);
    let next = existing
        .iter()
        .filter_map(|d| d.id.parse::<u32>().ok())
        .max()
        .unwrap_or(0)
        + 1;
    let id = format!("{next:04}");

    let body = match replaces {
        Some(old) => BODY_TEMPLATE.replacen(
            "## Context\n\n",
            &format!(
                "## Context\n\nThis replaces decision {}: {}.\n\n",
                old.id, old.meta.title
            ),
            1,
        ),
        None => BODY_TEMPLATE.to_string(),
    };
    let decision = Decision {
        path: dir.join(format!("{id}-{slug}.md")),
        id,
        meta: Meta {
            title: title.to_string(),
            status: status.to_string(),
            date: crate::cases::current_date(),
            tags,
            docs,
            supersedes: replaces.map(|old| vec![old.id.clone()]).unwrap_or_default(),
            superseded_by: None,
        },
        body,
    };
    save(&decision)?;
    Ok(decision)
}

fn add_revision(root: &Path, id: &str, note: &str, docs: Vec<String>) -> Result<Decision, String> {
    if note.trim().is_empty() {
        return Err("a revision needs a note saying what changed and why".to_string());
    }
    require_docs(root, &docs)?;
    let mut decision = find_decision(root, id)?;
    for doc in docs {
        if !decision.meta.docs.contains(&doc) {
            decision.meta.docs.push(doc);
        }
    }
    decision.body = append_revision(&decision.body, &crate::cases::current_date(), note);
    save(&decision)?;
    Ok(decision)
}

fn change_status(root: &Path, id: &str, status: &str, note: Option<&str>) -> Result<Decision, String> {
    if !STATUSES.contains(&status) {
        return Err(format!(
            "unknown status `{status}` (expected one of: {})",
            STATUSES.join(", ")
        ));
    }
    if status == "superseded" {
        return Err("use `typr decision supersede <id> \"<title>\"`, which links both decisions".to_string());
    }
    let mut decision = find_decision(root, id)?;
    if decision.meta.status == "superseded" {
        return Err(format!(
            "{} is superseded by {}: revise or replace that one instead",
            decision.id,
            decision.meta.superseded_by.as_deref().unwrap_or("?")
        ));
    }
    if decision.meta.status == status {
        return Err(format!("{} is already {status}", decision.id));
    }
    let entry = match note {
        Some(note) => format!("Status {} -> {status}: {note}", decision.meta.status),
        None => format!("Status {} -> {status}.", decision.meta.status),
    };
    decision.meta.status = status.to_string();
    decision.body = append_revision(&decision.body, &crate::cases::current_date(), &entry);
    save(&decision)?;
    Ok(decision)
}

/// Writes the replacement first: a failure in between leaves an extra decision, never a
/// superseded one pointing at nothing.
fn replace(root: &Path, id: &str, title: &str, tags: Vec<String>, docs: Vec<String>) -> Result<Decision, String> {
    let mut old = find_decision(root, id)?;
    if old.meta.status == "superseded" || old.meta.status == "rejected" {
        return Err(format!(
            "{} is {}: there is nothing in force to replace",
            old.id, old.meta.status
        ));
    }
    let tags = merged(&old.meta.tags, tags);
    let docs = merged(&old.meta.docs, docs);
    let created = create(root, title, tags, docs, "accepted", Some(&old))?;

    let entry = format!("Superseded by {}: {}", created.id, created.meta.title);
    old.meta.status = "superseded".to_string();
    old.meta.superseded_by = Some(created.id.clone());
    old.body = append_revision(&old.body, &crate::cases::current_date(), &entry);
    save(&old)?;
    Ok(created)
}

fn merged(inherited: &[String], added: Vec<String>) -> Vec<String> {
    let mut all = inherited.to_vec();
    for item in added {
        if !all.contains(&item) {
            all.push(item);
        }
    }
    all
}

/// Adds a dated line to the `## Revisions` section, wherever it is, creating it at the end if
/// needed. The decision text above it is left alone.
fn append_revision(body: &str, date: &str, note: &str) -> String {
    let flat_note = note.split_whitespace().collect::<Vec<_>>().join(" ");
    let line = format!("- {date}: {flat_note}");
    let lines: Vec<&str> = body.lines().collect();
    let Some(start) = lines.iter().position(|l| l.trim() == REVISIONS_HEADING) else {
        return format!("{}\n\n{REVISIONS_HEADING}\n\n{line}\n", body.trim_end());
    };
    let section_end = lines[start + 1..]
        .iter()
        .position(|l| l.starts_with("## "))
        .map(|offset| start + 1 + offset)
        .unwrap_or(lines.len());
    let last_filled = (start..section_end)
        .rev()
        .find(|&i| !lines[i].trim().is_empty())
        .unwrap_or(start);

    let mut out: Vec<&str> = lines[..=last_filled].to_vec();
    if last_filled == start {
        out.push("");
    }
    out.push(&line);
    out.extend_from_slice(&lines[last_filled + 1..]);
    format!("{}\n", out.join("\n").trim_end())
}

// ── Checking ───────────────────────────────────────────────────────────────────

fn check_errors(root: &Path) -> Vec<String> {
    let (decisions, unreadable) = load_decisions(root);
    let mut errors = unreadable;
    let date_shape = |d: &str| {
        let b = d.as_bytes();
        b.len() == 10
            && b.iter().enumerate().all(|(i, c)| {
                if i == 4 || i == 7 {
                    *c == b'-'
                } else {
                    c.is_ascii_digit()
                }
            })
    };

    for (i, d) in decisions.iter().enumerate() {
        let id = &d.id;
        if decisions[..i].iter().any(|other| other.id == *id) {
            errors.push(format!("{id}: number used by more than one file"));
        }
        if !STATUSES.contains(&d.meta.status.as_str()) {
            errors.push(format!("{id}: unknown status `{}`", d.meta.status));
        }
        if !date_shape(&d.meta.date) {
            errors.push(format!("{id}: date `{}` is not YYYY-MM-DD", d.meta.date));
        }
        for doc in &d.meta.docs {
            if !doc_exists(root, doc) {
                errors.push(format!("{id}: linked document `{doc}` does not exist"));
            }
        }
        if d.meta.status == "accepted" && section_text(&d.body, "## Decision").is_empty() {
            errors.push(format!("{id}: accepted, but its `## Decision` section is empty"));
        }
        for target in &d.meta.supersedes {
            match decisions.iter().find(|other| other.id == *target) {
                None => errors.push(format!("{id}: supersedes unknown decision {target}")),
                Some(old) if old.meta.superseded_by.as_deref() != Some(id) => errors.push(format!(
                    "{id}: supersedes {target}, but {target} does not name it in superseded_by"
                )),
                Some(_) => {}
            }
        }
        match (&d.meta.superseded_by, d.meta.status == "superseded") {
            (None, true) => errors.push(format!("{id}: superseded, but superseded_by is empty")),
            (Some(_), false) => errors.push(format!(
                "{id}: has superseded_by, but its status is `{}`",
                d.meta.status
            )),
            (Some(next), true) => match decisions.iter().find(|other| other.id == *next) {
                None => errors.push(format!("{id}: superseded by unknown decision {next}")),
                Some(new) if !new.meta.supersedes.contains(id) => errors.push(format!(
                    "{id}: superseded by {next}, but {next} does not list it in supersedes"
                )),
                Some(_) => {}
            },
            (None, false) => {}
        }
    }
    errors
}

/// A section's text with HTML comments removed, so an untouched template counts as empty.
fn section_text(body: &str, heading: &str) -> String {
    let mut text = String::new();
    let mut inside = false;
    for line in body.lines() {
        if line.starts_with("## ") {
            inside = line.trim() == heading;
        } else if inside {
            text.push_str(line);
            text.push('\n');
        }
    }
    while let (Some(open), Some(close)) = (text.find("<!--"), text.find("-->")) {
        if close < open {
            break;
        }
        text.replace_range(open..close + 3, "");
    }
    text.trim().to_string()
}

fn doc_exists(root: &Path, doc: &str) -> bool {
    if doc.starts_with("http://") || doc.starts_with("https://") {
        return true;
    }
    let path = doc.split('#').next().unwrap_or(doc);
    root.join(path).exists()
}

fn require_docs(root: &Path, docs: &[String]) -> Result<(), String> {
    match docs.iter().find(|doc| !doc_exists(root, doc)) {
        Some(doc) => Err(format!(
            "linked document `{doc}` does not exist (paths are relative to {})",
            root.display()
        )),
        None => Ok(()),
    }
}

// ── Loading and saving ─────────────────────────────────────────────────────────

/// The main working tree, even from inside a `git worktree`: `decisions/` is git-ignored, so a
/// worktree has no copy of its own. Outside a repository, the current directory.
fn repo_root() -> PathBuf {
    let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let output = Command::new("git")
        .args(["rev-parse", "--path-format=absolute", "--git-common-dir"])
        .output();
    let Ok(output) = output else { return cwd };
    if !output.status.success() {
        return cwd;
    }
    let common_dir = PathBuf::from(String::from_utf8_lossy(&output.stdout).trim());
    common_dir.parent().map(Path::to_path_buf).unwrap_or(cwd)
}

/// Readable decisions sorted by id, plus one message per file that could not be parsed.
fn load_decisions(root: &Path) -> (Vec<Decision>, Vec<String>) {
    let mut decisions = vec![];
    let mut unreadable = vec![];
    let Ok(dir) = std::fs::read_dir(root.join(DECISIONS_DIR)) else {
        return (decisions, unreadable);
    };
    for path in dir.filter_map(|e| e.ok()).map(|e| e.path()) {
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        if !is_decision_file(&name) {
            continue;
        }
        let parsed = std::fs::read_to_string(&path)
            .map_err(|e| e.to_string())
            .and_then(|text| parse_decision(&path, &text));
        match parsed {
            Ok(decision) => decisions.push(decision),
            Err(e) => unreadable.push(format!("{name}: {e}")),
        }
    }
    decisions.sort_by(|a, b| a.id.cmp(&b.id));
    unreadable.sort();
    (decisions, unreadable)
}

fn is_decision_file(name: &str) -> bool {
    let bytes = name.as_bytes();
    name.ends_with(".md") && bytes.len() > 8 && bytes[..4].iter().all(u8::is_ascii_digit) && bytes[4] == b'-'
}

fn parse_decision(path: &Path, text: &str) -> Result<Decision, String> {
    let mut lines = text.split_inclusive('\n');
    if lines.next().map(str::trim) != Some("+++") {
        return Err("does not start with a `+++` front matter".to_string());
    }
    let mut front = String::new();
    let mut closed = false;
    let mut consumed = text.split_inclusive('\n').next().map(str::len).unwrap_or(0);
    for line in lines {
        consumed += line.len();
        if line.trim() == "+++" {
            closed = true;
            break;
        }
        front.push_str(line);
    }
    if !closed {
        return Err("front matter is never closed by `+++`".to_string());
    }
    let meta: Meta = toml::from_str(&front).map_err(|e| format!("invalid front matter: {e}"))?;
    let name = path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    Ok(Decision {
        id: name.chars().take(4).collect(),
        path: path.to_path_buf(),
        meta,
        body: text[consumed..].to_string(),
    })
}

/// Atomic, because this directory has no git history to recover a half-written file from.
fn save(decision: &Decision) -> Result<(), String> {
    let front = toml::to_string(&decision.meta).map_err(|e| format!("cannot serialize {}: {e}", decision.id))?;
    let text = format!("+++\n{front}+++\n{}", decision.body);
    let tmp = decision.path.with_extension("md.tmp");
    std::fs::write(&tmp, text)
        .and_then(|_| std::fs::rename(&tmp, &decision.path))
        .map_err(|e| format!("cannot write {}: {e}", decision.path.display()))
}

fn find_decision(root: &Path, id: &str) -> Result<Decision, String> {
    if is_rfc_id(id) {
        return Err(format!(
            "{id} is an RFC: it changes through its own process, see rfcs/README.md"
        ));
    }
    let number = id.split('-').next().unwrap_or(id);
    if number.is_empty() || !number.chars().all(|c| c.is_ascii_digit()) {
        return Err(format!("`{id}` is not a decision number"));
    }
    let wanted = format!("{number:0>4}");
    let (decisions, unreadable) = load_decisions(root);
    if let Some(problem) = unreadable.iter().find(|p| p.starts_with(&wanted)) {
        return Err(problem.clone());
    }
    decisions
        .into_iter()
        .find(|d| d.id == wanted)
        .ok_or_else(|| format!("no decision {wanted} (see `typr decision list`)"))
}

fn find_entry(root: &Path, id: &str) -> Result<Entry, String> {
    if !is_rfc_id(id) {
        return find_decision(root, id).map(|d| decision_entry(root, d));
    }
    let needle = id[4..].to_lowercase();
    let mut candidates: Vec<Entry> = load_rfcs(root)
        .into_iter()
        .filter(|e| e.id[4..].to_lowercase().contains(&needle))
        .collect();
    match candidates.len() {
        0 => Err(format!("no RFC matches `{id}`")),
        1 => Ok(candidates.remove(0)),
        _ => Err(format!(
            "`{id}` matches several RFCs: {}",
            candidates.iter().map(|e| e.id.as_str()).collect::<Vec<_>>().join(", ")
        )),
    }
}

fn is_rfc_id(id: &str) -> bool {
    id.get(..4).is_some_and(|prefix| prefix.eq_ignore_ascii_case("RFC-"))
}

fn decision_entry(root: &Path, decision: Decision) -> Entry {
    let path = decision
        .path
        .strip_prefix(root)
        .unwrap_or(&decision.path)
        .display()
        .to_string();
    Entry {
        id: decision.id,
        kind: "decision",
        title: decision.meta.title,
        status: decision.meta.status,
        date: decision.meta.date,
        tags: decision.meta.tags,
        docs: decision.meta.docs,
        path,
        supersedes: decision.meta.supersedes,
        superseded_by: decision.meta.superseded_by,
        body: decision.body,
    }
}

fn load_rfcs(root: &Path) -> Vec<Entry> {
    let Ok(dir) = std::fs::read_dir(root.join(RFCS_DIR)) else {
        return vec![];
    };
    let mut rfcs: Vec<Entry> = dir
        .filter_map(|e| e.ok())
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            let is_rfc = is_decision_file(&name) && name != "0000-template.md";
            let text = if is_rfc {
                std::fs::read_to_string(e.path()).ok()
            } else {
                None
            };
            text.map(|text| parse_rfc(&name, text))
        })
        .collect();
    rfcs.sort_by(|a, b| a.id.cmp(&b.id));
    rfcs
}

fn parse_rfc(file_name: &str, text: String) -> Entry {
    let stem = file_name.trim_end_matches(".md");
    let path = format!("{RFCS_DIR}/{file_name}");
    Entry {
        id: format!("RFC-{stem}"),
        kind: "rfc",
        title: text
            .lines()
            .find_map(|l| l.strip_prefix("# "))
            .unwrap_or(stem)
            .trim()
            .to_string(),
        // `Status:` may carry a remark ("implemented (branch develop, ...)"): the first word is
        // the status, and `show` displays the whole file for the remark.
        status: rfc_header(&text, "Status")
            .and_then(|s| s.split_whitespace().next().map(str::to_lowercase))
            .unwrap_or_else(|| "unknown".to_string()),
        date: rfc_header(&text, "Start date").unwrap_or_default(),
        tags: vec!["rfc".to_string()],
        docs: vec![path.clone()],
        path,
        supersedes: vec![],
        superseded_by: None,
        body: text,
    }
}

fn rfc_header(text: &str, field: &str) -> Option<String> {
    let prefix = format!("- **{field}:**");
    text.lines()
        .find_map(|l| l.strip_prefix(&prefix))
        .map(|value| value.trim().to_string())
}

fn entries(root: &Path) -> (Vec<Entry>, Vec<String>) {
    let (decisions, unreadable) = load_decisions(root);
    let mut all: Vec<Entry> = decisions.into_iter().map(|d| decision_entry(root, d)).collect();
    all.extend(load_rfcs(root));
    (all, unreadable)
}

fn matches_filter(entry: &Entry, filter: &ListFilter) -> bool {
    let contains = |haystack: &str, needle: &str| haystack.to_lowercase().contains(&needle.to_lowercase());
    !(filter.no_rfcs && entry.kind == "rfc")
        && filter
            .status
            .as_ref()
            .is_none_or(|s| entry.status.eq_ignore_ascii_case(s))
        && filter
            .tag
            .as_ref()
            .is_none_or(|t| entry.tags.iter().any(|tag| tag.eq_ignore_ascii_case(t)))
        && filter
            .doc
            .as_ref()
            .is_none_or(|d| entry.docs.iter().any(|doc| contains(doc, d)))
        && filter
            .grep
            .as_ref()
            .is_none_or(|g| contains(&entry.title, g) || contains(&entry.body, g))
}

// ── Output ─────────────────────────────────────────────────────────────────────

const GREEN: &str = "\x1b[32m";
const RED: &str = "\x1b[31m";
const YELLOW: &str = "\x1b[33m";
const CYAN: &str = "\x1b[36m";
const GREY: &str = "\x1b[90m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

/// Colour only on a terminal: agents and pipes read this output too.
fn paint(color: &str, text: &str) -> String {
    if std::io::stdout().is_terminal() {
        format!("{color}{text}{RESET}")
    } else {
        text.to_string()
    }
}

fn status_color(status: &str) -> &'static str {
    match status {
        "accepted" | "implemented" => GREEN,
        "proposed" | "draft" => CYAN,
        "rejected" => RED,
        "deprecated" | "superseded" => GREY,
        _ => YELLOW,
    }
}

fn print_table(entries: &[Entry]) {
    let id_width = entries.iter().map(|e| e.id.chars().count()).max().unwrap_or(4);
    for entry in entries {
        let status = format!("{:<11}", entry.status);
        let tags = if entry.kind == "decision" && !entry.tags.is_empty() {
            format!("  [{}]", entry.tags.join(", "))
        } else {
            String::new()
        };
        let next = entry
            .superseded_by
            .as_ref()
            .map(|n| format!("  -> {n}"))
            .unwrap_or_default();
        println!(
            "{:<id_width$}  {}  {:<10}  {}{}{}",
            entry.id,
            paint(status_color(&entry.status), &status),
            entry.date,
            entry.title,
            paint(GREY, &tags),
            next,
        );
    }
}

fn print_created(decision: &Decision) {
    println!("Created {} ({})", decision.id, decision.meta.status);
    println!("{}", decision.path.display());
    println!("Fill in its sections, then run `typr decision check`.");
}

fn print_json<T: Serialize>(value: &T) {
    match serde_json::to_string_pretty(value) {
        Ok(json) => println!("{json}"),
        Err(e) => or_die(Err(format!("cannot serialize JSON: {e}"))),
    }
}

fn report_unreadable(problems: &[String]) {
    for problem in problems {
        eprintln!("{} skipped {problem}", paint(YELLOW, "warning:"));
    }
}

fn or_die<T>(result: Result<T, String>) -> T {
    match result {
        Ok(value) => value,
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    }
}

fn slugify(title: &str) -> String {
    let full = crate::cases::slugify(title);
    full.chars()
        .take(SLUG_MAX)
        .collect::<String>()
        .trim_matches('-')
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TempRoot(PathBuf);

    impl TempRoot {
        fn new() -> Self {
            let path = crate::cases::unique_tmp();
            std::fs::create_dir_all(&path).unwrap();
            TempRoot(path)
        }
    }

    impl Drop for TempRoot {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    fn new_decision(root: &Path, title: &str) -> Decision {
        create(root, title, vec![], vec![], "proposed", None).unwrap()
    }

    fn fill_decision_section(root: &Path, id: &str) {
        let mut decision = find_decision(root, id).unwrap();
        decision.body = decision
            .body
            .replacen("## Decision\n\n", "## Decision\n\nWe do this.\n\n", 1);
        save(&decision).unwrap();
    }

    #[test]
    fn create_numbers_sequentially_and_round_trips() {
        let root = TempRoot::new();
        let first = new_decision(&root.0, "Use uvr for packages!");
        let second = new_decision(&root.0, "Second");
        assert_eq!(first.id, "0001");
        assert_eq!(second.id, "0002");
        assert!(first.path.ends_with("0001-use-uvr-for-packages.md"));

        let reloaded = find_decision(&root.0, "1").unwrap();
        assert_eq!(reloaded.meta, first.meta);
        assert_eq!(reloaded.body, first.body);
    }

    #[test]
    fn ids_resolve_in_every_short_form() {
        let root = TempRoot::new();
        new_decision(&root.0, "Thing");
        for id in ["1", "0001", "0001-thing"] {
            assert_eq!(find_decision(&root.0, id).unwrap().id, "0001", "{id}");
        }
        assert!(find_decision(&root.0, "thing").is_err());
        assert!(find_decision(&root.0, "2").is_err());
    }

    #[test]
    fn create_rejects_dead_docs_and_empty_slugs() {
        let root = TempRoot::new();
        let dead = create(&root.0, "Title", vec![], vec!["nope.md".into()], "proposed", None);
        assert!(dead.unwrap_err().contains("nope.md"));
        assert!(create(&root.0, "!!!", vec![], vec![], "proposed", None).is_err());
    }

    #[test]
    fn revisions_keep_the_decision_text_and_stay_in_their_section() {
        let once = append_revision("\n## Decision\n\nDo X.\n", "2026-01-01", "First\nnote");
        assert_eq!(
            once,
            "\n## Decision\n\nDo X.\n\n## Revisions\n\n- 2026-01-01: First note\n"
        );
        let twice = append_revision(&once, "2026-01-02", "Second");
        assert!(twice.ends_with("- 2026-01-01: First note\n- 2026-01-02: Second\n"));

        let followed = append_revision("## Revisions\n\n- old\n\n## Notes\n\nkept\n", "2026-01-03", "new");
        assert_eq!(
            followed,
            "## Revisions\n\n- old\n- 2026-01-03: new\n\n## Notes\n\nkept\n"
        );
    }

    #[test]
    fn superseding_links_both_ways_and_inherits_links() {
        let root = TempRoot::new();
        std::fs::write(root.0.join("spec.md"), "x").unwrap();
        create(
            &root.0,
            "Old",
            vec!["infra".into()],
            vec!["spec.md".into()],
            "accepted",
            None,
        )
        .unwrap();
        fill_decision_section(&root.0, "1");

        let new = replace(&root.0, "1", "New", vec![], vec![]).unwrap();
        fill_decision_section(&root.0, &new.id);
        let old = find_decision(&root.0, "1").unwrap();

        assert_eq!(old.meta.status, "superseded");
        assert_eq!(old.meta.superseded_by.as_deref(), Some("0002"));
        assert!(old.body.contains("Superseded by 0002: New"));
        assert_eq!(new.meta.supersedes, vec!["0001"]);
        assert_eq!(new.meta.tags, vec!["infra"]);
        assert_eq!(new.meta.docs, vec!["spec.md"]);
        assert_eq!(check_errors(&root.0), Vec::<String>::new());
        assert!(
            replace(&root.0, "1", "Again", vec![], vec![]).is_err(),
            "already superseded"
        );
    }

    #[test]
    fn status_changes_are_logged_and_guarded() {
        let root = TempRoot::new();
        new_decision(&root.0, "Thing");
        assert!(change_status(&root.0, "1", "bogus", None).is_err());
        assert!(change_status(&root.0, "1", "proposed", None).is_err());
        assert!(change_status(&root.0, "1", "superseded", None).is_err());

        let accepted = change_status(&root.0, "1", "accepted", Some("reviewed")).unwrap();
        assert_eq!(accepted.meta.status, "accepted");
        assert!(accepted.body.contains("Status proposed -> accepted: reviewed"));

        replace(&root.0, "1", "New", vec![], vec![]).unwrap();
        assert!(change_status(&root.0, "1", "accepted", None).is_err());
    }

    #[test]
    fn check_reports_each_kind_of_inconsistency() {
        let root = TempRoot::new();
        let mut decision = new_decision(&root.0, "Thing");
        assert_eq!(check_errors(&root.0), Vec::<String>::new());

        decision.meta.status = "accepted".into();
        decision.meta.date = "soon".into();
        decision.meta.docs = vec!["gone.md".into(), "https://example.org/spec".into()];
        decision.meta.supersedes = vec!["0009".into()];
        decision.meta.superseded_by = Some("0008".into());
        save(&decision).unwrap();
        std::fs::write(root.0.join(DECISIONS_DIR).join("0002-broken.md"), "no front matter").unwrap();

        let errors = check_errors(&root.0).join("\n");
        for expected in [
            "0002-broken.md",
            "date `soon`",
            "`gone.md` does not exist",
            "`## Decision` section is empty",
            "supersedes unknown decision 0009",
            "has superseded_by, but its status",
        ] {
            assert!(errors.contains(expected), "missing `{expected}` in:\n{errors}");
        }
        assert!(!errors.contains("example.org"), "URLs are not checked");
    }

    #[test]
    fn unknown_front_matter_field_is_an_error_not_silently_lost() {
        let parsed = parse_decision(
            Path::new("0001-x.md"),
            "+++\ntitle = \"T\"\nstatus = \"accepted\"\ndate = \"2026-01-01\"\nsuperseeds = []\n+++\nbody\n",
        );
        assert!(parsed.unwrap_err().contains("superseeds"));
    }

    #[test]
    fn rfcs_are_indexed_and_read_only() {
        let root = TempRoot::new();
        let rfcs = root.0.join(RFCS_DIR);
        std::fs::create_dir_all(&rfcs).unwrap();
        let header = "- **Status:** implemented (branch `develop`)\n- **Start date:** 2026-09-30\n\n# Some RFC\n";
        std::fs::write(rfcs.join("0028-some-rfc.md"), header).unwrap();
        std::fs::write(rfcs.join("0000-template.md"), header).unwrap();
        std::fs::write(rfcs.join("README.md"), header).unwrap();

        let (all, _) = entries(&root.0);
        assert_eq!(all.len(), 1);
        assert_eq!(all[0].id, "RFC-0028-some-rfc");
        assert_eq!(all[0].status, "implemented");
        assert_eq!(all[0].date, "2026-09-30");
        assert_eq!(all[0].title, "Some RFC");

        assert_eq!(find_entry(&root.0, "rfc-0028").unwrap().id, "RFC-0028-some-rfc");
        assert!(find_decision(&root.0, "RFC-0028").unwrap_err().contains("own process"));
    }

    #[test]
    fn filters_combine_on_status_tag_doc_and_text() {
        let root = TempRoot::new();
        std::fs::write(root.0.join("uvr.md"), "x").unwrap();
        create(
            &root.0,
            "Packages",
            vec!["toolchain".into()],
            vec!["uvr.md".into()],
            "proposed",
            None,
        )
        .unwrap();
        new_decision(&root.0, "Other");
        let (all, _) = entries(&root.0);
        let titles = |filter: ListFilter| {
            all.iter()
                .filter(|e| matches_filter(e, &filter))
                .map(|e| e.title.as_str())
                .collect::<Vec<_>>()
        };

        assert_eq!(
            titles(ListFilter {
                tag: Some("TOOLCHAIN".into()),
                ..Default::default()
            }),
            ["Packages"]
        );
        assert_eq!(
            titles(ListFilter {
                doc: Some("uvr".into()),
                ..Default::default()
            }),
            ["Packages"]
        );
        assert_eq!(
            titles(ListFilter {
                grep: Some("other".into()),
                ..Default::default()
            }),
            ["Other"]
        );
        assert!(titles(ListFilter {
            status: Some("accepted".into()),
            ..Default::default()
        })
        .is_empty());
    }
}
