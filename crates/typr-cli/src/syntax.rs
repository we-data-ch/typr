//! `typr syntax` — serializes the syntax manifest and renders the editor
//! grammars from it.
//!
//! The manifest (`typr_core::components::syntax`) is the single source of
//! truth for TypR's lexemes; the grammars under `editors/` are build
//! products. Nothing here is hand-maintained downstream: a generated file
//! carries a header saying so, and `--check` is the CI gate that fails when
//! someone edits one by hand or forgets to regenerate after touching the
//! parser.

use std::path::{Path, PathBuf};

use serde_json::{json, Map, Value};
use typr_core::components::syntax::{Rule, RuleBody, SyntaxManifest};

/// What `typr syntax` can render.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    /// The manifest itself, as JSON.
    Manifest,
    /// `editors/vscode/syntaxes/typr.tmLanguage.json` (VSCode, Positron,
    /// Shiki, Monaco, and — the day TypR clears Linguist's 2000-file bar —
    /// GitHub).
    TmLanguage,
    /// `editors/vim/syntax/typr.vim` (Vim and Neovim).
    Vim,
}

impl Target {
    pub fn parse(name: &str) -> Result<Self, String> {
        match name {
            "manifest" | "json" => Ok(Target::Manifest),
            "tmlanguage" | "textmate" => Ok(Target::TmLanguage),
            "vim" => Ok(Target::Vim),
            other => Err(format!(
                "unknown syntax target `{other}` (expected `manifest`, `tmlanguage` or `vim`)"
            )),
        }
    }

    /// Where `--write` puts this target, relative to the repository root.
    fn default_path(self) -> Option<&'static str> {
        match self {
            Target::Manifest => None,
            Target::TmLanguage => Some("editors/vscode/syntaxes/typr.tmLanguage.json"),
            Target::Vim => Some("editors/vim/syntax/typr.vim"),
        }
    }

    fn render(self, manifest: &SyntaxManifest) -> String {
        match self {
            Target::Manifest => format!("{}\n", manifest.to_json()),
            Target::TmLanguage => format!("{}\n", render_tmlanguage(manifest)),
            Target::Vim => render_vim(manifest),
        }
    }
}

// ── regex helpers ───────────────────────────────────────────────────────────

/// Escapes a literal lexeme for use inside a regex alternation.
fn escape(literal: &str) -> String {
    const SPECIAL: &[char] = &[
        '\\', '.', '+', '*', '?', '(', ')', '[', ']', '{', '}', '^', '$', '|', '/', '-',
    ];
    literal
        .chars()
        .flat_map(|c| if SPECIAL.contains(&c) { vec!['\\', c] } else { vec![c] })
        .collect()
}

/// Longest-first, so `<=` is tried before `<` and `...` before `..`. This
/// ordering is the whole reason the grammars are generated rather than
/// transcribed: it is invisible in a keyword list and easy to get wrong by
/// hand — `editors/vim/syntax/typr.vim` matched single-character operators
/// before the kind sigils and swallowed them.
fn longest_first(lexemes: &[String]) -> Vec<&str> {
    let mut sorted: Vec<&str> = lexemes.iter().map(|s| s.as_str()).collect();
    sorted.sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
    sorted
}

/// Word alternation with explicit identifier-boundary guards rather than
/// `\b`: `as!` ends in a non-word character, so a trailing `\b` would refuse
/// to match it.
fn word_pattern(lexemes: &[String], followed_by: Option<&str>) -> String {
    let alternation: Vec<String> = longest_first(lexemes).iter().map(|l| escape(l)).collect();
    let tail = match followed_by {
        Some(delimiter) => format!("(?={delimiter})"),
        None => "(?![A-Za-z0-9_])".to_string(),
    };
    format!("(?<![A-Za-z0-9_])({}){tail}", alternation.join("|"))
}

fn annotation_pattern(lexemes: &[String]) -> String {
    let alternation: Vec<String> = longest_first(lexemes).iter().map(|l| escape(l)).collect();
    format!("({})(?![A-Za-z0-9_])", alternation.join("|"))
}

fn symbol_pattern(lexemes: &[String]) -> String {
    let alternation: Vec<String> = longest_first(lexemes).iter().map(|l| escape(l)).collect();
    alternation.join("|")
}

/// A generic name is one uppercase letter or `Self`
/// (`parsing/types.rs::upper_case_generic`), so the sigil rule stops there
/// instead of running on into a PascalCase alias.
fn sigil_pattern(manifest: &SyntaxManifest) -> String {
    let live: Vec<String> = manifest
        .sigils
        .iter()
        .filter(|s| !s.reserved)
        .map(|s| escape(&s.sigil))
        .collect();
    format!("[{}](?:Self|[A-Z])(?![A-Za-z0-9_])", live.join(""))
}

// ── TextMate rendering ──────────────────────────────────────────────────────

fn tm_pattern(rule: &Rule, manifest: &SyntaxManifest) -> Value {
    let mut entry = Map::new();
    match &rule.body {
        RuleBody::Span { begin, end, escapes } => {
            entry.insert("name".into(), json!(rule.scope));
            entry.insert("begin".into(), json!(escape(begin)));
            entry.insert("end".into(), json!(escape(end)));
            if *escapes {
                entry.insert(
                    "patterns".into(),
                    json!([{ "name": "constant.character.escape.typr", "match": "\\\\." }]),
                );
            }
        }
        body => {
            let pattern = match body {
                RuleBody::Words { lexemes, followed_by } => word_pattern(lexemes, followed_by.as_deref()),
                RuleBody::Annotations { lexemes } => annotation_pattern(lexemes),
                RuleBody::Symbols { lexemes } => symbol_pattern(lexemes),
                RuleBody::Sigils => sigil_pattern(manifest),
                RuleBody::Match { pattern, .. } => pattern.clone(),
                RuleBody::Span { .. } => unreachable!("handled above"),
            };
            entry.insert("name".into(), json!(rule.scope));
            entry.insert("match".into(), json!(pattern));
        }
    }
    Value::Object(entry)
}

pub fn render_tmlanguage(manifest: &SyntaxManifest) -> String {
    let includes: Vec<Value> = manifest
        .rules
        .iter()
        .map(|r| json!({ "include": format!("#{}", r.name) }))
        .collect();

    let mut repository = Map::new();
    for rule in &manifest.rules {
        let mut bucket = Map::new();
        if let Some(note) = &rule.note {
            bucket.insert("comment".into(), json!(note));
        }
        bucket.insert("patterns".into(), json!([tm_pattern(rule, manifest)]));
        repository.insert(rule.name.clone(), Value::Object(bucket));
    }

    let grammar = json!({
        "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
        "information_for_contributors": [
            "GENERATED FILE — do not edit.",
            "Produced by `typr syntax --target tmlanguage` from the syntax manifest in",
            "crates/typr-core/src/components/syntax/mod.rs, the single source of truth for",
            "TypR's lexemes. Edit the manifest and re-run `typr syntax --write`; CI runs",
            "`typr syntax --check` and fails on a hand-edited grammar."
        ],
        "name": manifest.language.name,
        "scopeName": manifest.language.scope_name,
        "patterns": includes,
        "repository": Value::Object(repository),
    });

    serde_json::to_string_pretty(&grammar).expect("the rendered grammar is plain data")
}

// ── Vim rendering ───────────────────────────────────────────────────────────
//
// Vim's regex engine resolves ambiguity differently from TextMate/Oniguruma,
// which is why this is a second renderer rather than a text substitution over
// `render_tmlanguage`'s output:
//
// - TextMate tries the `patterns` array in order and uses the first pattern
//   that matches at all, regardless of match length — that is what the
//   manifest's rule order encodes.
// - Vim instead always prefers the *longest* match at a position; only an
//   exact-length tie falls back to definition order, and there the *last*
//   `:syn` command wins (`:help :syn-priority`). `:syn keyword` is its own
//   exception: an exact keyword always beats a `:syn match` at the same word,
//   independent of definition order.
//
// Given that, rules are emitted in *reverse* manifest order: it is the
// mechanical dual of "first defined wins" that reproduces the same effective
// priority under "last defined wins", for the one real tie in this grammar
// (`functions.call` / `variables.parameter` / `variables.other` can all match
// the same bare identifier). Every other ordering note in the manifest
// (`<-` before `<`, `==` before `=`, …) involves lexemes of different length,
// which Vim already resolves on its own regardless of definition order.
//
// All `:syn match` patterns use Vim's very-magic (`\v`) mode, whose escaping
// rules are the closest to the manifest's PCRE-ish dialect: every character
// except `0-9A-Za-z_` is a metacharacter unless backslash-escaped (the
// opposite of default "magic" mode), and lookaround is postfix
// (`atom@=`/`atom@!`/`atom@<=`/`atom@<!`) instead of prefixed groups.

/// Escapes a literal lexeme for use as a `\v`-mode atom (outside a `[...]`
/// class, where escaping rules differ and are handled separately).
fn vim_escape(literal: &str) -> String {
    literal
        .chars()
        .flat_map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                vec![c]
            } else {
                vec!['\\', c]
            }
        })
        .collect()
}

/// A lexeme `:syn keyword` can hold: Vim keyword matching is a hash lookup
/// keyed on `'iskeyword'`, so anything outside `[A-Za-z_][A-Za-z0-9_]*` (`as!`,
/// `data.frame`, `@export`) has to fall back to `:syn match` instead.
fn is_plain_identifier(lexeme: &str) -> bool {
    let mut chars = lexeme.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => chars.all(|c| c.is_ascii_alphanumeric() || c == '_'),
        _ => false,
    }
}

/// Word alternation, guarded the same way as `word_pattern`: a leading `<`
/// (word-start — `\v` mode drops the backslash `:help \v<`) stands in for the
/// PCRE negative-lookbehind guard, since every lexeme here starts with a
/// letter. `followed_by` is manifest PCRE text (`\s*\[`, …) that also happens
/// to be valid unchanged inside a `\v`-mode lookahead — plain character
/// classes and `\s` do not vary across the two dialects.
fn vim_word_pattern(lexemes: &[String], followed_by: Option<&str>) -> String {
    let alternation: Vec<String> = longest_first(lexemes).iter().map(|l| vim_escape(l)).collect();
    let tail = match followed_by {
        Some(delimiter) => format!("%({delimiter})@="),
        None => "[A-Za-z0-9_]@!".to_string(),
    };
    format!("<%({}){tail}", alternation.join("|"))
}

fn vim_annotation_pattern(lexemes: &[String]) -> String {
    let alternation: Vec<String> = longest_first(lexemes).iter().map(|l| vim_escape(l)).collect();
    format!("%({})[A-Za-z0-9_]@!", alternation.join("|"))
}

fn vim_symbol_pattern(lexemes: &[String]) -> String {
    let alternation: Vec<String> = longest_first(lexemes).iter().map(|l| vim_escape(l)).collect();
    alternation.join("|")
}

/// Sigil characters sit inside a `[...]` class, whose escaping rules differ
/// from a bare `\v` atom: only `] ^ - \` need care there, and none of the six
/// live sigils (`# % @ ^ ? $`) is one of them at this position, so they are
/// inserted literally rather than through `vim_escape`.
fn vim_sigil_pattern(manifest: &SyntaxManifest) -> String {
    let live: Vec<&str> = manifest
        .sigils
        .iter()
        .filter(|s| !s.reserved)
        .map(|s| s.sigil.as_str())
        .collect();
    format!("[{}]%(Self|[A-Z])[A-Za-z0-9_]@!", live.join(""))
}

/// Rule name (`types.sigil-generic`) → Vim group id (`typrTypesSigilGeneric`).
/// `rule_names_are_unique` (components/syntax/mod.rs) guarantees no collision.
fn vim_group_name(rule_name: &str) -> String {
    let mut out = String::from("typr");
    for part in rule_name.split(['.', '-']) {
        let mut chars = part.chars();
        if let Some(first) = chars.next() {
            out.push(first.to_ascii_uppercase());
            out.push_str(chars.as_str());
        }
    }
    out
}

/// Maps a rule's TextMate scope to a standard Vim highlight group, by prefix,
/// most specific first — the manifest doesn't carry a Vim-specific scope, and
/// the TextMate scope is already descriptive enough to derive one from.
fn vim_highlight_link(scope: &str) -> &'static str {
    const TABLE: &[(&str, &str)] = &[
        ("constant.numeric.float", "Float"),
        ("constant.numeric", "Number"),
        ("constant.language", "Constant"),
        ("comment.", "Comment"),
        ("string.", "String"),
        ("storage.modifier.annotation", "PreProc"),
        ("keyword.control", "Statement"),
        ("keyword.declaration", "Keyword"),
        ("keyword.operator.cast", "Keyword"),
        ("keyword.operator.word", "Keyword"),
        ("keyword.operator", "Operator"),
        ("keyword.other.block", "Keyword"),
        ("support.function", "Function"),
        ("support.type", "Type"),
        ("entity.name.type.variant", "Identifier"),
        ("entity.name.type.parameter", "Type"),
        ("entity.name.type", "Type"),
        ("entity.name.function", "Function"),
        ("variable.parameter", "Identifier"),
        ("variable.other", "Identifier"),
        ("punctuation.", "Delimiter"),
    ];
    TABLE
        .iter()
        .find(|(prefix, _)| scope.starts_with(prefix))
        .map(|(_, group)| *group)
        .unwrap_or_else(|| panic!("no Vim highlight mapping for scope `{scope}` — add one to vim_highlight_link"))
}

/// One `:syn` command for a rule, or `None` for `RuleBody::Sigils` bundled
/// elsewhere — never true here since every rule produces exactly one command.
fn vim_syn_command(rule: &Rule, manifest: &SyntaxManifest) -> String {
    let group = vim_group_name(&rule.name);
    match &rule.body {
        RuleBody::Span { begin, end, escapes } => {
            let contains = if *escapes { " contains=typrEscape" } else { "" };
            let skip = if *escapes { " skip=+\\\\.+" } else { "" };
            format!("syn region  {group} start=+{begin}+{skip} end=+{end}+{contains}")
        }
        RuleBody::Words { lexemes, followed_by }
            if followed_by.is_none() && lexemes.iter().all(|l| is_plain_identifier(l)) =>
        {
            format!("syn keyword {group} {}", lexemes.join(" "))
        }
        RuleBody::Words { lexemes, followed_by } => {
            format!(
                "syn match   {group} \"\\v{}\"",
                vim_word_pattern(lexemes, followed_by.as_deref())
            )
        }
        RuleBody::Annotations { lexemes } => {
            format!("syn match   {group} \"\\v{}\"", vim_annotation_pattern(lexemes))
        }
        RuleBody::Symbols { lexemes } => {
            format!("syn match   {group} \"\\v{}\"", vim_symbol_pattern(lexemes))
        }
        RuleBody::Sigils => {
            format!("syn match   {group} \"\\v{}\"", vim_sigil_pattern(manifest))
        }
        RuleBody::Match { vim, .. } => {
            format!("syn match   {group} \"\\v{vim}\"")
        }
    }
}

pub fn render_vim(manifest: &SyntaxManifest) -> String {
    let mut out = String::new();
    out.push_str("\" syntax/typr.vim — GENERATED FILE, do not edit.\n");
    out.push_str("\" Produced by `typr syntax --target vim` from the syntax manifest in\n");
    out.push_str("\" crates/typr-core/src/components/syntax/mod.rs, the single source of truth\n");
    out.push_str("\" for TypR's lexemes. Edit the manifest and re-run `typr syntax --write`;\n");
    out.push_str("\" CI runs `typr syntax --check` and fails on a hand-edited grammar.\n");
    out.push('\n');
    out.push_str("if exists(\"b:current_syntax\")\n  finish\nendif\n\n");
    out.push_str("let s:save_cpo = &cpo\nset cpo&vim\n\n");
    out.push_str("syntax case match\n");
    out.push_str("syn sync fromstart\n");
    out.push_str("syn iskeyword @,48-57,_\n\n");
    out.push_str("syn match   typrEscape \"\\\\.\" contained\n\n");

    for rule in manifest.rules.iter().rev() {
        out.push_str(&vim_syn_command(rule, manifest));
        out.push('\n');
    }

    out.push('\n');
    out.push_str("hi def link typrEscape SpecialChar\n");
    for rule in &manifest.rules {
        out.push_str(&format!(
            "hi def link {} {}\n",
            vim_group_name(&rule.name),
            vim_highlight_link(&rule.scope)
        ));
    }

    out.push_str("\nlet b:current_syntax = \"typr\"\n\n");
    out.push_str("let &cpo = s:save_cpo\nunlet s:save_cpo\n");
    out
}

// ── command ─────────────────────────────────────────────────────────────────

/// Walks up from the current directory looking for the workspace root, so
/// `--write`/`--check` work from anywhere inside the repository.
fn repo_root() -> PathBuf {
    let mut dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    loop {
        if dir.join("editors").is_dir() && dir.join("crates").is_dir() {
            return dir;
        }
        if !dir.pop() {
            return std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        }
    }
}

fn targets_for(target: Option<Target>) -> Vec<Target> {
    match target {
        Some(t) => vec![t],
        // `--write`/`--check` with no `--target` cover every generated
        // grammar: forgetting one is how the copies drifted apart before.
        None => vec![Target::TmLanguage, Target::Vim],
    }
}

/// `typr syntax`
pub fn run(target: Option<Target>, output: Option<PathBuf>, write: bool, check: bool) {
    let manifest = SyntaxManifest::new();

    if check {
        check_targets(&manifest, targets_for(target));
        return;
    }

    if write {
        write_targets(&manifest, targets_for(target));
        return;
    }

    let rendered = target.unwrap_or(Target::Manifest).render(&manifest);
    match output {
        Some(path) => match std::fs::write(&path, &rendered) {
            Ok(()) => println!("wrote {}", path.display()),
            Err(e) => {
                eprintln!("error: could not write {}: {e}", path.display());
                std::process::exit(1);
            }
        },
        None => print!("{rendered}"),
    }
}

fn generated_path(root: &Path, target: Target) -> PathBuf {
    root.join(
        target
            .default_path()
            .expect("only file-backed targets reach --write/--check"),
    )
}

fn write_targets(manifest: &SyntaxManifest, targets: Vec<Target>) {
    let root = repo_root();
    for target in targets {
        if target.default_path().is_none() {
            eprintln!("error: `--write` needs a file-backed target; the manifest goes to stdout or `--output`.");
            std::process::exit(1);
        }
        let path = generated_path(&root, target);
        let rendered = target.render(manifest);
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        match std::fs::write(&path, &rendered) {
            Ok(()) => println!("generated {}", path.display()),
            Err(e) => {
                eprintln!("error: could not write {}: {e}", path.display());
                std::process::exit(1);
            }
        }
    }
}

fn check_targets(manifest: &SyntaxManifest, targets: Vec<Target>) {
    let root = repo_root();
    let mut stale = Vec::new();
    for target in targets {
        if target.default_path().is_none() {
            continue;
        }
        let path = generated_path(&root, target);
        let expected = target.render(manifest);
        match std::fs::read_to_string(&path) {
            Ok(actual) if actual == expected => println!("up to date: {}", path.display()),
            Ok(_) => stale.push(format!("{} differs from the manifest", path.display())),
            Err(e) => stale.push(format!("{} is unreadable: {e}", path.display())),
        }
    }
    if !stale.is_empty() {
        eprintln!("error: generated grammars are out of date:");
        for line in &stale {
            eprintln!("  - {line}");
        }
        eprintln!("Run `typr syntax --write` and commit the result.");
        std::process::exit(1);
    }
}
