//! The generated editor grammars must match what the manifest produces.
//!
//! `typr syntax --check` is the same gate for CI; this test makes a plain
//! `cargo test` catch the two ways the files go stale — someone edits
//! `editors/vscode/syntaxes/typr.tmLanguage.json` by hand, or someone adds a
//! lexeme to the manifest and forgets `typr syntax --write`.

use std::path::PathBuf;

use typr_cli::syntax::{render_tmlanguage, render_vim};
use typr_core::components::syntax::SyntaxManifest;

fn repo_root() -> PathBuf {
    // CARGO_MANIFEST_DIR is <root>/crates/typr-cli.
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("typr-cli lives two levels under the workspace root")
        .to_path_buf()
}

#[test]
fn vscode_tmlanguage_is_up_to_date() {
    let path = repo_root().join("editors/vscode/syntaxes/typr.tmLanguage.json");
    let committed = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let expected = format!("{}\n", render_tmlanguage(&SyntaxManifest::new()));

    assert_eq!(
        committed,
        expected,
        "\n{} is out of date with the syntax manifest.\nRun `typr syntax --write` and commit the result.\n",
        path.display()
    );
}

#[test]
fn vim_syntax_is_up_to_date() {
    let path = repo_root().join("editors/vim/syntax/typr.vim");
    let committed = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let expected = render_vim(&SyntaxManifest::new());

    assert_eq!(
        committed,
        expected,
        "\n{} is out of date with the syntax manifest.\nRun `typr syntax --write` and commit the result.\n",
        path.display()
    );
}

/// The generated file has to say so, or the next person edits it by hand —
/// which is how `editors/vim/syntax/typr.vim` inherited the tmLanguage's
/// ghost keywords the day it was written.
#[test]
fn generated_grammar_announces_itself() {
    for rendered in [
        render_tmlanguage(&SyntaxManifest::new()),
        render_vim(&SyntaxManifest::new()),
    ] {
        assert!(
            rendered.contains("GENERATED FILE"),
            "the rendered grammar must carry a do-not-edit header"
        );
        assert!(
            rendered.contains("typr syntax"),
            "the header must name the command that regenerates it"
        );
    }
}

/// Ordering is the property a hand-written grammar loses first: a
/// single-character operator placed ahead of a longer one silently eats its
/// prefix. Pin the pairs that actually broke in the hand-written copies.
#[test]
fn longer_lexemes_are_matched_before_their_prefixes() {
    let grammar: serde_json::Value =
        serde_json::from_str(&render_tmlanguage(&SyntaxManifest::new())).expect("rendered grammar is JSON");
    let order: Vec<String> = grammar["patterns"]
        .as_array()
        .expect("top-level patterns")
        .iter()
        .map(|p| {
            p["include"]
                .as_str()
                .expect("include")
                .trim_start_matches('#')
                .to_string()
        })
        .collect();
    let position = |name: &str| {
        order
            .iter()
            .position(|n| n == name)
            .unwrap_or_else(|| panic!("no rule named `{name}` in the generated grammar"))
    };

    // `<-` before `<`, or every binding reads as a comparison.
    assert!(position("operators.bind") < position("operators.comparison"));
    // `==`/`!=` before `=`/`!`.
    assert!(position("operators.comparison") < position("operators.assign"));
    assert!(position("operators.comparison") < position("operators.logical"));
    // `->`/`=>` before `-` and `=`.
    assert!(position("operators.arrow") < position("operators.arithmetic"));
    assert!(position("operators.arrow") < position("operators.assign"));
    // `%custom%` before bare `%` modulo.
    assert!(position("operators.custom") < position("operators.arithmetic"));
    // `...`/`..` before the `.` accessor, and `.Variant` before both.
    assert!(position("types.variant") < position("operators.spread"));
    assert!(position("operators.spread") < position("operators.access"));
    // Kind sigils before the operators that share their characters — the
    // exact bug in editors/vim/syntax/typr.vim.
    assert!(position("types.sigil-generic") < position("operators.access"));
    assert!(position("types.sigil-generic") < position("operators.arithmetic"));
    // `@export` before the `@T` interface sigil.
    assert!(position("annotations") < position("types.sigil-generic"));
    // Keywords before the bare-identifier catch-all.
    assert!(position("keywords.declaration") < position("variables.other"));
}
