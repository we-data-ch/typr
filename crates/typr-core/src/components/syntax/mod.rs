//! The TypR **syntax manifest** — the single source of truth for every
//! lexeme the language colors.
//!
//! ## Why this module exists
//!
//! Before it, the same syntax was written down six times by hand (the parser
//! here, VSCode's `typr.tmLanguage.json`, the Playground's Monarch rules,
//! the Docusaurus Prism rules, `editors/vim/syntax/typr.vim`, and the LSP's
//! semantic tokens) and the copies had already drifted apart: the editor
//! grammars colored `impl`/`trait`/`struct`/`enum`/`where`/`mut`/`Option`/
//! `Result` — Rust keywords copy-pasted in that TypR has never had — while
//! `opaque`, `module`, `record`, `object`, `interface`, `typeconstructor`,
//! `recursive`, `embed`, `@export` and the kind sigils were colored nowhere.
//!
//! TextMate (VSCode/Positron/Shiki/Monaco) and tree-sitter (Neovim/Helix/Zed)
//! do not convert into one another, so the source of truth has to sit one
//! level *above* any grammar format: a plain data description of the lexemes,
//! from which each target grammar is **generated**. That is this manifest.
//! `typr syntax` (typr-cli) serializes it and renders the targets.
//!
//! ## Consumers
//!
//! `editors/vscode/syntaxes/typr.tmLanguage.json` is the only artifact rendered
//! today, and it feeds four targets: the VSCode/Positron extension, the
//! playground (Shiki → Monaco, `src/lib/monaco-typr.ts`), the Docusaurus
//! documentation (`@shikijs/rehype`, `src/syntax/shiki.ts`) and — the day TypR
//! clears Linguist's 2000-file bar — GitHub. Both sites keep a copy under
//! `syntaxes/`, pushed there by the release's `grammar` job; it is not
//! hand-edited, and their CI warns when it drifts. Both read the grammar with
//! Shiki's JavaScript regex engine rather than Oniguruma, which is why
//! `RuleBody::Match` sticks to a dialect both engines accept.
//!
//! ## Invariant
//!
//! Every word-like literal the parser matches with `tag("…")` must appear in
//! this manifest. `tests::every_parser_tag_is_in_the_manifest` reads the
//! parser sources with `include_str!` and fails the build when one is
//! missing, so adding a keyword to the grammar without teaching the editors
//! about it is a test failure rather than a silent divergence.

use serde::{Deserialize, Serialize};

/// Bumped whenever the shape of the serialized manifest changes in a way a
/// consumer (a generator, a downstream editor plugin) has to notice.
pub const SCHEMA_VERSION: u32 = 1;

/// Identity of the language, for the target grammars' preamble.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Language {
    /// Editor language id (VSCode `contributes.languages[].id`, Vim filetype).
    pub id: String,
    /// Human-readable display name.
    pub name: String,
    /// TextMate scope name.
    pub scope_name: String,
    pub extensions: Vec<String>,
    pub line_comment: String,
}

/// A kind sigil (RFC `ai_context/sigils.md` §3.2.1), prefixed to a
/// single-uppercase-letter generic to fix its kind: `#N`, `%R`, `@T`, `^S`,
/// `?B`, `$L`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Sigil {
    pub sigil: String,
    /// The kind it denotes (`Number`, `Record`, …).
    pub kind: String,
    /// Suffix appended to `entity.name.type.parameter` in target scopes.
    pub scope_suffix: String,
    /// Reserved by the RFC but not parsed yet. Generators MUST skip these:
    /// `~`, `&` and `!` are live *operators* today, so coloring them as
    /// sigils would mis-highlight real code.
    pub reserved: bool,
    pub note: Option<String>,
}

/// What a rule matches. Ordering of `SyntaxManifest::rules` is significant:
/// earlier rules win, in every target format.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum RuleBody {
    /// Whole-word alternation, guarded by identifier boundaries.
    Words {
        lexemes: Vec<String>,
        /// When set, the word only counts glued to what this regex matches —
        /// the delimiter the parser consumes along with it (`c(`, `seq[`,
        /// `R {`). Without it a variable named `c` would be colored as the
        /// vector constructor.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        followed_by: Option<String>,
    },
    /// Symbol alternation, emitted longest-first so `<=` beats `<`.
    Symbols { lexemes: Vec<String> },
    /// `@`-prefixed annotations — a word rule whose `\b` prefix guard has to
    /// be dropped, since `@` is not a word character.
    Annotations { lexemes: Vec<String> },
    /// The kind sigils, expanded by each generator from `SyntaxManifest::sigils`.
    Sigils,
    /// A raw regex, in the ECMAScript-ish dialect TextMate/Oniguruma and
    /// Shiki's JS engine both accept.
    Match {
        pattern: String,
        /// The same rule in Vim's `\v` (very-magic) dialect. Vim's regex
        /// engine diverges from TextMate/Oniguruma too far for one pattern to
        /// serve both: lookaround is postfix (`atom@=`/`atom@!` instead of
        /// `(?=atom)`/`(?!atom)`), and in `\v` mode bare `<`/`>` mean
        /// word-boundary while a *literal* `<`/`>` character needs escaping
        /// — the opposite of `\b`. Kept side by side with `pattern` so both
        /// targets are generated from the same rule instead of one being
        /// hand-transcribed later.
        vim: String,
    },
    /// A begin/end span (strings).
    Span {
        begin: String,
        end: String,
        /// Whether `\\.` escape sequences are highlighted inside.
        escapes: bool,
    },
}

/// One highlighting rule: a named bucket of lexemes plus the scope the
/// targets should paint them with.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Rule {
    /// Repository key in TextMate, group name elsewhere.
    pub name: String,
    /// TextMate scope name. Other targets map it to their own convention.
    pub scope: String,
    #[serde(flatten)]
    pub body: RuleBody,
    /// Why this rule exists / what it deliberately excludes.
    pub note: Option<String>,
}

impl Rule {
    fn new(name: &str, scope: &str, body: RuleBody) -> Self {
        Rule {
            name: name.to_string(),
            scope: scope.to_string(),
            body,
            note: None,
        }
    }

    fn with_note(mut self, note: &str) -> Self {
        self.note = Some(note.to_string());
        self
    }

    /// The lexemes this rule contributes to the manifest's coverage set.
    pub fn lexemes(&self) -> &[String] {
        match &self.body {
            RuleBody::Words { lexemes, .. } | RuleBody::Symbols { lexemes } | RuleBody::Annotations { lexemes } => {
                lexemes
            }
            _ => &[],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SyntaxManifest {
    pub schema_version: u32,
    pub language: Language,
    pub sigils: Vec<Sigil>,
    /// Ordered; earlier rules take precedence in the generated grammars.
    pub rules: Vec<Rule>,
}

fn words(v: &[&str]) -> RuleBody {
    RuleBody::Words {
        lexemes: v.iter().map(|s| s.to_string()).collect(),
        followed_by: None,
    }
}

/// A word the parser only ever matches glued to a delimiter.
fn words_before(v: &[&str], followed_by: &str) -> RuleBody {
    RuleBody::Words {
        lexemes: v.iter().map(|s| s.to_string()).collect(),
        followed_by: Some(followed_by.to_string()),
    }
}

fn symbols(v: &[&str]) -> RuleBody {
    RuleBody::Symbols {
        lexemes: v.iter().map(|s| s.to_string()).collect(),
    }
}

fn annotations(v: &[&str]) -> RuleBody {
    RuleBody::Annotations {
        lexemes: v.iter().map(|s| s.to_string()).collect(),
    }
}

/// A raw regex rule, authored once in both dialects it has to serve.
fn matches(pattern: &str, vim: &str) -> RuleBody {
    RuleBody::Match {
        pattern: pattern.to_string(),
        vim: vim.to_string(),
    }
}

impl Default for SyntaxManifest {
    fn default() -> Self {
        Self::new()
    }
}

impl SyntaxManifest {
    /// The manifest itself. Every entry below is a lexeme the parser in
    /// `processes/parsing/` (or the tokenizer in `components/language/
    /// operators.rs`) actually matches — nothing aspirational, nothing
    /// inherited from another language's grammar.
    pub fn new() -> Self {
        SyntaxManifest {
            schema_version: SCHEMA_VERSION,
            language: Language {
                id: "typr".to_string(),
                name: "typR".to_string(),
                scope_name: "source.typr".to_string(),
                extensions: vec![".ty".to_string()],
                line_comment: "#".to_string(),
            },
            sigils: Self::sigils(),
            rules: Self::rules(),
        }
    }

    /// RFC `ai_context/sigils.md` §3.2.1. Six are parsed today
    /// (`parsing/types.rs::index_generic`, `label_generic`, `kinded_generic`),
    /// three are reserved and must not reach a grammar.
    fn sigils() -> Vec<Sigil> {
        let live = |sigil: &str, kind: &str, suffix: &str| Sigil {
            sigil: sigil.to_string(),
            kind: kind.to_string(),
            scope_suffix: suffix.to_string(),
            reserved: false,
            note: None,
        };
        let reserved = |sigil: &str| Sigil {
            sigil: sigil.to_string(),
            kind: "Reserved".to_string(),
            scope_suffix: "reserved".to_string(),
            reserved: true,
            note: Some(
                "Reserved by sigils.md §3.2.1 for a future kind, not parsed yet. \
                 Already live as an operator, so generators must not emit it."
                    .to_string(),
            ),
        };
        vec![
            live("#", "Number", "number"),
            live("%", "Record", "record"),
            live("@", "Interface", "interface"),
            live("^", "String", "string"),
            live("?", "Boolean", "boolean"),
            live("$", "Label", "label"),
            reserved("~"),
            reserved("&"),
            reserved("!"),
        ]
    }

    fn rules() -> Vec<Rule> {
        vec![
            // Ordering is load-bearing throughout: every target grammar tries
            // these in sequence at each position, so a shorter lexeme placed
            // ahead of a longer one silently eats its prefix. This is exactly
            // what a hand-written grammar cannot keep straight (the Vim plugin
            // matched single-character operators before the kind sigils and
            // swallowed them) and what generating from one ordered list fixes
            // for every target at once.
            Rule::new(
                "strings.raw-r",
                "string.quoted.other.raw.typr",
                RuleBody::Span {
                    begin: "r#\"".to_string(),
                    end: "\"#".to_string(),
                    escapes: false,
                },
            )
            .with_note(
                "`extern (...) -> T r#\"...\"#` raw R body — verbatim, no escape processing. \
                 Ahead of `comments` so its inner `#` never opens one.",
            ),
            Rule::new(
                "comments",
                "comment.line.number-sign.typr",
                matches(
                    "#(?!(?:Self|[A-Z])(?![A-Za-z0-9_])).*$",
                    "#%(%(Self|[A-Z])[A-Za-z0-9_]@!)@!.*$",
                ),
            )
            .with_note(
                "TypR comments are `#` only — never `//`, which the parser rejects outright \
                 (`wrong_comment` in parsing/mod.rs). The lookahead is the one place a \
                 regex grammar has to guess where the parser uses context: `#N` and `#Self` \
                 are the Number kind sigil, so they are left to `types.sigil-generic`; \
                 anything else after `#` opens a comment.",
            ),
            Rule::new(
                "strings.double",
                "string.quoted.double.typr",
                RuleBody::Span {
                    begin: "\"".to_string(),
                    end: "\"".to_string(),
                    escapes: true,
                },
            ),
            Rule::new(
                "strings.single",
                "string.quoted.single.typr",
                RuleBody::Span {
                    begin: "'".to_string(),
                    end: "'".to_string(),
                    escapes: true,
                },
            ),
            Rule::new(
                "strings.backtick",
                "string.quoted.other.backtick.typr",
                RuleBody::Span {
                    begin: "`".to_string(),
                    end: "`".to_string(),
                    escapes: false,
                },
            )
            .with_note("R-style non-syntactic name (`parsing/elements.rs::quoted_variable`)."),
            Rule::new(
                "numbers.float",
                "constant.numeric.float.typr",
                matches("\\b[0-9]+\\.[0-9]+\\b", "<[0-9]+\\.[0-9]+>"),
            )
            .with_note("`parsing/elements.rs::number` — digits, a dot, digits. No exponent form exists."),
            Rule::new(
                "numbers.integer",
                "constant.numeric.integer.typr",
                matches("\\b[0-9]+\\b", "<[0-9]+>"),
            ),
            Rule::new(
                "annotations",
                "storage.modifier.annotation.typr",
                annotations(&["@export", "@pub", "@testable", "@extern", "@importFrom"]),
            )
            .with_note(
                "Visibility/interop annotations (RFC-TR-032). Ahead of the `@T` interface \
                 sigil and of any bare-`@` rule, both of which would eat their `@`.",
            ),
            Rule::new(
                "keywords.control",
                "keyword.control.typr",
                words(&["if", "else", "match", "for", "while", "loop", "break", "next", "return"]),
            )
            .with_note(
                "No `continue`: the loop-skip keyword is `next;` (R spelling), \
                 `parsing/elements.rs::next_exp`.",
            ),
            Rule::new(
                "keywords.declaration",
                "keyword.declaration.typr",
                words(&[
                    "let",
                    "fn",
                    "function",
                    "type",
                    "opaque",
                    "typeconstructor",
                    "recursive",
                    "interface",
                    "record",
                    "object",
                    "module",
                    "mod",
                    "import",
                    "use",
                    "extern",
                    "embed",
                ]),
            )
            .with_note(
                "No `impl`/`trait`/`struct`/`enum`/`where`/`mut`/`pub`: none has ever existed \
                 in TypR — they were Rust leftovers in the hand-written grammars. Visibility \
                 is spelled with the `@pub`/`@export` annotations.",
            ),
            Rule::new("keywords.cast", "keyword.operator.cast.typr", words(&["as!", "as"]))
                .with_note("`as!` first, or the `as` alternative eats its prefix."),
            Rule::new(
                "keywords.operator-word",
                "keyword.operator.word.typr",
                words(&["and", "or", "in"]),
            )
            .with_note("Word-spelled operators (`components/language/operators.rs::bool_op`/`op`)."),
            Rule::new(
                "constants",
                "constant.language.typr",
                words(&["true", "TRUE", "false", "FALSE", "null", "NULL", "na", "NA"]),
            )
            .with_note(
                "Both spellings are real: `parsing/elements.rs` accepts the R form and the \
                 lowercase TypR form. No `NaN`/`Inf` — the parser has a tag for neither.",
            ),
            Rule::new(
                "types.primitive",
                "support.type.primitive.typr",
                words(&["int", "num", "char", "bool", "logic", "Any", "Empty", "Self"]),
            )
            .with_note("`logic` is the accepted alias of `bool` (`parsing/types.rs::boolean_type`)."),
            Rule::new(
                "types.builtin",
                "support.type.builtin.typr",
                words(&[
                    "Vec",
                    "Array",
                    "Tuple",
                    "Record",
                    "UnknownFunction",
                    "dataframe",
                    "data.frame",
                    "data__frame",
                    "list",
                    "tuple",
                ]),
            )
            .with_note(
                "No `Option`/`Result`/`List`/`Matrix`: TypR has none of them. These are the \
                 names usable bare in a type position; the ones that only exist in front of \
                 a delimiter are in `types.constructor` and `keywords.block`.",
            ),
            Rule::new(
                "types.builtin-indexed",
                "support.type.builtin.typr",
                words_before(&["df"], "\\s*\\["),
            )
            .with_note(
                "`df` is a type name the parser accepts bare too (`parsing/types.rs::dataframe_type`), \
                 but `df` is also the single most common data-frame *variable* name in R. \
                 Coloring it only in `df[...]` under-colors a bare `x: df` annotation; the \
                 alternative over-colors every `df` in every program, which is worse. \
                 `dataframe`, the unambiguous spelling, stays in `types.builtin`.",
            ),
            Rule::new(
                "types.constructor",
                "support.function.builtin.typr",
                words_before(&["c", "seq", "Class", "library"], "\\s*[\\[(]"),
            )
            .with_note(
                "The parser only ever matches these glued to their opening delimiter \
                 (`c(`, `seq[`, `Class(`, `library(`), so the delimiter is part of the rule. \
                 Without it, every variable named `c` would be colored as a builtin.",
            ),
            Rule::new(
                "keywords.block",
                "keyword.other.block.typr",
                words_before(&["R", "JS", "Test"], "\\s*[\\[{]"),
            )
            .with_note(
                "Escape-hatch and test block heads: `R { ... }`, `JS { ... }`, `Test { ... }`, \
                 `Test[...]`. Delimiter-guarded for the same reason as `types.constructor` — \
                 and `R` in particular is a single uppercase letter, i.e. also a valid \
                 generic name, so an unguarded rule would fight `types.generic`.",
            ),
            Rule::new(
                "types.variant",
                "entity.name.type.variant.typr",
                matches("\\.[A-Z][A-Za-z0-9_]*\\b", "\\.[A-Z][A-Za-z0-9_]*>"),
            )
            .with_note("Union-variant tag, `.Variant` (`parsing/elements.rs::tag_exp`)."),
            Rule::new(
                "types.sigil-generic",
                "entity.name.type.parameter.typr",
                RuleBody::Sigils,
            )
            .with_note(
                "Sigil + generic. A generic name is a *single* uppercase letter or `Self` \
                     (`parsing/types.rs::upper_case_generic`), so the rule stops there instead \
                     of running on into a PascalCase alias.",
            ),
            Rule::new(
                "types.generic",
                "entity.name.type.parameter.typr",
                matches("\\b[A-Z](?![A-Za-z0-9_])", "<[A-Z][A-Za-z0-9_]@!"),
            ),
            Rule::new(
                "types.alias",
                "entity.name.type.typr",
                matches("\\b[A-Z][A-Za-z0-9_]+\\b", "<[A-Z][A-Za-z0-9_]+>"),
            )
            .with_note("PascalCase alias / type-constructor name (`parsing/types.rs::pascal_case_no_space`)."),
            Rule::new("operators.arrow", "keyword.operator.arrow.typr", symbols(&["->", "=>"]))
                .with_note("Ahead of comparison and arithmetic, which own `-`, `=` and `>`."),
            Rule::new("operators.bind", "keyword.operator.assignment.typr", symbols(&["<-"]))
                .with_note("Ahead of comparison, whose `<` would otherwise split `<-`."),
            Rule::new(
                "operators.comparison",
                "keyword.operator.comparison.typr",
                symbols(&["==", "!=", "<=", ">=", "<", ">"]),
            ),
            Rule::new(
                "operators.logical",
                "keyword.operator.logical.typr",
                symbols(&["&&", "||", "&", "!"]),
            )
            .with_note(
                "After comparison, so `!=` is not read as `!` then `=`. `|` belongs to \
                 `operators.type-union`, which it doubles as.",
            ),
            Rule::new("operators.pipe", "keyword.operator.pipe.typr", symbols(&["|>"])).with_note(
                "No `|>>`: it has no type-checking or transpiling arm and no stdlib \
                     signature — see the tokenizer purge note on `operators.rs::op`.",
            ),
            Rule::new(
                "operators.type-union",
                "keyword.operator.type-union.typr",
                matches("\\|(?!>)", "\\|\\>@!"),
            ),
            Rule::new(
                "operators.custom",
                "keyword.operator.custom.typr",
                matches("%[^%\\s]*%", "\\%[^%\\s]*\\%"),
            )
            .with_note("R-style custom infix (`operators.rs::custom_op`). Ahead of the bare `%` modulo."),
            Rule::new(
                "operators.arithmetic",
                "keyword.operator.arithmetic.typr",
                symbols(&["+", "-", "*", "/", "%"]),
            )
            .with_note(
                "No `^`/`++`/`--`/`**`/`//`: `op()` recognizes none of them. `^` is the String \
                 kind sigil and nothing else — the hand-written grammars listed it as \
                 exponentiation, which TypR does not have.",
            ),
            Rule::new(
                "operators.vectorial-block",
                "keyword.operator.vectorial.typr",
                symbols(&["@{", "}@"]),
            )
            .with_note("`@{ ... }@` vectorized block (`parsing/elements.rs::vectorial_bloc`)."),
            Rule::new(
                "operators.spread",
                "keyword.operator.spread.typr",
                symbols(&["...", ".."]),
            )
            .with_note(
                "`...` runtime spread / variadic, `..` nominal spread (`Point:{ ..source }`). \
                     Longest-first inside the rule, and ahead of `operators.access`'s `.`.",
            ),
            Rule::new(
                "operators.access",
                "keyword.operator.access.typr",
                symbols(&["::", "$", "."]),
            ),
            Rule::new("operators.assign", "keyword.operator.assignment.typr", symbols(&["="])).with_note(
                "Bare `=` is never an infix operator in TypR — only a binder, a named-field \
                     separator and a default-value separator. Last of the `=`-shaped rules so \
                     `==`, `=>` and `!=` are already claimed.",
            ),
            Rule::new(
                "operators.lambda",
                "keyword.operator.lambda.typr",
                matches("\\\\(?=[({:])", "\\\\[({:]@="),
            )
            .with_note("`\\(x) ...` lambda shorthand (`parsing/elements.rs::lambda`)."),
            Rule::new(
                "functions.call",
                "entity.name.function.typr",
                matches("\\b([a-z_][A-Za-z0-9_]*)\\s*(?=\\()", "<[a-z_][A-Za-z0-9_]*\\s*\\(@="),
            )
            .with_note("After every keyword rule, so `if (`/`while (` stay keywords."),
            Rule::new(
                "variables.parameter",
                "variable.parameter.typr",
                matches(
                    "\\b([a-z_][A-Za-z0-9_]*)\\s*(?=:(?!:))",
                    "<[a-z_][A-Za-z0-9_]*\\s*%(:%(:@!))@=",
                ),
            )
            .with_note("`(?!:)` keeps the `x` of `x::y` out — that is namespace access, not an annotation."),
            Rule::new(
                "variables.other",
                "variable.other.typr",
                matches("\\b[a-z_][A-Za-z0-9_]*\\b", "<[a-z_][A-Za-z0-9_]*>"),
            )
            .with_note(
                "An identifier starts lowercase-or-underscore and continues in `[A-Za-z0-9_]` — \
                 no dots, unlike R (`parsing/elements.rs::starting_char`/`body_char`). \
                 `data.frame` is a single dedicated tag, not an identifier.",
            ),
            Rule::new("punctuation.terminator", "punctuation.terminator.typr", symbols(&[";"])),
            Rule::new(
                "punctuation.separator",
                "punctuation.separator.typr",
                symbols(&[",", ":"]),
            ),
            Rule::new(
                "punctuation.brackets",
                "punctuation.section.brackets.typr",
                matches("[\\[\\](){}]", "[][(){}]"),
            ),
        ]
    }

    /// Every lexeme the manifest claims, flattened — what the parser-coverage
    /// test checks against.
    pub fn all_lexemes(&self) -> Vec<&str> {
        self.rules
            .iter()
            .flat_map(|r| r.lexemes())
            .map(|s| s.as_str())
            .collect()
    }

    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("the syntax manifest is plain data; it cannot fail to serialize")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    /// The parser sources scanned for `tag("…")` literals. Everything that
    /// introduces a *word* of TypR syntax lives in one of these.
    const PARSER_SOURCES: &[(&str, &str)] = &[
        (
            "parsing/elements.rs",
            include_str!("../../processes/parsing/elements.rs"),
        ),
        ("parsing/mod.rs", include_str!("../../processes/parsing/mod.rs")),
        ("parsing/types.rs", include_str!("../../processes/parsing/types.rs")),
        (
            "parsing/indexation.rs",
            include_str!("../../processes/parsing/indexation.rs"),
        ),
        ("language/operators.rs", include_str!("../language/operators.rs")),
    ];

    /// Word-like `tag("…")` literals that deliberately have no manifest
    /// entry. Every exemption needs a reason; the list is meant to stay tiny.
    const NOT_A_LEXEME: &[(&str, &str)] = &[
        (
            "@",
            "Bare `@` heads a module signature declaration (`@name: T;`) and prefixes \
             the annotations. Painting a lone `@` would break `@export` and the `@T` \
             interface sigil, both of which the manifest already covers.",
        ),
        (
            "logical",
            "Not TypR syntax: `RIndex::Logical`'s parser in parsing/indexation.rs \
             matches R's `TRUE`/`FALSE`, and this literal is only in a rustdoc line.",
        ),
    ];

    /// Pulls every `tag("…")` string literal out of a Rust source, handling
    /// the `\"` escapes in literals like `tag("r#\"")`.
    fn scan_tag_literals(source: &str) -> Vec<String> {
        let mut out = Vec::new();
        let bytes: Vec<char> = source.chars().collect();
        let needle: Vec<char> = "tag(\"".chars().collect();
        let mut i = 0;
        while i + needle.len() <= bytes.len() {
            if bytes[i..i + needle.len()] != needle[..] {
                i += 1;
                continue;
            }
            // `tag_no_case(` / `stag(` etc. would end in the same suffix; require
            // the character before `tag` to not be an identifier character.
            let preceded_by_ident = i > 0 && (bytes[i - 1].is_alphanumeric() || bytes[i - 1] == '_');
            if preceded_by_ident {
                i += 1;
                continue;
            }
            let mut j = i + needle.len();
            let mut literal = String::new();
            while j < bytes.len() {
                match bytes[j] {
                    '\\' if j + 1 < bytes.len() => {
                        literal.push(bytes[j + 1]);
                        j += 2;
                    }
                    '"' => break,
                    c => {
                        literal.push(c);
                        j += 1;
                    }
                }
            }
            out.push(literal);
            i = j.max(i + 1);
        }
        out
    }

    /// A parser tag carries its delimiter along (`Array[`, `library(`,
    /// `break;`, `return `) because that is what the combinator consumes.
    /// The manifest stores the bare word.
    fn normalize(literal: &str) -> String {
        literal.trim_end_matches(['(', '[', ';', ' ']).to_string()
    }

    /// Word-like: the shape the manifest is responsible for. Symbols
    /// (`->`, `{`, `..`, `\`) are covered by hand-written symbol rules and
    /// are not part of this check.
    fn is_word_like(lexeme: &str) -> bool {
        let mut chars = lexeme.chars();
        match chars.next() {
            Some(c) if c.is_ascii_alphabetic() || c == '@' => {}
            _ => return false,
        }
        let rest: Vec<char> = chars.collect();
        let (body, _) = match rest.split_last() {
            Some((&'!', body)) => (body, true),
            _ => (&rest[..], false),
        };
        body.iter().all(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '.')
    }

    /// The invariant this whole module exists to enforce: adding a keyword to
    /// the parser without adding it to the manifest fails here, instead of
    /// silently leaving four editor grammars behind.
    #[test]
    fn every_parser_tag_is_in_the_manifest() {
        let manifest = SyntaxManifest::new();
        let known: HashSet<&str> = manifest.all_lexemes().into_iter().collect();
        let exempt: HashSet<&str> = NOT_A_LEXEME.iter().map(|(l, _)| *l).collect();

        let mut missing: Vec<(String, &str)> = Vec::new();
        for (file, source) in PARSER_SOURCES {
            for literal in scan_tag_literals(source) {
                let lexeme = normalize(&literal);
                if lexeme.is_empty() || !is_word_like(&lexeme) {
                    continue;
                }
                if known.contains(lexeme.as_str()) || exempt.contains(lexeme.as_str()) {
                    continue;
                }
                missing.push((lexeme, file));
            }
        }
        missing.sort();
        missing.dedup();

        assert!(
            missing.is_empty(),
            "these lexemes are parsed by TypR but absent from the syntax manifest, so no \
             editor would color them:\n{}\n\nAdd each to a rule in \
             components/syntax/mod.rs (or to NOT_A_LEXEME with a reason), then regenerate \
             the grammars with `typr syntax --write`.",
            missing
                .iter()
                .map(|(lexeme, file)| format!("  - `{lexeme}` (from {file})"))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    /// The other direction: the manifest must not invent syntax. This is the
    /// bug that started the project — `impl`, `trait`, `struct`, `enum`,
    /// `where`, `mut`, `pub`, `continue`, `Option`, `Result`, `List`,
    /// `Matrix`, `Some`, `None`, `Ok`, `Err` were colored by VSCode and the
    /// Playground while existing nowhere in TypR.
    #[test]
    fn manifest_claims_no_word_the_parser_does_not_know() {
        let mut parsed: HashSet<String> = HashSet::new();
        for (_, source) in PARSER_SOURCES {
            for literal in scan_tag_literals(source) {
                parsed.insert(normalize(&literal));
            }
        }
        // Words the grammar reaches through a dedicated combinator rather
        // than a `tag()`, so the scan above cannot see them.
        for extra in ["function", "and", "or"] {
            parsed.insert(extra.to_string());
        }

        let manifest = SyntaxManifest::new();
        let invented: Vec<&str> = manifest
            .all_lexemes()
            .into_iter()
            .filter(|l| is_word_like(l))
            .filter(|l| !parsed.contains(*l))
            .collect();

        assert!(
            invented.is_empty(),
            "the manifest colors words TypR does not have: {invented:?}. \
             Remove them — this is exactly the Rust-keyword copy-paste the manifest replaced."
        );
    }

    #[test]
    fn reserved_sigils_are_flagged() {
        let manifest = SyntaxManifest::new();
        let reserved: Vec<&str> = manifest
            .sigils
            .iter()
            .filter(|s| s.reserved)
            .map(|s| s.sigil.as_str())
            .collect();
        // sigils.md §3.2.1 reserves these three; all three are live operators
        // today, so a generator emitting them would mis-color real code.
        assert_eq!(reserved, vec!["~", "&", "!"]);
        assert_eq!(manifest.sigils.iter().filter(|s| !s.reserved).count(), 6);
    }

    #[test]
    fn rule_names_are_unique() {
        let manifest = SyntaxManifest::new();
        let mut names: Vec<&str> = manifest.rules.iter().map(|r| r.name.as_str()).collect();
        let count = names.len();
        names.sort();
        names.dedup();
        assert_eq!(names.len(), count, "rule names double as TextMate repository keys");
    }
}
