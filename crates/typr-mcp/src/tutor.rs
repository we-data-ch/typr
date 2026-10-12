//! The TypR tutor: a short, hands-on course in the spirit of `vimtutor`.
//!
//! Each lesson is a TypR file (`data/tutor/NN-slug.ty`) that is at once the
//! lesson's prose (in `#` comments) and its exercise (code with a gap or a
//! bug). The learner edits the file and asks for it to be graded; grading
//! runs the real type checker in process ([`crate::check_source`]) plus a few
//! substring rules, so a lesson can never accept code the compiler rejects.
//!
//! The same catalog serves two front ends:
//!
//! - **without AI**: `typr tutor` (crates/typr-cli/src/tutor.rs) copies the
//!   lessons into a workspace folder and grades them from the terminal;
//! - **with AI**: the `tutor_*` MCP tools and the `tutor` prompt
//!   (crates/typr-mcp/src/lib.rs) let an assistant walk a learner through
//!   the same lessons, grading with the same [`grade`].
//!
//! Every lesson ends by pointing at the documentation pages it summarizes
//! ([`Lesson::docs`]): the tutor is a doorway into the docs, not a substitute
//! for them. `tests::every_doc_link_is_in_the_docs_index` keeps those links
//! in step with the vendored `data/docs-index.md`.
//!
//! Adding a lesson: write the starter and its solution under `data/tutor/`,
//! add a [`Lesson`] row below, and run `cargo test -p typr-mcp tutor` — the
//! tests check that the starter fails, the solution passes, and the
//! "Lesson N of M" header matches.

use schemars::JsonSchema;
use serde::Serialize;

use crate::{check_source_as, explain, Diagnostic};

const DOCS: &str = "https://we-data-ch.github.io/typr.github.io/docs/";

/// What the compiler must say about the learner's code for the lesson to pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Expect {
    /// Type-checks cleanly.
    Compiles,
    /// Parses, but is rejected by the type checker (an inverted exercise:
    /// the learner writes code the compiler must refuse).
    TypeError,
}

/// A substring the learner's code must contain (or must not contain).
/// Matched with whitespace removed on both sides and comments stripped, so
/// `fn(x: num)` matches `fn( x:num )` and nothing in the prose counts.
pub struct Rule {
    pub pattern: &'static str,
    /// Shown to the learner when the rule is broken.
    pub message: &'static str,
}

pub struct DocLink {
    pub title: &'static str,
    /// Path under `typr.github.io/docs/`, e.g. `reference/types`.
    pub path: &'static str,
}

impl DocLink {
    pub fn url(&self) -> String {
        format!("{DOCS}{}", self.path)
    }
}

pub struct Lesson {
    pub number: usize,
    /// File name in the learner's workspace, without extension.
    pub slug: &'static str,
    pub title: &'static str,
    /// One sentence: what the learner can do after this lesson.
    pub goal: &'static str,
    /// The lesson file as handed to the learner: prose + exercise.
    pub starter: &'static str,
    /// A reference solution (never shown unless asked for).
    pub solution: &'static str,
    pub expect: Expect,
    pub requires: &'static [Rule],
    pub forbids: &'static [Rule],
    /// Progressive hints, vaguest first; the last one nearly gives it away.
    pub hints: &'static [&'static str],
    /// Where to read more, shown once the lesson is passed.
    pub docs: &'static [DocLink],
}

impl Lesson {
    pub fn file_name(&self) -> String {
        format!("{}.ty", self.slug)
    }
}

const NO_GAPS: Rule = Rule {
    pattern: "???",
    message: "There is still a '???' in your code: replace every one of them.",
};

macro_rules! lesson_files {
    ($slug:literal) => {
        (
            include_str!(concat!("../data/tutor/", $slug, ".ty")),
            include_str!(concat!("../data/tutor/solutions/", $slug, ".ty")),
        )
    };
}

const L01: (&str, &str) = lesson_files!("01-hello");
const L02: (&str, &str) = lesson_files!("02-types");
const L03: (&str, &str) = lesson_files!("03-break-it");
const L04: (&str, &str) = lesson_files!("04-fix-it");
const L05: (&str, &str) = lesson_files!("05-functions");
const L06: (&str, &str) = lesson_files!("06-three-calls");
const L07: (&str, &str) = lesson_files!("07-records");
const L08: (&str, &str) = lesson_files!("08-methods");
const L09: (&str, &str) = lesson_files!("09-unions-match");
const L10: (&str, &str) = lesson_files!("10-generics");
const L11: (&str, &str) = lesson_files!("11-talk-to-r");
const L12: (&str, &str) = lesson_files!("12-graduation");

pub const LESSONS: &[Lesson] = &[
    Lesson {
        number: 1,
        slug: "01-hello",
        title: "Say hello",
        goal: "Write and check a first TypR statement, ending it with ';'.",
        starter: L01.0,
        solution: L01.1,
        expect: Expect::Compiles,
        requires: &[Rule {
            pattern: "\"Hello, TypR!\"",
            message: "The program should print exactly \"Hello, TypR!\" (a string, in double quotes).",
        }],
        forbids: &[NO_GAPS],
        hints: &[
            "Text in TypR is written between double quotes, as in R.",
            "print() takes the string as its argument: print(\"...\");",
        ],
        docs: &[
            DocLink {
                title: "Getting started",
                path: "intro",
            },
            DocLink {
                title: "The beauty of syntax (why the ';')",
                path: "philosophy/beautiful_syntax",
            },
        ],
    },
    Lesson {
        number: 2,
        slug: "02-types",
        title: "Values and their types",
        goal: "Annotate a binding with one of the four primitive types: int, num, char, bool.",
        starter: L02.0,
        solution: L02.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "let name: char",
                message: "\"Ada\" is text: its type is char.",
            },
            Rule {
                pattern: "let age: int",
                message: "36 has no decimal point: its type is int.",
            },
            Rule {
                pattern: "let height: num",
                message: "1.68 has a decimal point: its type is num.",
            },
            Rule {
                pattern: "let is_admin: bool",
                message: "false is a truth value: its type is bool.",
            },
        ],
        forbids: &[NO_GAPS],
        hints: &[
            "The table in the lesson lists one example value for each type: match each value to its row.",
            "Watch the numbers: one has a decimal point, the other does not.",
        ],
        docs: &[
            DocLink {
                title: "Types",
                path: "reference/types",
            },
            DocLink {
                title: "Lexicon & Literals",
                path: "reference/lexicon",
            },
            DocLink {
                title: "Types for Beginners",
                path: "tutorials/typr-for-dummies",
            },
        ],
    },
    Lesson {
        number: 3,
        slug: "03-break-it",
        title: "Break it on purpose",
        goal: "See the type checker reject a call with a wrong argument type, and read its error.",
        starter: L03.0,
        solution: L03.1,
        expect: Expect::TypeError,
        requires: &[Rule {
            pattern: "let twice <- fn(x: int): int",
            message: "Keep the definition of 'twice' as it is: only add a new line at the end.",
        }],
        forbids: &[],
        hints: &[
            "'twice' expects an int. What other kinds of values do you know from lesson 2?",
            "Try calling it with a string: twice(\"two\");",
        ],
        docs: &[
            DocLink {
                title: "Why this type system",
                path: "philosophy/type-system-design",
            },
            DocLink {
                title: "Philosophy: freedom versus safety",
                path: "philosophy/intro",
            },
        ],
    },
    Lesson {
        number: 4,
        slug: "04-fix-it",
        title: "Fix it",
        goal: "Read two type errors and fix the values without weakening the types.",
        starter: L04.0,
        solution: L04.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "let year: int <-",
                message: "Keep 'let year: int': fix the value, not the annotation.",
            },
            Rule {
                pattern: "fn(x: num): num",
                message: "Keep 'fn(x: num): num': fix the argument, not the signature.",
            },
            Rule {
                pattern: "print(square(",
                message: "Keep the call to square at the end.",
            },
        ],
        forbids: &[],
        hints: &[
            "The first error is about \"2026\": what makes it a char rather than an int?",
            "The second one: square expects a num, and 3 is an int. How do you write three as a num?",
        ],
        docs: &[
            DocLink {
                title: "Types",
                path: "reference/types",
            },
            DocLink {
                title: "Known Pitfalls",
                path: "concepts/known-pitfalls",
            },
        ],
    },
    Lesson {
        number: 5,
        slug: "05-functions",
        title: "Write a typed function",
        goal: "Define a function with typed parameters and a return type.",
        starter: L05.0,
        solution: L05.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "let perimeter <- fn(",
                message: "Define perimeter with fn(...).",
            },
            Rule {
                pattern: "print(perimeter(3.0, 4.0))",
                message: "Keep the call perimeter(3.0, 4.0) at the end.",
            },
        ],
        forbids: &[NO_GAPS],
        hints: &[
            "Start from the shape of 'add' in the lesson: fn(a: T, b: T): T { ... }",
            "Both parameters and the result are num, and the body is 2.0 * (width + height).",
        ],
        docs: &[DocLink {
            title: "Functions",
            path: "reference/functions",
        }],
    },
    Lesson {
        number: 6,
        slug: "06-three-calls",
        title: "One function, three ways to call it",
        goal: "Use the pipe |> and the method style x.f(y) on any function.",
        starter: L06.0,
        solution: L06.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "|> add(",
                message: "'piped' should use the pipe: ... |> add(...)",
            },
            Rule {
                pattern: ").add(",
                message: "'chained' should use the method style: (1).add(...)",
            },
        ],
        forbids: &[NO_GAPS],
        hints: &[
            "The innermost call, add(1, 2), comes first in both rewrites: start from (1).",
            "piped: (1) |> add(2) |> add(3)   and chained follows the same idea with dots.",
        ],
        docs: &[
            DocLink {
                title: "Functions",
                path: "reference/functions",
            },
            DocLink {
                title: "TypR vs R",
                path: "reference/cheatsheet",
            },
        ],
    },
    Lesson {
        number: 7,
        slug: "07-records",
        title: "Model your data with records",
        goal: "Declare a record type and build a value of it.",
        starter: L07.0,
        solution: L07.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "title: char",
                message: "Book needs a field 'title: char'.",
            },
            Rule {
                pattern: "pages: int",
                message: "Book needs a field 'pages: int'.",
            },
            Rule {
                pattern: "Book:{",
                message: "Build 'dune' with the constructor syntax Book:{ ... }.",
            },
        ],
        forbids: &[NO_GAPS],
        hints: &[
            "Fields of a type are written 'name: type', separated by commas.",
            "Building a value uses '=' instead: Book:{ title = \"...\", pages = ... }",
        ],
        docs: &[
            DocLink {
                title: "Records & Constructors",
                path: "reference/records",
            },
            DocLink {
                title: "Model data with TypR types",
                path: "tutorials/typed-data-modeling",
            },
        ],
    },
    Lesson {
        number: 8,
        slug: "08-methods",
        title: "Functions on your types",
        goal: "Write a function on a record and call it in method style.",
        starter: L08.0,
        solution: L08.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "let is_long <- fn(",
                message: "Define is_long with fn(...).",
            },
            Rule {
                pattern: "dune.is_long()",
                message: "Call it in method style: dune.is_long()",
            },
        ],
        forbids: &[NO_GAPS],
        hints: &[
            "The signature is fn(b: Book): bool, and the body compares b$pages with 300.",
            "Then the call is: print(dune.is_long());",
        ],
        docs: &[
            DocLink {
                title: "Functions",
                path: "reference/functions",
            },
            DocLink {
                title: "Interfaces & Structural Validation",
                path: "reference/interfaces",
            },
        ],
    },
    Lesson {
        number: 9,
        slug: "09-unions-match",
        title: "Unions and pattern matching",
        goal: "Handle every case of a union with an exhaustive match.",
        starter: L09.0,
        solution: L09.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "| .Triangle(num)",
                message: "Keep .Triangle(num) in Shape: the point is to handle it, not remove it.",
            },
            Rule {
                pattern: "match s",
                message: "Keep the match in 'area'.",
            },
        ],
        forbids: &[],
        hints: &[
            "Each arm has the form  .Tag(name) => expression,  with a comma at the end.",
            "Add:  .Triangle(side) => 0.433 * side * side,",
        ],
        docs: &[
            DocLink {
                title: "Unions, Tags & Pattern Matching",
                path: "reference/unions-patterns",
            },
            DocLink {
                title: "Control flow",
                path: "reference/control-flow",
            },
        ],
    },
    Lesson {
        number: 10,
        slug: "10-generics",
        title: "Generics: no more NULL surprises",
        goal: "Write a generic function over Option<T>.",
        starter: L10.0,
        solution: L10.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "let unwrap_or <- fn(",
                message: "Define unwrap_or with fn(...).",
            },
            Rule {
                pattern: "match",
                message: "Use a match to look inside the Option.",
            },
            Rule {
                pattern: "let b: char <- unwrap_or(.None, \"empty\")",
                message: "Keep both calls at the bottom unchanged: they test your function.",
            },
        ],
        forbids: &[NO_GAPS],
        hints: &[
            "The signature: fn(o: Option<T>, default: T): T",
            "The body matches o: .Some(x) => x, and .None => the default.",
        ],
        docs: &[
            DocLink {
                title: "Generics & Kind Sigils",
                path: "concepts/generics-kind",
            },
            DocLink {
                title: "Type Constructors & Aliases",
                path: "concepts/type-constructors",
            },
        ],
    },
    Lesson {
        number: 11,
        slug: "11-talk-to-r",
        title: "Talk to R",
        goal: "Type an existing R function with an @ signature.",
        starter: L11.0,
        solution: L11.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "@strrep:",
                message: "Declare strrep's signature with @strrep: (...) -> ...;",
            },
            Rule {
                pattern: "let line: char <- strrep(",
                message: "Keep 'let line: char <- strrep(...)': the signature is what makes it compile.",
            },
        ],
        forbids: &[],
        hints: &[
            "The shape is @name: (parameter types) -> return type;",
            "strrep takes a char and an int, and returns a char.",
        ],
        docs: &[
            DocLink {
                title: "Type existing R functions",
                path: "howto/type-r-functions",
            },
            DocLink {
                title: "Signatures, @extern & Foreign",
                path: "reference/signatures",
            },
            DocLink {
                title: "Use dplyr/tidyr from TypR",
                path: "howto/use-dplyr-tidyr",
            },
            DocLink {
                title: "Escape Hatches",
                path: "reference/escape-hatches",
            },
        ],
    },
    Lesson {
        number: 12,
        slug: "12-graduation",
        title: "Your own program",
        goal: "Combine a type, a typed function and a match in a program of your own.",
        starter: L12.0,
        solution: L12.1,
        expect: Expect::Compiles,
        requires: &[
            Rule {
                pattern: "type ",
                message: "Your program needs at least one 'type'.",
            },
            Rule {
                pattern: "fn(",
                message: "Your program needs at least one typed function 'fn(...)'.",
            },
            Rule {
                pattern: "match ",
                message: "Your program needs at least one 'match'.",
            },
        ],
        forbids: &[NO_GAPS],
        hints: &[
            "Start small: a union type with three tags, then a function that matches on it.",
            "Lesson 9's file is a good model: a union, a function with a match, one call.",
        ],
        docs: &[
            DocLink {
                title: "Create your first TypR package",
                path: "tutorials/first-package",
            },
            DocLink {
                title: "Migrate an existing R package",
                path: "tutorials/migrate-r-package",
            },
            DocLink {
                title: "Connect an AI assistant via MCP",
                path: "howto/mcp-server",
            },
            DocLink {
                title: "Reference",
                path: "reference/intro",
            },
        ],
    },
];

pub fn find(number: usize) -> Option<&'static Lesson> {
    LESSONS.iter().find(|l| l.number == number)
}

/// Remove comments: a `#` followed by whitespace, another `#`, a quote or
/// end of line starts one, outside string literals. A `#` followed by a
/// letter is a kind sigil (`#N`), not a comment.
fn strip_comments(source: &str) -> String {
    let mut out = String::with_capacity(source.len());
    for line in source.lines() {
        let chars: Vec<char> = line.chars().collect();
        let mut quote: Option<char> = None;
        let mut end = chars.len();
        let mut i = 0;
        while i < chars.len() {
            let c = chars[i];
            match quote {
                Some(q) => {
                    if c == '\\' {
                        i += 1;
                    } else if c == q {
                        quote = None;
                    }
                }
                None => {
                    if c == '"' || c == '\'' {
                        quote = Some(c);
                    } else if c == '#' && chars.get(i + 1).map_or(true, |n| !n.is_alphanumeric()) {
                        end = i;
                        break;
                    }
                }
            }
            i += 1;
        }
        out.extend(&chars[..end]);
        out.push('\n');
    }
    out
}

fn squash(s: &str) -> String {
    s.chars().filter(|c| !c.is_whitespace()).collect()
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct Explanation {
    pub code: String,
    pub title: String,
    pub explanation: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct Grade {
    pub lesson: usize,
    pub passed: bool,
    /// What the compiler said about the code (with stable T0xx/S0xx codes).
    pub diagnostics: Vec<Diagnostic>,
    /// Long explanations for the diagnostics' codes, when one exists.
    pub explanations: Vec<Explanation>,
    /// Why the lesson is not passed yet, in the tutor's words.
    pub feedback: Vec<String>,
}

pub fn grade(lesson: &Lesson, source: &str) -> Grade {
    let code = squash(&strip_comments(source));
    let mut feedback = Vec::new();

    for rule in lesson.forbids {
        if code.contains(&squash(rule.pattern)) {
            feedback.push(rule.message.to_string());
        }
    }

    let result = check_source_as(source, &lesson.file_name());
    let syntax_error = result.diagnostics.iter().any(|d| d.code.starts_with('S'));
    match lesson.expect {
        // A leftover '???' already explains the syntax error it causes.
        Expect::Compiles if !result.ok && !(syntax_error && !feedback.is_empty()) => {
            feedback.push(if syntax_error {
                "The compiler cannot read the program yet (syntax error, S code): check the line it points at."
                    .to_string()
            } else {
                "The program does not type-check yet: read the compiler's diagnostics.".to_string()
            });
        }
        Expect::TypeError if result.ok => {
            feedback.push(
                "The compiler still accepts the program: add a call that passes a value of the wrong type.".to_string(),
            );
        }
        Expect::TypeError if syntax_error => {
            feedback.push(
                "That is a syntax error (S code): the compiler could not even read the line. Write a call \
                 that is valid TypR, but with an argument of the wrong type."
                    .to_string(),
            );
        }
        _ => {}
    }

    for rule in lesson.requires {
        if !code.contains(&squash(rule.pattern)) {
            feedback.push(rule.message.to_string());
        }
    }

    let mut explanations: Vec<Explanation> = Vec::new();
    for d in &result.diagnostics {
        if explanations.iter().any(|e| e.code == d.code) {
            continue;
        }
        if let Some(e) = explain::find(&d.code) {
            explanations.push(Explanation {
                code: e.code.to_string(),
                title: e.title.to_string(),
                explanation: e.explanation.to_string(),
            });
        }
    }

    Grade {
        lesson: lesson.number,
        passed: feedback.is_empty(),
        diagnostics: result.diagnostics,
        explanations,
        feedback,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lessons_are_numbered_one_to_n() {
        for (i, lesson) in LESSONS.iter().enumerate() {
            assert_eq!(lesson.number, i + 1, "{}", lesson.slug);
            assert!(lesson.slug.starts_with(&format!("{:02}-", i + 1)), "{}", lesson.slug);
        }
    }

    #[test]
    fn every_starter_announces_its_number() {
        let total = LESSONS.len();
        for lesson in LESSONS {
            let header = format!("Lesson {} of {total}", lesson.number);
            assert!(lesson.starter.contains(&header), "{}: missing '{header}'", lesson.slug);
            assert!(
                lesson.starter.contains(lesson.title),
                "{}: title not in header",
                lesson.slug
            );
        }
    }

    #[test]
    fn every_starter_fails_and_every_solution_passes() {
        for lesson in LESSONS {
            let starter = grade(lesson, lesson.starter);
            assert!(!starter.passed, "{}: the untouched starter already passes", lesson.slug);
            let solution = grade(lesson, lesson.solution);
            assert!(
                solution.passed,
                "{}: the solution fails: {:?} {:?}",
                lesson.slug, solution.feedback, solution.diagnostics
            );
        }
    }

    #[test]
    fn every_lesson_has_hints_and_docs() {
        for lesson in LESSONS {
            assert!(!lesson.hints.is_empty(), "{}", lesson.slug);
            assert!(!lesson.docs.is_empty(), "{}", lesson.slug);
        }
    }

    /// A dead link at the end of a lesson is a door that opens on a wall.
    #[test]
    fn every_doc_link_is_in_the_docs_index() {
        for lesson in LESSONS {
            for doc in lesson.docs {
                let url = doc.url();
                assert!(
                    crate::DOCS_INDEX_MD.contains(&format!("| {url} |")),
                    "{}: {url} is not in data/docs-index.md",
                    lesson.slug
                );
            }
        }
    }

    #[test]
    fn comments_do_not_count_toward_rules() {
        let lesson = find(6).unwrap();
        let g = grade(
            lesson,
            "# |> add( and ).add(\nlet add <- fn(a: int, b: int): int { a + b };",
        );
        assert!(!g.passed);
        assert_eq!(g.feedback.len(), 2, "{:?}", g.feedback);
    }

    #[test]
    fn strip_comments_keeps_hashes_in_strings_and_sigils() {
        assert_eq!(strip_comments("print(\"#1\"); # note"), "print(\"#1\"); \n");
        assert_eq!(strip_comments("fn(x: #N) # c"), "fn(x: #N) \n");
    }

    #[test]
    fn break_it_lesson_rejects_a_syntax_error_as_the_answer() {
        let lesson = find(3).unwrap();
        let source = format!("{}\ntwice(;\n", lesson.starter);
        let g = grade(lesson, &source);
        assert!(!g.passed);
    }

    #[test]
    fn failing_grade_carries_explanations() {
        let lesson = find(4).unwrap();
        let g = grade(lesson, lesson.starter);
        assert!(!g.diagnostics.is_empty());
        assert!(!g.explanations.is_empty(), "{:?}", g.diagnostics);
    }
}
