use crate::components::context::config::Target;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::help_message::SingleBuilder;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use miette::Result;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SyntaxError {
    FunctionWithoutType(HelpData),
    FunctionWithoutReturnType(HelpData),
    ForgottenSemicolon(HelpData),
    MissingListPrefix(HelpData),
    EmptyFunctionBody(HelpData),
    FunctionTypeSyntax(HelpData),
    RecordConstructorIndex(HelpData),
    RecordInRecursiveParams(HelpData),
    UnknownElement {
        element: String,
        line: u32,
        help_data: HelpData,
    },
    LetInsteadOfType {
        name: String,
        help_data: HelpData,
    },
    TypeInsteadOfLet {
        name: String,
        help_data: HelpData,
    },
    SingleLetterTypeName {
        name: String,
        help_data: HelpData,
    },
    KeywordRecordPositionalElements {
        keyword: String,
        help_data: HelpData,
    },
    MutationTargetNotAssignable(HelpData),
    /// `@target(rus)` / `@extern(js2 = ...)`: a target name the compiler doesn't know.
    UnknownTarget {
        name: String,
        help_data: HelpData,
    },
    /// `@extern base::Sys.sleep: T;`: the name after `@extern` must be a TypR
    /// identifier. An R name with a dot or a capital is given in `(r = "...")`.
    ExternRName {
        written: String,
        suggestion: String,
        help_data: HelpData,
    },
    WrongCommentSyntax(HelpData),
    SingleEqualsComparison(HelpData),
    TupleDestructureArityMismatch {
        expected: usize,
        found: usize,
        help_data: HelpData,
    },
    /// `Lovable @A` / `Lovable@Self`: the `@Id` bound suffix must be one capital
    /// letter touching the interface name.
    DetachedBoundSuffix(HelpData),
    /// `list{a: int}@A` / `df[#N]{a: int}@A`: an `@Id` suffix after a structural
    /// type, where only an interface name carries one.
    StructuralBoundSuffix(HelpData),
    /// `exact int` / `exact Point`: `exact` only prefixes a written-out record or
    /// data-frame type.
    ExactOnNonRecord(HelpData),
    /// `x..f()` / `r..a`: a hidden name right after the UFCS/field `.` operator.
    HiddenNameAfterDot(HelpData),
    /// `@pub let .x` / `@export let __x`: a hidden name is private by name.
    PublicHiddenName {
        name: String,
        annotation: String,
        help_data: HelpData,
    },
    WithNode(Box<Lang>, Box<SyntaxError>),
}

impl SyntaxError {
    /// Get the HelpData containing position information for this error.
    pub fn get_help_data(&self) -> Option<HelpData> {
        match self {
            SyntaxError::FunctionWithoutType(h) => Some(h.clone()),
            SyntaxError::FunctionWithoutReturnType(h) => Some(h.clone()),
            SyntaxError::ForgottenSemicolon(h) => Some(h.clone()),
            SyntaxError::MissingListPrefix(h) => Some(h.clone()),
            SyntaxError::EmptyFunctionBody(h) => Some(h.clone()),
            SyntaxError::FunctionTypeSyntax(h) => Some(h.clone()),
            SyntaxError::RecordConstructorIndex(h) => Some(h.clone()),
            SyntaxError::RecordInRecursiveParams(h) => Some(h.clone()),
            SyntaxError::UnknownElement { help_data, .. } => Some(help_data.clone()),
            SyntaxError::LetInsteadOfType { help_data, .. } => Some(help_data.clone()),
            SyntaxError::TypeInsteadOfLet { help_data, .. } => Some(help_data.clone()),
            SyntaxError::SingleLetterTypeName { help_data, .. } => Some(help_data.clone()),
            SyntaxError::KeywordRecordPositionalElements { help_data, .. } => Some(help_data.clone()),
            SyntaxError::MutationTargetNotAssignable(h) => Some(h.clone()),
            SyntaxError::WrongCommentSyntax(h) => Some(h.clone()),
            SyntaxError::SingleEqualsComparison(h) => Some(h.clone()),
            SyntaxError::TupleDestructureArityMismatch { help_data, .. } => Some(help_data.clone()),
            SyntaxError::DetachedBoundSuffix(h) => Some(h.clone()),
            SyntaxError::StructuralBoundSuffix(h) => Some(h.clone()),
            SyntaxError::ExactOnNonRecord(h) => Some(h.clone()),
            SyntaxError::HiddenNameAfterDot(h) => Some(h.clone()),
            SyntaxError::UnknownTarget { help_data, .. } => Some(help_data.clone()),
            SyntaxError::ExternRName { help_data, .. } => Some(help_data.clone()),
            SyntaxError::PublicHiddenName { help_data, .. } => Some(help_data.clone()),
            SyntaxError::WithNode(_, inner) => inner.get_help_data(),
        }
    }

    /// Stable identifier for this error variant, independent of its message text.
    /// Assigned once in declaration order (S001..) — never renumber an existing
    /// code when adding/removing/reordering variants, only append the next free one.
    /// `WithNode` is a wrapper for attaching AST context, not a distinct error kind,
    /// so it delegates to the wrapped error's code (mirrors `get_help_data`/`simple_message`).
    pub fn code(&self) -> &'static str {
        match self {
            SyntaxError::FunctionWithoutType(..) => "S001",
            SyntaxError::FunctionWithoutReturnType(..) => "S002",
            SyntaxError::ForgottenSemicolon(..) => "S003",
            SyntaxError::MissingListPrefix(..) => "S004",
            SyntaxError::EmptyFunctionBody(..) => "S005",
            SyntaxError::FunctionTypeSyntax(..) => "S006",
            SyntaxError::RecordConstructorIndex(..) => "S007",
            SyntaxError::RecordInRecursiveParams(..) => "S008",
            SyntaxError::UnknownElement { .. } => "S009",
            SyntaxError::LetInsteadOfType { .. } => "S010",
            SyntaxError::TypeInsteadOfLet { .. } => "S011",
            SyntaxError::SingleLetterTypeName { .. } => "S012",
            SyntaxError::KeywordRecordPositionalElements { .. } => "S013",
            SyntaxError::MutationTargetNotAssignable(..) => "S014",
            SyntaxError::WrongCommentSyntax(..) => "S015",
            SyntaxError::SingleEqualsComparison(..) => "S016",
            SyntaxError::TupleDestructureArityMismatch { .. } => "S017",
            SyntaxError::DetachedBoundSuffix(..) => "S018",
            SyntaxError::HiddenNameAfterDot(..) => "S019",
            SyntaxError::PublicHiddenName { .. } => "S020",
            SyntaxError::UnknownTarget { .. } => "S021",
            SyntaxError::ExternRName { .. } => "S022",
            SyntaxError::StructuralBoundSuffix(..) => "S023",
            SyntaxError::ExactOnNonRecord(..) => "S024",
            SyntaxError::WithNode(_, inner) => inner.code(),
        }
    }

    /// Get a simple error message without file access (for LSP use).
    pub fn simple_message(&self) -> String {
        match self {
            SyntaxError::FunctionWithoutType(_) => "Function parameter is missing a type annotation".to_string(),
            SyntaxError::FunctionWithoutReturnType(_) => {
                "Function is missing a return type annotation after ':'".to_string()
            }
            SyntaxError::ForgottenSemicolon(_) => "Missing semicolon at the end of the statement".to_string(),
            SyntaxError::MissingListPrefix(_) => "Missing list prefix ('list' or ':') before the braces".to_string(),
            SyntaxError::EmptyFunctionBody(_) => {
                "Empty function body is not allowed. Use `...` as a placeholder.".to_string()
            }
            SyntaxError::FunctionTypeSyntax(_) => {
                "Function types use parentheses without 'fn': use `(args) -> Type` instead of `fn(args) -> Type`"
                    .to_string()
            }
            SyntaxError::RecordConstructorIndex(_) => {
                "A record constructor takes exactly one integer length: `Name[N]{ ... }`".to_string()
            }
            SyntaxError::RecordInRecursiveParams(_) => {
                "Record blocks `{ ... }` are not allowed inside recursive type parameters".to_string()
            }
            SyntaxError::UnknownElement {
                element,
                line,
                help_data,
            } => {
                format!(
                    "Unknown element `{}` in `{}` at `{}`",
                    element,
                    help_data.get_file_name(),
                    line
                )
            }
            SyntaxError::LetInsteadOfType { name, .. } => {
                format!("Use `type` instead of `let` to create a type alias: `type {name} <- ...`")
            }
            SyntaxError::TypeInsteadOfLet { name, .. } => {
                format!("Use `let` instead of `type` to create a variable binding: `let {name} <- ...`")
            }
            SyntaxError::SingleLetterTypeName { name, .. } => {
                format!(
                    "`{name}` is a single uppercase letter, reserved for generic type variables — use a longer alias name"
                )
            }
            SyntaxError::KeywordRecordPositionalElements { keyword, .. } => {
                format!(
                    "`{keyword}{{...}}` requires named fields (`name = value`) — found a positional element; use `:{{...}}` for a positional tuple instead"
                )
            }
            SyntaxError::MutationTargetNotAssignable(_) => {
                "Mutation target is not assignable: `!;` requires a variable or pipeline/UFC starting with a variable"
                    .to_string()
            }
            SyntaxError::WrongCommentSyntax(_) => "TypR comments use `#`, not `//`".to_string(),
            SyntaxError::SingleEqualsComparison(_) => {
                "`=` is not a comparison operator — did you mean `==`?".to_string()
            }
            SyntaxError::TupleDestructureArityMismatch { expected, found, .. } => {
                format!("Tuple destructuring expects {expected} element(s), but the source tuple has {found}")
            }
            SyntaxError::DetachedBoundSuffix(_) => {
                "The `@Id` suffix must be a single capital letter touching the interface name: write `Lovable@A`, not `Lovable @A` or `Lovable@Self`".to_string()
            }
            SyntaxError::StructuralBoundSuffix(_) => {
                "Only an interface name, a `list{...}` or a `df[...]{...}` can carry an `@Id` suffix (`Lovable@A`, `list{a: int}@A`); tuples and vectors cannot".to_string()
            }
            SyntaxError::ExactOnNonRecord(_) => {
                "`exact` must be followed by a record or data-frame type: `exact list{...}`, `exact df[...]{...}`".to_string()
            }
            SyntaxError::HiddenNameAfterDot(_) => {
                "A hidden name can't follow the `.` operator (`x..f` reads as the `..` spread): write `x.__f()` or `x |> .f()`".to_string()
            }
            SyntaxError::PublicHiddenName { name, annotation, .. } => {
                format!("`{name}` is a hidden name (leading `.`/`__`), private by definition — it can't be `{annotation}`")
            }
            SyntaxError::UnknownTarget { name, .. } => format!(
                "Unknown target `{name}`: valid targets are {}",
                Target::NAMES.join(", ")
            ),
            SyntaxError::ExternRName { written, suggestion, .. } => format!(
                "`{written}` is not a TypR name: write `{suggestion}`"
            ),
            SyntaxError::WithNode(_, inner) => inner.simple_message(),
        }
    }
}

/// Default file data when source is not available
fn default_file_data() -> (String, String) {
    ("std.ty".to_string(), String::new())
}

impl ErrorMsg for SyntaxError {
    fn display(self) -> String {
        let msg: Result<()> = match self {
            SyntaxError::FunctionWithoutType(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Function parameter is missing a type annotation")
                    .pos_text("Here")
                    .help("Add a type after the parameter name, e.g. 'fn(x: Integer): ...'")
                    .build()
            }
            SyntaxError::FunctionWithoutReturnType(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Hey You forgot to specify the function return type after the ':' : 'fn(...): Type'")
                    .pos_text("Here")
                    .help("Just add the type")
                    .build()
            }
            SyntaxError::ForgottenSemicolon(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 1))
                    .text("You forgot a semicolon at the end of your statement")
                    .pos_text("Here")
                    .help("Just add a ';'")
                    .build()
            }
            SyntaxError::MissingListPrefix(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("You forgot to add a list prefix ('list' or ':') before the braces")
                    .pos_text("Here")
                    .help("Add 'list' or ':' before the braces, e.g. 'list {1, 2, 3}' or ': {1, 2, 3}'")
                    .build()
            }
            SyntaxError::EmptyFunctionBody(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Empty function body is not allowed")
                    .pos_text("Empty body here")
                    .help("Use `...` as a placeholder: `fn(): Type { ... }`")
                    .build()
            }
            SyntaxError::FunctionTypeSyntax(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Function types use parentheses without 'fn'")
                    .pos_text("Unexpected 'fn' here")
                    .help("Use `(args) -> Type` instead of `fn(args) -> Type`")
                    .build()
            }
            SyntaxError::RecordConstructorIndex(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("A record constructor takes exactly one integer length")
                    .pos_text("Here")
                    .help("Use `Name[N]{ field: Type, ... }` with a single integer N")
                    .build()
            }
            SyntaxError::RecordInRecursiveParams(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Record blocks `{ ... }` are not allowed inside recursive type parameters")
                    .pos_text("Here")
                    .help("Recursive types only take types or integers, e.g. `Matrix[3, 4, num]`")
                    .build()
            }
            SyntaxError::UnknownElement {
                element,
                line,
                help_data,
            } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name.clone(), text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), element.len()))
                    .text(format!(
                        "Unknown element `{}` in `{}` at `{}`",
                        element, file_name, line
                    ))
                    .pos_text("Here")
                    .build()
            }
            SyntaxError::LetInsteadOfType { name, help_data } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Use `type` instead of `let` to create a type alias")
                    .pos_text("`let` used here")
                    .help(format!("Did you mean `type {name}`?"))
                    .build()
            }
            SyntaxError::TypeInsteadOfLet { name, help_data } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Use `let` instead of `type` to create a variable binding")
                    .pos_text("`type` used here")
                    .help(format!("Did you mean `let {name}`?"))
                    .build()
            }
            SyntaxError::SingleLetterTypeName { name, help_data } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), name.len()))
                    .text("Single uppercase letters are reserved for generic type variables")
                    .pos_text("Here")
                    .help(format!(
                        "Rename `{name}` to a longer alias name, e.g. `{name}b` or a descriptive name"
                    ))
                    .build()
            }
            SyntaxError::KeywordRecordPositionalElements { keyword, help_data } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), keyword.len()))
                    .text(format!("`{keyword}{{...}}` requires named fields (`name = value`)"))
                    .pos_text("Positional element found here")
                    .help("Use `:{...}` for a positional tuple instead, e.g. `:{1, 2, 3}`")
                    .build()
            }
            SyntaxError::MutationTargetNotAssignable(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text("Mutation target is not assignable")
                    .pos_text("Here")
                    .help("`!;` can only be applied to a variable, pipeline (`x |> f()!;`), or UFC (`obj.method()!;`) whose head is a variable")
                    .build()
            }
            SyntaxError::WrongCommentSyntax(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 2))
                    .text("TypR comments use `#`, not `//`")
                    .pos_text("Treated as a comment here")
                    .help("Replace `//` with `#` — the rest of the line was ignored as a comment")
                    .build()
            }
            SyntaxError::SingleEqualsComparison(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 1))
                    .text("`=` is not a comparison operator")
                    .pos_text("Treated as `==` here")
                    .help("Replace `=` with `==` for comparison")
                    .build()
            }
            SyntaxError::TupleDestructureArityMismatch {
                expected,
                found,
                help_data,
            } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Tuple destructuring expects {expected} element(s), but the source tuple has {found}"
                    ))
                    .pos_text("Here")
                    .help(if expected < found {
                        format!(
                            "Add {} more binding(s), or use `_` to ignore the extra element(s)",
                            found - expected
                        )
                    } else {
                        format!(
                            "Remove {} binding(s) — the source tuple only has {found} element(s)",
                            expected - found
                        )
                    })
                    .build()
            }
            SyntaxError::DetachedBoundSuffix(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 1))
                    .text("Malformed `@Id` suffix")
                    .pos_text("Expected `@` + one capital letter, right after the name")
                    .help("Write `Lovable@A` (no space, one-letter id)")
                    .build()
            }
            SyntaxError::StructuralBoundSuffix(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 1))
                    .text("`@Id` after a tuple or vector type")
                    .pos_text("Only an interface name, a `list{...}` or a `df[...]{...}` can carry an `@Id` suffix")
                    .help("Remove the suffix")
                    .build()
            }
            SyntaxError::ExactOnNonRecord(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 5))
                    .text("`exact` must prefix a record or data-frame type")
                    .pos_text("Expected `list{...}` or `df[...]{...}` after `exact`")
                    .help("Write `exact list{a: int}`; to make an alias exact, declare it so: `type Point <- exact list{x: int, y: int}`")
                    .build()
            }
            SyntaxError::HiddenNameAfterDot(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), 1))
                    .text("A hidden name can't follow the `.` operator")
                    .pos_text("`..` here reads as the spread token")
                    .help("Spell the hidden name with `__` after a dot (`x.__f()`, `r.__a`), or use the pipe: `x |> .f()`")
                    .build()
            }
            SyntaxError::ExternRName {
                written,
                suggestion,
                help_data,
            } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), written.len()))
                    .text(format!("`{written}` can't name a TypR function"))
                    .pos_text("An R name with a dot or a capital is not a TypR identifier")
                    .help(format!(
                        "Give the R name in `(r = \"...\")` and pick a snake_case TypR name: {suggestion}"
                    ))
                    .build()
            }
            SyntaxError::UnknownTarget { name, help_data } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), name.len()))
                    .text(format!("Unknown target `{name}`"))
                    .pos_text("Not a compilation target")
                    .help(format!("Valid targets: {}", Target::NAMES.join(", ")))
                    .build()
            }
            SyntaxError::PublicHiddenName {
                name,
                annotation,
                help_data,
            } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .kind("Syntax error")
                    .pos((help_data.get_offset(), name.len()))
                    .text(format!("A hidden name can't be `{annotation}`"))
                    .pos_text("Hidden by its leading `.`/`__`")
                    .help(format!(
                        "Drop `{annotation}` to keep `{name}` private, or rename it without the leading `.`/`__` to make it public"
                    ))
                    .build()
            }
            SyntaxError::WithNode(_, inner) => return inner.display(),
        };
        msg.map_or_else(|e| format!("{:?}", e), |_| String::new())
    }
}

#[cfg(test)]
mod code_tests {
    use std::collections::HashSet;

    /// Slices out the body of `fn <fn_name>` by brace-counting from the first `{`.
    fn extract_fn_body<'a>(src: &'a str, fn_name: &str) -> &'a str {
        let needle = format!("fn {fn_name}(");
        let start = src.find(&needle).unwrap_or_else(|| panic!("fn {fn_name} not found"));
        let rest = &src[start..];
        let brace_start = rest.find('{').unwrap();
        let mut depth = 0i32;
        for (i, c) in rest[brace_start..].char_indices() {
            match c {
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        return &rest[brace_start..brace_start + i + 1];
                    }
                }
                _ => {}
            }
        }
        panic!("unbalanced braces in fn {fn_name}");
    }

    /// Extracts every quoted string literal shaped like a stable error code
    /// (one uppercase letter + 3 digits, e.g. `S012`).
    fn extract_codes(src: &str) -> Vec<String> {
        let mut codes = Vec::new();
        let mut in_string = false;
        let mut current = String::new();
        for c in src.chars() {
            if c == '"' {
                if in_string {
                    if current.len() == 4
                        && current.chars().next().is_some_and(|c| c.is_ascii_uppercase())
                        && current[1..].chars().all(|d| d.is_ascii_digit())
                    {
                        codes.push(current.clone());
                    }
                    current.clear();
                } else {
                    current.clear();
                }
                in_string = !in_string;
            } else if in_string {
                current.push(c);
            }
        }
        codes
    }

    #[test]
    fn syntax_error_codes_are_unique_and_cover_every_variant() {
        let src = include_str!("syntax_error.rs");
        let body = extract_fn_body(src, "code");
        let arm_count = body.matches("=>").count();
        let codes = extract_codes(body);

        // `WithNode` delegates to the wrapped error's code instead of returning
        // a literal, so it contributes one arm but no code of its own.
        assert_eq!(
            codes.len() + 1,
            arm_count,
            "every match arm in SyntaxError::code() but WithNode must return a literal `S0xx` code"
        );

        let unique: HashSet<&String> = codes.iter().collect();
        assert_eq!(
            codes.len(),
            unique.len(),
            "duplicate SyntaxError codes found: {:?}",
            codes
        );

        for code in &codes {
            assert!(code.starts_with('S'), "SyntaxError code must start with 'S': {code}");
        }
    }
}
