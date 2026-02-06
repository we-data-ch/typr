//! Token resolution and Markdown-highlighted type display for LSP hover.
//!
//! Given the full source text and a cursor position (line, character),
//! this module:
//!   1. Identifies the word (identifier / literal) under the cursor.
//!   2. Parses and type-checks the whole document using the project's
//!      pipeline (`parse` → `typing`) to build a fully-populated `Context`.
//!   3. Looks up the identifier in that context and returns its type.
//!   4. Renders the type string with Markdown syntax highlighting, using
//!      the same semantic categories as the REPL highlighter in `repl.rs`.

use crate::components::context::Context;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::parsing::parse;
use crate::processes::type_checking::typing;
use nom_locate::LocatedSpan;
use crate::components::language::var::Var;
use tower_lsp::lsp_types::{Position, Range};

type Span<'a> = LocatedSpan<&'a str, String>;

/// A resolved hover result: the Markdown-highlighted type and the LSP range.
#[derive(Debug, Clone)]
pub struct HoverInfo {
    /// Markdown string ready to be sent as hover contents.
    pub type_display: String,
    /// The source range of the token that was resolved.
    pub range: Range,
}

// ── public entry-point ─────────────────────────────────────────────────────

/// Main entry-point called by the LSP hover handler.
///
/// Returns `None` when:
///   - the cursor is not on an identifier/literal, or
///   - parsing or type-checking fails (e.g. incomplete code).
pub fn find_type_at(content: &str, line: u32, character: u32) -> Option<HoverInfo> {
    // 1. Extract the word under the cursor.
    let (word, word_range) = extract_word_at(content, line, character)?;

    // 2. Parse the whole document.
    let span: Span = LocatedSpan::new_extra(content, String::new());
    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));
    let ast = ast.ok()?;

    // 3. Type-check the whole document to build the context.
    let context = Context::default();
    let type_context =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)));
    let type_context = type_context.ok()?;
    let final_context = type_context.context;

    // 4. Look up the word in the context.
    let types = final_context.get_types_from_name(&word);

    let typ = if types.is_empty() {
        // Fallback: try to infer the type of the word as a literal.
        infer_literal_type(&word)?
    } else {
        // Pick the most specific (last) type when there are multiple overloads.
        types.last().unwrap().clone()
    };

    // 5. Render with Markdown highlighting.
    let highlighted = highlight_type(&typ.pretty());
    let markdown = format!(
        "**`{}`** : {}\n\n```\n{}\n```",
        word,                          // variable name in bold code
        highlighted,                   // inline Markdown-highlighted type
        typ.pretty()                   // plain code-block fallback (always readable)
    );

    Some(HoverInfo {
        type_display: markdown,
        range: word_range,
    })
}

// ── word extraction ────────────────────────────────────────────────────────

/// Extract the contiguous word (identifier or numeric literal) that contains
/// the given cursor position.  Returns the word text and its LSP Range.
fn extract_word_at(content: &str, line: u32, character: u32) -> Option<(String, Range)> {
    let source_line = content.lines().nth(line as usize)?;

    if (character as usize) > source_line.len() {
        return None;
    }

    let bytes = source_line.as_bytes();
    let col = character as usize;

    // Ensure cursor is on a word character (or one position past one).
    if col >= bytes.len() || !is_word_char(bytes[col]) {
        if col == 0 {
            return None;
        }
        if !is_word_char(bytes[col - 1]) {
            return None;
        }
    }

    let anchor = if col < bytes.len() && is_word_char(bytes[col]) {
        col
    } else {
        col - 1
    };

    let start = {
        let mut i = anchor;
        while i > 0 && is_word_char(bytes[i - 1]) {
            i -= 1;
        }
        i
    };

    let end = {
        let mut i = anchor;
        while i + 1 < bytes.len() && is_word_char(bytes[i + 1]) {
            i += 1;
        }
        i + 1
    };

    let word = &source_line[start..end];
    if word.is_empty() {
        return None;
    }

    Some((
        word.to_string(),
        Range {
            start: Position::new(line, start as u32),
            end: Position::new(line, end as u32),
        },
    ))
}

/// A character is part of a word if it is alphanumeric, an underscore, or a dot
/// (TypR / R identifiers can contain dots).
fn is_word_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'.'
}

// ── literal fallback ───────────────────────────────────────────────────────

/// For numeric literals that are not in the context, return their literal type.
fn infer_literal_type(word: &str) -> Option<Type> {
    use crate::utils::builder;

    if let Ok(i) = word.parse::<i32>() {
        return Some(builder::integer_type(i));
    }
    if let Ok(_f) = word.parse::<f32>() {
        return Some(builder::number_type());
    }
    None
}

// ── Markdown type highlighter ──────────────────────────────────────────────
//
// Semantic colour mapping (mirrors repl.rs `colors` mod):
//
//   REPL colour   │ Markdown rendering     │ Applies to
//   ──────────────┼────────────────────────┼─────────────────────────────
//   KEYWORD       │ ***bold italic***      │ `fn`, `Module`, `interface`
//   FUNCTION/TYPE │ **bold**               │ primitive types, alias names,
//   (cyan)        │                        │   tag names, `list`, `Seq`
//   NUMBER        │ *italic*               │ numeric literals in types
//   STRING        │ `code`                 │ char-literal values
//   OPERATOR      │ plain                  │ `->`, `|`, `&`, `+`, `-`, …
//   BRACKET       │ plain                  │ `( ) [ ] { }`
//   GENERIC       │ *italic*               │ `T`, `A`, `#N`, `%L`
//

/// Primitive type names that should be rendered in **bold** (FUNCTION/TYPE colour).
const PRIMITIVE_TYPES: &[&str] = &["int", "num", "bool", "char", "any", "Empty"];

/// Keywords rendered in ***bold italic*** (KEYWORD colour).
const TYPE_KEYWORDS: &[&str] = &["fn", "Module", "interface", "class"];

/// Highlight a TypR type string into inline Markdown.
///
/// The function does a single forward scan, accumulating word tokens and
/// emitting formatted spans.  Non-word characters (operators, brackets,
/// punctuation) are emitted as-is.
pub fn highlight_type(type_str: &str) -> String {
    let mut out = String::with_capacity(type_str.len() * 2);
    let chars: Vec<char> = type_str.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        let ch = chars[i];

        // ── generic prefixes: #identifier  or  %identifier ──────────────
        if (ch == '#' || ch == '%') && i + 1 < len && (chars[i + 1].is_alphanumeric() || chars[i + 1] == '_') {
            let start = i;
            i += 1; // skip # or %
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("*{}*", token)); // italic (generic)
            continue;
        }

        // ── char-literal string values: sequences like "hello" or 'hello' ─
        if ch == '"' || ch == '\'' {
            let delim = ch;
            let start = i;
            i += 1;
            while i < len && chars[i] != delim {
                i += 1;
            }
            if i < len {
                i += 1; // consume closing delimiter
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("`{}`", token)); // code span (STRING colour)
            continue;
        }

        // ── word token (letter / underscore / digit-start handled below) ──
        if ch.is_alphabetic() || ch == '_' {
            let start = i;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_' || chars[i] == '.') {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();
            out.push_str(&colorize_word(&word));
            continue;
        }

        // ── numeric literal (NUMBER colour → italic) ─────────────────────
        if ch.is_ascii_digit() {
            let start = i;
            while i < len && (chars[i].is_ascii_digit() || chars[i] == '.') {
                i += 1;
            }
            let token: String = chars[start..i].iter().collect();
            out.push_str(&format!("*{}*", token)); // italic
            continue;
        }

        // ── tag dot-prefix: .TagName ──────────────────────────────────────
        if ch == '.' && i + 1 < len && chars[i + 1].is_alphabetic() {
            out.push('.');
            i += 1;
            let start = i;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();
            // Tag names → bold (same as type names)
            out.push_str(&format!("**{}**", word));
            continue;
        }

        // ── arrow operator `->` ───────────────────────────────────────────
        if ch == '-' && i + 1 < len && chars[i + 1] == '>' {
            out.push_str(" → "); // use the prettier unicode arrow, plain text
            i += 2;
            continue;
        }

        // ── everything else: operators, brackets, commas, spaces ──────────
        out.push(ch);
        i += 1;
    }

    out
}

/// Classify a word and wrap it in the appropriate Markdown formatting.
fn colorize_word(word: &str) -> String {
    if TYPE_KEYWORDS.contains(&word) {
        // ***bold italic*** — KEYWORD colour (magenta in REPL)
        format!("***{}***", word)
    } else if PRIMITIVE_TYPES.contains(&word) {
        // **bold** — FUNCTION/TYPE colour (cyan in REPL)
        format!("**{}**", word)
    } else if is_generic_name(word) {
        // *italic* — generic type variable
        format!("*{}*", word)
    } else if word.chars().next().map_or(false, |c| c.is_uppercase()) {
        // Capitalised → user-defined type / alias name → **bold**
        format!("**{}**", word)
    } else {
        // Anything else (field names, variable names inside types) → plain
        word.to_string()
    }
}

/// A generic name is a single uppercase ASCII letter, optionally followed by
/// digits (e.g. `T`, `A`, `T1`).
fn is_generic_name(word: &str) -> bool {
    let mut chars = word.chars();
    match chars.next() {
        Some(c) if c.is_ascii_uppercase() => {
            chars.all(|c| c.is_ascii_digit())
        }
        _ => false,
    }
}

// ══════════════════════════════════════════════════════════════════════════
// ── AUTOCOMPLETION ────────────────────────────────────────────────────────
// ══════════════════════════════════════════════════════════════════════════

use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

/// Main entry point for LSP completion requests.
///
/// Parses and type-checks the entire document, detects the syntactic context
/// at the cursor position, and returns context-appropriate completion items.
///
/// Supports three trigger types:
///   - `|>` (pipe): functions whose first parameter matches the left-hand type
///   - `$` (dollar): record/list fields
///   - `.` (dot): module members, record fields, or applicable functions (hybrid)
pub fn get_completions_at(content: &str, line: u32, character: u32) -> Vec<CompletionItem> {
    // 1. Parse + type-check the whole document.
    let span: Span = LocatedSpan::new_extra(content, String::new());
    let ast = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parse(span)));

    let context = Context::default();
    let final_context = match ast {
        Ok(ast) => {
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| typing(&context, &ast)))
            {
                Ok(tc) => tc.context,
                Err(_) => return get_fallback_completions(),
            }
        }
        Err(_) => return get_fallback_completions(),
    };

    // 2. Extract the line prefix up to the cursor.
    let prefix = extract_line_prefix(content, line, character);

    // 3. Detect the completion context.
    let ctx = detect_completion_context(&prefix);

    // 4. Generate completions based on context.
    match ctx {
        CompletionCtx::Type => get_type_completions(&final_context),
        CompletionCtx::Module(name) => get_module_completions(&final_context, &name),
        CompletionCtx::Pipe(expr) => get_pipe_completions(&final_context, &expr),
        CompletionCtx::RecordField(expr) => get_record_field_completions(&final_context, &expr),
        CompletionCtx::DotAccess(expr) => get_dot_completions(&final_context, &expr),
        CompletionCtx::Expression => get_expression_completions(&final_context),
    }
}

// ── Context detection ──────────────────────────────────────────────────────

#[derive(Debug, Clone)]
enum CompletionCtx {
    /// Type annotation context (e.g., after `let x: ` or in `fn(a: `)
    Type,
    /// Module member access (e.g., `MyModule.`)
    Module(String),
    /// Pipe operator (e.g., `value |>`)
    Pipe(String),
    /// Record field access via $ (e.g., `myrecord$`)
    RecordField(String),
    /// Dot access - hybrid (module members, record fields, or applicable functions)
    DotAccess(String),
    /// General expression context (runtime values)
    Expression,
}

fn extract_line_prefix(content: &str, line: u32, character: u32) -> String {
    content
        .lines()
        .nth(line as usize)
        .and_then(|l| l.get(..character as usize))
        .unwrap_or("")
        .to_string()
}

fn detect_completion_context(prefix: &str) -> CompletionCtx {
    let trimmed = prefix.trim_end();

    // Pipe operator: "expr |>"
    if trimmed.ends_with("|>") {
        let before_pipe = trimmed[..trimmed.len() - 2].trim();
        return CompletionCtx::Pipe(extract_expression_before(before_pipe));
    }

    // Record field access via $: "record$"
    if let Some(dollar_pos) = trimmed.rfind('$') {
        let before_dollar = trimmed[..dollar_pos].trim();
        if !before_dollar.is_empty() {
            return CompletionCtx::RecordField(before_dollar.to_string());
        }
    }

    // Dot access - could be module or record field or function application
    if let Some(dot_pos) = trimmed.rfind('.') {
        let before_dot = trimmed[..dot_pos].trim();
        if !before_dot.is_empty() {
            // Check if it looks like a module name (starts with uppercase)
            if before_dot
                .chars()
                .next()
                .map_or(false, |c| c.is_uppercase())
            {
                return CompletionCtx::Module(before_dot.to_string());
            } else {
                // It's either a record field or a function chain
                return CompletionCtx::DotAccess(before_dot.to_string());
            }
        }
    }

    // Type annotation: "let x: " or "fn(a: "
    if let Some(colon_pos) = trimmed.rfind(':') {
        let after_colon = &trimmed[colon_pos + 1..];
        if !after_colon.contains('=') && !after_colon.contains(';') {
            return CompletionCtx::Type;
        }
    }

    // Type alias definition: "type MyType = "
    if trimmed.trim_start().starts_with("type ") && trimmed.contains('=') {
        return CompletionCtx::Type;
    }

    // Default: expression (runtime values)
    CompletionCtx::Expression
}

/// Extract the expression before a trigger (simple heuristic).
/// Looks backward for word boundaries, operators, or delimiters.
fn extract_expression_before(s: &str) -> String {
    let trimmed = s.trim_end();
    
    // Find the start of the last expression by scanning backwards
    // Stop at operators, semicolons, or opening delimiters
    let mut depth = 0;
    let mut start = trimmed.len();
    
    for (i, ch) in trimmed.char_indices().rev() {
        match ch {
            ')' | ']' | '}' => depth += 1,
            '(' | '[' | '{' => {
                depth -= 1;
                if depth < 0 {
                    start = i + 1;
                    break;
                }
            }
            ';' | ',' if depth == 0 => {
                start = i + 1;
                break;
            }
            _ => {}
        }
    }
    
    trimmed[start..].trim().to_string()
}

// ── Completion generators ──────────────────────────────────────────────────

/// Completions for type annotation contexts.
fn get_type_completions(context: &Context) -> Vec<CompletionItem> {
    use crate::utils::builder;

    let mut items = Vec::new();

    // Primitive types
    let primitives = [
        ("int", builder::integer_type_default()),
        ("num", builder::number_type()),
        ("bool", builder::boolean_type()),
        ("char", builder::character_type_default()),
        ("any", builder::any_type()),
    ];

    for (name, typ) in &primitives {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some(typ.pretty()),
            ..Default::default()
        });
    }

    // User-defined type aliases
    for (var, typ) in context.aliases() {
        if var.is_alias() {
            items.push(var_to_completion_item(
                var,
                typ,
                CompletionItemKind::CLASS,
            ));
        }
    }

    // Module-exported type aliases
    for (var, typ) in context.module_aliases() {
        items.push(var_to_completion_item(
            &var,
            &typ,
            CompletionItemKind::CLASS,
        ));
    }

    items
}

/// Completions for module member access (e.g., `MyModule.█`).
fn get_module_completions(context: &Context, module_name: &str) -> Vec<CompletionItem> {
    let module_context = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        context.extract_module_as_vartype(module_name)
    }));

    let module_ctx = match module_context {
        Ok(ctx) => ctx,
        Err(_) => return Vec::new(),
    };

    let mut items = Vec::new();

    for (var, typ) in module_ctx.variables() {
        let kind = if typ.is_function() {
            CompletionItemKind::FUNCTION
        } else {
            CompletionItemKind::VARIABLE
        };
        items.push(var_to_completion_item(var, typ, kind));
    }

    for (var, typ) in module_ctx.aliases() {
        items.push(var_to_completion_item(var, typ, CompletionItemKind::CLASS));
    }

    items
}

/// Completions for pipe operator (`expr |> █`).
///
/// Filters functions whose first parameter type is compatible with the
/// type of the expression on the left-hand side.
fn get_pipe_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    // Try to infer the type of the expression
    let expr_type = infer_expression_type(context, expr);

    let mut items = Vec::new();

    // Get all functions (user + std)
    let all_functions: Vec<_> = context
        .get_all_generic_functions()
        .into_iter()
        .chain(
            context
                .typing_context
                .standard_library()
                .into_iter()
                .filter(|(_, typ)| typ.is_function())
                .map(|(v, t)| (v.clone(), t.clone())),
        )
        .collect();

    for (var, typ) in all_functions {
        // Check if this function can accept expr_type as first parameter
        if let Some(first_param_type) = get_first_parameter_type(&typ) {
            if expr_type.is_subtype(&first_param_type, context) {
                items.push(var_to_completion_item(
                    &var,
                    &typ,
                    CompletionItemKind::FUNCTION,
                ));
            }
        }
    }

    items
}

/// Completions for record field access via $ (e.g., `myrecord$█`).
///
/// Looks up the type of the record expression and returns its fields.
fn get_record_field_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    let record_type = infer_expression_type(context, expr);

    match record_type {
        Type::Record(fields, _) => fields
            .iter()
            .map(|arg_type| CompletionItem {
                label: arg_type.get_argument_str(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(arg_type.get_type().pretty()),
                ..Default::default()
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Completions for dot access (`.`) — hybrid behavior.
///
/// Returns:
///   1. Module members (if expr looks like a module)
///   2. Record fields (if expr is a record type)
///   3. Applicable functions (like pipe, but for method-style calls)
fn get_dot_completions(context: &Context, expr: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    let expr_type = infer_expression_type(context, expr);

    // 1. If it's a record, show fields
    if let Type::Record(fields, _) = &expr_type {
        for arg_type in fields {
            items.push(CompletionItem {
                label: arg_type.get_argument_str(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(arg_type.get_type().pretty()),
                ..Default::default()
            });
        }
    }

    // 2. Show applicable functions (same logic as pipe)
    let all_functions: Vec<_> = context
        .get_all_generic_functions()
        .into_iter()
        .chain(
            context
                .typing_context
                .standard_library()
                .into_iter()
                .filter(|(_, typ)| typ.is_function())
                .map(|(v, t)| (v.clone(), t.clone())),
        )
        .collect();

    for (var, typ) in all_functions {
        if let Some(first_param_type) = get_first_parameter_type(&typ) {
            if expr_type.is_subtype(&first_param_type, context) {
                items.push(var_to_completion_item(
                    &var,
                    &typ,
                    CompletionItemKind::FUNCTION,
                ));
            }
        }
    }

    items
}

/// Completions for general expression contexts (runtime values).
fn get_expression_completions(context: &Context) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // User-defined variables (non-function values)
    for (var, typ) in context.variables() {
        if !typ.is_function() && !var.is_alias() {
            items.push(var_to_completion_item(
                var,
                typ,
                CompletionItemKind::VARIABLE,
            ));
        }
    }

    // User-defined functions
    for (var, typ) in context.get_all_generic_functions() {
        items.push(var_to_completion_item(
            &var,
            &typ,
            CompletionItemKind::FUNCTION,
        ));
    }

    // Standard library functions
    for (var, typ) in &context.typing_context.standard_library() {
        if typ.is_function() {
            items.push(var_to_completion_item(
                var,
                typ,
                CompletionItemKind::FUNCTION,
            ));
        }
    }

    items
}

/// Fallback completions when parsing or type-checking fails.
fn get_fallback_completions() -> Vec<CompletionItem> {
    use crate::utils::builder;

    let mut items = Vec::new();

    let primitives = [
        ("int", builder::integer_type_default()),
        ("num", builder::number_type()),
        ("bool", builder::boolean_type()),
        ("char", builder::character_type_default()),
        ("any", builder::any_type()),
    ];

    for (name, typ) in &primitives {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some(typ.pretty()),
            ..Default::default()
        });
    }

    items
}

// ── Type inference helpers ─────────────────────────────────────────────────

/// Attempt to infer the type of an expression string.
///
/// This is a best-effort heuristic:
///   - If expr is a known variable name, return its type from context
///   - If expr is a literal, return its literal type
///   - Otherwise, return Any
fn infer_expression_type(context: &Context, expr: &str) -> Type {
    use crate::utils::builder;

    let trimmed = expr.trim();

    // Try to parse as a variable name
    let types = context.get_types_from_name(trimmed);
    if !types.is_empty() {
        return types.last().unwrap().clone();
    }

    // Try to parse as a literal
    if let Some(typ) = infer_literal_type(trimmed) {
        return typ;
    }

    // Default: Any
    builder::any_type()
}

/// Extract the first parameter type from a function type.
fn get_first_parameter_type(typ: &Type) -> Option<Type> {
    match typ {
        Type::Function(params, _, _) => params.first().cloned(),
        _ => None,
    }
}

// ── Helpers ────────────────────────────────────────────────────────────────

/// Convert a `(Var, Type)` pair into a LSP `CompletionItem`.
fn var_to_completion_item(var: &Var, typ: &Type, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: var.get_name(),
        kind: Some(kind),
        detail: Some(typ.pretty()),
        ..Default::default()
    }
}
