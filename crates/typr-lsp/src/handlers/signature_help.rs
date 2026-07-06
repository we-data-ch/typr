//! Token resolution and Markdown-highlighted type display for LSP hover.
//!
//! Given the full source text and a cursor position (line, character),
//! this module:
//!   1. Identifies the word (identifier / literal) under the cursor.
//!   2. Parses and type-checks the whole document using the project's
//!      pipeline (`parse` → `typing`) to build a fully-populated `Context`.
//!   3. Looks up the identifier in that context and returns its type.
//!   4. Renders the type string with Markdown syntax highlighting.

use tower_lsp_server::ls_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureInformation,
};
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;

use super::*;

// ── signature help ──────────────────────────────────────────────────────────

/// Main entry-point called by the LSP signatureHelp handler: resolve the
/// active call's signature(s) against an already-built [`DocumentAnalysis`]
/// (see `analyze_document`).
///
/// Returns `None` when the cursor is not inside a function-call's argument
/// list, or when the call's callee can't be resolved to a `Type::Function`
/// in the typing context.
#[tracing::instrument(skip_all)]
pub fn resolve_signature_help(
    analysis: &DocumentAnalysis,
    content: &str,
    line: u32,
    character: u32,
) -> Option<SignatureHelp> {
    // 1. Find the enclosing call's callee name and which argument the cursor
    // currently sits in (by counting top-level commas back to the call's `(`).
    let (call_name, active_param) = find_enclosing_call(content, line, character)?;

    // 2. Resolve every `Type::Function` overload registered under that name.
    let types = analysis.context.get_types_from_name(&call_name);
    let function_types: Vec<&Type> = types.iter().filter(|t| t.is_function()).collect();
    if function_types.is_empty() {
        return None;
    }

    let signatures: Vec<SignatureInformation> = function_types
        .iter()
        .map(|t| build_signature_information(&call_name, t))
        .collect();

    // Prefer the first overload whose parameter count covers the active
    // index; fall back to the first overload otherwise.
    let active_signature = function_types
        .iter()
        .position(|t| matches!(t, Type::Function(params, _, _) if active_param < params.len()))
        .unwrap_or(0);

    Some(SignatureHelp {
        signatures,
        active_signature: Some(active_signature as u32),
        active_parameter: Some(active_param as u32),
    })
}

/// Build a `SignatureInformation` (label + per-parameter labels) from a
/// `Type::Function`.
fn build_signature_information(name: &str, typ: &Type) -> SignatureInformation {
    if let Type::Function(params, ret, _) = typ {
        let param_labels: Vec<String> = params
            .iter()
            .map(|p| format!("{}: {}", p.get_argument_str(), p.get_type().pretty()))
            .collect();

        let parameters: Vec<ParameterInformation> = param_labels
            .iter()
            .map(|label| ParameterInformation {
                label: ParameterLabel::Simple(label.clone()),
                documentation: None,
            })
            .collect();

        let label = format!("{}({}) -> {}", name, param_labels.join(", "), ret.pretty());

        SignatureInformation {
            label,
            documentation: None,
            parameters: Some(parameters),
            active_parameter: None,
        }
    } else {
        SignatureInformation {
            label: format!("{}: {}", name, typ.pretty()),
            documentation: None,
            parameters: None,
            active_parameter: None,
        }
    }
}

/// Scan backward from the cursor (across up to a few preceding lines, like
/// `extract_multiline_prefix`) to find the innermost unmatched `(` and the
/// callee name immediately preceding it, plus the number of top-level commas
/// between that `(` and the cursor (the active parameter index).
///
/// Returns `None` when the cursor sits directly inside a `[...]`/`{...}`
/// without an enclosing call between it and the scan window (no signature
/// help applies there), or when no callee name precedes the `(`.
fn find_enclosing_call(content: &str, line: u32, character: u32) -> Option<(String, usize)> {
    let prefix = extract_multiline_prefix(content, line, character);
    let chars: Vec<char> = prefix.chars().collect();

    let mut depth = 0i32;
    let mut comma_count = 0usize;
    let mut call_open_idx = None;

    let mut i = chars.len();
    while i > 0 {
        i -= 1;
        match chars[i] {
            ')' | ']' | '}' => depth += 1,
            '(' => {
                if depth == 0 {
                    call_open_idx = Some(i);
                    break;
                }
                depth -= 1;
            }
            '[' | '{' if depth == 0 => return None,
            '[' | '{' => depth -= 1,
            ',' if depth == 0 => comma_count += 1,
            _ => {}
        }
    }

    let open_idx = call_open_idx?;

    let mut end = open_idx;
    while end > 0 && chars[end - 1].is_whitespace() {
        end -= 1;
    }
    let mut start = end;
    while start > 0
        && (chars[start - 1].is_alphanumeric()
            || chars[start - 1] == '_'
            || chars[start - 1] == '.')
    {
        start -= 1;
    }
    if start == end {
        return None;
    }

    let name: String = chars[start..end].iter().collect();
    Some((name, comma_count))
}

#[cfg(test)]
mod signature_help_tests {
    use super::*;

    /// Test-only convenience wrapper around `analyze_document` +
    /// `resolve_signature_help`, mirroring the old single-shot entry point.
    fn find_signature_help_at(
        content: &str,
        line: u32,
        character: u32,
        file_path: &str,
    ) -> Option<SignatureHelp> {
        let analysis = analyze_document(content, file_path)?;
        resolve_signature_help(&analysis, content, line, character)
    }

    /// Cursor right after the `(` of a call to a known function must resolve
    /// its signature with `active_parameter` 0.
    #[test]
    fn resolves_signature_for_first_argument() {
        let content = "let add <- fn(a: int, b: int): int { a + b };\nlet x <- add(";
        let line = content.lines().last().unwrap();
        let help = find_signature_help_at(content, 1, line.len() as u32, "test.ty")
            .expect("expected signature help inside add(...)");

        assert_eq!(help.active_parameter, Some(0));
        let sig = &help.signatures[help.active_signature.unwrap() as usize];
        assert!(
            sig.label.starts_with("add("),
            "unexpected label: {}",
            sig.label
        );
        assert_eq!(sig.parameters.as_ref().unwrap().len(), 2);
    }

    /// After the first comma, the active parameter must advance to index 1.
    #[test]
    fn active_parameter_advances_past_comma() {
        let content = "let add <- fn(a: int, b: int): int { a + b };\nlet x <- add(1, ";
        let line = content.lines().last().unwrap();
        let help = find_signature_help_at(content, 1, line.len() as u32, "test.ty")
            .expect("expected signature help inside add(1, ...)");

        assert_eq!(help.active_parameter, Some(1));
    }

    /// Cursor inside an unrelated `[...]` with no enclosing call must not
    /// produce signature help.
    #[test]
    fn no_signature_help_inside_bare_brackets() {
        let content = "let x <- [1, 2, ";
        let help = find_signature_help_at(content, 0, content.len() as u32, "test.ty");
        assert!(help.is_none());
    }
}
