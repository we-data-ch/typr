#![allow(dead_code)]

use super::model::StdlibMeta;
use std::collections::HashMap;

/// Per-function metadata extracted from `#!` annotation blocks preceding
/// an `@name:` signature in a `.ty` source file.
#[derive(Debug, Clone, Default)]
pub struct FunctionMeta {
    pub pkg: Option<String>,
    pub tier: Option<String>,
    pub param_docs: Vec<(String, String)>,
    pub ret_doc: Option<String>,
    pub coercion_notes: Option<String>,
    pub examples: Vec<String>,
    pub seealso: Vec<String>,
}

impl FunctionMeta {
    /// Convert into the SPG-model `StdlibMeta` (lossless, just field mapping).
    pub fn into_stdlib_meta(self) -> StdlibMeta {
        StdlibMeta {
            tier: self.tier,
            param_docs: self.param_docs,
            ret_doc: self.ret_doc,
            coercion_notes: self.coercion_notes,
            examples: self.examples,
            seealso: self.seealso,
            pkg: self.pkg,
        }
    }

    /// Returns true if this metadata carries any non-empty field.
    pub fn is_non_empty(&self) -> bool {
        self.tier.is_some()
            || !self.param_docs.is_empty()
            || self.ret_doc.is_some()
            || self.coercion_notes.is_some()
            || !self.examples.is_empty()
            || !self.seealso.is_empty()
            || self.pkg.is_some()
    }
}

/// Parse all `#!` annotations from a `.ty` source and return a map from
/// **function name** to its accumulated metadata.
///
/// Rules:
/// - `#!` lines are accumulated until a non-`#!`, non-blank, non-`#`
///   line is encountered.
/// - When that line starts with `@`, it is treated as a signature and
///   the accumulated `#!` block is attached to the function name extracted
///   from the signature.
/// - Any `#!` line *after* a `@` signature (with no intervening non-comment
///   line) is discarded (orphaned annotation = authoring bug, silently ignored).
pub fn parse_meta_from_source(source: &str) -> HashMap<String, FunctionMeta> {
    let mut result: HashMap<String, FunctionMeta> = HashMap::new();
    let mut pending_meta: Option<FunctionMeta> = None;

    for line in source.lines() {
        let trimmed = line.trim();

        if let Some(rest) = trimmed.strip_prefix("#!") {
            let rest = rest.trim();
            // Key is the token before the first whitespace ("pkg", "param",
            // "tier", …), optionally followed by ':'. The value is the rest.
            // Fallback for the compact `key:value` form (no space).
            let (key, value) = match rest.split_once(char::is_whitespace) {
                Some((k, v)) => (k, v.trim()),
                None => match rest.split_once(':') {
                    Some((k, v)) => (k, v.trim()),
                    None => (rest, ""),
                },
            };
            let key = key.trim_end_matches(':').trim();
            let value = value.trim();
            if !key.is_empty() {
                match key {
                    "pkg" => {
                        let target = pending_meta.get_or_insert_with(FunctionMeta::default);
                        target.pkg = Some(strip_leading_colon(value));
                    }
                    "tier" => {
                        let target = pending_meta.get_or_insert_with(FunctionMeta::default);
                        target.tier = Some(strip_leading_colon(value));
                    }
                    "param" => {
                        // "x: description" — param name before first space.
                        if !value.is_empty() {
                            if let Some((pname, desc)) = value.split_once(char::is_whitespace) {
                                let target = pending_meta.get_or_insert_with(FunctionMeta::default);
                                target.param_docs.push((
                                    pname.trim_end_matches(':').trim().to_string(),
                                    desc.trim().to_string(),
                                ));
                            }
                        }
                    }
                    "ret" => {
                        let target = pending_meta.get_or_insert_with(FunctionMeta::default);
                        target.ret_doc = Some(strip_leading_colon(value));
                    }
                    "coercion" | "note" => {
                        let target = pending_meta.get_or_insert_with(FunctionMeta::default);
                        target.coercion_notes = Some(strip_leading_colon(value));
                    }
                    "example" => {
                        let target = pending_meta.get_or_insert_with(FunctionMeta::default);
                        target.examples.push(strip_leading_colon(value));
                    }
                    "seealso" => {
                        let target = pending_meta.get_or_insert_with(FunctionMeta::default);
                        target.seealso.extend(
                            strip_leading_colon(value)
                                .split(',')
                                .map(|s| s.trim().to_string())
                                .filter(|s| !s.is_empty()),
                        );
                    }
                    _ => {} // Unknown key — ignore silently (forward-compatible).
                }
            }
            continue;
        }

        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        // This is a real declaration line.
        if let Some(name) = extract_signature_name(trimmed) {
            if let Some(meta) = pending_meta.take() {
                result.insert(name, meta);
            }
        } else {
            // Non-@ declaration — discard any pending meta (orphaned).
            pending_meta.take();
        }
    }

    result
}

/// Strip an optional leading ':' (for `#! key:value` compact forms) and
/// surrounding whitespace from a metadata value.
fn strip_leading_colon(value: &str) -> String {
    value.trim().trim_start_matches(':').trim().to_string()
}

/// Extract the function name from a `@name: ...` signature line.
///
/// Returns `None` for non-signature lines. Handles `@extern pkg::name` by
/// extracting only the `name` part.
fn extract_signature_name(line: &str) -> Option<String> {
    let rest = line.strip_prefix('@')?;
    // Find the first ':' that is NOT part of a `::` double-colon (which
    // appears in `@extern pkg::name`). The separator colon has non-colon
    // characters on both sides.
    let bytes = rest.as_bytes();
    let mut sep = None;
    for (i, &b) in bytes.iter().enumerate() {
        if b == b':' {
            let prev = if i > 0 { bytes[i - 1] } else { 0 };
            let next = if i + 1 < bytes.len() { bytes[i + 1] } else { 0 };
            if prev != b':' && next != b':' {
                sep = Some(i);
                break;
            }
        }
    }
    let sep = sep?;
    let head = &rest[..sep];
    let name = head
        .strip_prefix("extern ")
        .map(|n| n.rsplit("::").next().unwrap_or(n))
        .unwrap_or(head)
        .trim();
    if name.is_empty() {
        None
    } else {
        Some(name.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_basic_meta_block() {
        let src = "\
#! pkg: base
#! tier: T1
#! param x: some description
#! ret: return desc
#! example: sum(c(1,2,3))
#! seealso: prod, mean
@sum: (vec[N, num]) -> num;";

        let map = parse_meta_from_source(src);
        assert_eq!(map.len(), 1);

        let meta = map.get("sum").unwrap();
        assert_eq!(meta.pkg.as_deref(), Some("base"));
        assert_eq!(meta.tier.as_deref(), Some("T1"));
        assert_eq!(meta.param_docs.len(), 1);
        assert_eq!(meta.param_docs[0].0, "x");
        assert_eq!(meta.param_docs[0].1, "some description");
        assert_eq!(meta.ret_doc.as_deref(), Some("return desc"));
        assert_eq!(meta.examples.len(), 1);
        assert_eq!(meta.examples[0], "sum(c(1,2,3))");
        assert_eq!(meta.seealso, vec!["prod", "mean"]);
    }

    #[test]
    fn orphaned_meta_is_discarded() {
        let src = "\
#! tier: T1
#! param x: desc
let x: int <- 5;";

        let map = parse_meta_from_source(src);
        assert!(map.is_empty());
    }

    #[test]
    fn no_meta_functions_get_empty_map() {
        let src = "@abs: (num) -> num;";
        let map = parse_meta_from_source(src);
        assert!(map.is_empty());
    }

    #[test]
    fn multiple_functions_with_meta() {
        let src = "\
#! pkg: base
#! tier: T1
@abs: (num) -> num;
#! tier: T2
#! param x: input
#! example: mean(c(1,2,3))
@mean: (vec[N, num]) -> num;";

        let map = parse_meta_from_source(src);
        assert_eq!(map.len(), 2);
        assert_eq!(map.get("abs").unwrap().tier.as_deref(), Some("T1"));
        assert_eq!(map.get("mean").unwrap().tier.as_deref(), Some("T2"));
    }

    #[test]
    fn coercion_and_note_are_equivalent() {
        let src = "\
#! coercion: logical -> num
@f1: (num) -> num;
#! note: same thing
@f2: (num) -> num;";

        let map = parse_meta_from_source(src);
        assert_eq!(map.len(), 2);
        assert_eq!(
            map.get("f1").unwrap().coercion_notes.as_deref(),
            Some("logical -> num")
        );
        assert_eq!(
            map.get("f2").unwrap().coercion_notes.as_deref(),
            Some("same thing")
        );
    }

    #[test]
    fn seealso_is_split_on_commas() {
        let src = "\
#! seealso: cumsum, prod, rowSums
@g: (num) -> num;";

        let map = parse_meta_from_source(src);
        let meta = map.get("g").unwrap();
        assert_eq!(meta.seealso, vec!["cumsum", "prod", "rowSums"]);
    }

    #[test]
    fn extern_signature_name_extraction() {
        let src = "\
#! tier: T1
@extern stats::rnorm: (n: int) -> vec[N, num];";

        let map = parse_meta_from_source(src);
        assert!(map.contains_key("rnorm"));
        assert!(!map.contains_key("stats::rnorm"));
    }

    #[test]
    fn empty_tier_is_none() {
        let src = "\
#! param x: desc
@f: (num) -> num;";

        let map = parse_meta_from_source(src);
        let meta = map.get("f").unwrap();
        assert!(meta.tier.is_none());
        assert!(meta.is_non_empty());
    }
}
