//! `typr-def.toml` — the manifest of an external Type Definition repository.
//!
//! This is the "spec du format figée" item of `typR/registry.md` §13 J2: it
//! fixes the manifest's shape and enforces the `format_version` gate, turning
//! `typr/rfcs/0031-external-type-definitions.md`'s "Definition repository
//! layout and manifest" section into a real type instead of a TOML example in
//! prose. Nothing here fetches a repository, resolves `typr.lock`, or loads a
//! definition into the type checker — those are the RFC's remaining
//! checklist items (registry.md §13 J2: loading into `standard_library.rs`,
//! the `trust` threshold, `typr types add|update|list|vendor`, `cases/`).
//!
//! Example manifest this module parses (RFC §"Definition repository layout
//! and manifest"):
//!
//! ```toml
//! format_version = 1
//!
//! [package]
//! name    = "shiny"
//! since   = "1.11.0"
//! # until = "2.0.0"
//!
//! [definition]
//! version = "0.3.0"
//! tier    = "T2"
//!
//! [provider]
//! type       = "community"
//! repository = "github:alice/typr-shiny"
//!
//! [capabilities]
//! r_shims    = false
//! extern_raw = false
//! ```

#![allow(dead_code)]

use serde::Deserialize;

/// The only `format_version` this build of typr understands. A manifest
/// declaring anything else is refused outright rather than guessed at — see
/// rfcs/0031: "`format_version` is what keeps this survivable across N
/// repositories the project does not control: when the definition format
/// changes, the compiler reads old manifests it recognizes or refuses the
/// ones it doesn't — it never silently misparses one."
pub const CURRENT_FORMAT_VERSION: i64 = 1;

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct DefinitionManifest {
    pub format_version: i64,
    pub package: PackageSection,
    pub definition: DefinitionSection,
    pub provider: ProviderSection,
    #[serde(default)]
    pub capabilities: CapabilitiesSection,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct PackageSection {
    pub name: String,
    /// Minimum R package version this definition was written against — a
    /// floor, never a closed range (`typR/registry.md` §7.2: `supports =
    /// ["1.11.x"]` would depend on the resolving machine and rot unnoticed).
    pub since: String,
    /// Only set when a break is *known*, never speculative.
    #[serde(default)]
    pub until: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct DefinitionSection {
    /// semver of the definition itself, independent of the R package's own
    /// version.
    pub version: String,
    /// Default tier (`T1`/`T2`/`T3`) for entries with no `#! tier:` of their
    /// own. Kept as a free-form string rather than an enum, matching
    /// `FunctionMeta::tier` — an unrecognized future tier degrades instead of
    /// failing the whole manifest to parse.
    pub tier: String,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum ProviderType {
    Official,
    Community,
    Generated,
    Local,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct ProviderSection {
    #[serde(rename = "type")]
    pub kind: ProviderType,
    pub repository: String,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq, Default)]
pub struct CapabilitiesSection {
    /// Ships executable R alongside the declarations (an `R/` shim
    /// directory)?
    #[serde(default)]
    pub r_shims: bool,
    /// Uses an `extern: (...) -> T r#"...R..."#` verbatim block anywhere?
    #[serde(default)]
    pub extern_raw: bool,
}

/// Parse a `typr-def.toml` manifest.
///
/// Checks `format_version` against a raw TOML value first, before attempting
/// to interpret the rest of the document against today's schema — so an
/// unsupported version is reported as exactly that, not as a confusing
/// "missing field" error from a future schema this build does not know about.
pub fn parse_manifest(source: &str) -> Result<DefinitionManifest, String> {
    let raw: toml::Value = source.parse().map_err(|e| format!("not valid TOML: {e}"))?;
    let found = raw
        .get("format_version")
        .ok_or_else(|| "missing required field `format_version`".to_string())?
        .as_integer()
        .ok_or_else(|| "`format_version` must be an integer".to_string())?;
    if found != CURRENT_FORMAT_VERSION {
        return Err(format!(
            "unsupported format_version = {found} (this build of typr understands \
             format_version = {CURRENT_FORMAT_VERSION}); update typr, or ask this \
             definition's provider to publish one this build supports"
        ));
    }
    toml::from_str(source)
        .map_err(|e| format!("manifest does not match format_version {CURRENT_FORMAT_VERSION}: {e}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    const SHINY_MANIFEST: &str = r#"
format_version = 1

[package]
name    = "shiny"
since   = "1.11.0"

[definition]
version = "0.3.0"
tier    = "T2"

[provider]
type       = "community"
repository = "github:alice/typr-shiny"

[capabilities]
r_shims    = false
extern_raw = false
"#;

    #[test]
    fn parses_the_rfc_example_manifest() {
        let manifest = parse_manifest(SHINY_MANIFEST).unwrap();
        assert_eq!(manifest.format_version, 1);
        assert_eq!(manifest.package.name, "shiny");
        assert_eq!(manifest.package.since, "1.11.0");
        assert_eq!(manifest.package.until, None);
        assert_eq!(manifest.definition.version, "0.3.0");
        assert_eq!(manifest.definition.tier, "T2");
        assert_eq!(manifest.provider.kind, ProviderType::Community);
        assert_eq!(manifest.provider.repository, "github:alice/typr-shiny");
        assert!(!manifest.capabilities.r_shims);
        assert!(!manifest.capabilities.extern_raw);
    }

    #[test]
    fn until_is_optional() {
        let manifest = parse_manifest(SHINY_MANIFEST).unwrap();
        assert!(manifest.package.until.is_none());

        let with_until = SHINY_MANIFEST.replacen(
            "since   = \"1.11.0\"",
            "since   = \"1.11.0\"\nuntil   = \"2.0.0\"",
            1,
        );
        let manifest = parse_manifest(&with_until).unwrap();
        assert_eq!(manifest.package.until.as_deref(), Some("2.0.0"));
    }

    #[test]
    fn capabilities_default_to_false_when_section_is_absent() {
        let without_capabilities = SHINY_MANIFEST
            .lines()
            .filter(|l| !l.contains("[capabilities]") && !l.contains("r_shims") && !l.contains("extern_raw"))
            .collect::<Vec<_>>()
            .join("\n");
        let manifest = parse_manifest(&without_capabilities).unwrap();
        assert!(!manifest.capabilities.r_shims);
        assert!(!manifest.capabilities.extern_raw);
    }

    #[test]
    fn missing_format_version_is_refused() {
        let source = SHINY_MANIFEST.replacen("format_version = 1\n", "", 1);
        let err = parse_manifest(&source).unwrap_err();
        assert!(err.contains("format_version"), "unexpected error: {err}");
    }

    #[test]
    fn unknown_format_version_is_refused_not_misparsed() {
        let source = SHINY_MANIFEST.replacen("format_version = 1", "format_version = 2", 1);
        let err = parse_manifest(&source).unwrap_err();
        assert!(err.contains("unsupported format_version = 2"), "unexpected error: {err}");
        assert!(err.contains("format_version = 1"), "unexpected error: {err}");
    }

    #[test]
    fn invalid_toml_is_refused() {
        let err = parse_manifest("this is not { toml").unwrap_err();
        assert!(err.contains("not valid TOML"), "unexpected error: {err}");
    }

    #[test]
    fn invalid_provider_type_is_refused() {
        let source = SHINY_MANIFEST.replacen("type       = \"community\"", "type = \"unofficial\"", 1);
        assert!(parse_manifest(&source).is_err());
    }
}
