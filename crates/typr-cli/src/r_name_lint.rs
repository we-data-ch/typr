//! Phase C of `soundness_transpilation.md` — the static oracle that
//! intersects, at compile time, every top-level name `typr build` is about to
//! emit (`UseMethod` stubs in `generic_functions.R`, record constructors)
//! against what the rest of R's object systems already define. This catches
//! the "collision with the rest of R" bug family without ever running R at
//! build time — the historical example is the `nlevels` bug documented in
//! CLAUDE.md: `nlevels` is a *plain* base-R function (not S3-generic), so
//! shadowing it with a bare `UseMethod` stub and no `nlevels.default` fallback
//! leaves no method to dispatch to.
//!
//! Since that collision has a mechanical fix — define `name.default` so the
//! stub has somewhere to land — this module does not merely report it: it
//! *plans* the fix. [`plan_generic_stubs`] returns the `<name>.default`
//! definitions the build should emit alongside the stubs, and only reports
//! what it cannot fix on its own. The facts it reasons from come from
//! [`RNameCache`], so packages the user installs are covered as well as base R.

use crate::r_name_cache::RNameCache;
use std::collections::BTreeSet;

const STD_R: &str = include_str!("../configs/src/std.R");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintSeverity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct LintFinding {
    pub severity: LintSeverity,
    /// Only read by tests/integration callers matching on the offending
    /// name rather than parsing `message` — the binary's own `report_findings`
    /// prints `message` alone, which already interpolates the name.
    #[allow(dead_code)]
    pub name: String,
    pub message: String,
}

impl LintFinding {
    #[allow(dead_code)]
    pub fn is_error(&self) -> bool {
        self.severity == LintSeverity::Error
    }
}

/// What the build should do about the `UseMethod` stubs it is about to emit.
#[derive(Debug, Clone, Default)]
pub struct StubPlan {
    pub findings: Vec<LintFinding>,
    /// `(name, pkg)` pairs needing a generated `<name>.default` that forwards
    /// to `pkg::name`, because shadowing `name` would otherwise strand it.
    pub auto_defaults: Vec<(String, String)>,
}

impl StubPlan {
    /// The R source for the generated defaults, appended to
    /// `generic_functions.R` next to the stubs themselves.
    pub fn render_auto_defaults(&self) -> String {
        if self.auto_defaults.is_empty() {
            return String::new();
        }
        let body = self
            .auto_defaults
            .iter()
            .map(|(name, pkg)| {
                // `UseMethod` re-matches the original call's arguments against
                // the method's formals, so a bare `...` forwards every call
                // shape the generic accepts — including the zero-argument and
                // variadic signatures a fixed formal list would break.
                format!(
                    "# `{name}` is not S3-generic in `{pkg}`: without this, the stub above\n\
                     # would strand it (\"no applicable method for '{name}'\").\n\
                     {} <- function(...) {pkg}::{}(...)",
                    r_name(&format!("{name}.default")),
                    r_name(name)
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        format!("\n# ---- Generated `.default` fallbacks (see r_name_lint.rs) ----\n{body}\n")
    }
}

/// Backtick an R name that is not syntactic (`%in%`, `[`, …) so it can be used
/// on the left of `<-` and after `::`.
fn r_name(name: &str) -> String {
    let syntactic = {
        let mut chars = name.chars();
        match chars.next() {
            Some(c) if c.is_ascii_alphabetic() || c == '.' => {
                chars.all(|c| c.is_ascii_alphanumeric() || c == '.' || c == '_')
            }
            _ => false,
        }
    };
    if syntactic {
        name.to_string()
    } else {
        format!("`{name}`")
    }
}

/// Whether `<name>.default` is already defined in the R the build is about to
/// emit — either typr's own hand-written fallback in `std.R` (the
/// `max`/`nlevels` pattern) or one written by user/project code
/// (`generated_r`, the transpiled program body; a TypR function whose first
/// parameter is `Any` transpiles to exactly that name).
///
/// The match is boundary-checked: a plain `contains` would see `sum.default`
/// inside `cumsum.default` and wrongly conclude a fallback exists, which would
/// silently suppress the generated one and reopen the very bug this guards.
fn emits_default(name: &str, generated_r: &str) -> bool {
    let needle = format!("{name}.default");
    [STD_R, generated_r].iter().any(|haystack| {
        haystack.match_indices(&needle).any(|(idx, _)| {
            let before = haystack[..idx].chars().next_back();
            !matches!(before, Some(c) if c.is_alphanumeric() || c == '.' || c == '_')
        })
    })
}

/// Stdlib names registered with a **zero-parameter** signature (`() -> T`,
/// e.g. `@dir: () -> [#N, char];` / `@getwd: () -> char;` in `file.ty`) that
/// still get the standard `name <- function(x, ...) UseMethod('name', x)`
/// stub every typed function name receives. Calling them with zero
/// arguments — as their own signature demands — fails on the missing `x`
/// receiver *before* `UseMethod` ever dispatches, regardless of whether a
/// `.default` method exists: generating one cannot fix this. This is a
/// distinct, deeper mismatch between zero-arity stdlib signatures and the
/// generic-stub codegen (`Context::get_all_generic_functions` unconditionally
/// wraps every typed name the same way), not a name collision this module's
/// fallback mechanism can paper over — tracked separately rather than papered
/// over with a `.default` that would never actually be reached.
const ZERO_ARITY_STDLIB_EXEMPT: &[&str] = &["dir", "getwd"];

/// Plan the `UseMethod` stubs the build is about to write.
///
/// `names` are the stub names (see `Context::get_all_generic_functions`),
/// `signature_only` those declared `@name: T;` with no TypR body — the ones
/// whose implementation is expected to already exist in R — `generated_r` the
/// transpiled program body, and `strict` escalates the S4-generic warning to
/// an error.
pub fn plan_generic_stubs(
    names: &[String],
    signature_only: &BTreeSet<String>,
    generated_r: &str,
    cache: &RNameCache,
    strict: bool,
) -> StubPlan {
    let mut plan = StubPlan::default();
    for name in names {
        if ZERO_ARITY_STDLIB_EXEMPT.contains(&name.as_str()) {
            continue;
        }
        let Some(entry) = cache.lookup(name) else {
            // Unknown to the cache. For an ordinary TypR function that is the
            // normal case — it collides with nothing and typr emits its own
            // methods. For a signature-only declaration it is not: the whole
            // point of `@name: T;` is that an R implementation exists
            // somewhere, and we just failed to find it in any known package.
            if signature_only.contains(name) {
                plan.findings.push(LintFinding {
                    severity: LintSeverity::Warning,
                    name: name.clone(),
                    message: format!(
                        "`{name}` is declared as a signature (`@{name}: ...;`) but was not found \
                         in base R or in any package known to the R-name cache, so no \
                         `{name}.default` fallback could be generated. If it comes from a \
                         package, add that package to DESCRIPTION (or declare it as \
                         `@extern pkg::{name}: ...;`) and re-run the build; if it is defined in \
                         hand-written R, define `{name}.default` there instead of `{name}`."
                    ),
                });
            }
            continue;
        };

        // Row 1: not dispatch-capable through *either* object system, and no
        // fallback defined anywhere — the exact shape of the historical
        // `nlevels` bug. This is mechanically fixable, so it is fixed rather
        // than reported: generate `name.default` forwarding to the package's
        // own implementation, which is the `max`/`nlevels` pattern std.R has
        // been applying by hand. Checked independently of the S4 row below: a
        // name that IS a known S4 generic gets only the S4 warning (row 2),
        // since S4 objects do not dispatch through an S3 `.default` anyway.
        if entry.needs_generated_default() && !emits_default(name, generated_r) {
            plan.auto_defaults.push((name.clone(), entry.pkg.clone()));
            continue;
        }

        // Row 2: known S4 generic — the UseMethod stub masks S4 dispatch for
        // any package attached later, regardless of whether an S3 fallback
        // exists.
        if entry.s4_generic {
            let has_fallback = entry.has_default || emits_default(name, generated_r);
            let fallback_note = if has_fallback {
                String::new()
            } else {
                format!(
                    " No `{name}.default` fallback exists either, so any S3-style call to \
                     `{name}` on a non-matching object will also fail at runtime."
                )
            };
            plan.findings.push(LintFinding {
                severity: if strict {
                    LintSeverity::Error
                } else {
                    LintSeverity::Warning
                },
                name: name.clone(),
                message: format!(
                    "`{name}` is a known S4 generic (package `{}`). The `UseMethod` stub \
                     emitted for it shadows S4 dispatch for `{name}` in any package attached \
                     later in the same R session.{fallback_note}",
                    entry.pkg
                ),
            });
        }
    }
    plan
}

/// Lint record-constructor names (`type X <- list { ... }` targets) against
/// known S4 class names.
pub fn lint_record_constructor_names(names: &[String], cache: &RNameCache) -> Vec<LintFinding> {
    names
        .iter()
        .filter(|name| cache.is_s4_class(name))
        .map(|name| LintFinding {
            severity: LintSeverity::Warning,
            name: name.clone(),
            message: format!(
                "record constructor `{name}` has the same name as a built-in S4 class. The \
                 generated `structure(list(...), class = c(\"{name}\", \"list\"))` value is \
                 unrelated to that S4 class, which may confuse code that inspects classes \
                 structurally (e.g. `is(x, \"{name}\")`)."
            ),
        })
        .collect()
}

/// Print findings to stderr the same way syntax/type errors are reported —
/// errors in red, warnings plain. Returns whether any error-level finding
/// was present (callers use this to decide whether to abort the build).
pub fn report_findings(findings: &[LintFinding]) -> bool {
    let mut has_error = false;
    for finding in findings {
        match finding.severity {
            LintSeverity::Error => {
                has_error = true;
                eprintln!("\x1b[31merror[r-name-lint]\x1b[0m: {}", finding.message);
            }
            LintSeverity::Warning => {
                eprintln!("\x1b[33mwarning[r-name-lint]\x1b[0m: {}", finding.message);
            }
        }
    }
    has_error
}

#[cfg(test)]
mod tests {
    use super::*;

    fn plan(names: &[&str], generated_r: &str, strict: bool) -> StubPlan {
        let names: Vec<String> = names.iter().map(|n| n.to_string()).collect();
        plan_generic_stubs(&names, &BTreeSet::new(), generated_r, &RNameCache::seed(), strict)
    }

    #[test]
    fn plain_base_collision_generates_a_default_instead_of_erroring() {
        // `crossprod` is plain (non-generic) base R and typr never defines
        // `crossprod.default` — the shape of the historical `nlevels` bug.
        // It is now fixed by codegen rather than reported.
        let plan = plan(&["crossprod"], "", false);
        assert!(plan.findings.is_empty());
        assert_eq!(plan.auto_defaults, vec![("crossprod".to_string(), "base".to_string())]);
        let r = plan.render_auto_defaults();
        assert!(
            r.contains("crossprod.default <- function(...) base::crossprod(...)"),
            "{r}"
        );
    }

    #[test]
    fn hand_written_default_in_std_r_wins_over_generation() {
        // Regression guard for the historical bug: std.R still defines
        // `nlevels.default`, so nothing is generated for it and nothing is
        // reported.
        assert!(STD_R.contains("nlevels.default"));
        let plan = plan(&["nlevels"], "", false);
        assert!(plan.findings.is_empty());
        assert!(plan.auto_defaults.is_empty());
    }

    #[test]
    fn user_supplied_default_in_the_program_body_wins_too() {
        let plan = plan(&["crossprod"], "crossprod.default <- function(x, ...) x\n", false);
        assert!(plan.auto_defaults.is_empty());
        assert!(plan.findings.is_empty());
    }

    #[test]
    fn a_longer_name_ending_in_the_same_suffix_is_not_mistaken_for_a_default() {
        // `cumprod.default` must not satisfy the `prod.default` lookup. Names
        // absent from std.R are used so only `generated_r` is under test.
        assert!(!emits_default("zz_prod", "cum_zz_prod.default <- function(x, ...) x\n"));
        assert!(!emits_default("zz_prod", "zz_prod2.default <- function(x, ...) x\n"));
        assert!(emits_default("zz_prod", "zz_prod.default <- function(x, ...) x\n"));
        // A definition that is not the first thing on its line still counts.
        assert!(emits_default("zz_prod", "  zz_prod.default <- function(x) x\n"));
    }

    #[test]
    fn already_generic_base_function_needs_nothing() {
        // `print` is S3-generic in base R and already has a base `.default`.
        let plan = plan(&["print"], "", false);
        assert!(plan.findings.is_empty());
        assert!(plan.auto_defaults.is_empty());
    }

    #[test]
    fn s4_generic_is_a_warning_not_strict() {
        let plan = plan(&["show"], "", false);
        assert_eq!(plan.findings.len(), 1);
        assert_eq!(plan.findings[0].severity, LintSeverity::Warning);
    }

    #[test]
    fn s4_generic_is_an_error_under_strict() {
        let plan = plan(&["show"], "", true);
        assert_eq!(plan.findings.len(), 1);
        assert!(plan.findings[0].is_error());
    }

    #[test]
    fn unknown_name_is_silent_for_an_ordinary_function() {
        let plan = plan(&["totally_made_up_name"], "", false);
        assert!(plan.findings.is_empty());
        assert!(plan.auto_defaults.is_empty());
    }

    #[test]
    fn unknown_name_warns_when_it_was_declared_as_a_signature() {
        let signature_only: BTreeSet<String> = ["div".to_string()].into_iter().collect();
        let plan = plan_generic_stubs(&["div".to_string()], &signature_only, "", &RNameCache::seed(), false);
        assert_eq!(plan.findings.len(), 1);
        assert_eq!(plan.findings[0].severity, LintSeverity::Warning);
        assert!(
            plan.findings[0].message.contains("not found"),
            "{}",
            plan.findings[0].message
        );
    }

    #[test]
    fn non_syntactic_names_are_backticked() {
        assert_eq!(r_name("nlevels"), "nlevels");
        assert_eq!(r_name("nlevels.default"), "nlevels.default");
        assert_eq!(r_name("%in%"), "`%in%`");
        assert_eq!(r_name("%in%.default"), "`%in%.default`");
    }

    #[test]
    fn record_constructor_colliding_with_s4_class_is_a_warning() {
        // `factor` is a built-in S4 class name in the base R class table.
        let findings = lint_record_constructor_names(&["factor".to_string()], &RNameCache::seed());
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].severity, LintSeverity::Warning);
    }

    #[test]
    fn record_constructor_with_ordinary_name_is_fine() {
        let findings = lint_record_constructor_names(&["Point".to_string()], &RNameCache::seed());
        assert!(findings.is_empty());
    }
}
