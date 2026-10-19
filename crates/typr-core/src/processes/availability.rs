//! Target availability (rfcs/0000-compilation-targets.md §4).
//!
//! Every top-level definition gets an *availability set*: the targets it can be
//! compiled to. It is inferred from what the definition uses, never written by
//! hand (`@target(...)` only states an upper bound that is checked). A root —
//! the top level, `main`, a `@pub`/`@export` member — that is unavailable on a
//! requested target is a compile error (T052) naming the chain of definitions
//! down to the construct that is tied to another target.
//!
//! The pass runs on the parsed AST, after type checking, and changes nothing
//! about it. Two simplifications for now, both listed in the RFC's checklist
//! as work for the next steps:
//!
//! * names are resolved by spelling inside one source file (no cross-file or
//!   cross-module resolution), and a local binding shadows a definition for the
//!   whole body that contains it;
//! * type aliases are not definitions: a data frame or `Foreign<T>` written
//!   *directly* in a signature or body counts, one hidden behind an alias doesn't.
//!
//! The AST is walked through its `serde` form. `Lang` has ~60 variants and no
//! generic visitor; a new variant is covered automatically instead of being
//! silently skipped by a hand-written traversal.

use crate::components::context::config::Target;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::{TargetUnavailable, TargetViolation, TypeError};
use crate::components::error_message::typr_error::TypRError;
use crate::components::language::{Lang, TargetAttr};
use crate::components::r#type::Type;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

/// The targets a definition can be declared for. `js` stays dormant.
fn universe() -> BTreeSet<Target> {
    [Target::R, Target::Rust].into_iter().collect()
}

/// Availability of the standard library and of preloaded R functions, by name.
/// An entry that isn't in `targets` is `r` only: every current entry is backed
/// by `std.R` alone (§4).
#[derive(Debug, Clone, Default)]
pub struct StdlibAvailability {
    /// Every name the preloaded context knows (standard library, R builtins).
    pub known: HashSet<String>,
    /// Entries that declared `#! targets:`.
    pub targets: HashMap<String, BTreeSet<Target>>,
}

impl StdlibAvailability {
    /// Names known to a context, none of them portable yet.
    pub fn from_context(context: &crate::components::context::Context) -> Self {
        let known = context
            .typing_context
            .variables()
            .chain(context.typing_context.std.iter())
            .map(|(var, _)| var.get_name())
            .collect();
        // `length` is an R builtin the standard library must not declare (it would shadow it), so
        // it cannot carry `#! targets:`; every other portable entry does (`with_targets`, fed from
        // the `.ty` sources by the CLI).
        let both: BTreeSet<Target> = [Target::R, Target::Rust].into_iter().collect();
        let targets = super::rust_backend::UNDECLARABLE_INTRINSICS
            .iter()
            .map(|n| (n.to_string(), both.clone()))
            .collect();
        StdlibAvailability { known, targets }
    }

    /// Adds the entries that declared `#! targets:` (on top of what is already known).
    pub fn with_targets(mut self, targets: HashMap<String, BTreeSet<Target>>) -> Self {
        self.targets.extend(targets);
        self
    }

    fn lookup(&self, name: &str) -> Option<BTreeSet<Target>> {
        if !self.known.contains(name) {
            return None;
        }
        Some(
            self.targets
                .get(name)
                .cloned()
                .unwrap_or_else(|| [Target::R].into_iter().collect()),
        )
    }
}

/// A construct that is tied to some targets.
#[derive(Debug, Clone)]
struct Leak {
    what: String,
    avail: BTreeSet<Target>,
    site: HelpData,
}

#[derive(Debug, Clone)]
struct Reference {
    name: String,
    site: HelpData,
    is_call: bool,
}

#[derive(Debug, Clone)]
struct Def {
    name: String,
    site: HelpData,
    declared: Option<(BTreeSet<Target>, HelpData)>,
    externs: BTreeSet<Target>,
    has_body: bool,
    is_root: bool,
    leaks: Vec<Leak>,
    refs: Vec<Reference>,
}

impl Def {
    fn new(name: &str, site: HelpData) -> Def {
        Def {
            name: name.to_string(),
            site,
            declared: None,
            externs: BTreeSet::new(),
            has_body: true,
            is_root: false,
            leaks: Vec::new(),
            refs: Vec::new(),
        }
    }

    fn declared_set(&self) -> BTreeSet<Target> {
        self.declared.as_ref().map(|(d, _)| d.clone()).unwrap_or_else(universe)
    }
}

/// Name of the pseudo-definition that gathers the top-level statements.
pub const TOP_LEVEL: &str = "<top level>";

pub fn check(program: &Lang, stdlib: &StdlibAvailability, targets: &[Target]) -> Vec<TypRError> {
    let defs = collect(program);
    let avail = solve(&defs, stdlib);
    let mut errors = Vec::new();

    // `@target(...)` is checked as well as applied: on the definition itself.
    for d in &defs {
        let Some((declared, site)) = &d.declared else { continue };
        let raw = raw_availability(d, &avail, stdlib, &defs);
        for t in declared {
            if raw.contains(t) {
                continue;
            }
            let (reason, reason_site) = match explain(d, *t, &defs, &avail, stdlib) {
                Some(e) => (e.reason(), e.site),
                None => ("nothing provides it".to_string(), d.site.clone()),
            };
            errors.push(TypRError::Type(TypeError::TargetDeclarationViolated(Box::new(
                TargetViolation {
                    name: d.name.clone(),
                    declared: declared.iter().copied().collect(),
                    target: *t,
                    declared_site: site.clone(),
                    reason,
                    reason_site,
                },
            ))));
        }
    }

    // Roots must be available on every requested target. A definition whose
    // own `@target(...)` was just reported is not reported a second time.
    for t in targets {
        for d in defs.iter().filter(|d| d.is_root) {
            if avail[&d.name].contains(t) {
                continue;
            }
            if d.declared.as_ref().is_some_and(|(decl, _)| decl.contains(t)) {
                continue;
            }
            let Some(e) = explain(d, *t, &defs, &avail, stdlib) else {
                continue;
            };
            let offender = e.chain.get(1).cloned().unwrap_or_else(|| d.name.clone());
            let offender_targets = avail
                .get(&offender)
                .cloned()
                .or_else(|| stdlib.lookup(&offender))
                .unwrap_or_default();
            errors.push(TypRError::Type(TypeError::NotAvailableOnTarget(Box::new(
                TargetUnavailable {
                    root: d.name.clone(),
                    target: *t,
                    use_site: e.first_use.clone().unwrap_or_else(|| e.site.clone()),
                    chain: e.chain[1..].to_vec(),
                    offender,
                    offender_targets: offender_targets.into_iter().collect(),
                    reason: e.reason(),
                    reason_site: e.site.clone(),
                },
            ))));
        }
    }
    errors
}

/// Availability of every definition: a greatest fixed point, so a cycle of
/// definitions that only use each other stays portable (§4, rule 5).
fn solve(defs: &[Def], stdlib: &StdlibAvailability) -> BTreeMap<String, BTreeSet<Target>> {
    let mut avail: BTreeMap<String, BTreeSet<Target>> = BTreeMap::new();
    for d in defs {
        avail.entry(d.name.clone()).or_insert_with(universe);
    }
    loop {
        let mut changed = false;
        for d in defs {
            let next = availability_of(d, &avail, stdlib, defs);
            let cur = avail.get_mut(&d.name).expect("seeded above");
            // Several definitions can share a name (overloads): they all must agree.
            let narrowed: BTreeSet<Target> = cur.intersection(&next).copied().collect();
            if narrowed != *cur {
                *cur = narrowed;
                changed = true;
            }
        }
        if !changed {
            return avail;
        }
    }
}

/// `avail_body(d) ∪ externs(d)`, before `@target(...)` is applied.
fn raw_availability(
    d: &Def,
    avail: &BTreeMap<String, BTreeSet<Target>>,
    stdlib: &StdlibAvailability,
    defs: &[Def],
) -> BTreeSet<Target> {
    let mut body = if d.has_body { universe() } else { BTreeSet::new() };
    if d.has_body {
        for leak in &d.leaks {
            body = body.intersection(&leak.avail).copied().collect();
        }
        for r in &d.refs {
            if let Some(a) = referenced_availability(r, avail, stdlib, defs) {
                body = body.intersection(&a).copied().collect();
            }
        }
    }
    body.union(&d.externs).copied().collect()
}

fn availability_of(
    d: &Def,
    avail: &BTreeMap<String, BTreeSet<Target>>,
    stdlib: &StdlibAvailability,
    defs: &[Def],
) -> BTreeSet<Target> {
    raw_availability(d, avail, stdlib, defs)
        .intersection(&d.declared_set())
        .copied()
        .collect()
}

/// What using `r` contributes: a project definition's current availability, or a
/// called stdlib / preloaded R function's. Anything else (a local variable, an
/// unknown name) contributes nothing.
fn referenced_availability(
    r: &Reference,
    avail: &BTreeMap<String, BTreeSet<Target>>,
    stdlib: &StdlibAvailability,
    defs: &[Def],
) -> Option<BTreeSet<Target>> {
    if defs.iter().any(|d| d.name == r.name) {
        return avail.get(&r.name).cloned();
    }
    if r.is_call {
        return stdlib.lookup(&r.name);
    }
    None
}

/// Why a definition lacks a target: the shortest chain to a leaf that does.
struct Explanation {
    /// `[root, callee, …, leaf]`.
    chain: Vec<String>,
    /// Where the root first uses `chain[1]`.
    first_use: Option<HelpData>,
    /// The construct (or the missing implementation) at the end of the chain.
    leaf: LeafReason,
    site: HelpData,
}

enum LeafReason {
    Construct(String),
    NoImplementation(Target),
    Stdlib,
    Declared(Target),
}

impl Explanation {
    fn reason(&self) -> String {
        match &self.leaf {
            LeafReason::Construct(what) => format!("uses {what}"),
            LeafReason::NoImplementation(t) => format!("has no implementation for `{t}`"),
            LeafReason::Stdlib => "is only available on `r` (the standard library is not portable yet)".to_string(),
            LeafReason::Declared(t) => format!("is declared without `{t}`"),
        }
    }
}

fn explain(
    start: &Def,
    target: Target,
    defs: &[Def],
    avail: &BTreeMap<String, BTreeSet<Target>>,
    stdlib: &StdlibAvailability,
) -> Option<Explanation> {
    // Breadth-first, so the chain shown is the shortest one.
    let mut queue: std::collections::VecDeque<(Vec<String>, Option<HelpData>, &Def)> = std::collections::VecDeque::new();
    let mut seen: HashSet<String> = HashSet::new();
    seen.insert(start.name.clone());
    queue.push_back((vec![start.name.clone()], None, start));
    while let Some((chain, first_use, d)) = queue.pop_front() {
        if d.has_body {
            if let Some(leak) = d.leaks.iter().find(|l| !l.avail.contains(&target)) {
                return Some(Explanation {
                    chain,
                    first_use,
                    leaf: LeafReason::Construct(leak.what.clone()),
                    site: leak.site.clone(),
                });
            }
        } else if !d.externs.contains(&target) {
            return Some(Explanation {
                chain,
                first_use,
                leaf: LeafReason::NoImplementation(target),
                site: d.site.clone(),
            });
        }
        if let Some((decl, _)) = &d.declared {
            if !decl.contains(&target) && !std::ptr::eq(d, start) {
                return Some(Explanation {
                    chain,
                    first_use,
                    leaf: LeafReason::Declared(target),
                    site: d.site.clone(),
                });
            }
        }
        for r in &d.refs {
            let next = defs.iter().find(|x| x.name == r.name);
            match next {
                Some(n) => {
                    let unavailable = avail.get(&n.name).is_some_and(|a| !a.contains(&target));
                    if unavailable && seen.insert(n.name.clone()) {
                        let mut c = chain.clone();
                        c.push(n.name.clone());
                        let fu = first_use.clone().or_else(|| Some(r.site.clone()));
                        queue.push_back((c, fu, n));
                    }
                }
                None if r.is_call => {
                    if stdlib.lookup(&r.name).is_some_and(|a| !a.contains(&target)) {
                        let mut c = chain.clone();
                        c.push(r.name.clone());
                        return Some(Explanation {
                            chain: c,
                            first_use: first_use.or_else(|| Some(r.site.clone())),
                            leaf: LeafReason::Stdlib,
                            site: r.site.clone(),
                        });
                    }
                }
                None => {}
            }
        }
    }
    None
}

// ── Collection ──────────────────────────────────────────────────────────────

fn collect(program: &Lang) -> Vec<Def> {
    let mut defs = Vec::new();
    let mut top = Def::new(TOP_LEVEL, program.get_help_data());
    top.is_root = true;
    collect_statements(statements_of(program), &mut defs, &mut top);
    defs.push(top);
    defs
}

fn statements_of(program: &Lang) -> &[Lang] {
    match program {
        Lang::Lines { value, .. } => value,
        other => std::slice::from_ref(other),
    }
}

fn collect_statements(stmts: &[Lang], defs: &mut Vec<Def>, top: &mut Def) {
    let mut pending: Vec<(TargetAttr, HelpData)> = Vec::new();
    for stmt in stmts {
        match stmt {
            Lang::Attribute { attr, help_data } => pending.push((attr.clone(), help_data.clone())),
            Lang::Comment { .. } => {}
            Lang::Let {
                variable,
                r#type,
                expression,
                is_public,
                is_export,
                help_data,
                ..
            } => {
                let name = match variable.as_ref() {
                    Lang::Variable { name, .. } => name.clone(),
                    _ => {
                        pending.clear();
                        scan_into(top, &serde_json::to_value(stmt).unwrap_or(Value::Null));
                        continue;
                    }
                };
                let mut d = Def::new(&name, help_data.clone());
                d.is_root = *is_public || *is_export || name == "main";
                apply_attributes(&mut d, std::mem::take(&mut pending));
                scan_type(&mut d, r#type);
                scan_into(&mut d, &serde_json::to_value(expression.as_ref()).unwrap_or(Value::Null));
                defs.push(d);
            }
            Lang::Signature {
                identifier,
                target_type,
                externs,
                help_data,
            } => {
                let mut d = Def::new(&identifier.get_name(), help_data.clone());
                d.has_body = false;
                d.externs = externs.keys().copied().collect();
                // A signature with no external of its own is backed by `std.R`,
                // like every standard-library entry.
                if d.externs.is_empty() {
                    d.externs.insert(Target::R);
                }
                apply_attributes(&mut d, std::mem::take(&mut pending));
                scan_type(&mut d, target_type);
                defs.push(d);
            }
            Lang::Module { body, .. } => {
                pending.clear();
                collect_statements(body, defs, top);
            }
            Lang::Alias { .. } | Lang::TypeConstructor { .. } | Lang::ModuleImport { .. } => pending.clear(),
            other => {
                pending.clear();
                scan_into(top, &serde_json::to_value(other).unwrap_or(Value::Null));
            }
        }
    }
}

fn apply_attributes(d: &mut Def, attrs: Vec<(TargetAttr, HelpData)>) {
    for (attr, site) in attrs {
        match attr {
            TargetAttr::Only(set) => d.declared = Some((set, site)),
            TargetAttr::Extern(map) => d.externs.extend(map.keys().copied()),
        }
    }
}

fn scan_type(d: &mut Def, typ: &Type) {
    if let Ok(v) = serde_json::to_value(typ) {
        scan_into(d, &v);
    }
}

/// Walk one serialised subtree, recording the non-portable constructs and the
/// names it refers to.
fn scan_into(d: &mut Def, root: &Value) {
    let mut locals: HashSet<String> = HashSet::new();
    walk(root, None, d, &mut locals);
    d.refs.retain(|r| !locals.contains(&r.name));
}

fn help_of(v: &Value) -> Option<HelpData> {
    match v {
        Value::Object(m) => m
            .get("help_data")
            .and_then(|h| serde_json::from_value(h.clone()).ok()),
        Value::Array(items) => items
            .iter()
            .rev()
            .find(|i| i.get("offset").is_some() && i.get("file_name").is_some())
            .and_then(|h| serde_json::from_value(h.clone()).ok()),
        _ => None,
    }
}

/// `{"Variant": payload}` — how serde writes a non-unit enum variant.
fn variant(v: &Value) -> Option<(&str, &Value)> {
    let m = v.as_object()?;
    if m.len() != 1 {
        return None;
    }
    m.iter().next().map(|(k, v)| (k.as_str(), v))
}

fn leak_of(key: &str, payload: &Value) -> Option<(String, BTreeSet<Target>)> {
    let r_only = |what: &str| Some((what.to_string(), [Target::R].into_iter().collect()));
    match key {
        "RBlock" => r_only("an `R { ... }` block"),
        "RFunction" => r_only("an untyped R function (`function(...)`)"),
        "ExternBlock" => r_only("a raw-R `extern` body"),
        "Library" => r_only("`library(...)`"),
        "ImportFrom" => r_only("`@importFrom`"),
        "Use" => r_only("`use(...)`"),
        "DataFrame" => r_only("a data frame"),
        "RClass" => r_only("an R class (`Class(...)`)"),
        "UnknownFunction" => r_only("an untyped R function type"),
        "Vec" if payload.get(0).and_then(Value::as_str) == Some("DataFrame") => r_only("a `dataframe` type"),
        "Alias" if payload.get(0).and_then(Value::as_str) == Some("Foreign") => r_only("`Foreign<T>`"),
        _ => None,
    }
}

fn walk(v: &Value, near: Option<&HelpData>, d: &mut Def, locals: &mut HashSet<String>) {
    match v {
        Value::Array(items) => items.iter().for_each(|i| walk(i, near, d, locals)),
        Value::Object(m) => {
            if let Some((key, payload)) = variant(v) {
                let here = help_of(payload);
                let site = here.clone().or_else(|| near.cloned()).unwrap_or_default();
                if let Some((what, avail)) = leak_of(key, payload) {
                    d.leaks.push(Leak { what, avail, site: site.clone() });
                }
                match key {
                    "Variable" => {
                        if let Some(name) = payload.get("name").and_then(Value::as_str) {
                            d.refs.push(Reference {
                                name: name.to_string(),
                                site: site.clone(),
                                is_call: false,
                            });
                        }
                    }
                    "FunctionApp" | "VecFunctionApp" => {
                        if let Some(callee) = payload.get("identifier").and_then(variant) {
                            if let ("Variable", c) = callee {
                                if let Some(name) = c.get("name").and_then(Value::as_str) {
                                    d.refs.push(Reference {
                                        name: name.to_string(),
                                        site: help_of(c).unwrap_or_else(|| site.clone()),
                                        is_call: true,
                                    });
                                }
                            }
                        }
                    }
                    "Let" => bound_names(payload.get("variable"), locals),
                    "ForLoop" => {
                        if let Some(n) = payload.get("identifier").and_then(|i| i.get("name")).and_then(Value::as_str) {
                            locals.insert(n.to_string());
                        }
                    }
                    "Function" | "Lambda" => bound_names(payload.get("parameters"), locals),
                    _ => {}
                }
                walk(payload, Some(&site), d, locals);
            } else {
                let here = help_of(v);
                let near = here.as_ref().or(near);
                m.values().for_each(|c| walk(c, near, d, locals));
            }
        }
        _ => {}
    }
}

/// Every name a binder introduces: variables, and the `Val("x")` of a
/// parameter's name.
fn bound_names(v: Option<&Value>, locals: &mut HashSet<String>) {
    fn go(v: &Value, locals: &mut HashSet<String>) {
        match v {
            Value::Object(m) => {
                if let Some(n) = m.get("Val").and_then(Value::as_str) {
                    locals.insert(n.to_string());
                }
                if let Some(n) = m.get("name").and_then(Value::as_str) {
                    locals.insert(n.to_string());
                }
                m.values().for_each(|c| go(c, locals));
            }
            Value::Array(items) => items.iter().for_each(|c| go(c, locals)),
            _ => {}
        }
    }
    if let Some(v) = v {
        go(v, locals);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::processes::parsing::parse_from_string;

    fn stdlib() -> StdlibAvailability {
        StdlibAvailability {
            known: ["print", "c"].iter().map(|s| s.to_string()).collect(),
            targets: HashMap::new(),
        }
    }

    fn codes(src: &str, targets: &[Target]) -> Vec<(String, String)> {
        let ast = parse_from_string(src, "test.ty");
        check(&ast, &stdlib(), targets)
            .into_iter()
            .map(|e| match e {
                TypRError::Type(t) => (t.code().to_string(), t.simple_message()),
                TypRError::Syntax(s) => (s.code().to_string(), s.simple_message()),
            })
            .collect()
    }

    #[test]
    fn portable_code_builds_for_every_target() {
        let src = "let area <- fn(x: num): num { x * x };\narea(2)";
        assert!(codes(src, &[Target::R, Target::Rust]).is_empty());
    }

    #[test]
    fn r_block_is_unavailable_on_rust_but_fine_on_r() {
        let src = "let load <- fn(p: char): Any { R { readr::read_csv(p) } };\nload(\"a\")";
        assert!(codes(src, &[Target::R]).is_empty());
        let errs = codes(src, &[Target::Rust]);
        assert_eq!(errs.len(), 1, "{errs:?}");
        assert_eq!(errs[0].0, "T052");
        assert!(errs[0].1.contains("R { ... }"), "{errs:?}");
    }

    #[test]
    fn chain_goes_through_intermediate_definitions() {
        let src = "let a <- fn(): Any { R { 1 } };\nlet b <- fn(): Any { a() };\nb()";
        let errs = codes(src, &[Target::Rust]);
        assert_eq!(errs.len(), 1, "{errs:?}");
        assert!(errs[0].1.contains("<top level>"), "{errs:?}");
    }

    #[test]
    fn unreachable_helper_is_never_reported() {
        let src = "let helper <- fn(): Any { R { 1 } };\nlet f <- fn(x: int): int { x };\nf(1)";
        assert!(codes(src, &[Target::Rust]).is_empty());
    }

    #[test]
    fn extern_adds_a_target() {
        let src = "@extern(r = \"stats::median\", rust = \"typr_std::median\") med: (num) -> num;\nmed(1)";
        assert!(codes(src, &[Target::R, Target::Rust]).is_empty());
        let r_only = "@extern(r = \"stats::median\") med: (num) -> num;\nmed(1)";
        assert_eq!(codes(r_only, &[Target::Rust]).len(), 1);
    }

    #[test]
    fn extern_with_body_fallback_is_portable() {
        let src = "@extern(r = \"base::rev\")\nlet rev2 <- fn(x: int): int { x };\nrev2(1)";
        assert!(codes(src, &[Target::R, Target::Rust]).is_empty());
    }

    #[test]
    fn target_declaration_is_checked_on_the_definition() {
        let src = "@target(r, rust)\nlet f <- fn(): Any { R { 1 } };";
        let errs = codes(src, &[Target::R]);
        assert_eq!(errs.len(), 1, "{errs:?}");
        assert_eq!(errs[0].0, "T053");
    }

    #[test]
    fn target_declaration_restricts_availability() {
        let src = "@target(r)\nlet f <- fn(x: int): int { x };\nf(1)";
        assert!(codes(src, &[Target::R]).is_empty());
        let errs = codes(src, &[Target::Rust]);
        assert_eq!(errs.len(), 1, "{errs:?}");
        assert_eq!(errs[0].0, "T052");
    }

    #[test]
    fn mutual_recursion_stays_portable() {
        let src = "let even <- fn(n: int): bool { if (n == 0) { true } else { odd(n - 1) } };\nlet odd <- fn(n: int): bool { if (n == 0) { false } else { even(n - 1) } };\neven(4)";
        assert!(codes(src, &[Target::Rust]).is_empty());
    }

    #[test]
    fn called_stdlib_function_is_r_only_until_it_declares_targets() {
        let src = "print(\"hi\")";
        assert_eq!(codes(src, &[Target::Rust]).len(), 1);
        let ast = parse_from_string(src, "test.ty");
        let ported = stdlib().with_targets(HashMap::from([(
            "print".to_string(),
            [Target::R, Target::Rust].into_iter().collect(),
        )]));
        assert!(check(&ast, &ported, &[Target::Rust]).is_empty());
    }

    #[test]
    fn local_binding_shadows_a_definition() {
        let src = "let g <- fn(): Any { R { 1 } };\nlet f <- fn(g: int): int { g };\nf(1)";
        assert!(codes(src, &[Target::Rust]).is_empty());
    }
}
