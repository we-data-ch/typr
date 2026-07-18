//! Type-directed random program generator (Phase B / Stage 1 of
//! `soundness_transpilation.md`). Builds well-typed-by-construction TypR source
//! text by recursively picking a goal `Type` and emitting a production that
//! yields a value of that type, tracking a `GenEnv` of bindings in scope.
//!
//! v1 scope: to keep every production terminating without a full subtype
//! check, record fields / vector elements / union payloads / function
//! params+return are restricted to primitive types (`int`/`num`/`bool`/
//! `char`). Excluded entirely (per the doc): kinded generics, interfaces,
//! partial application, `@testable`, lambdas, pipes, modules.

#![allow(dead_code)]

use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::type_printer;
use crate::components::r#type::Type;
use crate::utils::builder;
use rand::Rng;
use rand::RngExt;
use std::collections::HashMap;

/// A union alias's variants: `(variant_name, payload_type)`, `None` payload
/// meaning a bare tag (e.g. `.TaVb`).
type UnionVariants = Vec<(String, Option<Type>)>;

#[derive(Clone, Debug, Default)]
pub struct GenEnv {
    vars: Vec<(String, Type)>,
    fns: Vec<(String, Vec<Type>, Type)>,
    aliases: Vec<(String, Type)>,
    /// Union aliases by name. Tracked separately from `aliases` (which
    /// stores the raw `Type::Operator(Union, ...)` tree) so `Tag`/`Match`
    /// productions don't need to walk that tree.
    unions: Vec<(String, UnionVariants)>,
    fresh_counter: u32,
}

impl GenEnv {
    pub fn new() -> Self {
        Self::default()
    }

    fn fresh_name(&mut self, prefix: &str) -> String {
        let n = self.fresh_counter;
        self.fresh_counter += 1;
        format!("{prefix}{n}")
    }

    /// PascalCase alias names must be letters-only after the leading capital
    /// (the parser's `pascal_case` combinator is `one_of(A-Z), opt(alpha1)` —
    /// `alpha1` doesn't match digits, so `T0` would parse as just `T` with a
    /// dangling `0`). Base-26 letter suffix instead: Ta, Tb, ..., Tz, Taa, ...
    fn fresh_alias_name(&mut self) -> String {
        let n = self.fresh_counter;
        self.fresh_counter += 1;
        format!("T{}", letters_suffix(n))
    }
}

fn letters_suffix(mut n: u32) -> String {
    let mut s = Vec::new();
    loop {
        let rem = (n % 26) as u8;
        s.push((b'a' + rem) as char);
        n /= 26;
        if n == 0 {
            break;
        }
        n -= 1;
    }
    s.iter().rev().collect()
}

#[derive(Default)]
pub struct Coverage {
    hits: HashMap<&'static str, u32>,
}

impl Coverage {
    pub fn new() -> Self {
        Self::default()
    }

    fn record(&mut self, tag: &'static str) {
        *self.hits.entry(tag).or_insert(0) += 1;
    }

    pub fn summary(&self) -> String {
        let mut entries: Vec<_> = self.hits.iter().collect();
        entries.sort_by_key(|(k, _)| **k);
        let mut out = String::from("production coverage:\n");
        for (k, v) in entries {
            out.push_str(&format!("  {k}: {v}\n"));
        }
        out
    }
}

pub struct GenProgram {
    pub declarations: Vec<String>,
    pub terminal: String,
}

impl GenProgram {
    pub fn to_source(&self) -> String {
        let mut s = self.declarations.join("\n");
        if !s.is_empty() {
            s.push('\n');
        }
        s.push_str(&self.terminal);
        s.push_str(";\n");
        s
    }
}

/// Structural equality over the small subset of `Type` shapes this generator
/// ever produces (ignores literal-value refinement like `Tint::Val(5)` vs
/// `Tint::Unknown` — both count as "int" for matching a var/goal).
fn base_eq(a: &Type, b: &Type) -> bool {
    use Type::*;
    match (a, b) {
        (Integer(..), Integer(..)) => true,
        (Number(..), Number(..)) => true,
        (Boolean(..), Boolean(..)) => true,
        (Char(..), Char(..)) => true,
        (Alias(n1, ..), Alias(n2, ..)) => n1 == n2,
        (Vec(v1, _, i1, _), Vec(v2, _, i2, _)) => v1 == v2 && base_eq(i1, i2),
        (Record(f1, _), Record(f2, _)) => {
            f1.len() == f2.len()
                && f1.iter().all(|a1| {
                    f2.iter().any(|a2| {
                        a1.get_argument_str() == a2.get_argument_str()
                            && base_eq(&a1.get_type(), &a2.get_type())
                    })
                })
        }
        _ => false,
    }
}

fn is_primitive_goal(t: &Type) -> bool {
    matches!(
        t,
        Type::Integer(..) | Type::Number(..) | Type::Boolean(..) | Type::Char(..)
    )
}

fn primitive_types() -> Vec<Type> {
    vec![
        builder::integer_type_default(),
        builder::number_type(),
        builder::boolean_type(),
        builder::character_type_default(),
    ]
}

/// Field/element types safe for record and constructor literals. Excludes
/// `char`: `Lang::List`/`ConstructorCall` field typing keeps each field's
/// raw inferred type (no `.generalize()`, unlike array-literal typing) —
/// combined with `char`'s exact-value `PartialEq`/subtyping (vs. `int`'s
/// deliberately loose one), a record/constructor field built from a char
/// literal never satisfies a plain `char`-typed annotation, even with no
/// `if` involved (cases/0017-char-if-widening). Known, catalogued
/// type-checker gap, not a generator artifact — narrowed here rather than
/// worked around silently.
fn record_field_types() -> Vec<Type> {
    vec![
        builder::integer_type_default(),
        builder::number_type(),
        builder::boolean_type(),
    ]
}

fn gen_literal(goal: &Type, rng: &mut impl Rng) -> Option<String> {
    match goal {
        Type::Integer(..) => Some(rng.random_range(-100..100).to_string()),
        Type::Number(..) => {
            let n = rng.random_range(-1000..1000) as f64 / 100.0;
            Some(format!("{n:.2}"))
        }
        Type::Boolean(..) => Some(
            if rng.random_bool(0.5) {
                "true"
            } else {
                "false"
            }
            .to_string(),
        ),
        Type::Char(..) => {
            const WORDS: [&str; 5] = ["a", "b", "hello", "x", "typr"];
            let w = WORDS[rng.random_range(0..WORDS.len())];
            Some(format!("\"{w}\""))
        }
        _ => None,
    }
}

fn gen_var(env: &GenEnv, goal: &Type) -> Option<String> {
    env.vars
        .iter()
        .find(|(_, t)| base_eq(t, goal))
        .map(|(n, _)| n.clone())
}

/// Length encoded in a goal `Type::Vec`'s index type, as produced by
/// `pick_goal_type`'s ad-hoc array goals (always a concrete `Tint::Val`,
/// since v1 never hands a `Vec` goal to anything else — function
/// params/returns are primitive-only). Falls back to 2 otherwise.
fn vec_len(goal: &Type) -> usize {
    if let Type::Vec(_, idx, _, _) = goal {
        if let Type::Integer(Tint::Val(n), _) = idx.as_ref() {
            return (*n).max(1) as usize;
        }
    }
    2
}

fn record_fields_sorted(
    fields: &std::collections::HashSet<crate::components::r#type::argument_type::ArgumentType>,
) -> Vec<(String, Type)> {
    let mut v: Vec<(String, Type)> = fields
        .iter()
        .map(|a| (a.get_argument_str(), a.get_type()))
        .collect();
    v.sort_by(|a, b| a.0.cmp(&b.0));
    v
}

fn weighted_choice<T: Clone>(rng: &mut impl Rng, items: &[(T, u32)]) -> T {
    let total: u32 = items.iter().map(|(_, w)| *w).sum();
    let mut pick = rng.random_range(0..total);
    for (item, w) in items {
        if pick < *w {
            return item.clone();
        }
        pick -= *w;
    }
    items.last().expect("non-empty production list").0.clone()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Production {
    Literal,
    Var,
    If,
    Record,
    Ctor,
    Vector,
    FunctionApp,
    Tag,
    Match,
    DotAccess,
}

impl Production {
    fn tag(self) -> &'static str {
        match self {
            Production::Literal => "Literal",
            Production::Var => "Var",
            Production::If => "If",
            Production::Record => "Record",
            Production::Ctor => "Ctor",
            Production::Vector => "Vector",
            Production::FunctionApp => "FunctionApp",
            Production::Tag => "Tag",
            Production::Match => "Match",
            Production::DotAccess => "DotAccess",
        }
    }
}

const ALL_PRODUCTIONS: &[Production] = &[
    Production::Literal,
    Production::Var,
    Production::If,
    Production::Record,
    Production::Ctor,
    Production::Vector,
    Production::FunctionApp,
    Production::Tag,
    Production::Match,
    Production::DotAccess,
];

fn union_variants<'a>(
    env: &'a GenEnv,
    alias_name: &str,
) -> Option<&'a Vec<(String, Option<Type>)>> {
    env.unions
        .iter()
        .find(|(n, _)| n == alias_name)
        .map(|(_, v)| v)
}

/// `(var_name, field_name)` pairs for every var in scope (bare record or
/// alias-to-record typed) that has a field matching `goal`.
fn dot_access_candidates(env: &GenEnv, goal: &Type) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for (vname, vty) in &env.vars {
        let fields = match vty {
            Type::Record(fields, _) => Some(record_fields_sorted(fields)),
            Type::Alias(name, ..) => alias_record_fields(env, name).map(record_fields_sorted),
            _ => None,
        };
        if let Some(fields) = fields {
            for (fname, fty) in fields {
                if base_eq(&fty, goal) {
                    out.push((vname.clone(), fname));
                }
            }
        }
    }
    out
}

/// A var in scope whose type is a union alias (for `Match`'s scrutinee —
/// the parser requires the match target to be a bare variable or a `{...}`
/// scope, so `Match` only fires once *some* union-typed var already exists,
/// rather than synthesizing a fresh scrutinee inline).
fn union_var_in_scope(env: &GenEnv) -> Option<(String, String)> {
    env.vars.iter().find_map(|(vname, vty)| {
        if let Type::Alias(aname, ..) = vty {
            if union_variants(env, aname).is_some() {
                return Some((vname.clone(), aname.clone()));
            }
        }
        None
    })
}

fn alias_record_fields<'a>(
    env: &'a GenEnv,
    alias_name: &str,
) -> Option<&'a std::collections::HashSet<crate::components::r#type::argument_type::ArgumentType>> {
    env.aliases.iter().find_map(|(n, t)| {
        if n == alias_name {
            if let Type::Record(fields, _) = t {
                return Some(fields);
            }
        }
        None
    })
}

fn applicable(p: Production, env: &GenEnv, goal: &Type, depth: u32) -> bool {
    match p {
        Production::Literal => is_primitive_goal(goal),
        Production::Var => env.vars.iter().any(|(_, t)| base_eq(t, goal)),
        // Excludes Char: an `if`/`else` with two different char-literal
        // branches never widens to a plain `char` annotation the way
        // int/bool/num do — known, catalogued type-checker bug, not a
        // generator artifact (cases/0017-char-if-widening). Narrowed here
        // rather than worked around, per the triage rule: fix the checker
        // later, keep the generator honest about what's actually sound today.
        Production::If => depth >= 2 && !matches!(goal, Type::Char(..)),
        Production::Record => matches!(goal, Type::Record(..)) && depth >= 1,
        Production::Ctor => {
            depth >= 1
                && matches!(goal, Type::Alias(name, ..) if alias_record_fields(env, name).is_some())
        }
        Production::Vector => matches!(goal, Type::Vec(..)) && depth >= 1,
        Production::FunctionApp => {
            depth >= 1 && env.fns.iter().any(|(_, _, ret)| base_eq(ret, goal))
        }
        Production::Tag => {
            depth >= 1
                && matches!(goal, Type::Alias(name, ..) if union_variants(env, name).is_some())
        }
        Production::Match => depth >= 2 && union_var_in_scope(env).is_some(),
        Production::DotAccess => !dot_access_candidates(env, goal).is_empty(),
    }
}

fn weight(p: Production) -> u32 {
    match p {
        Production::Literal => 30,
        Production::Var => 20,
        Production::If => 8,
        Production::Record => 10,
        Production::Ctor => 10,
        Production::Vector => 10,
        Production::FunctionApp => 10,
        Production::Tag => 10,
        Production::Match => 6,
        Production::DotAccess => 12,
    }
}

/// Produce a TypR source expression of type `goal`. `depth` is a remaining
/// recursion budget: productions that recurse at the *same* goal type (`if`,
/// later `match`) require `depth >= 2` so their children (`depth - 1`) always
/// have budget left to resolve; structural productions (`Record`, `Ctor`,
/// `Vector`) need only `depth >= 1` since their children are always
/// primitive-typed in v1 (record fields / array elements / union payloads
/// are restricted to primitives — see module docs), so they always bottom
/// out at `Literal` regardless of remaining budget.
pub fn gen_expr(
    env: &GenEnv,
    goal: &Type,
    depth: u32,
    rng: &mut impl Rng,
    cov: &mut Coverage,
) -> String {
    let candidates: Vec<(Production, u32)> = ALL_PRODUCTIONS
        .iter()
        .copied()
        .filter(|p| applicable(*p, env, goal, depth))
        .map(|p| (p, weight(p)))
        .collect();
    let candidates = if candidates.is_empty() {
        // Safety net: pick_goal_type only ever hands out goals that at least
        // support Literal, so this should be unreachable in practice.
        vec![(Production::Literal, 1)]
    } else {
        candidates
    };
    let chosen = weighted_choice(rng, &candidates);
    cov.record(chosen.tag());
    match chosen {
        Production::Literal => gen_literal(goal, rng).unwrap_or_else(|| "0".to_string()),
        Production::Var => gen_var(env, goal).unwrap_or_else(|| "0".to_string()),
        Production::If => {
            let cond = gen_expr(env, &builder::boolean_type(), depth - 1, rng, cov);
            let a = gen_expr(env, goal, depth - 1, rng, cov);
            let b = gen_expr(env, goal, depth - 1, rng, cov);
            format!("if ({cond}) {{ {a} }} else {{ {b} }}")
        }
        Production::Record => {
            let fields = match goal {
                Type::Record(fields, _) => record_fields_sorted(fields),
                _ => unreachable!("Record production only applicable to a Record goal"),
            };
            let parts: Vec<String> = fields
                .iter()
                .map(|(fname, fty)| {
                    let val = gen_expr(env, fty, depth - 1, rng, cov);
                    format!("{fname} = {val}")
                })
                .collect();
            format!(":{{ {} }}", parts.join(", "))
        }
        Production::Ctor => {
            let alias_name = match goal {
                Type::Alias(name, ..) => name.clone(),
                _ => unreachable!("Ctor production only applicable to an Alias goal"),
            };
            let fields = record_fields_sorted(
                alias_record_fields(env, &alias_name).expect("checked applicable"),
            );
            let parts: Vec<String> = fields
                .iter()
                .map(|(fname, fty)| {
                    let val = gen_expr(env, fty, depth - 1, rng, cov);
                    format!("{fname} = {val}")
                })
                .collect();
            format!("{alias_name}:{{ {} }}", parts.join(", "))
        }
        Production::Vector => {
            let inner = match goal {
                Type::Vec(_, _, inner, _) => (**inner).clone(),
                _ => unreachable!("Vector production only applicable to a Vec goal"),
            };
            let len = vec_len(goal);
            let elems: Vec<String> = (0..len)
                .map(|_| gen_expr(env, &inner, depth - 1, rng, cov))
                .collect();
            format!("[{}]", elems.join(", "))
        }
        Production::FunctionApp => {
            let (fname, params) = env
                .fns
                .iter()
                .find(|(_, _, ret)| base_eq(ret, goal))
                .map(|(n, p, _)| (n.clone(), p.clone()))
                .expect("checked applicable");
            let args: Vec<String> = params
                .iter()
                .map(|pty| gen_expr(env, pty, depth - 1, rng, cov))
                .collect();
            format!("{fname}({})", args.join(", "))
        }
        Production::Tag => {
            let alias_name = match goal {
                Type::Alias(name, ..) => name.clone(),
                _ => unreachable!("Tag production only applicable to an Alias goal"),
            };
            let variants = union_variants(env, &alias_name).expect("checked applicable");
            let (vname, payload) = &variants[rng.random_range(0..variants.len())];
            match payload {
                Some(pty) => {
                    let val = gen_expr(env, pty, depth - 1, rng, cov);
                    format!(".{vname}({val})")
                }
                None => format!(".{vname}"),
            }
        }
        Production::Match => {
            let (var_name, alias_name) = union_var_in_scope(env).expect("checked applicable");
            let variants = union_variants(env, &alias_name)
                .expect("checked applicable")
                .clone();
            let arms: Vec<String> = variants
                .iter()
                .map(|(vname, payload)| {
                    let body = gen_expr(env, goal, depth - 1, rng, cov);
                    match payload {
                        Some(_) => format!(".{vname}(v) => {body}"),
                        None => format!(".{vname} => {body}"),
                    }
                })
                .collect();
            format!("match {var_name} {{ {} }}", arms.join(", "))
        }
        Production::DotAccess => {
            let candidates = dot_access_candidates(env, goal);
            let (vname, fname) = &candidates[rng.random_range(0..candidates.len())];
            format!("{vname}.{fname}")
        }
    }
}

fn random_primitive(rng: &mut impl Rng) -> Type {
    let candidates = primitive_types();
    candidates[rng.random_range(0..candidates.len())].clone()
}

fn random_record_field_type(rng: &mut impl Rng) -> Type {
    let candidates = record_field_types();
    candidates[rng.random_range(0..candidates.len())].clone()
}

/// Goal types a `let`/terminal expression can target: always the primitives,
/// plus (once declared) each record/union alias by name, plus occasional
/// ad-hoc anonymous-record and array goals (not tied to any declaration) so
/// the `Record`/`Vector` productions get exercised even in programs with no
/// alias declarations.
fn pick_goal_type(env: &GenEnv, rng: &mut impl Rng) -> Type {
    let mut candidates = primitive_types();
    for (name, _) in &env.aliases {
        candidates.push(Type::Alias(
            name.clone(),
            vec![],
            false,
            HelpData::default(),
        ));
    }
    if rng.random_bool(0.25) {
        let n_fields = rng.random_range(1..3u32);
        let fields: Vec<(String, Type)> = (0..n_fields)
            .map(|i| (format!("f{i}"), random_record_field_type(rng)))
            .collect();
        candidates.push(builder::record_type(&fields));
    }
    if rng.random_bool(0.25) {
        let len = rng.random_range(2..4);
        candidates.push(builder::array_type2(len, random_primitive(rng)));
    }
    candidates[rng.random_range(0..candidates.len())].clone()
}

/// `type <Name> <- list { f0: T0, ... };` — the underlying `Type::Record` is
/// what `Production::Ctor` looks up via `env.aliases` to build
/// `<Name>:{ f0 = ..., ... }`.
fn gen_record_alias_decl(env: &mut GenEnv, rng: &mut impl Rng) -> String {
    let name = env.fresh_alias_name();
    let n_fields = rng.random_range(1..4u32);
    let fields: Vec<(String, Type)> = (0..n_fields)
        .map(|i| (format!("f{i}"), random_record_field_type(rng)))
        .collect();
    let field_strs: Vec<String> = fields
        .iter()
        .map(|(n, t)| format!("{n}: {}", type_printer::format(t)))
        .collect();
    let decl = format!("type {name} <- list {{ {} }};", field_strs.join(", "));
    env.aliases.push((name, builder::record_type(&fields)));
    decl
}

/// `type <Name> <- .<Name>Va(T0) | .<Name>Vb | ...;` — variant names embed
/// the alias name (letters-only suffix, see `fresh_alias_name`) so two
/// unrelated unions in the same program never collide on a bare-tag lookup.
fn gen_union_alias_decl(env: &mut GenEnv, rng: &mut impl Rng) -> String {
    let name = env.fresh_alias_name();
    let n_variants = rng.random_range(2..4u32);
    let mut variants: Vec<(String, Option<Type>)> = Vec::new();
    let mut tag_types: Vec<Type> = Vec::new();
    for i in 0..n_variants {
        let vname = format!("{name}V{}", letters_suffix(i));
        let payload = if rng.random_bool(0.6) {
            Some(random_record_field_type(rng))
        } else {
            None
        };
        let payload_ty = payload.clone().unwrap_or_else(builder::empty_type);
        tag_types.push(Type::Tag(
            vname.clone(),
            Box::new(payload_ty),
            HelpData::default(),
        ));
        variants.push((vname, payload));
    }
    let variant_strs: Vec<String> = variants
        .iter()
        .map(|(vn, p)| match p {
            Some(t) => format!(".{vn}({})", type_printer::format(t)),
            None => format!(".{vn}"),
        })
        .collect();
    let decl = format!("type {name} <- {};", variant_strs.join(" | "));
    env.aliases
        .push((name.clone(), builder::union_type(&tag_types)));
    env.unions.push((name, variants));
    decl
}

/// `let <name> <- fn(p0: T0, ...): R { <body> };` — params/return are
/// primitive-only in v1 (see module docs), so the body always resolves
/// regardless of `max_depth`. Function values are never a recursive `goal`
/// elsewhere in v1 (nothing asks `gen_expr` for a `Type::Function`); they
/// only ever arise here, as a top-level declaration.
fn gen_fn_decl(env: &mut GenEnv, rng: &mut impl Rng, max_depth: u32, cov: &mut Coverage) -> String {
    let fn_name = env.fresh_name("f");
    let n_params = rng.random_range(0..3u32);
    let mut body_env = env.clone();
    let mut param_strs = Vec::new();
    let mut param_types = Vec::new();
    for _ in 0..n_params {
        let pty = random_primitive(rng);
        let pname = body_env.fresh_name("p");
        param_strs.push(format!("{pname}: {}", type_printer::format(&pty)));
        body_env.vars.push((pname, pty.clone()));
        param_types.push(pty);
    }
    let ret = random_primitive(rng);
    let body = gen_expr(&body_env, &ret, max_depth, rng, cov);
    let decl = format!(
        "let {fn_name} <- fn({}): {} {{ {body} }};",
        param_strs.join(", "),
        type_printer::format(&ret)
    );
    // fresh_counter advanced by body_env's own param names must not collide
    // with names env hands out later; carry it forward.
    env.fresh_counter = body_env.fresh_counter;
    env.fns.push((fn_name, param_types, ret));
    decl
}

pub fn gen_program(rng: &mut impl Rng, max_depth: u32, cov: &mut Coverage) -> GenProgram {
    let mut env = GenEnv::new();
    let mut declarations = Vec::new();

    let n_aliases = rng.random_range(0..3u32);
    for _ in 0..n_aliases {
        if rng.random_bool(0.5) {
            declarations.push(gen_record_alias_decl(&mut env, rng));
        } else {
            declarations.push(gen_union_alias_decl(&mut env, rng));
        }
    }

    let n_fns = rng.random_range(0..3u32);
    for _ in 0..n_fns {
        declarations.push(gen_fn_decl(&mut env, rng, max_depth, cov));
    }

    let n_vals = rng.random_range(0..3u32);
    for _ in 0..n_vals {
        let goal = pick_goal_type(&env, rng);
        let name = env.fresh_name("x");
        let expr = gen_expr(&env, &goal, max_depth, rng, cov);
        let ann = type_printer::format(&goal);
        declarations.push(format!("let {name}: {ann} <- {expr};"));
        env.vars.push((name, goal));
    }

    let goal = pick_goal_type(&env, rng);
    let expr = gen_expr(&env, &goal, max_depth, rng, cov);
    let terminal = format!("print({expr})");

    GenProgram {
        declarations,
        terminal,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    #[test]
    fn generates_deterministic_source_for_a_seed() {
        let mut rng1 = StdRng::seed_from_u64(42);
        let mut cov1 = Coverage::new();
        let p1 = gen_program(&mut rng1, 4, &mut cov1);

        let mut rng2 = StdRng::seed_from_u64(42);
        let mut cov2 = Coverage::new();
        let p2 = gen_program(&mut rng2, 4, &mut cov2);

        assert_eq!(p1.to_source(), p2.to_source());
    }

    #[test]
    fn base_eq_matches_same_shape_primitives() {
        assert!(base_eq(
            &builder::integer_type(1),
            &builder::integer_type_default()
        ));
        assert!(!base_eq(
            &builder::integer_type(1),
            &builder::boolean_type()
        ));
    }
}
