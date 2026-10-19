//! Rust backend (rfcs/0000-compilation-targets.md, Step 3).
//!
//! Reads the typed [`Program`] and writes one `main.rs`. The output is a build artifact: it is
//! optimised for `rustc`, not for a reader (§9), so names are mangled, everything is `clone()`d
//! and helper types are generated freely.
//!
//! Covered: scalars, `T | na` (as `Option<T>`), named functions, `if`/`match`, loops, records
//! (one generated `struct` per shape), tags (one `enum` per declared union), modules, checked
//! integer arithmetic, vectors (`typr_rt::Vector<T>`: literals, indexing, `for`, element-wise
//! operators with R's recycling rule), `State<T>`, and (Step 5) generics: functions, interface and
//! `Any` parameters are monomorphised per call site, generic types become one struct or enum per
//! argument list, overloads are resolved statically, and an `Any` that reaches data is a
//! `typr_rt::Dyn`. Everything else is reported as a [`RustError`].
//!
//! Known limits of this step, all caught by `rustc` rather than miscompiled:
//! * a top-level `let` that is not a literal lives in `main()`, so functions cannot see it;
//! * a nested `let f <- fn` is a Rust `fn` item and cannot capture its environment;
//! * a lambda is a `move` closure behind an `Rc`.

mod platform;

pub use platform::{boundary_errors, exports, Export, Options, Output, Platform};

use super::ir::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

/// Intrinsics that are R builtins, which the standard library cannot declare with a signature
/// (and so cannot tag with `#! targets:`).
pub const UNDECLARABLE_INTRINSICS: &[&str] = &["length"];

/// Calls the backend implements itself. Each one must be tagged `#! targets: r, rust` in the
/// standard library's `.ty` sources (the availability pass reads it from there), except the
/// undeclarable ones above; a test in `typr-cli` keeps the two lists equal.
pub const INTRINSICS: &[&str] = &[
    "print", "length", "sum", "seq", "state", "get", "set", "update", "version", "map", "filter", "reduce", "fold",
    "extend", "set_at", "as_vec", "startsWith", "endsWith", "stop", "join", "contains",
];

const RUNTIME: &str = include_str!("runtime.rs");

#[derive(Debug, Clone)]
pub struct RustError {
    pub message: String,
    pub span: Option<Span>,
}

impl std::fmt::Display for RustError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.span {
            Some(s) => write!(f, "{} ({}:{})", self.message, s.file, s.start),
            None => f.write_str(&self.message),
        }
    }
}

/// Emits the Rust source of `program` as a binary.
pub fn emit(program: &Program) -> Result<String, Vec<RustError>> {
    emit_with(program, &Options::default()).map(|o| o.source)
}

/// Emits `program` for a platform (Step 6): the crate root, and for `extendr` the R wrappers.
pub fn emit_with(program: &Program, options: &Options) -> Result<Output, Vec<RustError>> {
    let mut em = Emitter { platform: options.platform, module_name: options.module_name.clone(), ..Default::default() };
    em.ensure_root_scope();
    em.collect(&program.body, None);
    em.assigned_names(&program.body);
    let mut items = String::new();
    let mut main_body = String::new();
    for e in &program.body {
        em.top_item(e, &mut items, &mut main_body);
    }
    em.drain_instances(&mut items);
    let mut out = em.finish(&items, &main_body);
    let r_wrappers = em.platform_exports(program, &mut out);
    if em.errors.is_empty() {
        Ok(Output { source: out, r_wrappers })
    } else {
        Err(em.errors)
    }
}

const KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in",
    "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super",
    "trait", "true", "type", "unsafe", "use", "where", "while", "async", "await", "dyn", "abstract", "become", "box",
    "do", "final", "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "try", "gen", "main",
];

const RESERVED_TYPES: &[&str] = &[
    "Option", "Some", "None", "String", "Vec", "Box", "Rc", "Result", "Ok", "Err", "Self", "Show",
];

fn mangle(name: &str) -> String {
    let mut s: String = name.chars().map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' }).collect();
    if s.is_empty() || s.chars().next().unwrap().is_ascii_digit() {
        s.insert(0, '_');
    }
    if KEYWORDS.contains(&s.as_str()) {
        s.push('_');
    }
    s
}

fn mangle_type(name: &str) -> String {
    let mut s = mangle(name);
    if RESERVED_TYPES.contains(&s.as_str()) {
        s.push('_');
    }
    s
}

fn is_scalar(t: &IrType) -> bool {
    matches!(t, IrType::Int | IrType::Num | IrType::Bool | IrType::Char)
}

fn is_copy(t: &IrType) -> bool {
    matches!(t, IrType::Int | IrType::Num | IrType::Bool | IrType::Unit | IrType::Null)
}

#[derive(Default)]
struct Emitter {
    platform: Platform,
    module_name: String,
    errors: Vec<RustError>,
    /// Declared type aliases that are neither records nor unions of tags.
    aliases: Vec<(String, IrType)>,
    /// Declared records: name and fields (sorted).
    records: Vec<(String, Vec<(String, IrType)>)>,
    /// Record shapes with no declared name.
    anon_records: Vec<Vec<(String, IrType)>>,
    /// Declared unions of tags: name and `(tag, payload)` variants.
    enums: Vec<(String, Vec<(String, IrType)>)>,
    /// Tag name → (enum, payload type).
    tag_owner: HashMap<String, (String, IrType)>,
    /// Declared parameter and return types of every named function, by `name` or `module.name`.
    fn_sigs: HashMap<String, (Vec<IrType>, IrType)>,
    modules: HashSet<String>,
    assigned: HashSet<String>,
    scopes: Vec<HashMap<String, IrType>>,
    rets: Vec<IrType>,
    /// Generic functions (a type parameter, or an `Any` in a parameter), by key. Never emitted
    /// as such: each distinct instantiation is emitted as its own function (monomorphisation).
    generic_fns: HashMap<String, Expr>,
    /// Instantiations already requested: `(key, substitution)` → Rust name.
    instances: HashMap<String, String>,
    /// Instantiations still to emit.
    pending: Vec<PendingInstance>,
    /// Module bodies, by name: functions and the instances requested from anywhere.
    mod_bodies: std::collections::BTreeMap<String, String>,
    cur_module: Option<String>,
    /// Generic `type` declarations: parameters and body. Each use at concrete arguments is
    /// instantiated as its own struct or enum.
    generic_types: HashMap<String, (Vec<String>, IrType)>,
    /// Tag → the generic type that declares it.
    generic_tag_owner: HashMap<String, String>,
    /// `(type, args)` → the instance's Rust name.
    type_instances: HashMap<String, String>,
    /// `(instance enum, tag)` → payload.
    inst_payloads: HashMap<(String, String), IrType>,
    /// Names of interface aliases: a parameter of that type is an implicit type parameter.
    interfaces: HashSet<String>,
    /// Definitions of each function key, in source order. More than one is an overload set; the
    /// definitions are then `key#0`, `key#1`, ... (otherwise the id is the key itself).
    defs: HashMap<String, Vec<String>>,
    /// How many definitions of each key have been emitted (to name overloads).
    emitted: HashMap<String, usize>,
}

/// A user function found by name: see `Emitter::resolve_fn`.
struct Resolved {
    /// Key into `fn_sigs` / `generic_fns` (`key`, or `key#i` in an overload set).
    id: String,
    /// `name` or `module.name`.
    key: String,
    module: Option<String>,
    name: String,
    /// Position in the overload set.
    index: usize,
}

impl Resolved {
    /// The Rust name of the (non-generic) definition.
    fn rust_name(&self, defs: &HashMap<String, Vec<String>>) -> String {
        base_rust_name(self.module.as_deref(), &self.name, self.index, defs[&self.key].len() > 1)
    }

    /// How the call site spells it.
    fn path(&self, defs: &HashMap<String, Vec<String>>) -> String {
        match &self.module {
            Some(m) => format!("{}::{}", mangle(m), self.rust_name(defs)),
            None => self.rust_name(defs),
        }
    }
}

fn base_rust_name(module: Option<&str>, name: &str, index: usize, overloaded: bool) -> String {
    let base = if module.is_none() && name == "main" { "main_".to_string() } else { mangle(name) };
    if overloaded {
        format!("{base}__o{index}")
    } else {
        base
    }
}

/// Whether an argument of type `a` can be passed where `p` is declared (a loose reading, used to
/// pick between overloads once no definition matches exactly).
fn compatible(p: &IrType, a: &IrType) -> bool {
    match (p, a) {
        _ if p == a => true,
        (IrType::Any | IrType::Param(_), _) => true,
        (IrType::Num, IrType::Int) => true,
        (IrType::Optional(i), IrType::Na) => { let _ = i; true }
        (IrType::Optional(i), a) => compatible(i, a),
        (IrType::Union(ms), a) => ms.iter().any(|m| compatible(m, a)),
        (IrType::Vector(pe), IrType::Vector(ae)) => compatible(pe, ae),
        (IrType::Named(n, pa), IrType::Named(m, aa)) => n == m && pa.len() == aa.len(),
        _ => has_param(p),
    }
}

struct PendingInstance {
    key: String,
    module: Option<String>,
    rust_name: String,
    params: Vec<IrType>,
    ret: IrType,
    sub: HashMap<String, IrType>,
}

impl Emitter {
    fn err(&mut self, span: Option<&Span>, message: impl Into<String>) {
        self.errors.push(RustError { message: message.into(), span: span.cloned() });
    }

    // ---- pass 1: declarations -------------------------------------------------------------

    fn collect(&mut self, body: &[Expr], module: Option<&str>) {
        for e in body {
            match &e.kind {
                ExprKind::TypeDef { name, params, ty, .. } => self.declare_type(e, name, params, ty),
                ExprKind::Module { name, body } => {
                    self.modules.insert(name.clone());
                    self.collect(body, Some(name));
                }
                ExprKind::Let { name, value, .. } => {
                    if let ExprKind::Fn { params, ret, .. } = &value.kind {
                        let key = match module {
                            Some(m) => format!("{m}.{name}"),
                            None => name.clone(),
                        };
                        // Each `Any` (or interface) in a parameter is an implicit, independent
                        // type parameter.
                        let mut n = 0;
                        let ifaces = self.interfaces.clone();
                        let ps: Vec<IrType> = params.iter().map(|p| anyfy(&p.ty, &mut n, &ifaces)).collect();
                        let ret = if n > 0 && *ret == IrType::Any { IrType::Param("__any_ret".into()) } else { ret.clone() };
                        // A second definition of the same name makes an overload set.
                        let ids = self.defs.entry(key.clone()).or_default();
                        let id = if ids.is_empty() {
                            key.clone()
                        } else {
                            if ids.len() == 1 && ids[0] == key {
                                let first = format!("{key}#0");
                                ids[0] = first.clone();
                                if let Some(sig) = self.fn_sigs.remove(&key) {
                                    self.fn_sigs.insert(first.clone(), sig);
                                }
                                if let Some(g) = self.generic_fns.remove(&key) {
                                    self.generic_fns.insert(first, g);
                                }
                            }
                            format!("{key}#{}", ids.len())
                        };
                        self.defs.get_mut(&key).unwrap().push(id.clone());
                        if ps.iter().any(has_param) || has_param(&ret) {
                            self.generic_fns.insert(id.clone(), (**value).clone());
                        }
                        self.fn_sigs.insert(id, (ps, ret));
                    }
                }
                _ => {}
            }
        }
    }

    fn declare_type(&mut self, at: &Expr, name: &str, params: &[String], ty: &IrType) {
        if !params.is_empty() {
            for m in tags_of(ty) {
                self.generic_tag_owner.insert(m, name.to_string());
            }
            self.generic_types.insert(name.to_string(), (params.to_vec(), ty.clone()));
            return;
        }
        if matches!(ty, IrType::Other(s) if s.contains("interface")) {
            self.interfaces.insert(name.to_string());
            return;
        }
        let tname = mangle_type(name);
        if self.records.iter().any(|(n, _)| *n == tname)
            || self.enums.iter().any(|(n, _)| *n == tname)
            || self.aliases.iter().any(|(n, _)| *n == tname)
        {
            self.err(at.span.as_ref(), format!("type `{name}` is declared twice (types share one namespace on `rust`)"));
            return;
        }
        let variants: Option<Vec<(String, IrType)>> = match ty {
            IrType::Tag(n, p) => Some(vec![(n.clone(), (**p).clone())]),
            IrType::Union(ms) if !ms.is_empty() && ms.iter().all(|m| matches!(m, IrType::Tag(..))) => Some(
                ms.iter()
                    .map(|m| match m {
                        IrType::Tag(n, p) => (n.clone(), (**p).clone()),
                        _ => unreachable!(),
                    })
                    .collect(),
            ),
            _ => None,
        };
        if let Some(vs) = variants {
            for (tag, payload) in &vs {
                if self.tag_owner.insert(tag.clone(), (tname.clone(), payload.clone())).is_some() {
                    self.err(at.span.as_ref(), format!("tag `.{tag}` belongs to two declared types"));
                }
            }
            self.enums.push((tname, vs));
        } else if let IrType::Record(fs) = ty {
            self.records.push((tname, fs.clone()));
        } else {
            self.aliases.push((tname, ty.clone()));
        }
    }

    fn assigned_names(&mut self, body: &[Expr]) {
        fn walk(e: &Expr, out: &mut HashSet<String>) {
            if let ExprKind::Assign { name, .. } = &e.kind {
                out.insert(name.clone());
            }
            e.for_each_child(&mut |c| walk(c, out));
        }
        let mut out = HashSet::new();
        body.iter().for_each(|e| walk(e, &mut out));
        self.assigned = out;
    }

    // ---- types -----------------------------------------------------------------------------

    fn record_name(&mut self, fs: &[(String, IrType)]) -> String {
        if let Some((n, _)) = self.records.iter().find(|(_, f)| f == fs) {
            return n.clone();
        }
        let idx = match self.anon_records.iter().position(|f| f == fs) {
            Some(i) => i,
            None => {
                self.anon_records.push(fs.to_vec());
                self.anon_records.len() - 1
            }
        };
        format!("Rec{idx}")
    }

    fn rust_ty(&mut self, t: &IrType, at: Option<&Span>) -> String {
        match t {
            IrType::Int => "i32".into(),
            IrType::Num => "f64".into(),
            IrType::Bool => "bool".into(),
            IrType::Char => "String".into(),
            IrType::Unit | IrType::Null => "()".into(),
            IrType::Na => "Option<()>".into(),
            IrType::Optional(inner) => format!("Option<{}>", self.rust_ty(inner, at)),
            IrType::Record(fs) => self.record_name(fs),
            IrType::Tag(n, _) => match self.tag_owner.get(n) {
                Some((e, _)) => e.clone(),
                None => {
                    self.err(at, format!("tag `.{n}` is not part of a declared union (declare `type T <- .{n}(...) | ...`)"));
                    "()".into()
                }
            },
            IrType::Union(ms) => {
                let owners: HashSet<Option<String>> = ms
                    .iter()
                    .map(|m| match m {
                        IrType::Tag(n, _) => self.tag_owner.get(n).map(|(e, _)| e.clone()),
                        _ => None,
                    })
                    .collect();
                if owners.len() == 1 {
                    if let Some(Some(e)) = owners.into_iter().next() {
                        return e;
                    }
                }
                self.err(at, "a union of non-tag types is not supported on `rust` yet");
                "()".into()
            }
            IrType::Tuple(ts) => {
                let parts: Vec<String> = ts.iter().map(|t| self.rust_ty(t, at)).collect();
                format!("({},)", parts.join(", "))
            }
            IrType::Function(ps, r) => {
                let ps: Vec<String> = ps.iter().map(|t| self.rust_ty(t, at)).collect();
                let r = self.rust_ty(r, at);
                format!("std::rc::Rc<dyn Fn({}) -> {}>", ps.join(", "), r)
            }
            IrType::Named(n, args) if n == "State" && args.len() == 1 && !self.user_type("State") => {
                format!("typr_rt::State<{}>", self.rust_ty(&args[0], at))
            }
            IrType::Named(n, args) if args.is_empty() => mangle_type(n),
            IrType::Named(n, args) if self.generic_types.get(n).is_some_and(|(ps, _)| ps.len() == args.len()) => {
                self.named_instance(n, args, at)
            }
            IrType::Named(n, _) => {
                self.err(at, format!("generic type `{n}` is used with the wrong number of arguments"));
                "()".into()
            }
            IrType::Vector(elem) => format!("typr_rt::Vector<{}>", self.rust_ty(elem, at)),
            IrType::Param(n) => {
                self.err(at, format!("generic parameter `{n}` is not resolved at this point (it is a function used as a value, or a call the checker left uninstantiated)"));
                "()".into()
            }
            // An `Any` that reaches a data position holds values of any type: a `Dyn`. (An `Any`
            // in a function signature is a type parameter and never gets here.)
            IrType::Any => "typr_rt::Dyn".into(),
            IrType::Other(s) => {
                self.err(at, format!("type `{s}` has no representation on `rust`"));
                "()".into()
            }
            IrType::Unknown => {
                self.err(at, "the type checker recorded no type here");
                "()".into()
            }
        }
    }

    /// The struct or enum for generic type `name` at `args`, created on first use.
    fn named_instance(&mut self, name: &str, args: &[IrType], at: Option<&Span>) -> String {
        let key = format!("{name}|{args:?}");
        if let Some(n) = self.type_instances.get(&key) {
            return n.clone();
        }
        let (params, body) = self.generic_types[name].clone();
        if args.iter().any(has_param) {
            self.err(at, format!("generic type `{name}` is used at an unresolved type"));
            return "()".into();
        }
        let sub: HashMap<String, IrType> = params.into_iter().zip(args.iter().cloned()).collect();
        let body = subst_ty(&body, &sub);
        let is_data = matches!(body, IrType::Record(_) | IrType::Tag(..))
            || matches!(&body, IrType::Union(ms) if !ms.is_empty() && ms.iter().all(|m| matches!(m, IrType::Tag(..))));
        if !is_data {
            // `type V<T> <- [T]`: a pure alias, written out.
            let t = self.rust_ty(&body, at);
            self.type_instances.insert(key, t.clone());
            return t;
        }
        let rname = format!("{}__{}", mangle_type(name), self.type_instances.len());
        self.type_instances.insert(key, rname.clone());
        match &body {
            IrType::Record(fs) => {
                self.records.push((rname.clone(), fs.clone()));
                for (_, t) in fs {
                    self.rust_ty(t, at);
                }
            }
            _ => {
                let vs: Vec<(String, IrType)> = tags_of_typed(&body);
                for (tag, p) in &vs {
                    self.inst_payloads.insert((rname.clone(), tag.clone()), p.clone());
                    self.rust_ty(p, at);
                }
                self.enums.push((rname.clone(), vs));
            }
        }
        rname
    }

    /// The enum and payload type of `tag`, in the context of type `ctx` when the tag belongs to a
    /// generic type.
    fn tag_info(&mut self, tag: &str, ctx: &IrType, at: Option<&Span>) -> Option<(String, IrType)> {
        if let Some(r) = self.payload_of_tag(tag) {
            return Some(r);
        }
        let owner = self.generic_tag_owner.get(tag)?.clone();
        fn find<'a>(t: &'a IrType, owner: &str) -> Option<&'a Vec<IrType>> {
            match t {
                IrType::Named(n, a) if n == owner => Some(a),
                IrType::Optional(i) => find(i, owner),
                IrType::Union(ms) => ms.iter().find_map(|m| find(m, owner)),
                _ => None,
            }
        }
        let args = find(ctx, &owner)?.clone();
        let en = self.named_instance(&owner, &args, at);
        let p = self.inst_payloads.get(&(en.clone(), tag.to_string()))?.clone();
        Some((en, p))
    }

    /// Whether the program declares its own type called `name` (which then wins over the
    /// standard library's).
    fn user_type(&self, name: &str) -> bool {
        let t = mangle_type(name);
        self.records.iter().any(|(n, _)| *n == t)
            || self.enums.iter().any(|(n, _)| *n == t)
            || self.aliases.iter().any(|(n, _)| *n == t)
    }

    fn payload_of_tag(&self, tag: &str) -> Option<(String, IrType)> {
        self.tag_owner.get(tag).cloned()
    }

    // ---- scopes ----------------------------------------------------------------------------

    fn declare(&mut self, name: &str, ty: IrType) {
        if let Some(s) = self.scopes.last_mut() {
            s.insert(name.to_string(), ty);
        }
    }

    fn lookup(&self, name: &str) -> Option<IrType> {
        self.scopes.iter().rev().find_map(|s| s.get(name).cloned())
    }

    /// The type of `e`. A variable's recorded type can be wrong (the table is keyed by span, and a
    /// variable shares its span with the field access built on it), so variables are resolved
    /// through the scopes the emitter tracks itself.
    fn ty_of(&self, e: &Expr) -> IrType {
        if let ExprKind::Var(n) = &e.kind {
            if let Some(t) = self.lookup(n) {
                return t;
            }
            if let Some((ps, r)) = self.fn_sigs.get(n).or_else(|| {
                self.defs.get(n).and_then(|d| d.first()).and_then(|id| self.fn_sigs.get(id))
            }) {
                return IrType::Function(ps.clone(), Box::new(r.clone()));
            }
        }
        // A function literal's recorded type can be another function's (same span); its own
        // parameters and return type are what it is.
        if let ExprKind::Fn { params, ret, .. } = &e.kind {
            return IrType::Function(params.iter().map(|p| p.ty.clone()).collect(), Box::new(ret.clone()));
        }
        e.ty.clone()
    }

    fn mut_kw(&self, name: &str) -> &'static str {
        if self.assigned.contains(name) {
            "mut "
        } else {
            ""
        }
    }

    // ---- items -----------------------------------------------------------------------------

    fn top_item(&mut self, e: &Expr, items: &mut String, main_body: &mut String) {
        match &e.kind {
            ExprKind::TypeDef { .. } => {}
            ExprKind::Let { name, value, .. } if matches!(value.kind, ExprKind::Fn { .. }) => {
                let (id, rust_name) = self.def_for_emit(None, name);
                if !self.generic_fns.contains_key(&id) {
                    let f = self.fn_item_named(None, rust_name, value);
                    items.push_str(&f);
                }
            }
            ExprKind::Let { name, value, .. }
                if matches!(value.kind, ExprKind::Int(_) | ExprKind::Num(_) | ExprKind::Bool(_))
                    && !self.assigned.contains(name) =>
            {
                // A literal is a constant, so functions can use it.
                let ty = self.rust_ty(&value.ty, e.span.as_ref());
                let lit = self.expr(value, &value.ty.clone());
                let _ = writeln!(items, "pub const {}: {} = {};", mangle(name), ty, lit);
                self.scopes_root().insert(name.clone(), value.ty.clone());
            }
            ExprKind::Module { name, body } => {
                let mut mod_items = String::new();
                for m in body {
                    match &m.kind {
                        ExprKind::TypeDef { .. } => {}
                        ExprKind::Let { name: n, value, .. } if matches!(value.kind, ExprKind::Fn { .. }) => {
                            let (id, rust_name) = self.def_for_emit(Some(name), n);
                            if !self.generic_fns.contains_key(&id) {
                                let f = self.fn_item_named(Some(name), rust_name, value);
                                mod_items.push_str(&f);
                            }
                        }
                        ExprKind::Module { .. } => {
                            self.err(m.span.as_ref(), "nested modules are not supported on `rust` yet");
                        }
                        _ => self.err(
                            m.span.as_ref(),
                            "only functions and types are supported inside a module on `rust` yet",
                        ),
                    }
                }
                self.mod_bodies.entry(name.clone()).or_default().insert_str(0, &mod_items);
            }
            _ => {
                self.ensure_root_scope();
                let s = self.stmt(e);
                for line in s.lines() {
                    let _ = writeln!(main_body, "    {line}");
                }
            }
        }
    }

    /// The next definition of `name` to emit: its id and Rust name (overloads are numbered).
    fn def_for_emit(&mut self, module: Option<&str>, name: &str) -> (String, String) {
        let key = match module {
            Some(m) => format!("{m}.{name}"),
            None => name.to_string(),
        };
        let idx = {
            let c = self.emitted.entry(key.clone()).or_insert(0);
            *c += 1;
            *c - 1
        };
        match self.defs.get(&key) {
            Some(ids) if ids.len() > 1 && idx < ids.len() => {
                (ids[idx].clone(), base_rust_name(module, name, idx, true))
            }
            _ => (key, base_rust_name(module, name, 0, false)),
        }
    }

    fn ensure_root_scope(&mut self) {
        if self.scopes.is_empty() {
            self.scopes.push(HashMap::new());
        }
    }

    fn scopes_root(&mut self) -> &mut HashMap<String, IrType> {
        self.ensure_root_scope();
        &mut self.scopes[0]
    }

    fn fn_item_in(&mut self, module: Option<&str>, name: &str, f: &Expr) -> String {
        let rust_name = if module.is_none() && name == "main" { "main_".to_string() } else { mangle(name) };
        self.fn_item_named(module, rust_name, f)
    }

    fn fn_item_named(&mut self, module: Option<&str>, rust_name: String, f: &Expr) -> String {
        let prev_module = std::mem::replace(&mut self.cur_module, module.map(String::from));
        let out = self.fn_item_body(rust_name, f);
        self.cur_module = prev_module;
        out
    }

    fn fn_item_body(&mut self, rust_name: String, f: &Expr) -> String {
        let ExprKind::Fn { params, ret, body } = &f.kind else { return String::new() };
        let at = f.span.as_ref();
        self.scopes.push(HashMap::new());
        self.rets.push(ret.clone());
        let mut ps = Vec::new();
        for p in params {
            let ty = self.rust_ty(&p.ty, at);
            ps.push(format!("{}{}: {}", self.mut_kw(&p.name), mangle(&p.name), ty));
            self.declare(&p.name, p.ty.clone());
        }
        let ret_ty = self.rust_ty(ret, at);
        let body_s = self.body(body, ret);
        self.rets.pop();
        self.scopes.pop();
        let ret_s = if *ret == IrType::Unit { String::new() } else { format!(" -> {ret_ty}") };
        format!("#[allow(dead_code)]\npub fn {}({}){} {{\n{}\n}}\n", rust_name, ps.join(", "), ret_s, indent(&body_s))
    }

    // ---- statements and expressions ---------------------------------------------------------

    /// The inside of a `{ ... }` whose value has type `want`.
    fn body(&mut self, e: &Expr, want: &IrType) -> String {
        match &e.kind {
            ExprKind::Block(items) => self.block_inner(items, want),
            _ if *want == IrType::Unit => format!("{};", self.expr(e, &IrType::Unit)),
            _ => self.expr(e, want),
        }
    }

    fn block_inner(&mut self, items: &[Expr], want: &IrType) -> String {
        self.scopes.push(HashMap::new());
        let mut s = String::new();
        let unit = matches!(want, IrType::Unit | IrType::Unknown);
        for (i, it) in items.iter().enumerate() {
            let last = i + 1 == items.len();
            if last && !unit && !is_statement(it) {
                s.push_str(&self.expr(it, want));
            } else {
                s.push_str(&self.stmt(it));
            }
            s.push('\n');
        }
        self.scopes.pop();
        s.trim_end().to_string()
    }

    /// An expression used as a statement: ends with `;` (or is an item).
    fn stmt(&mut self, e: &Expr) -> String {
        let at = e.span.as_ref();
        match &e.kind {
            ExprKind::Let { name, value, .. } => {
                if matches!(value.kind, ExprKind::Fn { .. }) {
                    self.declare(name, value.ty.clone());
                    if let ExprKind::Fn { params, ret, .. } = &value.kind {
                        self.fn_sigs.insert(name.clone(), (params.iter().map(|p| p.ty.clone()).collect(), ret.clone()));
                    }
                    return self.fn_item_in(None, name, value);
                }
                // `let a: Opt<int> <- .Some(4)`: the binding has the declared type, not the tag's.
                let ty = match (&e.ty, &value.ty) {
                    (IrType::Named(..), IrType::Tag(..) | IrType::Union(_)) => e.ty.clone(),
                    (IrType::Any, v) if *v != IrType::Any => IrType::Any,
                    _ => value.ty.clone(),
                };
                let annotation = match ty {
                    IrType::Unknown => String::new(),
                    _ => format!(": {}", self.rust_ty(&ty, at)),
                };
                let v = self.expr(value, &ty);
                self.ensure_root_scope();
                self.declare(name, ty);
                format!("let {}{}{} = {};", self.mut_kw(name), mangle(name), annotation, v)
            }
            ExprKind::Block(_) | ExprKind::If { .. } | ExprKind::Match { .. } | ExprKind::While { .. }
            | ExprKind::Loop(_) | ExprKind::For { .. } => {
                let ty = e.ty.clone();
                let want = if matches!(ty, IrType::Unit | IrType::Unknown) { IrType::Unit } else { ty };
                let v = self.expr(e, &want);
                if matches!(e.kind, ExprKind::While { .. } | ExprKind::Loop(_) | ExprKind::For { .. }) {
                    v
                } else {
                    format!("{v};")
                }
            }
            _ => format!("{};", self.expr(e, &IrType::Unknown)),
        }
    }

    /// `e` converted to `want`: `T` into `T | na`, `int` into `num`.
    fn expr(&mut self, e: &Expr, want: &IrType) -> String {
        let at = e.span.clone();
        let at = at.as_ref();
        let have = self.ty_of(e);
        // Branching forms push the target type into their branches.
        let pushes = matches!(e.kind, ExprKind::If { .. } | ExprKind::Match { .. } | ExprKind::Block(_));
        let want_inner = match want {
            IrType::Unknown => have.clone(),
            w => w.clone(),
        };
        let s = self.expr_raw(e, &want_inner);
        if pushes {
            return s;
        }
        match (want, &have) {
            (IrType::Optional(_), IrType::Optional(_) | IrType::Na | IrType::Unknown) => s,
            (IrType::Optional(inner), h) => {
                let inner_conv = if **inner == IrType::Num && *h == IrType::Int { format!("({s} as f64)") } else { s };
                format!("Some({inner_conv})")
            }
            (IrType::Num, IrType::Int) => format!("({s} as f64)"),
            (IrType::Any, h) if !matches!(h, IrType::Any | IrType::Unknown) => format!("typr_rt::Dyn::new({s})"),
            (IrType::Vector(w), IrType::Vector(h)) => match elem_conv(w, h) {
                Some(f) => format!("{s}.map({f})"),
                None => s,
            },
            _ => {
                let _ = at;
                s
            }
        }
    }

    fn expr_raw(&mut self, e: &Expr, want: &IrType) -> String {
        use ExprKind as K;
        let at = e.span.clone();
        let at = at.as_ref();
        match &e.kind {
            K::Int(n) => {
                if *n < 0 {
                    format!("({n}_i32)")
                } else {
                    format!("{n}_i32")
                }
            }
            K::Num(x) => {
                if x.is_nan() {
                    "f64::NAN".into()
                } else if x.is_infinite() {
                    if *x > 0.0 { "f64::INFINITY" } else { "f64::NEG_INFINITY" }.into()
                } else {
                    format!("({x:?}_f64)")
                }
            }
            K::Bool(b) => b.to_string(),
            K::Str(s) => format!("String::from({s:?})"),
            K::Null | K::Unit => "()".into(),
            K::Na => match want {
                IrType::Optional(_) | IrType::Na | IrType::Unknown => "None".into(),
                _ => "None".into(),
            },
            K::Var(n) => self.var(e, n),
            K::Block(items) => {
                let inner = self.block_inner(items, want);
                format!("{{\n{}\n}}", indent(&inner))
            }
            K::Let { .. } => {
                let s = self.stmt(e);
                format!("{{ {s} }}")
            }
            K::Assign { name, value } => {
                let ty = self.lookup(name).unwrap_or(IrType::Unknown);
                let v = self.expr(value, &ty);
                format!("{} = {}", mangle(name), v)
            }
            K::Fn { params, ret, body } => {
                self.scopes.push(HashMap::new());
                self.rets.push(ret.clone());
                let mut ps = Vec::new();
                for p in params {
                    let ty = self.rust_ty(&p.ty, at);
                    ps.push(format!("{}{}: {}", self.mut_kw(&p.name), mangle(&p.name), ty));
                    self.declare(&p.name, p.ty.clone());
                }
                let ret_ty = self.rust_ty(ret, at);
                let b = self.body(body, ret);
                self.rets.pop();
                self.scopes.pop();
                format!("std::rc::Rc::new(move |{}| -> {} {{\n{}\n}})", ps.join(", "), ret_ty, indent(&b))
            }
            K::Call { func, args } => self.call(e, func, args),
            K::Binary { op, lhs, rhs } => self.binary(e, *op, lhs, rhs),
            K::Not(x) => match self.ty_of(x) {
                IrType::Vector(_) => format!("typr_rt::vnot(&{})", self.expr(x, &IrType::Unknown)),
                _ => format!("(!{})", self.expr(x, &IrType::Bool)),
            },
            K::If { cond, then, otherwise } => {
                let c = self.expr(cond, &IrType::Bool);
                let t = self.branch(then, want);
                let o = self.branch(otherwise, want);
                format!("if {c} {{\n{}\n}} else {{\n{}\n}}", indent(&t), indent(&o))
            }
            K::Match { scrutinee, arms } => self.match_expr(e, scrutinee, arms, want),
            K::Vector(items) => {
                let elem = match (&e.ty, want) {
                    (IrType::Vector(t), _) => (**t).clone(),
                    (_, IrType::Vector(t)) => (**t).clone(),
                    _ => IrType::Unknown,
                };
                let parts: Vec<String> = items.iter().map(|i| self.expr(i, &elem)).collect();
                format!("typr_rt::Vector::new(vec![{}])", parts.join(", "))
            }
            K::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(|i| self.expr(i, &IrType::Unknown)).collect();
                format!("({},)", parts.join(", "))
            }
            K::Record { name, fields } => self.record(e, name.as_deref(), fields),
            K::Tag { name, payload } => match {
                let ctx = if matches!(want, IrType::Named(..)) { want.clone() } else { e.ty.clone() };
                self.tag_info(name, &ctx, at)
            } {
                Some((en, pty)) => {
                    if pty == IrType::Unit {
                        format!("{en}::{}", mangle_type(name))
                    } else {
                        let p = self.expr(payload, &pty);
                        format!("{en}::{}({p})", mangle_type(name))
                    }
                }
                None => {
                    self.err(at, format!("tag `.{name}` is not part of a declared union"));
                    "todo!()".into()
                }
            },
            K::Field { target, name } => {
                if let K::Var(m) = &target.kind {
                    if self.modules.contains(m) && self.lookup(m).is_none() {
                        return format!("{}::{}", mangle(m), mangle(name));
                    }
                }
                let place = self.place(target);
                let access = format!("{place}.{}", mangle(name));
                if is_copy(&e.ty) {
                    access
                } else {
                    format!("{access}.clone()")
                }
            }
            K::Index { target, index } => {
                if !matches!(self.ty_of(target), IrType::Vector(_)) {
                    self.err(at, "only vectors can be indexed on `rust`");
                    return "todo!()".into();
                }
                let t = self.place(target);
                // The parser wraps an index in a one-element vector literal: `v[2]` is `v[[2]]`.
                let inner = match &index.kind {
                    ExprKind::Vector(items) if items.len() == 1 => &items[0],
                    _ => &**index,
                };
                if matches!(e.ty, IrType::Vector(_)) {
                    let is = self.expr(inner, &IrType::Vector(Box::new(IrType::Int)));
                    format!("{t}.select(&{is})")
                } else {
                    let i = self.expr(inner, &IrType::Int);
                    format!("{t}.at({i})")
                }
            }
            K::Return(v) => {
                let r = self.rets.last().cloned().unwrap_or(IrType::Unknown);
                format!("return {}", self.expr(v, &r))
            }
            K::While { cond, body } => {
                let c = self.expr(cond, &IrType::Bool);
                let b = self.body(body, &IrType::Unit);
                format!("while {c} {{\n{}\n}}", indent(&b))
            }
            K::Loop(body) => {
                let b = self.body(body, &IrType::Unit);
                format!("loop {{\n{}\n}}", indent(&b))
            }
            K::For { var, iter, body } => {
                let elem = match self.ty_of(iter) {
                    IrType::Vector(t) => *t,
                    other => {
                        self.err(at, format!("`for` iterates a vector on `rust`, not `{other:?}`"));
                        IrType::Unknown
                    }
                };
                let it = self.place(iter);
                self.scopes.push(HashMap::new());
                self.declare(var, elem);
                let b = self.body(body, &IrType::Unit);
                self.scopes.pop();
                format!("for {}{} in {it}.iter() {{\n{}\n}}", self.mut_kw(var), mangle(var), indent(&b))
            }
            K::Cast { value, name } => {
                let have = self.ty_of(value);
                let v = self.expr(value, &IrType::Unknown);
                if have == IrType::Any {
                    let to = self.rust_ty(&e.ty, at);
                    format!("{v}.downcast::<{to}>({name:?})")
                } else if have == e.ty {
                    v
                } else if is_scalar(&have) && is_scalar(&e.ty) {
                    // A value of a known scalar type narrowed to another: it always fails.
                    format!("{{ let _ = {v}; panic!(\"Validation failed for type {name}: expected {name}\") }}")
                } else {
                    self.err(at, format!("`as! {name}` (a validation of a {have:?}) is not supported on `rust` yet"));
                    "todo!()".into()
                }
            }
            K::Refine { value, checks, loc } => {
                let have = self.ty_of(value);
                let numeric = matches!(have, IrType::Int | IrType::Num)
                    || matches!(&have, IrType::Vector(t) if matches!(**t, IrType::Int | IrType::Num));
                let vector = matches!(have, IrType::Vector(_));
                let mut out = self.expr(value, &IrType::Unknown);
                for c in checks {
                    let ok = if c.length { vector } else { numeric };
                    if !ok {
                        self.err(at, format!("a refinement of a {have:?} cannot be checked on `rust`"));
                        return "todo!()".into();
                    }
                    let end = |b: &Option<(f64, bool)>, inf: &str| match b {
                        Some((v, open)) => (format!("{v:?}"), *open),
                        None => (inf.to_string(), false),
                    };
                    let (lo, lo_open) = end(&c.lo, "f64::NEG_INFINITY");
                    let (hi, hi_open) = end(&c.hi, "f64::INFINITY");
                    let f = if c.length { "refine_length" } else { "refine_value" };
                    out = format!("typr_rt::{f}({out}, {lo}, {lo_open}, {hi}, {hi_open}, {loc:?})");
                }
                out
            }
            K::Break => "break".into(),
            K::Next => "continue".into(),
            K::TypeDef { .. } => "()".into(),
            K::Module { .. } => {
                self.err(at, "a module must be declared at the top level on `rust`");
                "()".into()
            }
            K::Unsupported { what } => {
                self.err(at, format!("{what} cannot be compiled to `rust`"));
                "todo!()".into()
            }
        }
    }

    /// A branch of `if` / `match`: its own block, the value coerced to `want`.
    fn branch(&mut self, e: &Expr, want: &IrType) -> String {
        match &e.kind {
            ExprKind::Block(items) => self.block_inner(items, want),
            _ => self.expr(e, want),
        }
    }

    fn var(&mut self, e: &Expr, n: &str) -> String {
        let ty = self.ty_of(e);
        let name = if n == "main" && self.lookup(n).is_none() { "main_".to_string() } else { mangle(n) };
        // A named function used as a value.
        if self.lookup(n).is_none() && (self.fn_sigs.contains_key(n) || self.defs.contains_key(n)) {
            return format!("std::rc::Rc::new({name})");
        }
        if is_copy(&ty) {
            name
        } else {
            format!("{name}.clone()")
        }
    }

    /// A place expression: no clone, for field access and calls.
    fn place(&mut self, e: &Expr) -> String {
        match &e.kind {
            ExprKind::Var(n) => mangle(n),
            ExprKind::Field { target, name } => format!("{}.{}", self.place(target), mangle(name)),
            _ => format!("({})", self.expr(e, &IrType::Unknown)),
        }
    }

    /// `print`, `length`, `sum`, `seq` and `State<T>`: the calls the backend implements itself.
    fn intrinsic(&mut self, e: &Expr, name: &str, args: &[Expr]) -> String {
        let at = e.span.clone();
        let at = at.as_ref();
        let tys: Vec<IrType> = args.iter().map(|a| self.ty_of(a)).collect();
        let state_inner = |t: &IrType| match t {
            IrType::Named(n, a) if n == "State" && a.len() == 1 => Some(a[0].clone()),
            _ => None,
        };
        match (name, tys.as_slice()) {
            ("print", [_]) => format!("typr_rt::print({})", self.expr(&args[0], &IrType::Unknown)),
            ("length", [IrType::Vector(_)]) => format!("typr_rt::length({})", self.expr(&args[0], &IrType::Unknown)),
            ("sum", [IrType::Vector(t)]) if **t == IrType::Int => {
                format!("typr_rt::sum_i({})", self.expr(&args[0], &IrType::Unknown))
            }
            ("sum", [IrType::Vector(t)]) if **t == IrType::Num => {
                format!("typr_rt::sum_n({})", self.expr(&args[0], &IrType::Unknown))
            }
            ("seq", [IrType::Int, IrType::Int, IrType::Int]) => {
                let a: Vec<String> = args.iter().map(|a| self.expr(a, &IrType::Int)).collect();
                format!("typr_rt::seq({})", a.join(", "))
            }
            ("state", [t]) => format!("typr_rt::state({})", self.expr(&args[0], &t.clone())),
            ("get" | "version", [s]) if state_inner(s).is_some() => {
                format!("typr_rt::{name}({})", self.expr(&args[0], &IrType::Unknown))
            }
            ("set" | "update", [s, _]) if state_inner(s).is_some() => {
                let inner = state_inner(s).unwrap();
                let want = if name == "set" { inner.clone() } else { IrType::Function(vec![inner.clone()], Box::new(inner)) };
                let a = self.expr(&args[0], &IrType::Unknown);
                let b = self.expr(&args[1], &want);
                format!("typr_rt::{name}({a}, {b})")
            }
            ("map", [IrType::Vector(t), IrType::Function(..)]) => {
                let f = IrType::Function(vec![(**t).clone()], Box::new(IrType::Unknown));
                let (v, f) = (self.expr(&args[0], &IrType::Unknown), self.expr(&args[1], &f));
                format!("typr_rt::vmap({v}, {f})")
            }
            ("map", [s, IrType::Function(..)]) if state_inner(s).is_some() => {
                let f = IrType::Function(vec![state_inner(s).unwrap()], Box::new(IrType::Unknown));
                let (v, f) = (self.expr(&args[0], &IrType::Unknown), self.expr(&args[1], &f));
                format!("typr_rt::state_map({v}, {f})")
            }
            ("filter", [IrType::Vector(t), IrType::Function(..)]) => {
                let f = IrType::Function(vec![(**t).clone()], Box::new(IrType::Bool));
                let (v, f) = (self.expr(&args[0], &IrType::Unknown), self.expr(&args[1], &f));
                format!("typr_rt::filter({v}, {f})")
            }
            ("reduce", [IrType::Vector(t), IrType::Function(..)]) => {
                let f = IrType::Function(vec![(**t).clone(), (**t).clone()], t.clone());
                let (v, f) = (self.expr(&args[0], &IrType::Unknown), self.expr(&args[1], &f));
                format!("typr_rt::reduce({v}, {f})")
            }
            ("fold", [IrType::Vector(t), u, IrType::Function(..)]) => {
                let f = IrType::Function(vec![u.clone(), (**t).clone()], Box::new(u.clone()));
                let (v, i, f) =
                    (self.expr(&args[0], &IrType::Unknown), self.expr(&args[1], u), self.expr(&args[2], &f));
                format!("typr_rt::fold({v}, {i}, {f})")
            }
            ("extend", [IrType::Vector(t), _]) => {
                let (v, x) = (self.expr(&args[0], &IrType::Unknown), self.expr(&args[1], t));
                format!("typr_rt::extend({v}, {x})")
            }
            ("set_at", [IrType::Vector(t), IrType::Int, _]) => {
                let (v, i, x) = (
                    self.expr(&args[0], &IrType::Unknown),
                    self.expr(&args[1], &IrType::Int),
                    self.expr(&args[2], t),
                );
                format!("typr_rt::set_at({v}, {i}, {x})")
            }
            ("as_vec", [IrType::Vector(_)]) => self.expr(&args[0], &IrType::Unknown),
            ("startsWith" | "endsWith", [IrType::Char, IrType::Char]) => {
                let f = if name == "startsWith" { "starts_with" } else { "ends_with" };
                let a: Vec<String> = args.iter().map(|a| self.expr(a, &IrType::Char)).collect();
                format!("typr_rt::{f}({})", a.join(", "))
            }
            ("contains", [IrType::Char, IrType::Char]) => {
                let a: Vec<String> = args.iter().map(|a| self.expr(a, &IrType::Char)).collect();
                format!("typr_rt::contains({})", a.join(", "))
            }
            ("join", [IrType::Vector(t), IrType::Char]) if **t == IrType::Char => {
                let (v, s) = (self.expr(&args[0], &IrType::Unknown), self.expr(&args[1], &IrType::Char));
                format!("typr_rt::join({v}, {s})")
            }
            ("stop", [IrType::Char]) => format!("typr_rt::stop({})", self.expr(&args[0], &IrType::Char)),
            _ => {
                self.err(at, format!("`{name}` is not available on `rust` for these argument types"));
                "todo!()".into()
            }
        }
    }

    fn call(&mut self, e: &Expr, func: &Expr, args: &[Expr]) -> String {
        let at = e.span.clone();
        let at = at.as_ref();
        let arg_tys: Vec<IrType> = args.iter().map(|a| self.ty_of(a)).collect();
        let (callee, params) = if let Some(r) = self.resolve_fn(func, &arg_tys) {
            if self.generic_fns.contains_key(&r.id) {
                return self.generic_call(e, &r, func, args);
            }
            let ps = self.fn_sigs.get(&r.id).map(|s| s.0.clone()).unwrap_or_default();
            (r.path(&self.defs), ps)
        } else {
            match &func.kind {
                ExprKind::Var(n) if self.lookup(n).is_some() || self.fn_sigs.contains_key(n) => {
                    // A local function or closure.
                    let ps = match self.ty_of(func) {
                        IrType::Function(ps, _) => ps,
                        _ => Vec::new(),
                    };
                    (mangle(n), ps)
                }
                ExprKind::Var(name) => {
                    // The operators the checker desugars to calls (`q$x + p$x`).
                    if args.len() == 2 {
                        if let Some(op) = op_of(name) {
                            return self.binary(e, op, &args[0], &args[1]);
                        }
                    }
                    if INTRINSICS.contains(&name.as_str()) {
                        return self.intrinsic(e, name, args);
                    }
                    self.err(at, format!("`{name}` is not available on `rust` (no implementation for this target)"));
                    return "todo!()".into();
                }
                _ => {
                    let ps = match &func.ty {
                        IrType::Function(ps, _) => ps.clone(),
                        _ => Vec::new(),
                    };
                    (format!("({})", self.expr(func, &IrType::Unknown)), ps)
                }
            }
        };
        // A scalar function applied to vectors is lifted element-wise, with recycling.
        let is_vec = |t: &IrType| matches!(t, IrType::Vector(_));
        let lifted: Vec<bool> = args
            .iter()
            .enumerate()
            .map(|(i, a)| is_vec(&self.ty_of(a)) && params.get(i).is_some_and(|p| !is_vec(p) && !matches!(p, IrType::Any | IrType::Param(_))))
            .collect();
        if lifted.iter().any(|l| *l) {
            let rendered: Vec<String> = args
                .iter()
                .enumerate()
                .map(|(i, a)| {
                    let p = params.get(i).cloned().unwrap_or(IrType::Unknown);
                    if lifted[i] {
                        self.expr(a, &IrType::Vector(Box::new(p)))
                    } else {
                        format!("typr_rt::scalar({})", self.expr(a, &p))
                    }
                })
                .collect();
            return match rendered.as_slice() {
                [a] => format!("{a}.map(|a| {callee}(a))"),
                [a, b] => format!("typr_rt::zip_with(&{a}, &{b}, |a, b| {callee}(a, b))"),
                _ => {
                    self.err(at, "a function lifted over vectors takes at most two arguments on `rust`");
                    "todo!()".into()
                }
            };
        }
        let rendered: Vec<String> = args
            .iter()
            .enumerate()
            .map(|(i, a)| self.expr(a, params.get(i).unwrap_or(&IrType::Unknown)))
            .collect();
        format!("{callee}({})", rendered.join(", "))
    }

    // ---- generics: monomorphisation ----------------------------------------------------------

    /// The user function `func` names at a call with arguments of types `arg_tys`: its
    /// definition (an overload set is resolved by the arguments) and where it lives.
    fn resolve_fn(&self, func: &Expr, arg_tys: &[IrType]) -> Option<Resolved> {
        let (key, module, name) = match &func.kind {
            ExprKind::Var(n) if self.lookup(n).is_none() => {
                let sibling = self.cur_module.as_ref().map(|m| (format!("{m}.{n}"), Some(m.clone())));
                match sibling.filter(|(k, _)| self.defs.contains_key(k)) {
                    Some((k, m)) => (k, m, n.clone()),
                    None if self.defs.contains_key(n) => (n.clone(), None, n.clone()),
                    None => return None,
                }
            }
            ExprKind::Field { target, name } => match &target.kind {
                ExprKind::Var(m) if self.modules.contains(m) && self.lookup(m).is_none() => {
                    let k = format!("{m}.{name}");
                    if !self.defs.contains_key(&k) {
                        return None;
                    }
                    (k, Some(m.clone()), name.clone())
                }
                _ => return None,
            },
            _ => return None,
        };
        let ids = &self.defs[&key];
        let index = if ids.len() == 1 { 0 } else { self.pick_overload(ids, arg_tys) };
        Some(Resolved { id: ids[index].clone(), key, module, name, index })
    }

    /// The overload whose parameters fit `arg_tys`: an exact match first, then a compatible one.
    fn pick_overload(&self, ids: &[String], arg_tys: &[IrType]) -> usize {
        let sig = |i: usize| self.fn_sigs.get(&ids[i]).map(|s| s.0.clone()).unwrap_or_default();
        let exact = (0..ids.len()).find(|&i| {
            let ps = sig(i);
            ps.len() == arg_tys.len() && ps.iter().zip(arg_tys).all(|(p, a)| p == a)
        });
        exact
            .or_else(|| {
                (0..ids.len()).find(|&i| {
                    let ps = sig(i);
                    ps.len() == arg_tys.len() && ps.iter().zip(arg_tys).all(|(p, a)| compatible(p, a))
                })
            })
            .unwrap_or(0)
    }

    /// A call of a generic function: the type parameters are read off the call site, and the
    /// call goes to the instance for those types (emitted later, once).
    fn generic_call(&mut self, e: &Expr, r: &Resolved, func: &Expr, args: &[Expr]) -> String {
        let at = e.span.clone();
        let at = at.as_ref();
        let key = r.id.as_str();
        let (dps, dret) = self.fn_sigs[key].clone();
        let (module, name) = (r.module.clone(), r.name.clone());
        // The arguments' own types first: the checker's instantiation of the callee is a
        // single signature shared by every parameter and can disagree with them.
        let mut sub = HashMap::new();
        let tys: Vec<IrType> = args.iter().map(|a| self.ty_of(a)).collect();
        dps.iter().zip(&tys).for_each(|(d, a)| unify(d, a, &mut sub));
        if let IrType::Function(ps, r) = &func.ty {
            if ps.len() == dps.len() {
                dps.iter().zip(ps).for_each(|(d, a)| unify(d, a, &mut sub));
                unify(&dret, r, &mut sub);
            }
        }
        if !matches!(e.ty, IrType::Any | IrType::Union(_)) {
            unify(&dret, &e.ty, &mut sub);
        }
        // A return `Any` the call site cannot pin down holds whatever the body produces: a `Dyn`.
        sub.entry("__any_ret".to_string()).or_insert(IrType::Any);
        let ps: Vec<IrType> = dps.iter().map(|d| subst_ty(d, &sub)).collect();
        let ret = subst_ty(&dret, &sub);
        if ps.iter().any(has_param) || has_param(&ret) {
            self.err(at, format!("cannot infer the type parameters of `{name}` at this call"));
            return "todo!()".into();
        }
        let mut sorted: Vec<(&String, &IrType)> = sub.iter().collect();
        sorted.sort_by(|a, b| a.0.cmp(b.0));
        let inst_key = format!("{key}|{sorted:?}");
        let rust_name = match self.instances.get(&inst_key) {
            Some(n) => n.clone(),
            None => {
                let n = format!("{}__{}", mangle(&name), self.instances.len());
                self.instances.insert(inst_key, n.clone());
                self.pending.push(PendingInstance {
                    key: key.to_string(),
                    module: module.clone(),
                    rust_name: n.clone(),
                    params: ps.clone(),
                    ret,
                    sub,
                });
                n
            }
        };
        let rendered: Vec<String> = args.iter().zip(&ps).map(|(a, p)| self.expr(a, p)).collect();
        let path = match &module {
            Some(m) => format!("{}::{rust_name}", mangle(m)),
            None => rust_name,
        };
        format!("{path}({})", rendered.join(", "))
    }

    /// Emits every requested instance (which may request others).
    fn drain_instances(&mut self, items: &mut String) {
        while let Some(inst) = self.pending.pop() {
            let Some(orig) = self.generic_fns.get(&inst.key).cloned() else { continue };
            let ExprKind::Fn { params, body, .. } = &orig.kind else { continue };
            // The body names its type parameters with the checker's rigid variables, which are
            // not the ones the signature uses: read them off the parameters' uses.
            let mut sub = inst.sub.clone();
            let env: HashMap<String, IrType> =
                params.iter().zip(&inst.params).map(|(p, t)| (p.name.clone(), t.clone())).collect();
            derive_rigids(body, &env, &mut sub);
            unify(&body.ty, &inst.ret, &mut sub);
            let mut f = orig.clone();
            subst_expr(&mut f, &sub);
            if let ExprKind::Fn { params, ret, .. } = &mut f.kind {
                for (p, t) in params.iter_mut().zip(&inst.params) {
                    p.ty = t.clone();
                }
                *ret = inst.ret.clone();
            }
            let root = self.scopes.first().cloned().unwrap_or_default();
            let saved_scopes = std::mem::replace(&mut self.scopes, vec![root]);
            let saved_rets = std::mem::take(&mut self.rets);
            let text = self.fn_item_named(inst.module.as_deref(), inst.rust_name.clone(), &f);
            self.scopes = saved_scopes;
            self.rets = saved_rets;
            match &inst.module {
                Some(m) => self.mod_bodies.entry(m.clone()).or_default().push_str(&text),
                None => items.push_str(&text),
            }
        }
    }

    fn binary(&mut self, e: &Expr, op: BinOp, lhs: &Expr, rhs: &Expr) -> String {
        let at = e.span.clone();
        let at = at.as_ref();
        let (lt, rt) = (self.ty_of(lhs), self.ty_of(rhs));
        let (vec_l, vec_r) = (matches!(lt, IrType::Vector(_)), matches!(rt, IrType::Vector(_)));
        let (le, re) = (elem_of(&lt), elem_of(&rt));
        let numeric = |t: &IrType| matches!(t, IrType::Int | IrType::Num);
        // `int` and `num` mix: the integer is widened, as R does.
        let operand_ty = if numeric(&le) && numeric(&re) && le != re { IrType::Num } else { le.clone() };
        if vec_l || vec_r {
            // Element-wise, with R's recycling rule; a scalar is a vector of length one.
            if op == BinOp::In {
                self.err(at, "`in` is not supported on `rust` yet");
                return "todo!()".into();
            }
            let vt = IrType::Vector(Box::new(operand_ty.clone()));
            let side = |em: &mut Self, x: &Expr, is_vec: bool| {
                if is_vec {
                    em.expr(x, &vt)
                } else {
                    format!("typr_rt::scalar({})", em.expr(x, &operand_ty))
                }
            };
            let l = side(self, lhs, vec_l);
            let r = side(self, rhs, vec_r);
            let body = op_text(op, operand_ty == IrType::Int, "a", "b");
            return format!("typr_rt::zip_with(&{l}, &{r}, |a, b| {body})");
        }
        let l = self.expr(lhs, &operand_ty);
        let r = self.expr(rhs, &operand_ty);
        if op == BinOp::In {
            self.err(at, "`in` is not supported on `rust` yet");
            return "todo!()".into();
        }
        op_text(op, operand_ty == IrType::Int, &l, &r)
    }

    fn record(&mut self, e: &Expr, name: Option<&str>, fields: &[(String, Expr)]) -> String {
        let at = e.span.clone();
        let at = at.as_ref();
        let (sname, decl): (String, Vec<(String, IrType)>) = match name {
            Some(n) => {
                let t = match (&e.ty, self.generic_types.contains_key(n)) {
                    (IrType::Named(en, args), true) if en == n => self.named_instance(n, args, at),
                    _ => mangle_type(n),
                };
                match self.records.iter().find(|(rn, _)| *rn == t) {
                    Some((_, fs)) => (t, fs.clone()),
                    None => {
                        self.err(at, format!("`{n}` is not a declared record type"));
                        return "todo!()".into();
                    }
                }
            }
            None => match &e.ty {
                IrType::Record(fs) => (self.record_name(fs), fs.clone()),
                IrType::Named(n, a) if a.is_empty() => {
                    let t = mangle_type(n);
                    let fs = self.records.iter().find(|(rn, _)| *rn == t).map(|(_, f)| f.clone()).unwrap_or_default();
                    (t, fs)
                }
                other => {
                    self.err(at, format!("record of type `{other:?}` has no representation on `rust`"));
                    return "todo!()".into();
                }
            },
        };
        let parts: Vec<String> = fields
            .iter()
            .map(|(f, v)| {
                let want = decl.iter().find(|(n, _)| n == f).map(|(_, t)| t.clone()).unwrap_or(IrType::Unknown);
                format!("{}: {}", mangle(f), self.expr(v, &want))
            })
            .collect();
        format!("{sname} {{ {} }}", parts.join(", "))
    }

    fn match_expr(&mut self, e: &Expr, scrutinee: &Expr, arms: &[Arm], want: &IrType) -> String {
        let at = e.span.clone();
        let at = at.as_ref();
        let sty = self.ty_of(scrutinee);
        let mut s = self.expr(scrutinee, &IrType::Unknown);
        if sty == IrType::Char {
            s = format!("{s}.as_str()");
        }
        let mut out = format!("match {s} {{\n");
        for arm in arms {
            self.scopes.push(HashMap::new());
            let pat = match &arm.pattern {
                Pattern::Tag { name, binding } => match self.tag_info(name, &sty, at) {
                    Some((en, pty)) => {
                        if pty == IrType::Unit {
                            format!("{en}::{}", mangle_type(name))
                        } else if let Some(b) = binding {
                            self.declare(b, pty);
                            format!("{en}::{}({})", mangle_type(name), mangle(b))
                        } else {
                            format!("{en}::{}(_)", mangle_type(name))
                        }
                    }
                    None => {
                        self.err(at, format!("tag `.{name}` is not part of a declared union"));
                        "_".into()
                    }
                },
                Pattern::Bind(n) if n == "_" => "_".into(),
                Pattern::Bind(n) => {
                    self.declare(n, sty.clone());
                    mangle(n)
                }
                Pattern::Lit(l) => match &l.kind {
                    ExprKind::Int(n) => format!("{n}_i32"),
                    ExprKind::Bool(b) => b.to_string(),
                    ExprKind::Str(t) => format!("{t:?}"),
                    _ => {
                        self.err(at, "only integer, boolean and string literals can be matched on `rust` yet");
                        "_".into()
                    }
                },
                Pattern::Type { .. } => {
                    self.err(at, "matching on a type (`x as int`) needs union types, not supported on `rust` yet");
                    "_".into()
                }
            };
            let body = self.branch(&arm.body, want);
            self.scopes.pop();
            let _ = writeln!(out, "    {pat} => {{\n{}\n    }}", indent(&indent(&body)));
        }
        out.push('}');
        out
    }

    // ---- output ----------------------------------------------------------------------------

    fn finish(&mut self, items: &str, main_body: &str) -> String {
        let mut out = String::new();
        out.push_str("// Generated by `typr build --target rust`. A build artifact: do not edit.\n");
        out.push_str("#![allow(unused, non_snake_case, non_camel_case_types, non_upper_case_globals, unused_mut, unreachable_code, clippy::all)]\n\n");
        out.push_str("mod typr_rt {\n");
        out.push_str(RUNTIME);
        out.push_str("}\n\n");
        out.push_str("use typr_rt::Show;\n\n");

        let mut decls = String::new();
        let enums = self.enums.clone();
        for (name, variants) in &enums {
            let vs: Vec<String> = variants
                .iter()
                .map(|(tag, p)| {
                    if *p == IrType::Unit {
                        mangle_type(tag)
                    } else {
                        format!("{}({})", mangle_type(tag), self.rust_ty(p, None))
                    }
                })
                .collect();
            let _ = writeln!(
                decls,
                "#[derive(Debug, Clone, PartialEq)]\npub enum {name} {{ {} }}\nimpl Show for {name} {{ fn show(&self) -> String {{ format!(\"{{:?}}\", self) }} }}\nimpl typr_rt::Elem for {name} {{ fn cell(&self) -> String {{ format!(\"{{:?}}\", self) }} }}",
                vs.join(", ")
            );
        }
        let mut structs: Vec<(String, Vec<(String, IrType)>)> = self.records.clone();
        let anon = self.anon_records.clone();
        for (i, fs) in anon.iter().enumerate() {
            structs.push((format!("Rec{i}"), fs.clone()));
        }
        for (name, fields) in &structs {
            let fs: Vec<String> =
                fields.iter().map(|(f, t)| format!("pub {}: {}", mangle(f), self.rust_ty(t, None))).collect();
            let _ = writeln!(
                decls,
                "#[derive(Debug, Clone, PartialEq)]\npub struct {name} {{ {} }}\nimpl Show for {name} {{ fn show(&self) -> String {{ format!(\"{{:?}}\", self) }} }}\nimpl typr_rt::Elem for {name} {{ fn cell(&self) -> String {{ format!(\"{{:?}}\", self) }} }}",
                fs.join(", ")
            );
        }
        let aliases = self.aliases.clone();
        for (name, ty) in &aliases {
            let t = self.rust_ty(ty, None);
            let _ = writeln!(decls, "pub type {name} = {t};");
        }
        out.push_str(&decls);
        out.push('\n');
        out.push_str(items);
        for (name, body) in &self.mod_bodies {
            let _ = writeln!(
                out,
                "pub mod {} {{\n    #[allow(unused_imports)]\n    use super::*;\n{}}}",
                mangle(name),
                body
            );
        }
        if self.platform == Platform::Bin {
            out.push_str("\nfn main() {\n    typr_rt::init();\n");
            out.push_str(main_body);
            out.push_str("}\n");
        }
        out
    }

    /// The part of the output that only exists on `wasm` and `extendr`: the exported entry points
    /// are appended to `out`; the R side of an `extendr` crate is returned.
    fn platform_exports(&mut self, program: &Program, out: &mut String) -> Option<String> {
        match self.platform {
            Platform::Bin | Platform::Lib => None,
            Platform::Wasm => {
                // A C ABI carries scalars only; other `pub` functions stay callable from Rust.
                for ex in exports(program) {
                    let scalar = |t: &IrType| platform::c_scalar(t).is_some();
                    if !ex.params.iter().all(|p| scalar(&p.ty)) || !(scalar(&ex.ret) || ex.ret == IrType::Unit) {
                        continue;
                    }
                    let ps: Vec<String> =
                        ex.params.iter().map(|p| format!("{}: {}", mangle(&p.name), platform::c_scalar(&p.ty).unwrap())).collect();
                    let args: Vec<String> = ex.params.iter().map(|p| mangle(&p.name)).collect();
                    let ret = platform::c_scalar(&ex.ret).map(|t| format!(" -> {t}")).unwrap_or_default();
                    let _ = writeln!(
                        out,
                        "\n#[export_name = \"{}\"]\npub extern \"C\" fn __wasm_{}({}){} {{ {}({}) }}",
                        ex.name,
                        mangle(&ex.name),
                        ps.join(", "),
                        ret,
                        mangle(&ex.name),
                        args.join(", ")
                    );
                }
                None
            }
            Platform::Extendr => Some(self.extendr_exports(program, out)),
        }
    }

    /// `#[extendr]` wrappers around the exported functions, the `extendr_module!`, and the R
    /// functions that `.Call` them.
    fn extendr_exports(&mut self, program: &Program, out: &mut String) -> String {
        let mut seen = HashSet::new();
        let mut rust_fns = Vec::new();
        let mut r = String::from(
            "# Generated by `typr build --target rust --platform extendr`. A build artifact: do not edit.\n",
        );
        let _ = writeln!(r, "#' @useDynLib {}, .registration = TRUE\nNULL\n", self.module_name);
        out.push_str("\nuse extendr_api::prelude::*;\n");
        for ex in exports(program) {
            if !seen.insert(ex.name.clone()) {
                self.err(ex.span.as_ref(), format!("`{}` is defined more than once: an overloaded function cannot be exported", ex.name));
                continue;
            }
            let at = ex.span.clone();
            let mut ps = Vec::new();
            let mut args = Vec::new();
            for p in &ex.params {
                let ty = self.rust_ty(&p.ty, at.as_ref());
                let name = mangle(&p.name);
                match self.boundary_in(&p.ty, &name) {
                    Some((host_ty, conv)) => {
                        ps.push(format!("{name}: {}", host_ty.unwrap_or(ty)));
                        args.push(conv);
                    }
                    None => {
                        self.err(at.as_ref(), format!("`{}`: parameter `{}` of type `{}` is not supported through extendr yet", ex.name, p.name, p.ty));
                    }
                }
            }
            let rust_name = format!("typr_export_{}", mangle(&ex.name));
            let call = format!("{}({})", mangle(&ex.name), args.join(", "));
            let (ret_ty, body) = match &ex.ret {
                IrType::Unit => (String::new(), format!("{call};")),
                t => match self.boundary_out(t, &call) {
                    Some((host_ty, conv)) => {
                        let ty = host_ty.unwrap_or_else(|| self.rust_ty(t, at.as_ref()));
                        (format!(" -> {ty}"), conv)
                    }
                    None => {
                        self.err(at.as_ref(), format!("`{}`: return type `{}` is not supported through extendr yet", ex.name, t));
                        continue;
                    }
                },
            };
            let _ = writeln!(out, "\n#[extendr]\nfn {rust_name}({}){ret_ty} {{ {body} }}", ps.join(", "));
            let r_params: Vec<String> = ex.params.iter().map(|p| p.name.clone()).collect();
            let _ = writeln!(
                r,
                "{} <- function({}) .Call(wrap__{rust_name}{})",
                ex.name,
                r_params.join(", "),
                r_params.iter().map(|p| format!(", {p}")).collect::<String>()
            );
            rust_fns.push(rust_name);
        }
        let _ = writeln!(
            out,
            "\nextendr_module! {{\n    mod {};\n{}}}",
            if self.module_name.is_empty() { "typr_out" } else { &self.module_name },
            rust_fns.iter().map(|f| format!("    fn {f};\n")).collect::<String>()
        );
        r
    }

    /// How a host value becomes the TypR parameter: the host type (when it isn't the TypR one),
    /// and the expression that converts `name`. `None`: not supported yet.
    fn boundary_in(&mut self, t: &IrType, name: &str) -> Option<(Option<String>, String)> {
        match t {
            IrType::Int | IrType::Num | IrType::Bool | IrType::Char => Some((None, name.to_string())),
            IrType::Optional(inner) if is_scalar(inner) => Some((None, name.to_string())),
            IrType::Vector(el) if matches!(**el, IrType::Int | IrType::Num | IrType::Char) => {
                let el = self.rust_ty(el, None);
                Some((Some(format!("Vec<{el}>")), format!("typr_rt::Vector::new({name})")))
            }
            _ => None,
        }
    }

    /// Same, for a returned value: the host type and the expression converting `expr`.
    fn boundary_out(&mut self, t: &IrType, expr: &str) -> Option<(Option<String>, String)> {
        match t {
            IrType::Int | IrType::Num | IrType::Bool | IrType::Char => Some((None, expr.to_string())),
            IrType::Optional(inner) if is_scalar(inner) => Some((None, expr.to_string())),
            IrType::Vector(el) if matches!(**el, IrType::Int | IrType::Num | IrType::Char) => {
                let el = self.rust_ty(el, None);
                Some((Some(format!("Vec<{el}>")), format!("{expr}.to_vec()")))
            }
            _ => None,
        }
    }
}

/// The tag names of a tag or a union of tags.
fn tags_of(t: &IrType) -> Vec<String> {
    tags_of_typed(t).into_iter().map(|(n, _)| n).collect()
}

fn tags_of_typed(t: &IrType) -> Vec<(String, IrType)> {
    match t {
        IrType::Tag(n, p) => vec![(n.clone(), (**p).clone())],
        IrType::Union(ms) => ms.iter().flat_map(tags_of_typed).collect(),
        _ => Vec::new(),
    }
}

fn has_param(t: &IrType) -> bool {
    let mut found = false;
    map_ty(t, &mut |x| {
        if matches!(x, IrType::Param(_)) {
            found = true;
        }
        None
    });
    found
}

/// Rebuilds `t` bottom-up: `f` may replace a node (returning `Some`), else the node is kept with
/// its children mapped.
fn map_ty(t: &IrType, f: &mut dyn FnMut(&IrType) -> Option<IrType>) -> IrType {
    if let Some(r) = f(t) {
        return r;
    }
    let mut go = |x: &IrType| map_ty(x, f);
    match t {
        IrType::Optional(a) => IrType::Optional(Box::new(go(a))),
        IrType::Vector(a) => IrType::Vector(Box::new(go(a))),
        IrType::Record(fs) => IrType::Record(fs.iter().map(|(n, x)| (n.clone(), go(x))).collect()),
        IrType::Tag(n, p) => IrType::Tag(n.clone(), Box::new(go(p))),
        IrType::Union(ms) => IrType::Union(ms.iter().map(&mut go).collect()),
        IrType::Tuple(ms) => IrType::Tuple(ms.iter().map(&mut go).collect()),
        IrType::Function(ps, r) => IrType::Function(ps.iter().map(&mut go).collect(), Box::new(go(r))),
        IrType::Named(n, a) => IrType::Named(n.clone(), a.iter().map(&mut go).collect()),
        other => other.clone(),
    }
}

/// Each `Any` is an independent implicit type parameter (RFC §2, *`Any`*).
fn anyfy(t: &IrType, n: &mut usize, interfaces: &HashSet<String>) -> IrType {
    map_ty(t, &mut |x| {
        let opaque = match x {
            IrType::Any => true,
            IrType::Named(name, args) => args.is_empty() && interfaces.contains(name),
            _ => false,
        };
        if opaque {
            *n += 1;
            Some(IrType::Param(format!("__any{}", *n - 1)))
        } else {
            None
        }
    })
}

fn subst_ty(t: &IrType, sub: &HashMap<String, IrType>) -> IrType {
    map_ty(t, &mut |x| match x {
        IrType::Param(n) => sub.get(n).cloned(),
        _ => None,
    })
}

/// Binds the parameters of `pat` so that it matches `conc`. Never overwrites a binding, and
/// ignores what the checker did not type.
fn unify(pat: &IrType, conc: &IrType, out: &mut HashMap<String, IrType>) {
    use IrType as T;
    match (pat, conc) {
        (_, T::Unknown) => {}
        (T::Param(n), c) => {
            // A parameter in `conc` is one the checker left unresolved: nothing to learn.
            if !has_param(c) {
                out.entry(n.clone()).or_insert_with(|| c.clone());
            }
        }
        (T::Optional(p), T::Optional(c)) => unify(p, c, out),
        (T::Optional(p), c) => unify(p, c, out),
        (T::Vector(p), T::Vector(c)) => unify(p, c, out),
        (T::Record(p), T::Record(c)) if p.len() == c.len() => {
            for ((pn, pt), (cn, ct)) in p.iter().zip(c) {
                if pn == cn {
                    unify(pt, ct, out);
                }
            }
        }
        (T::Tag(pn, p), T::Tag(cn, c)) if pn == cn => unify(p, c, out),
        (T::Union(p), T::Union(c)) | (T::Tuple(p), T::Tuple(c)) if p.len() == c.len() => {
            p.iter().zip(c).for_each(|(a, b)| unify(a, b, out));
        }
        (T::Function(pp, pr), T::Function(cp, cr)) if pp.len() == cp.len() => {
            pp.iter().zip(cp).for_each(|(a, b)| unify(a, b, out));
            unify(pr, cr, out);
        }
        (T::Named(pn, p), T::Named(cn, c)) if pn == cn && p.len() == c.len() => {
            p.iter().zip(c).for_each(|(a, b)| unify(a, b, out));
        }
        _ => {}
    }
}

/// Reads the body's rigid type variables off the places where a parameter is used.
fn derive_rigids(e: &Expr, env: &HashMap<String, IrType>, out: &mut HashMap<String, IrType>) {
    if let ExprKind::Var(n) = &e.kind {
        if let Some(t) = env.get(n) {
            unify(&e.ty, t, out);
        }
    }
    e.for_each_child(&mut |c| derive_rigids(c, env, out));
}

fn subst_expr(e: &mut Expr, sub: &HashMap<String, IrType>) {
    e.ty = subst_ty(&e.ty, sub);
    match &mut e.kind {
        ExprKind::Fn { params, ret, .. } => {
            params.iter_mut().for_each(|p| p.ty = subst_ty(&p.ty, sub));
            *ret = subst_ty(ret, sub);
        }
        ExprKind::Match { arms, .. } => {
            for a in arms {
                if let Pattern::Type { ty, .. } = &mut a.pattern {
                    *ty = subst_ty(ty, sub);
                }
            }
        }
        _ => {}
    }
    e.for_each_child_mut(&mut |c| subst_expr(c, sub));
}

fn is_statement(e: &Expr) -> bool {
    matches!(
        e.kind,
        ExprKind::Let { .. } | ExprKind::While { .. } | ExprKind::Loop(_) | ExprKind::For { .. } | ExprKind::TypeDef { .. }
    )
}

fn op_of(name: &str) -> Option<BinOp> {
    Some(match name.trim_matches('`') {
        "+" => BinOp::Add,
        "-" => BinOp::Sub,
        "*" => BinOp::Mul,
        "/" => BinOp::Div,
        "%" | "%%" => BinOp::Mod,
        "==" => BinOp::Eq,
        "!=" => BinOp::NotEq,
        "<" => BinOp::Lt,
        ">" => BinOp::Gt,
        "<=" => BinOp::Le,
        ">=" => BinOp::Ge,
        "&" => BinOp::And,
        "&&" => BinOp::AndAnd,
        "|" => BinOp::Or,
        "||" => BinOp::OrOr,
        _ => return None,
    })
}

fn indent(s: &str) -> String {
    s.lines().map(|l| if l.is_empty() { String::new() } else { format!("    {l}") }).collect::<Vec<_>>().join("\n")
}

/// The Rust for `l <op> r` on two scalars. `int` `+ - *` are checked: overflow is an error.
fn op_text(op: BinOp, both_int: bool, l: &str, r: &str) -> String {
    match op {
        BinOp::Add if both_int => format!("typr_rt::add_i({l}, {r})"),
        BinOp::Sub if both_int => format!("typr_rt::sub_i({l}, {r})"),
        BinOp::Mul if both_int => format!("typr_rt::mul_i({l}, {r})"),
        _ => {
            let sym = match op {
                BinOp::And => "&",
                BinOp::Or => "|",
                other => return format!("({l} {} {r})", other),
            };
            format!("({l} {sym} {r})")
        }
    }
}

fn elem_of(t: &IrType) -> IrType {
    match t {
        IrType::Vector(e) => (**e).clone(),
        other => other.clone(),
    }
}

/// The closure that converts an element of type `have` into `want` (`int` into `num`, `T` into
/// `T | na`), or `None` when they already agree.
fn elem_conv(want: &IrType, have: &IrType) -> Option<String> {
    match (want, have) {
        (IrType::Num, IrType::Int) => Some("|x| x as f64".into()),
        (IrType::Optional(_), IrType::Optional(_) | IrType::Na | IrType::Unknown) => None,
        (IrType::Optional(inner), h) => match elem_conv(inner, h) {
            Some(f) => Some(format!("|x| Some(({f})(x))")),
            None => Some("Some".into()),
        },
        _ => None,
    }
}

#[cfg(test)]
mod runtime_tests {
    //! The runtime is plain Rust that the backend inlines into every generated `main.rs`; it is
    //! compiled here too so its printing and recycling are tested without a `cargo` round trip.
    #[allow(dead_code)]
    mod rt {
        include!("runtime.rs");
    }
    use rt::*;

    fn v<T>(xs: Vec<T>) -> Vector<T> {
        Vector::new(xs)
    }

    #[test]
    fn integer_vectors_print_like_r() {
        assert_eq!(v(vec![1, 2, 3]).show(), "[1] 1 2 3");
        assert_eq!(v(vec![5, 10, 100]).show(), "[1]   5  10 100");
        assert_eq!(v(Vec::<i32>::new()).show(), "integer(0)");
    }

    #[test]
    fn numeric_vectors_share_their_decimals() {
        assert_eq!(v(vec![1.5, 2.0, 3.25]).show(), "[1] 1.50 2.00 3.25");
        assert_eq!(v(vec![1.0, 2.0]).show(), "[1] 1 2");
        assert_eq!(v(vec![0.1 + 0.2]).show(), "[1] 0.3");
        assert_eq!(v(vec![f64::NAN, 1.0, f64::INFINITY]).show(), "[1] NaN   1 Inf");
        assert_eq!(12.56636_f64.show(), "[1] 12.56636");
    }

    #[test]
    fn character_vectors_are_quoted_and_left_aligned() {
        assert_eq!(v(vec!["a".to_string(), "bcd".to_string()]).show(), "[1] \"a\"   \"bcd\"");
    }

    #[test]
    fn missing_values_print_as_na_and_align_with_the_rest() {
        assert_eq!(v(vec![Some(1), None, Some(30)]).show(), "[1]  1 NA 30");
        assert_eq!(v(vec![Some(1.5), None]).show(), "[1] 1.5  NA");
    }

    #[test]
    fn long_vectors_wrap_at_80_columns_with_index_labels() {
        let shown = rt::seq(1, 30, 1).show();
        assert_eq!(
            shown,
            " [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25\n[26] 26 27 28 29 30"
        );
    }

    #[test]
    fn recycling_follows_rs_rule() {
        let a = v(vec![1, 2, 3, 4]);
        assert_eq!(zip_with(&a, &scalar(10), |x, y| x + y).to_vec(), [11, 12, 13, 14]);
        assert_eq!(zip_with(&a, &v(vec![1, 2]), |x, y| x * y).to_vec(), [1, 4, 3, 8]);
        assert!(zip_with(&a, &v(Vec::<i32>::new()), |x, y| x + y).len() == 0);
    }

    #[test]
    fn indexing_is_one_based_and_checked() {
        let a = v(vec![10, 20, 30]);
        assert_eq!(a.at(1), 10);
        assert_eq!(a.select(&v(vec![3, 1])).to_vec(), [30, 10]);
        assert!(std::panic::catch_unwind(|| a.at(0)).is_err());
        assert!(std::panic::catch_unwind(|| a.at(4)).is_err());
    }

    #[test]
    fn vectors_copy_on_modify_and_states_share() {
        let a = v(vec![1, 2]);
        let b = a.clone();
        assert_eq!(a, b);
        let s = state(1);
        let t = s.clone();
        set(t, 7);
        update(s.clone(), std::rc::Rc::new(|x| x + 1));
        assert_eq!((get(s.clone()), version(s)), (8, 2));
    }

    #[test]
    fn dyn_prints_what_it_holds_and_narrows_back() {
        let d = Dyn::new(v(vec![1, 2]));
        assert_eq!(d.show(), "[1] 1 2");
        assert_eq!(d.downcast::<Vector<i32>>("[int]").to_vec(), [1, 2]);
        assert!(std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| d.downcast::<String>("char"))).is_err());
        assert_eq!(Dyn::new(1), Dyn::new(1));
    }

    #[test]
    fn seq_goes_both_ways() {
        assert_eq!(seq(1, 3, 1).to_vec(), [1, 2, 3]);
        assert_eq!(seq(3, 1, -1).to_vec(), [3, 2, 1]);
        assert!(seq(3, 1, 1).len() == 0);
    }
}

#[cfg(test)]
mod platform_tests {
    //! Step 6: what each platform adds to the emitted crate, and the T054 boundary check.
    use super::*;
    use crate::components::context::Context;
    use crate::processes::ir;
    use crate::processes::parsing::parse_from_string;

    fn lower(src: &str) -> Program {
        let lang = parse_from_string(src, "platform_test");
        let (program, errors) = ir::check_and_lower(&Context::default(), &lang);
        assert!(errors.is_empty(), "{errors:?}");
        program
    }

    fn build(src: &str, platform: Platform) -> Output {
        emit_with(&lower(src), &Options { platform, module_name: "mylib".into() }).expect("emits")
    }

    const API: &str = "@pub let area <- fn(r: num): num { 3.0 * r };\n@pub let nth <- fn(xs: [#N, int]): int | na { NA };\nlet hidden <- fn(): int { 1 };\n";

    #[test]
    fn a_library_has_no_entry_point() {
        let out = build(API, Platform::Lib);
        assert!(!out.source.contains("fn main()"));
        assert!(out.source.contains("pub fn area("));
        assert!(out.r_wrappers.is_none());
    }

    #[test]
    fn wasm_exports_pub_scalar_functions_by_name() {
        let src = build(API, Platform::Wasm).source;
        assert!(src.contains("#[export_name = \"area\"]"), "{src}");
        // `nth` takes a vector: it cannot cross a C ABI.
        assert!(!src.contains("export_name = \"nth\""), "{src}");
        assert!(!src.contains("export_name = \"hidden\""), "{src}");
    }

    #[test]
    fn extendr_wraps_exports_and_writes_the_r_side() {
        let out = build(API, Platform::Extendr);
        assert!(out.source.contains("#[extendr]\nfn typr_export_area(r: f64) -> f64"), "{}", out.source);
        assert!(out.source.contains("fn typr_export_nth(xs: Vec<i32>) -> Option<i32> { nth(typr_rt::Vector::new(xs)) }"), "{}", out.source);
        assert!(out.source.contains("mod mylib;"), "{}", out.source);
        assert!(!out.source.contains("typr_export_hidden"));
        let r = out.r_wrappers.unwrap();
        assert!(r.contains("area <- function(r) .Call(wrap__typr_export_area, r)"), "{r}");
        assert!(r.contains("@useDynLib mylib, .registration = TRUE"), "{r}");
    }

    #[test]
    fn boundary_check_accepts_scalars_vectors_and_records() {
        let p = lower("type Pt <- list { x: int, y: char };\n@pub let f <- fn(p: Pt, v: [#N, num]): num | na { NA };\n@pub let g <- fn(): Empty { print(1) };\n");
        assert!(boundary_errors(&p).is_empty());
    }

    #[test]
    fn boundary_check_rejects_unions_and_closures_with_t054() {
        let p = lower("type Sh <- .Left(num) | .Right(num);\n@pub let f <- fn(s: Sh): num { 1.0 };\n@pub let g <- fn(k: (int) -> int): int { 1 };\nlet private <- fn(s: Sh): num { 1.0 };\n");
        let errors = boundary_errors(&p);
        assert_eq!(errors.len(), 2, "{errors:?}");
        let text: Vec<String> = errors.iter().map(|e| format!("{e:?}")).collect();
        assert!(text[0].contains("NotExportable") && text[0].contains("union"), "{}", text[0]);
        assert!(text[1].contains("closure"), "{}", text[1]);
    }
}
