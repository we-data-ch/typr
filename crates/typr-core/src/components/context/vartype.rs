#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use crate::components::context::config::Config;
use crate::components::context::config::TargetLanguage;
use crate::components::context::Context;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::alias_type::Alias;
use crate::components::r#type::type_category::TypeCategory;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::parsing::type_token::TypeToken;
use crate::utils::builder;
use countmap::CountMap;
use indexmap::IndexSet;
use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use std::ops::Add;
use std::sync::Arc;
use std::sync::OnceLock;

#[cfg(not(target_arch = "wasm32"))]
use std::fs::File;
#[cfg(not(target_arch = "wasm32"))]
use std::io::Read;
#[cfg(not(target_arch = "wasm32"))]
use std::io::Write;

pub fn same_var_type(element1: &(Var, Type), element2: &(Var, Type)) -> bool {
    (element1.0.get_name() == element2.0.get_name())
        && (element1.0.get_type() == element2.0.get_type())
}

/// True for names shaped like the ones `push_alias_increment` generates:
/// a `TypeCategory` display prefix followed by a counter (`Array0`,
/// `Record2`, `Function15`, …). Used to tell auto-registered structural
/// types apart from user-declared aliases when hoisting out of an inner
/// scope. A user alias that happens to match (`Vec2`) is also hoisted —
/// harmless, it only widens where that name resolves.
pub fn is_generated_alias_name(name: &str) -> bool {
    let prefix = name.trim_end_matches(|c: char| c.is_ascii_digit());
    prefix.len() < name.len()
        && !prefix.is_empty()
        && prefix.chars().all(|c| c.is_ascii_alphabetic())
}

pub fn merge_variables(
    set1: IndexSet<(Var, Type)>,
    set2: IndexSet<(Var, Type)>,
) -> IndexSet<(Var, Type)> {
    let mut result = IndexSet::new();

    for elem2 in &set2 {
        let mut replaced = false;

        for elem1 in &set1 {
            if same_var_type(elem1, elem2) {
                result.insert(elem2.clone());
                replaced = true;
                break;
            }
        }

        if !replaced {
            result.insert(elem2.clone());
        }
    }

    for elem1 in &set1 {
        let mut should_keep = true;

        for elem2 in &set2 {
            if same_var_type(elem1, elem2) {
                should_keep = false;
                break;
            }
        }

        if should_keep {
            result.insert(elem1.clone());
        }
    }

    result
}

/// Lazily-built index of `VarType::variables` keyed by name, so name-keyed
/// lookups (`get_type_from_variable`, `get_types_from_name`,
/// `get_functions_from_name`) don't linear-scan the whole (stdlib-heavy,
/// ~1700-entry) set on every call. `Arc<OnceLock<_>>` rather than
/// `Rc<RefCell<_>>` so `VarType`/`Context` stay `Send + Sync` (`Context`
/// crosses the multi-thread tokio runtime in typr-lsp). Every mutator of
/// `variables` must reset this to a fresh empty cell instead of carrying it
/// forward via `..self` — see `VarType::entries_named`.
type NameIndex = Arc<OnceLock<HashMap<String, Vec<(Var, Type)>>>>;

/// `variables`/`aliases`/`std` are the stdlib-heavy sets (~1700 entries once
/// R's standard library is loaded) that used to be deep-cloned on every
/// `Context::clone()` — 92 call sites in `type_checking/mod.rs` alone, one
/// per AST node in the worst case. Wrapping them in `Arc` turns
/// `VarType::clone()` (and so `Context::clone()`) into a handful of refcount
/// bumps instead of an O(n) copy. Every mutator gets an owned `IndexSet` back
/// via `Arc::unwrap_or_clone`/`Arc::make_mut`, which only actually clones the
/// underlying set when another live `Arc` still points at it (i.e. some
/// earlier `.clone()` is still alive) — the common case of a context moved
/// straight through a chain of `fold`/`pipe` calls pays zero copies.
type SharedVarSet = Arc<IndexSet<(Var, Type)>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarType {
    pub variables: SharedVarSet,
    pub aliases: SharedVarSet,
    pub std: SharedVarSet,
    #[serde(skip)]
    pub alias_counter: CountMap<TypeCategory, usize>,
    #[serde(skip)]
    name_index: NameIndex,
}

impl PartialEq for VarType {
    fn eq(&self, other: &Self) -> bool {
        self.variables == other.variables && self.aliases == other.aliases && self.std == other.std
    }
}

/// Buckets `variables` by name, preserving `variables()`'s existing
/// `.iter().rev()` order within each bucket — `get_type_from_variable`'s
/// `reduce(...)` over same-name candidates picks the first-seen one when two
/// matches are mutually non-subtypes, so shadowing (most-recently-pushed wins)
/// depends on this order being preserved exactly.
fn build_name_index(variables: &IndexSet<(Var, Type)>) -> HashMap<String, Vec<(Var, Type)>> {
    let mut index: HashMap<String, Vec<(Var, Type)>> = HashMap::new();
    for pair in variables.iter().rev() {
        index
            .entry(pair.0.get_name())
            .or_default()
            .push(pair.clone());
    }
    index
}

//main
impl VarType {
    pub fn new() -> VarType {
        let var = Var::from("Generic").set_type(builder::params_type());
        let typ = builder::generic_type();
        let mut aliases = IndexSet::new();
        aliases.insert((var, typ));
        VarType {
            variables: Arc::new(IndexSet::new()),
            aliases: Arc::new(aliases),
            std: Arc::new(IndexSet::new()),
            alias_counter: CountMap::new(),
            name_index: Arc::new(OnceLock::new()),
        }
    }

    /// Entries in `variables` whose name is `name`, in the same order
    /// `variables().filter(|(v, _)| v.get_name() == name)` would yield
    /// (i.e. most-recently-pushed first). O(1) amortized via a lazily-built,
    /// per-mutation-invalidated index — see `name_index`.
    pub fn entries_named(&self, name: &str) -> Vec<(Var, Type)> {
        self.name_index
            .get_or_init(|| build_name_index(&self.variables))
            .get(name)
            .cloned()
            .unwrap_or_default()
    }

    pub fn push_interface(
        self,
        var: Var,
        typ: Type,
        original_type: Type,
        context: &Context,
    ) -> VarType {
        match typ {
            Type::Interface(args, _) => {
                let alias = match original_type.clone() {
                    Type::Alias(name, params, _, h) => {
                        Alias::new(format!("{}_", name), params, true, h).to_type()
                    }
                    Type::Interface(_, _) => Alias::default().set_opacity(true).to_type(),
                    _ => Alias::default().set_opacity(true).to_type(),
                };
                args.iter()
                    .map(|arg_typ| {
                        (
                            arg_typ.clone().to_var(context),
                            arg_typ.get_type().replace_function_types(
                                builder::self_generic_type(),
                                alias.clone(),
                            ),
                        )
                    })
                    .fold(self, |acc, x| acc.push_var_type(&[x]))
                    .push_var_type(&[(var, alias)])
            }
            _ => self,
        }
    }

    pub fn from_config(config: Config) -> VarType {
        let vartype = VarType::new();
        match config.target_language {
            TargetLanguage::R => vartype.load_r().unwrap().load_typed_r().unwrap(),
            TargetLanguage::JS => vartype.load_js().unwrap().load_typed_js().unwrap(),
        }
    }

    pub fn variables(&self) -> impl Iterator<Item = &(Var, Type)> + '_ {
        self.variables.iter().rev()
    }

    pub fn aliases(&self) -> impl Iterator<Item = &(Var, Type)> + '_ {
        self.aliases.iter().rev()
    }

    pub fn get_types(&self) -> IndexSet<Type> {
        self.variables
            .iter()
            .chain(self.aliases.iter())
            .flat_map(|(_var, typ)| typ.clone().extract_types())
            .collect()
    }

    pub fn push_var_type(self, vt: &[(Var, Type)]) -> Self {
        let (var, ali) = Self::separate_variables_aliases(vt.to_vec());
        let ali = ali.iter().cloned().collect::<Vec<_>>();
        self.push_variables(var).push_aliases(&ali)
    }

    pub fn replace_or_push_var_type(self, vt: &[(Var, Type)]) -> Self {
        let (var, ali) = Self::separate_variables_aliases(vt.to_vec());
        let ali = ali.iter().cloned().collect::<Vec<_>>();
        self.replace_or_push_variables(var).push_aliases(&ali)
    }

    pub fn push_alias_increment(self, vt: (TypeCategory, Type)) -> Self {
        let (category, typ) = vt;
        match category {
            TypeCategory::Generic
            | TypeCategory::GenericKinded(_)
            | TypeCategory::Char
            | TypeCategory::Integer
            | TypeCategory::Alias
            | TypeCategory::Any
            | TypeCategory::RFunction => self,
            _ => {
                let count = self.alias_counter.get_count(&category).unwrap_or(0);
                let name = format!("{}{}", category, count);
                let var = Var::from_name(&name).set_type(builder::params_type());
                let mut new_counter = self.alias_counter.clone();
                new_counter.insert_or_increment(category);
                let mut result = self;
                result.alias_counter = new_counter;
                result.push_aliases(&[(var, typ)])
            }
        }
    }

    pub fn exists(&self, typ: &Type) -> bool {
        self.aliases.iter().any(|(_, typ2)| typ == typ2) || typ.is_primitive()
    }

    fn push_type_if_not_exists(self, typ: Type) -> Self {
        if !self.exists(&typ) {
            self.push_alias_increment((typ.to_category(), typ))
        } else {
            self
        }
    }

    pub fn push_types(self, types: &[Type]) -> Self {
        types.iter().fold(self, |vartyp, typ| {
            vartyp.push_type_if_not_exists(typ.clone())
        })
    }

    /// Carries auto-generated structural type registrations (`Array0`,
    /// `Record2`, `Function5`, … — created by `push_alias_increment` for
    /// anonymous array/record/function types, e.g. from an inline
    /// `expr as! [T]` cast or a signature's parameter type) made in an inner
    /// scope (function body, module body) back into this outer context. The
    /// transpiler resolves them here to emit `|> as.ArrayN()` annotations,
    /// S3 method suffixes and the `types.R` entries — R's S3 class registry
    /// is whole-program, so these must not stay scoped. The inner alias
    /// counter is adopted too (it only ever advances) so later registrations
    /// can't reuse a hoisted name for a different type. User-declared aliases
    /// and variables stay scoped to the inner context.
    pub fn hoist_aliases(self, inner: &VarType) -> Self {
        let new_aliases = self.hoisted_alias_pairs(inner);
        let mut result = self.push_aliases(&new_aliases);
        result.alias_counter = inner.alias_counter.clone();
        result
    }

    /// The alias pairs `hoist_aliases` would carry over from `inner`.
    pub fn hoisted_alias_pairs(&self, inner: &VarType) -> Vec<(Var, Type)> {
        inner
            .aliases
            .iter()
            .filter(|pair| !self.aliases.contains(*pair))
            .filter(|(var, _)| is_generated_alias_name(&var.get_name()))
            .cloned()
            .collect()
    }

    pub fn separate_variables_aliases(
        val: Vec<(Var, Type)>,
    ) -> (IndexSet<(Var, Type)>, IndexSet<(Var, Type)>) {
        let variables = val
            .iter()
            .filter(|(var, _)| var.is_variable())
            .cloned()
            .collect::<IndexSet<(Var, Type)>>();
        let aliases = val
            .iter()
            .filter(|(var, _)| var.is_alias())
            .cloned()
            .collect::<IndexSet<(Var, Type)>>();
        (variables, aliases)
    }

    fn push_variables(self, vt: IndexSet<(Var, Type)>) -> Self {
        let VarType {
            mut variables,
            aliases,
            std,
            alias_counter,
            ..
        } = self;
        {
            let set = Arc::make_mut(&mut variables);
            for pair in vt {
                set.insert(pair);
            }
        }
        VarType {
            variables,
            aliases,
            std,
            alias_counter,
            name_index: Arc::new(OnceLock::new()),
        }
    }

    fn replace_or_push_variables(self, vt: IndexSet<(Var, Type)>) -> Self {
        let VarType {
            variables,
            aliases,
            std,
            alias_counter,
            ..
        } = self;
        let res = merge_variables(Arc::unwrap_or_clone(variables), vt);
        VarType {
            variables: Arc::new(res),
            aliases,
            std,
            alias_counter,
            name_index: Arc::new(OnceLock::new()),
        }
    }

    fn push_aliases(self, vt: &[(Var, Type)]) -> Self {
        let VarType {
            variables,
            mut aliases,
            std,
            alias_counter,
            name_index,
        } = self;
        {
            let set = Arc::make_mut(&mut aliases);
            for pair in vt.iter().cloned() {
                set.insert(pair);
            }
        }
        VarType {
            variables,
            aliases,
            std,
            alias_counter,
            name_index,
        }
    }

    fn replace_aliases(self, vt: &[(Var, Type)]) -> Self {
        let vt_set: IndexSet<(Var, Type)> = vt.iter().cloned().collect();
        let res = merge_variables(Arc::unwrap_or_clone(self.variables.clone()), vt_set);
        VarType {
            aliases: Arc::new(res),
            ..self
        }
    }

    pub fn get_class(&self, t: &Type) -> String {
        let res = match t {
            Type::Integer(_, _) => "integer".to_string(),
            Type::Char(_, _) => "character".to_string(),
            Type::Boolean(_, _) => "logical".to_string(),
            Type::Number(_, _) => "numeric".to_string(),
            Type::Any(_) => "Any".to_string(),
            // Unresolved type variables (`T`, `%T`, `#N`, `$L`, …) have no
            // fixed runtime class. R's `UseMethod` always falls back to
            // `<generic>.default` when no more specific class matches, so
            // that's the only suffix that actually dispatches at runtime —
            // unlike a literal `"Generic"` class, which no constructed value
            // (record types in particular) ever carries in its class chain.
            _ => self
                .aliases
                .iter()
                .find(|(_, typ)| typ == t)
                .map(|(var, _)| var.get_name())
                .unwrap_or("default".to_string()),
        };
        "'".to_string() + &res + "'"
    }

    /// Does `name` resolve (through zero or more single-parameter alias
    /// hops) to the stdlib `Foreign<T>` alias (`opaque Foreign<T> <- Any;`,
    /// `configs/std/foreign.ty`)? Foreign-family aliases (e.g. `type
    /// DataFrame <- Foreign<Any>;`) name genuinely external R values (S4/R6/
    /// RC instances, third-party S3 objects, …) that TypR cannot and must
    /// not touch — see `is_foreign_alias`'s caller in `get_type_anotation`.
    pub fn resolves_to_foreign(&self, name: &str) -> bool {
        let mut current = name.to_string();
        for _ in 0..16 {
            if current == "Foreign" {
                return true;
            }
            match self.aliases.iter().find(|(v, _)| v.get_name() == current) {
                Some((_, Type::Alias(target_name, _, _, _))) => {
                    current = target_name.clone();
                }
                _ => return false,
            }
        }
        false
    }

    pub fn get_type_anotation(&self, t: &Type) -> String {
        let res = match t {
            Type::Boolean(_, _) => "as.Boolean".to_string(),
            Type::Integer(_, _) => "as.Integer".to_string(),
            Type::Number(_, _) => "as.Number".to_string(),
            Type::Char(_, _) => "as.Character".to_string(),
            Type::Vec(vtype, _, _, _) if vtype.is_vector() => "".to_string(),
            // `as.LmModel(x)` (`x |> struct(c('LmModel', ...))`) appends
            // classes onto the runtime value's class vector. For a genuine
            // foreign value that's actively harmful, not just redundant: R
            // silently drops S4-ness the moment `class(x) <-` is assigned a
            // multi-element vector (soundness_transpilation.md Phase D,
            // found instrumenting `type LmModel <- Foreign<Any>;` — an `@`
            // slot access on the annotated value failed with "no applicable
            // method for `@`" after going through exactly this cast).
            // `identity()` is the real base-R passthrough.
            Type::Alias(name, _, _, _) if self.resolves_to_foreign(name) => "identity".to_string(),
            Type::Alias(name, _, _, _) => format!("as.{}", name),
            _ => self
                .aliases
                .iter()
                .find(|(_, typ)| typ == t)
                .map(|(var, _)| format!("as.{}", var.get_name()))
                .unwrap_or("as.Generic".to_string()),
        };
        format!("{}()", res)
    }

    pub fn get_type_anotation_no_parentheses(&self, t: &Type) -> String {
        match t {
            Type::Boolean(_, _) => "logical".to_string(),
            Type::Integer(_, _) => "integer".to_string(),
            Type::Number(_, _) => "number".to_string(),
            Type::Char(_, _) => "character".to_string(),
            Type::Alias(name, _, _, _) => name.to_string(),
            _ => self
                .aliases
                .iter()
                .find(|(_, typ)| typ == t)
                .map(|(var, _)| var.get_name())
                .unwrap_or("Generic".to_string()),
        }
    }

    pub fn get_class_unquoted(&self, t: &Type) -> String {
        match t {
            Type::Integer(_, _) => "integer".to_string(),
            Type::Char(_, _) => "character".to_string(),
            Type::Boolean(_, _) => "logical".to_string(),
            Type::Number(_, _) => "numeric".to_string(),
            Type::Any(_) => "Any".to_string(),
            // Same rationale as `get_class`'s fallback above: "default" is
            // the only suffix `UseMethod` actually finds for these.
            _ => self
                .aliases
                .iter()
                .find(|(_, typ)| typ == t)
                .map(|(var, _)| var.get_name())
                .unwrap_or("default".to_string()),
        }
    }

    pub fn get_type_from_class(&self, class: &str) -> Type {
        self.aliases
            .iter()
            .find(|(var, _)| var.get_name() == class)
            .map(|(_, typ)| typ)
            .unwrap_or_else(|| {
                panic!(
                    "{} isn't an existing Alias name (don't know where it come from)",
                    class
                )
            })
            .clone()
    }

    fn in_aliases(&self, alias_name: &str) -> bool {
        self.aliases
            .iter()
            .any(|(var, _)| var.get_name() == alias_name)
    }

    pub fn push_alias(self, alias_name: String, typ: Type) -> Self {
        let var = Var::from_name(&alias_name).set_type(builder::params_type());
        if self.in_aliases(&alias_name) {
            return self;
        }
        let VarType {
            variables,
            mut aliases,
            std,
            alias_counter,
            name_index,
        } = self;
        Arc::make_mut(&mut aliases).insert((var, typ));
        Self {
            variables,
            aliases,
            std,
            alias_counter,
            name_index,
        }
    }

    pub fn push_alias2(self, var: Var, typ: Type) -> Self {
        if self.in_aliases(&var.get_name()) {
            return self;
        }
        let VarType {
            variables,
            mut aliases,
            std,
            alias_counter,
            name_index,
        } = self;
        Arc::make_mut(&mut aliases).insert((var, typ));
        Self {
            variables,
            aliases,
            std,
            alias_counter,
            name_index,
        }
    }

    pub fn get_aliases(&self) -> String {
        let mut aliases_vec: Vec<_> = self.aliases.iter().collect();
        aliases_vec.sort_by_key(|(var, _)| var.get_name());
        aliases_vec
            .iter()
            .map(|(var, typ)| format!("{} = {}", var.get_name(), typ.pretty()))
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn variable_exist(&self, var: Var, context: &Context) -> Option<Var> {
        self.variables
            .iter()
            .find(|(v, _)| v.match_with(&var, context))
            .map(|(v, _)| v.clone())
    }

    pub fn update_variable(self, var: Var) -> Self {
        let old_var = self
            .variables
            .iter()
            .find(|(v, _)| v.get_name() == var.get_name())
            .expect("Variable not found")
            .clone();

        let VarType {
            variables,
            aliases,
            std,
            alias_counter,
            ..
        } = self;
        // `IndexSet::remove` is deprecated because it disrupts set order.
        // Use `retain` to remove the old variable in place while preserving order.
        let mut new_variables = Arc::unwrap_or_clone(variables);
        new_variables.retain(|x| x != &old_var);
        new_variables.insert((var.clone(), var.get_type()));

        Self {
            variables: Arc::new(new_variables),
            aliases,
            std,
            alias_counter,
            name_index: Arc::new(OnceLock::new()),
        }
    }

    pub fn name_exists_outside_of_std(&self, name: &str) -> bool {
        self.variables
            .iter()
            .filter(|(var, _)| var.get_name() == name)
            .filter(|(var, typ)| !(var.get_type().is_any() && typ.is_unknown_function()))
            .collect::<Vec<_>>()
            .is_empty()
    }

    pub fn remove_vars(self, vars: &[Var]) -> Self {
        vars.iter().fold(self, |acc, x| acc.remove_var(x))
    }

    pub fn remove_var(self, var: &Var) -> Self {
        let VarType {
            variables,
            aliases,
            std,
            alias_counter,
            ..
        } = self;
        let mut new_variables = Arc::unwrap_or_clone(variables);
        new_variables.retain(|(var2, _)| var != var2);
        Self {
            variables: Arc::new(new_variables),
            aliases,
            std,
            alias_counter,
            name_index: Arc::new(OnceLock::new()),
        }
    }

    pub fn set_default_var_types(self) -> Self {
        let mut vars = IndexSet::new();
        vars.insert((Var::from("add"), "(T, T) -> T".parse::<Type>().unwrap()));
        vars.insert((Var::from("minus"), "(T, T) -> T".parse::<Type>().unwrap()));
        vars.insert((Var::from("mul"), "(T, T) -> T".parse::<Type>().unwrap()));
        vars.insert((Var::from("div"), "(T, T) -> T".parse::<Type>().unwrap()));
        self.push_variables(vars)
    }

    pub fn get_related_functions(&self, typ: &Type) -> Vec<Var> {
        todo!();
    }

    pub fn set_js_var_types(self) -> Self {
        let mut vars = IndexSet::new();
        vars.insert((Var::alias("Document", &[]), builder::opaque_type("Doc")));
        self.set_default_var_types().push_variables(vars)
    }

    pub fn set_r_var_types(self) -> Self {
        self.set_default_var_types()
    }

    pub fn source(self, target_language: TargetLanguage) -> Self {
        match target_language {
            TargetLanguage::JS => self.set_js_var_types(),
            TargetLanguage::R => self.set_r_var_types(),
        }
    }

    pub fn set_std(self, v: Vec<(Var, Type)>) -> Self {
        Self {
            std: Arc::new(v.into_iter().collect()),
            ..self
        }
    }

    /// Save to a file (only available in native mode)
    #[cfg(not(target_arch = "wasm32"))]
    pub fn save(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let binary_data = bincode::serialize(self)?;
        let mut file = File::create(path)?;
        file.write_all(&binary_data)?;
        Ok(())
    }

    /// Stub for WASM mode
    #[cfg(target_arch = "wasm32")]
    pub fn save(&self, _path: &str) -> Result<(), Box<dyn std::error::Error>> {
        Err("File saving not supported in WASM mode".into())
    }

    /// Load from a file path (only available in native mode)
    #[cfg(not(target_arch = "wasm32"))]
    pub fn load(self, path: &str) -> Result<VarType, Box<dyn std::error::Error>> {
        let mut file = File::open(path)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        let var_type: VarType = bincode::deserialize(&buffer)?;
        Ok(self + var_type)
    }

    /// Stub for WASM mode
    #[cfg(target_arch = "wasm32")]
    pub fn load(self, _path: &str) -> Result<VarType, Box<dyn std::error::Error>> {
        Err("File loading not supported in WASM mode".into())
    }

    /// Load R standard library (embedded at compile time)
    pub fn load_r(self) -> Result<VarType, Box<dyn std::error::Error>> {
        let buffer = include_bytes!("../../../configs/bin/.std_r.bin");
        let var_type: VarType = bincode::deserialize(buffer)?;
        Ok(self + var_type)
    }

    /// Load typed R standard library (embedded at compile time)
    pub fn load_typed_r(self) -> Result<VarType, Box<dyn std::error::Error>> {
        let buffer = include_bytes!("../../../configs/bin/.std_r_typed.bin");
        let var_type: VarType = bincode::deserialize(buffer)?;
        Ok(self + var_type)
    }

    /// Load JS standard library (embedded at compile time)
    pub fn load_js(self) -> Result<VarType, Box<dyn std::error::Error>> {
        let buffer = include_bytes!("../../../configs/bin/.std_js.bin");
        let var_type: VarType = bincode::deserialize(buffer)?;
        Ok(self + var_type)
    }

    /// Load typed JS standard library (embedded at compile time)
    pub fn load_typed_js(self) -> Result<VarType, Box<dyn std::error::Error>> {
        let buffer = include_bytes!("../../../configs/bin/.std_js_typed.bin");
        let var_type: VarType = bincode::deserialize(buffer)?;
        Ok(self + var_type)
    }

    /// Load from file (only available in native mode)
    #[cfg(not(target_arch = "wasm32"))]
    pub fn from_file(path: &str) -> Result<VarType, Box<dyn std::error::Error>> {
        let mut file = File::open(path)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        let var_type: VarType = bincode::deserialize(&buffer)?;
        Ok(var_type)
    }

    /// Stub for WASM mode
    #[cfg(target_arch = "wasm32")]
    pub fn from_file(_path: &str) -> Result<VarType, Box<dyn std::error::Error>> {
        Err("File loading not supported in WASM mode".into())
    }

    /// Load from bytes (WASM-compatible alternative to from_file)
    pub fn from_bytes(data: &[u8]) -> Result<VarType, Box<dyn std::error::Error>> {
        let var_type: VarType = bincode::deserialize(data)?;
        Ok(var_type)
    }

    pub fn standard_library(&self) -> Vec<(Var, Type)> {
        self.std.iter().cloned().collect()
    }
}

impl Default for VarType {
    fn default() -> Self {
        VarType::new().load_r().unwrap()
    }
}

impl From<Vec<(Var, Type)>> for VarType {
    fn from(val: Vec<(Var, Type)>) -> Self {
        let (variables, aliases) = VarType::separate_variables_aliases(val);
        VarType {
            variables: Arc::new(variables),
            aliases: Arc::new(aliases),
            std: Arc::new(IndexSet::new()),
            alias_counter: CountMap::new(),
            name_index: Arc::new(OnceLock::new()),
        }
    }
}

impl Add for VarType {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        use std::collections::hash_map::Entry;
        use std::collections::HashMap;

        let mut counter: HashMap<TypeCategory, usize> = self.alias_counter.into_iter().collect();
        for (cat, count) in other.alias_counter.into_iter() {
            match counter.entry(cat) {
                Entry::Occupied(mut e) => *e.get_mut() += count,
                Entry::Vacant(e) => {
                    e.insert(count);
                }
            }
        }
        let alias_counter: CountMap<TypeCategory, usize> = counter.into_iter().collect();

        let mut variables = Arc::unwrap_or_clone(self.variables);
        variables.extend(other.variables.iter().cloned());
        let mut aliases = Arc::unwrap_or_clone(self.aliases);
        aliases.extend(other.aliases.iter().cloned());
        let mut std = Arc::unwrap_or_clone(self.std);
        std.extend(other.std.iter().cloned());

        Self {
            variables: Arc::new(variables),
            aliases: Arc::new(aliases),
            std: Arc::new(std),
            alias_counter,
            name_index: Arc::new(OnceLock::new()),
        }
    }
}
