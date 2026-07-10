#![allow(dead_code, unused_variables, unused_imports)]
//! Per-module incremental type-check cache (stage 3 of incremental builds).
//!
//! Installed by the CLI (`typr build`, project mode) via [`install`]; inert
//! everywhere else (REPL, LSP, tests) — the thread-local store is `None` and
//! every lookup is a no-op.
//!
//! ## Key
//!
//! `H(salt ‖ module name ‖ module source hash ‖ enclosing-context
//! fingerprint)` where:
//! - `salt` covers cache format, typr version and `--test` mode (CLI-side);
//! - the source hash covers the module's own `.ty` file **and its transitive
//!   `mod` imports** (computed by the CLI from `ExpansionInfo`);
//! - the fingerprint ([`Context::fingerprint`]) covers everything typed
//!   before the module, so any upstream change cascades into a miss.
//!
//! ## What a hit skips / replays
//!
//! On a hit, `typing()` on the module body is skipped entirely and the
//! enclosing context is rebuilt by replaying the exact operations the
//! `Lang::Module` arm performs (`push_var_type` + `merge_record_aliases` +
//! `hoist_aliases` + `store_module_inner_context` + `cache_processed_module`)
//! with the cached module type and a restored inner context.
//!
//! The inner `Context` round-trips through serde, which drops its
//! `#[serde(skip)]` fields. The load-bearing one — `VarType::alias_counter`,
//! which drives RecordN/ArrayN numbering and must evolve exactly as in a
//! clean build — is carried explicitly in the entry and restored on replay.
//! `module_inner_contexts` (inners of *nested* modules) is genuinely lost:
//! the transpiler's existing fallback re-types a nested module body when its
//! inner context is missing, so output stays correct at a small time cost.
//!
//! Transpilation is **not** cached: a module's generated R depends on the
//! *final* whole-program registries (see the overlay in the transpiler's
//! `Lang::Module` arm), not just on what was typed before it, so a
//! prefix-keyed cache of R output would be unsound.

use crate::components::context::Context;
use crate::components::r#type::type_category::TypeCategory;
use crate::components::r#type::Type;
use serde::Deserialize;
use serde::Serialize;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleCacheEntry {
    /// The exported `Type::Module` pushed into the enclosing context.
    pub module_type: Type,
    /// The module body's final typing context, with the stdlib table
    /// stripped (identical in every context; restored from the enclosing
    /// context on replay to keep entries small).
    pub inner_context: Context,
    /// `VarType::alias_counter` after typing the body — `#[serde(skip)]` on
    /// `VarType`, so carried explicitly (see module docs).
    pub alias_counter: Vec<(TypeCategory, usize)>,
}

impl ModuleCacheEntry {
    /// Build an entry from the module's exported type and its body's final
    /// typing context.
    pub fn capture(module_type: &Type, inner: &Context) -> Self {
        let mut inner_context = inner.clone();
        inner_context.typing_context.std = std::sync::Arc::new(indexmap::IndexSet::new());
        ModuleCacheEntry {
            module_type: module_type.clone(),
            alias_counter: inner
                .typing_context
                .alias_counter
                .clone()
                .into_iter()
                .collect(),
            inner_context,
        }
    }

    /// Reconstruct the inner context for replay: stdlib table and alias
    /// counter are restored explicitly (both absent from the serialized
    /// form).
    pub fn restore_inner(&self, enclosing: &Context) -> Context {
        let mut inner = self.inner_context.clone();
        inner.typing_context.std = enclosing.typing_context.std.clone();
        inner.typing_context.alias_counter = self.alias_counter.iter().cloned().collect();
        inner
    }
}

/// Storage backend, provided by the CLI (filesystem under `.typr_cache/`).
/// Implementations must be fail-open: any I/O or decode error is a miss.
pub trait ModuleCacheStore {
    fn get(&self, key: u64) -> Option<ModuleCacheEntry>;
    fn put(&self, key: u64, entry: &ModuleCacheEntry);
}

thread_local! {
    static STORE: RefCell<Option<Box<dyn ModuleCacheStore>>> = const { RefCell::new(None) };
    /// Module name → combined source hash (own file + transitive deps).
    static SOURCE_HASHES: RefCell<HashMap<String, u64>> = RefCell::new(HashMap::new());
    static SALT: Cell<u64> = const { Cell::new(0) };
    /// (hits, misses) for the current build.
    static STATS: Cell<(usize, usize)> = const { Cell::new((0, 0)) };
}

/// Activate the cache for the current thread. `source_hashes` decides which
/// modules are cacheable at all (file-backed modules only).
pub fn install(store: Box<dyn ModuleCacheStore>, salt: u64, source_hashes: HashMap<String, u64>) {
    STORE.with(|s| *s.borrow_mut() = Some(store));
    SOURCE_HASHES.with(|s| *s.borrow_mut() = source_hashes);
    SALT.with(|s| s.set(salt));
    STATS.with(|s| s.set((0, 0)));
}

/// Deactivate the cache and return the (hits, misses) counters.
pub fn uninstall() -> (usize, usize) {
    STORE.with(|s| *s.borrow_mut() = None);
    SOURCE_HASHES.with(|s| s.borrow_mut().clear());
    SALT.with(|s| s.set(0));
    STATS.with(|s| s.replace((0, 0)))
}

/// `Some(key)` when the cache is active and `module_name` is a file-backed
/// module registered by the CLI; `None` disables caching for this module.
pub fn cache_key_for(module_name: &str, enclosing: &Context) -> Option<u64> {
    let active = STORE.with(|s| s.borrow().is_some());
    if !active {
        return None;
    }
    let source_hash = SOURCE_HASHES.with(|s| s.borrow().get(module_name).copied())?;
    use std::hash::Hasher;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    hasher.write_u64(SALT.with(|s| s.get()));
    hasher.write(module_name.as_bytes());
    hasher.write_u64(source_hash);
    hasher.write_u64(enclosing.fingerprint());
    Some(hasher.finish())
}

pub fn get(key: u64) -> Option<ModuleCacheEntry> {
    let entry = STORE.with(|s| s.borrow().as_ref().and_then(|store| store.get(key)));
    STATS.with(|s| {
        let (hits, misses) = s.get();
        s.set(if entry.is_some() {
            (hits + 1, misses)
        } else {
            (hits, misses + 1)
        });
    });
    entry
}

pub fn put(key: u64, entry: &ModuleCacheEntry) {
    STORE.with(|s| {
        if let Some(store) = s.borrow().as_ref() {
            store.put(key, entry);
        }
    });
}
