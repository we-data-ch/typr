#![allow(dead_code, unused_variables, unused_imports)]
//! Deterministic fingerprint of a typing `Context`.
//!
//! Used as one half of the per-module incremental cache key (see
//! `processes::type_checking::module_cache`): it captures "everything
//! type-checked before this point", so any upstream change — a new binding,
//! a different module type, a shifted alias counter — cascades into a
//! different fingerprint and invalidates downstream cached modules.
//!
//! Determinism contract: for the same compilation prefix (same binary, same
//! sources in the same order), the fingerprint must be identical across
//! processes. `IndexSet`/`Vec` fields iterate in insertion order and can be
//! hashed as-is; anything `HashMap`/`HashSet`-backed must be sorted first
//! (or, for the subtype graph, restricted to its ordered node tree). Do not
//! add a field here without checking its iteration order.

use crate::components::context::Context;
use std::fmt::Write as _;
use std::hash::Hasher;

/// `fmt::Write` adapter feeding formatted bytes straight into a `Hasher`,
/// so the (large) context never has to be rendered into one big `String`.
struct HashWriter<'a, H: Hasher>(&'a mut H);

impl<H: Hasher> std::fmt::Write for HashWriter<'_, H> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write(s.as_bytes());
        Ok(())
    }
}

impl Context {
    /// Order-stable hash of everything in the context that can influence how
    /// a subsequent expression is typed or transpiled.
    pub fn fingerprint(&self) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        let mut w = HashWriter(&mut hasher);

        // VarType: IndexSets, insertion-ordered.
        for pair in self.typing_context.variables.iter() {
            let _ = write!(w, "v{:?}", pair);
        }
        for pair in self.typing_context.aliases.iter() {
            let _ = write!(w, "a{:?}", pair);
        }
        for pair in self.typing_context.std.iter() {
            let _ = write!(w, "s{:?}", pair);
        }
        // alias_counter is HashMap-backed (and #[serde(skip)], but it drives
        // RecordN/ArrayN numbering): sort before hashing.
        let mut counter: Vec<String> = self
            .typing_context
            .alias_counter
            .clone()
            .into_iter()
            .map(|(category, count)| format!("{:?}={}", category, count))
            .collect();
        counter.sort();
        let _ = write!(w, "c{:?}", counter);

        // Subtype graph: ordered node tree only (see Graph::structure_debug).
        let _ = write!(w, "g{}", self.subtypes.structure_debug());

        // Vec-backed registries: insertion-ordered.
        let _ = write!(w, "tc{:?}", self.type_constructors);
        let _ = write!(w, "ra{:?}", self.record_aliases);
        let _ = write!(w, "em{:?}", self.embedded_methods);
        let _ = write!(w, "ef{:?}", self.extern_fns);
        let _ = write!(w, "if{:?}", self.import_from_fns);

        // HashMap-backed: sort.
        let mut constraints: Vec<String> = self
            .interface_constraints
            .iter()
            .map(|(name, typ)| format!("{}={:?}", name, typ))
            .collect();
        constraints.sort();
        let _ = write!(w, "ic{:?}", constraints);

        let mut processed: Vec<String> = self
            .processed_modules
            .iter()
            .map(|(name, typ)| format!("{}={:?}", name, typ))
            .collect();
        processed.sort();
        let _ = write!(w, "pm{:?}", processed);

        let _ = write!(w, "rc{}", self.rigid_counter);
        // `config` is private to the parent module; fingerprint is a child
        // module of `context`, so direct field access is allowed.
        let _ = write!(w, "cf{:?}", self.config);

        hasher.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::language::var::Var;
    use crate::utils::builder;

    #[test]
    fn fingerprint_is_deterministic_for_identical_contexts() {
        let a = Context::default();
        let b = Context::default();
        assert_eq!(a.fingerprint(), b.fingerprint());
    }

    #[test]
    fn fingerprint_changes_when_a_binding_is_added() {
        let base = Context::default();
        let with_var =
            base.clone()
                .push_var_type(Var::from_name("x"), builder::integer_type_default(), &base);
        assert_ne!(base.fingerprint(), with_var.fingerprint());
    }

    #[test]
    fn fingerprint_survives_clone() {
        let base = Context::default();
        let with_var =
            base.clone()
                .push_var_type(Var::from_name("x"), builder::integer_type_default(), &base);
        assert_eq!(with_var.fingerprint(), with_var.clone().fingerprint());
    }
}
