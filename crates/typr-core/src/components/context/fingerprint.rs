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
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::Type;
use std::fmt::Write as _;
use std::hash::Hasher;

/// Deterministic rendering of an `ArgumentType`: mirrors the derived `Debug`
/// shape but descends through the type so nested `Record`/`Interface`/`RClass`
/// field sets are rendered in sorted (stable) order.
fn canonical_arg_debug(arg: &ArgumentType) -> String {
    let label = format!("{:?}", arg.0);
    let typ = canonical_type_debug(&arg.1);
    let embedded = arg.2;
    let variadic = arg.3;
    let default = format!("{:?}", arg.4);
    format!(
        "ArgumentType({}, {}, {}, {}, {})",
        label, typ, embedded, variadic, default
    )
}

/// Deterministic, order-stable rendering of a `Type`. The derived `Debug`
/// iterates the `HashSet`-backed fields of `Record`/`Interface`/`RClass` in
/// per-process random order, which would make any fingerprint over a type
/// containing them unstable across processes (and defeat the per-module
/// incremental cache). Variants whose fields are all order-stable (Vec, Box,
/// primitives, HelpData) delegate to the derived `Debug` unchanged.
pub fn canonical_type_debug(t: &Type) -> String {
    match t {
        Type::Record(fields, h) => {
            let mut v: Vec<String> = fields.iter().map(canonical_arg_debug).collect();
            v.sort();
            format!("Record({:?}, {:?})", v, h)
        }
        Type::Interface(fields, h) => {
            let mut v: Vec<String> = fields.iter().map(canonical_arg_debug).collect();
            v.sort();
            format!("Interface({:?}, {:?})", v, h)
        }
        Type::RClass(set, h) => {
            let mut v: Vec<String> = set.iter().cloned().collect();
            v.sort();
            format!("RClass({:?}, {:?})", v, h)
        }
        Type::Function(args, ret, h) => format!(
            "Function({:?}, {:?}, {:?})",
            args.iter().map(canonical_arg_debug).collect::<Vec<_>>(),
            canonical_type_debug(ret),
            h
        ),
        Type::Vec(vt, size, inner, h) => format!(
            "Vec({:?}, {:?}, {:?}, {:?})",
            vt,
            canonical_type_debug(size),
            canonical_type_debug(inner),
            h
        ),
        Type::Tuple(elems, h) => format!(
            "Tuple({:?}, {:?})",
            elems.iter().map(canonical_type_debug).collect::<Vec<_>>(),
            h
        ),
        Type::Params(ts, h) => format!(
            "Params({:?}, {:?})",
            ts.iter().map(canonical_type_debug).collect::<Vec<_>>(),
            h
        ),
        Type::Module(args, names, h) => format!(
            "Module({:?}, {:?}, {:?})",
            args.iter().map(canonical_arg_debug).collect::<Vec<_>>(),
            names,
            h
        ),
        Type::Alias(name, params, opaque, h) => format!(
            "Alias({:?}, {:?}, {:?}, {:?})",
            name,
            params.iter().map(canonical_type_debug).collect::<Vec<_>>(),
            opaque,
            h
        ),
        Type::Tag(name, body, h) => format!("Tag({:?}, {:?}, {:?})", name, canonical_type_debug(body), h),
        Type::If(cond, branches, h) => format!(
            "If({:?}, {:?}, {:?})",
            canonical_type_debug(cond),
            branches.iter().map(canonical_type_debug).collect::<Vec<_>>(),
            h
        ),
        Type::Condition(a, b, c, h) => format!(
            "Condition({:?}, {:?}, {:?}, {:?})",
            canonical_type_debug(a),
            canonical_type_debug(b),
            canonical_type_debug(c),
            h
        ),
        Type::Operator(op, a, b, h) => format!(
            "Operator({:?}, {:?}, {:?}, {:?})",
            op,
            canonical_type_debug(a),
            canonical_type_debug(b),
            h
        ),
        Type::Multi(body, h) => format!("Multi({:?}, {:?})", canonical_type_debug(body), h),
        _ => format!("{:?}", t),
    }
}

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

        // VarType: IndexSets, insertion-ordered. Each pair must be rendered
        // through `canonical_type_debug`: a signature carrying an inlined
        // `Record`/`Interface`/`RClass` prints its field set in random order
        // under the derived Debug, which would desynchronize cache keys
        // between processes (see canonical_type_debug).
        for pair in self.typing_context.variables.iter() {
            let _ = write!(w, "v{:?}|{}", pair.0, canonical_type_debug(&pair.1));
        }
        for pair in self.typing_context.aliases.iter() {
            let _ = write!(w, "a{:?}|{}", pair.0, canonical_type_debug(&pair.1));
        }
        for pair in self.typing_context.std.iter() {
            let _ = write!(w, "s{:?}|{}", pair.0, canonical_type_debug(&pair.1));
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

        // Vec-backed registries: insertion-ordered. `record_aliases` carries
        // `Type`s (possibly records) — render canonically.
        let _ = write!(w, "tc{:?}", self.type_constructors);
        {
            let ra: Vec<String> = self
                .record_aliases
                .iter()
                .map(|(name, typ)| format!("{}={}", name, canonical_type_debug(typ)))
                .collect();
            let _ = write!(w, "ra{:?}", ra);
        }
        let _ = write!(w, "em{:?}", self.embedded_methods);
        let _ = write!(w, "ef{:?}", self.extern_fns);
        let _ = write!(w, "if{:?}", self.import_from_fns);
        let _ = write!(w, "sf{:?}", self.signature_fns);
        let _ = write!(w, "vf{:?}", self.vectorizable_fns);

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
        let with_var = base
            .clone()
            .push_var_type(Var::from_name("x"), builder::integer_type_default(), &base);
        assert_ne!(base.fingerprint(), with_var.fingerprint());
    }

    #[test]
    fn fingerprint_survives_clone() {
        let base = Context::default();
        let with_var = base
            .clone()
            .push_var_type(Var::from_name("x"), builder::integer_type_default(), &base);
        assert_eq!(with_var.fingerprint(), with_var.clone().fingerprint());
    }
}
