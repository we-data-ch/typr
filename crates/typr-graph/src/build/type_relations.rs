//! Type relations pass (spec §5.1, étape 3: `Satisfies`, `DeclaredAs`, `Subtype`).
//!
//! A post-pass over the program's top-level `Lang::Alias` declarations, run once every
//! `TypeDecl`/`Interface` block already exists (spec §7.1 step 4). Not folded into `build_expr`'s
//! per-node recursion because these relations compare *pairs* of top-level declarations against
//! each other, not a single node against its children.

use super::blocks::{alias_name, discover_methods, safe_argument_name};
use super::variable_name;
use crate::key::{BlockKey, Namespace};
use crate::model::{BlockGraph, Evidence, Relation};
use std::collections::HashSet;
use typr_core::components::context::Context;
use typr_core::components::language::Lang;
use typr_core::components::r#type::argument_type::ArgumentType;
use typr_core::components::r#type::type_operator::TypeOperator;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;
use typr_core::processes::type_checking::interface_satisfaction::check_interface_satisfaction;

struct TypeAlias {
    name: String,
    key: BlockKey,
    /// The type exactly as written in `type <name> <- <target_type>;` — not reduced, so an
    /// intersection member's own alias name is still visible for `DeclaredAs`.
    target_type: Type,
    /// `Interface`'s required methods, once reduced — `None` for every other kind of alias.
    interface_methods: Option<HashSet<ArgumentType>>,
}

pub(super) fn build(graph: &mut BlockGraph, context: &Context, items: &[&Lang]) {
    let aliases = collect_type_aliases(context, items);
    let (interfaces, type_decls): (Vec<&TypeAlias>, Vec<&TypeAlias>) =
        aliases.iter().partition(|a| a.interface_methods.is_some());

    for decl in &type_decls {
        declared_as(graph, context, decl);
        for iface in &interfaces {
            satisfies(graph, context, decl, iface);
        }
        for other in &type_decls {
            if other.key != decl.key && decl.target_type.is_subtype(&other.target_type, context).0 {
                graph.relations.push(Relation::subtype(decl.key.clone(), other.key.clone()));
            }
        }
    }
}

fn collect_type_aliases(context: &Context, items: &[&Lang]) -> Vec<TypeAlias> {
    items
        .iter()
        .filter_map(|item| match item {
            Lang::Alias { identifier, target_type, .. } => {
                let name = variable_name(identifier)?;
                let key = BlockKey::top_level(Namespace::Type, &name);
                let interface_methods = match target_type.reduce(context) {
                    Type::Interface(methods, _) => Some(methods),
                    _ => None,
                };
                Some(TypeAlias { name, key, target_type: target_type.clone(), interface_methods })
            }
            _ => None,
        })
        .collect()
}

/// `type Foo <- Record & Printable;`: the intersection's interface member is named directly in
/// the declaration (`type_arithmetic::norm_intersection` keeps a `Record & Interface` mix
/// symbolic rather than merging it), so `DeclaredAs` doesn't need structural satisfaction — the
/// declaration itself names the interface it commits to.
fn declared_as(graph: &mut BlockGraph, context: &Context, decl: &TypeAlias) {
    let Type::Operator(TypeOperator::Intersection, a, b, _) = &decl.target_type else {
        return;
    };
    for member in [a.as_ref(), b.as_ref()] {
        if let Some(member_name) = alias_name(member) {
            if matches!(member.reduce(context), Type::Interface(..)) {
                let iface_key = BlockKey::top_level(Namespace::Type, &member_name);
                graph.relations.push(Relation::declared_as(decl.key.clone(), iface_key));
            }
        }
    }
}

/// Structural satisfaction (spec §5.1 `Satisfies`), computed with the same primitive the type
/// checker itself uses for `Record & Interface` declarations and `I(x)` interface-constructor
/// calls (`interface_satisfaction::check_interface_satisfaction`) — this pass doesn't reimplement
/// the check, only renders its result plus a per-method evidence trail (spec §5.2). Evidence
/// falls back to the `TypeDecl` itself (rather than a method block) when a requirement is met
/// structurally — e.g. a record field read as a trivial getter — instead of by a discovered
/// free-function block.
fn satisfies(graph: &mut BlockGraph, context: &Context, decl: &TypeAlias, iface: &TypeAlias) {
    let Some(required) = &iface.interface_methods else {
        return;
    };
    // Nominal reference, matching how a parameter declared `p: <name>` would be typed — see
    // `discover_methods`'s own by-name match and the doc comment on
    // `interface_satisfaction::candidate_methods` (`typr-core`).
    let concrete = Type::Alias(decl.name.clone(), Vec::new(), false, Default::default());
    if check_interface_satisfaction(context, &concrete, required).is_err() {
        return;
    }
    let provided = discover_methods(context, &decl.name);
    let mut evidence: Vec<Evidence> = required
        .iter()
        .map(|req| {
            let name = safe_argument_name(req);
            let provided_by = provided
                .iter()
                .find(|(method_name, ..)| *method_name == name)
                .map(|(_, _, key)| key.clone())
                .unwrap_or_else(|| decl.key.clone());
            Evidence { requires: format!("{name}: {}", req.get_type().pretty()), provided_by }
        })
        .collect();
    evidence.sort_by(|a, b| a.requires.cmp(&b.requires));
    graph.relations.push(Relation::satisfies(decl.key.clone(), iface.key.clone(), evidence));
}

#[cfg(test)]
mod tests {
    use crate::key::Namespace;
    use crate::model::{BlockGraph, RelationKind};
    use crate::BlockKey;
    use typr_core::components::context::Context;
    use typr_core::processes::parsing::parse_from_string;
    use typr_core::processes::type_checking::type_recorder::with_recording;
    use typr_core::processes::type_checking::typing_with_errors;

    fn build_graph(source: &str) -> BlockGraph {
        let lang = parse_from_string(source, "type_relations_test");
        let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));
        assert!(!result.has_errors(), "{:?}", result.display_errors());
        crate::build(&lang, &result.type_context.context, &table)
    }

    #[test]
    fn satisfies_links_a_type_to_the_interface_its_method_satisfies() {
        let graph = build_graph(
            r#"
type Printable <- interface { show: (Self) -> char };
type Point <- list { x: int, y: int };
let show <- fn(p: Point): char { "Point" };
"#,
        );
        let point = BlockKey::top_level(Namespace::Type, "Point");
        let printable = BlockKey::top_level(Namespace::Type, "Printable");
        let rel = graph
            .relations
            .iter()
            .find(|r| r.kind == RelationKind::Satisfies && r.from == point && r.to == printable)
            .expect("expected a Satisfies relation from Point to Printable");
        let evidence = rel.evidence.as_ref().expect("Satisfies carries evidence");
        assert_eq!(evidence.len(), 1);
        assert_eq!(evidence[0].provided_by, BlockKey::top_level(Namespace::Val, "show"));
    }

    #[test]
    fn declared_as_links_an_intersection_alias_to_its_named_interface_member() {
        let graph = build_graph(
            r#"
type Printable <- interface { show: (Self) -> char };
type Point <- list { x: int, y: int };
type Combined <- Point & Printable;
"#,
        );
        let combined = BlockKey::top_level(Namespace::Type, "Combined");
        let printable = BlockKey::top_level(Namespace::Type, "Printable");
        assert!(graph
            .relations
            .iter()
            .any(|r| r.kind == RelationKind::DeclaredAs && r.from == combined && r.to == printable));
    }

    #[test]
    fn subtype_links_a_wider_record_to_a_structurally_narrower_one() {
        let graph = build_graph(
            r#"
type Named <- list { name: char };
type Person <- list { name: char, age: int };
"#,
        );
        let named = BlockKey::top_level(Namespace::Type, "Named");
        let person = BlockKey::top_level(Namespace::Type, "Person");
        assert!(graph
            .relations
            .iter()
            .any(|r| r.kind == RelationKind::Subtype && r.from == person && r.to == named));
    }
}
