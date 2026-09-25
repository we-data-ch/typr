//! Turns `Lang` + the final `Context` + a recorded `TypeTable` into a [`BlockGraph`] (spec §7.1).

mod blocks;
mod refs;
mod scope;
mod type_relations;

use crate::key::{BlockKey, Namespace};
use crate::model::*;
use scope::ScopeStack;
use std::collections::HashMap;
use typr_core::components::context::Context;
use typr_core::components::language::Lang;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;
use typr_core::processes::type_checking::type_recorder::{SpanKey, TypeTable};

pub fn build(lang: &Lang, context: &Context, types: &TypeTable) -> BlockGraph {
    let root_key = BlockKey::program_root();
    let mut b = Builder {
        context,
        types,
        graph: BlockGraph::new(root_key.clone()),
        scope: ScopeStack::new(),
        pending_captures: HashMap::new(),
        reference_blocks: HashMap::new(),
    };
    b.scope.push_boundary(root_key.clone());

    let items = top_level_items(lang);

    // Pre-pass: every top-level declaration's name is registered before any body is built, so a
    // forward or mutual reference still resolves (spec §7.1 step 2).
    for item in &items {
        if let Some((namespace, name)) = top_level_name(item) {
            let key = BlockKey::top_level(namespace, &name);
            b.scope.bind(&name, PortRef { block: key, port: "out".to_string() });
        }
    }

    let mut children = Vec::new();
    for (index, item) in items.iter().enumerate() {
        b.build_top_level_item(item, index, &mut children);
    }

    type_relations::build(&mut b.graph, b.context, &items);

    b.scope.pop();
    let (inputs, refs) = b.take_captures(&root_key);
    b.graph.relations.extend(refs);
    b.graph.insert(Block {
        key: root_key.clone(),
        kind: BlockKind::Program,
        name: None,
        span: None,
        r#type: None,
        inputs,
        outputs: Vec::new(),
        origin: Origin::User,
        body: Some(Body { children, wires: Vec::new() }),
    });
    b.graph
}

fn top_level_items(lang: &Lang) -> Vec<&Lang> {
    match lang {
        Lang::Lines { value, .. } => value.iter().collect(),
        other => vec![other],
    }
}

fn top_level_name(item: &Lang) -> Option<(Namespace, String)> {
    match item {
        Lang::Let { variable, .. } => variable_name(variable).map(|n| (Namespace::Val, n)),
        Lang::Alias { identifier, .. } => variable_name(identifier).map(|n| (Namespace::Type, n)),
        // `module Name { ... }` (spec §4, étape 5) parses straight to `Lang::Module`, never
        // wrapped in a `Let` — it names itself directly, in the value namespace (it's accessed
        // like a record, via `$`/`use`).
        Lang::Module { name, .. } => Some((Namespace::Val, name.clone())),
        _ => None,
    }
}

pub(super) fn variable_name(lang: &Lang) -> Option<String> {
    match lang {
        Lang::Variable { name, .. } => Some(name.clone()),
        Lang::TypeConstructor { name, .. } => Some(name.clone()),
        _ => None,
    }
}

pub(super) fn span_of(lang: &Lang) -> Span {
    let h = lang.get_help_data();
    Span {
        file: h.get_file_name(),
        start: h.get_offset(),
        end: h.get_end(),
    }
}

/// The outcome of resolving a free name (spec §5.3): where it's really defined, how confident
/// that resolution is, and which boundary (if any) it crosses to get from there to here.
struct Resolved {
    target: PortRef,
    confidence: Confidence,
    crosses: Option<BlockKey>,
}

struct Builder<'a> {
    context: &'a Context,
    types: &'a TypeTable,
    graph: BlockGraph,
    scope: ScopeStack,
    /// Captures discovered so far for a boundary block that hasn't been finalized yet (it's
    /// still being walked) — folded into its `inputs`/`Ref` relations by [`Builder::take_captures`]
    /// once its own body is fully built.
    pending_captures: HashMap<BlockKey, Vec<(String, PortRef, Confidence, Option<String>)>>,
    /// Memoizes synthesized leaf blocks for names resolved outside the program (stdlib, R
    /// packages) so the same name doesn't get a fresh block every time it's referenced.
    reference_blocks: HashMap<String, BlockKey>,
}

impl<'a> Builder<'a> {
    fn recorded_type(&self, lang: &Lang) -> Option<Type> {
        self.types.get(&SpanKey::from(&lang.get_help_data())).cloned()
    }

    fn pretty_type(&self, lang: &Lang) -> Option<String> {
        self.recorded_type(lang).map(|t| t.pretty())
    }

    fn build_top_level_item(&mut self, item: &Lang, index: usize, children: &mut Vec<BlockKey>) {
        // `x <- expr;` outside a loop (spec §4.1's "Assign hors boucle"): not a fresh
        // declaration, so it gets an anonymous positional key like any unnamed statement, but
        // rebinds `x` going forward — later top-level reads of `x` wire from this block, not
        // from whatever defined it before.
        if let Lang::Assign { identifier, expression, .. } = item {
            if let Some(reassigned) = variable_name(identifier) {
                let key = BlockKey::top_level(Namespace::Val, &format!("#{index}"));
                let port = self.build_expr(expression, key.clone(), Some(&reassigned));
                self.scope.bind(&reassigned, port);
                children.push(key);
                return;
            }
        }
        match top_level_name(item) {
            Some((namespace, name)) => {
                let key = BlockKey::top_level(namespace, &name);
                let expr = match item {
                    Lang::Let { expression, .. } => expression.as_ref(),
                    _ => item,
                };
                self.build_expr(expr, key.clone(), Some(&name));
                children.push(key);
            }
            None => {
                let key = BlockKey::top_level(Namespace::Val, &format!("#{index}"));
                self.build_expr(item, key.clone(), None);
                children.push(key);
            }
        }
    }

    /// A statement list belonging to some enclosing block `owner` (a function body, a bare
    /// `{ ... }` scope): named lets are keyed and bound under `owner`, everything else gets an
    /// anonymous positional key (spec §6).
    fn build_nested_body(&mut self, stmts: &[&Lang], owner: &BlockKey) -> Vec<BlockKey> {
        let mut children = Vec::new();
        for (index, stmt) in stmts.iter().enumerate() {
            match stmt {
                Lang::Let { variable, expression, .. } => match variable_name(variable) {
                    Some(name) => {
                        let key = owner.named(&name);
                        let port = self.build_expr(expression, key.clone(), Some(&name));
                        self.scope.bind(&name, port);
                        children.push(key);
                    }
                    None => {
                        // Pattern-destructuring let: not modeled yet (étape 1 keeps bindings
                        // simple); built anonymously so nothing is lost, just unnamed.
                        let key = owner.anonymous(index);
                        self.build_expr(expression, key.clone(), None);
                        children.push(key);
                    }
                },
                // `x <- expr;` (spec §4.1's "Assign hors boucle", also the mechanism behind a
                // `Loop`'s state ports, étape 5): rebinds `x` going forward, same as a `Let`,
                // but keyed anonymously since the name is already claimed by its original
                // declaration.
                Lang::Assign { identifier, expression, .. } => {
                    let key = owner.anonymous(index);
                    match variable_name(identifier) {
                        Some(reassigned) => {
                            let port = self.build_expr(expression, key.clone(), Some(&reassigned));
                            self.scope.bind(&reassigned, port);
                        }
                        None => {
                            self.build_expr(expression, key.clone(), None);
                        }
                    }
                    children.push(key);
                }
                other => {
                    let key = owner.anonymous(index);
                    self.build_expr(other, key.clone(), None);
                    children.push(key);
                }
            }
        }
        children
    }

    /// Builds `lang` as an operand feeding `owner`'s `port` input. A bare name reference doesn't
    /// get its own block (spec §3.2's "un fil... à l'intérieur d'un même bloc parent" collapses a
    /// pure variable read into a direct wire from its definition); anything else recurses into a
    /// real child block first.
    fn build_operand(
        &mut self,
        lang: &Lang,
        key: BlockKey,
        owner: &BlockKey,
        port: &str,
        children: &mut Vec<BlockKey>,
        wires: &mut Vec<Wire>,
    ) {
        let src = match lang {
            Lang::Variable { name, .. } => self.resolve_and_wire(name, None),
            _ => {
                let src = self.build_expr(lang, key.clone(), None);
                children.push(key);
                src
            }
        };
        wires.push(Wire {
            from: src,
            to: PortRef { block: owner.clone(), port: port.to_string() },
        });
    }

    fn resolve_name(&mut self, name: &str, arg0: Option<&Type>) -> Option<Resolved> {
        if let Some((port, is_local)) = self.scope.resolve(name) {
            let crosses = if is_local { None } else { self.scope.nearest_boundary() };
            return Some(Resolved { target: port, confidence: Confidence::Exact, crosses });
        }
        let (ty, confidence) = refs::resolve_by_name(self.context, name, arg0)?;
        let target_key = self.ensure_reference_block(name, &ty);
        Some(Resolved {
            target: PortRef { block: target_key, port: "out".to_string() },
            confidence,
            crosses: self.scope.nearest_boundary(),
        })
    }

    /// Resolves a name for a **reference** (e.g. `Apply.callee`, spec Q2): the `Ref` returned
    /// always points straight at the real definition, never at a capture port — a call always
    /// "goes to the definition". But the name is still genuinely free in the enclosing boundary's
    /// interior, so per Q1 that boundary still gets an honest implicit port for it (with its own
    /// separate boundary-level `Ref`) even though nothing wires through it for a call.
    fn resolve_definition(&mut self, name: &str, arg0: Option<&Type>) -> Option<(BlockKey, Confidence)> {
        let resolved = self.resolve_name(name, arg0)?;
        if let Some(boundary) = resolved.crosses.clone() {
            self.ensure_capture_port(boundary, name, resolved.target.clone(), resolved.confidence.clone(), None);
        }
        Some((resolved.target.block, resolved.confidence))
    }

    /// Resolves a name for a **value read** (a bare variable used as data): if it crosses a
    /// boundary, wires through that boundary's capture port instead of reaching past it directly
    /// (spec §3.3).
    fn resolve_and_wire(&mut self, name: &str, arg0: Option<&Type>) -> PortRef {
        match self.resolve_name(name, arg0) {
            Some(Resolved { target, confidence, crosses: Some(boundary) }) => {
                self.ensure_capture_port(boundary.clone(), name, target, confidence, None);
                PortRef { block: boundary, port: name.to_string() }
            }
            Some(Resolved { target, crosses: None, .. }) => target,
            None => {
                let key = self.ensure_unresolved_block(name);
                PortRef { block: key, port: "out".to_string() }
            }
        }
    }

    fn ensure_reference_block(&mut self, name: &str, ty: &Type) -> BlockKey {
        let cache_key = format!("std:{name}");
        if let Some(key) = self.reference_blocks.get(&cache_key) {
            return key.clone();
        }
        let key = BlockKey::top_level(Namespace::Std, name);
        self.graph.insert(Block {
            key: key.clone(),
            kind: BlockKind::Opaque,
            name: Some(name.to_string()),
            span: None,
            r#type: Some(ty.pretty()),
            inputs: Vec::new(),
            outputs: vec![Port::explicit("out", Some(ty.pretty()))],
            origin: Origin::Std,
            body: None,
        });
        self.reference_blocks.insert(cache_key, key.clone());
        key
    }

    /// A name that type-checking accepted but this best-effort by-name pass couldn't find at all
    /// (should not happen on a program that passed `typr check` — kept only so the builder stays
    /// total rather than panicking on a gap in this pass).
    fn ensure_unresolved_block(&mut self, name: &str) -> BlockKey {
        let cache_key = format!("unresolved:{name}");
        if let Some(key) = self.reference_blocks.get(&cache_key) {
            return key.clone();
        }
        let key = BlockKey::top_level(Namespace::Std, &format!("@unresolved-{name}"));
        self.graph.insert(Block {
            key: key.clone(),
            kind: BlockKind::Opaque,
            name: Some(name.to_string()),
            span: None,
            r#type: None,
            inputs: Vec::new(),
            outputs: vec![Port::explicit("out", None)],
            origin: Origin::User,
            body: None,
        });
        self.reference_blocks.insert(cache_key, key.clone());
        key
    }

    fn ensure_capture_port(
        &mut self,
        boundary: BlockKey,
        name: &str,
        target: PortRef,
        confidence: Confidence,
        ty: Option<String>,
    ) {
        let entry = self.pending_captures.entry(boundary).or_default();
        if entry.iter().any(|(n, ..)| n == name) {
            return;
        }
        entry.push((name.to_string(), target, confidence, ty));
    }

    fn take_captures(&mut self, boundary: &BlockKey) -> (Vec<Port>, Vec<Relation>) {
        let entries = self.pending_captures.remove(boundary).unwrap_or_default();
        let mut inputs = Vec::with_capacity(entries.len());
        let mut relations = Vec::with_capacity(entries.len());
        for (name, target, confidence, ty) in entries {
            inputs.push(Port::implicit(name.clone(), ty));
            relations.push(Relation::r#ref(boundary.clone(), &name, target.block, confidence));
        }
        (inputs, relations)
    }

    /// Common tail of every `build_*` arm: records the recorded type, links a named-alias result
    /// to its `TypeDecl`/`Interface` via `HasType`, inserts the block, and returns a `PortRef` to
    /// its default value port (`outputs[0]`, `"out"` by convention — except `Access`, whose sole
    /// output is the field itself).
    fn finish_block(
        &mut self,
        lang: &Lang,
        key: BlockKey,
        kind: BlockKind,
        name: Option<String>,
        inputs: Vec<Port>,
        outputs: Vec<Port>,
        origin: Origin,
        body: Option<Body>,
    ) -> PortRef {
        let ty = self.recorded_type(lang);
        if let Some(Type::Alias(alias_name, _, _, _)) = &ty {
            self.graph
                .relations
                .push(Relation::has_type(key.clone(), BlockKey::top_level(Namespace::Type, alias_name)));
        }
        let default_port = outputs.first().map(|p| p.name.clone()).unwrap_or_else(|| "out".to_string());
        self.graph.insert(Block {
            key: key.clone(),
            kind,
            name,
            span: Some(span_of(lang)),
            r#type: ty.map(|t| t.pretty()),
            inputs,
            outputs,
            origin,
            body,
        });
        PortRef { block: key, port: default_port }
    }
}
