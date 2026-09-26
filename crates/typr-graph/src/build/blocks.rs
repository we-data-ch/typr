//! Per-`Lang`-variant block construction (spec §4). [`Builder::build_expr`] is total: every
//! `Lang` variant not explicitly matched here — and every shape inside a matched variant that
//! doesn't fit the common case — falls back to a plain `Opaque` block rather than panicking,
//! which is the étape-1 acceptance criterion (spec §12).

use super::{span_of, top_level_name, variable_name, Builder};
use crate::key::{BlockKey, Namespace};
use crate::model::*;
use std::collections::HashSet;
use typr_core::components::context::Context;
use typr_core::components::language::argument_value::ArgumentValue;
use typr_core::components::language::operators::Op;
use typr_core::components::language::var::Var;
use typr_core::components::language::Lang;
use typr_core::components::r#type::argument_type::ArgumentType;
use typr_core::components::r#type::tchar::Tchar;
use typr_core::components::r#type::type_system::TypeSystem;
use typr_core::components::r#type::Type;

/// A parameter/field/method name, read the same way `ArgumentType::get_argument_str` does but
/// without its `panic!` on an unexpected shape (a generic or embedded parameter, say) — falls
/// back to `"_"` instead, since totality here matters more than a perfect label.
pub(super) fn safe_argument_name(arg: &ArgumentType) -> String {
    match arg.get_argument() {
        Type::Char(Tchar::Val(s), _) => s.to_string(),
        Type::LabelGen(s, _) => s.to_uppercase(),
        _ => "_".to_string(),
    }
}

pub(super) fn alias_name(ty: &Type) -> Option<String> {
    match ty {
        Type::Alias(name, ..) => Some(name.clone()),
        _ => None,
    }
}

/// A type's interface per Q4 ("l'interface d'un type = ses méthodes"): every top-level function
/// whose first parameter is `type_name`, discovered via `TypePosition{index: 0}` rather than a
/// declared `impl`. Shared between `build_type_decl` (its `TypeDecl` outputs) and the type
/// relations pass (étape 3's `Satisfies` evidence).
pub(super) fn discover_methods(context: &Context, type_name: &str) -> Vec<(String, String, BlockKey)> {
    context
        .variables()
        .filter_map(|(var, ty)| match ty {
            Type::Function(params, _, _) => {
                let p0 = params.first()?;
                if alias_name(&p0.get_type()).as_deref() == Some(type_name) {
                    let method_name = var.get_name();
                    Some((method_name.clone(), ty.pretty(), BlockKey::top_level(Namespace::Val, &method_name)))
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect()
}

fn body_statements(body: &Lang) -> Vec<&Lang> {
    match body {
        Lang::Scope { body, .. } => body.iter().collect(),
        other => vec![other],
    }
}

impl<'a> Builder<'a> {
    pub(super) fn build_expr(&mut self, lang: &Lang, key: BlockKey, name_hint: Option<&str>) -> PortRef {
        let name = name_hint.map(|s| s.to_string());
        match lang {
            Lang::Number { .. }
            | Lang::Integer { .. }
            | Lang::Bool { .. }
            | Lang::Char { .. }
            | Lang::Null(_)
            | Lang::NA(_)
            | Lang::Empty(_) => {
                let ty = self.pretty_type(lang);
                self.finish_block(lang, key, BlockKind::Literal, name, Vec::new(), vec![Port::explicit("out", ty)], Origin::User, None)
            }

            // Reached only for a *named* position whose RHS is a bare rename (`let x <- y;`) —
            // an unnamed use is short-circuited by `build_operand` before it calls `build_expr`.
            // Modeled as a reference to the real definition rather than a copy of it.
            Lang::Variable { name: var_name, .. } => {
                if let Some((target, confidence)) = self.resolve_definition(var_name, None) {
                    self.graph.relations.push(Relation::r#ref(key.clone(), "value", target, confidence));
                }
                let ty = self.pretty_type(lang);
                self.finish_block(lang, key, BlockKind::Opaque, name, Vec::new(), vec![Port::explicit("out", ty)], Origin::User, None)
            }

            // `Lang::Operator`'s own field names are swapped from what they read as: `Op::combine`
            // (operators.rs) builds `Operator{lhs: right_operand, rhs: left_operand}`, and every
            // other constructor (e.g. `to_module_helper`, and the `dollar_access`/`dot_pipe_access`
            // call sites in `processes/type_checking/mod.rs`) follows the same convention. So the
            // *syntactic* first operand is `rhs`, the second is `lhs` — swapped here once, at the
            // single place this `Lang` shape is taken apart, so every helper below can use
            // `lhs`/`rhs` in their ordinary, non-swapped sense.
            Lang::Operator { operator, lhs, rhs, .. } => self.build_operator_like(operator, rhs, lhs, lang, key, name),

            Lang::Not { value, .. } => {
                let mut children = Vec::new();
                let mut wires = Vec::new();
                self.build_operand(value, key.role("value"), &key, "value", &mut children, &mut wires);
                let inputs = vec![Port::explicit("value", self.pretty_type(value))];
                let out_ty = self.pretty_type(lang);
                self.finish_block(
                    lang,
                    key,
                    BlockKind::Operator,
                    name,
                    inputs,
                    vec![Port::explicit("out", out_ty)],
                    Origin::User,
                    Some(Body { children, wires }),
                )
            }

            Lang::FunctionApp { identifier, arguments, .. } | Lang::VecFunctionApp { identifier, arguments, .. } => {
                self.build_apply(identifier, arguments, lang, key, name)
            }

            Lang::Function { parameters, return_type, body, .. } => {
                self.build_function(parameters, return_type, body, lang, key, name)
            }
            Lang::Lambda { parameters, body, .. } => self.build_lambda(parameters, body, lang, key, name),

            Lang::List { value, .. } => self.build_record(value, lang, key, name),
            Lang::ConstructorCall { fields, .. } => self.build_record(fields, lang, key, name),

            Lang::Alias { identifier, target_type, .. } => self.build_alias(identifier, target_type, lang, key, name),

            Lang::If { condition, if_block, else_block, .. } => self.build_if(condition, if_block, else_block, lang, key, name),

            Lang::Tuple { value, .. } => self.build_sequence(BlockKind::Tuple, value, lang, key, name),
            Lang::Array { value, .. } | Lang::Vector { value, .. } => self.build_sequence(BlockKind::Array, value, lang, key, name),

            Lang::Scope { body, .. } => {
                self.scope.push_plain();
                let stmts = body_statements_from_vec(body);
                let children = self.build_nested_body(&stmts, &key);
                self.scope.pop();
                let out_ty = self.pretty_type(lang);
                self.finish_block(
                    lang,
                    key,
                    BlockKind::Scope,
                    name,
                    Vec::new(),
                    vec![Port::explicit("out", out_ty)],
                    Origin::User,
                    Some(Body { children, wires: Vec::new() }),
                )
            }

            Lang::Module { name: module_name, body, .. } => self.build_module(module_name, body, lang, key, name),

            Lang::ForLoop { identifier, expression, body, .. } => self.build_for_loop(identifier, expression, body, lang, key, name),
            Lang::WhileLoop { condition, body, .. } => self.build_while_loop(condition, body, lang, key, name),
            Lang::Loop { body, .. } => self.build_bare_loop(body, lang, key, name),

            Lang::Match { target, branches, .. } => self.build_match(target, branches, lang, key, name),

            // Raw R (spec §4: "frontière opaque vers R") — the body text is never parsed as
            // TypR, so unlike every other block above, no inputs/wires are derived from it.
            Lang::RBlock { .. } | Lang::RFunction { .. } | Lang::ExternBlock { .. } => {
                let ty = self.pretty_type(lang);
                self.finish_block(lang, key, BlockKind::RCode, name, Vec::new(), vec![Port::explicit("out", ty)], Origin::User, None)
            }

            // Everything else — genuinely unhandled, or not worth a dedicated shape (`Assign`
            // outside a body list, raw `Tag`/`Sequence`, …): falls back to a total, panic-free
            // Opaque.
            other => {
                let ty = self.pretty_type(other);
                self.finish_block(other, key, BlockKind::Opaque, name, Vec::new(), vec![Port::explicit("out", ty)], Origin::User, None)
            }
        }
    }

    fn build_operator_like(&mut self, op: &Op, lhs: &Lang, rhs: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        match op {
            Op::Dollar(_) => self.build_access(lhs, rhs, lang, key, name),
            Op::Dot(_) => match rhs {
                Lang::FunctionApp { identifier, arguments, .. } | Lang::VecFunctionApp { identifier, arguments, .. } => {
                    self.build_ufcs_apply(lhs, identifier, arguments, lang, key, name)
                }
                Lang::Variable { .. } => self.build_access(lhs, rhs, lang, key, name),
                _ => self.build_binary_operator(lhs, rhs, lang, key, name),
            },
            Op::Pipe(_) => match rhs {
                Lang::FunctionApp { identifier, arguments, .. } | Lang::VecFunctionApp { identifier, arguments, .. } => {
                    self.build_ufcs_apply(lhs, identifier, arguments, lang, key, name)
                }
                other => self.build_ufcs_apply(lhs, other, &[], lang, key, name),
            },
            _ => self.build_binary_operator(lhs, rhs, lang, key, name),
        }
    }

    fn build_binary_operator(&mut self, lhs: &Lang, rhs: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        self.build_operand(lhs, key.role("lhs"), &key, "lhs", &mut children, &mut wires);
        self.build_operand(rhs, key.role("rhs"), &key, "rhs", &mut children, &mut wires);
        let inputs = vec![Port::explicit("lhs", self.pretty_type(lhs)), Port::explicit("rhs", self.pretty_type(rhs))];
        let out_ty = self.pretty_type(lang);
        self.finish_block(
            lang,
            key,
            BlockKind::Operator,
            name,
            inputs,
            vec![Port::explicit("out", out_ty)],
            Origin::User,
            Some(Body { children, wires }),
        )
    }

    fn build_access(&mut self, lhs: &Lang, rhs: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        self.build_operand(lhs, key.role("value"), &key, "value", &mut children, &mut wires);
        let field_name = variable_name(rhs).unwrap_or_else(|| "field".to_string());
        let inputs = vec![Port::explicit("value", self.pretty_type(lhs))];
        let out_ty = self.pretty_type(lang);
        self.finish_block(
            lang,
            key,
            BlockKind::Access,
            name,
            inputs,
            vec![Port::explicit(field_name, out_ty)],
            Origin::User,
            Some(Body { children, wires }),
        )
    }

    /// `lhs.f(args)` / `lhs |> f(args)`: UFCS desugars to `f(lhs, args)` at this level already
    /// (the parser keeps `.`/`|>` as plain operators, spec §2's note), so the graph builds it
    /// directly as the `Apply` it becomes rather than as a nested `Access` + call.
    fn build_ufcs_apply(&mut self, receiver: &Lang, callee: &Lang, rest: &[Lang], lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        let mut inputs = vec![Port::explicit("callee", None)];
        match callee {
            Lang::Variable { name: callee_name, .. } => {
                let arg0_ty = self.recorded_type(receiver);
                if let Some((target, confidence)) = self.resolve_definition(callee_name, arg0_ty.as_ref()) {
                    self.graph.relations.push(Relation::r#ref(key.clone(), "callee", target, confidence));
                }
            }
            _ => self.build_operand(callee, key.role("callee"), &key, "callee", &mut children, &mut wires),
        }
        let all_args: Vec<&Lang> = std::iter::once(receiver).chain(rest.iter()).collect();
        for (i, arg) in all_args.iter().enumerate() {
            let port = format!("arg{i}");
            inputs.push(Port::explicit(&port, self.pretty_type(arg)));
            self.build_operand(arg, key.role(&port), &key, &port, &mut children, &mut wires);
        }
        let out_ty = self.pretty_type(lang);
        self.finish_block(
            lang,
            key,
            BlockKind::Apply,
            name,
            inputs,
            vec![Port::explicit("out", out_ty)],
            Origin::User,
            Some(Body { children, wires }),
        )
    }

    fn build_apply(&mut self, identifier: &Lang, arguments: &[Lang], lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        let mut inputs = vec![Port::explicit("callee", None)];
        match identifier {
            Lang::Variable { name: callee_name, .. } => {
                let arg0_ty = arguments.first().and_then(|a| self.recorded_type(a));
                if let Some((target, confidence)) = self.resolve_definition(callee_name, arg0_ty.as_ref()) {
                    self.graph.relations.push(Relation::r#ref(key.clone(), "callee", target, confidence));
                }
            }
            _ => self.build_operand(identifier, key.role("callee"), &key, "callee", &mut children, &mut wires),
        }
        for (i, arg) in arguments.iter().enumerate() {
            let port = format!("arg{i}");
            inputs.push(Port::explicit(&port, self.pretty_type(arg)));
            self.build_operand(arg, key.role(&port), &key, &port, &mut children, &mut wires);
        }
        let out_ty = self.pretty_type(lang);
        self.finish_block(
            lang,
            key,
            BlockKind::Apply,
            name,
            inputs,
            vec![Port::explicit("out", out_ty)],
            Origin::User,
            Some(Body { children, wires }),
        )
    }

    fn build_function(&mut self, parameters: &[ArgumentType], return_type: &Type, body: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        self.scope.push_boundary(key.clone());
        let mut inputs = Vec::new();
        for p in parameters {
            let pname = safe_argument_name(p);
            self.scope.bind(&pname, PortRef { block: key.clone(), port: pname.clone() });
            inputs.push(Port::explicit(pname, Some(p.get_type().pretty())));
        }
        let stmts = body_statements(body);
        let children = self.build_nested_body(&stmts, &key);
        self.scope.pop();
        let (captured, refs) = self.take_captures(&key);
        inputs.extend(captured);
        self.graph.relations.extend(refs);
        self.finish_block(
            lang,
            key,
            BlockKind::Function,
            name,
            inputs,
            vec![Port::explicit("out", Some(return_type.pretty()))],
            Origin::User,
            Some(Body { children, wires: Vec::new() }),
        )
    }

    fn build_lambda(&mut self, parameters: &[Lang], body: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        self.scope.push_boundary(key.clone());
        let mut inputs = Vec::new();
        for (i, p) in parameters.iter().enumerate() {
            let pname = variable_name(p).unwrap_or_else(|| format!("arg{i}"));
            self.scope.bind(&pname, PortRef { block: key.clone(), port: pname.clone() });
            inputs.push(Port::explicit(pname, self.pretty_type(p)));
        }
        let stmts = body_statements(body);
        let children = self.build_nested_body(&stmts, &key);
        self.scope.pop();
        let (captured, refs) = self.take_captures(&key);
        inputs.extend(captured);
        self.graph.relations.extend(refs);
        let out_ty = self.pretty_type(lang);
        self.finish_block(
            lang,
            key,
            BlockKind::Function,
            name,
            inputs,
            vec![Port::explicit("out", out_ty)],
            Origin::User,
            Some(Body { children, wires: Vec::new() }),
        )
    }

    fn build_record(&mut self, fields: &[ArgumentValue], lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        let mut inputs = Vec::new();
        let mut outputs = vec![Port::explicit("out", self.pretty_type(lang))];
        for field in fields {
            let field_name = field.get_argument();
            let value = field.get_value();
            let ty = self.pretty_type(&value);
            inputs.push(Port::explicit(field_name.clone(), ty.clone()));
            outputs.push(Port::explicit(field_name.clone(), ty));
            self.build_operand(&value, key.named(&field_name), &key, &field_name, &mut children, &mut wires);
        }
        self.finish_block(lang, key, BlockKind::Record, name, inputs, outputs, Origin::User, Some(Body { children, wires }))
    }

    fn build_alias(&mut self, _identifier: &Lang, target_type: &Type, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        match target_type {
            Type::Interface(methods, _) => self.build_interface(methods, lang, key, name),
            other => self.build_type_decl(other, lang, key, name),
        }
    }

    fn build_interface(&mut self, methods: &HashSet<ArgumentType>, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        for m in methods {
            children.push(self.insert_type_expr_leaf(&key, m));
        }
        self.finish_block(lang, key, BlockKind::Interface, name, Vec::new(), Vec::new(), Origin::User, Some(Body { children, wires: Vec::new() }))
    }

    fn build_type_decl(&mut self, target_type: &Type, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        if let Type::Record(fields, _) = target_type {
            for f in fields {
                children.push(self.insert_type_expr_leaf(&key, f));
            }
        }
        let mut outputs = vec![Port::explicit("out", self.pretty_type(lang))];
        if let Some(type_name) = &name {
            // Q4: a TypeDecl's interface is its methods — functions taking it as their 1st
            // parameter, discovered via `TypePosition{index: 0}` rather than a declared `impl`.
            let methods = discover_methods(self.context, type_name);
            for (method_name, pretty, method_key) in methods {
                outputs.push(Port::explicit(method_name, Some(pretty)));
                self.graph.relations.push(Relation::type_position(method_key, key.clone(), 0));
            }
        }
        self.finish_block(lang, key, BlockKind::TypeDecl, name, Vec::new(), outputs, Origin::User, Some(Body { children, wires: Vec::new() }))
    }

    fn insert_type_expr_leaf(&mut self, parent: &BlockKey, member: &ArgumentType) -> BlockKey {
        let member_name = safe_argument_name(member);
        let child_key = parent.named(&member_name);
        let ty = member.get_type();
        self.graph.insert(Block {
            key: child_key.clone(),
            kind: BlockKind::TypeExpr,
            name: Some(member_name),
            span: None,
            r#type: Some(ty.pretty()),
            inputs: Vec::new(),
            outputs: vec![Port::explicit("out", Some(ty.pretty()))],
            origin: Origin::User,
            body: None,
        });
        child_key
    }

    fn build_if(&mut self, condition: &Lang, if_block: &Lang, else_block: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        self.build_operand(condition, key.role("cond"), &key, "cond", &mut children, &mut wires);
        // `then`/`else` are always materialized (never elided as a bare-variable read) so both
        // branches stay independently explorable, per the catalog's "chacun explorable".
        let then_key = key.role("then");
        self.build_expr(if_block, then_key.clone(), None);
        children.push(then_key);
        let else_key = key.role("else");
        self.build_expr(else_block, else_key.clone(), None);
        children.push(else_key);
        let inputs = vec![Port::explicit("cond", self.pretty_type(condition))];
        let out_ty = self.pretty_type(lang);
        self.finish_block(
            lang,
            key,
            BlockKind::If,
            name,
            inputs,
            vec![Port::explicit("out", out_ty)],
            Origin::User,
            Some(Body { children, wires }),
        )
    }

    fn build_sequence(&mut self, kind: BlockKind, elements: &[Lang], lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        let mut inputs = Vec::new();
        let mut outputs = vec![Port::explicit("out", self.pretty_type(lang))];
        for (i, el) in elements.iter().enumerate() {
            let port = format!("elem{i}");
            let ty = self.pretty_type(el);
            inputs.push(Port::explicit(&port, ty.clone()));
            if kind == BlockKind::Tuple {
                outputs.push(Port::explicit(&port, ty));
            }
            self.build_operand(el, key.role(&port), &key, &port, &mut children, &mut wires);
        }
        self.finish_block(lang, key, kind, name, inputs, outputs, Origin::User, Some(Body { children, wires }))
    }

    /// `module Name { ... }` (spec §4, §4.1, étape 5). A capture boundary like `Function`, whose
    /// members (`Let`/`Alias`, `@pub` or not) are pre-bound before any of them is built so they
    /// can forward/mutually reference each other — the same two-pass shape as the program root
    /// itself (spec §7.1 step 2). Every member is a body child (`is_public` or not — "le secret
    /// se voit de l'intérieur"), but only `@pub` members get an output port: that's the only
    /// place privacy is actually enforced, since a private member simply isn't reachable from
    /// outside the module.
    fn build_module(&mut self, _module_name: &str, members: &[Lang], lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        self.scope.push_boundary(key.clone());

        for item in members {
            if let Some((_, member_name)) = top_level_name(item) {
                self.scope.bind(&member_name, PortRef { block: key.named(&member_name), port: "out".to_string() });
            }
        }

        let mut children = Vec::new();
        let mut outputs = Vec::new();
        for (index, item) in members.iter().enumerate() {
            match top_level_name(item) {
                Some((_, member_name)) => {
                    let member_key = key.named(&member_name);
                    let expr = match item {
                        Lang::Let { expression, .. } => expression.as_ref(),
                        _ => item,
                    };
                    if is_public_member(item) {
                        outputs.push(Port::explicit(member_name.clone(), self.pretty_type(expr)));
                    }
                    self.build_expr(expr, member_key.clone(), Some(&member_name));
                    children.push(member_key);
                }
                None => {
                    let member_key = key.anonymous(index);
                    self.build_expr(item, member_key.clone(), None);
                    children.push(member_key);
                }
            }
        }

        self.scope.pop();
        let (captured, refs) = self.take_captures(&key);
        self.graph.relations.extend(refs);
        self.finish_block(lang, key, BlockKind::Module, name, captured, outputs, Origin::User, Some(Body { children, wires: Vec::new() }))
    }

    /// `for i in xs { ... }`: the iterable feeds a port named after the loop variable itself
    /// (mirroring a `Function` parameter — the loop variable's port *is* its binding inside the
    /// body), plus the state-port pair of every outer variable reassigned in the loop (spec
    /// §4.1 Q3, `build_loop_state_and_body`).
    fn build_for_loop(&mut self, identifier: &Var, expression: &Lang, body: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let var_name = identifier.get_name();
        let mut children = Vec::new();
        let mut wires = Vec::new();
        let mut inputs = vec![Port::explicit(&var_name, self.pretty_type(expression))];
        self.build_operand(expression, key.role(&var_name), &key, &var_name, &mut children, &mut wires);

        let outputs = self.build_loop_state_and_body(body, &key, Some((&var_name, &var_name)), &mut inputs, &mut wires, &mut children);
        self.finish_block(lang, key, BlockKind::Loop, name, inputs, outputs, Origin::User, Some(Body { children, wires }))
    }

    /// `while (cond) { ... }`: the condition feeds a `cond` input port (same convention as
    /// `If`), re-read every iteration — no loop-variable binding, unlike `ForLoop`.
    fn build_while_loop(&mut self, condition: &Lang, body: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        let mut inputs = vec![Port::explicit("cond", self.pretty_type(condition))];
        self.build_operand(condition, key.role("cond"), &key, "cond", &mut children, &mut wires);

        let outputs = self.build_loop_state_and_body(body, &key, None, &mut inputs, &mut wires, &mut children);
        self.finish_block(lang, key, BlockKind::Loop, name, inputs, outputs, Origin::User, Some(Body { children, wires }))
    }

    /// `loop { ... }`: no condition or iterable at all, just the state-port pairs (the loop is
    /// only ever left via `break`, not modeled as a port here — spec §7 doesn't ask for it).
    fn build_bare_loop(&mut self, body: &Lang, lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        let mut inputs = Vec::new();
        let outputs = self.build_loop_state_and_body(body, &key, None, &mut inputs, &mut wires, &mut children);
        self.finish_block(lang, key, BlockKind::Loop, name, inputs, outputs, Origin::User, Some(Body { children, wires }))
    }

    /// Shared tail of the three loop shapes (spec §4.1 Q3): every outer variable reassigned by a
    /// bare `x <- expr;` directly among the loop's own top-level statements becomes a paired
    /// `{name}_in`/`{name}_out` port — entering, it's what the body reads; leaving, it's
    /// whatever the last reassignment inside the loop wrote (or unchanged, if the loop body
    /// never ran — not distinguished here, same "no version label" stance as a plain
    /// reassignment, spec §4.1). `lead_binding` is `ForLoop`'s extra local name (the loop
    /// variable), bound in the same fresh frame as the state-entering ports before the body is
    /// walked.
    ///
    /// Simplification, in the same spirit as the capture-boundary one (`scope.rs`): only a
    /// direct `Assign` among the loop's immediate statements is seen, not one nested inside an
    /// `if` inside the loop — revisit if a real case needs it.
    fn build_loop_state_and_body(
        &mut self,
        body: &Lang,
        key: &BlockKey,
        lead_binding: Option<(&str, &str)>,
        inputs: &mut Vec<Port>,
        wires: &mut Vec<Wire>,
        children: &mut Vec<BlockKey>,
    ) -> Vec<Port> {
        let stmts = body_statements(body);
        let mut state_names = Vec::new();
        for stmt in &stmts {
            if let Lang::Assign { identifier, .. } = stmt {
                if let Some(n) = variable_name(identifier) {
                    if self.scope.resolve(&n).is_some() && !state_names.contains(&n) {
                        state_names.push(n);
                    }
                }
            }
        }

        for n in &state_names {
            let in_port = format!("{n}_in");
            let src = self.resolve_and_wire(n, None);
            inputs.push(Port::implicit(in_port.clone(), None));
            wires.push(Wire { from: src, to: PortRef { block: key.clone(), port: in_port } });
        }

        self.scope.push_plain();
        if let Some((bind_name, port)) = lead_binding {
            self.scope.bind(bind_name, PortRef { block: key.clone(), port: port.to_string() });
        }
        for n in &state_names {
            self.scope.bind(n, PortRef { block: key.clone(), port: format!("{n}_in") });
        }
        children.extend(self.build_nested_body(&stmts, key));

        let mut outputs = Vec::with_capacity(state_names.len());
        for n in &state_names {
            let (final_port, _) = self.scope.resolve(n).expect("a name just bound in this frame resolves in it");
            outputs.push(Port::implicit(format!("{n}_out"), None));
            wires.push(Wire { from: final_port, to: PortRef { block: key.clone(), port: format!("{n}_out") } });
        }
        self.scope.pop();
        outputs
    }

    /// `match target { pattern => body, ... }` (spec §4, §13). Each branch is its own explorable
    /// sub-block: whatever names its pattern binds become that sub-block's own input ports (the
    /// open question in §13 — "un sous-bloc par bras, dont les liaisons du motif sont les
    /// entrées" — resolved that way), with the sub-block's body wired up exactly like a
    /// function's or scope's. The pattern itself isn't rendered as a block, only the names it
    /// introduces.
    fn build_match(&mut self, target: &Lang, branches: &[(Lang, Box<Lang>)], lang: &Lang, key: BlockKey, name: Option<String>) -> PortRef {
        let mut children = Vec::new();
        let mut wires = Vec::new();
        self.build_operand(target, key.role("target"), &key, "target", &mut children, &mut wires);
        let inputs = vec![Port::explicit("target", self.pretty_type(target))];

        for (index, (pattern, body)) in branches.iter().enumerate() {
            let branch_key = key.role(&format!("branch{index}"));
            let bindings = pattern_bindings(pattern);

            self.scope.push_plain();
            let mut branch_inputs = Vec::with_capacity(bindings.len());
            for n in &bindings {
                self.scope.bind(n, PortRef { block: branch_key.clone(), port: n.clone() });
                branch_inputs.push(Port::implicit(n.clone(), None));
            }
            let stmts = body_statements(body);
            let branch_children = self.build_nested_body(&stmts, &branch_key);
            self.scope.pop();

            let branch_out_ty = self.pretty_type(body);
            self.graph.insert(Block {
                key: branch_key.clone(),
                kind: BlockKind::Scope,
                name: None,
                span: Some(span_of(body)),
                r#type: branch_out_ty.clone(),
                inputs: branch_inputs,
                outputs: vec![Port::explicit("out", branch_out_ty)],
                origin: Origin::User,
                body: Some(Body { children: branch_children, wires: Vec::new() }),
            });
            children.push(branch_key);
        }

        let out_ty = self.pretty_type(lang);
        self.finish_block(lang, key, BlockKind::Match, name, inputs, vec![Port::explicit("out", out_ty)], Origin::User, Some(Body { children, wires }))
    }
}

/// Whether a module member's own `@pub` flag is set (spec §4.1: "un module n'expose que ses
/// membres `@pub`").
fn is_public_member(item: &Lang) -> bool {
    match item {
        Lang::Let { is_public, .. } => *is_public,
        Lang::Alias { is_public, .. } => *is_public,
        _ => false,
    }
}

/// The names a match-branch pattern binds, mirroring `match_expression::build_match_branch_context`
/// (the type checker's own pattern-binding pass) but collecting names only — no types, no
/// errors: this pass runs after type-checking already accepted the program.
fn pattern_bindings(pattern: &Lang) -> Vec<String> {
    match pattern {
        Lang::Tag { value: inner, .. } => match inner.as_ref() {
            Lang::Variable { name, .. } if name != "_" => vec![name.clone()],
            _ => Vec::new(),
        },
        Lang::TypePattern { variable_name: name, .. } => vec![name.clone()],
        Lang::Tuple { value: elements, .. } => elements
            .iter()
            .filter_map(|e| match e {
                Lang::Variable { name, .. } if name != "_" => Some(name.clone()),
                _ => None,
            })
            .collect(),
        Lang::List { value: fields, .. } => fields
            .iter()
            .filter_map(|f| match f.get_value() {
                Lang::Variable { name, .. } if name != "_" => Some(name),
                _ => None,
            })
            .collect(),
        Lang::Variable { name, .. } if name != "_" => vec![name.clone()],
        _ => Vec::new(),
    }
}

fn body_statements_from_vec(body: &[Lang]) -> Vec<&Lang> {
    body.iter().collect()
}

#[cfg(test)]
mod tests {
    use crate::key::Namespace;
    use crate::model::{BlockGraph, BlockKind};
    use crate::BlockKey;
    use typr_core::components::context::Context;
    use typr_core::processes::parsing::parse_from_string;
    use typr_core::processes::type_checking::type_recorder::with_recording;
    use typr_core::processes::type_checking::typing_with_errors;

    fn build_graph(source: &str) -> BlockGraph {
        let lang = parse_from_string(source, "blocks_test");
        let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));
        assert!(!result.has_errors(), "{:?}", result.display_errors());
        crate::build(&lang, &result.type_context.context, &table)
    }

    // `while`'s own condition parser (`single_element`) can't take a bare comparison — this is a
    // pre-existing `typr-core` parser quirk, unrelated to this crate, that double parens work
    // around; not this crate's bug to fix.
    #[test]
    fn while_loop_gets_state_in_out_ports_for_reassigned_outer_variables() {
        let graph = build_graph(
            r#"
let total <- fn(seed: int): int {
    let acc <- seed;
    let i <- 0;
    while ((i < 3)) {
        acc <- acc + i;
        i <- i + 1;
    };
    acc
};
"#,
        );
        let loop_key = BlockKey::top_level(Namespace::Val, "total").role("#2");
        let block = graph.blocks.get(&loop_key).expect("the while loop is built as a top-level function's 3rd body statement");
        assert_eq!(block.kind, BlockKind::Loop);
        let input_names: Vec<_> = block.inputs.iter().map(|p| p.name.as_str()).collect();
        assert!(input_names.contains(&"acc_in"), "{input_names:?}");
        assert!(input_names.contains(&"i_in"), "{input_names:?}");
        let output_names: Vec<_> = block.outputs.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(output_names, vec!["acc_out", "i_out"]);
    }

    #[test]
    fn for_loop_binds_its_variable_to_a_port_named_after_it() {
        let graph = build_graph(
            r#"
let total <- fn(seed: int): int {
    let acc <- seed;
    for (i in [1, 2, 3]) {
        acc <- acc + i;
    };
    acc
};
"#,
        );
        let loop_key = BlockKey::top_level(Namespace::Val, "total").role("#1");
        let block = graph.blocks.get(&loop_key).expect("the for loop is built as a top-level function's 2nd body statement");
        assert_eq!(block.kind, BlockKind::Loop);
        let input_names: Vec<_> = block.inputs.iter().map(|p| p.name.as_str()).collect();
        assert!(input_names.contains(&"i"), "{input_names:?}");
        assert!(input_names.contains(&"acc_in"), "{input_names:?}");
        assert_eq!(block.outputs.iter().map(|p| p.name.as_str()).collect::<Vec<_>>(), vec!["acc_out"]);
    }

    #[test]
    fn match_branch_gets_its_pattern_bindings_as_its_own_input_ports() {
        let graph = build_graph(
            r#"
type Shape <- .Circle(int) | .Square(int);
let area <- fn(s: Shape): int {
    match s {
        .Circle(r) => r * r,
        .Square(side) => side * side,
    }
};
"#,
        );
        let match_key = BlockKey::top_level(Namespace::Val, "area").role("#0");
        let block = graph.blocks.get(&match_key).expect("the match is the function's sole body statement");
        assert_eq!(block.kind, BlockKind::Match);
        assert_eq!(block.inputs.iter().map(|p| p.name.as_str()).collect::<Vec<_>>(), vec!["target"]);

        let branch0 = graph.blocks.get(&match_key.role("branch0")).expect("first branch sub-block");
        assert_eq!(branch0.inputs.iter().map(|p| p.name.as_str()).collect::<Vec<_>>(), vec!["r"]);
        let branch1 = graph.blocks.get(&match_key.role("branch1")).expect("second branch sub-block");
        assert_eq!(branch1.inputs.iter().map(|p| p.name.as_str()).collect::<Vec<_>>(), vec!["side"]);
    }

    #[test]
    fn module_exposes_only_pub_members_as_outputs_but_keeps_all_of_them_in_its_body() {
        let graph = build_graph(
            r#"
module Reporter {
    let helper <- fn(x: int): int { x + 1 };
    @pub let describe <- fn(x: int): int { helper(x) };
};
"#,
        );
        let module_key = BlockKey::top_level(Namespace::Val, "Reporter");
        let block = graph.blocks.get(&module_key).expect("the module is built at the top level, named after itself");
        assert_eq!(block.kind, BlockKind::Module);
        assert_eq!(block.outputs.iter().map(|p| p.name.as_str()).collect::<Vec<_>>(), vec!["describe"]);
        assert!(block.body.as_ref().unwrap().children.contains(&module_key.named("helper")));
        assert!(block.body.as_ref().unwrap().children.contains(&module_key.named("describe")));
    }

    #[test]
    fn r_block_is_a_total_opaque_boundary_with_no_inputs() {
        let graph = build_graph("let total <- R { sum(c(1, 2, 3)) };\n");
        let block = graph.blocks.get(&BlockKey::top_level(Namespace::Val, "total")).expect("top-level R block");
        assert_eq!(block.kind, BlockKind::RCode);
        assert!(block.inputs.is_empty());
        assert!(block.body.is_none());
    }
}
