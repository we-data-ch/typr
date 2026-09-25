//! Per-`Lang`-variant block construction (spec §4). [`Builder::build_expr`] is total: every
//! `Lang` variant not explicitly matched here — and every shape inside a matched variant that
//! doesn't fit the common case — falls back to a plain `Opaque` block rather than panicking,
//! which is the étape-1 acceptance criterion (spec §12).

use super::{variable_name, Builder};
use crate::key::{BlockKey, Namespace};
use crate::model::*;
use std::collections::HashSet;
use typr_core::components::context::Context;
use typr_core::components::language::argument_value::ArgumentValue;
use typr_core::components::language::operators::Op;
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

            // Everything else (Module, Loop, Match, RCode, and any construct not yet modeled) —
            // étape 5 territory, or genuinely unhandled: falls back to a total, panic-free Opaque.
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
}

fn body_statements_from_vec(body: &[Lang]) -> Vec<&Lang> {
    body.iter().collect()
}
