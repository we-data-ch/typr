//! `Lang` (as returned by `typing()`) + the recorded type table → [`Program`].
//!
//! Lowering is total: a node it does not know how to represent becomes
//! [`ExprKind::Unsupported`], never an error. See the module docs of `ir`.

use super::*;
use crate::components::r#type::refinement::{Bound, Measure};
use crate::processes::transpiling::refinement_checks::format_loc;
use ExprKind as K;
use crate::components::error_message::help_data::HelpData;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::operators::Op;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::tbool::Tbool;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::tnumber::Tnum;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::Type;
use crate::components::context::Context;
use crate::components::r#type::clear_subtype_cache;
use crate::components::error_message::typr_error::TypRError;
use crate::processes::type_checking::type_recorder::{with_recording, SpanKey, TypeTable};
use crate::processes::type_checking::typing;

/// Type-checks `lang` and lowers it, statement by statement.
///
/// `typing()` does not hand back a rewritten program: for a `Lines` node it returns the desugared
/// form of the *last* statement only (the R backend transpiles the parsed AST and re-types
/// locally). So the top level is folded here the way `typing()` folds `Lines`, keeping each
/// statement's own desugared `Lang`; nested blocks are lowered from whatever that statement
/// became. The errors are the checker's, unchanged: a program with errors still lowers, but its
/// types may be `Unknown`.
pub fn check_and_lower(context: &Context, lang: &Lang) -> (Program, Vec<TypRError>) {
    clear_subtype_cache();
    let ((statements, errors, final_context), types) = with_recording(|| {
        let items: Vec<Lang> = match lang {
            Lang::Lines { value, .. } => value.clone(),
            other => vec![other.clone()],
        };
        let mut ctx = context.clone();
        let mut errors = Vec::new();
        let mut out = Vec::new();
        for item in &items {
            let tc = typing(&ctx, item);
            errors.extend(tc.errors);
            ctx = tc.context;
            out.push(tc.lang);
        }
        (out, errors, ctx)
    });
    let lowerer = Lowerer { types: &types, context: &final_context };
    let body = statements.iter().filter_map(|l| lowerer.stmt(l)).collect();
    (Program { body }, errors)
}

/// The IR type of a checker type. Exposed for backends that need to map signatures (`Fn` types,
/// `TypeDef`s) without going through an expression.
pub fn lower_type(t: &Type) -> IrType {
    match t {
        Type::Integer(Tint::Val(_) | Tint::Unknown, _) => IrType::Int,
        Type::Number(Tnum::Val(_) | Tnum::Unknown, _) => IrType::Num,
        Type::Boolean(Tbool::Val(_) | Tbool::Unknown, _) => IrType::Bool,
        Type::Char(..) => IrType::Char,
        Type::Null(_) => IrType::Null,
        Type::Empty(_) => IrType::Unit,
        Type::NA(_) => IrType::Na,
        Type::Any(_) => IrType::Any,
        Type::Generic(n, _) | Type::Variable(n, _) => IrType::Param(n.clone()),
        Type::Vec(vt, _, elem, _) => match vt {
            // A data frame is not a vector: it has no portable representation yet.
            crate::components::r#type::vector_type::VecType::DataFrame => IrType::Other(t.to_string()),
            _ => IrType::Vector(Box::new(lower_type(elem))),
        },
        Type::Record(fields, _) => {
            let mut v: Vec<(String, IrType)> = fields
                .iter()
                .map(|f: &ArgumentType| (f.get_argument_str(), lower_type(&f.get_type())))
                .collect();
            v.sort_by(|a, b| a.0.cmp(&b.0));
            IrType::Record(v)
        }
        Type::Tag(name, payload, _) => IrType::Tag(name.clone(), Box::new(lower_type(payload))),
        Type::Tuple(ts, _) => IrType::Tuple(ts.iter().map(lower_type).collect()),
        Type::Function(params, ret, _) => IrType::Function(
            params.iter().map(|p| lower_type(&p.get_type())).collect(),
            Box::new(lower_type(ret)),
        ),
        Type::Alias(name, args, _, _) => IrType::Named(name.clone(), args.iter().map(lower_type).collect()),
        // A refinement is a runtime check, not a representation.
        Type::Refined(base, _, _) => lower_type(base),
        // `int & (> 0)` before normalisation: the properties are checks, the other side is the type.
        Type::Operator(TypeOperator::Intersection, a, b, _) if matches!(**a, Type::Property(..)) => lower_type(b),
        Type::Operator(TypeOperator::Intersection, a, b, _) if matches!(**b, Type::Property(..)) => lower_type(a),
        Type::Operator(TypeOperator::Union, ..) => {
            let mut members = Vec::new();
            flatten_union(t, &mut members);
            let (nas, rest): (Vec<_>, Vec<_>) = members.into_iter().partition(|m| matches!(m, IrType::Na));
            match (nas.is_empty(), rest.len()) {
                (true, 1) => rest.into_iter().next().unwrap(),
                (true, _) => IrType::Union(rest),
                (false, 0) => IrType::Na,
                (false, 1) => IrType::Optional(Box::new(rest.into_iter().next().unwrap())),
                (false, _) => IrType::Optional(Box::new(IrType::Union(rest))),
            }
        }
        other => IrType::Other(other.to_string()),
    }
}

fn flatten_union(t: &Type, out: &mut Vec<IrType>) {
    match t {
        Type::Operator(TypeOperator::Union, a, b, _) => {
            flatten_union(a, out);
            flatten_union(b, out);
        }
        other => {
            let lowered = lower_type(other);
            if !out.contains(&lowered) {
                out.push(lowered);
            }
        }
    }
}

struct Lowerer<'a> {
    types: &'a TypeTable,
    context: &'a Context,
}

fn span_of(h: &HelpData) -> Span {
    let k = SpanKey::from(h);
    Span {
        file: k.file,
        start: k.start,
        end: k.end,
    }
}

fn bound(b: Bound) -> Option<(f64, bool)> {
    match b {
        Bound::Unbounded => None,
        Bound::Open(c) => Some((c.get(), true)),
        Bound::Closed(c) => Some((c.get(), false)),
    }
}

fn binop(op: &Op) -> Option<BinOp> {
    Some(match op {
        Op::Add(_) => BinOp::Add,
        Op::Minus(_) => BinOp::Sub,
        Op::Mul(_) => BinOp::Mul,
        Op::Div(_) => BinOp::Div,
        Op::Modulo(_) => BinOp::Mod,
        Op::Eq(_) => BinOp::Eq,
        Op::NotEq(_) => BinOp::NotEq,
        Op::LesserThan(_) => BinOp::Lt,
        Op::GreaterThan(_) => BinOp::Gt,
        Op::LesserOrEqual(_) => BinOp::Le,
        Op::GreaterOrEqual(_) => BinOp::Ge,
        Op::And(_) => BinOp::And,
        Op::And2(_) => BinOp::AndAnd,
        Op::Or(_) => BinOp::Or,
        Op::Or2(_) => BinOp::OrOr,
        Op::In(_) => BinOp::In,
        _ => return None,
    })
}

impl Lowerer<'_> {
    fn ty(&self, l: &Lang) -> IrType {
        self.types
            .get(&SpanKey::from(&l.get_help_data()))
            .map(lower_type)
            .unwrap_or(IrType::Unknown)
    }

    fn mk(&self, l: &Lang, kind: ExprKind) -> Expr {
        let mut ty = self.ty(l);
        // A literal's type is its own, whether or not the checker recorded it (it does not for
        // an argument of a call through a module field).
        if ty == IrType::Unknown {
            ty = match &kind {
                ExprKind::Int(_) => IrType::Int,
                ExprKind::Num(_) => IrType::Num,
                ExprKind::Bool(_) => IrType::Bool,
                ExprKind::Str(_) => IrType::Char,
                _ => IrType::Unknown,
            };
        }
        Expr { kind, ty, span: Some(span_of(&l.get_help_data())) }
    }

    fn unsupported(&self, l: &Lang, what: &str) -> Expr {
        self.mk(l, ExprKind::Unsupported { what: what.to_string() })
    }

    fn boxed(&self, l: &Lang) -> Box<Expr> {
        Box::new(self.expr(l))
    }

    /// A top-level or block item. Comments, imports and attributes carry no runtime meaning and
    /// disappear; everything else is an expression.
    fn stmt(&self, l: &Lang) -> Option<Expr> {
        match l {
            Lang::Comment { .. } | Lang::Attribute { .. } | Lang::ModuleImport { .. } | Lang::UseModule { .. } => {
                None
            }
            _ => Some(self.expr(l)),
        }
    }

    fn name_of(&self, l: &Lang) -> String {
        match l {
            Lang::Variable { name, .. } => name.clone(),
            _ => String::new(),
        }
    }

    fn fields(&self, fs: &[ArgumentValue]) -> Vec<(String, Expr)> {
        fs.iter().map(|a| (a.get_argument(), self.expr(&a.get_value()))).collect()
    }

    fn block(&self, items: &[Lang], at: &Lang) -> Expr {
        self.mk(at, ExprKind::Block(items.iter().filter_map(|l| self.stmt(l)).collect()))
    }

    /// An expression, wrapped in the run-time check the type checker recorded at its span, if any
    /// (a refinement it could not prove: `let a: [3, int] <- v`, an argument, a return value).
    fn expr(&self, l: &Lang) -> Expr {
        let e = self.expr_inner(l);
        let h = l.get_help_data();
        match self.context.refinement_obligation_at(&h) {
            Some(set) => {
                let checks = set
                    .iter()
                    .map(|(m, iv)| RefineCheck {
                        length: matches!(m, Measure::Length),
                        lo: bound(iv.lo()),
                        hi: bound(iv.hi()),
                    })
                    .collect();
                let (ty, span) = (e.ty.clone(), e.span.clone());
                Expr { kind: K::Refine { value: Box::new(e), checks, loc: format_loc(&h) }, ty, span }
            }
            None => e,
        }
    }

    fn expr_inner(&self, l: &Lang) -> Expr {
        use ExprKind as K;
        match l {
            Lang::Integer { value, .. } => self.mk(l, K::Int(*value)),
            Lang::Number { value, .. } => self.mk(l, K::Num(*value)),
            Lang::Bool { value, .. } => self.mk(l, K::Bool(*value)),
            Lang::Char { value, .. } => self.mk(l, K::Str(value.clone())),
            Lang::Null(_) => self.mk(l, K::Null),
            Lang::NA(_) => self.mk(l, K::Na),
            Lang::Empty(_) => self.mk(l, K::Unit),
            Lang::Variable { name, .. } => self.mk(l, K::Var(name.clone())),
            Lang::Lines { value, .. } => self.block(value, l),
            Lang::Scope { body, .. } | Lang::Sequence { body, .. } => self.block(body, l),
            Lang::Let { variable, expression, is_public, .. } => self.mk(
                l,
                K::Let {
                    name: self.name_of(variable),
                    public: *is_public,
                    value: self.boxed(expression),
                },
            ),
            Lang::Assign { identifier, expression, .. } => self.mk(
                l,
                K::Assign {
                    name: self.name_of(identifier),
                    value: self.boxed(expression),
                },
            ),
            Lang::Function { parameters, return_type, body, .. } => self.mk(
                l,
                K::Fn {
                    params: parameters
                        .iter()
                        .map(|p| Param {
                            name: p.get_argument_str(),
                            ty: lower_type(&p.get_type()),
                        })
                        .collect(),
                    ret: lower_type(return_type),
                    body: self.boxed(body),
                },
            ),
            Lang::Lambda { parameters, body, .. } => self.mk(
                l,
                K::Fn {
                    params: parameters
                        .iter()
                        .map(|p| Param {
                            name: self.name_of(p),
                            ty: self.ty(p),
                        })
                        .collect(),
                    ret: self.ty(body),
                    body: self.boxed(body),
                },
            ),
            // A `VecFunctionApp` is a call the checker lifted over vectors; the backend sees the
            // vector argument types and lifts it again.
            Lang::FunctionApp { identifier, arguments, .. } | Lang::VecFunctionApp { identifier, arguments, .. } => {
                let args: Vec<Expr> = arguments.iter().map(|a| self.expr(a)).collect();
                let mut func = self.expr(identifier);
                // `function_application` resolves the callee without calling `typing()` on it, so
                // the table has no entry for it. Its type at this call site is the instantiated
                // signature: what the arguments are, and what the call returned.
                if func.ty == IrType::Unknown {
                    func.ty = IrType::Function(args.iter().map(|a| a.ty.clone()).collect(), Box::new(self.ty(l)));
                }
                self.mk(l, K::Call { func: Box::new(func), args })
            }
            // `Lang::Operator` names its operands backwards: `rhs` is the syntactic *left* operand
            // and `lhs` the right one (the R backend renders `rhs <op> lhs`). Swapped here, once.
            Lang::Operator { operator, lhs: right, rhs: left, .. } => match operator {
                // `m$f(args)` / `r$field(args)`: the call is the right operand of the `$`.
                Op::Dollar(_) if matches!(&**right, Lang::FunctionApp { .. }) => {
                    let Lang::FunctionApp { identifier, arguments, .. } = &**right else { unreachable!() };
                    let args: Vec<Expr> = arguments.iter().map(|a| self.expr(a)).collect();
                    let callee = Expr::new(
                        K::Field { target: self.boxed(left), name: self.name_of(identifier) },
                        IrType::Function(args.iter().map(|a| a.ty.clone()).collect(), Box::new(self.ty(l))),
                    );
                    self.mk(l, K::Call { func: Box::new(callee), args })
                }
                Op::Dollar(_) => self.mk(
                    l,
                    K::Field {
                        target: self.boxed(left),
                        name: self.name_of(right),
                    },
                ),
                Op::Custom(name, _) => self.mk(
                    l,
                    K::Call {
                        func: Box::new(Expr::new(K::Var(name.clone()), IrType::Unknown)),
                        args: vec![self.expr(left), self.expr(right)],
                    },
                ),
                other => match binop(other) {
                    Some(op) => self.mk(
                        l,
                        K::Binary {
                            op,
                            lhs: self.boxed(left),
                            rhs: self.boxed(right),
                        },
                    ),
                    None => self.unsupported(l, "operator"),
                },
            },
            Lang::Not { value, .. } => self.mk(l, K::Not(self.boxed(value))),
            Lang::If { condition, if_block, else_block, .. } => self.mk(
                l,
                K::If {
                    cond: self.boxed(condition),
                    then: self.boxed(if_block),
                    otherwise: self.boxed(else_block),
                },
            ),
            Lang::Match { target, branches, .. } => self.mk(
                l,
                K::Match {
                    scrutinee: self.boxed(target),
                    arms: branches
                        .iter()
                        .map(|(pat, body)| Arm {
                            pattern: self.pattern(pat),
                            body: self.expr(body),
                        })
                        .collect(),
                },
            ),
            Lang::Array { value, .. } | Lang::Vector { value, .. } => {
                self.mk(l, K::Vector(value.iter().map(|e| self.expr(e)).collect()))
            }
            Lang::Tuple { value, .. } => self.mk(l, K::Tuple(value.iter().map(|e| self.expr(e)).collect())),
            Lang::List { value, spreads, .. } if spreads.is_empty() => {
                self.mk(l, K::Record { name: None, fields: self.fields(value) })
            }
            Lang::ConstructorCall { type_name, fields, spread, spreads, .. }
                if spread.is_none() && spreads.is_empty() =>
            {
                self.mk(
                    l,
                    K::Record {
                        name: Some(type_name.clone()),
                        fields: self.fields(fields),
                    },
                )
            }
            Lang::Tag { name, value, .. } => self.mk(
                l,
                K::Tag {
                    name: name.clone(),
                    payload: self.boxed(value),
                },
            ),
            Lang::ArrayIndexing { identifier, indexing, .. } => self.mk(
                l,
                K::Index {
                    target: self.boxed(identifier),
                    index: self.boxed(indexing),
                },
            ),
            Lang::Return { value, .. } => self.mk(l, K::Return(self.boxed(value))),
            Lang::WhileLoop { condition, body, .. } => self.mk(
                l,
                K::While {
                    cond: self.boxed(condition),
                    body: self.boxed(body),
                },
            ),
            Lang::Loop { body, .. } => self.mk(l, K::Loop(self.boxed(body))),
            Lang::ForLoop { identifier, expression, body, .. } => self.mk(
                l,
                K::For {
                    var: identifier.get_name(),
                    iter: self.boxed(expression),
                    body: self.boxed(body),
                },
            ),
            Lang::Break(_) => self.mk(l, K::Break),
            Lang::Next(_) => self.mk(l, K::Next),
            Lang::Alias { identifier, parameters, target_type, is_public, .. } => Expr::new(
                K::TypeDef {
                    name: self.name_of(identifier),
                    params: parameters.iter().map(|p| p.to_string()).collect(),
                    ty: lower_type(target_type),
                    public: *is_public,
                },
                IrType::Unit,
            )
            .at(l),
            Lang::Module { name, body, .. } => self.mk(
                l,
                K::Module {
                    name: name.clone(),
                    body: body.iter().filter_map(|e| self.stmt(e)).collect(),
                },
            ),
            Lang::RBlock { .. } | Lang::VecBlock { .. } | Lang::Exp { .. } => self.unsupported(l, "R block"),
            Lang::RFunction { .. } | Lang::ExternBlock { .. } => self.unsupported(l, "untyped R function"),
            Lang::DataFrame { .. } => self.unsupported(l, "data frame"),
            Lang::Library { .. } | Lang::ImportFrom { .. } | Lang::Import { .. } => self.unsupported(l, "R package import"),
            Lang::Signature { .. } => self.unsupported(l, "external signature"),
            Lang::ValidatingCast { expression, type_name, literal_type, .. } => {
                let name = literal_type.as_ref().map(|t| t.to_string()).unwrap_or_else(|| type_name.clone());
                self.mk(l, K::Cast { value: self.boxed(expression), name })
            }
            Lang::List { .. } | Lang::ConstructorCall { .. } => self.unsupported(l, "record spread"),
            _ => self.unsupported(l, "construct"),
        }
    }

    fn pattern(&self, pat: &Lang) -> Pattern {
        match pat {
            Lang::Tag { name, value, .. } => Pattern::Tag {
                name: name.clone(),
                binding: match &**value {
                    Lang::Variable { name, .. } if name != "_" => Some(name.clone()),
                    _ => None,
                },
            },
            Lang::TypePattern { variable_name, matched_type, .. } => Pattern::Type {
                binding: variable_name.clone(),
                ty: lower_type(matched_type),
            },
            Lang::Variable { name, .. } => Pattern::Bind(name.clone()),
            other => Pattern::Lit(Box::new(self.expr(other))),
        }
    }
}
