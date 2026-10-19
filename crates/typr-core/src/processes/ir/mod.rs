//! Typed intermediate representation (rfcs/0000-compilation-targets.md, Step 2).
//!
//! The R backend goes straight from `Lang` to an R string. A second target cannot be built that
//! way: it needs every node's type, and it must not depend on how R happens to spell things.
//! This IR is what the non-R backends read. It is:
//!
//! * **typed**: every expression carries an [`IrType`], taken from the table `typing()` fills in
//!   (`type_checking::type_recorder`), keyed by source span;
//! * **desugared**: it is built from the `Lang` that `typing()` *returns*, so partial
//!   applications, spreads and the like are already gone;
//! * **small**: ~25 expression kinds against `Lang`'s ~60 variants. A construct with no portable
//!   meaning (`R { ... }`, data frames, untyped R functions, ...) becomes [`ExprKind::Unsupported`]
//!   instead of an error, so lowering never fails and the availability pass stays the one place
//!   that decides what is allowed on which target.
//!
//! The R backend is not migrated to this IR; `cases/` protects it in the meantime.

mod lower;
mod print;

pub use lower::{check_and_lower, lower_type};

use std::fmt;

/// The type of an IR node, with aliases, refinements and the `Any` family flattened to what a
/// backend needs to pick a representation. It is *not* a second type checker: nothing here is
/// ever compared for subtyping.
#[derive(Debug, Clone, PartialEq)]
pub enum IrType {
    Int,
    Num,
    Bool,
    Char,
    Null,
    /// The unit type (`Empty`): the type of a statement.
    Unit,
    /// `na` on its own.
    Na,
    Any,
    /// `T | na`.
    Optional(Box<IrType>),
    /// `[#N, T]`, `Vec[#N, T]`, `Array[...]`. The length is a type-level fact and is dropped.
    Vector(Box<IrType>),
    /// Fields sorted by name.
    Record(Vec<(String, IrType)>),
    /// `.Name(payload)`.
    Tag(String, Box<IrType>),
    /// A union with no `na` member, flattened and in source order.
    Union(Vec<IrType>),
    Tuple(Vec<IrType>),
    Function(Vec<IrType>, Box<IrType>),
    /// A user type alias, by name, with its arguments.
    Named(String, Vec<IrType>),
    /// A generic parameter (`T`).
    Param(String),
    /// A type with no portable representation (class, interface, module, ...). The text is the
    /// type as the user would write it; used in diagnostics.
    Other(String),
    /// `typing()` recorded nothing for this node.
    Unknown,
}

/// Where an IR node comes from, for diagnostics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: IrType,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    /// `&` — element-wise on vectors.
    And,
    /// `&&`.
    AndAnd,
    Or,
    OrOr,
    /// `x in xs`.
    In,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::NotEq => "!=",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::Le => "<=",
            BinOp::Ge => ">=",
            BinOp::And => "&",
            BinOp::AndAnd => "&&",
            BinOp::Or => "|",
            BinOp::OrOr => "||",
            BinOp::In => "in",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// `.Tag(x)` / `.Tag`: matches a tag, binding its payload.
    Tag { name: String, binding: Option<String> },
    /// `x as int`: matches a primitive type, binding the value.
    Type { binding: String, ty: IrType },
    /// A plain name: matches anything and binds it (`_` binds nothing).
    Bind(String),
    /// A literal.
    Lit(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Int(i32),
    Num(f64),
    Bool(bool),
    Str(String),
    Null,
    Na,
    Unit,
    Var(String),
    /// A sequence of expressions; the value is the last one's (`Unit` when empty).
    Block(Vec<Expr>),
    Let { name: String, public: bool, value: Box<Expr> },
    Assign { name: String, value: Box<Expr> },
    Fn { params: Vec<Param>, ret: IrType, body: Box<Expr> },
    Call { func: Box<Expr>, args: Vec<Expr> },
    Binary { op: BinOp, lhs: Box<Expr>, rhs: Box<Expr> },
    Not(Box<Expr>),
    If { cond: Box<Expr>, then: Box<Expr>, otherwise: Box<Expr> },
    Match { scrutinee: Box<Expr>, arms: Vec<Arm> },
    Vector(Vec<Expr>),
    Tuple(Vec<Expr>),
    /// `Name:{ f = e, ... }` or `{ f = e, ... }`; fields in source order. `name` is the declared
    /// type when the record was built through a constructor.
    Record { name: Option<String>, fields: Vec<(String, Expr)> },
    Tag { name: String, payload: Box<Expr> },
    Field { target: Box<Expr>, name: String },
    Index { target: Box<Expr>, index: Box<Expr> },
    Return(Box<Expr>),
    /// `value as! Name`: a runtime check that `value` is a `Name` (`ty`), then that value.
    Cast { value: Box<Expr>, name: String },
    /// A boundary the checker could not prove (`refinement_check.rs`): `value` is checked
    /// against `checks` at run time, then is that value. `loc` is `file:line`, as in R's message.
    Refine { value: Box<Expr>, checks: Vec<RefineCheck>, loc: String },
    While { cond: Box<Expr>, body: Box<Expr> },
    Loop(Box<Expr>),
    For { var: String, iter: Box<Expr>, body: Box<Expr> },
    Break,
    Next,
    /// `type Name<params> <- ty;`
    TypeDef { name: String, params: Vec<String>, ty: IrType, public: bool },
    Module { name: String, body: Vec<Expr> },
    /// A construct with no portable meaning. `what` names it (`"R block"`, `"data frame"`, ...).
    Unsupported { what: String },
}

/// One property of a refinement: an interval on what is measured. `None` is unbounded; the
/// `bool` is `true` for an open end (`> c`, `< c`).
#[derive(Debug, Clone, PartialEq)]
pub struct RefineCheck {
    pub length: bool,
    pub lo: Option<(f64, bool)>,
    pub hi: Option<(f64, bool)>,
}

/// A lowered source file: its top-level expressions, in order.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub body: Vec<Expr>,
}

impl Program {
    /// Every [`ExprKind::Unsupported`] in the program, with its location. A backend that cannot
    /// represent a construct reports it from here, and the availability pass has already said why.
    pub fn unsupported(&self) -> Vec<(&str, Option<&Span>)> {
        fn walk<'a>(e: &'a Expr, out: &mut Vec<(&'a str, Option<&'a Span>)>) {
            e.for_each_child(&mut |c| walk(c, out));
            if let ExprKind::Unsupported { what } = &e.kind {
                out.push((what, e.span.as_ref()));
            }
        }
        let mut out = Vec::new();
        self.body.iter().for_each(|e| walk(e, &mut out));
        out
    }
}

impl Expr {
    pub fn new(kind: ExprKind, ty: IrType) -> Self {
        Expr { kind, ty, span: None }
    }

    /// Sets the span from the `Lang` node this expression was lowered from.
    pub(crate) fn at(mut self, l: &crate::components::language::Lang) -> Self {
        self.span = Some(Span {
            file: l.get_help_data().get_file_name(),
            start: l.get_help_data().get_offset(),
            end: l.get_help_data().get_end(),
        });
        self
    }

    /// Calls `f` on each direct sub-expression.
    pub fn for_each_child<'a>(&'a self, f: &mut dyn FnMut(&'a Expr)) {
        use ExprKind::*;
        match &self.kind {
            Int(_) | Num(_) | Bool(_) | Str(_) | Null | Na | Unit | Var(_) | Break | Next
            | TypeDef { .. } | Unsupported { .. } => {}
            Block(v) | Vector(v) | Tuple(v) => v.iter().for_each(f),
            Module { body, .. } => body.iter().for_each(f),
            Let { value, .. } | Assign { value, .. } => f(value),
            Fn { body, .. } => f(body),
            Call { func, args } => {
                f(func);
                args.iter().for_each(f);
            }
            Binary { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            Not(e) | Return(e) | Loop(e) => f(e),
            Cast { value, .. } | Refine { value, .. } => f(value),
            If { cond, then, otherwise } => {
                f(cond);
                f(then);
                f(otherwise);
            }
            Match { scrutinee, arms } => {
                f(scrutinee);
                for a in arms {
                    if let Pattern::Lit(l) = &a.pattern {
                        f(l);
                    }
                    f(&a.body);
                }
            }
            Record { fields, .. } => fields.iter().for_each(|(_, e)| f(e)),
            Tag { payload, .. } => f(payload),
            Field { target, .. } => f(target),
            Index { target, index } => {
                f(target);
                f(index);
            }
            While { cond, body } => {
                f(cond);
                f(body);
            }
            For { iter, body, .. } => {
                f(iter);
                f(body);
            }
        }
    }
}

impl Expr {
    /// Calls `f` on each direct sub-expression, mutably.
    pub fn for_each_child_mut(&mut self, f: &mut dyn FnMut(&mut Expr)) {
        use ExprKind::*;
        match &mut self.kind {
            Int(_) | Num(_) | Bool(_) | Str(_) | Null | Na | Unit | Var(_) | Break | Next
            | TypeDef { .. } | Unsupported { .. } => {}
            Block(v) | Vector(v) | Tuple(v) => v.iter_mut().for_each(f),
            Module { body, .. } => body.iter_mut().for_each(f),
            Let { value, .. } | Assign { value, .. } => f(value),
            Fn { body, .. } => f(body),
            Call { func, args } => {
                f(func);
                args.iter_mut().for_each(f);
            }
            Binary { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            Not(e) | Return(e) | Loop(e) => f(e),
            Cast { value, .. } | Refine { value, .. } => f(value),
            If { cond, then, otherwise } => {
                f(cond);
                f(then);
                f(otherwise);
            }
            Match { scrutinee, arms } => {
                f(scrutinee);
                for a in arms {
                    if let Pattern::Lit(l) = &mut a.pattern {
                        f(l);
                    }
                    f(&mut a.body);
                }
            }
            Record { fields, .. } => fields.iter_mut().for_each(|(_, e)| f(e)),
            Tag { payload, .. } => f(payload),
            Field { target, .. } => f(target),
            Index { target, index } => {
                f(target);
                f(index);
            }
            While { cond, body } => {
                f(cond);
                f(body);
            }
            For { iter, body, .. } => {
                f(iter);
                f(body);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::context::Context;
    use crate::processes::parsing::parse_from_string;

    /// Type-checks `src` (which must be clean) and lowers it.
    fn lower(src: &str) -> Program {
        let lang = parse_from_string(src, "ir_test");
        let (program, errors) = check_and_lower(&Context::default(), &lang);
        assert!(errors.is_empty(), "{errors:?}");
        program
    }

    const PORTABLE: &str = r#"
type Shape <- .Circle(num) | .Square(num);
let area <- fn(s: Shape): num {
    match s { .Circle(r) => 3.14159 * r * r, .Square(c) => c * c }
};
let safe_div <- fn(a: int, b: int): int | na { if (b == 0) { NA } else { a } };
let p <- list { x = 1, y = "a" };
let v <- [1, 2, 3];
let total <- area(.Circle(2.0)) + 1.5;
"#;

    fn all_exprs(p: &Program) -> Vec<&Expr> {
        fn walk<'a>(e: &'a Expr, out: &mut Vec<&'a Expr>) {
            out.push(e);
            e.for_each_child(&mut |c| walk(c, out));
        }
        let mut out = Vec::new();
        p.body.iter().for_each(|e| walk(e, &mut out));
        out
    }

    #[test]
    fn portable_code_lowers_with_every_node_typed_and_nothing_unsupported() {
        let p = lower(PORTABLE);
        assert!(p.unsupported().is_empty(), "{:?}", p.unsupported());
        let untyped: Vec<String> = all_exprs(&p)
            .into_iter()
            .filter(|e| e.ty == IrType::Unknown)
            .map(|e| format!("{:?}", e.kind))
            .collect();
        assert!(untyped.is_empty(), "nodes without a type: {untyped:#?}");
    }

    #[test]
    fn missing_values_are_optional_and_records_are_sorted_structs() {
        let p = lower(PORTABLE);
        let text = p.to_string();
        assert!(text.contains("let safe_div : (int, int) -> int | na"), "{text}");
        assert!(text.contains("let p : { x: int, y: char }"), "{text}");
        assert!(text.contains("let v : [int]"), "{text}");
        assert!(text.contains("type Shape = .Circle(num) | .Square(num)"), "{text}");
        let Some(Expr { kind: ExprKind::Let { value, .. }, .. }) =
            p.body.iter().find(|e| matches!(&e.kind, ExprKind::Let { name, .. } if name == "safe_div"))
        else {
            panic!("safe_div not lowered")
        };
        let IrType::Function(_, ret) = &value.ty else { panic!("not a function: {:?}", value.ty) };
        assert_eq!(**ret, IrType::Optional(Box::new(IrType::Int)));
    }

    #[test]
    fn operands_keep_their_source_order() {
        let p = lower("let f <- fn(b: int): bool { b == 0 };");
        let text = p.to_string();
        let b = text.find("var b").expect("var b");
        let zero = text.find("int 0").expect("int 0");
        assert!(b < zero, "left operand must come first:\n{text}");
        assert!(text.contains("var b : int"), "the operand keeps its own type:\n{text}");
    }

    #[test]
    fn match_arms_carry_their_patterns() {
        let text = lower(PORTABLE).to_string();
        assert!(text.contains("arm .Circle(r)"), "{text}");
        assert!(text.contains("arm .Square(c)"), "{text}");
    }

    #[test]
    fn r_only_constructs_become_unsupported_instead_of_failing() {
        let p = lower(r#"let raw <- fn(): Any { R { Sys.time() } };"#);
        let found = p.unsupported();
        assert_eq!(found.len(), 1, "{found:?}");
        assert_eq!(found[0].0, "R block");
        assert!(found[0].1.is_some(), "an unsupported node keeps its location");
    }

    #[test]
    fn comments_and_attributes_disappear() {
        let p = lower("# hello\n@target(r, rust)\nlet x <- 1;");
        assert_eq!(p.body.len(), 1, "{p}");
    }
}
