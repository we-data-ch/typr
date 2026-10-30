//! A stable textual form of the IR, for tests and for `typr ir`. Not a serialisation format.

use super::*;
use std::fmt;

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let list = |ts: &[IrType]| ts.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
        match self {
            IrType::Int => f.write_str("int"),
            IrType::Num => f.write_str("num"),
            IrType::Bool => f.write_str("bool"),
            IrType::Char => f.write_str("char"),
            IrType::Null => f.write_str("null"),
            IrType::Unit => f.write_str("()"),
            IrType::Na => f.write_str("na"),
            IrType::Any => f.write_str("Any"),
            IrType::Optional(t) => write!(f, "{t} | na"),
            IrType::Vector(t) => write!(f, "[{t}]"),
            IrType::Record(fs) => {
                let body: Vec<String> = fs.iter().map(|(n, t)| format!("{n}: {t}")).collect();
                write!(f, "{{ {} }}", body.join(", "))
            }
            IrType::Tag(n, t) => write!(f, ".{n}({t})"),
            IrType::Union(ts) => write!(
                f,
                "{}",
                ts.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(" | ")
            ),
            IrType::Tuple(ts) => write!(f, "({})", list(ts)),
            IrType::Function(ps, r) => write!(f, "({}) -> {r}", list(ps)),
            IrType::Named(n, args) if args.is_empty() => f.write_str(n),
            IrType::Named(n, args) => write!(f, "{n}<{}>", list(args)),
            IrType::Param(n) => f.write_str(n),
            IrType::Other(s) => write!(f, "<{s}>"),
            IrType::Unknown => f.write_str("?"),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for e in &self.body {
            write_expr(f, e, 0)?;
        }
        Ok(())
    }
}

fn pad(f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
    for _ in 0..depth {
        f.write_str("  ")?;
    }
    Ok(())
}

fn head(e: &Expr) -> String {
    use ExprKind::*;
    match &e.kind {
        Int(i) => format!("int {i}"),
        Num(n) => format!("num {n}"),
        Bool(b) => format!("bool {b}"),
        Str(s) => format!("str {s:?}"),
        Null => "null".into(),
        Na => "na".into(),
        Unit => "unit".into(),
        Var(n) => format!("var {n}"),
        Block(_) => "block".into(),
        Let { name, public, .. } => format!("let {}{name}", if *public { "pub " } else { "" }),
        Assign { name, .. } => format!("assign {name}"),
        Fn { params, ret, .. } => {
            let ps: Vec<String> = params.iter().map(|p| format!("{}: {}", p.name, p.ty)).collect();
            format!("fn({}) -> {ret}", ps.join(", "))
        }
        Call { .. } => "call".into(),
        Binary { op, .. } => format!("binary {op}"),
        Not(_) => "not".into(),
        If { .. } => "if".into(),
        Match { .. } => "match".into(),
        Vector(_) => "vector".into(),
        Tuple(_) => "tuple".into(),
        Record { name, fields } => {
            let names: Vec<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
            format!(
                "record {}{{{}}}",
                name.as_deref().map(|n| format!("{n} ")).unwrap_or_default(),
                names.join(", ")
            )
        }
        Tag { name, .. } => format!("tag .{name}"),
        Field { name, .. } => format!("field {name}"),
        Index { .. } => "index".into(),
        Return(_) => "return".into(),
        Cast { name, .. } => format!("cast as! {name}"),
        Refine { checks, .. } => format!("refine ({} check(s))", checks.len()),
        While { .. } => "while".into(),
        Loop(_) => "loop".into(),
        For { var, .. } => format!("for {var}"),
        Break => "break".into(),
        Next => "next".into(),
        TypeDef {
            name,
            params,
            ty,
            public,
        } => {
            let ps = if params.is_empty() {
                String::new()
            } else {
                format!("<{}>", params.join(", "))
            };
            format!("type {}{name}{ps} = {ty}", if *public { "pub " } else { "" })
        }
        Module { name, .. } => format!("module {name}"),
        Unsupported { what } => format!("unsupported ({what})"),
    }
}

fn pattern_text(p: &Pattern) -> String {
    match p {
        Pattern::Tag { name, binding: Some(b) } => format!(".{name}({b})"),
        Pattern::Tag { name, binding: None } => format!(".{name}"),
        Pattern::Type { binding, ty } => format!("{binding} as {ty}"),
        Pattern::Bind(n) => n.clone(),
        Pattern::Lit(e) => head(e),
    }
}

fn write_expr(f: &mut fmt::Formatter<'_>, e: &Expr, depth: usize) -> fmt::Result {
    pad(f, depth)?;
    writeln!(f, "{} : {}", head(e), e.ty)?;
    match &e.kind {
        ExprKind::Match { scrutinee, arms } => {
            write_expr(f, scrutinee, depth + 1)?;
            for arm in arms {
                pad(f, depth + 1)?;
                writeln!(f, "arm {}", pattern_text(&arm.pattern))?;
                write_expr(f, &arm.body, depth + 2)?;
            }
            Ok(())
        }
        _ => {
            let mut res = Ok(());
            e.for_each_child(&mut |c| {
                if res.is_ok() {
                    res = write_expr(f, c, depth + 1);
                }
            });
            res
        }
    }
}
