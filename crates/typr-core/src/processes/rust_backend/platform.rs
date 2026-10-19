//! Platforms of the `rust` target (rfcs/0000-compilation-targets.md §1, §6, Step 6).
//!
//! A platform is a way of packaging what the backend emits. It never changes availability, with
//! one exception: `extendr` adds the **boundary check** (T054), because an exported function's
//! signature has to cross the R <-> Rust FFI.

use super::super::ir::*;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::type_error::{BoundaryViolation, TypeError};
use crate::components::error_message::typr_error::TypRError;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Platform {
    /// A native executable: the top-level statements are `fn main()`.
    #[default]
    Bin,
    /// A Rust library: the functions, no entry point.
    Lib,
    /// A `wasm32-unknown-unknown` `cdylib`; `pub` functions over scalars are exported by name.
    Wasm,
    /// A static library for `rextendr`, plus the R wrappers.
    Extendr,
}

impl Platform {
    pub const NAMES: &'static [&'static str] = &["bin", "lib", "wasm", "extendr"];

    pub fn name(self) -> &'static str {
        match self {
            Platform::Bin => "bin",
            Platform::Lib => "lib",
            Platform::Wasm => "wasm",
            Platform::Extendr => "extendr",
        }
    }

    /// The crate root file Cargo expects.
    pub fn entry_file(self) -> &'static str {
        if self == Platform::Bin {
            "main.rs"
        } else {
            "lib.rs"
        }
    }
}

impl std::fmt::Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl std::str::FromStr for Platform {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "bin" => Ok(Platform::Bin),
            "lib" => Ok(Platform::Lib),
            "wasm" => Ok(Platform::Wasm),
            "extendr" => Ok(Platform::Extendr),
            other => Err(other.to_string()),
        }
    }
}

/// What to emit and for whom.
#[derive(Debug, Clone, Default)]
pub struct Options {
    pub platform: Platform,
    /// The name `extendr_module!` registers (the crate name).
    pub module_name: String,
}

/// What the emitter produced: the crate root, and the files that go beside it.
#[derive(Debug, Clone, Default)]
pub struct Output {
    pub source: String,
    /// `R/extendr-wrappers.R` (extendr only).
    pub r_wrappers: Option<String>,
}

/// A `pub` top-level function: what the host (R, a wasm embedder, a Rust caller) can call.
#[derive(Debug, Clone)]
pub struct Export {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: IrType,
    pub span: Option<Span>,
}

pub fn exports(program: &Program) -> Vec<Export> {
    program
        .body
        .iter()
        .filter_map(|e| match &e.kind {
            ExprKind::Let { name, public: true, value } => match &value.kind {
                ExprKind::Fn { params, ret, .. } => {
                    Some(Export { name: name.clone(), params: params.clone(), ret: ret.clone(), span: e.span.clone() })
                }
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// T054: every exported function's signature must only hold boundary types
/// (`int`, `num`, `bool`, `char` and their `| na` forms, vectors of those, records of those,
/// `Empty` as a return type). Applies to `--platform extendr`.
pub fn boundary_errors(program: &Program) -> Vec<TypRError> {
    let aliases: HashMap<String, IrType> = program
        .body
        .iter()
        .filter_map(|e| match &e.kind {
            ExprKind::TypeDef { name, params, ty, .. } if params.is_empty() => Some((name.clone(), ty.clone())),
            _ => None,
        })
        .collect();
    let mut errors = Vec::new();
    for ex in exports(program) {
        let site = match &ex.span {
            Some(s) => HelpData::at(s.file.clone(), s.start, s.end),
            None => HelpData::default(),
        };
        let mut check = |position: String, ty: &IrType, allow_unit: bool| {
            if allow_unit && *ty == IrType::Unit {
                return;
            }
            if let Some(reason) = crosses(ty, &aliases, 0) {
                errors.push(TypRError::Type(TypeError::NotExportable(Box::new(BoundaryViolation {
                    name: ex.name.clone(),
                    position,
                    ty: ty.to_string(),
                    reason,
                    site: site.clone(),
                }))));
            }
        };
        for p in &ex.params {
            check(format!("parameter `{}`", p.name), &p.ty, false);
        }
        check("the return type".to_string(), &ex.ret, true);
    }
    errors
}

/// `None` when `t` crosses the boundary, else why not.
fn crosses(t: &IrType, aliases: &HashMap<String, IrType>, depth: usize) -> Option<String> {
    if depth > 16 {
        return Some(format!("`{t}` is recursive"));
    }
    match t {
        IrType::Int | IrType::Num | IrType::Bool | IrType::Char => None,
        IrType::Optional(inner) => match &**inner {
            IrType::Int | IrType::Num | IrType::Bool | IrType::Char => None,
            other => crosses(other, aliases, depth + 1).or_else(|| Some(format!("`{t}`: only a scalar can be `| na`"))),
        },
        IrType::Vector(el) => match &**el {
            IrType::Vector(_) => Some(format!("`{t}` is a vector of vectors")),
            el => crosses(el, aliases, depth + 1),
        },
        IrType::Record(fields) => fields.iter().find_map(|(f, ft)| crosses(ft, aliases, depth + 1).map(|r| format!("field `{f}`: {r}"))),
        IrType::Named(n, args) if args.is_empty() => match aliases.get(n) {
            Some(IrType::Record(_)) => crosses(&aliases[n], aliases, depth + 1),
            Some(IrType::Tag(..) | IrType::Union(_)) => Some(format!("`{n}` is a union")),
            Some(other) => crosses(other, aliases, depth + 1),
            None => Some(format!("`{n}` is not a type that crosses into R")),
        },
        IrType::Named(n, _) if n == "State" => Some("`State<T>` is a mutable cell shared inside Rust".to_string()),
        IrType::Named(n, _) => Some(format!("`{n}` is generic")),
        IrType::Tag(..) | IrType::Union(_) => Some(format!("`{t}` is a union")),
        IrType::Function(..) => Some(format!("`{t}` is a closure")),
        IrType::Param(_) | IrType::Any => Some(format!("`{t}` is not a concrete type")),
        IrType::Unit => Some("`Empty` is only a return type".to_string()),
        _ => Some(format!("`{t}` has no representation in R")),
    }
}

/// `Some(c_type)` when a value of `t` can be passed through a C ABI as is.
pub(super) fn c_scalar(t: &IrType) -> Option<&'static str> {
    match t {
        IrType::Int => Some("i32"),
        IrType::Num => Some("f64"),
        IrType::Bool => Some("bool"),
        _ => None,
    }
}
