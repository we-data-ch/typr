pub mod translatable;

use crate::components::context::config::Environment;
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::language::argument_value::ArgumentValue;
use crate::components::language::format_backtick;
use crate::components::language::function_lang::Function;
use crate::components::language::operators::Op;
use crate::components::language::set_related_type_if_variable;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::language::ModulePosition;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::array_type::ArrayType;
use crate::components::r#type::function_type::FunctionType;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::transpiling::translatable::Translatable;
use crate::processes::type_checking::flatten_operator_union;
use crate::processes::type_checking::resolve_module_member_type;
use crate::processes::type_checking::type_comparison::reduce_type;
use crate::processes::type_checking::typing;
use translatable::RTranslatable;

#[cfg(not(feature = "wasm"))]
use std::fs::File;
#[cfg(not(feature = "wasm"))]
use std::io::Write;
#[cfg(not(feature = "wasm"))]
use std::path::PathBuf;

use std::cell::RefCell;
use std::collections::HashMap;

/// Render a string value as an R double-quoted literal (R's canonical string
/// form). The value is assumed to be already decoded (see
/// `parsing::elements::decode_escapes`), so this is the single place that knows
/// how to escape for the R target: backslashes and double quotes must be
/// escaped, control characters are emitted as escape sequences.
pub fn escape_r_string(s: &str) -> String {
    let escaped = s
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t");
    format!("\"{}\"", escaped)
}

// Thread-local storage for generated files (used in WASM mode)
thread_local! {
    static GENERATED_FILES: RefCell<HashMap<String, String>> = RefCell::new(HashMap::new());
}

// Thread-local stack of roxygen2 `@include` dependencies, scoped per output file.
//
// In Project mode, `mod foo;` dependencies must surface as top-level
// `#' @include foo.R` tags in the *header* of the file that references them — a
// `#'` comment buried inside a `local({ ... })` block is not attached to any
// top-level object, so roxygen2 ignores it. Instead of emitting the tag inline,
// each external module registers its filename into the current frame; the file
// that owns that frame drains it into its header.
//
// The stack mirrors the `to_r` recursion: a new frame is pushed before
// transpiling the body of a module that writes its own file, and drained when
// that file is written. The bottom frame collects top-level (`main`) includes.
thread_local! {
    static INCLUDE_STACK: RefCell<Vec<Vec<String>>> = RefCell::new(vec![Vec::new()]);
}

/// Reset the include stack to a single empty bottom frame (call before a build).
pub fn reset_include_stack() {
    INCLUDE_STACK.with(|s| *s.borrow_mut() = vec![Vec::new()]);
}

/// Push a new frame for the body of a module that writes its own file.
fn push_include_frame() {
    INCLUDE_STACK.with(|s| s.borrow_mut().push(Vec::new()));
}

/// Pop the current frame, returning the includes collected within it.
fn pop_include_frame() -> Vec<String> {
    INCLUDE_STACK.with(|s| s.borrow_mut().pop().unwrap_or_default())
}

/// Register an `@include` target (e.g. "foo.R") into the current frame.
fn register_include(file: &str) {
    INCLUDE_STACK.with(|s| {
        if let Some(top) = s.borrow_mut().last_mut() {
            top.push(file.to_string());
        }
    });
}

/// Drain the bottom (main) frame — the top-level includes for `main.R`.
pub fn take_main_includes() -> Vec<String> {
    INCLUDE_STACK.with(|s| {
        let mut stack = s.borrow_mut();
        match stack.first_mut() {
            Some(bottom) => std::mem::take(bottom),
            None => Vec::new(),
        }
    })
}

/// Register a generated file (used for WASM mode to capture file outputs)
pub fn register_generated_file(path: &str, content: &str) {
    GENERATED_FILES.with(|files| {
        files
            .borrow_mut()
            .insert(path.to_string(), content.to_string());
    });
}

/// Get all generated files
pub fn get_generated_files() -> HashMap<String, String> {
    GENERATED_FILES.with(|files| files.borrow().clone())
}

/// Clear all generated files
pub fn clear_generated_files() {
    GENERATED_FILES.with(|files| {
        files.borrow_mut().clear();
    });
}

/// Write a file - in native mode writes to filesystem, in WASM mode stores in memory
#[cfg(not(feature = "wasm"))]
fn write_output_file(path: &str, content: &str) -> Result<(), String> {
    use std::fs;

    // Also register in memory for consistency
    register_generated_file(path, content);

    let path_buf = PathBuf::from(path);
    if let Some(parent) = path_buf.parent() {
        fs::create_dir_all(parent).map_err(|e| e.to_string())?;
    }
    let mut file = File::create(&path_buf).map_err(|e| e.to_string())?;
    file.write_all(content.as_bytes())
        .map_err(|e| e.to_string())?;
    Ok(())
}

#[cfg(feature = "wasm")]
fn write_output_file(path: &str, content: &str) -> Result<(), String> {
    register_generated_file(path, content);
    Ok(())
}

pub trait ToSome {
    fn to_some(self) -> Option<Self>
    where
        Self: Sized;
}

impl<T: Sized> ToSome for T {
    fn to_some(self) -> Option<Self> {
        Some(self)
    }
}

trait AndIf {
    fn and_if<F>(self, condition: F) -> Option<Self>
    where
        F: Fn(Self) -> bool,
        Self: Sized;
}

impl<T: Clone> AndIf for T {
    fn and_if<F>(self, condition: F) -> Option<Self>
    where
        F: Fn(Self) -> bool,
    {
        if condition(self.clone()) {
            Some(self)
        } else {
            None
        }
    }
}

const JS_HEADER: &str = "";

fn to_pattern_match_statement(
    exp: Lang,
    branches: &[(Lang, Box<Lang>)],
    context: &Context,
) -> String {
    let match_var = "match_val__";
    let res = branches
        .iter()
        .enumerate()
        .map(|(id, (pattern, body))| {
            let (cond, bindings) = pattern_to_condition(pattern, match_var, context);
            let body_str = body.to_r(context).0;
            let body_with_bindings = if bindings.is_empty() {
                body_str
            } else {
                format!("{}\n{}", bindings, body_str)
            };
            if cond == "TRUE" {
                // wildcard pattern: always matches
                if id == 0 {
                    format!("{{\n{}\n}}", body_with_bindings)
                } else {
                    format!("else {{\n{}\n}}", body_with_bindings)
                }
            } else if id == 0 {
                format!("if ({}) {{\n{}\n}}", cond, body_with_bindings)
            } else {
                format!("else if ({}) {{\n{}\n}}", cond, body_with_bindings)
            }
        })
        .collect::<Vec<_>>()
        .join(" ");
    format!("{{\n{} <- {}\n{}\n}}", match_var, exp.to_r(context).0, res)
}

/// Map a Type to its corresponding R type-check function name.
fn type_to_r_check(typ: &Type) -> Option<&'static str> {
    match typ {
        Type::Integer(_, _) => Some("is.integer"),
        Type::Boolean(_, _) => Some("is.logical"),
        Type::Number(_, _) => Some("is.numeric"),
        Type::Char(_, _) => Some("is.character"),
        Type::Null(_) => Some("is.null"),
        _ => None,
    }
}

/// R class a value of `typ` is expected to carry at runtime, used by record
/// validators to check field types via `inherits`. Relies on monomorphisation:
/// every value already carries its type's class. Returns `None` for types with
/// no reliable nominal class (generics, functions, unions…), in which case the
/// field is only checked for presence.
fn record_field_class(typ: &Type, cont: &Context) -> Option<String> {
    match typ {
        Type::Integer(_, _) => Some("integer".to_string()),
        Type::Number(_, _) => Some("numeric".to_string()),
        Type::Char(_, _) => Some("character".to_string()),
        Type::Boolean(_, _) => Some("logical".to_string()),
        Type::Alias(name, _, _, _) => match cont
            .aliases()
            .find(|(var, _)| var.get_name() == *name)
            .map(|(_, t)| t)
        {
            // Record aliases carry their alias name as the S3 class.
            Some(Type::Record(_, _)) => Some(name.clone()),
            // Primitive aliases (e.g. `type Meters <- int`) carry the underlying
            // R class — no constructor adds the alias name as a class.
            Some(inner) => record_field_class(&inner, cont),
            None => None,
        },
        _ => None,
    }
}

/// Find the name of a union alias that declares a tag variant called
/// `tag_name`. Used by the `Lang::Tag` literal to enrich its runtime class
/// with the union name (canonical representation, see
/// `validation_variant_d_union.md` §2). Returns `None` for standalone tags
/// (no declared union).
fn find_union_for_tag(tag_name: &str, cont: &Context) -> Option<String> {
    cont.aliases().find_map(|(var, typ)| {
        let is_union = matches!(
            typ,
            Type::Operator(
                crate::components::r#type::type_operator::TypeOperator::Union,
                _,
                _,
                _
            )
        );
        if !is_union {
            return None;
        }
        let declares_tag = flatten_operator_union(typ)
            .iter()
            .any(|m| matches!(m, Type::Tag(n, _, _) if n == tag_name));
        if declares_tag {
            Some(var.get_name())
        } else {
            None
        }
    })
}

/// Build the structural body-validation block for a tag's payload, shared by
/// standalone tag aliases (`type Hello <- .Hello(char)`) and union variants.
/// `name` is the type/variant name used in error messages; `inner_type` is the
/// declared payload type. An empty payload (`.Nothing`) yields an empty block.
fn tag_body_validation(name: &str, inner_type: &Type) -> String {
    match inner_type {
        Type::Empty(_) => String::new(),
        Type::Integer(tint, _) => {
            use crate::components::r#type::tint::Tint;
            let null_check = format!("\n  if (is.null(x[[\"body\"]])) stop(\"Validation failed for type {name}: missing 'body' field\")\n  if (!is.integer(x[[\"body\"]])) stop(\"Validation failed for type {name}: body must be int\")");
            match tint {
                Tint::Val(i) => format!("{null_check}\n  if (x[[\"body\"]] != {i}L) stop(\"Validation failed for type {name}: body must be literal {i}\")"),
                Tint::Unknown => null_check,
            }
        }
        Type::Char(tchar, _) => {
            use crate::components::r#type::tchar::Tchar;
            let null_check = format!("\n  if (is.null(x[[\"body\"]])) stop(\"Validation failed for type {name}: missing 'body' field\")\n  if (!is.character(x[[\"body\"]])) stop(\"Validation failed for type {name}: body must be char\")");
            match tchar {
                Tchar::Val(s) => format!("{null_check}\n  if (x[[\"body\"]] != '{s}') stop(\"Validation failed for type {name}: body must be literal '{s}'\")"),
                Tchar::Unknown => null_check,
            }
        }
        Type::Boolean(tbool, _) => {
            use crate::components::r#type::tbool::Tbool;
            let null_check = format!("\n  if (is.null(x[[\"body\"]])) stop(\"Validation failed for type {name}: missing 'body' field\")\n  if (!is.logical(x[[\"body\"]])) stop(\"Validation failed for type {name}: body must be bool\")");
            match tbool {
                Tbool::Val(b) => {
                    let r_val = if *b { "TRUE" } else { "FALSE" };
                    format!("{null_check}\n  if (x[[\"body\"]] != {r_val}) stop(\"Validation failed for type {name}: body must be literal {r_val}\")")
                }
                Tbool::Unknown => null_check,
            }
        }
        Type::Number(tnum, _) => {
            use crate::components::r#type::tnumber::Tnum;
            let null_check = format!("\n  if (is.null(x[[\"body\"]])) stop(\"Validation failed for type {name}: missing 'body' field\")\n  if (!is.numeric(x[[\"body\"]])) stop(\"Validation failed for type {name}: body must be num\")");
            match tnum {
                Tnum::Val(v) => format!("{null_check}\n  if (x[[\"body\"]] != {v}) stop(\"Validation failed for type {name}: body must be literal {v}\")"),
                Tnum::Unknown => null_check,
            }
        }
        Type::Alias(alias_name, _, _, _) => format!(
            "\n  if (is.null(x[[\"body\"]])) stop(\"Validation failed for type {name}: missing 'body' field\")\n  validate_{alias_name}(x[[\"body\"]])"
        ),
        _ => format!(
            "\n  if (is.null(x[[\"body\"]])) stop(\"Validation failed for type {name}: missing 'body' field\")"
        ),
    }
}

/// Emit the full constructor/annotator/validator pipeline for a single tag
/// variant `V` of union `U`, in the canonical representation
/// (`structure(list("V", body = p), class = c("V", "U", "Tag", "list"))`).
/// Mirrors the record pipeline (see `Lang::Alias` / `Type::Record`).
fn tag_variant_pipeline(variant_name: &str, union_name: &str, inner_type: &Type) -> String {
    let is_empty = matches!(inner_type, Type::Empty(_));
    // Constructor: build the raw value, then delegate entirely to the
    // annotator. It neither sets the class nor validates.
    let constructor = if is_empty {
        format!(
            "{variant_name} <- function() {{\n  x <- list(\"{variant_name}\")\n  as.{variant_name}(x)\n}}"
        )
    } else {
        format!(
            "{variant_name} <- function(x) {{\n  v <- list(\"{variant_name}\", body = x)\n  as.{variant_name}(v)\n}}"
        )
    };
    // Annotator: single entry point. Sets the class idempotently, then runs
    // the internal validator and the user validator (`validate` S3 generic,
    // which dispatches to `validate.{variant_name}` then `validate.{union_name}`).
    let annotator = format!(
        "as.{variant_name} <- function(x) {{\n  if (!inherits(x, \"{variant_name}\")) class(x) <- c(\"{variant_name}\", \"{union_name}\", \"Tag\", \"list\")\n  x <- validate_{variant_name}(x)\n  x <- validate(x)\n  x\n}}"
    );
    // Internal validator: pure structural invariants (tag identity + payload).
    let body_validation = tag_body_validation(variant_name, inner_type);
    let validator = format!(
        "validate_{variant_name} <- function(x) {{\n  if (x[[1]] != '{variant_name}') stop(\"Validation failed for type {variant_name}: expected tag '{variant_name}'\")\n{body_validation}\n  x\n}}"
    );
    format!("{constructor}\n{annotator}\n{validator}")
}

fn pattern_to_condition(pattern: &Lang, match_var: &str, _context: &Context) -> (String, String) {
    match pattern {
        // Tag with a binding variable: .Some(a)
        Lang::Tag {
            name, value: inner, ..
        } => {
            let cond = format!("{}[[1]] == '{}'", match_var, name);
            match inner.as_ref() {
                Lang::Variable { name: var_name, .. } => {
                    let binding = format!("{} <- {}[[\"body\"]]", var_name, match_var);
                    (cond, binding)
                }
                Lang::Empty(_) => (cond, String::new()),
                _ => (cond, String::new()),
            }
        }
        // Type pattern: x as int
        Lang::TypePattern {
            variable_name: var_name,
            matched_type: typ,
            ..
        } => {
            let check_fn = type_to_r_check(typ).unwrap_or("is.logical");
            let cond = format!("{}({})", check_fn, match_var);
            let binding = format!("{} <- {}", var_name, match_var);
            (cond, binding)
        }
        // Tuple pattern: :{a, b, c}
        Lang::Tuple {
            value: elements, ..
        } => {
            let cond = format!(
                "inherits({}, 'Tuple') && length({}) == {}",
                match_var,
                match_var,
                elements.len()
            );
            let bindings: Vec<String> = elements
                .iter()
                .enumerate()
                .filter_map(|(i, elem)| {
                    if let Lang::Variable { name: var_name, .. } = elem {
                        if var_name == "_" {
                            None
                        } else {
                            Some(format!("{} <- {}[[{}]]", var_name, match_var, i + 1))
                        }
                    } else {
                        None
                    }
                })
                .collect();
            (cond, bindings.join("\n"))
        }
        // List/record pattern: :{nom: n, age: a}
        Lang::List { value: fields, .. } => {
            let conditions: Vec<String> = fields
                .iter()
                .map(|arg_val: &ArgumentValue| {
                    format!("!is.null({}[[\"{}\"]])", match_var, arg_val.get_argument())
                })
                .collect();
            let cond = if conditions.is_empty() {
                "is.list(".to_string() + match_var + ")"
            } else {
                format!("is.list({}) && {}", match_var, conditions.join(" && "))
            };
            let bindings: Vec<String> = fields
                .iter()
                .filter_map(|arg_val| {
                    if let Lang::Variable { name: var_name, .. } = &arg_val.get_value() {
                        Some(format!(
                            "{} <- {}[[\"{}\"]]",
                            var_name,
                            match_var,
                            arg_val.get_argument()
                        ))
                    } else {
                        None
                    }
                })
                .collect();
            (cond, bindings.join("\n"))
        }
        // DataFrame pattern: data__frame(col1 = x, col2 = y)
        Lang::DataFrame { value: fields, .. } => {
            let conditions: Vec<String> = fields
                .iter()
                .map(|arg_val: &ArgumentValue| {
                    format!("!is.null({}[[\"{}\"]])", match_var, arg_val.get_argument())
                })
                .collect();
            let cond = if conditions.is_empty() {
                "is.data.frame(".to_string() + match_var + ")"
            } else {
                format!(
                    "is.data.frame({}) && {}",
                    match_var,
                    conditions.join(" && ")
                )
            };
            let bindings: Vec<String> = fields
                .iter()
                .filter_map(|arg_val| {
                    if let Lang::Variable { name: var_name, .. } = &arg_val.get_value() {
                        Some(format!(
                            "{} <- {}[[\"{}\"]]",
                            var_name,
                            match_var,
                            arg_val.get_argument()
                        ))
                    } else {
                        None
                    }
                })
                .collect();
            (cond, bindings.join("\n"))
        }
        // Wildcard: _
        Lang::Variable { name, .. } if name == "_" => ("TRUE".to_string(), String::new()),
        // Other variable: bind the whole value
        Lang::Variable { name, .. } => {
            let binding = format!("{} <- {}", name, match_var);
            ("TRUE".to_string(), binding)
        }
        _ => ("TRUE".to_string(), String::new()),
    }
}

impl RTranslatable<(String, Context)> for Lang {
    fn to_r(&self, cont: &Context) -> (String, Context) {
        let result = match self {
            Lang::Bool { value: b, .. } => {
                let (typ, _, _) = typing(cont, self).to_tuple();
                let anotation = cont.get_type_anotation(&typ);
                (
                    format!("{} |> {}", b.to_string().to_uppercase(), anotation),
                    cont.clone(),
                )
            }
            Lang::Number { value: n, .. } => {
                let (typ, _, _) = typing(cont, self).to_tuple();
                let anotation = cont.get_type_anotation(&typ);
                (format!("{} |> {}", n, anotation), cont.clone())
            }
            Lang::Integer { value: i, .. } => {
                let (typ, _, _) = typing(cont, self).to_tuple();
                let anotation = cont.get_type_anotation(&typ);
                (format!("{}L |> {}", i, anotation), cont.clone())
            }
            Lang::Char { value: s, .. } => {
                let (typ, _, _) = typing(cont, self).to_tuple();
                let anotation = cont.get_type_anotation(&typ);
                (
                    format!("{} |> {}", escape_r_string(s), anotation),
                    cont.clone(),
                )
            }
            Lang::Operator {
                operator: op @ (Op::Dot(_) | Op::Pipe(_)),
                rhs: e1,
                lhs: e2,
                ..
            } => {
                // `rhs` is the syntactic-left operand, `lhs` is the
                // syntactic-right operand (same convention as `Op::Dollar`
                // and the generic operator case below: `e1.to_r() <op>
                // e2.to_r()`). For plain field access (`p.x`, `e2` a bare
                // `Lang::Variable` field name and `e1` not an `Integer`),
                // that means the receiver `e1` must render first:
                // `e1[['e2']]`, not `e2[['e1']]`. The `Lang::Integer`
                // sub-case is the synthetic tuple-destructuring node built
                // in `parsing/mod.rs` (`rhs: Integer(index), lhs: tmp_var`),
                // which already has the receiver/index roles swapped
                // relative to that convention on purpose — left as-is, and
                // `Op::Pipe` keeps its prior (separate, untouched) ordering
                // since a bare-variable pipe target (`a |> f`) means
                // something else entirely (`f(a)`, not field indexing).
                let is_dot = matches!(op, Op::Dot(_));
                let e1 = (**e1).clone();
                let e2 = (**e2).clone();
                match e2.clone() {
                    Lang::Variable { .. } => match e1 {
                        Lang::Integer { .. } => Translatable::from(cont.clone())
                            .to_r(&e2)
                            .add("[[")
                            .to_r(&e1)
                            .add("]]")
                            .into(),
                        _ if is_dot => Translatable::from(cont.clone())
                            .to_r(&e1)
                            .add("[['")
                            .to_r(&e2)
                            .add("']]")
                            .into(),
                        _ => Translatable::from(cont.clone())
                            .to_r(&e2)
                            .add("[['")
                            .to_r(&e1)
                            .add("']]")
                            .into(),
                    },
                    Lang::List { value: fields, .. } => {
                        let at = fields[0].clone();
                        Translatable::from(cont.clone())
                            .add("within(")
                            .to_r(&e2)
                            .add(", { ")
                            .add(&at.get_argument())
                            .add(" <- ")
                            .to_r(&at.get_value())
                            .add(" })")
                            .into()
                    }
                    Lang::DataFrame { value: fields, .. } => {
                        let at = fields[0].clone();
                        Translatable::from(cont.clone())
                            .add("within(")
                            .to_r(&e2)
                            .add(", { ")
                            .add(&at.get_argument())
                            .add(" <- ")
                            .to_r(&at.get_value())
                            .add(" })")
                            .into()
                    }
                    Lang::FunctionApp {
                        identifier: var,
                        arguments: v,
                        help_data: h,
                    } => {
                        let v = [e1].iter().chain(v.iter()).cloned().collect();
                        Lang::FunctionApp {
                            identifier: var,
                            arguments: v,
                            help_data: h,
                        }
                        .to_r(cont)
                    }
                    _ => Translatable::from(cont.clone())
                        .to_r(&e2)
                        .add("[[")
                        .add("]]")
                        .to_r(&e1)
                        .into(),
                }
            }
            Lang::Operator {
                operator: Op::Dollar(_),
                rhs: e1,
                lhs: e2,
                ..
            } => {
                let e1 = (**e1).clone();
                let e2 = (**e2).clone();
                let t1 = typing(cont, &e1).value;
                let val = match (t1.clone(), e2.clone()) {
                    (Type::Vec(vtype, _, _, _), Lang::Variable { name, .. })
                        if vtype.is_array() =>
                    {
                        format!("vec_apply(get, {}, typed_vec('{}'))", e1.to_r(cont).0, name)
                    }
                    (Type::Vec(VecType::S3, _, _, _), Lang::Variable { name, .. }) => {
                        let name_str = name.replace("__", ".");
                        format!("get({}, '{}')", e1.to_r(cont).0, name_str)
                    }
                    (_, Lang::Variable { name, .. }) => format!("{}${}", e1.to_r(cont).0, name),
                    _ => format!("{}${}", e1.to_r(cont).0, e2.to_r(cont).0),
                };
                (val, cont.clone())
            }
            Lang::Operator {
                operator: op,
                rhs: e1,
                lhs: e2,
                ..
            } => {
                let op_str = format!(" {} ", op);
                Translatable::from(cont.clone())
                    .to_r(e1)
                    .add(&op_str)
                    .to_r(e2)
                    .into()
            }
            Lang::Scope { body: exps, .. } => Translatable::from(cont.clone())
                .add("{\n")
                .join(exps, "\n")
                .add("\n}")
                .into(),
            Lang::Function {
                parameters: params,
                body,
                ..
            } => {
                let fn_type = FunctionType::try_from(typing(cont, self).value.clone())
                    .expect("function expression should have a function type");
                let return_type = fn_type.get_return_type();

                // Record alias constructors take specific named fields — calling
                // TypeName(single_value) would fail.  The body already constructs
                // the correct type (via ConstructorCall or List), so skip the
                // output conversion for record aliases.
                let is_record_alias_return = match &return_type {
                    Type::Alias(alias_name, _, _, _) => cont
                        .aliases()
                        .find(|(var, _)| var.get_name() == *alias_name)
                        .map(|(_, t)| matches!(t, Type::Record(_, _)))
                        .unwrap_or(false),
                    _ => false,
                };
                let output_conversion = if is_record_alias_return {
                    "".to_string()
                } else {
                    cont.get_type_anotation(&return_type)
                };

                let has_variadic = params.last().map(|p| p.is_variadic()).unwrap_or(false);
                let list_of_types = params
                    .iter()
                    .map(ArgumentType::body_type)
                    .collect::<Vec<_>>();
                let sub_context = params
                    .iter()
                    .map(|arg_typ| arg_typ.clone().set_type(arg_typ.body_type()).to_var(cont))
                    .zip(list_of_types.clone())
                    .fold(cont.clone(), |context: Context, (var, typ)| {
                        context.clone().push_var_type(var, typ, &context)
                    });
                let res = if output_conversion.is_empty() {
                    "".to_string()
                } else {
                    " |> ".to_owned() + &output_conversion
                };
                let body_r = body.to_r(&sub_context).0;
                let final_body_r = if has_variadic {
                    let vname = params.last().unwrap().get_argument_str();
                    // The body sees the variadic param as `[#N, T]`, so collect
                    // the R `...` into a `typed_vec` to match the S3 dispatch the
                    // stdlib array functions (`sum`, `map`, `length`, …) rely on.
                    let collector = "typed_vec(..., dim = c(...length()))";
                    // inject `vname <- typed_vec(...)` after opening `{`
                    if body_r.starts_with('{') {
                        format!("{{\n{} <- {}{}", vname, collector, &body_r[1..])
                    } else {
                        body_r
                    }
                } else {
                    body_r
                };
                (
                    format!(
                        "(function({}) {}{}) |> {}",
                        params
                            .iter()
                            .map(|x| x.to_r())
                            .collect::<Vec<_>>()
                            .join(", "),
                        final_body_r,
                        res,
                        cont.get_type_anotation(&fn_type.into())
                    ),
                    cont.clone(),
                )
            }
            Lang::Variable { .. } => {
                //Here we only keep the variable name, the path and the type
                let var = Var::from_language(self.clone()).unwrap();
                let name = if var.contains("__") {
                    var.replace("__", ".").get_name()
                } else {
                    var.display_type(cont).get_name()
                };
                (name.to_string(), cont.clone())
            }
            Lang::FunctionApp {
                identifier: exp,
                arguments: vals,
                ..
            } => {
                let var = Var::try_from(exp.clone()).unwrap();

                let (exp_str, cont1) = exp.to_r(cont);
                let fn_t = FunctionType::try_from(
                    cont1
                        .get_type_from_variable(&var)
                        .unwrap_or_else(|_| panic!("variable {} don't have a related type", var)),
                )
                .map(|ft| ft.adjust_nb_parameters(vals.len()))
                .expect("function application identifier should have a function type");
                let new_args = fn_t
                    .get_param_types()
                    .iter()
                    .map(|arg| reduce_type(&cont1, arg))
                    .collect::<Vec<_>>();
                let new_vals = vals
                    .iter()
                    .zip(new_args.iter())
                    .map(set_related_type_if_variable)
                    .collect::<Vec<_>>();
                let (args, current_cont) = Translatable::from(cont1).join(&new_vals, ", ").into();
                Var::from_language(*exp.clone())
                    .map(|var| {
                        let name = var.get_name();
                        let new_name = if &name[0..1] == "%" {
                            format!("`{}`", name.replace("__", "."))
                        } else {
                            name.replace("__", ".")
                        };
                        (format!("{}({})", new_name, args), current_cont.clone())
                    })
                    .unwrap_or((format!("{}({})", exp_str, args), current_cont))
            }
            Lang::VecFunctionApp {
                vector_type,
                identifier: exp,
                arguments: vals,
                ..
            } => {
                let var = Var::try_from(exp.clone()).unwrap();
                let name = var.get_name();
                let str_vals = vals
                    .iter()
                    .map(|x| x.to_r(cont).0)
                    .collect::<Vec<_>>()
                    .join(", ");
                if *vector_type == VecType::Vector {
                    // `Vec[N, T]` values transpile to plain R atomic vectors (see
                    // vectors.md): R already vectorizes arithmetic/comparison
                    // operators and ordinary scalar functions over them natively,
                    // so no `vec_apply` (typed_vec normalization + manual
                    // recycling, needed for the `[N, T]` / S3-array mechanism) is
                    // required here — just call the function plainly.
                    if cont.is_an_untyped_function(&name) {
                        let name = name.replace("__", ".");
                        let new_name = if &name[0..1] == "%" {
                            format!("`{}`", name)
                        } else {
                            name.to_string()
                        };
                        (format!("{}({})", new_name, str_vals), cont.clone())
                    } else {
                        let (exp_str, cont1) = exp.to_r(cont);
                        let fn_t = FunctionType::try_from(
                            cont1.get_type_from_variable(&var).unwrap_or_else(|_| {
                                panic!("variable {} don't have a related type", var)
                            }),
                        )
                        .expect(
                            "vector function application identifier should have a function type",
                        );
                        let new_args = fn_t
                            .get_param_types()
                            .iter()
                            .map(|arg| reduce_type(&cont1, arg))
                            .collect::<Vec<_>>();
                        let new_vals = vals
                            .iter()
                            .zip(new_args.iter())
                            .map(set_related_type_if_variable)
                            .collect::<Vec<_>>();
                        let (args, current_cont) =
                            Translatable::from(cont1).join(&new_vals, ", ").into();
                        Var::from_language(*exp.clone())
                            .map(|var| {
                                let name = var.get_name();
                                let new_name = if &name[0..1] == "%" {
                                    format!("`{}`", name.replace("__", "."))
                                } else {
                                    name.replace("__", ".")
                                };
                                (format!("{}({})", new_name, args), current_cont.clone())
                            })
                            .unwrap_or((format!("{}({})", exp_str, args), current_cont))
                    }
                } else if name == "reduce" {
                    (format!("vec_reduce({})", str_vals), cont.clone())
                } else if name == "extend" {
                    (format!("vec_extend({})", str_vals), cont.clone())
                } else if cont.is_an_untyped_function(&name) {
                    let name = name.replace("__", ".");
                    let new_name = if &name[0..1] == "%" {
                        format!("`{}`", name)
                    } else {
                        name.to_string()
                    };
                    let s = format!("vec_apply({}, {})", new_name, str_vals);
                    (s, cont.clone())
                } else {
                    let (exp_str, cont1) = exp.to_r(cont);
                    let fn_t = FunctionType::try_from(
                        cont1.get_type_from_variable(&var).unwrap_or_else(|_| {
                            panic!("variable {} don't have a related type", var)
                        }),
                    )
                    .expect("vector function application identifier should have a function type");
                    let new_args = fn_t
                        .get_param_types()
                        .iter()
                        .map(|arg| reduce_type(&cont1, arg))
                        .collect::<Vec<_>>();
                    let new_vals = vals
                        .iter()
                        .zip(new_args.iter())
                        .map(set_related_type_if_variable)
                        .collect::<Vec<_>>();
                    let (args, current_cont) =
                        Translatable::from(cont1).join(&new_vals, ", ").into();
                    Var::from_language(*exp.clone())
                        .map(|var| {
                            let name = var.get_name();
                            let new_name = if &name[0..1] == "%" {
                                format!("`{}`", name.replace("__", "."))
                            } else {
                                name.replace("__", ".")
                            };
                            (
                                format!("vec_apply({}, {})", new_name, args),
                                current_cont.clone(),
                            )
                        })
                        .unwrap_or((format!("vec_apply({}, {})", exp_str, args), current_cont))
                }
            }
            Lang::ArrayIndexing {
                identifier: exp,
                indexing: val,
                ..
            } => {
                let (exp_str, _) = exp.to_r(cont);
                // v[-n] → v[[length(v) + (1 - n)]] (count from end)
                let negative_idx = val.get_members_if_array().and_then(|members| {
                    if members.len() == 1 {
                        if let Lang::Integer { value: i, .. } = &members[0] {
                            if *i < 0 {
                                Some(*i)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                });
                let res = if let Some(neg) = negative_idx {
                    let offset = 1 + neg; // e.g. -1 → 0, -2 → -1
                    if offset == 0 {
                        format!("{}[[length({})]]", exp_str, exp_str)
                    } else if offset < 0 {
                        format!("{}[[length({}) - {}L]]", exp_str, exp_str, -offset)
                    } else {
                        format!("{}[[length({}) + {}L]]", exp_str, exp_str, offset)
                    }
                } else {
                    let (val_str, _) = val.to_simple_r(cont);
                    format!("{}[[{}]]", exp_str, val_str)
                };
                (res, cont.clone())
            }
            Lang::GenFunc { name: func, .. } => (
                format!("function(x, ...) UseMethod('{}')", func),
                cont.clone(),
            ),
            Lang::Let {
                variable: expr,
                r#type: ttype,
                expression: body,
                is_public: _,
                is_testable: _,
                is_export,
                help_data: _,
            } => {
                let (body_str, new_cont) = body.to_r(cont);
                let new_name = format_backtick(expr.clone().to_r(cont).0);

                let (r_code, _new_name2) = Function::try_from((**body).clone())
                    .map(|_| {
                        let related_type = Var::try_from(expr)
                            .ok()
                            .map(|v| v.get_type())
                            .filter(|t| !matches!(t, Type::Empty(_) | Type::UnknownFunction(_)))
                            .unwrap_or_else(|| typing(cont, expr).value);
                        let method = match cont.get_environment() {
                            Environment::Project => format!(
                                "#' @method {}\n",
                                new_name.replace(".", " ").replace("`", "")
                            ),
                            _ => "".to_string(),
                        };
                        match related_type {
                            Type::Empty(_) => {
                                (format!("{} <- {}", new_name, body_str), new_name.clone())
                            }
                            Type::Any(_) | Type::Generic(_, _) => (
                                format!("{}.default <- {}", new_name, body_str),
                                new_name.clone(),
                            ),
                            _ => (
                                format!("{}{} <- {}", method, new_name, body_str),
                                new_name.clone(),
                            ),
                        }
                    })
                    .unwrap_or((format!("{} <- {}", new_name, body_str), new_name));
                let code = if !ttype.is_empty() {
                    let type_annotation = new_cont.get_type_anotation(ttype);
                    format!("{} |> {}\n", r_code, type_annotation)
                } else {
                    r_code + "\n"
                };
                // RFC-TR-032: @export prepends `#' @export` for R package API
                let code = if *is_export {
                    format!("#' @export\n{}", code)
                } else {
                    code
                };
                (code, new_cont)
            }
            Lang::Array { .. } => {
                let typ = self.typing(cont).value;

                let dimension = ArrayType::try_from(typ.clone())
                    .expect("array literal should have an array type")
                    .get_shape()
                    .map(|sha| format!("c({})", sha))
                    .unwrap_or_else(|| "c(0)".to_string());

                let array = &self
                    .linearize_array()
                    .iter()
                    .map(|lang| lang.to_r(cont).0)
                    .collect::<Vec<_>>()
                    .join(", ")
                    .and_if(|lin_array| !lin_array.is_empty())
                    .map(|lin_array| format!("typed_vec({}, dim = {})", lin_array, dimension))
                    .unwrap_or_else(|| format!("typed_vec(dim = {})", dimension));

                (
                    format!("{} |> {}", array, cont.get_type_anotation(&typ)),
                    cont.to_owned(),
                )
            }
            Lang::List {
                value: args,
                spreads,
                ..
            } if spreads.is_empty() => {
                let (body, current_cont) = Translatable::from(cont.clone())
                    .join_arg_val(args, ",\n ")
                    .into();
                let (typ, _, _) = typing(cont, self).to_tuple();
                // For record-alias types use the constructor directly
                if let Type::Alias(alias_name, _, _, _) = &typ {
                    let is_record = cont
                        .aliases()
                        .find(|(var, _)| var.get_name() == *alias_name)
                        .map(|(_, t)| matches!(t, Type::Record(_, _)))
                        .unwrap_or(false);
                    if is_record {
                        return (format!("{}({})", alias_name, body), current_cont);
                    }
                }
                let anotation = cont.get_type_anotation(&typ);
                cont.get_classes(&typ)
                    .map(|_| format!("list({}) |> {}", body, anotation))
                    .unwrap_or(format!("list({}) |> {}", body, anotation))
                    .to_some()
                    .map(|s| (s, current_cont))
                    .unwrap()
            }
            // Record literal with one or more `...source` spreads
            // (spread_operator2.md): merge spreads sequentially into `base`
            // with the runtime `spread()` helper, then apply explicit
            // fields as the final override — never via static field
            // expansion, so unknown/row-polymorphic fields carried by the
            // runtime value of `source` are preserved (§5-§7).
            Lang::List {
                value: args,
                spreads,
                ..
            } => {
                let mut spreads_iter = spreads.iter();
                let first = spreads_iter.next().expect("checked non-empty above");
                let (mut base, mut current_cont) = first.to_r(cont);
                for spread_expr in spreads_iter {
                    let (next, next_cont) = spread_expr.to_r(&current_cont);
                    base = format!("spread({}, {})", base, next);
                    current_cont = next_cont;
                }
                if args.is_empty() {
                    return (base, current_cont);
                }
                let (overrides, current_cont) = Translatable::from(current_cont)
                    .join_arg_val(args, ", ")
                    .into();
                (
                    format!("spread({}, list({}))", base, overrides),
                    current_cont,
                )
            }
            Lang::DataFrame { value: args, .. } => {
                let (body, current_cont) = Translatable::from(cont.clone())
                    .join_arg_val(args, ",\n ")
                    .into();
                let (typ, _, _) = typing(cont, self).to_tuple();
                let anotation = cont.get_type_anotation(&typ);
                cont.get_classes(&typ)
                    .map(|_| format!("data.frame({}) |> {}", body, anotation))
                    .unwrap_or(format!("data.frame({}) |> {}", body, anotation))
                    .to_some()
                    .map(|s| (s, current_cont))
                    .unwrap()
            }
            Lang::If {
                condition: cond,
                if_block: exp,
                else_block: els,
                ..
            } if els == &Box::new(Lang::Empty(HelpData::default())) => {
                Translatable::from(cont.clone())
                    .add("if(")
                    .to_r(cond)
                    .add(") {\n")
                    .to_r(exp)
                    .add(" \n}")
                    .into()
            }
            Lang::If {
                condition: cond,
                if_block: exp,
                else_block: els,
                help_data: _,
            } => Translatable::from(cont.clone())
                .add("if(")
                .to_r(cond)
                .add(") {\n")
                .to_r(exp)
                .add(" \n} else ")
                .to_r(els)
                .into(),
            Lang::Tuple { value: vals, .. } => Translatable::from(cont.clone())
                .add("struct(list(")
                .join(vals, ", ")
                .add("), 'Tuple')")
                .into(),
            Lang::Assign {
                identifier: var,
                expression: exp,
                ..
            } => Translatable::from(cont.clone())
                .to_r(var)
                .add(" <- ")
                .to_r(exp)
                .into(),
            Lang::Comment { value: txt, .. } => ("#".to_string() + &txt, cont.clone()),
            Lang::Tag {
                name: s, value: t, ..
            } => {
                let (t_str, new_cont) = t.to_r(cont);
                let is_empty = matches!(t.as_ref(), Lang::Empty(_));
                // Canonical representation (see validation_variant_d_union.md §2):
                // tag identity in position 1, payload under `body`, class enriched
                // with the union name (when the tag belongs to a declared union)
                // plus `Tag`/`list`. This makes the literal interchangeable with
                // the value produced by the variant constructor `V(...)`, so
                // `match` and the variant validators apply to both origins.
                let class = match find_union_for_tag(s, cont) {
                    Some(union_name) => format!("c('{}', '{}', 'Tag', 'list')", s, union_name),
                    None => format!("c('{}', 'Tag', 'list')", s),
                };
                let value = if is_empty {
                    format!("structure(list('{}'), class = {})", s, class)
                } else {
                    format!(
                        "structure(list('{}', body = {}), class = {})",
                        s, t_str, class
                    )
                };
                (value, new_cont)
            }
            Lang::Null(_) => ("NULL".to_string(), cont.clone()),
            Lang::Empty(_) => ("NA".to_string(), cont.clone()),
            Lang::Lines { value: exps, .. } => {
                Translatable::from(cont.clone()).join(exps, "\n").into()
            }
            Lang::Return { value: exp, .. } => Translatable::from(cont.clone())
                .add("return ")
                .to_r(exp)
                .into(),
            Lang::Lambda {
                parameters: params,
                body: bloc,
                ..
            } => {
                let param_names: Vec<String> = params
                    .iter()
                    .map(|p: &Lang| match p {
                        Lang::Variable { name, .. } => name.clone(),
                        _ => "x".to_string(),
                    })
                    .collect();
                (
                    format!(
                        "function({}) {{ {} }}",
                        param_names.join(", "),
                        bloc.to_r(cont).0
                    ),
                    cont.clone(),
                )
            }
            Lang::VecBlock { value: bloc, .. } => (bloc.to_string(), cont.clone()),
            Lang::Library { value: name, .. } => (format!("library({})", name), cont.clone()),
            Lang::Match {
                target: exp,
                branches,
                ..
            } => (
                to_pattern_match_statement((**exp).clone(), branches, cont),
                cont.clone(),
            ),
            Lang::Exp { value: exp, .. } => (exp.clone(), cont.clone()),
            Lang::ForLoop {
                identifier: var,
                expression: iterator,
                body,
                ..
            } => Translatable::from(cont.clone())
                .add("for (")
                .to_r_safe(var)
                .add(" in ")
                .to_r_safe(iterator)
                .add(") {\n")
                .to_r_safe(body)
                .add("\n}")
                .into(),
            Lang::RFunction {
                parameters: vars,
                body,
                ..
            } => Translatable::from(cont.clone())
                .add("function (")
                .join(vars, ", ")
                .add(") \n")
                .add(body)
                .add("\n")
                .into(),
            Lang::Signature { .. } => ("".to_string(), cont.clone()),
            Lang::TypeConstructor { .. } => ("".to_string(), cont.clone()),
            Lang::Alias {
                identifier: ident,
                target_type: typ,
                ..
            } => {
                let name = Var::from_language(*ident.clone())
                    .map(|v| v.get_name())
                    .unwrap_or_default();
                // An alias mixing a record-kinded generic with a concrete
                // record (e.g. `type Animator<%T> <- %T & list { animations:
                // [Animation] }`) has no fixed runtime shape on the generic
                // side — TypR doesn't monomorphize, and R has no class for
                // "any record" — so only the concrete `Record` side carries
                // fields worth validating. Substitute the alias's type with
                // that concrete side so it falls into the existing
                // `Type::Record` pipeline below (constructor/annotator/
                // validator), instead of the unrelated union-alias
                // `Type::Operator` catch-all, which doesn't apply here and
                // previously produced no R code at all for this shape,
                // leaving `validate_Animator`/`as.Animator` referenced
                // elsewhere but never defined.
                let typ_for_dispatch: Type = match typ {
                    Type::Operator(TypeOperator::Intersection, t1, t2, _) => {
                        match (t1.reduce(cont), t2.reduce(cont)) {
                            (record @ Type::Record(_, _), other) if other.has_generic() => record,
                            (other, record @ Type::Record(_, _)) if other.has_generic() => record,
                            _ => typ.clone(),
                        }
                    }
                    _ => typ.clone(),
                };
                match &typ_for_dispatch {
                    Type::Record(fields, _) => {
                        let mut sorted_fields: Vec<&ArgumentType> = fields.iter().collect();
                        sorted_fields.sort_by_key(|f| f.get_argument_str());
                        let params = sorted_fields
                            .iter()
                            .map(|f| f.get_argument_str())
                            .collect::<Vec<_>>()
                            .join(", ");
                        // Each field is only added to `explicit` when the caller
                        // actually supplied it (`missing()`, not a NULL default):
                        // a record-typed `.spread` (spread_operator3.md) may cover
                        // the field instead, and `missing()` never forces the
                        // argument promise, so unsupplied fields stay lazy/unevaluated.
                        let explicit_lines = sorted_fields
                            .iter()
                            .map(|f| {
                                let n = f.get_argument_str();
                                format!("  if (!missing({n})) explicit[[\"{n}\"]] <- {n}")
                            })
                            .collect::<Vec<_>>()
                            .join("\n");
                        // Constructor: collect the explicitly-supplied fields, merge
                        // them over `.spread` (explicit wins, extra spread fields are
                        // kept), then delegate entirely to the annotator. It neither
                        // adds classes nor validates.
                        let constructor = format!(
                            "{name} <- function({params}, .spread = NULL) {{\n  explicit <- list()\n{explicit_lines}\n  x <- typr_spread_record(explicit, .spread)\n  as.{name}(x)\n}}"
                        );
                        // Structural supertypes: any record alias whose fields are a
                        // strict subset of this alias's fields. They are included in
                        // the S3 class vector so that methods defined on the supertype
                        // dispatch correctly to subtype values.
                        //
                        // Candidates come from the whole-program `record_aliases`
                        // registry, not just `cont.aliases()`: the latter is scoped
                        // to the current module body, so a supertype declared in a
                        // sibling `mod` file (e.g. `Position` while transpiling
                        // `Circle` in another file) would otherwise never be found,
                        // even though R's S3 classes have no module privacy.
                        // Built as an ordered Vec (not a HashMap) and deduplicated by
                        // first occurrence, so candidate order — and therefore the
                        // final sort below — stays deterministic across runs.
                        let mut seen_names: std::collections::HashSet<String> =
                            std::collections::HashSet::new();
                        let candidates: Vec<(String, Type)> = cont
                            .aliases()
                            .map(|(var, typ)| (var.get_name(), typ.clone()))
                            .chain(cont.record_aliases.iter().cloned())
                            .filter(|(other_name, _)| seen_names.insert(other_name.clone()))
                            .collect();
                        let mut supertype_entries: Vec<(String, usize)> = candidates
                            .into_iter()
                            .filter_map(|(other_name, typ)| {
                                if other_name == name {
                                    return None;
                                }
                                if let Type::Record(other_fields, _) = typ {
                                    if fields.is_superset(&other_fields) && other_fields != *fields
                                    {
                                        Some((other_name, other_fields.len()))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            })
                            .collect();
                        // More-specific supertypes (more fields) first for correct S3
                        // dispatch order; ties broken by name for determinism.
                        supertype_entries.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
                        let supertype_class_str = if supertype_entries.is_empty() {
                            String::new()
                        } else {
                            let names = supertype_entries
                                .iter()
                                .map(|(n, _)| format!("\"{n}\""))
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!(", {names}")
                        };
                        // Annotator: the single entry point that adds the class
                        // (idempotently), runs the internal validator, then the
                        // user validator (`validate` S3 generic, default = identity).
                        let annotator = format!(
                            "as.{name} <- function(x) {{\n  if (!inherits(x, \"{name}\")) class(x) <- c(\"{name}\"{supertype_class_str}, \"list\")\n  x <- validate_{name}(x)\n  x <- validate(x)\n  x\n}}"
                        );
                        let fields_quoted = sorted_fields
                            .iter()
                            .map(|f| format!("\"{}\"", f.get_argument_str()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        // Per-field type invariants, checked by class (`inherits`).
                        // Fields whose type has no reliable nominal class are only
                        // checked for presence (above).
                        let field_checks = sorted_fields
                            .iter()
                            .filter_map(|f| {
                                let n = f.get_argument_str();
                                record_field_class(&f.body_type(), cont).map(|cls| {
                                    format!(
                                        "  if (!inherits(x[[\"{n}\"]], \"{cls}\")) stop(\"Validation failed for type {name}: field '{n}' must be of class {cls}\")"
                                    )
                                })
                            })
                            .collect::<Vec<_>>()
                            .join("\n");
                        let field_checks_block = if field_checks.is_empty() {
                            String::new()
                        } else {
                            format!("{field_checks}\n")
                        };
                        // Internal validator: pure structural invariants only.
                        // It must not reconstruct the value (that would recurse
                        // back through the constructor / annotator).
                        let validator = format!(
                            "validate_{name} <- function(x) {{\n  required_fields <- c({fields_quoted})\n  missing_fields <- setdiff(required_fields, names(x))\n  if (length(missing_fields) > 0) {{\n    stop(paste0(\"Validation failed for type {name}: missing fields: \", paste(missing_fields, collapse = \", \")))\n  }}\n{field_checks_block}  x\n}}"
                        );
                        (
                            format!("{constructor}\n{annotator}\n{validator}"),
                            cont.clone(),
                        )
                    }
                    // Dataframe alias (`type Df <- dataframe[#N]{ ... }` /
                    // `df[3]{ ... }`): same constructor/annotator/validator
                    // pipeline as a record alias, but columns are assembled
                    // into a `data.frame` instead of a `list`, the class
                    // chain carries `"data.frame"`, and a concrete size
                    // index (`df[3]{...}`) additionally checks `nrow(x)`.
                    Type::Vec(VecType::DataFrame, size, fields_type, _)
                        if matches!(fields_type.as_ref(), Type::Record(_, _)) =>
                    {
                        use crate::components::r#type::tint::Tint;
                        let fields = match fields_type.as_ref() {
                            Type::Record(fields, _) => fields,
                            _ => unreachable!(),
                        };
                        let mut sorted_fields: Vec<&ArgumentType> = fields.iter().collect();
                        sorted_fields.sort_by_key(|f| f.get_argument_str());
                        let params = sorted_fields
                            .iter()
                            .map(|f| f.get_argument_str())
                            .collect::<Vec<_>>()
                            .join(", ");
                        let explicit_lines = sorted_fields
                            .iter()
                            .map(|f| {
                                let n = f.get_argument_str();
                                format!("  if (!missing({n})) explicit[[\"{n}\"]] <- {n}")
                            })
                            .collect::<Vec<_>>()
                            .join("\n");
                        // Constructor: collect the explicitly-supplied columns,
                        // merge them over `.spread`, assemble into a
                        // `data.frame`, then delegate to the annotator.
                        let constructor = format!(
                            "{name} <- function({params}, .spread = NULL) {{\n  explicit <- list()\n{explicit_lines}\n  x <- typr_spread_record(explicit, .spread)\n  as.{name}(do.call(data.frame, c(x, list(stringsAsFactors = FALSE))))\n}}"
                        );
                        // Annotator: adds the class (idempotently), runs the
                        // internal validator, then the user validator.
                        let annotator = format!(
                            "as.{name} <- function(x) {{\n  if (!inherits(x, \"{name}\")) class(x) <- c(\"{name}\", \"data.frame\", \"list\")\n  x <- validate_{name}(x)\n  x <- validate(x)\n  x\n}}"
                        );
                        let fields_quoted = sorted_fields
                            .iter()
                            .map(|f| format!("\"{}\"", f.get_argument_str()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        // Per-column type invariants, checked by class
                        // (`inherits`) on the column vector.
                        let field_checks = sorted_fields
                            .iter()
                            .filter_map(|f| {
                                let n = f.get_argument_str();
                                record_field_class(&f.body_type(), cont).map(|cls| {
                                    format!(
                                        "  if (!inherits(x[[\"{n}\"]], \"{cls}\")) stop(\"Validation failed for type {name}: column '{n}' must be of class {cls}\")"
                                    )
                                })
                            })
                            .collect::<Vec<_>>()
                            .join("\n");
                        let field_checks_block = if field_checks.is_empty() {
                            String::new()
                        } else {
                            format!("{field_checks}\n")
                        };
                        // Row-count check: only emitted when the size index is
                        // a concrete literal (`df[3]{...}`); a generic (`#N`)
                        // or unconstrained (`df{...}`) size imposes no check.
                        let size_check = if let Type::Integer(Tint::Val(n), _) = size.as_ref() {
                            format!(
                                "  if (nrow(x) != {n}) stop(paste0(\"Validation failed for type {name}: expected {n} rows, got \", nrow(x)))\n"
                            )
                        } else {
                            String::new()
                        };
                        let validator = format!(
                            "validate_{name} <- function(x) {{\n  if (!is.data.frame(x)) stop(\"Validation failed for type {name}: expected a data.frame\")\n  required_fields <- c({fields_quoted})\n  missing_fields <- setdiff(required_fields, names(x))\n  if (length(missing_fields) > 0) {{\n    stop(paste0(\"Validation failed for type {name}: missing columns: \", paste(missing_fields, collapse = \", \")))\n  }}\n{field_checks_block}{size_check}  x\n}}"
                        );
                        (
                            format!("{constructor}\n{annotator}\n{validator}"),
                            cont.clone(),
                        )
                    }
                    // Vector alias (`type V <- Vec[#N, T]` / `Vec[3, T]` /
                    // `Vec[T]`): no class is added (R already distinguishes
                    // atomic vector kinds via `is.*`/implicit class), so the
                    // pipeline is the same shape as a primitive alias —
                    // constructor delegates straight to the validator — plus
                    // an optional length check when the size index is a
                    // concrete literal.
                    Type::Vec(VecType::Vector, size, elem_type, _) => {
                        use crate::components::r#type::tint::Tint;
                        let constructor =
                            format!("{name} <- function(x) {{\n  validate_{name}(x)\n}}");
                        let elem_check = record_field_class(elem_type.as_ref(), cont).map(|cls| {
                            format!(
                                "  if (!inherits(x, \"{cls}\")) stop(\"Validation failed for type {name}: expected vector of {cls}\")\n"
                            )
                        }).unwrap_or_default();
                        let size_check = if let Type::Integer(Tint::Val(n), _) = size.as_ref() {
                            format!(
                                "  if (length(x) != {n}) stop(paste0(\"Validation failed for type {name}: expected length {n}, got \", length(x)))\n"
                            )
                        } else {
                            String::new()
                        };
                        let validator = format!(
                            "validate_{name} <- function(x) {{\n{elem_check}{size_check}  x\n}}"
                        );
                        (format!("{constructor}\n{validator}"), cont.clone())
                    }
                    // Array alias (`type A <- [#N, T]` / `[3, T]` / bare
                    // brackets, and the explicit `Array[#N, T]` spelling —
                    // both `VecType::S3` and `VecType::Array` produce the
                    // same runtime shape): the literal/constructor-call
                    // transpilation already wraps elements in `typed_vec`
                    // (see `Lang::Array`/`Lang::ArrayConstructorCall`), so
                    // this alias just needs to register the alias name as a
                    // `typed_vec` subclass — same annotator/validator shape
                    // as a record alias — so generic functions written
                    // against `typed_vec` dispatch on it.
                    Type::Vec(VecType::S3, size, elem_type, _)
                    | Type::Vec(VecType::Array, size, elem_type, _) => {
                        use crate::components::r#type::tint::Tint;
                        let constructor = format!(
                            "{name} <- function(x) {{\n  if (!inherits(x, \"typed_vec\")) x <- typed_vec(x)\n  as.{name}(x)\n}}"
                        );
                        let annotator = format!(
                            "as.{name} <- function(x) {{\n  if (!inherits(x, \"{name}\")) class(x) <- c(\"{name}\", class(x))\n  x <- validate_{name}(x)\n  x <- validate(x)\n  x\n}}"
                        );
                        let elem_check = record_field_class(elem_type.as_ref(), cont).map(|cls| {
                            format!(
                                "  if (!all(vapply(x$data, inherits, logical(1), \"{cls}\"))) stop(\"Validation failed for type {name}: expected elements of class {cls}\")\n"
                            )
                        }).unwrap_or_default();
                        let size_check = if let Type::Integer(Tint::Val(n), _) = size.as_ref() {
                            format!(
                                "  if (length(x) != {n}) stop(paste0(\"Validation failed for type {name}: expected length {n}, got \", length(x)))\n"
                            )
                        } else {
                            String::new()
                        };
                        let validator = format!(
                            "validate_{name} <- function(x) {{\n  if (!inherits(x, \"typed_vec\")) stop(\"Validation failed for type {name}: expected typed_vec\")\n{elem_check}{size_check}  x\n}}"
                        );
                        (
                            format!("{constructor}\n{annotator}\n{validator}"),
                            cont.clone(),
                        )
                    }
                    Type::Operator(_, _, _, _) => {
                        // Union alias: generate the full constructor/annotator/
                        // validator pipeline for each variant (see
                        // validation_variant_d_union.md).
                        let union_name = &name;
                        let members = flatten_operator_union(typ);
                        // Sort for deterministic output
                        let mut members_vec: Vec<Type> = members.into_iter().collect();
                        members_vec.sort_by_key(|t| t.pretty2());
                        let constructors: Vec<String> = members_vec
                            .iter()
                            .filter_map(|member| match member {
                                Type::Tag(variant_name, inner, _) => Some(tag_variant_pipeline(
                                    variant_name,
                                    union_name,
                                    inner.as_ref(),
                                )),
                                Type::Alias(alias_name, _, _, _) => {
                                    // Look up the record fields for this alias
                                    let record_fields = cont
                                        .aliases()
                                        .find(|(var, _)| var.get_name() == *alias_name)
                                        .and_then(|(_, t)| {
                                            if let Type::Record(fields, _) = t {
                                                Some(fields.clone())
                                            } else {
                                                None
                                            }
                                        });
                                    if let Some(fields) = record_fields {
                                        let mut sorted: Vec<&ArgumentType> =
                                            fields.iter().collect();
                                        sorted.sort_by_key(|f| f.get_argument_str());
                                        let params = sorted
                                            .iter()
                                            .map(|f| f.get_argument_str())
                                            .collect::<Vec<_>>()
                                            .join(", ");
                                        let field_args = sorted
                                            .iter()
                                            .map(|f| {
                                                let n = f.get_argument_str();
                                                format!("{n} = {n}")
                                            })
                                            .collect::<Vec<_>>()
                                            .join(", ");
                                        Some(format!(
                                            "{alias_name} <- function({params}) {{\n  structure(list({field_args}), class = c(\"{alias_name}\", \"{union_name}\", \"list\"))\n}}"
                                        ))
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                            .collect();
                        (constructors.join("\n"), cont.clone())
                    }
                    Type::Integer(tint, _) => {
                        use crate::components::r#type::tint::Tint;
                        let validator = match tint {
                            Tint::Val(i) => format!(
                                "validate_{name} <- function(x) {{\n  if (!is.integer(x)) stop(\"Validation failed for type {name}: expected int\")\n  if (x != {i}L) stop(\"Validation failed for type {name}: expected literal {i}\")\n  x\n}}"
                            ),
                            Tint::Unknown => format!(
                                "validate_{name} <- function(x) {{\n  if (!is.integer(x)) stop(\"Validation failed for type {name}: expected int\")\n  x\n}}"
                            ),
                        };
                        let constructor =
                            format!("{name} <- function(x) {{\n  validate_{name}(x)\n}}");
                        (format!("{constructor}\n{validator}"), cont.clone())
                    }
                    Type::Char(tchar, _) => {
                        use crate::components::r#type::tchar::Tchar;
                        let validator = match tchar {
                            Tchar::Val(s) => format!(
                                "validate_{name} <- function(x) {{\n  if (!is.character(x)) stop(\"Validation failed for type {name}: expected char\")\n  if (x != '{s}') stop(\"Validation failed for type {name}: expected literal '{s}'\")\n  x\n}}"
                            ),
                            Tchar::Unknown => format!(
                                "validate_{name} <- function(x) {{\n  if (!is.character(x)) stop(\"Validation failed for type {name}: expected char\")\n  x\n}}"
                            ),
                        };
                        let constructor =
                            format!("{name} <- function(x) {{\n  validate_{name}(x)\n}}");
                        (format!("{constructor}\n{validator}"), cont.clone())
                    }
                    Type::Boolean(tbool, _) => {
                        use crate::components::r#type::tbool::Tbool;
                        let validator = match tbool {
                            Tbool::Val(b) => {
                                let r_val = if *b { "TRUE" } else { "FALSE" };
                                format!(
                                    "validate_{name} <- function(x) {{\n  if (!is.logical(x)) stop(\"Validation failed for type {name}: expected bool\")\n  if (x != {r_val}) stop(\"Validation failed for type {name}: expected literal {r_val}\")\n  x\n}}"
                                )
                            }
                            Tbool::Unknown => format!(
                                "validate_{name} <- function(x) {{\n  if (!is.logical(x)) stop(\"Validation failed for type {name}: expected bool\")\n  x\n}}"
                            ),
                        };
                        let constructor =
                            format!("{name} <- function(x) {{\n  validate_{name}(x)\n}}");
                        (format!("{constructor}\n{validator}"), cont.clone())
                    }
                    Type::Number(tnum, _) => {
                        use crate::components::r#type::tnumber::Tnum;
                        let validator = match tnum {
                            Tnum::Val(v) => format!(
                                "validate_{name} <- function(x) {{\n  if (!is.numeric(x)) stop(\"Validation failed for type {name}: expected num\")\n  if (x != {v}) stop(\"Validation failed for type {name}: expected literal {v}\")\n  x\n}}"
                            ),
                            Tnum::Unknown => format!(
                                "validate_{name} <- function(x) {{\n  if (!is.numeric(x)) stop(\"Validation failed for type {name}: expected num\")\n  x\n}}"
                            ),
                        };
                        let constructor =
                            format!("{name} <- function(x) {{\n  validate_{name}(x)\n}}");
                        (format!("{constructor}\n{validator}"), cont.clone())
                    }
                    Type::Tag(tag_name, inner_type, _) => {
                        let body_validation = tag_body_validation(&name, inner_type.as_ref());
                        let validator = format!(
                            "validate_{name} <- function(x) {{\n  if (x[[1]] != '{tag_name}') stop(\"Validation failed for type {name}: expected tag '{tag_name}'\")\n{body_validation}\n  x\n}}"
                        );
                        (validator, cont.clone())
                    }
                    // Alias-to-alias (`type Object <- Circle;`): transparent, so reuse
                    // the target's own constructor/validator pipeline under this name
                    // instead of generating nothing — otherwise the module export step
                    // below (`module$Object <- Object`) would reference a binding that
                    // was never created.
                    Type::Alias(target_name, ..) => {
                        (format!("{name} <- {target_name}"), cont.clone())
                    }
                    _ => ("".to_string(), cont.clone()),
                }
            }
            Lang::UnionConstructor {
                variant_name,
                fields,
                ..
            } => {
                if fields.is_empty() {
                    (format!("{}()", variant_name), cont.clone())
                } else {
                    let (body, current_cont) = Translatable::from(cont.clone())
                        .join_arg_val(fields, ", ")
                        .into();
                    (format!("{}({})", variant_name, body), current_cont)
                }
            }
            Lang::KeyValue {
                key: k, value: v, ..
            } => (format!("{} = {}", k, v.to_r(cont).0), cont.clone()),
            Lang::Vector { value: vals, .. } => {
                let res = "c(".to_string()
                    + &vals
                        .iter()
                        .map(|x: &Lang| x.to_r(cont).0)
                        .collect::<Vec<_>>()
                        .join(", ")
                    + ")";
                (res, cont.to_owned())
            }
            Lang::Not { value: exp, .. } => (format!("!{}", exp.to_r(cont).0), cont.clone()),
            Lang::Sequence { body: vals, .. } => {
                let res = if !vals.is_empty() {
                    "c(".to_string()
                        + &vals
                            .iter()
                            .map(|x: &Lang| "list(".to_string() + &x.to_r(cont).0 + ")")
                            .collect::<Vec<_>>()
                            .join(", ")
                        + ")"
                } else {
                    "c(list())".to_string()
                };
                (res, cont.to_owned())
            }
            Lang::TestBlock {
                value: body,
                help_data: h,
            } => {
                let file_name = h
                    .get_file_data()
                    .map(|(name, _)| format!("test-{}", name))
                    .unwrap_or_else(|| "test-unknown".to_string())
                    .replace("TypR/", "")
                    .replace(".ty", ".R");

                let file_path = format!("tests/testthat/{}", file_name);
                let body_str = body.to_r(cont).0;
                // RFC-TR-031: bring `@testable` private members of the enclosing
                // module into scope (e.g. `sq <- Math$.test_sq`) so the test body
                // can call them by their bare names.
                let content = if cont.test_preamble.is_empty() {
                    body_str
                } else {
                    format!("{}\n{}", cont.test_preamble.join("\n"), body_str)
                };

                let _ = write_output_file(&file_path, &content);
                ("".to_string(), cont.clone())
            }
            Lang::JSBlock(exp, _id, _h) => {
                let js_cont = Context::default(); //TODO get js context from memory
                let res = exp.to_js(&js_cont).0;
                (format!("'{}{}'", JS_HEADER, res), cont.clone())
            }
            Lang::WhileLoop {
                condition, body, ..
            } => (
                format!(
                    "while ({}) {{\n{}\n}}",
                    condition.to_r(cont).0,
                    body.to_r(cont).0
                ),
                cont.clone(),
            ),
            Lang::Loop { body, .. } => (
                format!("while (TRUE) {{\n{}\n}}", body.to_r(cont).0),
                cont.clone(),
            ),
            Lang::Break(_) => ("break".to_string(), cont.clone()),
            Lang::Next(_) => ("next".to_string(), cont.clone()),
            Lang::NA(_) => ("NA".to_string(), cont.clone()),
            Lang::Module {
                name,
                body,
                module_position: position,
                config,
                ..
            } => {
                let name_str = if (name == "main") && (config.environment == Environment::Project) {
                    "a_main"
                } else {
                    name
                };

                // A module that writes its own roxygen2 file (External in Project)
                // gets a dedicated frame so the `@include` deps generated inside its
                // body land in *its* header rather than the enclosing file's.
                let writes_own_file = matches!(position, ModulePosition::External)
                    && config.environment == Environment::Project;
                if writes_own_file {
                    push_include_frame();
                }

                // Re-derive the module's internal typing context so body elements
                // can resolve sibling definitions (e.g. a `Test { ... }` block or a
                // top-level call referencing an internal `let`). The outer `cont`
                // only knows the module itself, not its private members.
                let module_expr = if body.len() > 1 {
                    Lang::Lines {
                        value: body.to_vec(),
                        help_data: HelpData::default(),
                    }
                } else {
                    body.first()
                        .cloned()
                        .unwrap_or(Lang::Empty(HelpData::default()))
                };
                let mut inner_cont =
                    typing(&cont.clone().set_in_module_body(), &module_expr).context;

                // RFC-TR-031: in a test build, give any `Test { ... }` block in this
                // module access to its `@testable` private members by binding their
                // bare names to the exposed `M$.test_<name>` aliases at the top of the
                // generated test file (see `Lang::TestBlock` below).
                if cont.get_test_mode() {
                    let preamble: Vec<String> = body
                        .iter()
                        .filter_map(|lang| match lang {
                            Lang::Let {
                                variable: var,
                                is_testable: true,
                                ..
                            } => Var::from_language(*var.clone()).map(|v| {
                                let raw = v.get_name();
                                format!("{} <- {}$`.test_{}`", raw, name_str, raw)
                            }),
                            _ => None,
                        })
                        .collect();
                    inner_cont = inner_cont.set_test_preamble(preamble);
                }

                // C1: partition body into file-level imports (mod foo; / External sub-modules)
                // and runtime content. Imports are processed first so their side-effects
                // (file writes, register_include) happen before the new.env binding, and
                // their output is emitted outside the local({}) block.
                let (import_langs, runtime_langs): (Vec<_>, Vec<_>) =
                    body.iter().partition(|lang| {
                        matches!(
                            lang,
                            Lang::ModuleImport { .. }
                                | Lang::Module {
                                    module_position: ModulePosition::External,
                                    ..
                                }
                        )
                    });

                let imports_parts: Vec<String> = import_langs
                    .iter()
                    .map(|lang| lang.to_r(&inner_cont).0)
                    .filter(|s| !s.is_empty())
                    .collect();
                let imports_preamble = if imports_parts.is_empty() {
                    String::new()
                } else {
                    imports_parts.join("\n") + "\n"
                };

                let body_content = runtime_langs
                    .iter()
                    .map(|lang| lang.to_r(&inner_cont).0)
                    .collect::<Vec<_>>()
                    .join("\n");

                // Build exports (inside local) and generics (outside local) for @pub/@export members
                let mut exports: Vec<String> = Vec::new();
                let mut generics: Vec<String> = Vec::new();
                let mut generic_exports: Vec<String> = Vec::new();
                // RFC-TR-032: @export members also get a top-level #' @export re-export
                let mut package_exports: Vec<String> = Vec::new();

                for lang in body.iter() {
                    if let Lang::Let {
                        variable: var,
                        is_public: true,
                        is_export,
                        ..
                    } = lang
                    {
                        if let Some(v) = Var::from_language(*var.clone()) {
                            let raw_name = v.get_name();
                            let typed_name = v.clone().display_type(cont).get_name();

                            // Export the (possibly type-suffixed) member into the module env
                            exports.push(format!("{}${} <- {}", name_str, typed_name, typed_name));

                            // RFC-TR-032: @export also surfaces as a package-level function
                            if *is_export {
                                package_exports.push(format!(
                                    "#' @export\n{} <- {}${}",
                                    raw_name, name_str, typed_name
                                ));
                            }

                            // For typed functions: register as S3 method and create generic
                            let var_type = v.get_type();
                            if !var_type.is_empty() && typed_name != raw_name {
                                let class_name = cont.get_class_unquoted(&var_type);
                                exports.push(format!(
                                    "registerS3method(\"{}\", \"{}\", {})",
                                    raw_name, class_name, typed_name
                                ));

                                let generic_def = format!(
                                    "{} <- function(x, ...) UseMethod(\"{}\")",
                                    raw_name, raw_name
                                );
                                if !generics.contains(&generic_def) {
                                    generics.push(generic_def);
                                    generic_exports
                                        .push(format!("{}${} <- {}", name_str, raw_name, raw_name));
                                }

                                // The method implementation itself (e.g. `do.Object`) is only
                                // bound inside `local({...})`, so it never becomes a top-level
                                // binding that load_module.R's dependency-copy step can see.
                                // `registerS3method` alone doesn't help here either: it relies
                                // on a real package namespace, which a `sys.source`'d module env
                                // is not. Re-expose it at top level (outside local) from the
                                // module env so UseMethod can find `typed_name` via normal
                                // lexical scoping from any file that imports this module.
                                generic_exports
                                    .push(format!("{} <- {}${}", typed_name, name_str, typed_name));
                            }
                        }
                    }
                    // RFC-TR-032: in a test build, expose `@testable` (and implied-testable
                    // `@pub`/`@export`) members as `M$.test_<name>` while keeping them
                    // private in the module's regular API.
                    if cont.get_test_mode() {
                        if let Lang::Let {
                            variable: var,
                            is_testable: true,
                            ..
                        } = lang
                        {
                            if let Some(v) = Var::from_language(*var.clone()) {
                                let raw_name = v.get_name();
                                // Reference the actual (possibly type-suffixed)
                                // binding emitted in the module body.
                                let typed_name = v.clone().display_type(cont).get_name();
                                exports.push(format!(
                                    "{}$`.test_{}` <- {}",
                                    name_str, raw_name, typed_name
                                ));
                            }
                        }
                    }
                    // Export @pub opaque type constructors into the module environment
                    if let Lang::Alias {
                        identifier: var,
                        is_public: true,
                        ..
                    } = lang
                    {
                        if let Some(v) = Var::from_language(*var.clone()) {
                            let alias_name = v.get_name();
                            exports.push(format!("{}${} <- {}", name_str, alias_name, alias_name));
                        }
                    }
                }

                let exports_str = if exports.is_empty() {
                    String::new()
                } else {
                    "\n".to_string() + &exports.join("\n")
                };

                let generics_defs_str = if generics.is_empty() {
                    String::new()
                } else {
                    generics.join("\n") + "\n"
                };

                let generic_exports_str = if generic_exports.is_empty() {
                    String::new()
                } else {
                    "\n".to_string() + &generic_exports.join("\n")
                };

                let package_exports_str = if package_exports.is_empty() {
                    String::new()
                } else {
                    "\n".to_string() + &package_exports.join("\n")
                };

                let content = format!(
                    "{}{}{} <- new.env(parent = emptyenv())\nlocal({{\n{}{}\n}}){}{}",
                    generics_defs_str,
                    imports_preamble,
                    name_str,
                    body_content,
                    exports_str,
                    generic_exports_str,
                    package_exports_str
                );

                match (position, config.environment) {
                    (ModulePosition::Internal, _) => (content, cont.clone()),
                    // In WASM mode, inline all external modules instead of writing files
                    (ModulePosition::External, Environment::Wasm) => {
                        let file_path = format!("{}.R", name_str);
                        let _ = write_output_file(&file_path, &content);
                        (content, cont.clone())
                    }
                    (ModulePosition::External, Environment::StandAlone)
                    | (ModulePosition::External, Environment::Repl) => {
                        let file_path = format!("{}.R", name_str);
                        let _ = write_output_file(&file_path, &content);
                        (format!("source('{}')", file_path), cont.clone())
                    }
                    (ModulePosition::External, Environment::Project) => {
                        let file_path = format!("R/{}.R", name_str);
                        // Drain the deps collected within this module's body and emit
                        // them as top-level `@include` tags in this file's header.
                        let nested = pop_include_frame();
                        let nested_includes = nested
                            .iter()
                            .map(|f| format!("#' @include {}\n", f))
                            .collect::<String>();
                        let project_preamble = "#' @include std.R\n#' @include generic_functions.R\n#' @include types.R\n";
                        let _ = write_output_file(
                            &file_path,
                            &format!("{}{}{}", project_preamble, nested_includes, content),
                        );
                        // The enclosing file depends on this one: hoist the tag to its
                        // header instead of emitting it inline inside a `local({...})`.
                        register_include(&format!("{}.R", name_str));
                        (String::new(), cont.clone())
                    }
                }
            }
            Lang::UseModule {
                module_path,
                selector,
                ..
            } => {
                use crate::components::language::use_lang::UseSelector;

                // Build the R accessor prefix: A::B::C → A$B$C
                let r_path = module_path.join("$");

                // Resolve the module type from context to enumerate public members for wildcards
                let mod_type_opt = (|| {
                    let root = cont
                        .get_type_from_variable(&Var::from_name(&module_path[0]))
                        .ok()?;
                    let mut current = root;
                    for seg in module_path.iter().skip(1) {
                        current = current
                            .to_module_type()
                            .ok()?
                            .get_type_from_name(seg)
                            .ok()?;
                    }
                    current.to_module_type().ok()
                })();

                let bindings: Vec<String> = match selector {
                    UseSelector::Wildcard => mod_type_opt
                        .map(|mt| {
                            mt.get_public_members()
                                .iter()
                                .map(|m| {
                                    let name = m.get_argument_str();
                                    format!("{} <- {}${}", name, r_path, name)
                                })
                                .collect()
                        })
                        .unwrap_or_default(),
                    UseSelector::Items(items) => items
                        .iter()
                        .map(|item| {
                            let local_name = item.alias.as_deref().unwrap_or(&item.name);
                            format!("{} <- {}${}", local_name, r_path, item.name)
                        })
                        .collect(),
                };

                (bindings.join("\n"), cont.clone())
            }
            Lang::ModuleImport { .. } => ("".to_string(), cont.clone()),
            // `Self:{ field = expr, ...base }` (generic_constructor.md §5):
            // resolve the actual R constructor straight from `base`'s type
            // rather than from the literal name "Self" — by the
            // type-checking rule `base`'s type is always Self or a subtype
            // of it, so this is exactly the constructor that should run.
            Lang::ConstructorCall {
                type_name,
                fields,
                spreads,
                ..
            } if type_name == "Self" => {
                let base_typ = spreads
                    .first()
                    .map(|e| typing(cont, e).value)
                    .unwrap_or_else(|| Type::Any(HelpData::default()));
                let resolved_name = match &base_typ {
                    Type::Alias(alias_name, ..) => cont
                        .aliases()
                        .find(|(var, _)| var.get_name() == *alias_name)
                        .map(|(_, t)| matches!(t, Type::Record(_, _)))
                        .unwrap_or(false)
                        .then(|| alias_name.clone()),
                    _ => None,
                };
                match (resolved_name, spreads.first()) {
                    (Some(name), Some(spread_expr)) => {
                        // Same codegen as the plain runtime-spread
                        // ConstructorCall path below, with the resolved
                        // alias name substituted for "Self".
                        let (spread_r, current_cont) = spread_expr.to_r(cont);
                        let (body, current_cont) = if fields.is_empty() {
                            (format!(".spread = {}", spread_r), current_cont)
                        } else {
                            let (overrides, next_cont) = Translatable::from(current_cont)
                                .join_arg_val(fields, ", ")
                                .into();
                            (format!("{}, .spread = {}", overrides, spread_r), next_cont)
                        };
                        (format!("{}({})", name, body), current_cont)
                    }
                    (None, Some(spread_expr)) => {
                        // No nominal constructor to call: fall back to the
                        // generic spread()-merge used for plain `{ f=e, ...x }`
                        // record literals.
                        let (base, current_cont) = spread_expr.to_r(cont);
                        if fields.is_empty() {
                            (base, current_cont)
                        } else {
                            let (overrides, current_cont) = Translatable::from(current_cont)
                                .join_arg_val(fields, ", ")
                                .into();
                            (
                                format!("spread({}, list({}))", base, overrides),
                                current_cont,
                            )
                        }
                    }
                    // No spread: ill-typed (type-checking already reports
                    // SelfOutsideContext); nothing sensible to emit.
                    (_, None) => ("NULL".to_string(), cont.clone()),
                }
            }
            Lang::ConstructorCall {
                module_path,
                type_name,
                fields,
                spreads,
                ..
            } if !spreads.is_empty() => {
                // Single runtime `...source` spread (spread_operator3.md): pass the
                // explicit fields plus the spread source straight to the generated
                // constructor's `.spread` parameter — it merges them at runtime via
                // `typr_spread_record` (explicit fields win, extra fields on the
                // source are preserved, unlike the old do.call/spread/select).
                let spread_expr = spreads.first().expect("checked non-empty above");
                let (spread_r, current_cont) = spread_expr.to_r(cont);
                let qualified = if module_path.is_empty() {
                    type_name.clone()
                } else {
                    format!("{}${}", module_path.join("$"), type_name)
                };
                let (body, current_cont) = if fields.is_empty() {
                    (format!(".spread = {}", spread_r), current_cont)
                } else {
                    let (overrides, next_cont) = Translatable::from(current_cont)
                        .join_arg_val(fields, ", ")
                        .into();
                    (format!("{}, .spread = {}", overrides, spread_r), next_cont)
                };
                (format!("{}({})", qualified, body), current_cont)
            }
            Lang::ConstructorCall {
                module_path,
                type_name,
                fields,
                spread,
                help_data: h,
                ..
            } => {
                // With a spread present, statically expand every record field absent
                // from `fields` into a `source$field` access (RFC-TR-033 §5: static
                // expansion, no runtime merge so the record's R class is preserved).
                let all_fields: Vec<ArgumentValue> = match spread {
                    Some((spread_path, spread_var, _)) => {
                        let resolved_alias = if module_path.is_empty() {
                            cont.get_type_from_aliases(&Var::from_name(type_name))
                        } else {
                            resolve_module_member_type(cont, module_path, type_name)
                        };
                        let record_fields = resolved_alias.and_then(|t| match t.reduce(cont) {
                            Type::Record(fields, _) => Some(fields),
                            _ => None,
                        });
                        let receiver = {
                            let qualifier = spread_path.split_first().map(|(first, rest)| {
                                rest.iter()
                                    .fold(Var::from_name(first).to_language(), |acc, seg| {
                                        Lang::Operator {
                                            operator: Op::Dollar(h.clone()),
                                            rhs: Box::new(acc),
                                            lhs: Box::new(Var::from_name(seg).to_language()),
                                            help_data: h.clone(),
                                        }
                                    })
                            });
                            match qualifier {
                                Some(qualifier) => Lang::Operator {
                                    operator: Op::Dollar(h.clone()),
                                    rhs: Box::new(qualifier),
                                    lhs: Box::new(Var::from_name(spread_var).to_language()),
                                    help_data: h.clone(),
                                },
                                None => Var::from_name(spread_var).to_language(),
                            }
                        };
                        let provided: std::collections::HashSet<String> =
                            fields.iter().map(|f| f.get_argument()).collect();
                        let synthetic = record_fields
                            .into_iter()
                            .flatten()
                            .filter(|rf| !provided.contains(&rf.get_argument_str()))
                            .map(|rf| {
                                let field_access = Lang::Operator {
                                    operator: Op::Dollar(h.clone()),
                                    rhs: Box::new(receiver.clone()),
                                    lhs: Box::new(
                                        Var::from_name(&rf.get_argument_str()).to_language(),
                                    ),
                                    help_data: h.clone(),
                                };
                                ArgumentValue(rf.get_argument_str(), field_access)
                            });
                        fields.iter().cloned().chain(synthetic).collect()
                    }
                    None => fields.clone(),
                };
                let (body, current_cont) = Translatable::from(cont.clone())
                    .join_arg_val(&all_fields, ", ")
                    .into();
                let qualified = if module_path.is_empty() {
                    type_name.clone()
                } else {
                    format!("{}${}", module_path.join("$"), type_name)
                };
                (format!("{}({})", qualified, body), current_cont)
            }
            Lang::ArrayConstructorCall {
                type_name,
                elements,
                help_data: h,
            } => {
                let resolved_alias = cont
                    .get_type_from_aliases(&Var::from_name(type_name))
                    .map(|t| t.reduce(cont));
                if let Some(Type::Vec(VecType::Vector, ..)) = resolved_alias {
                    // Plain vector alias (`type V <- Vec[#N, T]`): the runtime
                    // value is a bare R vector (no `dim`/`typed_vec` wrapper, see
                    // the Vec constructor pipeline in `Lang::Alias`), so the
                    // elements are simply collected with `c(...)`.
                    let inner = elements
                        .iter()
                        .map(|el| el.to_r(cont).0)
                        .collect::<Vec<_>>()
                        .join(", ");
                    (format!("{}(c({}))", type_name, inner), cont.clone())
                } else {
                    let temp_array = Lang::Array {
                        value: elements.clone(),
                        help_data: h.clone(),
                    };
                    let typ = temp_array.typing(cont).value;
                    let dimension = ArrayType::try_from(typ)
                        .expect("array constructor call should have an array type")
                        .get_shape()
                        .map(|sha| format!("c({})", sha))
                        .unwrap_or_else(|| "c(0)".to_string());
                    let lin_array = temp_array
                        .linearize_array()
                        .iter()
                        .map(|lang| lang.to_r(cont).0)
                        .collect::<Vec<_>>()
                        .join(", ");
                    let inner = if lin_array.is_empty() {
                        format!("typed_vec(dim = {})", dimension)
                    } else {
                        format!("typed_vec({}, dim = {})", lin_array, dimension)
                    };
                    (format!("{}({})", type_name, inner), cont.clone())
                }
            }
            Lang::Import { .. } | Lang::Test { .. } | Lang::Use { .. } => {
                ("".to_string(), cont.clone())
            }
            Lang::ValidatingCast {
                expression,
                type_name,
                literal_type,
                ..
            } => {
                let expr_r = expression.to_r(cont).0;
                match literal_type {
                    // Inline structural type (`as! [Any, int]` and friends):
                    // call the auto-generated `as.ArrayN`-style cast (see
                    // `Context::get_type_anotations`) registered for it at
                    // typing time, instead of a named `validate_<name>`.
                    Some(t) => (
                        format!("{} |> {}", expr_r, cont.get_type_anotation(t)),
                        cont.clone(),
                    ),
                    None => (format!("validate_{}({})", type_name, expr_r), cont.clone()),
                }
            }
            _ => ("".to_string(), cont.clone()),
        };

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::components::context::config::{Config, Environment};
    use crate::components::context::Context;
    use crate::components::error_message::help_data::HelpData;
    use crate::components::language::{Lang, ModulePosition};
    use crate::processes::transpiling::translatable::RTranslatable;
    use crate::utils::fluent_parser::FluentParser;

    #[test]
    fn test_escape_r_string() {
        use super::escape_r_string;
        assert_eq!(escape_r_string("hello"), r#""hello""#);
        assert_eq!(escape_r_string(r#"say "hi""#), r#""say \"hi\"""#);
        assert_eq!(escape_r_string(r"a\b"), r#""a\\b""#);
        assert_eq!(escape_r_string("line1\nline2"), r#""line1\nline2""#);
    }

    #[test]
    fn test_validating_cast_transpiles_to_validate_call() {
        let r_code = FluentParser::new()
            .push("type Person <- list { name: char, age: int };")
            .run()
            .check_transpiling("x as! Person");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Person(x)"),
            "expected validate_Person(x), got: {}",
            r_str
        );
    }

    #[test]
    fn test_validating_cast_type_is_alias() {
        let typ = FluentParser::new()
            .push("type Person <- list { name: char, age: int };")
            .run()
            .check_typing("x as! Person");
        assert!(
            typ.pretty2().contains("Person"),
            "expected Alias(Person), got: {}",
            typ.pretty2()
        );
    }

    #[test]
    fn test_validating_cast_literal_array_type() {
        let r_code = FluentParser::new().check_transpiling("c() as! [Any, int]");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("c() |> as.Array0()"),
            "expected c() |> as.Array0(), got: {}",
            r_str
        );
    }

    #[test]
    fn test_validating_cast_literal_vec_and_array_keywords() {
        // `Vec[...]` / `Array[...]` prefixes are equivalent to the bare
        // `[...]` form for an inline (non-aliased) cast target.
        let r_code = FluentParser::new().check_transpiling("c() as! Vec[Any, int]");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("c() |> as.Array0()"),
            "expected c() |> as.Array0(), got: {}",
            r_str
        );
    }

    #[test]
    fn test_validating_cast_literal_type_dedup() {
        // Two casts to the same structural type reuse the same auto-generated
        // alias instead of registering a new one each time.
        let r_code = FluentParser::new()
            .push("let a <- c() as! [Any, int];")
            .run()
            .push("let b <- c() as! [Any, int];")
            .run()
            .get_r_code();
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert_eq!(
            r_str.matches("as.Array0()").count(),
            2,
            "expected both casts to reuse as.Array0, got: {}",
            r_str
        );
    }

    #[test]
    fn test_alias_record_generates_validator() {
        let r_code =
            FluentParser::new().check_transpiling("type Person <- list { name: char, age: int };");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Person <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("required_fields"),
            "expected field validation, got: {}",
            r_str
        );
    }

    #[test]
    fn test_record_kinded_generic_param_transpiles_without_panic() {
        // Regression for a `get_class` panic ("%T has no class equivalent")
        // when a function parameter/alias is typed with a record-kinded
        // generic (`%T`), e.g. `type Animator<%T> <- %T & list {...}` plus
        // a function spreading a `%T`-typed parameter. `%T` has no fixed
        // runtime R class, so it must fall back to the same `Generic`
        // convention used for plain `T`, never panic.
        let fp = FluentParser::new()
            .push("type Animator<%T> <- %T & list { extra: int };")
            .run()
            .push("let combine <- fn(target: %T): %T { let more <- :{ extra = 1 }; :{ ...target, ...more } };")
            .run();
        assert_eq!(fp.get_last_log(), "The logs are empty");
        let r_code = fp
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("spread(target, more)"),
            "expected the spread merge to transpile, got:\n{}",
            r_code
        );
    }

    #[test]
    fn test_generic_intersection_alias_generates_validator() {
        // An alias mixing a record-kinded generic with a concrete record
        // (`%T & list {...}`) used to produce no R code at all for itself
        // (it fell into the union-alias `Type::Operator` catch-all), while
        // `as! Animator` elsewhere still emitted a call to `validate_Animator`
        // — a dangling reference at actual R runtime. The concrete side's
        // fields are the only part with a fixed runtime shape, so the
        // constructor/annotator/validator pipeline should be generated from
        // them, exactly like a plain `Type::Record` alias.
        let r_code = FluentParser::new()
            .check_transpiling("type Animator<%T> <- %T & list { animations: int };");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("Animator <- function(animations, .spread = NULL)"),
            "expected a constructor for the concrete side's fields, got:\n{}",
            r_str
        );
        assert!(
            r_str.contains("as.Animator <- function(x)"),
            "expected an annotator, got:\n{}",
            r_str
        );
        assert!(
            r_str.contains("validate_Animator <- function(x)")
                && r_str.contains("required_fields <- c(\"animations\")"),
            "expected a validator checking the concrete field, got:\n{}",
            r_str
        );
    }

    #[test]
    fn test_external_module_project_generates_include() {
        use super::{reset_include_stack, take_main_includes};
        reset_include_stack();
        let module = Lang::Module {
            name: "MyModule".to_string(),
            body: vec![],
            module_position: ModulePosition::External,
            config: Config::default().set_environment(Environment::Project),
            help_data: HelpData::default(),
        };
        let context = Context::default().set_environment(Environment::Project);
        let (r_code, _) = module.to_r(&context);
        // The include is no longer emitted inline; it is hoisted to the enclosing
        // file's header via the include stack.
        assert_eq!(r_code, "", "got: {}", r_code);
        let includes = take_main_includes();
        assert!(
            includes.contains(&"MyModule.R".to_string()),
            "expected MyModule.R to be registered, got: {:?}",
            includes
        );
    }

    #[test]
    fn test_module_transpilation_with_pub() {
        let r_code = FluentParser::new()
            .push("module Math { let sq <- 2; @pub let pi <- 3; };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Math <- new.env(parent = emptyenv())"),
            "missing env init: {}",
            r_code
        );
        assert!(
            r_code.contains("local({"),
            "missing local block: {}",
            r_code
        );
        assert!(
            r_code.contains("Math$pi <- pi"),
            "missing public export: {}",
            r_code
        );
        assert!(
            !r_code.contains("Math$sq"),
            "private member should not be exported: {}",
            r_code
        );
    }

    #[test]
    fn test_testable_member_hidden_in_normal_build() {
        // Without --test, a @testable member stays fully private.
        let r_code = FluentParser::new()
            .push("module Math { @testable let sq <- fn(x: int): int { x * x }; };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            !r_code.contains(".test_sq"),
            "testable member must not be exposed in a normal build: {}",
            r_code
        );
    }

    #[test]
    fn test_testable_member_exposed_in_test_build() {
        // With test_mode on, a @testable member is exposed as M$.test_<name>.
        let r_code = FluentParser::new()
            .set_context(Context::empty().set_test_mode(true))
            .push("module Math { @testable let sq <- fn(x: int): int { x * x }; };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Math$`.test_sq` <- sq"),
            "testable member must be exposed in a test build: {}",
            r_code
        );
    }

    #[test]
    fn test_module_transpilation_no_pub() {
        let r_code = FluentParser::new()
            .push("module Empty { let x <- 1; };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Empty <- new.env(parent = emptyenv())"),
            "missing env init: {}",
            r_code
        );
        assert!(
            r_code.contains("local({"),
            "missing local block: {}",
            r_code
        );
        assert!(
            !r_code.contains("Empty$x"),
            "private member should not be exported: {}",
            r_code
        );
    }

    #[test]
    fn test_module_s3_registration_for_typed_pub_fn() {
        let r_code = FluentParser::new()
            .push("module Math { @pub let double <- fn(x: Integer): Integer { x }; };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Math <- new.env(parent = emptyenv())"),
            "missing env init: {}",
            r_code
        );
        assert!(
            r_code.contains("registerS3method(\"double\", \"integer\", double.integer)"),
            "missing S3 registration: {}",
            r_code
        );
        assert!(
            r_code.contains("double <- function(x, ...) UseMethod(\"double\")"),
            "missing generic: {}",
            r_code
        );
        assert!(
            r_code.contains("Math$double <- double"),
            "missing generic export: {}",
            r_code
        );
    }

    #[test]
    fn test_module_no_trailing_semicolon() {
        let r_code = FluentParser::new()
            .push("module Geo { @pub let pi <- 3; }")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Geo <- new.env(parent = emptyenv())"),
            "missing env init: {}",
            r_code
        );
        assert!(
            r_code.contains("Geo$pi <- pi"),
            "missing public export: {}",
            r_code
        );
    }

    #[test]
    fn test_import_module() {
        let r_code = FluentParser::new()
            .push("module Math { @pub let pi <- 3; }")
            .push("import Math")
            .run()
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Math <- new.env(parent = emptyenv())"),
            "missing env init: {}",
            r_code
        );
    }

    #[test]
    fn test_import_module_as_alias() {
        let r_code = FluentParser::new()
            .push("module Math { @pub let pi <- 3; }")
            .push("import Math as Maths")
            .run()
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Math <- new.env(parent = emptyenv())"),
            "missing env init: {}",
            r_code
        );
        assert!(
            r_code.contains("Maths <- Math") || r_code.contains("`Maths` <- Math"),
            "missing alias assignment: {}",
            r_code
        );
    }

    #[test]
    fn test_use_items_transpiles() {
        let r_code = FluentParser::new()
            .push("module Math { @pub let pi <- 3; @pub let e <- 2; };")
            .push("use Math::{pi, e as euler};")
            .run()
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("pi <- Math$pi"),
            "missing pi binding: {}",
            r_code
        );
        assert!(
            r_code.contains("euler <- Math$e"),
            "missing euler binding: {}",
            r_code
        );
    }

    #[test]
    fn test_use_wildcard_transpiles() {
        let r_code = FluentParser::new()
            .push("module Math { @pub let pi <- 3; @pub let e <- 2; let secret <- 0; };")
            .push("use Math::*;")
            .run()
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("pi <- Math$pi"),
            "missing pi binding: {}",
            r_code
        );
        assert!(
            r_code.contains("e <- Math$e"),
            "missing e binding: {}",
            r_code
        );
        assert!(
            !r_code.contains("secret <- Math$secret"),
            "private member must not be imported: {}",
            r_code
        );
    }

    // RFC-TR-032 @export tests

    #[test]
    fn test_export_at_top_level_prepends_roxygen_tag() {
        let r_code = FluentParser::new()
            .push("@export let answer <- 42;")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("#' @export"),
            "missing #' @export tag: {}",
            r_code
        );
        assert!(r_code.contains("answer"), "missing assignment: {}", r_code);
    }

    #[test]
    fn test_export_in_module_is_public_and_package_exported() {
        let r_code = FluentParser::new()
            .push("module Math { @export let norm <- fn(x: int): int { x }; };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Math$norm <- norm"),
            "missing module export: {}",
            r_code
        );
        assert!(
            r_code.contains("#' @export"),
            "missing roxygen export tag: {}",
            r_code
        );
        assert!(
            r_code.contains("norm <- Math$norm"),
            "missing package-level re-export: {}",
            r_code
        );
    }

    #[test]
    fn test_export_in_module_test_build_adds_test_alias() {
        let r_code = FluentParser::new()
            .set_context(Context::empty().set_test_mode(true))
            .push("module Math { @export let norm <- fn(x: int): int { x }; };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Math$`.test_norm` <- norm"),
            "export member must get .test_ alias in test build: {}",
            r_code
        );
    }

    #[test]
    fn test_pub_in_module_test_build_adds_test_alias() {
        let r_code = FluentParser::new()
            .set_context(Context::empty().set_test_mode(true))
            .push("module Math { @pub let pi <- 3; };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Math$`.test_pi` <- pi"),
            "@pub member must get .test_ alias in test build (RFC-TR-032 §3.2): {}",
            r_code
        );
    }

    #[test]
    fn test_array_constructor_call_transpilation() {
        let r_code = FluentParser::new()
            .push("type Bits <- [Any, int];")
            .run()
            .push("let b <- Bits:[1, 2, 3];")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_code.contains("Bits(typed_vec("),
            "expected Bits(...) constructor: {}",
            r_code
        );
        assert!(
            r_code.contains("dim = c(3)"),
            "expected dimension annotation: {}",
            r_code
        );
    }

    #[test]
    fn test_record_alias_return_no_constructor_pipe() {
        // A function returning a record alias must NOT get `|> TypeName()` in
        // its body — the constructor takes specific named fields, not a single
        // value, so piping the body result through it would fail at runtime.
        let r_code = FluentParser::new()
            .push("type Point <- list { x: int, y: int };")
            .run()
            .push("let incr <- fn(p: Point): Point { Point:{x: (p$x+1), y: (p$y+1)} };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        // The method definition must not contain `|> Point()` inside the body
        assert!(
            !r_code.contains("}) |> Point()"),
            "record alias output conversion should not be added: {}",
            r_code
        );
        // The method should still be wrapped by as.Generic() or as.FunctionN() for S3 dispatch
        assert!(
            r_code.contains("|> as.Generic()") || r_code.contains("|> Function"),
            "function type annotation should still be applied: {}",
            r_code
        );
    }

    #[test]
    fn test_alias_int_generates_validator() {
        let r_code = FluentParser::new().check_transpiling("type Meters <- int;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Meters <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("is.integer"),
            "expected is.integer check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_alias_char_generates_validator() {
        let r_code = FluentParser::new().check_transpiling("type Name <- char;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Name <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("is.character"),
            "expected is.character check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_alias_bool_generates_validator() {
        let r_code = FluentParser::new().check_transpiling("type Flag <- bool;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Flag <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("is.logical"),
            "expected is.logical check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_alias_num_generates_validator() {
        let r_code = FluentParser::new().check_transpiling("type Real <- num;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Real <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("is.numeric"),
            "expected is.numeric check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_validating_cast_int() {
        let r_code = FluentParser::new()
            .push("type Meters <- int;")
            .run()
            .check_transpiling("x as! Meters");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Meters(x)"),
            "expected validate_Meters(x), got: {}",
            r_str
        );
    }

    #[test]
    fn test_tag_alias_char_generates_validator() {
        let r_code = FluentParser::new().check_transpiling("type Hello <- .Hello(char);");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Hello <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x[[1]] != 'Hello'"),
            "expected tag name check, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x[[\"body\"]]"),
            "expected body field check, got: {}",
            r_str
        );
        assert!(
            r_str.contains("is.character"),
            "expected is.character check on body, got: {}",
            r_str
        );
    }

    #[test]
    fn test_tag_alias_int_generates_validator() {
        let r_code = FluentParser::new().check_transpiling("type Count <- .Count(int);");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Count <- function(x)"),
            "expected validator, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x[[1]] != 'Count'"),
            "expected tag name check, got: {}",
            r_str
        );
        assert!(
            r_str.contains("is.integer"),
            "expected is.integer check on body, got: {}",
            r_str
        );
    }

    #[test]
    fn test_tag_alias_with_alias_body_calls_nested_validator() {
        let r_code = FluentParser::new()
            .push("type Name <- char;")
            .run()
            .check_transpiling("type Tagged <- .Tagged(Name);");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Tagged <- function(x)"),
            "expected validator, got: {}",
            r_str
        );
        assert!(
            r_str.contains("validate_Name(x[[\"body\"]])"),
            "expected nested validator call, got: {}",
            r_str
        );
    }

    #[test]
    fn test_union_variant_generates_full_pipeline() {
        // Each union variant gets the same constructor/annotator/validator
        // contract as records (validation_variant_d_union.md §3).
        let r_str = FluentParser::new()
            .check_transpiling("type Shape <- .Circle(num) | .Nothing;")
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        // Constructor (payload variant) builds the canonical value then delegates.
        assert!(
            r_str.contains("Circle <- function(x) {")
                && r_str.contains("v <- list(\"Circle\", body = x)")
                && r_str.contains("as.Circle(v)"),
            "expected Circle constructor, got: {r_str}"
        );
        // Annotator sets the enriched class idempotently and validates.
        assert!(
            r_str.contains("as.Circle <- function(x) {")
                && r_str.contains("class(x) <- c(\"Circle\", \"Shape\", \"Tag\", \"list\")")
                && r_str.contains("x <- validate_Circle(x)")
                && r_str.contains("x <- validate(x)"),
            "expected Circle annotator, got: {r_str}"
        );
        // Internal validator checks tag identity and payload type.
        assert!(
            r_str.contains("validate_Circle <- function(x) {")
                && r_str.contains("x[[1]] != 'Circle'")
                && r_str.contains("is.numeric(x[[\"body\"]])"),
            "expected Circle validator, got: {r_str}"
        );
        // Empty variant has a zero-arg constructor and no body.
        assert!(
            r_str.contains("Nothing <- function() {") && r_str.contains("x <- list(\"Nothing\")"),
            "expected Nothing constructor, got: {r_str}"
        );
    }

    #[test]
    fn test_tag_literal_canonical_representation() {
        // A `.Circle(..)` literal must produce the same runtime shape as the
        // variant constructor: tag in position 1, payload under `body`, class
        // enriched with the union name (validation_variant_d_union.md §2).
        let r_str = FluentParser::new()
            .push("type Shape <- .Circle(num) | .Square(num);")
            .run()
            .check_transpiling(".Circle(3.14)")
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_str.contains("structure(list('Circle', body =")
                && r_str.contains("class = c('Circle', 'Shape', 'Tag', 'list')"),
            "expected canonical tag literal with union class, got: {r_str}"
        );
    }

    #[test]
    fn test_tag_literal_without_union_omits_union_class() {
        // A tag with no declared union still uses the canonical shape, but the
        // class carries no union name.
        let r_str = FluentParser::new()
            .check_transpiling(".Loose(1)")
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_str.contains("structure(list('Loose', body =")
                && r_str.contains("class = c('Loose', 'Tag', 'list')"),
            "expected canonical tag literal without union class, got: {r_str}"
        );
    }

    #[test]
    fn test_literal_char_alias_generates_exact_validator() {
        let r_code = FluentParser::new().check_transpiling("type Hello <- \"hello\";");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Hello <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x != 'hello'"),
            "expected literal equality check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_literal_int_alias_generates_exact_validator() {
        let r_code = FluentParser::new().check_transpiling("type Byte <- 89;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Byte <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x != 89L"),
            "expected literal equality check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_literal_num_alias_generates_exact_validator() {
        let r_code = FluentParser::new().check_transpiling("type Pi <- 3.14;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Pi <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x != 3.14"),
            "expected literal equality check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_literal_bool_true_alias_generates_exact_validator() {
        let r_code = FluentParser::new().check_transpiling("type Yes <- true;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_Yes <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x != TRUE"),
            "expected literal TRUE check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_literal_bool_false_alias_generates_exact_validator() {
        let r_code = FluentParser::new().check_transpiling("type No <- false;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains("validate_No <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x != FALSE"),
            "expected literal FALSE check, got: {}",
            r_str
        );
    }

    #[test]
    fn test_record_subtype_includes_supertype_in_s3_class() {
        // Person has all fields of Position, so Person <: Position.
        // The annotator for Person must include "Position" in its class vector
        // so that S3 methods defined on Position dispatch for Person values.
        let r_str = FluentParser::new()
            .push("type Position <- list{ position: int };")
            .run()
            .push("type Person <- list{ name: char, age: int, position: int };")
            .run()
            .get_r_code()
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_str.contains("class(x) <- c(\"Person\", \"Position\", \"list\")"),
            "expected Person's annotator to include Position, got: {r_str}"
        );
    }

    #[test]
    fn test_record_without_supertype_keeps_plain_class() {
        // Position has no other record supertype, so its class stays c("Position", "list").
        let r_str = FluentParser::new()
            .check_transpiling("type Position <- list{ position: int };")
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            r_str.contains("class(x) <- c(\"Position\", \"list\")"),
            "expected Position's annotator with no supertype, got: {r_str}"
        );
    }
}
