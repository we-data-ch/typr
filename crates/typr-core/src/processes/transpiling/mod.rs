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
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use crate::processes::transpiling::translatable::Translatable;
use crate::processes::type_checking::flatten_operator_union;
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
                operator: Op::Dot(_),
                rhs: e1,
                lhs: e2,
                ..
            }
            | Lang::Operator {
                operator: Op::Pipe(_),
                rhs: e1,
                lhs: e2,
                ..
            } => {
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
                if name == "reduce" {
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
                let (val_str, _) = val.to_simple_r(cont);
                let (typ, _, _) = typing(cont, exp).to_tuple();
                let res = match typ {
                    _ => format!("{}[[{}]]", exp_str, val_str),
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
                    .unwrap_or("logical(0)".to_string());

                (
                    format!("{} |> {}", array, cont.get_type_anotation(&typ)),
                    cont.to_owned(),
                )
            }
            Lang::List { value: args, .. } => {
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
                let (typ, _, _) = typing(cont, self).to_tuple();
                let anotation = cont.get_type_anotation(&typ);
                (
                    format!(
                        "structure(list('{}', body = {}), class = c('.{}', 'Tag')) |> {}",
                        s, t_str, s, anotation
                    ),
                    new_cont,
                )
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
                match typ {
                    Type::Record(fields, _) => {
                        let mut sorted_fields: Vec<&ArgumentType> = fields.iter().collect();
                        sorted_fields.sort_by_key(|f| f.get_argument_str());
                        let params = sorted_fields
                            .iter()
                            .map(|f| f.get_argument_str())
                            .collect::<Vec<_>>()
                            .join(", ");
                        let field_args = sorted_fields
                            .iter()
                            .map(|f| {
                                let n = f.get_argument_str();
                                format!("{n} = {n}")
                            })
                            .collect::<Vec<_>>()
                            .join(", ");
                        let constructor = format!(
                            "{name} <- function({params}) {{\n  structure(list({field_args}), class = c(\"{name}\", \"list\"))\n}}"
                        );
                        let fields_quoted = sorted_fields
                            .iter()
                            .map(|f| format!("\"{}\"", f.get_argument_str()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let field_access = sorted_fields
                            .iter()
                            .map(|f| {
                                let n = f.get_argument_str();
                                format!("{n} = x[[\"{n}\"]]")
                            })
                            .collect::<Vec<_>>()
                            .join(", ");
                        let validator = format!(
                            ".validate_{name} <- function(x) {{\n  required_fields <- c({fields_quoted})\n  missing_fields <- setdiff(required_fields, names(x))\n  if (length(missing_fields) > 0) {{\n    stop(paste0(\"Validation failed for type {name}: missing fields: \", paste(missing_fields, collapse = \", \")))\n  }}\n  {name}({field_access})\n}}"
                        );
                        (format!("{constructor}\n{validator}"), cont.clone())
                    }
                    Type::Operator(_, _, _, _) => {
                        // Union alias: generate constructors for each variant
                        let union_name = &name;
                        let members = flatten_operator_union(typ);
                        // Sort for deterministic output
                        let mut members_vec: Vec<Type> = members.into_iter().collect();
                        members_vec.sort_by_key(|t| t.pretty2());
                        let constructors: Vec<String> = members_vec
                            .iter()
                            .filter_map(|member| match member {
                                Type::Tag(variant_name, inner, _) => {
                                    match inner.as_ref() {
                                        Type::Empty(_) => Some(format!(
                                            "{variant_name} <- function() {{\n  structure(list(), class = c(\"{variant_name}\", \"{union_name}\", \"list\"))\n}}"
                                        )),
                                        _ => {
                                            // Tag with payload: wrap as single-field constructor
                                            Some(format!(
                                                "{variant_name} <- function(x) {{\n  structure(list(x), class = c(\"{variant_name}\", \"{union_name}\", \"list\"))\n}}"
                                            ))
                                        }
                                    }
                                }
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
                                ".validate_{name} <- function(x) {{\n  if (!is.integer(x)) stop(\"Validation failed for type {name}: expected int\")\n  if (x != {i}L) stop(\"Validation failed for type {name}: expected literal {i}\")\n  x\n}}"
                            ),
                            Tint::Unknown => format!(
                                ".validate_{name} <- function(x) {{\n  if (!is.integer(x)) stop(\"Validation failed for type {name}: expected int\")\n  x\n}}"
                            ),
                        };
                        (validator, cont.clone())
                    }
                    Type::Char(tchar, _) => {
                        use crate::components::r#type::tchar::Tchar;
                        let validator = match tchar {
                            Tchar::Val(s) => format!(
                                ".validate_{name} <- function(x) {{\n  if (!is.character(x)) stop(\"Validation failed for type {name}: expected char\")\n  if (x != '{s}') stop(\"Validation failed for type {name}: expected literal '{s}'\")\n  x\n}}"
                            ),
                            Tchar::Unknown => format!(
                                ".validate_{name} <- function(x) {{\n  if (!is.character(x)) stop(\"Validation failed for type {name}: expected char\")\n  x\n}}"
                            ),
                        };
                        (validator, cont.clone())
                    }
                    Type::Boolean(tbool, _) => {
                        use crate::components::r#type::tbool::Tbool;
                        let validator = match tbool {
                            Tbool::Val(b) => {
                                let r_val = if *b { "TRUE" } else { "FALSE" };
                                format!(
                                    ".validate_{name} <- function(x) {{\n  if (!is.logical(x)) stop(\"Validation failed for type {name}: expected bool\")\n  if (x != {r_val}) stop(\"Validation failed for type {name}: expected literal {r_val}\")\n  x\n}}"
                                )
                            }
                            Tbool::Unknown => format!(
                                ".validate_{name} <- function(x) {{\n  if (!is.logical(x)) stop(\"Validation failed for type {name}: expected bool\")\n  x\n}}"
                            ),
                        };
                        (validator, cont.clone())
                    }
                    Type::Number(tnum, _) => {
                        use crate::components::r#type::tnumber::Tnum;
                        let validator = match tnum {
                            Tnum::Val(v) => format!(
                                ".validate_{name} <- function(x) {{\n  if (!is.numeric(x)) stop(\"Validation failed for type {name}: expected num\")\n  if (x != {v}) stop(\"Validation failed for type {name}: expected literal {v}\")\n  x\n}}"
                            ),
                            Tnum::Unknown => format!(
                                ".validate_{name} <- function(x) {{\n  if (!is.numeric(x)) stop(\"Validation failed for type {name}: expected num\")\n  x\n}}"
                            ),
                        };
                        (validator, cont.clone())
                    }
                    Type::Tag(tag_name, inner_type, _) => {
                        let body_validation = match inner_type.as_ref() {
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
                                "\n  if (is.null(x[[\"body\"]])) stop(\"Validation failed for type {name}: missing 'body' field\")\n  .validate_{alias_name}(x[[\"body\"]])"
                            ),
                            _ => format!(
                                "\n  if (is.null(x[[\"body\"]])) stop(\"Validation failed for type {name}: missing 'body' field\")"
                            ),
                        };
                        let validator = format!(
                            ".validate_{name} <- function(x) {{\n  if (x[[1]] != '{tag_name}') stop(\"Validation failed for type {name}: expected tag '{tag_name}'\")\n{body_validation}\n  x\n}}"
                        );
                        (validator, cont.clone())
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
                let content = body.to_r(cont).0;

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

                let body_content = body
                    .iter()
                    .map(|lang| lang.to_r(cont).0)
                    .collect::<Vec<_>>()
                    .join("\n");

                // Build exports (inside local) and generics (outside local) for @pub members
                let mut exports: Vec<String> = Vec::new();
                let mut generics: Vec<String> = Vec::new();
                let mut generic_exports: Vec<String> = Vec::new();

                for lang in body.iter() {
                    if let Lang::Let {
                        variable: var,
                        is_public: true,
                        ..
                    } = lang
                    {
                        if let Some(v) = Var::from_language(*var.clone()) {
                            let raw_name = v.get_name();
                            let typed_name = v.clone().display_type(cont).get_name();

                            // Export the (possibly type-suffixed) member into the module env
                            exports.push(format!("{}${} <- {}", name_str, typed_name, typed_name));

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

                let content = format!(
                    "{}{} <- new.env(parent = emptyenv())\nlocal({{\n{}{}\n}}){}",
                    generics_defs_str, name_str, body_content, exports_str, generic_exports_str
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
                        let _ = write_output_file(&file_path, &content);
                        (format!("#' @include {}.R", name_str), cont.clone())
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
            Lang::ConstructorCall {
                type_name, fields, ..
            } => {
                let (body, current_cont) = Translatable::from(cont.clone())
                    .join_arg_val(fields, ", ")
                    .into();
                (format!("{}({})", type_name, body), current_cont)
            }
            Lang::ArrayConstructorCall {
                type_name,
                elements,
                help_data: h,
            } => {
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
                    "logical(0)".to_string()
                } else {
                    format!("typed_vec({}, dim = {})", lin_array, dimension)
                };
                (format!("{}({})", type_name, inner), cont.clone())
            }
            Lang::Import { .. } | Lang::Test { .. } | Lang::Use { .. } => {
                ("".to_string(), cont.clone())
            }
            Lang::ValidatingCast {
                expression,
                type_name,
                ..
            } => {
                let expr_r = expression.to_r(cont).0;
                (format!(".validate_{}({})", type_name, expr_r), cont.clone())
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
            r_str.contains(".validate_Person(x)"),
            "expected .validate_Person(x), got: {}",
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
    fn test_alias_record_generates_validator() {
        let r_code =
            FluentParser::new().check_transpiling("type Person <- list { name: char, age: int };");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains(".validate_Person <- function(x)"),
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
    fn test_external_module_project_generates_include() {
        let module = Lang::Module {
            name: "MyModule".to_string(),
            body: vec![],
            module_position: ModulePosition::External,
            config: Config::default().set_environment(Environment::Project),
            help_data: HelpData::default(),
        };
        let context = Context::default().set_environment(Environment::Project);
        let (r_code, _) = module.to_r(&context);
        assert_eq!(r_code, "#' @include MyModule.R", "got: {}", r_code);
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
        // The method should still be wrapped by Generic() for S3 dispatch
        assert!(
            r_code.contains("|> Generic()") || r_code.contains("|> Function"),
            "function type annotation should still be applied: {}",
            r_code
        );
    }

    #[test]
    fn test_alias_int_generates_validator() {
        let r_code = FluentParser::new().check_transpiling("type Meters <- int;");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains(".validate_Meters <- function(x)"),
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
            r_str.contains(".validate_Name <- function(x)"),
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
            r_str.contains(".validate_Flag <- function(x)"),
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
            r_str.contains(".validate_Real <- function(x)"),
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
            r_str.contains(".validate_Meters(x)"),
            "expected .validate_Meters(x), got: {}",
            r_str
        );
    }

    #[test]
    fn test_tag_alias_char_generates_validator() {
        let r_code = FluentParser::new().check_transpiling("type Hello <- .Hello(char);");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains(".validate_Hello <- function(x)"),
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
            r_str.contains(".validate_Count <- function(x)"),
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
            r_str.contains(".validate_Tagged <- function(x)"),
            "expected validator, got: {}",
            r_str
        );
        assert!(
            r_str.contains(".validate_Name(x[[\"body\"]])"),
            "expected nested validator call, got: {}",
            r_str
        );
    }

    #[test]
    fn test_literal_char_alias_generates_exact_validator() {
        let r_code = FluentParser::new().check_transpiling("type Hello <- \"hello\";");
        let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
        assert!(
            r_str.contains(".validate_Hello <- function(x)"),
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
            r_str.contains(".validate_Byte <- function(x)"),
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
            r_str.contains(".validate_Pi <- function(x)"),
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
            r_str.contains(".validate_Yes <- function(x)"),
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
            r_str.contains(".validate_No <- function(x)"),
            "expected validator function, got: {}",
            r_str
        );
        assert!(
            r_str.contains("x != FALSE"),
            "expected literal FALSE check, got: {}",
            r_str
        );
    }
}
