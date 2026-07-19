use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::help_message::DoubleBuilder;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::help_message::SingleBuilder;
use crate::components::error_message::locatable::Locatable;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use miette::Result;

#[derive(Debug, Clone)]
pub enum TypeError {
    Let(Type, Type),
    Param(Type, Type),
    UndefinedFunction(Var),
    UndefinedVariable(Lang),
    UnmatchingReturnType(Type, Type),
    ImmutableVariable(Var, Var),
    PrivateVariable(Var, Var),
    GenericPatternMatch(Type, Type),
    FieldNotFound((String, HelpData), Type),
    WrongExpression(HelpData),
    WrongIndexing(Type, Type),
    AliasNotFound(Type),
    /// A type alias name resolves to a real alias declared in another module,
    /// but that module's member was never brought into this scope via `use`
    /// — distinguished from `AliasNotFound` (a genuinely unknown name) so the
    /// message can point at the fix — `(alias_name, module_name, position)`.
    AliasNotImported(String, String, HelpData),
    /// A variable or function name resolves to a real member declared in
    /// another module, but that module's member was never brought into this
    /// scope via `use` — the ordinary-name counterpart of `AliasNotImported`.
    /// `(name, module_name, is_public, position)`: `is_public` distinguishes
    /// a straightforward `use M::name;` fix from a member that also needs
    /// `@pub`/`@export` before that `use` will actually work.
    VariableNotImported(String, String, bool, HelpData),
    FunctionNotFound(Var),
    AliasMissingGenerics(String, Vec<String>, Type),
    InterfaceReturnOnly(Type),
    UnknownTypeConstructor(String, HelpData),
    /// A field is provided more than once in a `TypeName:{ ... }` constructor call.
    DuplicateField(String, HelpData),
    /// A field required by the record type isn't covered by explicit fields nor a spread.
    MissingField(String, Type, HelpData),
    /// A `..source` spread variable isn't of the exact same alias as the constructed type.
    SpreadTypeMismatch(Type, Type, HelpData),
    /// A type-level operator (`+ - * /` or `&`) was applied to operand(s) outside
    /// its domain of definition (e.g. `+` on a `char`, `&` on an `int`).
    InvalidTypeOperatorDomain(String, HelpData),
    /// A field annotated `@Type` (named type embedding, E-EMBED-002) doesn't have
    /// kind `Record` — `(field_name, embedded_type, position)`.
    EmbedNonRecord(String, Type, HelpData),
    /// Two or more embedded fields provide a function with the same name
    /// (E-EMBED-001) — `(function_name, field_names, position)`.
    EmbedCollision(String, Vec<String>, HelpData),
    /// An explicit function definition collides with a function already inherited
    /// through named type embedding (E-EMBED-003) —
    /// `(function_name, type_name, source_field_name, position)`.
    EmbedConflict(String, String, String, HelpData),
    /// A generic's kind sigil conflicts with another occurrence of the same
    /// generic name within the same function signature (sigils.md §7) —
    /// `(established_kind_desc, conflicting_kind_desc, position)`.
    KindMismatch(String, String, HelpData),
    /// `Self:{ ... }` (generic_constructor.md §8.1) used with no enclosing
    /// function parameter to anchor it, or without the required `...base`
    /// spread.
    SelfOutsideContext(HelpData),
    /// A parameter with no default value follows a parameter that has one
    /// (e.g. `fn(a: int = 1, b: int): int { ... }`) — `(param_name, position)`.
    NonTrailingDefaultParam(String, HelpData),
    /// A `use module::Name` imports a member that exists but is private (no `@pub`/`@export`) —
    /// `(member_name, module_name, position)`.
    PrivateImport(String, String, HelpData),
    /// A circular dependency was detected during type-checking: module `name` is
    /// already being type-checked when a `use` directive targeting it was
    /// encountered.
    CircularModuleDependency {
        module_name: String,
        help_data: HelpData,
    },
    /// A concrete type doesn't structurally implement an interface: one or
    /// more required methods have no counterpart at all —
    /// `(concrete_type, interface_type, missing_method_names, position)`.
    /// See `interface_satisfaction.rs` / `interface_constructeurs.md`.
    InterfaceNotSatisfied(Type, Type, Vec<String>, HelpData),
    /// A concrete type has a method matching a required interface method by
    /// name, but its signature isn't compatible (contravariant params /
    /// covariant return, RFC §8.1) — `(method_name, required_signature,
    /// found_signature, position)`.
    IncompatibleInterfaceMethod(String, Type, Type, HelpData),
    /// A `match` over a tag-union scrutinee doesn't cover every variant, and
    /// has no catch-all (`_ => ...` / `name => ...`) branch either —
    /// `(missing_tag_names, position)`.
    NonExhaustiveMatch(Vec<String>, HelpData),
    /// A `match` pattern's shape can never match its scrutinee's type at all
    /// (e.g. a tag pattern `.Foo(..)` naming a variant the scrutinee's union
    /// doesn't have, or a tag/tuple/record pattern against a scrutinee that
    /// isn't tag/tuple/record-shaped) — `(pattern_description, scrutinee_type,
    /// position)`.
    PatternTypeMismatch(String, Type, HelpData),
    /// A `match` pattern shape isn't supported by the checker (e.g. a nested
    /// pattern inside a tag/tuple/record pattern) — `(pattern_description,
    /// position)`. Raised instead of silently dropping the bindings the
    /// nested pattern would have introduced.
    UnsupportedPattern(String, HelpData),
    /// A generic alias is referenced with the wrong number of concrete type
    /// arguments (e.g. `Option` bare or `Option<int, char>` where `Option<T>`
    /// takes exactly one) — `(alias_name, expected_arity, found_arity,
    /// position)`. Without this check `reduce_alias`'s `generics.zip(concret_types)`
    /// silently truncates: extra args are dropped, missing ones leave
    /// unsubstituted `Generic` placeholders in the reduced type.
    AliasArityMismatch(String, usize, usize, HelpData),
    /// `break`/`next` used outside of any enclosing loop body — `(keyword,
    /// position)`. R would fail at runtime with a similarly-shaped error, but
    /// silently: catching it at compile time (audit_type_checking.md C2).
    LoopControlOutsideLoop(String, HelpData),
    /// A `DataFrame` literal column isn't vector-shaped — `(column_name,
    /// column_type, position)`. Without this check the DataFrame's index
    /// silently fell back to `Any` (audit_type_checking.md D1).
    DataFrameColumnNotVector(String, Type, HelpData),
    /// Two `DataFrame` literal columns have statically known, differing
    /// lengths — `(first_column_name, first_len, second_column_name,
    /// second_len, position)` (audit_type_checking.md D1).
    DataFrameColumnLengthMismatch(String, i32, String, i32, HelpData),
    /// `Union.Variant:{ field = val }` / `TagName:{ field = val }` (and the
    /// bare, brace-less form) used against a real `Type::Tag` union member.
    /// Tag constructors always take exactly one positional payload
    /// (`.Variant(value)`, or `.Variant(:{...})` when the payload itself is
    /// record-shaped) — never named record-style fields, and never zero
    /// arguments when the payload isn't empty. The generated R constructor
    /// is always `Variant <- function(x) { ... }`, so this construct
    /// type-checked but crashed at runtime with "unused argument"
    /// (audit_type_checking.md U1) before this check existed —
    /// `(variant_name, union_name, position)`.
    TagFieldConstructorNotSupported(String, String, HelpData),
    /// `Union.Variant`/`Union.Variant:{...}` names a variant that isn't a
    /// member of `Union`'s declared union type at all — `(variant_name,
    /// union_name, position)`.
    UnknownUnionVariant(String, String, HelpData),
    /// The called name *is* bound to one or more function signatures, but none
    /// of them accepts the argument types of this call (wrong arity, or no
    /// overload matching the argument types). Distinguished from
    /// `FunctionNotFound` (a genuinely unknown name) so a call like `f()` on a
    /// 1-parameter function reads as an argument mismatch instead of the
    /// misleading "not defined in this scope" —
    /// `(function_name, call_arg_types, available_signatures_pretty, position)`.
    NoMatchingSignature(String, Vec<Type>, Vec<String>, HelpData),
}

impl TypeError {
    /// Get the primary HelpData containing position information for this error.
    pub fn get_help_data(&self) -> Option<HelpData> {
        match self {
            TypeError::Let(t1, _) => Some(t1.get_help_data()),
            TypeError::Param(t1, _) => Some(t1.get_help_data()),
            TypeError::UndefinedFunction(var) => Some(var.get_help_data()),
            TypeError::UndefinedVariable(lang) => Some(lang.get_help_data()),
            TypeError::UnmatchingReturnType(t1, _) => Some(t1.get_help_data()),
            TypeError::ImmutableVariable(var, _) => Some(var.get_help_data()),
            TypeError::PrivateVariable(var, _) => Some(var.get_help_data()),
            TypeError::GenericPatternMatch(t1, _) => Some(t1.get_help_data()),
            TypeError::FieldNotFound((_, h), _) => Some(h.clone()),
            TypeError::WrongExpression(h) => Some(h.clone()),
            TypeError::WrongIndexing(t1, _) => Some(t1.get_help_data()),
            TypeError::AliasNotFound(typ) => Some(typ.get_help_data()),
            TypeError::AliasNotImported(_, _, h) => Some(h.clone()),
            TypeError::VariableNotImported(_, _, _, h) => Some(h.clone()),
            TypeError::FunctionNotFound(var) => Some(var.get_help_data()),
            TypeError::AliasMissingGenerics(_, _, typ) => Some(typ.get_help_data()),
            TypeError::InterfaceReturnOnly(typ) => Some(typ.get_help_data()),
            TypeError::UnknownTypeConstructor(_, h) => Some(h.clone()),
            TypeError::DuplicateField(_, h) => Some(h.clone()),
            TypeError::MissingField(_, _, h) => Some(h.clone()),
            TypeError::SpreadTypeMismatch(_, _, h) => Some(h.clone()),
            TypeError::InvalidTypeOperatorDomain(_, h) => Some(h.clone()),
            TypeError::EmbedNonRecord(_, _, h) => Some(h.clone()),
            TypeError::EmbedCollision(_, _, h) => Some(h.clone()),
            TypeError::EmbedConflict(_, _, _, h) => Some(h.clone()),
            TypeError::KindMismatch(_, _, h) => Some(h.clone()),
            TypeError::SelfOutsideContext(h) => Some(h.clone()),
            TypeError::NonTrailingDefaultParam(_, h) => Some(h.clone()),
            TypeError::PrivateImport(_, _, h) => Some(h.clone()),
            TypeError::CircularModuleDependency { help_data: h, .. } => Some(h.clone()),
            TypeError::InterfaceNotSatisfied(_, _, _, h) => Some(h.clone()),
            TypeError::IncompatibleInterfaceMethod(_, _, _, h) => Some(h.clone()),
            TypeError::NonExhaustiveMatch(_, h) => Some(h.clone()),
            TypeError::PatternTypeMismatch(_, _, h) => Some(h.clone()),
            TypeError::UnsupportedPattern(_, h) => Some(h.clone()),
            TypeError::AliasArityMismatch(_, _, _, h) => Some(h.clone()),
            TypeError::LoopControlOutsideLoop(_, h) => Some(h.clone()),
            TypeError::DataFrameColumnNotVector(_, _, h) => Some(h.clone()),
            TypeError::TagFieldConstructorNotSupported(_, _, h) => Some(h.clone()),
            TypeError::UnknownUnionVariant(_, _, h) => Some(h.clone()),
            TypeError::DataFrameColumnLengthMismatch(_, _, _, _, h) => Some(h.clone()),
            TypeError::NoMatchingSignature(_, _, _, h) => Some(h.clone()),
        }
    }

    /// Get a simple error message without file access (for LSP use).
    pub fn simple_message(&self) -> String {
        match self {
            TypeError::Let(t1, t2) => {
                format!("Type mismatch: expected {}, got {}", t1.pretty(), t2.pretty())
            }
            TypeError::Param(t1, t2) => {
                format!("Parameter type mismatch: expected {}, got {}", t1.pretty(), t2.pretty())
            }
            TypeError::UndefinedFunction(var) => {
                format!("Undefined function: {}", var.get_name())
            }
            TypeError::UndefinedVariable(lang) => {
                if let Some(var) = Var::from_language(lang.clone()) {
                    format!("Undefined variable: {}", var.get_name())
                } else {
                    "Undefined variable".to_string()
                }
            }
            TypeError::UnmatchingReturnType(t1, t2) => {
                format!("Return type mismatch: expected {}, got {}", t1.pretty(), t2.pretty())
            }
            TypeError::ImmutableVariable(var, _) => {
                format!("Cannot assign to immutable variable: {}", var.get_name())
            }
            TypeError::PrivateVariable(var, _) => {
                format!("Cannot access private variable: {}", var.get_name())
            }
            TypeError::GenericPatternMatch(t1, t2) => {
                format!("Cannot pattern match {} => {}", t1.pretty(), t2.pretty())
            }
            TypeError::FieldNotFound((name, _), typ) => {
                format!("Field '{}' not found on type {}", name, typ.pretty())
            }
            TypeError::WrongExpression(_) => "Type error in expression".to_string(),
            TypeError::WrongIndexing(t1, t2) => {
                format!("Cannot index {} with {}", t1.pretty(), t2.pretty())
            }
            TypeError::AliasNotFound(typ) => {
                format!("Type alias not found: {}", typ.pretty())
            }
            TypeError::AliasNotImported(name, module, _) => {
                format!(
                    "Alias '{}' is defined in module '{}' but not imported. Add `use {}::{};` or `use {}::*;`.",
                    name, module, module, name, module
                )
            }
            TypeError::VariableNotImported(name, module, is_public, _) => {
                if *is_public {
                    format!(
                        "'{}' is defined in module '{}' but not imported here. Add `use {}::{};` or `use {}::*;`.",
                        name, module, module, name, module
                    )
                } else {
                    format!(
                        "'{}' is defined in module '{}' but is private and not imported here. Add `use {}::{};` (after marking it `@pub` or `@export` in '{}').",
                        name, module, module, name, module
                    )
                }
            }
            TypeError::FunctionNotFound(var) => {
                format!(
                    "Function '{}' not defined for type {}",
                    var.get_name(),
                    var.get_type().pretty()
                )
            }
            TypeError::AliasMissingGenerics(name, generics, _) => {
                let generics_str = generics.join(", ");
                format!(
                    "The alias {} isn't catching every generics. You should write `type {}<{}> <- ...;` instead. If you don't want them as a parameter, you can replace them with `Any`.",
                    name, name, generics_str
                )
            }
            TypeError::InterfaceReturnOnly(typ) => {
                format!(
                    "Interface type '{}' appears only in return position. Use an opaque type instead: `opaque MyType <- ...`.",
                    typ.pretty()
                )
            }
            TypeError::UnknownTypeConstructor(name, _) => {
                format!(
                    "'{}' is used as a record constructor `{}[N]{{...}}` but is not declared. Add `typeconstructor {}[N] record;`.",
                    name, name, name
                )
            }
            TypeError::DuplicateField(name, _) => {
                format!("Field '{}' is provided more than once", name)
            }
            TypeError::MissingField(name, typ, _) => {
                format!("Field '{}' of type {} is missing", name, typ.pretty())
            }
            TypeError::SpreadTypeMismatch(expected, found, _) => {
                format!(
                    "Spread source has type {}, expected {}",
                    found.pretty(),
                    expected.pretty()
                )
            }
            TypeError::InvalidTypeOperatorDomain(message, _) => message.clone(),
            TypeError::EmbedNonRecord(name, typ, _) => {
                format!("Cannot embed non-record type '{}' in field '{}'", typ.pretty(), name)
            }
            TypeError::EmbedCollision(name, fields, _) => {
                format!(
                    "Function '{}' is provided by multiple embeddings: {}",
                    name,
                    fields.join(", ")
                )
            }
            TypeError::EmbedConflict(name, type_name, field, _) => {
                format!(
                    "Function '{}' already defined in '{}' through embedded field '{}'",
                    name, type_name, field
                )
            }
            TypeError::KindMismatch(expected, actual, _) => {
                format!(
                    "Kind mismatch: generic established as kind `{}` elsewhere but used as kind `{}` here",
                    expected, actual
                )
            }
            TypeError::SelfOutsideContext(_) => {
                "Self is not defined here: no enclosing function parameter to anchor it".to_string()
            }
            TypeError::NonTrailingDefaultParam(name, _) => {
                format!(
                    "Parameter '{}' has no default value, but appears after a parameter that does",
                    name
                )
            }
            TypeError::PrivateImport(member, module, _) => {
                format!(
                    "'{}' is private in '{}': add `@pub` or `@export` before its declaration",
                    member, module
                )
            }
            TypeError::CircularModuleDependency { module_name, .. } => {
                format!(
                    "Circular module dependency: module '{}' is currently being \
                     type-checked — use of it inside its own body is not allowed",
                    module_name
                )
            }
            TypeError::InterfaceNotSatisfied(concrete, interface, missing, _) => {
                format!(
                    "Type {} does not implement interface {}\nmissing function{}: {}",
                    concrete.pretty(),
                    interface.pretty(),
                    if missing.len() > 1 { "s" } else { "" },
                    missing.join(", ")
                )
            }
            TypeError::IncompatibleInterfaceMethod(name, expected, found, _) => {
                format!(
                    "Function {} has incompatible signature\nexpected: {}\nfound:    {}",
                    name,
                    expected.pretty(),
                    found.pretty()
                )
            }
            TypeError::NonExhaustiveMatch(missing, _) => {
                format!(
                    "Non-exhaustive match: missing variant{} {}",
                    if missing.len() > 1 { "s" } else { "" },
                    missing.join(", ")
                )
            }
            TypeError::PatternTypeMismatch(pattern, typ, _) => {
                format!("Pattern {} can never match value of type {}", pattern, typ.pretty())
            }
            TypeError::UnsupportedPattern(pattern, _) => {
                format!("Unsupported match pattern: {}", pattern)
            }
            TypeError::AliasArityMismatch(name, expected, found, _) => {
                format!(
                    "{} expects {} type argument{}, found {}",
                    name,
                    expected,
                    if *expected > 1 { "s" } else { "" },
                    found
                )
            }
            TypeError::LoopControlOutsideLoop(keyword, _) => {
                format!("'{}' used outside of a loop", keyword)
            }
            TypeError::DataFrameColumnNotVector(name, typ, _) => {
                format!("DataFrame column '{}' must be a vector, found {}", name, typ.pretty())
            }
            TypeError::DataFrameColumnLengthMismatch(name1, len1, name2, len2, _) => {
                format!(
                    "DataFrame columns have mismatched lengths: '{}' has {}, '{}' has {}",
                    name1, len1, name2, len2
                )
            }
            TypeError::TagFieldConstructorNotSupported(variant_name, union_name, _) => {
                format!(
                    "'{}' is a tag of union '{}'; it can't be constructed with `:{{ field = val }}` syntax",
                    variant_name, union_name
                )
            }
            TypeError::UnknownUnionVariant(variant_name, union_name, _) => {
                format!("'{}' is not a variant of union '{}'", variant_name, union_name)
            }
            TypeError::NoMatchingSignature(name, arg_types, signatures, _) => {
                format!(
                    "No signature of '{}' matches this call with ({}); available: {}",
                    name,
                    arg_types.iter().map(|t| t.pretty()).collect::<Vec<_>>().join(", "),
                    signatures.join(" | ")
                )
            }
        }
    }
}

/// Default file data when source is not available
fn default_file_data() -> (String, String) {
    ("std.ty".to_string(), String::new())
}

/// Returns (file_data, (offset, span_len)) — both clamped so miette never goes OutOfBounds.
/// When the source text is empty (e.g. a stdlib type with no user-file location), returns (0, 0).
fn safe_file_pos(help_data: &HelpData, span_len: usize) -> ((String, String), (usize, usize)) {
    let (file, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
    if text.is_empty() {
        return ((file, text), (0, 0));
    }
    let offset = help_data.get_offset().min(text.len().saturating_sub(1));
    let safe_len = span_len.min(text.len() - offset);
    ((file, text), (offset, safe_len))
}

// main
impl ErrorMsg for TypeError {
    fn display(self) -> String {
        let msg: Result<()> = match self {
            TypeError::FunctionNotFound(var) => {
                let (file_name, text) = var
                    .get_file_name_and_text()
                    .unwrap_or_else(|| ("std.ty".to_string(), String::new()));
                SingleBuilder::new(file_name, text)
                    .pos((var.get_help_data().get_offset(), 0))
                    .text(format!(
                        "Function {}<{}> not defined in this scope.",
                        var.get_name(),
                        var.get_type().pretty()
                    ))
                    .pos_text("Not defined in this scope")
                    .build()
            }
            TypeError::AliasNotFound(typ) => {
                let (file_name, text) = typ
                    .get_file_name_and_text()
                    .unwrap_or_else(|| ("".to_string(), "".to_string()));
                SingleBuilder::new(file_name, text)
                    .pos((typ.get_help_data().get_offset(), 0))
                    .text(format!("Alias {} not defined in this scope.", typ.pretty()))
                    .pos_text("Not defined in this scope")
                    .build()
            }
            TypeError::AliasNotImported(name, module, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "'{}' is defined in module '{}' but not imported here",
                        name, module
                    ))
                    .pos_text(format!("'{}' is not in scope", name))
                    .help(format!(
                        "Add `use {}::{};` or `use {}::*;` to import it.",
                        module, name, module
                    ))
                    .build()
            }
            TypeError::VariableNotImported(name, module, is_public, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                let help = if is_public {
                    format!("Add `use {}::{};` or `use {}::*;` to import it.", module, name, module)
                } else {
                    format!(
                        "It's declared without `@pub`/`@export` in '{}', so mark it public first, \
                         then add `use {}::{};` to import it.",
                        module, module, name
                    )
                };
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "'{}' is defined in module '{}' but not imported here",
                        name, module
                    ))
                    .pos_text(format!("'{}' is not in scope", name))
                    .help(help)
                    .build()
            }
            TypeError::AliasMissingGenerics(name, generics, typ) => {
                let generics_str = generics.join(", ");
                let (file_name, text) = typ
                    .get_file_name_and_text()
                    .unwrap_or_else(|| ("std.ty".to_string(), "".to_string()));
                SingleBuilder::new(file_name, text)
                    .pos((typ.get_help_data().get_offset(), 0))
                    .text(format!("The alias {} isn't catching every generics.", name))
                    .pos_text(format!(
                        "You should write `type {}<{}> <- ...;` instead.",
                        name, generics_str
                    ))
                    .help("If you don't want them as a parameter, you can replace them with `Any`.")
                    .build()
            }
            TypeError::Let(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let ((file_name1, text1), pos1) = safe_file_pos(&help_data1, 0);
                let ((file_name2, text2), pos2) = safe_file_pos(&help_data2, 1);
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1(pos1)
                    .pos2(pos2)
                    .text(format!("type {} doesn't match type {}", t1.pretty(), t2.pretty()))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Received {}", t2.pretty()))
                    .build()
            }
            TypeError::Param(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let ((file_name1, text1), pos1) = safe_file_pos(&help_data1, 0);
                let ((file_name2, text2), pos2) = safe_file_pos(&help_data2, 1);
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1(pos1)
                    .pos2(pos2)
                    .text(format!("type {} doesn't match type {}", t1.pretty(), t2.pretty()))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Received {}", t2.pretty()))
                    .build()
            }
            TypeError::GenericPatternMatch(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let ((file_name1, text1), pos1) = safe_file_pos(&help_data1, 0);
                let ((file_name2, text2), pos2) = safe_file_pos(&help_data2, 1);
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1(pos1)
                    .pos2(pos2)
                    .text(format!("can't pattern match {{{} => {}}}", t1.pretty2(), t2.pretty2()))
                    .pos_text1(format!("{} should be a generic variable", t1.pretty2()))
                    .pos_text2(format!("Substitution type: {}", t2.pretty2()))
                    .build()
            }
            TypeError::UnmatchingReturnType(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let ((file_name1, text1), pos1) = safe_file_pos(&help_data1, 0);
                let ((file_name2, text2), pos2) = safe_file_pos(&help_data2, 1);
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1(pos1)
                    .pos2(pos2)
                    .text(format!(
                        "The output type of the function don't match it's type annotation\nExpected: {}\nFound: {}",
                        t1.pretty(),
                        t2.pretty()
                    ))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Received {}", t2.pretty()))
                    .build()
            }
            TypeError::UndefinedFunction(fun) => {
                let help_data = fun.get_help_data();
                let (file_name, text) = help_data
                    .get_file_data()
                    .unwrap_or_else(|| ("std.ty".to_string(), "".to_string()));
                let res = SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text("The function doesn't exist");

                res.build()
            }
            TypeError::UndefinedVariable(var) => {
                let help_data = var.get_help_data();
                let var2 = match Var::from_language(var) {
                    Some(v) => v,
                    None => return "Undefined variable".to_string(),
                };
                let (file_name, text) = help_data
                    .get_file_data()
                    .unwrap_or_else(|| ("std.ty".to_string(), "".to_string()));
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .pos_text(format!("Undefined variable '{}'", var2.get_name()))
                    .text(format!("Undefined variable '{}'", var2.get_name()))
                    .help(format!(
                        "- Check the orthograph \n- if it's a function check if it's defined for the given type {}",
                        var2.get_type()
                    ))
                    .build()
            }
            TypeError::ImmutableVariable(var_assign, var) => {
                let var = var.clone().set_type(var.get_type().generalize());
                let help_data1 = var_assign.get_help_data();
                let help_data2 = var.get_help_data();
                let ((file_name1, text1), pos1) = safe_file_pos(&help_data1, 0);
                let ((file_name2, text2), pos2) = safe_file_pos(&help_data2, 1);
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1(pos1)
                    .pos2(pos2)
                    .text(format!("The variable {} is immutable", var))
                    .pos_text1(format!("Forbidden assignation to {}", var))
                    .pos_text2(format!("{} defined with 'let' (=immutable) here", var))
                    .help("Try to replace the 'let' keyword by the 'mut' keyword")
                    .build()
            }
            TypeError::PrivateVariable(var_used, var) => {
                let var = var.clone().set_type(var.get_type().generalize());
                let help_data1 = var_used.get_help_data();
                let help_data2 = var.get_help_data();
                let ((file_name1, text1), pos1) = safe_file_pos(&help_data1, 0);
                let ((file_name2, text2), pos2) = safe_file_pos(&help_data2, 1);
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1(pos1)
                    .pos2(pos2)
                    .text(format!("The variable {} is private", var))
                    .pos_text1(format!("Forbidden access to {} which is private", var))
                    .pos_text2(format!("{} defined as private (without 'pub') here", var))
                    .help("Try to add the 'pub' keyword befor the 'let' keyword")
                    .build()
            }
            TypeError::FieldNotFound((name, h), typ) => {
                let help_data1 = h;
                let help_data2 = typ.get_help_data();
                let ((file_name1, text1), pos1) = safe_file_pos(&help_data1, 0);
                let ((file_name2, text2), pos2) = safe_file_pos(&help_data2, 1);
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1(pos1)
                    .pos2(pos2)
                    .text(format!("{} doesn't have a field '{}'", typ.pretty(), name))
                    .pos_text1(format!("Field '{}' doesn't exist", name))
                    .pos_text2(format!("Type {} defined here", typ.pretty()))
                    .build()
            }
            TypeError::WrongExpression(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                let offset = help_data.get_offset().min(text.len());
                let line = (text[..offset].lines().count() + 1) as u32;
                let element = text[offset..].lines().next().unwrap_or("").trim().to_string();
                SingleBuilder::new(file_name.clone(), text)
                    .pos((offset, 0))
                    .text(format!(
                        "Unknown element `{}` in `{}` at `{}`",
                        element, file_name, line
                    ))
                    .build()
            }
            TypeError::WrongIndexing(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let ((file_name1, text1), pos1) = safe_file_pos(&help_data1, 0);
                let ((file_name2, text2), pos2) = safe_file_pos(&help_data2, 1);
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1(pos1)
                    .pos2(pos2)
                    .text("Wrong indexing".to_string())
                    .pos_text1(format!("{} Can't be indexed", t1.pretty2()))
                    .pos_text2(format!("Has a bigger dimension {}", t2.pretty2()))
                    .build()
            }
            TypeError::InterfaceReturnOnly(typ) => {
                let help_data = typ.get_help_data();
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!("Interface '{}' appears only in return position.", typ.pretty()))
                    .pos_text("Interface type used as existential return")
                    .help(
                        "An interface in return position (without a matching parameter) \
                         acts as an existential type, which is not supported. \
                         Use an opaque type to hide the implementation: `opaque MyType <- ...`.",
                    )
                    .build()
            }
            TypeError::UnknownTypeConstructor(name, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "'{}' is used as a record constructor but is not declared",
                        name
                    ))
                    .pos_text("Undeclared type constructor")
                    .help(format!("Declare it first: `typeconstructor {}[N] record;`", name))
                    .build()
            }
            TypeError::DuplicateField(name, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!("Field '{}' is provided more than once", name))
                    .pos_text("Duplicate field")
                    .build()
            }
            TypeError::MissingField(name, typ, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Field '{}' of type {} is missing in this constructor call",
                        name,
                        typ.pretty()
                    ))
                    .pos_text(format!("Missing field '{}'", name))
                    .help("Provide it explicitly, or spread it from another value of the same type with `..source`.")
                    .build()
            }
            TypeError::SpreadTypeMismatch(expected, found, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Spread source has type {} but {} was expected",
                        found.pretty(),
                        expected.pretty()
                    ))
                    .pos_text("Spread source type mismatch")
                    .help("The `..source` value must have exactly the same alias as the constructed type.")
                    .build()
            }
            TypeError::InvalidTypeOperatorDomain(message, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(message.clone())
                    .pos_text("Invalid operand for this type-level operator")
                    .build()
            }
            TypeError::EmbedNonRecord(name, typ, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Cannot embed non-record type '{}' in field '{}'",
                        typ.pretty(),
                        name
                    ))
                    .pos_text("Embedded type must have kind Record")
                    .help("Only record-kinded types (`list { ... }` aliases) can be embedded with `field: @Type`.")
                    .build()
            }
            TypeError::EmbedCollision(name, fields, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Function '{}' is provided by multiple embeddings: {}",
                        name,
                        fields.join(", ")
                    ))
                    .pos_text("Embedding collision")
                    .help("Rename or remove one of the conflicting embedded fields.")
                    .build()
            }
            TypeError::EmbedConflict(name, type_name, field, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Function '{}' already defined in '{}' through embedded field '{}'",
                        name, type_name, field
                    ))
                    .pos_text("Conflicts with an inherited embedded function")
                    .help("No implicit shadowing is allowed between an explicit definition and an embedded function.")
                    .build()
            }
            TypeError::KindMismatch(expected, actual, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "This generic is used as kind `{}` here, but as kind `{}` elsewhere in the same signature",
                        actual, expected
                    ))
                    .pos_text(format!("Expected kind `{}`", expected))
                    .help("A generic name must denote the same kind everywhere in a single function signature.")
                    .build()
            }
            TypeError::SelfOutsideContext(help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text("Self is not defined in this context".to_string())
                    .pos_text("Self has no anchoring parameter")
                    .help("Self:{ ... } must appear inside a function body and include a `...base` spread (e.g. `Self:{ x = 1, ...a }`).")
                    .build()
            }
            TypeError::NonTrailingDefaultParam(name, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Parameter '{}' has no default value, but appears after a parameter that does",
                        name
                    ))
                    .pos_text("Missing default value")
                    .help("Once a parameter has a default value, every parameter after it must also have one.")
                    .build()
            }
            TypeError::PrivateImport(member, module, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!("'{}' is private in module '{}'", member, module))
                    .pos_text(format!("'{}' is not public", member))
                    .help(format!(
                        "Add `@pub` or `@export` before the declaration of '{}' in '{}'",
                        member, module
                    ))
                    .build()
            }
            TypeError::CircularModuleDependency { module_name, help_data } => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Circular module dependency: '{}' is currently being type-checked",
                        module_name
                    ))
                    .pos_text("Cyclic module import")
                    .help(
                        "Module A cannot import from module B while B's body is already being \
                         type-checked (directly or transitively). Break the cycle by moving the \
                         shared definitions into a third module.",
                    )
                    .build()
            }
            TypeError::InterfaceNotSatisfied(concrete, interface, missing, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Type {} does not implement interface {}",
                        concrete.pretty(),
                        interface.pretty()
                    ))
                    .pos_text(format!(
                        "missing function{}: {}",
                        if missing.len() > 1 { "s" } else { "" },
                        missing.join(", ")
                    ))
                    .help(format!(
                        "Add {} to {} (or to a value passed where this interface is required).",
                        missing
                            .iter()
                            .map(|m| format!("`{}`", m))
                            .collect::<Vec<_>>()
                            .join(", "),
                        concrete.pretty()
                    ))
                    .build()
            }
            TypeError::IncompatibleInterfaceMethod(name, expected, found, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!("Function {} has incompatible signature", name))
                    .pos_text(format!(
                        "expected: {}\nfound:    {}",
                        expected.pretty(),
                        found.pretty()
                    ))
                    .help("Parameters must be contravariant and the return type covariant with the interface's declared signature.")
                    .build()
            }
            TypeError::NonExhaustiveMatch(missing, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text("This match doesn't cover every variant".to_string())
                    .pos_text(format!(
                        "Missing variant{}: {}",
                        if missing.len() > 1 { "s" } else { "" },
                        missing.join(", ")
                    ))
                    .help("Add an arm for each missing variant, or a catch-all `_ => ...` branch.")
                    .build()
            }
            TypeError::PatternTypeMismatch(pattern, typ, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "Pattern {} can never match value of type {}",
                        pattern,
                        typ.pretty()
                    ))
                    .pos_text("This pattern's shape is incompatible with the scrutinee's type")
                    .build()
            }
            TypeError::UnsupportedPattern(pattern, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!("Unsupported match pattern: {}", pattern))
                    .pos_text("This pattern shape isn't supported")
                    .help("Bind a plain variable name here instead; nested destructuring inside this position isn't implemented yet.")
                    .build()
            }
            TypeError::AliasArityMismatch(name, expected, found, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "{} expects {} type argument{}, found {}",
                        name,
                        expected,
                        if expected > 1 { "s" } else { "" },
                        found
                    ))
                    .pos_text("Wrong number of type arguments here")
                    .help(format!(
                        "Provide exactly {} type argument{} to `{}`.",
                        expected,
                        if expected > 1 { "s" } else { "" },
                        name
                    ))
                    .build()
            }
            TypeError::LoopControlOutsideLoop(keyword, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!("'{}' used outside of a loop", keyword))
                    .pos_text(format!("'{}' has no enclosing loop", keyword))
                    .help("break/next are only valid inside the body of a loop/while/for.")
                    .build()
            }
            TypeError::DataFrameColumnNotVector(name, typ, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "DataFrame column '{}' must be a vector, found {}",
                        name,
                        typ.pretty()
                    ))
                    .pos_text("This column isn't vector-shaped")
                    .help("Every DataFrame column must be a vector (e.g. `[1, 2, 3]`).")
                    .build()
            }
            TypeError::DataFrameColumnLengthMismatch(name1, len1, name2, len2, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "DataFrame columns have mismatched lengths: '{}' has {}, '{}' has {}",
                        name1, len1, name2, len2
                    ))
                    .pos_text("Column lengths must all match")
                    .help("Every column in a DataFrame literal must have the same length.")
                    .build()
            }
            TypeError::TagFieldConstructorNotSupported(variant_name, union_name, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!(
                        "'{}' is a tag of union '{}'; it can't be constructed with `:{{ field = val }}` syntax",
                        variant_name, union_name
                    ))
                    .pos_text("Tag constructors take a single positional payload, not named fields")
                    .help(format!(
                        "Use `.{}(value)` instead (e.g. `.{}(:{{ ... }})` if the payload is itself a record).",
                        variant_name, variant_name
                    ))
                    .build()
            }
            TypeError::UnknownUnionVariant(variant_name, union_name, help_data) => {
                let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .text(format!("'{}' is not a variant of union '{}'", variant_name, union_name))
                    .pos_text("Unknown variant")
                    .help(format!("Check the variants declared by `type {} <- ...;`.", union_name))
                    .build()
            }
            TypeError::NoMatchingSignature(name, arg_types, signatures, help_data) => {
                let (file_data, pos) = safe_file_pos(&help_data, name.len());
                let args_pretty = arg_types.iter().map(|t| t.pretty()).collect::<Vec<_>>().join(", ");
                SingleBuilder::new(file_data.0, file_data.1)
                    .pos(pos)
                    .text(format!("No signature of function '{}' matches this call.", name))
                    .pos_text(format!(
                        "Called with {} argument(s): ({})",
                        arg_types.len(),
                        args_pretty
                    ))
                    .help(format!(
                        "'{}' exists but none of its signature(s) accepts these arguments:\n    {}",
                        name,
                        signatures.join("\n    ")
                    ))
                    .build()
            }
        };
        msg.map_or_else(|e| format!("{:?}", e), |_| String::new())
    }
}
