#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::language::Lang;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::Type;
use crate::components::r#type::VecType;
use crate::processes::type_checking::unification_map::UnificationMap;
use crate::utils::builder;
use crate::utils::standard_library::validate_vectorization;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    arguments: Vec<Type>,
    return_type: Type,
    infered_return_type: Type,
    help_data: HelpData,
    vec_type: VecType,
    is_variadic: bool,
    // Number of leading parameters with no default value. Parameters from
    // this index onward (up to `arguments.len()`) all have a default and
    // may be omitted by the caller.
    min_arity: usize,
}

fn lift(max_index: &(VecType, i32), types: &[Type]) -> Vec<Type> {
    types.iter().map(|typ| typ.clone().lift(max_index)).collect()
}

//main
impl FunctionType {
    pub fn new(vec_type: VecType, arguments: Vec<ArgumentType>, return_type: Type, help_data: HelpData) -> Self {
        let is_variadic = arguments.last().map(|a| a.is_variadic()).unwrap_or(false);
        let min_arity = arguments.iter().take_while(|a| !a.has_default()).count();
        let arguments: Vec<Type> = arguments.iter().map(|arg| arg.get_type()).collect();
        Self {
            return_type,
            help_data,
            arguments,
            vec_type,
            infered_return_type: builder::empty_type(),
            is_variadic,
            min_arity,
        }
    }

    pub fn adjust_nb_parameters(self, nb: usize) -> Self {
        let arguments = if self.is_variadic {
            let fixed_count = self.arguments.len().saturating_sub(1);
            if nb >= fixed_count {
                let variadic_type = self.arguments.last().cloned().unwrap_or(builder::any_type());
                let mut expanded = self.arguments[..fixed_count].to_vec();
                for _ in fixed_count..nb {
                    expanded.push(variadic_type.clone());
                }
                expanded
            } else {
                (0..nb).map(|_| builder::any_type()).collect()
            }
        } else if self.arguments.len() == nb {
            self.arguments
        } else {
            (0..nb).map(|_| builder::any_type()).collect::<Vec<Type>>()
        };

        Self { arguments, ..self }
    }

    pub fn is_variadic(&self) -> bool {
        self.is_variadic
    }

    pub fn min_arity(&self) -> usize {
        self.min_arity
    }

    pub fn has_defaults(&self) -> bool {
        self.min_arity < self.arguments.len()
    }

    pub fn set_vectorized(self, vec_type: VecType) -> Self {
        Self { vec_type, ..self }
    }

    pub fn is_vectorized(&self) -> bool {
        !self.vec_type.is_empty()
    }

    pub fn get_vec_type(&self) -> VecType {
        self.vec_type.clone()
    }

    fn lift_and_unification(context: &Context, types: &[Type], param_types: &[Type]) -> Option<UnificationMap> {
        let unique_types = types.iter().map(|x| x.get_size_type()).collect::<HashSet<_>>();
        validate_vectorization(unique_types)
            .and_then(|hash: HashSet<(i32, VecType, Type)>| hash.iter().cloned().max_by_key(|x| x.0))
            .map(|(index, vectyp, _)| (vectyp, index))
            .and_then(|max_index| {
                context
                    .get_unification_map(&lift(&max_index, types), &lift(&max_index, param_types))
                    .map(|um| um.set_vectorized(max_index.0.clone(), max_index.1))
            })
    }

    pub fn infer_return_type(self, types: &[Type], context: &Context) -> Option<Self> {
        if self.is_variadic {
            return self.infer_return_type_variadic_fn(types, context);
        }
        let param_types = self.get_param_types();
        context
            .get_unification_map(types, &param_types)
            .or(Self::lift_and_unification(context, types, &param_types))
            .map(|um| self.apply_unification_to_return_type(context, um))
    }

    fn infer_return_type_variadic_fn(self, types: &[Type], context: &Context) -> Option<Self> {
        let fixed_count = self.arguments.len().saturating_sub(1);
        if types.len() < fixed_count {
            return None;
        }
        let variadic_type = self.arguments.last()?.clone();
        // Expand params to match number of args passed
        let mut expanded_params = self.arguments[..fixed_count].to_vec();
        for _ in fixed_count..types.len() {
            expanded_params.push(variadic_type.clone());
        }
        let expanded_self = self.clone().set_params(expanded_params.clone());
        context
            .get_unification_map(types, &expanded_params)
            .or(Self::lift_and_unification(context, types, &expanded_params))
            .map(|um| expanded_self.apply_unification_to_return_type(context, um))
    }

    pub fn infer_return_type_direct(self, types: &[Type], context: &Context) -> Option<Self> {
        let param_types = self.get_param_types();
        // Exact-arity match only: arity-flexible calls go through the
        // variadic path or `infer_return_type_partial` instead. Without this
        // guard, `Context::get_unification_map`'s `zip` silently ignores a
        // shorter `types` against the trailing, unmatched params.
        if types.len() != param_types.len() {
            return None;
        }
        context
            .get_unification_map(types, &param_types)
            .map(|um| self.apply_unification_to_return_type(context, um))
    }

    pub fn infer_return_type_vectorized(self, types: &[Type], context: &Context) -> Option<Self> {
        let param_types = self.get_param_types();
        if types.len() != param_types.len() {
            return None;
        }
        Self::lift_and_unification(context, types, &param_types)
            .map(|um| self.apply_unification_to_return_type(context, um))
    }

    /// Matches a call with fewer arguments than declared, when the missing
    /// trailing parameters all have a default value (`has_defaults()`).
    /// Unifies only against the supplied `types`, truncating the signature
    /// to that length — the omitted defaulted params never participate in
    /// unification. The transpiled R function carries its own native
    /// default syntax, so the caller-supplied argument count is all that
    /// needs to round-trip through here.
    pub fn infer_return_type_partial(self, types: &[Type], context: &Context) -> Option<Self> {
        if !self.has_defaults() || self.is_variadic {
            return None;
        }
        if types.len() < self.min_arity || types.len() >= self.arguments.len() {
            return None;
        }
        let truncated_params = self.arguments[..types.len()].to_vec();
        let truncated_self = self.clone().set_params(truncated_params.clone());
        context
            .get_unification_map(types, &truncated_params)
            .or(Self::lift_and_unification(context, types, &truncated_params))
            .map(|um| truncated_self.apply_unification_to_return_type(context, um))
    }

    fn lift(self, index: (VecType, i32)) -> Self {
        Self {
            arguments: self.arguments.iter().map(|typ| typ.clone().lift(&index)).collect(),
            return_type: self.return_type.lift(&index),
            vec_type: index.0,
            ..self
        }
    }

    fn apply_unification_to_return_type(self, context: &Context, um: UnificationMap) -> FunctionType {
        let fun_typ = um
            .get_vectorization()
            .map_or(self.clone(), |(vec_type, index)| self.lift((vec_type, index)));
        let new_return_type = um.apply_unification_type(context, &fun_typ.get_return_type()).0;
        let new_args: Vec<Type> = fun_typ
            .get_param_types()
            .iter()
            .map(|arg| um.apply_unification_type(context, arg).0)
            .collect();
        fun_typ
            .set_infered_return_type(new_return_type.clone())
            .set_params(new_args)
            .set_return_type(new_return_type)
    }

    pub fn set_infered_return_type(self, ret_typ: Type) -> Self {
        Self {
            infered_return_type: ret_typ,
            ..self
        }
    }

    pub fn get_param_types(&self) -> Vec<Type> {
        self.arguments.clone()
    }

    pub fn get_return_type(&self) -> Type {
        self.return_type.clone()
    }

    pub fn get_infered_return_type(&self) -> Type {
        self.infered_return_type.clone()
    }

    pub fn get_help_data(&self) -> HelpData {
        self.help_data.clone()
    }

    pub fn is_r_function(&self) -> bool {
        (self.arguments == vec![]) && (self.return_type == builder::unknown_function_type())
    }

    pub fn get_first_param(&self) -> Option<Type> {
        let params = self.get_param_types();
        if !params.is_empty() {
            Some(params[0].clone())
        } else {
            None
        }
    }

    pub fn set_help_data(self, h: HelpData) -> Self {
        Self { help_data: h, ..self }
    }

    pub fn set_params(self, params: Vec<Type>) -> Self {
        Self {
            arguments: params,
            ..self
        }
    }

    pub fn set_return_type(self, ret_typ: Type) -> Self {
        Self {
            return_type: ret_typ,
            ..self
        }
    }
}

impl TryFrom<Type> for FunctionType {
    type Error = String;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::Function(args, ret, h) => Ok(FunctionType::new(VecType::Empty, args, *ret, h)),
            Type::UnknownFunction(h) => Ok(FunctionType::default().set_help_data(h.clone())),
            _ => Err(format!("{} is a type not convertible to FunctionType", value)),
        }
    }
}

impl Default for FunctionType {
    fn default() -> FunctionType {
        FunctionType {
            arguments: vec![],
            return_type: builder::unknown_function_type(),
            infered_return_type: builder::empty_type(),
            help_data: HelpData::default(),
            vec_type: VecType::Empty,
            is_variadic: false,
            min_arity: 0,
        }
    }
}
