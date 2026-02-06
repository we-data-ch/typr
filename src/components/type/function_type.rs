#![allow(
    dead_code,
    unused_variables,
    unused_imports,
    unreachable_code,
    unused_assignments
)]
use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::language::Lang;
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
    vectorized: bool,
}

fn lift(max_index: (VecType, i32), types: &[Type]) -> Vec<Type> {
    types
        .iter()
        .map(|typ| typ.clone().lift(max_index))
        .collect()
}

//main
impl FunctionType {
    pub fn new(arguments: Vec<Type>, return_type: Type, help_data: HelpData) -> Self {
        Self {
            return_type,
            help_data,
            arguments,
            vectorized: false,
            infered_return_type: builder::empty_type(),
        }
    }

    pub fn adjust_nb_parameters(self, nb: usize) -> Self {
        let arguments = if self.arguments.len() == nb {
            self.arguments
        } else {
            (0..nb).map(|_| builder::any_type()).collect::<Vec<Type>>()
        };

        Self {
            arguments,
            ..self
        }
    }

    pub fn set_vectorized(self) -> Self {
        Self {
            vectorized: true,
            ..self
        }
    }

    pub fn is_vectorized(&self) -> bool {
        self.vectorized
    }

    fn lift_and_unification(
        context: &Context,
        types: &[Type],
        param_types: &[Type],
    ) -> Option<UnificationMap> {
        let unique_types = types
            .iter()
            .map(|x| x.get_size_type())
            .collect::<HashSet<_>>();
        validate_vectorization(unique_types)
            .and_then(|hash| hash.iter().cloned().max_by_key(|x| x.0))
            .map(|(index, vectyp, _)| (vectyp, index))
            .and_then(|max_index| {
                context
                    .get_unification_map(&lift(max_index, types), &lift(max_index, param_types))
                    .map(|um| um.set_vectorized(max_index.0, max_index.1))
            })
    }

    pub fn infer_return_type(self, types: &[Type], context: &Context) -> Option<Self> {
        let param_types = self.get_param_types();
        context
            .get_unification_map(types, &param_types)
            .or(Self::lift_and_unification(context, types, &param_types))
            .map(|um| self.apply_unification_to_return_type(context, um))
    }

    fn lift(self, index: (VecType, i32)) -> Self {
        Self {
            arguments: self
                .arguments
                .iter()
                .map(|typ| typ.clone().lift(index))
                .collect(),
            return_type: self.return_type.lift(index),
            vectorized: true,
            ..self
        }
    }

    fn apply_unification_to_return_type(
        self,
        context: &Context,
        um: UnificationMap,
    ) -> FunctionType {
        let fun_typ = um
            .get_vectorization()
            .map_or(self.clone(), |(vec_type, index)| {
                self.lift((vec_type, index))
            });
        let new_return_type = um
            .apply_unification_type(context, &fun_typ.get_return_type())
            .0;
        fun_typ.set_infered_return_type(new_return_type)
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
        if params.len() > 0 {
            Some(params[0].clone())
        } else {
            None
        }
    }

    pub fn set_help_data(self, h: HelpData) -> Self {
        Self {
            help_data: h,
            ..self
        }
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
            Type::Function(args, ret, h) => Ok(FunctionType::new(args, *ret, h)),
            Type::UnknownFunction(h) => Ok(FunctionType::default().set_help_data(h.clone())),
            _ => Err(format!(
                "{} is a type not convertible to FunctionType",
                value
            )),
        }
    }
}

impl Default for FunctionType {
    fn default() -> FunctionType {
        FunctionType::new(
            vec![],
            builder::unknown_function_type(),
            HelpData::default(),
        )
    }
}
