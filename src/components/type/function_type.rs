#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::processes::type_checking::unification_map::UnificationMap;
use crate::components::error_message::help_data::HelpData;
use crate::components::context::Context;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use crate::utils::builder;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    arguments: Vec<Type>,
    return_type: Type,
    infered_return_type: Type,
    help_data: HelpData,
    vectorized: bool
}

//main
impl FunctionType {

    pub fn new(arguments: Vec<Type>, return_type: Type, help_data: HelpData) -> Self {
        Self {
            return_type,
            help_data,
            arguments,
            vectorized: false,
            infered_return_type: builder::empty_type()
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

    pub fn infer_return_type(self, types: Vec<Type>, context: &Context, name: &str) -> Self {
        let param_types = self.get_param_types();
        let unification_map = context
                .get_unification_map(types, &param_types, name)
                .unwrap_or(UnificationMap::new(vec![]));
        let (new_return_type, _new_context) = unification_map
                .apply_unification_type(context, &self.get_return_type());
        let final_return_type = new_return_type.is_reduced().then(|| new_return_type);
        self.set_infered_return_type(final_return_type.unwrap())
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
        (self.arguments == vec![]) &&
        (self.return_type == builder::unknown_function_type())
    }

    pub fn get_first_param(&self) -> Option<Type> {
        let params = self.get_param_types();
        if params.len() > 0 {
            Some(params[0].clone())
        } else { None }
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
        Type::UnknownFunction(h)  => Ok(FunctionType::default().set_help_data(h.clone())),
        _ => Err(format!("{} is a type not convertible to FunctionType", value)) 
        }
    }
}

impl Default for FunctionType {
    fn default() -> FunctionType {
        FunctionType::new(vec![], builder::unknown_function_type(), HelpData::default())
    }
}

