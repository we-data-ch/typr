use crate::unification_map::UnificationMap;
use crate::Type;
use crate::help_data::HelpData;
use crate::builder;
use crate::Lang;
use crate::Context;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType(pub Vec<Type>, pub Type, pub HelpData);

//main
impl FunctionType {
    pub fn infer_return_type2(self, values: &[Lang], context: &Context, name: &str) -> Option<Type> {
        let param_types = self.get_param_types();
        let unification_map = context
                .get_unification_map(values, &param_types, name)
                .unwrap_or(UnificationMap::new(vec![]));
        if name == "reduce" { dbg!(&values); }
        let (new_return_type, _new_context) = unification_map
                .apply_unification_type(context, &self.get_return_type());
        new_return_type.is_reduced().then(|| new_return_type)
    }

    pub fn infer_return_type(self, values: &[Lang], context: &Context) -> Option<Type> {
        let param_types = self.get_param_types();
        let unification_map = context
                .get_unification_map(values, &param_types, "ok")
                .unwrap_or(UnificationMap::new(vec![]));
        let (new_return_type, _new_context) = unification_map
                .apply_unification_type(context, &self.get_return_type());
        new_return_type.is_reduced().then(|| new_return_type)
    }

    pub fn get_param_types(&self) -> Vec<Type> {
        self.0.clone()
    }

    pub fn get_return_type(&self) -> Type {
        self.get_help_data();
        self.1.clone()
    }

    pub fn get_help_data(&self) -> HelpData {
        self.2.clone()
    }

    pub fn is_r_function(&self) -> bool {
        (self.0 == vec![]) &&
        (self.1 == builder::unknown_function())
    }

    pub fn get_first_param(&self) -> Option<Type> {
        let params = self.get_param_types();
        if params.len() > 0 {
            Some(params[0].clone())
        } else { None }
    }

    pub fn set_help_data(self, h: HelpData) -> Self {
        FunctionType(self.0, self.1, h)
    }

    pub fn set_params(self, params: Vec<Type>) -> Self {
        FunctionType(params, self.1, self.2)
    }

    pub fn set_return_type(self, ret_typ: Type) -> Self {
        FunctionType(self.0, ret_typ, self.2)
    }

}

impl TryFrom<Type> for FunctionType {
    type Error = String;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
        Type::Function(args, ret, h) => Ok(FunctionType(args, *ret, h)),
        Type::UnknownFunction(h)  => Ok(FunctionType::default().set_help_data(h.clone())),
        _ => Err(format!("{} is a type not convertible to FunctionType", value)) 
        }
    }
}

impl Default for FunctionType {
    fn default() -> FunctionType {
        FunctionType(vec![], builder::unknown_function(), HelpData::default())
    }
}

