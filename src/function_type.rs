use crate::argument_kind::ArgumentKind;
use crate::Type;
use crate::help_data::HelpData;
use crate::builder;

#[derive(Debug)]
pub struct FunctionType(pub Vec<ArgumentKind>, pub Vec<Type>, pub Type, pub HelpData);

impl FunctionType {
    pub fn get_param_types(&self) -> Vec<Type> {
        self.1.clone()
    }

    pub fn get_ret_type(&self) -> Type {
        self.get_help_data();
        self.2.clone()
    }

    pub fn get_help_data(&self) -> HelpData {
        self.3.clone()
    }

    pub fn is_r_function(&self) -> bool {
        (self.0 == vec![]) &&
        (self.1 == vec![]) &&
        (self.2 == builder::empty_type())
    }

    pub fn get_first_param(&self) -> Option<Type> {
        let params = self.get_param_types();
        if params.len() > 0 {
            Some(params[0].clone())
        } else { None }
    }

    pub fn set_help_data(self, h: HelpData) -> Self {
        FunctionType(self.0, self.1, self.2, h)
    }
}

impl TryFrom<Type> for FunctionType {
    type Error = String;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
        Type::Function(kinds, args, ret, h) => Ok(FunctionType(kinds, args, *ret, h)),
        Type::RFunction(h)  => Ok(FunctionType::default().set_help_data(h.clone())),
        _ => Err(format!("{} is a type not convertible to FunctionType", value)) 
        }
    }
}

impl Default for FunctionType {
    fn default() -> FunctionType {
        FunctionType(vec![], vec![], builder::empty_type(), HelpData::default())
    }
}

