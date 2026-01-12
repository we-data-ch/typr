use crate::parsing::operation_priority::PriorityToken;
use crate::parsing::operation_priority::TokenKind;
use crate::r#type::type_operator::TypeOperator;
use crate::help_data::HelpData;
use crate::Type;
use std::fmt;

#[derive(Debug, Default, Clone, PartialEq)]
pub enum TypeToken {
    Operator(TypeOperator),
    Expression(Type),
    #[default]
    EmptyOperator
}

impl From<Type> for TypeToken {
   fn from(val: Type) -> Self {
       TypeToken::Expression(val)
   } 
}

impl From<TypeOperator> for TypeToken {
   fn from(val: TypeOperator) -> Self {
       TypeToken::Operator(val)
   } 
}

impl From<TokenKind> for TypeToken {
   fn from(_: TokenKind) -> Self {
        TypeToken::EmptyOperator
   } 
}

impl fmt::Display for TypeToken {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            TypeToken::Expression(exp) => format!("{}", exp),
            TypeToken::Operator(exp) => format!("{}", exp),
            _ => "?".to_string()
        };
        write!(f, "{}", res)       
    }
}

impl TypeToken {
    pub fn get_help_data(&self) -> HelpData {
        match self {
            TypeToken::Expression(exp) => exp.get_help_data(),
            TypeToken::Operator(_) => HelpData::default(),
            TypeToken::EmptyOperator => HelpData::default()
        }
    }
}

impl PriorityToken for TypeToken {
    fn get_token_type(&self) -> TokenKind {
        match self {
            TypeToken::Operator(op) => op.get_token_type(),
            TypeToken::Expression(exp) => exp.get_token_type(),
            TypeToken::EmptyOperator => TokenKind::Operator
        }
    }

    fn get_binding_power(&self) -> i32 {
        match self {
            TypeToken::Operator(op) => op.get_binding_power(),
            TypeToken::Expression(exp) => exp.get_binding_power(),
            TypeToken::EmptyOperator => -1
        }
    }

    fn combine(self, left: TypeToken, right: TypeToken) -> Self {
        match (self.clone(), left.clone(), right.clone()) {
            (TypeToken::Operator(TypeOperator::Arrow),
            TypeToken::Expression(Type::Tuple(v, h)), 
            TypeToken::Expression(exp2))  
                => TypeToken::Expression(Type::Function(v, Box::new(exp2), h)),
            (TypeToken::Operator(op), TypeToken::Expression(exp1), TypeToken::Expression(exp2)) 
                => TypeToken::Expression(op.combine(exp1, exp2)),
            _ => panic!("Should be (op exp1 exp2) not ({} {} {})", self, left, right)
        }
    }


}
