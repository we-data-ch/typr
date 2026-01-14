use crate::parsing::operation_priority::PriorityToken;
use crate::parsing::operation_priority::TokenKind;
use crate::help_data::HelpData;
use crate::lang::operators::Op;
use std::fmt;
use crate::components::lang::language::Lang;

#[derive(Debug, Default, Clone, PartialEq)]
pub enum LangToken {
    Operator(Op),
    Expression(Lang),
    #[default]
    EmptyOperator
}

impl LangToken {
    pub fn get_help_data(&self) -> HelpData {
        match self {
            LangToken::Operator(op) => op.get_help_data(),
            LangToken::Expression(exp) => exp.get_help_data(),
            LangToken::EmptyOperator => HelpData::default()
        }
    }
}

impl From<Lang> for LangToken {
   fn from(val: Lang) -> Self {
       LangToken::Expression(val)
   } 
}

impl From<Op> for LangToken {
   fn from(val: Op) -> Self {
       LangToken::Operator(val)
   } 
}

impl From<TokenKind> for LangToken {
   fn from(_: TokenKind) -> Self {
        LangToken::EmptyOperator
   } 
}

impl fmt::Display for LangToken {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match self {
            LangToken::Expression(exp) => format!("{}", exp),
            LangToken::Operator(exp) => format!("{}", exp),
            _ => "?".to_string()
        };
        write!(f, "{}", res)       
    }
}

impl PriorityToken for LangToken {
    fn get_token_type(&self) -> TokenKind {
        match self {
            LangToken::Operator(op) => op.get_token_type(),
            LangToken::Expression(exp) => exp.get_token_type(),
            LangToken::EmptyOperator => TokenKind::Operator
        }
    }

    fn get_binding_power(&self) -> i32 {
        match self {
            LangToken::Operator(op) => op.get_binding_power(),
            LangToken::Expression(exp) => exp.get_binding_power(),
            LangToken::EmptyOperator => -1
        }
    }

    fn combine(self, left: LangToken, right: LangToken) -> Self {
        match (self.clone(), left.clone(), right.clone()) {
            (LangToken::Operator(op), LangToken::Expression(exp1), LangToken::Expression(exp2)) 
                => LangToken::Expression(op.combine(exp1, exp2)),
            _ => panic!("Should be (op exp1 exp2) not ({} {} {})", self, left, right)
        }
    }
}
