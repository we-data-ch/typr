#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::operation_priority::PriorityTokens;
use crate::operation_priority::PriorityToken;
use crate::type_token::TypeToken;
use crate::lang_token::LangToken;
use crate::help_data::HelpData;

pub struct VectorPriority<T: PriorityToken> {
    body: Vec<T>,
    initial_expression: String,
    help_data: HelpData
}

impl<T: PriorityToken, E: From<T> + Default> PriorityTokens<T, E> for VectorPriority<T> {
    fn get_first(&mut self) -> Option<T> {
        if self.body.len() > 0 {
            Some(self.body.remove(0))
        } else { None }
    }

    fn peak_first(&self) -> Option<T> {
        self.body.iter().next().cloned()
    }

    fn len(&self) -> usize {
        self.body.len()
    }

    fn display_state(&self) -> String {
        format!("self: {}", self)
    }

    fn get_initial_expression(&self) -> HelpData {
            self.help_data.clone()
    }

}

use std::fmt;
impl<T: PriorityToken> fmt::Display for VectorPriority<T> {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let txt = self.body.iter()
            .fold("".to_string(), |x, y| format!("{} {}", x, y));
        write!(f, "{}", txt)       
    }
}

impl From<Vec<TypeToken>> for VectorPriority<TypeToken> {
   fn from(val: Vec<TypeToken>) -> Self {
       let res = val.iter().next().unwrap().clone();
       let expression = val.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" ");
       VectorPriority {
        body: val,
        initial_expression: expression,
        help_data: res.get_help_data()
       }
   } 
}

impl From<Vec<LangToken>> for VectorPriority<LangToken> {
   fn from(val: Vec<LangToken>) -> Self {
       let res = val.iter().next().unwrap().clone();
       let expression = val.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" ");
       VectorPriority {
           body: val,
           initial_expression: expression,
           help_data: res.get_help_data()
       }
   } 
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::type_operator::TypeOperator;
    use crate::builder;
    use crate::Type;
    use crate::type_category::TypeCategory;

    #[test]
    fn test_type_tokens1(){
        let exp: Vec<TypeToken> = vec![
            builder::boolean_type().into(),
            TypeOperator::Union.into(),
            builder::number_type().into()
        ]; 
        let res: Type = VectorPriority::from(exp).run();
        assert_eq!(res.to_category(), TypeCategory::Operator)
    }

    #[test]
    fn test_type_tokens2(){
        let exp: Vec<TypeToken> = vec![
            builder::boolean_type().into(),
            TypeOperator::Union.into(),
            builder::integer_type(2).into(),
            TypeOperator::Addition.into(),
            builder::integer_type(5).into(),
        ]; 
        let res: Type = VectorPriority::from(exp).run();
        assert_eq!(res.to_category(), TypeCategory::Operator)
    }

    #[test]
    fn test_type_tokens3(){
        let exp: Vec<TypeToken> = vec![
            builder::integer_type(2).into(),
            TypeOperator::Addition.into(),
            builder::integer_type(5).into(),
            TypeOperator::Union.into(),
            builder::boolean_type().into(),
        ]; 
        let res: Type = VectorPriority::from(exp).run();
        assert_eq!(res.to_category(), TypeCategory::Operator)
    }

    #[test]
    fn test_type_tokens4(){
        let exp: Vec<TypeToken> = vec![
            builder::integer_type(2).into(),
            TypeOperator::Addition.into(),
            builder::integer_type(5).into(),
            TypeOperator::Union.into(),
            builder::boolean_type().into(),
        ]; 
        let res = VectorPriority::from(exp);
        println!("{}", PriorityTokens::<TypeToken, Type>::display_state(&res));
        assert!(true)
    }

    #[test]
    fn test_priority1() {
        let vec: Vec<TypeToken> = vec![];
        let res = VectorPriority::from(vec);
        assert_eq!(PriorityTokens::<TypeToken, Type>::len(&res), 0_usize);
    }

    #[test]
    fn test_priority2() {
        let vec: Vec<TypeToken> = vec![];
        let res = VectorPriority::from(vec);
        assert_eq!(PriorityTokens::<TypeToken, Type>::peak_first(&res),
                   None);
    }

}

