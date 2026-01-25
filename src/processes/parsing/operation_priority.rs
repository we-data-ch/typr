#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::type_error::TypeError;
use crate::components::error_message::help_data::HelpData;
use std::fmt::Display;
use std::fmt::Debug;

#[derive(Debug, Default)]
pub enum TokenKind {
    Operator,
    Expression,
    #[default]
    Empty
}

pub trait PriorityToken: ToString + Default + Clone + From<TokenKind> + Debug + Display {
    fn get_token_type(&self) -> TokenKind;
    fn get_binding_power(&self) -> i32;
    fn combine(self, left: Self, right: Self) -> Self;

    fn is_operator(&self) -> bool {
        match self.get_token_type() {
            TokenKind::Operator => true,
            _ => false
        }
    }

    fn is_expression(&self) -> bool {
        !self.is_operator()
    }
}

pub trait PriorityTokens<T: PriorityToken, E: From<T> + Default>: Sized {

    fn get_first(&mut self) -> Option<T>;
    fn peak_first(&self) -> Option<T>;
    fn len(&self) -> usize;
    fn display_state(&self) -> String;
    fn get_initial_expression(&self) -> HelpData;

    fn get_operator(&mut self) -> Result<T, String> {
        match self.get_first() {
            Some(n) if n.is_operator() => Ok(n),
            Some(m) if m.is_expression() 
                => Err(format!("{} is not an operator", m.to_string())),
            _ => Err("The collection is empty".to_string())
        }
    }

    fn peak_operator(&self) -> Result<T, String> {
        match self.peak_first() {
            Some(n) if n.is_operator() => Ok(n),
            Some(m) if m.is_expression() 
                => Err(format!("{} is not an operator", m.to_string())),
            _ => Err("The collection is empty".to_string())
        }
    }

    fn get_expression(&mut self) -> Result<T, String> {
        match self.get_first() {
            Some(n) if n.is_expression() => Ok(n),
            Some(m) if m.is_operator() 
                => Err(format!("{} is not an expression", m.to_string())),
            _ => Err("The collection is empty".to_string())
        }
    }

    fn run(&mut self) -> E {
        if self.len() == 0 {
            E::default()
        } else {
            //println!("START SESSION");
            let res = E::from(self.run_helper(0));
            //println!("END SESSION");
            res
        }
    }

    fn run_helper(&mut self, binding_power: i32) -> T {
        //println!("--------------------");
        //println!("binding_power: {}", binding_power);
        //println!("{}", self.display_state());
        let mut left = self.get_expression()
            .expect(&TypeError::WrongExpression(self.get_initial_expression()).display());
        //println!("left: {}", left);
        loop {
            let op = match self.peak_operator() {
                Ok(op) => {
                    //println!("Peaked operator: {}", op);
                    op
                },
                Err(_) => {
                    //println!("Peaked operator: None");
                    //println!("No more symbol. Going back.");
                    //println!("--------------------");
                    break
                },
            };
            if op.get_binding_power() <= binding_power {
                //println!("{} is less important. return {}", op, left);
                //println!("--------------------");
                break;
            } 
            //else { println!("{} is more important. Go with next", op); }
            let op = self.get_operator().unwrap();
            let right = self.run_helper(op.get_binding_power());
            left = op.combine(left, right);
            //println!("Combined {}", left.to_string());
            //println!("--------------------");
        }
        left
    }

}

