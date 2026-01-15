#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::components::language::Lang;
use crate::components::language::var::Var;
use std::iter::Sum;
use std::ops::Add;
use rpds::Vector;

#[derive(Debug, Clone)]
pub struct VarFunction(Vector<(Var, Lang)>);

impl VarFunction {
    pub fn get_bodies(&self, names: &[Var]) -> Vec<Lang> {
        todo!();
    }
}

impl TryFrom<Lang> for VarFunction {
    type Error = String;

    fn try_from(value: Lang) -> Result<Self, Self::Error> {
        match value {
            Lang::Let(var, _, body, _) if body.is_function() 
                => {
                    let var = Var::try_from(var).unwrap();
                    Ok(VarFunction(Vector::new().push_back((var, *body))))
                },
            _ => Err("It's not a function declaration".to_string())
        }
    }
}

impl Default for VarFunction {
    fn default() -> Self {
        VarFunction(Vector::new())
    }
}

impl Add for VarFunction {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        VarFunction(self.0.iter().chain(other.0.iter()).cloned().collect())
    }
}

impl Sum for VarFunction {
    fn sum<I: Iterator<Item = VarFunction>>(iter: I) ->  Self {
        iter.reduce(|x, y| x + y).unwrap_or_default()
    }
}
