use crate::Var;
use crate::Lang;

#[derive(Debug, Clone)]
pub struct VarFunction(Vec<(Var, Lang)>);

impl VarFunction {
    pub fn get_bodies(&self, names: &[Var]) -> Vec<Lang> {
        todo!();
    }
}
