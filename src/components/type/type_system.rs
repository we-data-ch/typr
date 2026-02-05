use crate::components::r#type::Context;
use std::fmt::Debug;
use std::hash::Hash;

pub trait TypeSystem: PartialOrd + Debug + Eq + Hash + Clone + Default {
    fn pretty(&self) -> String;
    fn simple_pretty(&self) -> String;
    fn verbose_pretty(&self) -> String;

    fn is_subtype(&self, other: &Self, context: &Context) -> bool;

    fn prettys(v: &[Self]) -> String {
        "[".to_string() + &v.iter().map(|x| x.pretty()).collect::<Vec<_>>().join(", ") + "]"
    }
}
