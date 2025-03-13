use crate::Type;
use crate::context::Context;
use crate::var::Var;
use crate::kind::Kind;
use crate::NominalContext;
use crate::nominals::Nominals;

fn get_right_elements(params: &[(String, Type)]) -> Vec<Type> {
    params.iter().map(|(_, ty)| ty.clone()).collect()
}

fn get_left_elements(params: &[(String, Type)]) -> Vec<String> {
    params.iter().map(|(name, _)| name.clone()).collect()
}

fn get_right_element<T: PartialEq + Clone, U: Clone>
(element: &T, list: &[(T, U)]) -> Option<U> {
    list.iter()
        .find(|&(left, _)| left == element)
        .cloned()
        .map(|(_, right)| right)
}

fn concat<T: Clone>(list1: Vec<T>, list2: Vec<T>) -> Vec<T> {
    [list1, list2].concat()
}

fn concat_context(context1: &Context, context2: &Context) -> Context {
    let typ_kind = concat(context1.get_kind_map().clone(), context2.get_kind_map().clone());
    let var_typ = concat(context1.get_type_map().clone(), context2.get_type_map().clone());
    Context::new(var_typ, typ_kind, Nominals::new())
}

fn add_type(context: &mut Context, var: &Var, ty: Type) {
        context.get_type_map().push((var.clone(), ty));
}

fn add_kind(context: &mut Context, typ: &Type, kind: Kind) {
        context.get_kind_map().push((typ.clone(), kind));
}

fn intersection<T: Eq + std::hash::Hash + Clone>
(list1: &[T], list2: &[T]) -> Vec<T> {
    let set: std::collections::HashSet<_> = list2.iter().collect();
    list1.iter().filter(|item| set.contains(item)).cloned().collect()
}


fn get_from_context(context: &Context, var: &Var, _context_list: &Context) -> Option<Type> {
    context.iter()
        .find(|(var2, _)| var == var2)
        .map(|(_, typ)| typ.clone())
}

fn get_type(var: &Var, context: &Context) -> Option<Type> {
    get_right_element(var, &context.get_type_map())
}

fn get_kind(var: &Type, context: &Context) -> Option<Kind> {
    get_right_element(var, &context.get_kind_map())
}
