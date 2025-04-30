use crate::unification::type_substitution;
use crate::argument_type::ArgumentType;
use crate::r#type::Type;
use crate::var::Var;
use crate::context::Context;
use crate::tag::Tag;
use crate::is_subset;

// Implementation of the Prolog rules as Rust functions
pub fn all_subtype(cont: &Context, set1: &[ArgumentType], set2: &[ArgumentType]) -> bool {
    set1.iter().zip(set2.iter())
        .all(|(argt1, argt2)| {
           is_subtype(cont, &argt1.get_argument(), &argt2.get_argument())
           && is_subtype(cont, &argt1.get_type(), &argt2.get_type())
        })
}

fn contains_all(cont: &Context, vec1: &[ArgumentType], vec2: &[ArgumentType]) -> bool {
    vec1.iter()
        .any(|sub| {
            vec2.iter()
                .any(|sup| 
                     (sub.get_argument() == sup.get_argument())
                     && is_subtype(cont, &sub.get_type(), &sup.get_type()))
        })
}

fn to_self(t1: Type, t2: Type) -> Type {
    if  t1 == t2 {
        Type::Alias("Self".to_string(), vec![], "".to_string())
    } else {
       t1
    }
}

fn set_self(fs: &[(Var, Type)]) -> Vec<(Var, Type)> {
    fs.iter().map(|(var, typ)| {
        match typ {
            Type::Function(kinds, arg_types, ret_type) => {
                if arg_types.len() > 0 {
                    let first = arg_types.first().cloned().unwrap();
                    let args = arg_types.iter()
                        .map(|typ_ele| to_self(typ_ele.clone(), first.clone()))
                        .collect();
                    let ret2 = to_self((**ret_type).clone(), first);
                    (var.clone().set_type(Type::Empty), Type::Function(kinds.clone(), args, Box::new(ret2)))
                } else { (var.clone().set_type(Type::Empty), typ.clone()) }
            },
            _ => (var.clone(), typ.clone())
        }
    }).collect()
}

pub fn check_interface_functions(
    functions: &[(Var, Type)],
    self_type: &Type,
    context: &Context
) -> bool {
    let functions2 = set_self(&context.get_functions(self_type));
    is_subset(functions, &functions2)
}

fn has_generic_label(v: &[ArgumentType]) -> bool {
    v.iter().any(|arg| match arg.get_argument() {
        Type::LabelGen(_) => true,
        _ => false
    })
}

pub fn is_subtype(context: &Context, type1: &Type, type2: &Type) -> bool {
    match (type1, type2) {
        (typ1, typ2) if typ1 == typ2 => true,
        // Array subtyping
        (_, Type::Any) => true,
        (Type::Array(n1, t1), Type::Array(n2, t2)) => {
            is_subtype(context, n1, n2) && is_subtype(context, t1, t2)
        },
        (type1, Type::Alias(_, _, _)) => {
            let reduced = reduce_type(context, type2);
            is_subtype(context, type1, &reduced)
        },
        (Type::Function(_, args1, ret_typ1), Type::Function(_, args2, ret_typ2)) => {
            args1.iter().chain([&(**ret_typ1)])
                .zip(args2.iter().chain([&(**ret_typ2)]))
                .all(|(typ1, typ2)| is_subtype(context, typ1, typ2))
        }
        // Interface subtyping
        (type1, Type::Interface(args)) => {
            check_interface_functions(
                &args.iter()
                    .map(|arg| (Var::default().set_name(&arg.get_argument_str()), arg.1.clone()))
                    .collect::<Vec<_>>(),
                type1,
                context
            )
        }

        // Record subtyping
        (Type::Record(r1), Type::Record(r2)) => {
            if has_generic_label(r2) && (r1.len() == r2.len()) {
                all_subtype(context, r1, r2)
            } else if let Some(arg_typ) = type2.get_type_pattern() {
                true
            } else {
                contains_all(context, r1, r2)
            }
        },

        (Type::Union(types1), Type::Union(_types2)) => {
            types1.iter().all(|t1| is_subtype(context, &t1.to_type(), type2))
        },

        // Union subtyping
        (Type::Tag(name, body), Type::Union(types)) => {
            types.iter().any(|t| is_matching(context, type1, &t.to_type()))
        },
        (Type::Tag(name1, body1), Type::Tag(name2, body2)) => {
            (name1 == name2) && is_matching(context, body1, body2)
        },

        // Generic subtyping
        (_, Type::Generic(_)) => true,
        (Type::Index(_), Type::IndexGen(_)) => true,
        (Type::Integer, Type::IndexGen(_)) => true,
        (Type::Label(_), Type::LabelGen(_)) => true,
        (Type::Char, Type::LabelGen(_)) => true,
        (Type::IndexGen(_), Type::IndexGen(_)) => true,

        // Params subtyping
        (Type::Params(p1), Type::Params(p2)) => {
            p1.len() == p2.len() && 
            p1.iter().zip(p2.iter()).all(|(t1, t2)| is_subtype(context, t1, t2))
        }

        _ => false
    }
}

pub fn is_matching(context: &Context, type1: &Type, type2: &Type) -> bool {


    // Basic equality
    if type1 == type2 {
        return true;
    }

    // Handle special cases
    match (type1, type2) {
        (Type::Empty, _) | (_, Type::Empty) => true,
        (Type::Any, _) | (_, Type::Any) => true,
        
        // Reduce types and check again
        _ => {
            let reduced1 = reduce_type(context, type1);
            let reduced2 = reduce_type(context, type2);

            reduced1 == reduced2 ||
            is_subtype(context, &reduced1, &reduced2)
        }
    }
}

pub fn reduce_param(
    context: &Context,
    param: &ArgumentType  // List of pairs [X, Y1]
) -> ArgumentType {     // Returns list of pairs [X, Y2]
    
    let name = &param.0;
    let type_ = &param.1;
    let rest = &param.2;
    // Reduce the type part of each parameter
    let reduced_type = reduce_type(context, type_);
    ArgumentType(name.clone(), reduced_type, rest.clone())
}

pub fn reduce_type(context: &Context, type_: &Type) -> Type {
    match type_ {
        Type::Record(args) => {
            Type::Record(args.iter()
                .map(|arg| reduce_param(context, arg))
                .collect())
        }
        Type::Alias(name, concret_types, _base_type) => {
            let var = Var::from_name(name).set_type(Type::Params(concret_types.to_vec()));
            if let Some((aliased_type, generics)) = context.get_with_gen(&var) {
                let substituted = type_substitution(
                    &aliased_type,
                    &generics.iter()
                        .zip(concret_types.iter())
                        .map(|(gen, typ)| (gen.clone(), typ.clone()))
                        .collect::<Vec<_>>()
                );
                reduce_type(context, &substituted)
            } else {
                panic!("The alias {} wasn't founded", name)
            }
        }

        Type::Union(types) => {
            Type::Union(types.iter()
                .map(|t| reduce_type(context, &t.to_type()))
                .flat_map(Tag::from_type)
                .collect())
        }

        Type::Tag(name, inner) => {
            Type::Tag(name.clone(), Box::new(reduce_type(context, inner)))
        }
        Type::If(typ, _conditions) => *typ.clone(),
        _ => type_.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::subtypes::is_true_subtype;

    #[test]
    fn test_matching(){
        let typ1 = Type::Union(vec![
                               Tag("Some".to_string(), Type::Integer),
                               Tag("None".to_string(), Type::Empty)]);
        let typ2 = Type::Union(vec![
                               Tag("Some".to_string(), Type::Generic("T".to_string())),
                               Tag("None".to_string(), Type::Empty)]);
        let ctx = Context::default();
        assert_eq!(is_true_subtype(&ctx, &typ1, &typ2), true);
    }
}
