use crate::unification::type_substitution;
use crate::argument_type::ArgumentType;
use crate::unification::unify;
use crate::r#type::Type;
use crate::var::Var;
use crate::types::string_to_type;
use crate::context::Context;
use crate::tag::Tag;

// Implementation of the Prolog rules as Rust functions
pub fn all_in(subset: &[ArgumentType], superset: &[ArgumentType]) -> bool {
    subset.iter().all(|item| superset.contains(item))
}

pub fn check_interface_function(
    var: &Var,
    fn_type1: &Type,
    self_type: &Type,
    context: &Context
) -> bool {
    // Equivalent to the Prolog unification:type_substitution
    let fn_type2 = type_substitution(fn_type1, &vec![(Type::Generic("self".to_string()), self_type.clone())]);
    // Equivalent to type_context:get_from_context
    match context.get(&var) {
        Some(context_type) => is_matching(context, &fn_type2, &context_type),
        None => false
    }
}

pub fn check_interface_functions(
    functions: &[(Var, Type)],
    self_type: &Type,
    context: &Context
) -> bool {
    functions.iter().all(|(var, fn_type)| {
        check_interface_function(var, fn_type, self_type, context)
    })
}

pub fn is_subtype(context: &Context, type1: &Type, type2: &Type) -> bool {
    match (type1, type2) {
        (typ1, typ2) if typ1 == typ2 => true,
        // Array subtyping
        (_, Type::Any) => true,
        (Type::Array(n1, t1), Type::Array(n2, t2)) => {
            is_subtype(context, n1, n2) && is_subtype(context, t1, t2)
        }

        // Interface subtyping
        (type1, Type::Interface(args)) => {
            check_interface_functions(
                &args.iter()
                    .map(|arg| (Var::default(), arg.1.clone()))
                    .collect::<Vec<_>>(),
                type1,
                context
            )
        }

        // Record subtyping
        (Type::Record(r1), Type::Record(r2)) => all_in(r2, r1),

        (Type::Union(types1), Type::Union(_types2)) => {
            types1.iter().all(|t1| is_subtype(context, &t1.to_type(), type2))
        },

        // Union subtyping
        (type1, Type::Union(types)) => {
            types.iter().any(|t| is_matching(context, type1, &t.to_type()))
        },

        // Generic subtyping
        (_, Type::Generic(_)) => true,
        (Type::Index(_), Type::IndexGen(_)) => true,

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
            
            is_same_type(context, &reduced1, &reduced2) ||
            is_subtype(context, &reduced1, &reduced2) ||
            is_subtype(context, &reduced2, &reduced1)
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

        Type::Alias(name, params, _base_type) => {
            if let Some(aliased_type) = context.get(&Var::from_name(name)) {
                let substituted = type_substitution(
                    &aliased_type,
                    &params.iter()
                        .enumerate()
                        .map(|(i, t)| (Type::Generic(i.to_string()), t.clone()))
                        .collect::<Vec<_>>()
                );
                reduce_type(context, &substituted)
            } else {
                type_.clone()
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

        _ => type_.clone()
    }
}

pub fn get_best_type(context: &Context, type1: &Type, type2: &Type) -> Type {
    match (type1, type2) {
        (Type::Any, type2) => type2.clone(), 
        (type1, type2) if type1 == type2 => type1.clone(),
        _ => {
            let reduced1 = reduce_type(context, type1);
            let reduced2 = reduce_type(context, type2);
            
            match unify(&reduced1, &reduced2) {
                Some(unification_result) 
                    => type_substitution(type1, &unification_result),
                _ => panic!("Unification failed !")
            }
        }
    }
}

fn is_same_type(context: &Context, type1: &Type, type2: &Type) -> bool {
    // Basic implementation - could be extended based on your needs
    type1 == type2 || match (type1, type2) {
        (Type::Alias(_, _, base1), type2) => is_same_type(context, &string_to_type(base1), type2),
        (type1, Type::Alias(_, _, base2)) => is_same_type(context, type1, &string_to_type(base2)),
        _ => false
    }
}

pub fn get_common_type_denominator(_context: &Context, type1: &Type, type2: &Type) -> Option<Type> {
    match (type1, type2) {
        // Record case
        (Type::Record(r1), Type::Record(r2)) => {
            // If r1 contains all fields from r2, return r1
            if all_in(r2, r1) {
                return Some(type1.clone());
            }
            // If r2 contains all fields from r1, return r2
            if all_in(r1, r2) {
                return Some(type2.clone());
            }
            None
        },

        // Direct equality case
        (t1, t2) if t1 == t2 => Some(t1.clone()),

        // No common denominator found
        _ => None
    }
}
