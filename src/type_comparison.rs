use crate::unification::type_substitution;
use crate::argument_type::ArgumentType;
use crate::r#type::Type;
use crate::var::Var;
use crate::context::Context;
use crate::tag::Tag;
use crate::help_data::HelpData;
use std::collections::HashSet;
use rpds::Vector;

pub fn is_subset(v1: &[(Var, Type)], v2: &[(Var, Type)], cont: &Context) -> bool {
    v1.iter().all(|(v1, t1)| {
        v2.iter()
            .any(|(v2, t2)| v1.match_with(v2, cont) && t1 == t2)
    })
}

fn to_self(t1: Type, t2: Type) -> Type {
    if  t1 == t2 {
        Type::Alias("Self".to_string(), vec![], "".into(), true, t1.into())
    } else {
       t1
    }
}

fn set_self(fs: &[(Var, Type)]) -> Vec<(Var, Type)> {
    fs.iter().map(|(var, typ)| {
        match typ {
            Type::Function(kinds, arg_types, ret_type, h) => {
                if arg_types.len() > 0 {
                    let first = arg_types.first().cloned().unwrap();
                    let args = arg_types.iter()
                        .map(|typ_ele| to_self(typ_ele.clone(), first.clone()))
                        .collect();
                    let ret2 = to_self((**ret_type).clone(), first);
                    (var.clone().set_type(Type::Empty(HelpData::default())), Type::Function(kinds.clone(), args, Box::new(ret2), h.clone()))
                } else { (var.clone().set_type(Type::Empty(HelpData::default())), typ.clone()) }
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
    is_subset(functions, &functions2, context)
}

pub fn has_generic_label(v: &HashSet<ArgumentType>) -> bool {
    v.iter().any(|arg| match arg.get_argument() {
        Type::LabelGen(_, _) => true,
        _ => false
    })
}

pub fn is_subtype(context: &Context, type1: &Type, type2: &Type) -> bool {
    match (type1, type2) {
        (typ1, typ2) if typ1 == typ2 => true,
        // Array subtyping
        (_, Type::Any(_)) => true,
        (Type::Any(_), _) => true,
        (Type::RFunction(_), _) => true, // TODO: should make RFunction subtype of functions maybe
        (Type::Array(n1, t1, _), Type::Array(n2, t2, _)) => {
            is_subtype(context, n1, n2) && is_subtype(context, t1, t2)
        },
        (Type::Vector(n1, t1, _), Type::Vector(n2, t2, _)) => {
            is_subtype(context, n1, n2) && is_subtype(context, t1, t2)
        },
        (Type::Sequence(n1, t1, _), Type::Sequence(n2, t2, _)) => {
            is_subtype(context, n1, n2) && is_subtype(context, t1, t2)
        },
        (type1, Type::Alias(_, _, _, _, _)) => {
            let reduced = reduce_type(context, type2);
            is_subtype(context, type1, &reduced)
        },
        (Type::Function(_, args1, ret_typ1, _), Type::Function(_, args2, ret_typ2, _)) => {
            args1.iter().chain([&(**ret_typ1)])
                .zip(args2.iter().chain([&(**ret_typ2)]))
                .all(|(typ1, typ2)| is_subtype(context, typ1, typ2))
        }
        // Interface subtyping
        (type1, Type::Interface(args, _)) => {
            let res = args.iter()
                .map(|arg| {
                    let var = Var::default()
                        .set_name(&arg.get_argument_str());
                        //.set_type(arg.get_type());
                    (var, arg.get_type().clone())
                })
                .collect::<Vec<_>>();
            check_interface_functions(
                &res,
                type1,
                context
            )
        }

        // Record subtyping
        (Type::Record(r1, _), Type::Record(r2, _)) => {
            if has_generic_label(r2) && (r1.len() == r2.len()) {
                //all_subtype(context, r1, r2)
                r1.is_superset(r2)
            } else if let Some(_arg_typ) = type2.get_type_pattern() {
                true
            } else {
                //contains_all(context, r1, r2)
                r1.is_superset(r2)
            }
        },

        (Type::StrictUnion(types1, _), Type::StrictUnion(_types2, _)) => {
            types1.iter().all(|t1| is_subtype(context, &t1.to_type(), type2))
        },

        // Union subtyping
        (Type::Tag(_name, _body, _h), Type::StrictUnion(types, _)) => {
            types.iter().any(|t| is_matching(context, type1, &t.to_type()))
        },
        (Type::Tag(name1, body1, _h1), Type::Tag(name2, body2, _h2)) => {
            (name1 == name2) && is_matching(context, body1, body2)
        },

        // Generic subtyping
        (_, Type::Generic(_, _)) => true,
        (Type::Integer(_, _), Type::IndexGen(_, _)) => true,
        (Type::Char(_, _), Type::LabelGen(_, _)) => true,
        (Type::IndexGen(_, _), Type::IndexGen(_, _)) => true,

        // Params subtyping
        (Type::Params(p1, _), Type::Params(p2, _)) => {
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
        (Type::Empty(_), _) | (_, Type::Empty(_)) => true,
        (Type::Any(_), _) | (_, Type::Any(_)) => true,
        
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
    param: &ArgumentType,
    memory: Vector<String>// List of pairs [X, Y1]
) -> ArgumentType {     // Returns list of pairs [X, Y2]
    
    // Reduce the type part of each parameter
    let reduced_type = reduce_type_helper(context, &param.get_type(), memory);
    ArgumentType(param.get_argument(), reduced_type, param.2.to_owned())
}

fn is_in_memory(name: &str, memory: &Vector<String>) -> bool {
    memory.iter().find(|&val| val == name).is_some()
}

pub fn reduce_type(context: &Context, type_: &Type) -> Type {
    reduce_type_helper(context, type_, Vector::new())
}

pub fn reduce_type_helper(context: &Context, type_: &Type, memory: Vector<String>) -> Type {
    match type_ {
        Type::Record(args, h) => {
            Type::Record(args.iter()
                .map(|arg| reduce_param(context, arg, memory.clone()))
                .collect(), h.clone())
        },
        Type::Alias(name, concret_types, path, opacity, _h) => {
            if *opacity == true || is_in_memory(name, &memory) {
                type_.clone()
            } else {
                let var = Var::from_type(type_.clone())
                    .expect(&format!("The alias {} is malformed", type_))
                    .set_path(path.clone());
                if let Some((aliased_type, generics)) = context.get_matching_alias_signature(&var) {
                    let substituted = type_substitution(
                        &aliased_type,
                        &generics.iter()
                            .zip(concret_types.iter())
                            .map(|(r#gen, typ)| (r#gen.clone(), typ.clone()))
                            .collect::<Vec<_>>()
                    );
                    reduce_type_helper(context, &substituted, memory.push_back(name.clone()))
                } else {
                    let mvar = Var::from_type(type_.clone()).unwrap();
                    if mvar.is_opaque() {
                        type_.clone() 
                    } else {
                        panic!("The alias {} wasn't found in the context\n{}", 
                           var, context.display_typing_context());
                    }
                }
            }
        }
        Type::StrictUnion(types, h) => {
            Type::StrictUnion(types.iter()
                .map(|t| reduce_type_helper(context, &t.to_type(), memory.clone()))
                .flat_map(Tag::from_type)
                .collect(), h.clone())
        }
        Type::Tag(name, inner, h) => {
            Type::Tag(name.clone(), Box::new(reduce_type_helper(context, inner, memory.clone())), h.clone())
        }
        Type::If(typ, _conditions, _) => *typ.clone(),
        Type::Function(kinds, typs, ret_typ, h) => {
            let typs2 = typs.iter()
                .map(|x| reduce_type_helper(context, x, memory.clone()))
                .collect::<Vec<_>>();
            let ret_typ2 = reduce_type_helper(context, ret_typ, memory.clone());
            Type::Function(kinds.to_owned(), typs2, Box::new(ret_typ2), h.to_owned())
        },
        Type::Array(ind, typ, h) => {
            Type::Array(ind.clone(),
                    Box::new(reduce_type_helper(context, typ, memory.clone())),
                    h.clone())
        },
        Type::Vector(ind, typ, h) => {
            Type::Vector(ind.clone(),
                    Box::new(reduce_type_helper(context, typ, memory.clone())),
                    h.clone())
        },
        Type::Sequence(ind, typ, h) => {
            Type::Sequence(ind.clone(),
                    Box::new(reduce_type_helper(context, typ, memory.clone())),
                    h.clone())
        },
        _ => type_.clone()
    }
}
