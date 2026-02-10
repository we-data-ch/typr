use crate::components::context::Context;
use crate::components::language::var::Var;
use crate::components::r#type::argument_type::ArgumentType;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::Type;
use crate::processes::type_checking::unification::type_substitution;
use rpds::Vector;

pub fn reduce_param(
    context: &Context,
    param: &ArgumentType,
    memory: Vector<String>, // List of pairs [X, Y1]
) -> ArgumentType {
    // Returns list of pairs [X, Y2]

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

pub fn reduce_alias(
    aliased_type: Type,
    generics: &[Type],
    concret_types: &[Type],
    name: &str,
    memory: Vector<String>,
    context: &Context,
) -> Type {
    let substituted = type_substitution(
        &aliased_type,
        &generics
            .iter()
            .zip(concret_types.iter())
            .map(|(r#gen, typ)| (r#gen.clone(), typ.clone()))
            .collect::<Vec<_>>(),
    );
    reduce_type_helper(context, &substituted, memory.push_back(name.to_string()))
}

pub fn reduce_type_helper(context: &Context, type_: &Type, memory: Vector<String>) -> Type {
    match type_ {
        Type::Record(args, h) => Type::Record(
            args.iter()
                .map(|arg| reduce_param(context, arg, memory.clone()))
                .collect(),
            h.clone(),
        ),
        Type::Alias(name, concret_types, is_opaque, h) => {
            match (is_opaque, is_in_memory(name, &memory)) {
                (true, _) | (_, true) => type_.clone(),
                (false, _) => {
                    match Var::from_type(type_.clone()) {
                        Some(var) => {
                            context
                                .get_matching_alias_signature(&var)
                                .map(|(aliased_type, generics)| {
                                    reduce_alias(
                                        aliased_type,
                                        &generics,
                                        concret_types,
                                        name,
                                        memory,
                                        context,
                                    )
                                })
                                // Return Any type instead of panicking if alias not found
                                .unwrap_or_else(|| Type::Any(h.clone()))
                        }
                        None => Type::Any(h.clone()),
                    }
                }
            }
        }
        Type::Tag(name, inner, h) => Type::Tag(
            name.clone(),
            Box::new(reduce_type_helper(context, inner, memory.clone())),
            h.clone(),
        ),
        Type::If(typ, _conditions, _) => *typ.clone(),
        Type::Function(typs, ret_typ, h) => {
            let typs2 = typs
                .iter()
                .map(|x| reduce_type_helper(context, x, memory.clone()))
                .collect::<Vec<_>>();
            let ret_typ2 = reduce_type_helper(context, ret_typ, memory.clone());
            Type::Function(typs2, Box::new(ret_typ2), h.to_owned())
        }
        Type::Vec(vtype, ind, typ, h) => Type::Vec(
            *vtype,
            ind.clone(),
            Box::new(reduce_type_helper(context, typ, memory.clone())),
            h.clone(),
        ),
        Type::Operator(TypeOperator::Union, t1, t2, _) => {
            let typ1: Type = (**t1).clone();
            let typ2: Type = (**t2).clone();
            if typ1.is_subtype(&typ2, context) {
                typ1
            } else if typ2.is_subtype(&typ1, context) {
                typ2
            } else {
                type_.clone()
            }
        }
        Type::Operator(TypeOperator::Access, t1, t2, h) => {
            let t1_inner = (**t1).clone();
            let t2_inner = (**t2).clone();
            match (t1_inner, t2_inner) {
                (Type::Variable(module_name, _), Type::Alias(alias_name, _args, _opaque, _)) => {
                    context
                        .get_type_from_variable(&Var::from_name(&module_name))
                        .ok()
                        .and_then(|t| t.to_module_type().ok())
                        .and_then(|module_type| module_type.get_type_from_name(&alias_name).ok())
                        .unwrap_or_else(|| Type::Any(h.clone()))
                }
                _ => Type::Any(h.clone()),
            }
        }
        t => t.clone(),
    }
}
