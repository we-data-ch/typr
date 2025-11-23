use crate::unification::type_substitution;
use crate::argument_type::ArgumentType;
use crate::r#type::Type;
use crate::var::Var;
use crate::context::Context;
use crate::tag::Tag;
use rpds::Vector;

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

pub fn reduce_alias(aliased_type: Type, generics: &[Type], concret_types: &[Type], name: &str, memory: Vector<String>, context: &Context) -> Type {
    let substituted = type_substitution(
        &aliased_type,
        &generics.iter()
            .zip(concret_types.iter())
            .map(|(r#gen, typ)| (r#gen.clone(), typ.clone()))
            .collect::<Vec<_>>()
    );
    reduce_type_helper(context, &substituted, memory.push_back(name.to_string()))
}

pub fn reduce_type_helper(context: &Context, type_: &Type, memory: Vector<String>) -> Type {
    match type_ {
        Type::Record(args, h) => {
            Type::Record(args.iter()
                .map(|arg| reduce_param(context, arg, memory.clone()))
                .collect(), h.clone())
        },
        Type::Alias(name, concret_types, is_opaque, _h) => {
            match (is_opaque, is_in_memory(name, &memory)) {
                (true, _) | (_, true) => type_.clone(),
                (false, _) => {
                    let var = Var::from_type(type_.clone()).unwrap();
                    context.get_matching_alias_signature(&var)
                        .map(|(aliased_type, generics)| 
                             reduce_alias(aliased_type, &generics, concret_types, name, memory, context))
                        .expect(&format!("The alias {} wasn't found in the context\n{}",
                                type_, context.display_typing_context()))
                }
            }
        },
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
        Type::Function(typs, ret_typ, h) => {
            let typs2 = typs.iter()
                .map(|x| reduce_type_helper(context, x, memory.clone()))
                .collect::<Vec<_>>();
            let ret_typ2 = reduce_type_helper(context, ret_typ, memory.clone());
            Type::Function(typs2, Box::new(ret_typ2), h.to_owned())
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
