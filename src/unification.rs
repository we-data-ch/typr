use crate::r#type::Type;
use crate::argument_type::ArgumentType;
use crate::Context;
use crate::type_comparison;
use crate::tag::Tag;

pub fn type_substitution(type_: &Type, substitutions: &[(Type, Type)]) -> Type {
    if substitutions.is_empty() {
        return type_.clone();
    }

    match type_ {
        // Generic type substitution
        Type::Generic(_, _) => {
            if let Some((_, replacement)) = substitutions.iter()
                .find(|(gen_name, _)| gen_name == type_) {
                replacement.clone()
            } else {
                type_.clone()
            }
        }

        // Index generic substitution
        Type::IndexGen(name, h) => {
            if let Some((_, replacement)) = substitutions.iter()
                .find(|(idx_name, _)| idx_name == &Type::IndexGen(name.clone(), h.clone())) {
                replacement.clone()
            } else {
                type_.clone()
            }
        }

        // Label generic substitution
        Type::LabelGen(name, h) => {
            if let Some((_, replacement)) = substitutions.iter()
                .find(|(idx_name, _)| idx_name == &Type::LabelGen(name.clone(), h.clone())) {
                replacement.clone()
            } else {
                type_.clone()
            }
        }

        // Arithmetic operations
        Type::Add(t1, t2, h) => {
            let v1 = type_substitution(t1, substitutions);
            let v2 = type_substitution(t2, substitutions);
            match (v1.clone(), v2.clone()) {
                (Type::Number(h), Type::Number(_)) => Type::Number(h),
                (Type::Integer(i1, h), Type::Integer(i2, _)) => Type::Integer(i1+i2, h),
                _ => Type::Add(Box::new(v1), Box::new(v2), h.clone())
            }
        }

        Type::Minus(t1, t2, h) => {
            let v1 = type_substitution(t1, substitutions);
            let v2 = type_substitution(t2, substitutions);
            match (v1.clone(), v2.clone()) {
                (Type::Number(h), Type::Number(_)) => Type::Number(h),
                (Type::Integer(i1, h), Type::Integer(i2, _)) => Type::Integer(i1-i2, h),
                _ => Type::Minus(Box::new(v1), Box::new(v2), h.clone())
            }
        }

        Type::Mul(t1, t2, h) => {
            let v1 = type_substitution(t1, substitutions);
            let v2 = type_substitution(t2, substitutions);
            match (v1.clone(), v2.clone()) {
                (Type::Number(h), Type::Number(_)) => Type::Number(h),
                (Type::Integer(i1, h), Type::Integer(i2, _)) => Type::Integer(i1*i2, h),
                _ => Type::Mul(Box::new(v1), Box::new(v2), h.clone())
            }
        }

        Type::Div(t1, t2, h) => {
            let v1 = type_substitution(t1, substitutions);
            let v2 = type_substitution(t2, substitutions);
            match (v1.clone(), v2.clone()) {
                (Type::Number(h), Type::Number(_)) => Type::Number(h),
                (Type::Integer(i1, h), Type::Integer(i2, _)) => Type::Integer(i1/i2, h),
                _ => Type::Div(Box::new(v1), Box::new(v2), h.clone())
            }
        }

        // Array type substitution
        Type::Array(size, element_type, h) => {
            Type::Array(
                Box::new(type_substitution(size, substitutions)),
                Box::new(type_substitution(element_type, substitutions)),
                h.clone()
            )
        }

        // Record type substitution
        Type::Record(fields, h) => {
            Type::Record(
                fields.iter()
                    .map(|arg_type| {
                        ArgumentType(
                            arg_type.0.clone(),
                            type_substitution(&arg_type.1, substitutions),
                            arg_type.2)
                    })
                    .collect(), h.clone()
            )
        }

        // Function type substitution
        Type::Function(kinds, params, return_type, h) => {
            Type::Function(
                kinds.clone(),
                params.iter()
                    .map(|param| type_substitution(param, substitutions))
                    .collect(),
                Box::new(type_substitution(return_type, substitutions)),
                h.clone()
            )
        }

        // Alias type substitution
        Type::Alias(name, params, base_type, opacity, h) => {
            Type::Alias(
                name.clone(),
                params.iter()
                    .map(|param| type_substitution(param, substitutions))
                    .collect(),
                base_type.clone(),
                opacity.clone(),
                h.clone()
            )
        }

        // Tag type substitution
        Type::Tag(name, inner_type, h) => {
            Type::Tag(
                name.clone(),
                Box::new(type_substitution(inner_type, substitutions)),
                h.clone()
            )
        }

        Type::Union(types, h) => {
            let new_types = types.iter()
                .map(|typ| {
                    Tag::from_type(type_substitution(&typ.to_type(), substitutions)).unwrap()
                }).collect::<Vec<_>>();
            Type::Union(new_types, h.clone())
        }

        // Default case: return the type unchanged
        _ => type_.clone()
    }
}

fn match_wildcard(fields: &[ArgumentType], arg_type: ArgumentType) -> Vec<(Type, Type)> {
    let (labels, types) = fields.iter()
        .fold((vec![], vec![]),
        |(mut lbl, mut typ), el| {
            lbl.push(el.get_argument());
            typ.push(el.get_type());
            (lbl, typ)
        });
    vec![
        (arg_type.get_argument(), Type::Tuple(labels.clone(), labels.into())),
        (arg_type.get_type(), Type::Tuple(types.clone(), types.into()))
    ]
}

// Add these new functions to the previous implementation

fn unification_helper(values: &[Type], type1: &Type, type2: &Type
) -> Vec<(Type, Type)> {
    match (type1, type2) {
        // Direct equality case
        (t1, t2) if t1 == t2 => vec![],
        // Any case
        (Type::Any(_), _) => vec![],
        (_, Type::Any(_)) => vec![],

        // Generic case
        (t, Type::Generic(g, h)) | (Type::Generic(g, h), t) => {
            vec![(Type::Generic(g.clone(), h.clone()), t.clone())]
        }

        // label generic case with label
        (Type::Char(s, h), Type::LabelGen(g, h2)) | (Type::LabelGen(g, h2), Type::Char(s, h)) => {
            vec![(Type::LabelGen(g.clone(), h2.clone()), Type::Char(s.clone(), h.clone()))]
        }

        // Index generic case with number
        (Type::Integer(i, h), Type::IndexGen(g, h2)) | (Type::IndexGen(g, h2), Type::Integer(i, h)) => {
            vec![(Type::IndexGen(g.clone(), h2.clone()), Type::Integer(*i, h.clone()))]
        }

        // Function case
        (Type::Function(_, params1, ret1, _), Type::Function(_, params2, ret2, _)) => {
            if params1.len() != params2.len() {
                return vec![];
            }

            // Unify return types
            let mut matches = unification_helper(values, ret1, ret2);

            // Unify parameters
            for (p1, p2) in params1.iter().zip(params2.iter()) {
                let param_matches = unification_helper(values, p1, p2);
                merge_substitutions(&mut matches, param_matches);
            }

            matches
        }

        // Array case
        (Type::Array(size1, elem1, _), Type::Array(size2, elem2, _)) => {
            let size_matches = unification_helper(values, size1, size2);
            let elem_matches = unification_helper(values, elem1, elem2);
            let mut combined = size_matches;
            merge_substitutions(&mut combined, elem_matches);
            combined
        }

        // Tag case
        (Type::Tag(name1, type1, _h1), Type::Tag(name2, type2, _h2)) if name1 == name2 => {
            unification_helper(values, type1, type2)
        }

        // Record case
        (Type::Record(fields1, _), Type::Record(fields2, _)) => {
            if let Some((intersection1, intersection2)) = record_intersection(fields1, fields2) {
                let types1: Vec<_> = intersection1.iter().map(|arg| &arg.1).collect();
                let types2: Vec<_> = intersection2.iter().map(|arg| &arg.1).collect();
                
                let mut all_matches = vec![];
                for (t1, t2) in types1.iter().zip(types2.iter()) {
                    let matches = unification_helper(values, t1, t2);
                    merge_substitutions(&mut all_matches, matches);
                }
                all_matches
            } else if let Some(arg_type) = type2.get_type_pattern() {
               match_wildcard(fields1, arg_type)
            } else {
                vec![]
            }
        }

        // Default case - types are not unifiable
        _ => vec![]
    }
}

pub fn unify(cont: &Context, type1: &Type, type2: &Type) -> Vec<(Type, Type)> {
    let new_type1 = type_comparison::reduce_type(cont, type1);
    let new_type2 = type_comparison::reduce_type(cont, type2);
    // try unification helper
    unification_helper(&vec![], &new_type1, &new_type2)
}

// Helper functions needed for unification

fn merge_substitutions(existing: &mut Vec<(Type, Type)>, new: Vec<(Type, Type)>) {
    for (name, type_) in new {
        if let Some(pos) = existing.iter().position(|(n, _)| n == &name) {
            existing[pos] = (name, type_);
        } else {
            existing.push((name, type_));
        }
    }
}

pub fn record_intersection(
    record1: &[ArgumentType],
    record2: &[ArgumentType]
) -> Option<(Vec<ArgumentType>, Vec<ArgumentType>)> {
    // Get labels (left elements) from both records
    let labels1: Vec<String> = record1.iter()
        .map(|arg| arg.get_argument_str().clone())  // Assuming ArgumentType has a label field
        .collect();
    
    let labels2: Vec<String> = record2.iter()
        .map(|arg| arg.get_argument_str())
        .collect();

    // Find intersection of labels
    let common_labels: Vec<String> = labels1.iter()
        .filter(|label| labels2.contains(label))
        .cloned()
        .collect();

    // Get values for the common labels from each record
    let mut values1 = Vec::new();
    let mut values2 = Vec::new();

    for label in &common_labels {
        if let Some(value1) = record1.iter()
            .find(|arg| arg.get_argument_str() == *label)
            .cloned() {
            if let Some(value2) = record2.iter()
                .find(|arg| arg.get_argument_str() == *label)
                .cloned() {
                values1.push(value1);
                values2.push(value2);
            }
        }
    }

    // Merge labels with their respective values
    let intersection1 = common_labels.iter()
        .zip(values1.into_iter())
        .map(|(_label, value)| value)
        .collect();

    let intersection2 = common_labels.iter()
        .zip(values2.into_iter())
        .map(|(_label, value)| value)
        .collect();

    Some((intersection1, intersection2))
}
