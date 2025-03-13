use std::fs::File;
use std::io::{Write, Result};
use crate::Type;
use crate::kind::Kind;
use crate::NominalContext;

fn format_kind(ki: &Kind) -> String {
   match ki {
       Kind::Type => "k_type",
       _ => "k_index"
   }.to_string()
}

fn format(ty: &Type) -> String {
    match ty {
        Type::Alias(name, params, path) => {
            if params.len() == 0 {
                format!("{}", name)
            } else {
            let paras = params.iter().map(|typ| format(typ)).collect::<Vec<_>>();
                format!("{}<{}>", name, paras.join(", "))
            }
        },
        Type::Array(dim, ty) => format!("[{}, {}]", dim, format(ty)),
        Type::Function(kinds, params, ret_ty) => {
            let formatted_kinds = kinds.iter().map(|arg_kind| format_kind(&arg_kind.get_kind())).collect::<Vec<_>>();
            let formatted_params = params.iter().map(|param| format(param)).collect::<Vec<_>>();
            format!("fn<{}>({}) -> {}", formatted_kinds.join(", "), formatted_params.join(", "), format(ret_ty))
        }
        Type::Tag(name, param) => format!(".{}({})", name, format(param)),
        Type::Record(fields) => {
            let formatted_fields = fields.iter().map(|arg_typ| format!("{}: {}", arg_typ.get_argument(), format(&arg_typ.get_type()))).collect::<Vec<_>>();
            format!("{{{}}}", formatted_fields.join(", "))
        }
        Type::Generic(name) => name.to_uppercase(),
        Type::Index(gen) => format!("#{}", gen),
        Type::Number => "Num".to_string(),
        Type::Boolean => "Bool".to_string(),
        Type::Integer => "Int".to_string(),
        Type::Char => "Char".to_string(),
        Type::Empty => "Empty".to_string(),
        Type::Union(types) => {
            let formatted_types = types.iter().map(|ty| format(&ty.to_type())).collect::<Vec<_>>();
            format!("Union({})", formatted_types.join(", "))
        }
        Type::Any => "Any".to_string(),
        _ => "Unknown".to_string(),
    }
}

pub fn pretty_print(ty: &Type) {
    let formatted = format(ty);
    println!("Type checking:\n{:?}\n", formatted);
}

fn write_structure(structure: &str) -> Result<()> {
    let mut file = File::create("context.txt")?;
    writeln!(file, "{}", structure)?;
    Ok(())
}

