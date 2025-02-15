use std::fs::File;
use std::io::{Write, Result};

fn format(ty: &Type) -> String {
    match ty {
        Type::TArray(dim, ty) => format!("[{}, {}]", dim, format(ty)),
        Type::Var(name, _, _, _, Type::Params(params)) if params.is_empty() => name.clone(),
        Type::Var(name, _, _, _, Type::Params(params)) => {
            let formatted_params = params.iter().map(|param| format(param)).collect::<Vec<_>>();
            format!("{}({})", name, formatted_params.join(", "))
        }
        Type::Var(name, _, _, _, _) => name.clone(),
        Type::TFn(kinds, params, ret_ty) => {
            let formatted_kinds = kinds.iter().map(|kind| format(kind)).collect::<Vec<_>>();
            let formatted_params = params.iter().map(|param| format(param)).collect::<Vec<_>>();
            format!("fn<{}>({}) -> {}", formatted_kinds.join(", "), formatted_params.join(", "), format(ret_ty))
        }
        Type::TTag(name, param) => format!("{}({})", name, format(param)),
        Type::TRecord(fields) => {
            let formatted_fields = fields.iter().map(|(label, ty)| format!("{}: {}", label, format(ty))).collect::<Vec<_>>();
            format!("{{{}}}", formatted_fields.join(", "))
        }
        Type::Gen(name) => name.to_uppercase(),
        Type::Ind(gen) => format!("#{}", gen.to_uppercase()),
        Type::Num => "Num".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Int => "Int".to_string(),
        Type::Empty => "Empty".to_string(),
        Type::Union(types) => {
            let formatted_types = types.iter().map(|ty| format(ty)).collect::<Vec<_>>();
            format!("Union({})", formatted_types.join(", "))
        }
        _ => "Unknown".to_string(),
    }
}

fn pretty_print(ty: &Type) {
    let formatted = format(ty);
    println!("{}", formatted);
}

fn write_structure(structure: &str) -> Result<()> {
    let mut file = File::create("context.txt")?;
    writeln!(file, "{}", structure)?;
    Ok(())
}

