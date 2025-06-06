use crate::Type;
use crate::kind::Kind;
use crate::help_data::HelpData;

fn format_kind(ki: &Kind) -> String {
   match ki {
       Kind::Type => "k_type",
       _ => "k_index"
   }.to_string()
}

pub fn format(ty: &Type) -> String {
    match ty {
        Type::Alias(name, params, _path, _) => {
            if params.len() == 0 {
                format!("{}", name)
            } else {
            let paras = params.iter().map(|typ| format(typ)).collect::<Vec<_>>();
                format!("{}<{}>", name, paras.join(", "))
            }
        },
        Type::Array(dim, ty, _) => format!("[{}, {}]", dim, format(ty)),
        Type::Function(kinds, params, ret_ty, _h) => {
            let formatted_kinds = kinds.iter().map(|arg_kind| format_kind(&arg_kind.get_kind())).collect::<Vec<_>>();
            let formatted_params = params.iter().map(|param| format(param)).collect::<Vec<_>>();
            format!("fn<{}>({}) -> {}", formatted_kinds.join(", "), formatted_params.join(", "), format(ret_ty))
        }
        Type::Tag(name, param, _) => {
            if (**param == Type::Any(HelpData::default())) || (**param == Type::Empty(HelpData::default())){
                format!(".{}", name)
            } else {
                format!(".{}({})", name, format(param))
            }
        },
        Type::Record(fields, _) => {
            let formatted_fields = fields.iter().map(|arg_typ| format!("{}: {}", arg_typ.get_argument(), format(&arg_typ.get_type()))).collect::<Vec<_>>();
            format!("{{{}}}", formatted_fields.join(", "))
        }
        Type::Generic(name, _) => name.to_uppercase(),
        Type::Integer(gen, _) => format!("#{}", gen),
        Type::Number(_) => "num".to_string(),
        Type::Boolean(_) => "bool".to_string(),
        Type::Integer(_, _) => "int".to_string(),
        Type::Char(_) => "char".to_string(),
        Type::Empty(_) => "Empty".to_string(),
        Type::Union(types, _) => {
            let formatted_types = types.iter().map(|ty| format(&ty.to_type())).collect::<Vec<_>>();
            format!("{}", formatted_types.join(" | "))
        }
        Type::Any(_) => "any".to_string(),
        Type::IndexGen(i, _) => format!("#{}", i),
        Type::LabelGen(l, _) => format!("%{}", l),
        Type::Tuple(elements, _) => {
            let body = elements.iter()
                .map(format)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", body)
        },
        Type::Label(l, _) => l.to_string(),
        Type::Add(a, b, _) => format!("{}+{}", a, b),
        Type::Minus(a, b, _) => format!("{}-{}", a, b),
        Type::Mul(a, b, _) => format!("{}*{}", a, b),
        Type::Div(a, b, _) => format!("{}/{}", a, b),
        t => format!("{:?}", t)
    }
}

pub fn pretty_print(ty: &Type) {
    let formatted = format(ty);
    println!("Type checking:\n{:?}\n", formatted);
}
