use crate::Type;
use crate::help_data::HelpData;
use crate::tint::Tint;
use crate::tchar::Tchar;
use crate::type_category::TypeCategory;
use crate::graph::TypeSystem;
use crate::r#type::pretty;
use crate::type_operator::TypeOperator;

pub fn format(ty: &Type) -> String {
    match ty {
        Type::Alias(name, params, _, _) => {
            if params.len() == 0 {
                format!("{}", name)
            } else {
            let paras = params.iter().map(|typ| format2(typ)).collect::<Vec<_>>();
                format!("{}<{}>", name, paras.join(", "))
            }
        },
        Type::Array(dim, ty, _) => {
            format!("[{}, {}]", format2(dim), format(ty))
        },
        Type::Vector(dim, ty, _) => {
            format!("Vec[{}, {}]", format2(dim), format(ty))
        },
        Type::Sequence(dim, ty, _) => {
            format!("Seq[{}, {}]", format2(dim), format(ty))
        },
        Type::Function(params, ret_ty, _h) => {
            let formatted_params = params.iter().map(|param| format(param)).collect::<Vec<_>>();
            format!("fn({}) -> {}", formatted_params.join(", "), format(ret_ty))
        }
        Type::Tag(name, param, _) => {
            if (**param == Type::Any(HelpData::default())) || (**param == Type::Empty(HelpData::default())){
                format!(".{}", name)
            } else {
                format!(".{}({})", name, format(param))
            }
        },
        Type::Record(fields, _) => {
            let formatted_fields = fields.iter().map(|arg_typ| format!("{}: {}", format2(&arg_typ.get_argument()), format(&arg_typ.get_type()))).collect::<Vec<_>>();
            format!("list{{{}}}", formatted_fields.join(", "))
        }
        Type::Generic(name, _) => name.to_uppercase(),
        Type::Integer(tint, _) => {
            match tint {
                Tint::Val(i) => format!("{}", i),
                _ => "int".to_string()
            }
        },
        Type::Number(_) => "num".to_string(),
        Type::Boolean(_) => "bool".to_string(),
        Type::Char(tchar, _) => {
            match tchar {
                Tchar::Val(c) => format!("{}", c),
                _ => "char".to_string()
            }
        },
        Type::Empty(_) => "Empty".to_string(),
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
        Type::Add(a, b, _) => format!("{}+{}", a, b),
        Type::Minus(a, b, _) => format!("{}-{}", a, b),
        Type::Mul(a, b, _) => format!("{}*{}", a, b),
        Type::Div(a, b, _) => format!("{}/{}", a, b),
        Type::UnknownFunction(_) => "UnknownFunction".to_string(),
        Type::RClass(elem, _) => format!("class({})", elem.iter().cloned().collect::<Vec<_>>().join(", ")),
        Type::Union(s, _) => format!("{}", s.iter().cloned().map(|x| x.pretty()).collect::<Vec<_>>().join(" | ")),
        Type::Intersection(s, _) => format!("{}", s.iter().cloned().map(|x| x.pretty()).collect::<Vec<_>>().join(" & ")),
        Type::Interface(args, _) => {
            format!("interface{{ {} }}", pretty(args.clone()))
        }
        Type::Module(args, _) => {
            let body = args.iter()
                .map(|arg| arg.pretty2())
                .collect::<Vec<_>>().join("\n");
            format!("Module {{\n{}\n}}", body)
        },
        Type::Operator(TypeOperator::Access, left, right, _) => {
            format!("{}${}", left.pretty(), right.pretty())
        },
        Type::Operator(op, left, right, _) => {
            format!("({} {} {})", left.pretty(), op.to_string(), right.pretty())
        },
        Type::Variable(name, _) => name.to_string(),
        t => format!("{:?}", t)
    }
}

pub fn format2(t: &Type) -> String {
    match t {
        Type::Integer(tint, _) => {
            match tint {
                Tint::Val(i) => format!("int({})", i),
                _ => "int".to_string()
            }
        },
        Type::Number(_) => "num".to_string(),
        Type::Boolean(_) => "bool".to_string(),
        Type::Char(_tchar, _) => {
            "char".to_string()
        },
        Type::Record(fields, _) => {
            //"char".to_string()
            let formatted_fields = fields.iter().map(|arg_typ| format!("{}: {}", format(&arg_typ.get_argument()), format(&arg_typ.get_type()))).collect::<Vec<_>>();
            format!("list{{{}}}", formatted_fields.join(", "))
        },
        Type::IndexGen(idgen, _) => format!("#{}", idgen),
        Type::Array(i, t, _) => {
            format!("[{}, {}]", i.pretty(), t.pretty2())
        },
        Type::Vector(i, t, _) => {
            format!("Vec[{}, {}]", i.pretty(), t.pretty2())
        },
        val if val.to_category() == TypeCategory::Template
            => val.pretty(),
        val => val.pretty()
        //val => panic!("{:?} doesn't have a second format", val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::type_operator::TypeOperator;
    use crate::builder;

    #[test]
    fn test_my_pretty(){
        let typ = Type::Operator(
                TypeOperator::Union,
                Box::new(builder::boolean_type()),
                Box::new(builder::number_type()),
                HelpData::default()
                );
        dbg!(&typ.pretty());
        assert!(true)
    }
}
