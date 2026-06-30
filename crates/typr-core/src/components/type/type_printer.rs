use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::intersection_type::IntersectionType;
use crate::components::r#type::pretty;
use crate::components::r#type::tchar::Tchar;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::type_category::TypeCategory;
use crate::components::r#type::type_operator::TypeOperator;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;

fn simplify_for_dataframe(ty: &Type) -> String {
    let fmt = |t: &Type| format(t);
    match ty {
        Type::Vec(VecType::Vector, _, inner, _) => simplify_for_dataframe(inner),
        Type::Vec(VecType::DataFrame, _, inner, _) => simplify_for_dataframe(inner),
        Type::Vec(VecType::Array, _, inner, _) => simplify_for_dataframe(inner),
        Type::Vec(VecType::S3, _, inner, _) => simplify_for_dataframe(inner),
        Type::Record(fields, _) => {
            let formatted_fields = fields
                .iter()
                .map(|arg_typ| {
                    format!(
                        "{}: {}",
                        arg_typ.get_argument(),
                        simplify_for_dataframe(&arg_typ.get_type())
                    )
                })
                .collect::<Vec<_>>();
            format!("{{{}}}", formatted_fields.join(", "))
        }
        _ => fmt(ty),
    }
}

pub fn format(ty: &Type) -> String {
    match ty {
        Type::Alias(name, params, _, _) => {
            if params.is_empty() {
                name.to_string()
            } else {
                let paras = params.iter().map(verbose).collect::<Vec<_>>();
                format!("{}<{}>", name, paras.join(", "))
            }
        }
        Type::Vec(vtype, dim, ty, _) => {
            let inner = if matches!(vtype, VecType::DataFrame) {
                simplify_for_dataframe(ty)
            } else {
                verbose(ty)
            };
            format!("{}[{}, {}]", vtype, short(dim), inner)
        }
        Type::Function(params, ret_ty, _h) => {
            let formatted_params = params
                .iter()
                .map(|arg| format!("{}: {}", arg.get_argument_str(), format(&arg.get_type())))
                .collect::<Vec<_>>();
            format!("fn({}) -> {}", formatted_params.join(", "), format(ret_ty))
        }
        Type::Tag(name, param, _) => {
            if (**param == Type::Any(HelpData::default()))
                || (**param == Type::Empty(HelpData::default()))
            {
                format!(".{}", name)
            } else {
                format!(".{}({})", name, format(param))
            }
        }
        Type::Record(fields, _) => {
            let formatted_fields = fields
                .iter()
                .map(|arg_typ| {
                    format!(
                        "{}: {}",
                        arg_typ.get_argument_str(),
                        format(&arg_typ.get_type())
                    )
                })
                .collect::<Vec<_>>();
            format!("list{{{}}}", formatted_fields.join(", "))
        }
        Type::Generic(name, _) => name.to_uppercase(),
        Type::Integer(_, _) => "int".to_string(),
        Type::Number(tnum, _) => match tnum {
            crate::components::r#type::tnumber::Tnum::Val(v) => format!("{}", v),
            _ => "num".to_string(),
        },
        Type::Boolean(tbool, _) => match tbool {
            crate::components::r#type::tbool::Tbool::Val(true) => "true".to_string(),
            crate::components::r#type::tbool::Tbool::Val(false) => "false".to_string(),
            _ => "bool".to_string(),
        },
        Type::Char(tchar, _) => match tchar {
            Tchar::Val(c) => c.to_string(),
            _ => "char".to_string(),
        },
        Type::Null(_) => "null".to_string(),
        Type::Empty(_) => "Empty".to_string(),
        Type::Any(_) => "any".to_string(),
        Type::IndexGen(i, _) => format!("#{}", i),
        Type::LabelGen(l, _) => format!("${}", l),
        Type::KindedGen(k, name, _) => format!("{}{}", k, name),
        Type::Tuple(elements, _) => {
            let body = elements.iter().map(format).collect::<Vec<_>>().join(", ");
            format!("tuple{{{}}}", body)
        }
        Type::Operator(
            op @ (TypeOperator::Addition
            | TypeOperator::Substraction
            | TypeOperator::Multiplication
            | TypeOperator::Division),
            a,
            b,
            _,
        ) => format!("{}{}{}", a, op, b),
        Type::UnknownFunction(_) => "UnknownFunction".to_string(),
        Type::RClass(elem, _) => format!(
            "class({})",
            elem.iter().cloned().collect::<Vec<_>>().join(", ")
        ),
        Type::Operator(TypeOperator::Intersection, _, _, _) => {
            IntersectionType::try_from(ty.clone())
                .map(|intersection| {
                    intersection
                        .get_types()
                        .iter()
                        .map(|x| x.pretty())
                        .collect::<Vec<_>>()
                        .join(" & ")
                })
                .unwrap_or_default()
        }
        Type::Interface(args, _) => {
            format!("interface{{ {} }}", pretty(args.clone()))
        }
        Type::Module(args, _, _) => {
            let body = args
                .iter()
                .map(|arg| arg.pretty2())
                .collect::<Vec<_>>()
                .join("\n");
            format!("Module {{\n{}\n}}", body)
        }
        Type::Operator(TypeOperator::Access, left, right, _) => {
            format!("{}${}", left.pretty(), right.pretty())
        }
        Type::Operator(op, left, right, _) => {
            format!("({} {} {})", left.pretty(), op, right.pretty())
        }
        Type::Variable(name, _) => name.to_string(),
        t => format!("{:?}", t),
    }
}

pub fn short(t: &Type) -> String {
    match t {
        Type::Integer(tint, _) => match tint {
            Tint::Val(i) => format!("{}", i),
            _ => "int".to_string(),
        },
        val => val.pretty(),
    }
}

pub fn verbose(t: &Type) -> String {
    match t {
        Type::Integer(tint, _) => match tint {
            Tint::Val(i) => format!("int({})", i),
            _ => "int".to_string(),
        },
        Type::Number(_, _) => "num".to_string(),
        Type::Boolean(_, _) => "bool".to_string(),
        Type::Null(_) => "null".to_string(),
        Type::Char(_tchar, _) => "char".to_string(),
        Type::Record(fields, _) => {
            let formatted_fields = fields
                .iter()
                .map(|arg_typ| {
                    format!(
                        "{}: {}",
                        format(&arg_typ.get_argument()),
                        format(&arg_typ.get_type())
                    )
                })
                .collect::<Vec<_>>();
            format!("list{{{}}}", formatted_fields.join(", "))
        }
        Type::IndexGen(idgen, _) => format!("#{}", idgen),
        Type::KindedGen(k, name, _) => format!("{}{}", k, name),
        Type::Vec(vtype, i, t, _) => {
            format!("{}[{}, {}]", vtype, i.pretty(), t.pretty2())
        }
        val if val.to_category() == TypeCategory::Template => val.pretty(),
        val => val.pretty(), //val => panic!("{:?} doesn't have a second format", val)
    }
}

pub fn litteral(t: &Type) -> String {
    match t {
        Type::Integer(tint, _) => match tint {
            Tint::Val(i) => format!("{}", i),
            _ => "int".to_string(),
        },
        Type::Char(tchar, _) => match tchar {
            Tchar::Val(c) => format!("'{}'", c),
            _ => "char".to_string(),
        },
        val => val.pretty2(), //val => panic!("{:?} doesn't have a second format", val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::r#type::type_operator::TypeOperator;
    use crate::utils::builder;

    #[test]
    fn test_my_pretty() {
        let typ = Type::Operator(
            TypeOperator::Union,
            Box::new(builder::boolean_type()),
            Box::new(builder::number_type()),
            HelpData::default(),
        );
        assert_eq!(typ.pretty(), "(bool | num)");
    }

    #[test]
    fn test_label_gen_prints_with_dollar_sigil() {
        let typ = Type::LabelGen("L".to_string(), HelpData::default());
        assert_eq!(typ.pretty(), "$L");
    }

    #[test]
    fn test_kinded_gen_prints_with_its_sigil() {
        use crate::components::r#type::kind::Kind;
        assert_eq!(
            Type::KindedGen(Kind::Record, "R".to_string(), HelpData::default()).pretty(),
            "%R"
        );
        assert_eq!(
            Type::KindedGen(Kind::Interface, "T".to_string(), HelpData::default()).pretty(),
            "@T"
        );
        assert_eq!(
            Type::KindedGen(Kind::String, "S".to_string(), HelpData::default()).pretty(),
            "^S"
        );
        assert_eq!(
            Type::KindedGen(Kind::Boolean, "B".to_string(), HelpData::default()).pretty(),
            "?B"
        );
    }
}
