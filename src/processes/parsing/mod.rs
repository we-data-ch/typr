pub mod elements;
pub mod indexation;
pub mod lang_token;
pub mod operation_priority;
pub mod type_token;
pub mod types;
pub mod vector_priority;

use crate::components::context::config::Config;
use crate::components::error_message::help_data::HelpData;
use crate::components::error_message::syntax_error::SyntaxError;
use crate::components::language::operators::custom_op;
use crate::components::language::operators::Op;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::language::ModulePosition;
use crate::components::r#type::Type;
use crate::processes::parsing::elements::break_exp;
use crate::processes::parsing::elements::chars;
use crate::processes::parsing::elements::parse_elements;
use crate::processes::parsing::elements::return_exp;
use crate::processes::parsing::elements::scope;
use crate::processes::parsing::elements::single_element;
use crate::processes::parsing::elements::tag_exp;
use crate::processes::parsing::elements::variable;
use crate::processes::parsing::elements::variable2;
use crate::processes::parsing::elements::variable_exp;
use crate::processes::parsing::elements::variable_recognizer;
use crate::processes::parsing::elements::vector;
use crate::processes::parsing::elements::Case;
use crate::processes::parsing::types::ltype;
use crate::processes::parsing::types::type_alias;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::line_ending;
use nom::character::complete::multispace0;
use nom::character::complete::not_line_ending;
use nom::combinator::opt;
use nom::multi::many0;
use nom::sequence::delimited;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::IResult;
use nom::Parser;
use nom_locate::LocatedSpan;
use std::ops::Deref;

type Span<'a> = LocatedSpan<&'a str, String>;

/// Result of parsing containing the AST and any syntax errors collected
#[derive(Debug, Clone)]
pub struct ParseResult {
    /// The parsed AST (may contain Lang::SyntaxErr nodes)
    pub ast: Lang,
    /// All syntax errors collected from the AST
    pub errors: Vec<SyntaxError>,
}

impl ParseResult {
    /// Create a new ParseResult from an AST, automatically collecting errors
    pub fn new(ast: Lang) -> Self {
        let errors = collect_syntax_errors(&ast);
        ParseResult { ast, errors }
    }

    /// Check if parsing produced any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the AST (regardless of errors)
    pub fn get_ast(&self) -> &Lang {
        &self.ast
    }

    /// Get a clean AST with SyntaxErr nodes replaced by their inner expressions
    pub fn get_clean_ast(&self) -> Lang {
        clean_syntax_errors(&self.ast)
    }
}

/// Recursively collect all SyntaxError from a Lang AST
fn collect_syntax_errors(lang: &Lang) -> Vec<SyntaxError> {
    let mut errors = Vec::new();
    collect_syntax_errors_recursive(lang, &mut errors);
    errors
}

fn collect_syntax_errors_recursive(lang: &Lang, errors: &mut Vec<SyntaxError>) {
    match lang {
        Lang::SyntaxErr(inner, err) => {
            errors.push(err.clone());
            collect_syntax_errors_recursive(inner, errors);
        }
        Lang::Lines(v, _)
        | Lang::Scope(v, _)
        | Lang::Array(v, _)
        | Lang::Tuple(v, _)
        | Lang::Vector(v, _)
        | Lang::Sequence(v, _)
        | Lang::Test(v, _) => {
            for item in v {
                collect_syntax_errors_recursive(item, errors);
            }
        }
        Lang::Module(_, v, _, _, _) => {
            for item in v {
                collect_syntax_errors_recursive(item, errors);
            }
        }
        Lang::Function(_, _, body, _) => {
            collect_syntax_errors_recursive(body, errors);
        }
        Lang::Let(var, _, body, _) => {
            collect_syntax_errors_recursive(var, errors);
            collect_syntax_errors_recursive(body, errors);
        }
        Lang::Alias(var, _, _, _) => {
            collect_syntax_errors_recursive(var, errors);
        }
        Lang::If(cond, then_branch, else_branch, _) => {
            collect_syntax_errors_recursive(cond, errors);
            collect_syntax_errors_recursive(then_branch, errors);
            collect_syntax_errors_recursive(else_branch, errors);
        }
        Lang::Match(expr, _, cases, _) => {
            collect_syntax_errors_recursive(expr, errors);
            for (_, case_body) in cases {
                collect_syntax_errors_recursive(case_body, errors);
            }
        }
        Lang::FunctionApp(func, args, _) | Lang::VecFunctionApp(func, args, _) => {
            collect_syntax_errors_recursive(func, errors);
            for arg in args {
                collect_syntax_errors_recursive(arg, errors);
            }
        }
        Lang::MethodCall(obj, args, _, _) => {
            collect_syntax_errors_recursive(obj, errors);
            for arg in args {
                collect_syntax_errors_recursive(arg, errors);
            }
        }
        Lang::Operator(_, left, right, _) => {
            collect_syntax_errors_recursive(left, errors);
            collect_syntax_errors_recursive(right, errors);
        }
        Lang::Union(left, right, _) => {
            collect_syntax_errors_recursive(left, errors);
            collect_syntax_errors_recursive(right, errors);
        }
        Lang::Assign(target, value, _) => {
            collect_syntax_errors_recursive(target, errors);
            collect_syntax_errors_recursive(value, errors);
        }
        Lang::ArrayIndexing(arr, idx, _) => {
            collect_syntax_errors_recursive(arr, errors);
            collect_syntax_errors_recursive(idx, errors);
        }
        Lang::Tag(_, inner, _) => {
            collect_syntax_errors_recursive(inner, errors);
        }
        Lang::Return(inner, _)
        | Lang::Lambda(inner, _)
        | Lang::Not(inner, _)
        | Lang::TestBlock(inner, _) => {
            collect_syntax_errors_recursive(inner, errors);
        }
        Lang::ForLoop(_, iter, body, _) => {
            collect_syntax_errors_recursive(iter, errors);
            collect_syntax_errors_recursive(body, errors);
        }
        Lang::WhileLoop(cond, body, _) => {
            collect_syntax_errors_recursive(cond, errors);
            collect_syntax_errors_recursive(body, errors);
        }
        Lang::Use(lib, members, _) => {
            collect_syntax_errors_recursive(lib, errors);
            collect_syntax_errors_recursive(members, errors);
        }
        Lang::KeyValue(_, value, _) => {
            collect_syntax_errors_recursive(value, errors);
        }
        Lang::JSBlock(inner, _, _) => {
            collect_syntax_errors_recursive(inner, errors);
        }
        Lang::Record(args, _) => {
            for arg in args {
                collect_syntax_errors_recursive(&arg.1, errors);
            }
        }
        Lang::RFunction(args, _, _) => {
            for arg in args {
                collect_syntax_errors_recursive(arg, errors);
            }
        }
        // Leaf nodes - no children to check
        Lang::Number(_, _)
        | Lang::Integer(_, _)
        | Lang::Bool(_, _)
        | Lang::Char(_, _)
        | Lang::Variable(_, _, _, _)
        | Lang::Comment(_, _)
        | Lang::ModuleImport(_, _)
        | Lang::Import(_, _)
        | Lang::GenFunc(_, _, _)
        | Lang::VecBlock(_, _)
        | Lang::Library(_, _)
        | Lang::Exp(_, _)
        | Lang::Signature(_, _, _)
        | Lang::Empty(_)
        | Lang::Break(_)
        | Lang::ModuleDecl(_, _) => {}
    }
}

/// Clean an AST by replacing SyntaxErr nodes with their inner expressions
fn clean_syntax_errors(lang: &Lang) -> Lang {
    match lang {
        Lang::SyntaxErr(inner, _) => clean_syntax_errors(inner),
        Lang::Lines(v, h) => Lang::Lines(v.iter().map(clean_syntax_errors).collect(), h.clone()),
        Lang::Scope(v, h) => Lang::Scope(v.iter().map(clean_syntax_errors).collect(), h.clone()),
        Lang::Array(v, h) => Lang::Array(v.iter().map(clean_syntax_errors).collect(), h.clone()),
        Lang::Tuple(v, h) => Lang::Tuple(v.iter().map(clean_syntax_errors).collect(), h.clone()),
        Lang::Vector(v, h) => Lang::Vector(v.iter().map(clean_syntax_errors).collect(), h.clone()),
        Lang::Sequence(v, h) => {
            Lang::Sequence(v.iter().map(clean_syntax_errors).collect(), h.clone())
        }
        Lang::Test(v, h) => Lang::Test(v.iter().map(clean_syntax_errors).collect(), h.clone()),
        Lang::Module(name, v, pos, config, h) => Lang::Module(
            name.clone(),
            v.iter().map(clean_syntax_errors).collect(),
            pos.clone(),
            config.clone(),
            h.clone(),
        ),
        Lang::Function(params, ret_ty, body, h) => Lang::Function(
            params.clone(),
            ret_ty.clone(),
            Box::new(clean_syntax_errors(body)),
            h.clone(),
        ),
        Lang::Let(var, ty, body, h) => Lang::Let(
            Box::new(clean_syntax_errors(var)),
            ty.clone(),
            Box::new(clean_syntax_errors(body)),
            h.clone(),
        ),
        Lang::If(cond, then_b, else_b, h) => Lang::If(
            Box::new(clean_syntax_errors(cond)),
            Box::new(clean_syntax_errors(then_b)),
            Box::new(clean_syntax_errors(else_b)),
            h.clone(),
        ),
        Lang::Match(expr, var, cases, h) => {
            let clean_cases = cases
                .iter()
                .map(|(ty, body)| (ty.clone(), Box::new(clean_syntax_errors(body))))
                .collect();
            Lang::Match(
                Box::new(clean_syntax_errors(expr)),
                var.clone(),
                clean_cases,
                h.clone(),
            )
        }
        Lang::FunctionApp(func, args, h) => Lang::FunctionApp(
            Box::new(clean_syntax_errors(func)),
            args.iter().map(clean_syntax_errors).collect(),
            h.clone(),
        ),
        Lang::VecFunctionApp(func, args, h) => Lang::VecFunctionApp(
            Box::new(clean_syntax_errors(func)),
            args.iter().map(clean_syntax_errors).collect(),
            h.clone(),
        ),
        Lang::MethodCall(obj, args, ty, h) => Lang::MethodCall(
            Box::new(clean_syntax_errors(obj)),
            args.iter().map(clean_syntax_errors).collect(),
            ty.clone(),
            h.clone(),
        ),
        Lang::Operator(op, left, right, h) => Lang::Operator(
            op.clone(),
            Box::new(clean_syntax_errors(left)),
            Box::new(clean_syntax_errors(right)),
            h.clone(),
        ),
        Lang::Union(left, right, h) => Lang::Union(
            Box::new(clean_syntax_errors(left)),
            Box::new(clean_syntax_errors(right)),
            h.clone(),
        ),
        Lang::Assign(target, value, h) => Lang::Assign(
            Box::new(clean_syntax_errors(target)),
            Box::new(clean_syntax_errors(value)),
            h.clone(),
        ),
        Lang::ArrayIndexing(arr, idx, h) => Lang::ArrayIndexing(
            Box::new(clean_syntax_errors(arr)),
            Box::new(clean_syntax_errors(idx)),
            h.clone(),
        ),
        Lang::Tag(name, inner, h) => Lang::Tag(
            name.clone(),
            Box::new(clean_syntax_errors(inner)),
            h.clone(),
        ),
        Lang::Return(inner, h) => Lang::Return(Box::new(clean_syntax_errors(inner)), h.clone()),
        Lang::Lambda(inner, h) => Lang::Lambda(Box::new(clean_syntax_errors(inner)), h.clone()),
        Lang::Not(inner, h) => Lang::Not(Box::new(clean_syntax_errors(inner)), h.clone()),
        Lang::TestBlock(inner, h) => {
            Lang::TestBlock(Box::new(clean_syntax_errors(inner)), h.clone())
        }
        Lang::ForLoop(var, iter, body, h) => Lang::ForLoop(
            var.clone(),
            Box::new(clean_syntax_errors(iter)),
            Box::new(clean_syntax_errors(body)),
            h.clone(),
        ),
        Lang::WhileLoop(cond, body, h) => Lang::WhileLoop(
            Box::new(clean_syntax_errors(cond)),
            Box::new(clean_syntax_errors(body)),
            h.clone(),
        ),
        Lang::Use(lib, members, h) => Lang::Use(
            Box::new(clean_syntax_errors(lib)),
            Box::new(clean_syntax_errors(members)),
            h.clone(),
        ),
        Lang::KeyValue(key, value, h) => {
            Lang::KeyValue(key.clone(), Box::new(clean_syntax_errors(value)), h.clone())
        }
        Lang::JSBlock(inner, n, h) => {
            Lang::JSBlock(Box::new(clean_syntax_errors(inner)), *n, h.clone())
        }
        // Nodes that don't need deep cleaning or are leaf nodes
        other => other.clone(),
    }
}

fn pattern_var(s: Span) -> IResult<Span, (Vec<Lang>, Option<String>)> {
    let res = alt((tag_exp, variable2)).parse(s);
    match res {
        Ok((s, Lang::Tag(name, val, _h))) => {
            if let Lang::Variable(name2, mutopa, typ, h) = *val {
                Ok((
                    s,
                    (
                        vec![Lang::Variable(name2.to_string(), mutopa, typ, h.clone())],
                        Some(name.to_string()),
                    ),
                ))
            } else {
                Ok((s, (vec![], Some(name.to_string()))))
            }
        }
        Ok((s, Lang::Variable(name, mutopa, typ, h))) => Ok((
            s,
            (vec![Lang::Variable(name, mutopa, typ, h.clone())], None),
        )),
        Err(r) => Err(r),
        _ => todo!(),
    }
}

fn single_parse(s: Span) -> IResult<Span, Lang> {
    let res = (parse_elements, opt(terminated(tag(";"), multispace0))).parse(s);
    match res {
        Ok((s, (exp, Some(_)))) => Ok((s, exp)),
        Ok((s, (exp, None))) => {
            // Return a SyntaxError wrapped in the Lang to be collected later
            Ok((
                s,
                Lang::SyntaxErr(
                    Box::new(exp.clone()),
                    SyntaxError::ForgottenSemicolon(exp.into()),
                ),
            ))
        }
        Err(r) => Err(r),
    }
}

fn equality_operator(s: Span) -> IResult<Span, Span> {
    terminated(alt((tag("="), tag("<-"))), multispace0).parse(s)
}

fn base_let_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("let"), multispace0),
        pattern_var,
        opt(preceded(terminated(tag(":"), multispace0), ltype)),
        equality_operator,
        single_parse,
    )
        .parse(s);
    match res {
        Ok((s, (_let, (pat_var, None), typ, _eq, Lang::Function(params, ty, body, h))))
            if params.len() > 0 =>
        {
            let newvar = Var::from_language(pat_var[0].clone())
                .unwrap()
                .set_type(params[0].1.clone());
            Ok((
                s,
                vec![Lang::Let(
                    Box::new(newvar.to_language()),
                    typ.unwrap_or(Type::Empty(HelpData::default())),
                    Box::new(Lang::Function(params, ty, body, h)),
                    _let.into(),
                )],
            ))
        }
        Ok((s, (_let, (pat_var, None), typ, _eq, body))) => Ok((
            s,
            vec![Lang::Let(
                Box::new(pat_var[0].clone()),
                typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                Box::new(body),
                _let.into(),
            )],
        )),
        Ok((s, (_let, (pat_var, Some(_)), typ, eq, body))) => {
            if pat_var.len() == 1 {
                Ok((
                    s,
                    vec![Lang::Let(
                        Box::new(pat_var[0].clone()),
                        typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                        Box::new(Lang::Operator(
                            Op::Dollar(HelpData::default()),
                            Box::new(Lang::Number(0.0, eq.into())),
                            Box::new(body),
                            pat_var.into(),
                        )),
                        _let.into(),
                    )],
                ))
            } else {
                Ok((
                    s,
                    pat_var
                        .iter()
                        .map(|x| {
                            Lang::Let(
                                Box::new(x.clone()),
                                typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                                Box::new(body.clone()),
                                HelpData::default(),
                            )
                        })
                        .collect::<Vec<_>>(),
                ))
            }
        }
        Err(r) => Err(r),
    }
}

fn let_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(terminated(tag("pub"), multispace0)), base_let_exp).parse(s);
    match res {
        Ok((s, (None, le))) => Ok((s, le)),
        Ok((s, (Some(_pu), le))) => {
            let new_le = le
                .iter()
                .map(|x| match x {
                    Lang::Let(var, typ, body, h) => {
                        let vari = Var::from_language(var.deref().clone())
                            .unwrap()
                            .to_language();
                        Lang::Let(Box::new(vari), typ.clone(), body.clone(), h.clone())
                    }
                    lan => lan.clone(),
                })
                .collect();
            Ok((s, new_le))
        }
        Err(r) => Err(r),
    }
}

fn base_type_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("type"), multispace0),
        type_alias,
        equality_operator,
        ltype,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_ty, Type::Alias(name, params, _, h), _eq, ty, _))) => {
            let h2 = if params.len() > 0 {
                params[0].clone().into()
            } else {
                HelpData::default()
            };
            let vari = Var::from_name(&name)
                .set_type(Type::Params(params.clone(), h2))
                .to_language();
            Ok((s, Lang::Alias(Box::new(vari), params, ty, h)))
        }
        Ok((s, (_ty, _, _eq, _ty2, _))) => Ok((s, Lang::Empty(_ty.into()))),
        Err(r) => Err(r),
    }
}

fn type_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(terminated(tag("pub"), multispace0)), base_type_exp).parse(s);
    match res {
        Ok((s, (Some(_pu), ali))) => Ok((s, vec![ali])),
        Ok((s, (None, Lang::Alias(var, params, typ, h)))) => {
            let vari = Var::from_language(var.deref().clone())
                .unwrap()
                .to_language();
            Ok((s, vec![Lang::Alias(Box::new(vari), params, typ, h)]))
        }
        Err(r) => Err(r),
        _ => todo!(),
    }
}

fn base_opaque_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("opaque"), multispace0),
        type_alias,
        terminated(tag("="), multispace0),
        ltype,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_ty, Type::Alias(name, params, _, h), _eq, ty, _))) => {
            let vari = Var::from_name(&name)
                .set_type(Type::Params(params.clone(), params.clone().into()))
                .set_opacity(true)
                .to_language();
            Ok((s, Lang::Alias(Box::new(vari), params, ty, h)))
        }
        Ok((s, (_ty, _, _eq, _ty2, _))) => Ok((s, Lang::Empty(_ty.into()))),
        Err(r) => Err(r),
    }
}

fn opaque_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(terminated(tag("pub"), multispace0)), base_opaque_exp).parse(s);
    match res {
        Ok((s, (Some(_pu), Lang::Alias(var, params, typ, h)))) => {
            let vari = Var::from_language(var.deref().clone())
                .unwrap()
                .set_opacity(true)
                .to_language();
            Ok((s, vec![Lang::Alias(Box::new(vari), params, typ, h)]))
        }
        Ok((s, (None, Lang::Alias(var, params, typ, h)))) => {
            let vari = Var::from_language(var.deref().clone())
                .unwrap()
                .set_opacity(true)
                .to_language();
            Ok((s, vec![Lang::Alias(Box::new(vari), params, typ, h)]))
        }
        Err(r) => Err(r),
        _ => todo!(),
    }
}

pub fn module(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("module"), multispace0),
        terminated(variable_exp, multispace0),
        terminated(tag("{"), multispace0),
        base_parse,
        terminated(tag("}"), multispace0),
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (modu, (name, _), _op, v, _cl, _dv))) => Ok((
            s,
            vec![Lang::Module(
                name,
                v,
                ModulePosition::Internal,
                Config::default(),
                modu.into(),
            )],
        )),
        Err(r) => Err(r),
    }
}

fn assign(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        variable,
        alt((
            terminated(tag("="), multispace0),
            terminated(tag("<-"), multispace0),
        )),
        parse_elements,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, ((var, _), _eq, exp, _pv))) => Ok((
            s,
            vec![Lang::Assign(
                Box::new(var.clone()),
                Box::new(exp),
                var.into(),
            )],
        )),
        Err(r) => Err(r),
    }
}

fn comment(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("#"), not_line_ending, opt(line_ending), multispace0).parse(s);
    match res {
        Ok((s, (_hashtag, txt, _, _))) => {
            Ok((s, vec![Lang::Comment(txt.to_string(), _hashtag.into())]))
        }
        Err(r) => Err(r),
    }
}

pub fn simple_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (parse_elements, terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (lang, _sc))) => Ok((s, vec![lang])),
        Err(r) => Err(r),
    }
}

fn mod_imp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("mod"), multispace0),
        terminated(variable_exp, multispace0),
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_mod, (name, _), _sc))) => {
            Ok((s, vec![Lang::ModuleImport(name.to_string(), _mod.into())]))
        }
        Err(r) => Err(r),
    }
}

fn import_var(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("use"), multispace0),
        variable,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_use, (lang, case), _sc))) => {
            let res = match case {
                Case::Maj => Var::from_language(lang).unwrap().to_alias_lang(),
                _ => Var::from_language(lang).unwrap().to_let(),
            };
            Ok((s, vec![res]))
        }
        Err(r) => Err(r),
    }
}

fn import_type(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("use"), multispace0),
        type_alias,
        terminated(tag(";"), multispace0),
    )
        .parse(s);

    match res {
        Ok((s, (_use, alias, _sc))) => Ok((s, vec![Lang::Import(alias, _use.into())])),
        Err(r) => Err(r),
    }
}

fn tests(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("Test"), delimited(tag("["), base_parse, tag("]"))).parse(s);
    match res {
        Ok((s, (_t, body))) => Ok((s, vec![Lang::Test(body, _t.into())])),
        Err(r) => Err(r),
    }
}

fn library(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        tag("library("),
        variable_exp,
        tag(")"),
        opt(tag(";")),
        multispace0,
    )
        .parse(s);

    match res {
        Ok((s, (_lib, (var, h), _cl, Some(_col), _))) => {
            Ok((s, vec![Lang::Library(var, h.clone())]))
        }
        Ok((_, (_lib, _var, _cl, None, _))) => {
            panic!("You forgot to put a ';' at the end of the line")
        }
        Err(r) => Err(r),
    }
}

fn use_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        tag("use("),
        chars,
        tag(", "),
        alt((vector, chars)),
        terminated(tag(");"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (us, lib, _, members, _))) => Ok((
            s,
            vec![Lang::Use(Box::new(lib), Box::new(members), us.into())],
        )),
        Err(r) => Err(r),
    }
}

fn custom_operators(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = custom_op.parse(s);
    match res {
        Ok((s, co)) => Ok((s, (co.clone().to_string(), co.into()))),
        Err(r) => Err(r),
    }
}

fn signature_variable(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        tag("@"),
        alt((variable_recognizer, custom_operators)),
        terminated(tag(":"), multispace0),
        ltype,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (at, (name, h), _col, typ, _))) => {
            let var2 = Var::from_name(&name).set_help_data(h).set_type(typ.clone());
            Ok((s, vec![Lang::Signature(var2, typ, at.into())]))
        }
        Err(r) => Err(r),
    }
}

fn signature_opaque(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        tag("@"),
        type_alias,
        terminated(tag(":"), multispace0),
        ltype,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (at, Type::Alias(name, _params, _, h), _, typ, _))) => {
            let var2 = Var::from_name(&name)
                .set_help_data(h.clone())
                .set_type(typ.clone());
            Ok((s, vec![Lang::Signature(var2, typ, at.into())]))
        }
        Ok((_s, (_, _, _, _, _))) => todo!(),
        Err(r) => Err(r),
    }
}

pub fn signature(s: Span) -> IResult<Span, Vec<Lang>> {
    alt((signature_opaque, signature_variable)).parse(s)
}

fn for_loop(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("for"), multispace0),
        terminated(tag("("), multispace0),
        terminated(variable_exp, multispace0),
        terminated(tag("in"), multispace0),
        terminated(single_element, multispace0),
        terminated(tag(")"), multispace0),
        scope,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_for, _op, (var_str, _h), _in, iterator, _cl, scop, _semi))) => Ok((
            s,
            vec![Lang::ForLoop(
                Var::from_name(&var_str),
                Box::new(iterator),
                Box::new(scop),
                _for.into(),
            )],
        )),
        Err(r) => Err(r),
    }
}

fn while_loop(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("while"), multispace0),
        terminated(tag("("), multispace0),
        terminated(single_element, multispace0),
        terminated(tag(")"), multispace0),
        scope,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_while, _op, condition, _cl, scop, _semi))) => Ok((
            s,
            vec![Lang::WhileLoop(
                Box::new(condition),
                Box::new(scop),
                _while.into(),
            )],
        )),
        Err(r) => Err(r),
    }
}

fn test_block(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("Test"), multispace0), scope).parse(s);
    //parse_block).parse(s);

    match res {
        Ok((s, (tst, body))) => Ok((s, vec![Lang::TestBlock(Box::new(body), tst.into())])),
        Err(r) => Err(r),
    }
}

// main
pub fn base_parse(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        opt(multispace0),
        many0(alt((
            library,
            break_exp,
            use_exp,
            test_block,
            while_loop,
            for_loop,
            signature,
            tests,
            import_type,
            import_var,
            mod_imp,
            comment,
            type_exp,
            opaque_exp,
            let_exp,
            module,
            assign,
            simple_exp,
        ))),
        opt(alt((return_exp, parse_elements))),
    )
        .parse(s);
    match res {
        Ok((s, (_, v, Some(exp)))) => {
            let mut new_v = v.iter().flatten().cloned().collect::<Vec<_>>();
            new_v.push(exp);
            Ok((s, new_v))
        }
        Ok((s, (_, v, None))) => Ok((s, v.iter().flatten().cloned().collect())),
        Err(r) => Err(r),
    }
}

/// Parse source code and return a ParseResult containing the AST and any syntax errors
///
/// This function collects syntax errors instead of panicking, allowing the caller
/// to handle errors appropriately (e.g., display all errors, continue with partial AST)
pub fn parse(s: Span) -> ParseResult {
    let res = base_parse(s.clone());
    match res {
        Ok((_, v)) => ParseResult::new(Lang::Lines(v.clone(), v.into())),
        Err(_) => panic!("Can't parse string {}", s),
    }
}

/// Parse source code and return just the Lang AST (legacy behavior)
///
/// This function is kept for backwards compatibility. It returns the AST directly
/// and will panic if parsing fails completely.
pub fn parse_legacy(s: Span) -> Lang {
    parse(s).ast
}

pub fn parse2(s: Span) -> Result<Lang, String> {
    let res = base_parse(s.clone());
    match res {
        Ok((_, v)) => Ok(v[0].clone()),
        Err(_) => Err(format!("Can't parse string {}", s)),
    }
}

// main test
#[cfg(test)]
mod tesus {
    use super::*;
    use crate::utils::builder;

    #[test]
    fn test_semicolon1() {
        let res = parse("let a <- 5".into());
        // This should now collect a ForgottenSemicolon error
        assert!(
            res.has_errors(),
            "Missing semicolon should produce a syntax error"
        );
        assert_eq!(res.errors.len(), 1, "Should have exactly one error");
        match &res.errors[0] {
            SyntaxError::ForgottenSemicolon(_) => (),
            _ => panic!("Expected ForgottenSemicolon error"),
        }
    }

    #[test]
    fn test_type_exp2() {
        let res = type_exp("type Mat<M, N, T> = [M, [N, T]];".into())
            .unwrap()
            .0;
        assert_eq!(res, "alias(var('Mat'), [M, N, T], [M, [N, T]])".into());
    }

    #[test]
    fn test_assign1() {
        let res = assign("a <- 12;".into()).unwrap().1;
        assert_eq!(
            "Assign",
            res[0].simple_print(),
            "The expression 'a <- 12;' should be identified as an assignation"
        );
    }

    #[test]
    fn test_assign2() {
        let res = assign("a.b() <- 12;".into()).unwrap().1;
        assert_eq!(
            "Assign",
            res[0].simple_print(),
            "The expression 'a.b() <- 12;' should be identified as an assignation"
        );
    }

    #[test]
    fn test_assign3() {
        let res = assign("a$b <- 12;".into()).unwrap().1;
        assert_eq!(
            "Assign",
            res[0].simple_print(),
            "The expression 'a$b <- 12;' should be identified as an assignation"
        );
    }
}
