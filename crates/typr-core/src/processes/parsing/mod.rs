#![allow(dead_code)]

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
use crate::processes::parsing::elements::tuple_exp;
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

use std::cell::RefCell;

thread_local! {
    static PARSE_ERRORS: RefCell<Vec<SyntaxError>> = RefCell::new(Vec::new());
}

fn push_parse_error(err: SyntaxError) {
    PARSE_ERRORS.with(|e| e.borrow_mut().push(err));
}

fn take_parse_errors() -> Vec<SyntaxError> {
    PARSE_ERRORS.with(|e| e.borrow_mut().drain(..).collect())
}

/// Result of parsing containing the AST and any syntax errors collected
#[derive(Debug, Clone)]
pub struct ParseResult {
    pub ast: Lang,
    pub errors: Vec<SyntaxError>,
}

impl ParseResult {
    pub fn new(ast: Lang) -> Self {
        let errors = take_parse_errors();
        ParseResult { ast, errors }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn get_ast(&self) -> &Lang {
        &self.ast
    }

    pub fn get_clean_ast(&self) -> Lang {
        self.ast.clone()
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
            push_parse_error(SyntaxError::ForgottenSemicolon(exp.clone().into()));
            Ok((s, exp))
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
        Ok((
            s,
            (
                _let,
                (pat_var, None),
                typ,
                _eq,
                Lang::Function {
                    parameters: params,
                    return_type: ty,
                    body,
                    help_data: h,
                },
            ),
        )) if !params.is_empty() => {
            let newvar = Var::from_language(pat_var[0].clone())
                .unwrap()
                .set_type(params[0].1.clone());
            Ok((
                s,
                vec![Lang::Let {
                    variable: Box::new(newvar.to_language()),
                    r#type: typ.unwrap_or(Type::Empty(HelpData::default())),
                    expression: Box::new(Lang::Function {
                        parameters: params,
                        return_type: ty,
                        body,
                        help_data: h,
                    }),
                    help_data: _let.into(),
                }],
            ))
        }
        Ok((s, (_let, (pat_var, None), typ, _eq, body))) => Ok((
            s,
            vec![Lang::Let {
                variable: Box::new(pat_var[0].clone()),
                r#type: typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                expression: Box::new(body),
                help_data: _let.into(),
            }],
        )),
        Ok((s, (_let, (pat_var, Some(_)), typ, eq, body))) => {
            if pat_var.len() == 1 {
                Ok((
                    s,
                    vec![Lang::Let {
                        variable: Box::new(pat_var[0].clone()),
                        r#type: typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                        expression: Box::new(Lang::Operator(
                            Op::Dollar(HelpData::default()),
                            Box::new(Lang::Number {
                                value: 0.0,
                                help_data: eq.into(),
                            }),
                            Box::new(body),
                            pat_var.into(),
                        )),
                        help_data: _let.into(),
                    }],
                ))
            } else {
                Ok((
                    s,
                    pat_var
                        .iter()
                        .map(|x| Lang::Let {
                            variable: Box::new(x.clone()),
                            r#type: typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                            expression: Box::new(body.clone()),
                            help_data: HelpData::default(),
                        })
                        .collect::<Vec<_>>(),
                ))
            }
        }
        Err(r) => Err(r),
    }
}

fn let_tuple_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("let"), multispace0),
        tuple_exp,
        opt(preceded(terminated(tag(":"), multispace0), ltype)),
        equality_operator,
        single_parse,
    )
        .parse(s);
    match res {
        Ok((s, (_let, Lang::Tuple(elements, _th), typ, _eq, body))) => {
            let tmp_name = "__tuple_tmp__";
            let tmp_var = Var::from_name(tmp_name).to_language();

            // First: let __tuple_tmp__ <- body;
            let tmp_let = Lang::Let {
                variable: Box::new(tmp_var.clone()),
                r#type: typ.unwrap_or(Type::Empty(HelpData::default())),
                expression: Box::new(body),
                help_data: _let.into(),
            };

            // Then: let a <- 1.__tuple_tmp__; let b <- 2.__tuple_tmp__; ...
            let mut result = vec![tmp_let];
            for (i, elem) in elements.iter().enumerate() {
                if let Lang::Variable(name, _, _, _) = elem {
                    if name == "_" {
                        continue; // skip wildcard
                    }
                }
                result.push(Lang::Let {
                    variable: Box::new(elem.clone()),
                    r#type: Type::Empty(HelpData::default()),
                    expression: Box::new(Lang::Operator(
                        Op::Dot(HelpData::default()),
                        Box::new(Lang::Integer {
                            value: (i + 1) as i32,
                            help_data: HelpData::default(),
                        }),
                        Box::new(tmp_var.clone()),
                        HelpData::default(),
                    )),
                    help_data: HelpData::default(),
                });
            }

            Ok((s, result))
        }
        Ok(_) => unreachable!("tuple_exp always returns Lang::Tuple"),
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
                    Lang::Let {
                        variable: var,
                        r#type: typ,
                        expression: body,
                        help_data: h,
                    } => {
                        let vari = Var::from_language(var.deref().clone())
                            .unwrap()
                            .to_language();
                        Lang::Let {
                            variable: Box::new(vari),
                            r#type: typ.clone(),
                            expression: body.clone(),
                            help_data: h.clone(),
                        }
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
            let h2 = if !params.is_empty() {
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
            vec![Lang::Module {
                name,
                body: v,
                module_position: ModulePosition::Internal,
                config: Config::default(),
                help_data: modu.into(),
            }],
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
        opt(terminated(tag(";"), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, ((var, _), _eq, exp, Some(_)))) => Ok((
            s,
            vec![Lang::Assign(
                Box::new(var.clone()),
                Box::new(exp),
                var.into(),
            )],
        )),
        Ok((s, ((var, _), _eq, exp, None))) => {
            push_parse_error(SyntaxError::ForgottenSemicolon(exp.clone().into()));
            let assign = Lang::Assign(Box::new(var.clone()), Box::new(exp), var.into());
            Ok((s, vec![assign]))
        }
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
    let res = (parse_elements, opt(terminated(tag(";"), multispace0))).parse(s);
    match res {
        Ok((s, (lang, Some(_)))) => Ok((s, vec![lang])),
        Ok((s, (lang, None))) => {
            push_parse_error(SyntaxError::ForgottenSemicolon(lang.clone().into()));
            Ok((s, vec![lang]))
        }
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

fn return_stmt(s: Span) -> IResult<Span, Vec<Lang>> {
    let (s, e) = return_exp(s)?;
    Ok((s, vec![e]))
}

fn stmt_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (parse_elements, terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (lang, _))) => Ok((s, vec![lang])),
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
    let initial = s.clone();
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
            let_tuple_exp,
            let_exp,
            module,
            assign,
            return_stmt,
            stmt_exp,
        ))),
        opt(parse_elements),
    )
        .parse(s);
    match res {
        // Case 1: nothing → Empty
        Ok((s, (_, v, None))) if v.is_empty() => Ok((s, vec![Lang::Empty(initial.into())])),
        // Case 2: no statements, one trailing expression (no ";") → return it directly
        Ok((s, (_, v, Some(expr)))) if v.is_empty() => Ok((s, vec![expr])),
        // Case 3: one or more statements + optional trailing expression
        Ok((s, (_, v, trailing))) => {
            let mut result: Vec<Lang> = v.into_iter().flatten().collect();
            if let Some(expr) = trailing {
                result.push(expr);
            }
            Ok((s, result))
        }
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

/// Parse source code from a string with a filename for error reporting
///
/// This is the main entry point for parsing source code in the public API.
/// It registers the source for error display and returns the AST.
pub fn parse_from_string(source: &str, filename: &str) -> Lang {
    use crate::components::error_message::help_data::register_source;

    // Register source for error display
    register_source(filename, source);

    // Create a span with the filename as extra data
    let span: Span = LocatedSpan::new_extra(source, filename.to_string());

    // Parse and return the AST
    parse(span).ast
}

/// Parse source code from a string with a filename, returning full ParseResult
pub fn parse_from_string_with_errors(source: &str, filename: &str) -> ParseResult {
    use crate::components::error_message::help_data::register_source;

    // Register source for error display
    register_source(filename, source);

    // Create a span with the filename as extra data
    let span: Span = LocatedSpan::new_extra(source, filename.to_string());

    // Parse and return full result
    parse(span)
}

// main test
#[cfg(test)]
mod tesus {
    use super::*;

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
    fn test_semicolon_standalone_exp() {
        let res = parse("f(x)".into());
        // Case 2: a single expression without ";" is valid — no error
        assert!(
            !res.has_errors(),
            "A standalone expression without semicolon is valid (case 2)"
        );
    }

    #[test]
    fn test_semicolon_assign() {
        let res = parse("a <- 12".into());
        assert!(
            res.has_errors(),
            "An assign expression without semicolon should produce a syntax error"
        );
        match &res.errors[0] {
            SyntaxError::ForgottenSemicolon(_) => (),
            _ => panic!("Expected ForgottenSemicolon error"),
        }
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

    // ==================== Let Tuple Destructuring Tests ====================

    #[test]
    fn test_let_tuple_basic() {
        let res = let_tuple_exp("let :{a, b, c} <- :{1, 2, 3};".into())
            .unwrap()
            .1;
        // Should produce 4 Let statements: 1 tmp + 3 bindings
        assert_eq!(
            res.len(),
            4,
            "Should produce 4 Let statements (1 tmp + 3 bindings)"
        );
        for item in &res {
            assert!(
                item.simple_print().starts_with("let"),
                "Each item should be a Let, got: {}",
                item.simple_print()
            );
        }
    }

    #[test]
    fn test_let_tuple_tmp_variable() {
        let res = let_tuple_exp("let :{a, b} <- :{1, 2};".into()).unwrap().1;
        // First Let should bind __tuple_tmp__
        if let Lang::Let { variable: var, .. } = &res[0] {
            if let Lang::Variable(name, _, _, _) = var.as_ref() {
                assert_eq!(name, "__tuple_tmp__");
            } else {
                panic!("Expected Variable in first Let");
            }
        } else {
            panic!("Expected Let");
        }
    }

    #[test]
    fn test_let_tuple_bindings() {
        let res = let_tuple_exp("let :{x, y} <- :{10, 20};".into()).unwrap().1;
        // Second Let should bind 'x' using Dot access
        if let Lang::Let {
            variable: var,
            expression: body,
            ..
        } = &res[1]
        {
            if let Lang::Variable(name, _, _, _) = var.as_ref() {
                assert_eq!(name, "x");
            } else {
                panic!("Expected Variable 'x'");
            }
            assert_eq!(
                body.simple_print(),
                "Operator",
                "Body should be a Dot operator"
            );
        } else {
            panic!("Expected Let");
        }
        // Third Let should bind 'y'
        if let Lang::Let { variable: var, .. } = &res[2] {
            if let Lang::Variable(name, _, _, _) = var.as_ref() {
                assert_eq!(name, "y");
            } else {
                panic!("Expected Variable 'y'");
            }
        } else {
            panic!("Expected Let");
        }
    }

    #[test]
    fn test_let_tuple_wildcard() {
        let res = let_tuple_exp("let :{a, _, c} <- :{1, 2, 3};".into())
            .unwrap()
            .1;
        // Should produce 3 Let statements: 1 tmp + 2 bindings (wildcard skipped)
        assert_eq!(
            res.len(),
            3,
            "Should produce 3 Let statements (wildcard skipped)"
        );
        // Second Let should bind 'a'
        if let Lang::Let { variable: var, .. } = &res[1] {
            if let Lang::Variable(name, _, _, _) = var.as_ref() {
                assert_eq!(name, "a");
            } else {
                panic!("Expected Variable 'a'");
            }
        } else {
            panic!("Expected Let");
        }
        // Third Let should bind 'c'
        if let Lang::Let { variable: var, .. } = &res[2] {
            if let Lang::Variable(name, _, _, _) = var.as_ref() {
                assert_eq!(name, "c");
            } else {
                panic!("Expected Variable 'c'");
            }
        } else {
            panic!("Expected Let");
        }
    }

    #[test]
    fn test_let_tuple_in_full_parse() {
        let res = parse("let :{a, b, c} <- :{1, 2, 3};".into());
        assert!(
            !res.has_errors(),
            "Let tuple destructuring should parse without errors"
        );
    }

    #[test]
    fn test_let_tuple_with_equals() {
        let res = let_tuple_exp("let :{a, b} = :{1, 2};".into()).unwrap().1;
        assert_eq!(res.len(), 3, "Should work with '=' operator too");
    }

    #[test]
    fn test_let_tuple_type_check() {
        use crate::components::context::Context;
        use crate::processes::type_checking::typing;
        use crate::utils::builder;

        let ast = parse("let :{a, b, c} <- :{1, 2, 3};".into()).ast;
        let context = Context::empty();
        let tc = typing(&context, &ast);

        // After type-checking, 'a', 'b', 'c' should be in the context as integers
        let ty_a = tc
            .context
            .get_type_from_existing_variable(crate::components::language::var::Var::from_name("a"));
        let ty_b = tc
            .context
            .get_type_from_existing_variable(crate::components::language::var::Var::from_name("b"));
        let ty_c = tc
            .context
            .get_type_from_existing_variable(crate::components::language::var::Var::from_name("c"));

        assert_eq!(
            ty_a,
            builder::integer_type(1),
            "Variable 'a' should be Integer(1)"
        );
        assert_eq!(
            ty_b,
            builder::integer_type(2),
            "Variable 'b' should be Integer(2)"
        );
        assert_eq!(
            ty_c,
            builder::integer_type(3),
            "Variable 'c' should be Integer(3)"
        );
    }

    #[test]
    fn test_let_tuple_type_check_mixed_types() {
        use crate::components::context::Context;
        use crate::processes::type_checking::typing;
        use crate::utils::builder;

        let ast = parse("let :{x, y} <- :{1, 'hello'};".into()).ast;
        let context = Context::empty();
        let tc = typing(&context, &ast);

        let ty_x = tc
            .context
            .get_type_from_existing_variable(crate::components::language::var::Var::from_name("x"));
        let ty_y = tc
            .context
            .get_type_from_existing_variable(crate::components::language::var::Var::from_name("y"));

        assert_eq!(
            ty_x,
            builder::integer_type(1),
            "Variable 'x' should be Integer(1)"
        );
        assert_eq!(
            ty_y,
            builder::character_type("hello"),
            "Variable 'y' should be Character('hello')"
        );
    }
}
