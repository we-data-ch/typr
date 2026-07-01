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
use crate::components::r#type::vector_type::ConstructorCategory;
use crate::components::r#type::Type;
use crate::processes::parsing::elements::break_exp;
use crate::processes::parsing::elements::chars;
use crate::processes::parsing::elements::next_exp;
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
use crate::processes::parsing::types::pascal_case_no_space;
use crate::processes::parsing::types::type_alias;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while1;
use nom::character::complete::line_ending;
use nom::character::complete::multispace0;
use nom::character::complete::multispace1;
use nom::character::complete::not_line_ending;
use nom::combinator::map;
use nom::combinator::opt;
use nom::multi::many0;
use nom::multi::many1;
use nom::multi::separated_list0;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::IResult;
use nom::Parser;
use nom_locate::LocatedSpan;
use std::ops::Deref;

type Span<'a> = LocatedSpan<&'a str, String>;

use std::cell::RefCell;

thread_local! {
    static PARSE_ERRORS: RefCell<Vec<SyntaxError>> = const { RefCell::new(Vec::new()) };
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
        Ok((
            s,
            Lang::Tag {
                name, value: val, ..
            },
        )) => {
            if let Lang::Variable {
                name: name2,
                is_opaque: mutopa,
                related_type: typ,
                help_data: h,
            } = *val
            {
                Ok((
                    s,
                    (
                        vec![Lang::Variable {
                            name: name2.to_string(),
                            is_opaque: mutopa,
                            related_type: typ,
                            help_data: h.clone(),
                        }],
                        Some(name.to_string()),
                    ),
                ))
            } else {
                Ok((s, (vec![], Some(name.to_string()))))
            }
        }
        Ok((
            s,
            Lang::Variable {
                name,
                is_opaque: mutopa,
                related_type: typ,
                help_data: h,
            },
        )) => Ok((
            s,
            (
                vec![Lang::Variable {
                    name,
                    is_opaque: mutopa,
                    related_type: typ,
                    help_data: h.clone(),
                }],
                None,
            ),
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
            if let Lang::Variable {
                name, help_data, ..
            } = &pat_var[0]
            {
                if name.chars().next().is_some_and(|c| c.is_uppercase()) {
                    push_parse_error(SyntaxError::LetInsteadOfType {
                        name: name.clone(),
                        help_data: help_data.clone(),
                    });
                }
            }
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
                    is_public: false,
                    is_testable: false,
                    is_export: false,
                    help_data: _let.into(),
                }],
            ))
        }
        Ok((s, (_let, (pat_var, None), typ, _eq, body))) => {
            if let Lang::Variable {
                name, help_data, ..
            } = &pat_var[0]
            {
                if name.chars().next().is_some_and(|c| c.is_uppercase()) {
                    push_parse_error(SyntaxError::LetInsteadOfType {
                        name: name.clone(),
                        help_data: help_data.clone(),
                    });
                }
            }
            Ok((
                s,
                vec![Lang::Let {
                    variable: Box::new(pat_var[0].clone()),
                    r#type: typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                    expression: Box::new(body),
                    is_public: false,
                    is_testable: false,
                    is_export: false,
                    help_data: _let.into(),
                }],
            ))
        }
        Ok((s, (_let, (pat_var, Some(_)), typ, eq, body))) => {
            if pat_var.len() == 1 {
                Ok((
                    s,
                    vec![Lang::Let {
                        variable: Box::new(pat_var[0].clone()),
                        r#type: typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                        expression: Box::new(Lang::Operator {
                            operator: Op::Dollar(HelpData::default()),
                            rhs: Box::new(Lang::Number {
                                value: 0.0,
                                help_data: eq.into(),
                            }),
                            lhs: Box::new(body),
                            help_data: pat_var.into(),
                        }),
                        is_public: false,
                        is_testable: false,
                        is_export: false,
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
                            is_public: false,
                            is_testable: false,
                            is_export: false,
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
        Ok((
            s,
            (
                _let,
                Lang::Tuple {
                    value: elements,
                    help_data: _th,
                },
                typ,
                _eq,
                body,
            ),
        )) => {
            let tmp_name = "__tuple_tmp__";
            let tmp_var = Var::from_name(tmp_name).to_language();

            // First: let __tuple_tmp__ <- body;
            let tmp_let = Lang::Let {
                variable: Box::new(tmp_var.clone()),
                r#type: typ.unwrap_or(Type::Empty(HelpData::default())),
                expression: Box::new(body),
                is_public: false,
                is_testable: false,
                is_export: false,
                help_data: _let.into(),
            };

            // Then: let a <- 1.__tuple_tmp__; let b <- 2.__tuple_tmp__; ...
            let mut result = vec![tmp_let];
            for (i, elem) in elements.iter().enumerate() {
                if let Lang::Variable { name, .. } = elem {
                    if name == "_" {
                        continue; // skip wildcard
                    }
                }
                result.push(Lang::Let {
                    variable: Box::new(elem.clone()),
                    r#type: Type::Empty(HelpData::default()),
                    expression: Box::new(Lang::Operator {
                        operator: Op::Dot(HelpData::default()),
                        rhs: Box::new(Lang::Integer {
                            value: (i + 1) as i32,
                            help_data: HelpData::default(),
                        }),
                        lhs: Box::new(tmp_var.clone()),
                        help_data: HelpData::default(),
                    }),
                    is_public: false,
                    is_testable: false,
                    is_export: false,
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
    let res = (
        opt(terminated(
            alt((tag("@export"), tag("@pub"), tag("@testable"))),
            multispace0,
        )),
        base_let_exp,
    )
        .parse(s);
    match res {
        Ok((s, (None, le))) => Ok((s, le)),
        Ok((s, (Some(annotation), le))) => {
            // `@export` → public + testable + exported in R package (RFC-TR-032).
            // `@pub`    → public + testable (RFC-TR-032, §3.2).
            // `@testable` → private but exposed as `M$.test_<name>` in test builds.
            let frag = *annotation.fragment();
            let is_pub = frag == "@pub" || frag == "@export";
            let is_test = frag == "@testable" || frag == "@pub" || frag == "@export";
            let is_exp = frag == "@export";
            let new_le = le
                .iter()
                .map(|x| match x {
                    Lang::Let {
                        variable: var,
                        r#type: typ,
                        expression: body,
                        is_public: _,
                        is_testable: _,
                        is_export: _,
                        help_data: h,
                    } => {
                        let vari = Var::from_language(var.deref().clone())
                            .unwrap()
                            .to_language();
                        Lang::Let {
                            variable: Box::new(vari),
                            r#type: typ.clone(),
                            expression: body.clone(),
                            is_public: is_pub,
                            is_testable: is_test,
                            is_export: is_exp,
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

/// Parses a `typeconstructor` declaration that registers a new type constructor:
///   `typeconstructor Tibble[N] record;`
///   `typeconstructor Matrix[N, M, T] recursive;`
fn typeconstructor_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("typeconstructor"), multispace0),
        pascal_case_no_space,
        delimited(
            terminated(tag("["), multispace0),
            separated_list0(
                terminated(tag(","), multispace0),
                terminated(ltype, multispace0),
            ),
            terminated(tag("]"), multispace0),
        ),
        terminated(alt((tag("recursive"), tag("record"))), multispace0),
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_kw, (name, h), params, category, _semi))) => {
            let category = match *category.fragment() {
                "recursive" => ConstructorCategory::Recursive,
                _ => ConstructorCategory::Record,
            };
            Ok((
                s,
                vec![Lang::TypeConstructor {
                    name,
                    parameters: params,
                    category,
                    help_data: h,
                }],
            ))
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
            Ok((
                s,
                Lang::Alias {
                    identifier: Box::new(vari),
                    parameters: params,
                    target_type: ty,
                    is_public: false,
                    help_data: h,
                },
            ))
        }
        Ok((s, (_ty, _, _eq, _ty2, _))) => Ok((s, Lang::Empty(_ty.into()))),
        Err(r) => Err(r),
    }
}

fn type_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(terminated(tag("@pub"), multispace0)), base_type_exp).parse(s);
    match res {
        Ok((
            s,
            (
                Some(_pu),
                Lang::Alias {
                    identifier: var,
                    parameters: params,
                    target_type: typ,
                    help_data: h,
                    ..
                },
            ),
        )) => Ok((
            s,
            vec![Lang::Alias {
                identifier: var,
                parameters: params,
                target_type: typ,
                is_public: true,
                help_data: h,
            }],
        )),
        Ok((
            s,
            (
                None,
                Lang::Alias {
                    identifier: var,
                    parameters: params,
                    target_type: typ,
                    help_data: h,
                    ..
                },
            ),
        )) => {
            let vari = Var::from_language(var.deref().clone())
                .unwrap()
                .to_language();
            Ok((
                s,
                vec![Lang::Alias {
                    identifier: Box::new(vari),
                    parameters: params,
                    target_type: typ,
                    is_public: false,
                    help_data: h,
                }],
            ))
        }
        Err(r) => Err(r),
        _ => todo!(),
    }
}

/// Detects `type <lowercase_var> <- <expr>` which should be `let` instead.
/// Parses the full expression and returns a `Lang::Let` with a push_parse_error.
fn type_instead_of_let_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        opt(terminated(tag("@pub"), multispace0)),
        terminated(tag("type"), multispace0),
        terminated(variable_exp, multispace0),
        equality_operator,
        single_parse,
    )
        .parse(s);
    match res {
        Ok((s, (pub_ann, type_kw, (name, h), _eq, body))) => {
            push_parse_error(SyntaxError::TypeInsteadOfLet {
                name: name.clone(),
                help_data: h.clone(),
            });
            let is_pub = pub_ann.is_some();
            Ok((
                s,
                vec![Lang::Let {
                    variable: Box::new(Lang::Variable {
                        name,
                        is_opaque: false,
                        related_type: Type::Empty(HelpData::default()),
                        help_data: h,
                    }),
                    r#type: Type::Empty(type_kw.clone().into()),
                    expression: Box::new(body),
                    is_public: is_pub,
                    is_testable: is_pub,
                    is_export: false,
                    help_data: type_kw.into(),
                }],
            ))
        }
        Err(r) => Err(r),
    }
}

fn base_opaque_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
        terminated(tag("opaque"), multispace0),
        type_alias,
        equality_operator,
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
            Ok((
                s,
                Lang::Alias {
                    identifier: Box::new(vari),
                    parameters: params,
                    target_type: ty,
                    is_public: false,
                    help_data: h,
                },
            ))
        }
        Ok((s, (_ty, _, _eq, _ty2, _))) => Ok((s, Lang::Empty(_ty.into()))),
        Err(r) => Err(r),
    }
}

fn opaque_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(terminated(tag("@pub"), multispace0)), base_opaque_exp).parse(s);
    match res {
        Ok((
            s,
            (
                Some(_pu),
                Lang::Alias {
                    identifier: var,
                    parameters: params,
                    target_type: typ,
                    help_data: h,
                    ..
                },
            ),
        )) => {
            let vari = Var::from_language(var.deref().clone())
                .unwrap()
                .set_opacity(true)
                .to_language();
            Ok((
                s,
                vec![Lang::Alias {
                    identifier: Box::new(vari),
                    parameters: params,
                    target_type: typ,
                    is_public: true,
                    help_data: h,
                }],
            ))
        }
        Ok((
            s,
            (
                None,
                Lang::Alias {
                    identifier: var,
                    parameters: params,
                    target_type: typ,
                    help_data: h,
                    ..
                },
            ),
        )) => {
            let vari = Var::from_language(var.deref().clone())
                .unwrap()
                .set_opacity(true)
                .to_language();
            Ok((
                s,
                vec![Lang::Alias {
                    identifier: Box::new(vari),
                    parameters: params,
                    target_type: typ,
                    is_public: false,
                    help_data: h,
                }],
            ))
        }
        Err(r) => Err(r),
        _ => todo!(),
    }
}

pub fn module(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("module"), multispace0),
        terminated(variable_recognizer, multispace0),
        terminated(tag("{"), multispace0),
        base_parse,
        terminated(tag("}"), multispace0),
        opt(terminated(tag(";"), multispace0)),
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

fn import_module(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("import"), multispace0),
        terminated(variable_recognizer, multispace0),
        opt((
            terminated(tag("as"), multispace0),
            terminated(variable_recognizer, multispace0),
        )),
        opt(terminated(tag(";"), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (import_kw, (name, _), None, _))) => Ok((
            s,
            vec![Lang::ModuleImport {
                value: name,
                help_data: import_kw.into(),
            }],
        )),
        Ok((s, (_, (name, h), Some((_, (alias, _))), _))) => {
            let module_var = Lang::Variable {
                name,
                is_opaque: false,
                related_type: Type::Empty(HelpData::default()),
                help_data: h.clone(),
            };
            let alias_var = Var::from_name(&alias).to_language();
            Ok((
                s,
                vec![Lang::Let {
                    variable: Box::new(alias_var),
                    r#type: Type::Empty(HelpData::default()),
                    expression: Box::new(module_var),
                    is_public: false,
                    is_testable: false,
                    is_export: false,
                    help_data: h,
                }],
            ))
        }
        Err(r) => Err(r),
    }
}

fn import_from_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("@importFrom"), multispace1),
        terminated(
            take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '_'),
            multispace1,
        ),
        many1(terminated(
            take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '_'),
            multispace0,
        )),
        opt(terminated(tag(";"), multispace0)),
    )
        .parse(s);
    match res {
        Ok((s, (kw, pkg, fns, _))) => Ok((
            s,
            vec![Lang::ImportFrom {
                package: pkg.fragment().to_string(),
                functions: fns.iter().map(|f| f.fragment().to_string()).collect(),
                help_data: kw.into(),
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
            vec![Lang::Assign {
                identifier: Box::new(var.clone()),
                expression: Box::new(exp),
                help_data: var.into(),
            }],
        )),
        Ok((s, ((var, _), _eq, exp, None))) => {
            push_parse_error(SyntaxError::ForgottenSemicolon(exp.clone().into()));
            let assign = Lang::Assign {
                identifier: Box::new(var.clone()),
                expression: Box::new(exp),
                help_data: var.into(),
            };
            Ok((s, vec![assign]))
        }
        Err(r) => Err(r),
    }
}

fn comment(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("#"), not_line_ending, opt(line_ending), multispace0).parse(s);
    match res {
        Ok((s, (_hashtag, txt, _, _))) => Ok((
            s,
            vec![Lang::Comment {
                value: txt.to_string(),
                help_data: _hashtag.into(),
            }],
        )),
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
        Ok((s, (_mod, (name, _), _sc))) => Ok((
            s,
            vec![Lang::ModuleImport {
                value: name.to_string(),
                help_data: _mod.into(),
            }],
        )),
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
        Ok((s, (_use, alias, _sc))) => Ok((
            s,
            vec![Lang::Import {
                value: alias,
                help_data: _use.into(),
            }],
        )),
        Err(r) => Err(r),
    }
}

fn tests(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("Test"), delimited(tag("["), base_parse, tag("]"))).parse(s);
    match res {
        Ok((s, (_t, body))) => Ok((
            s,
            vec![Lang::Test {
                value: body,
                help_data: _t.into(),
            }],
        )),
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
        Ok((s, (_lib, (var, h), _cl, Some(_col), _))) => Ok((
            s,
            vec![Lang::Library {
                value: var,
                help_data: h.clone(),
            }],
        )),
        Ok((_, (_lib, _var, _cl, None, _))) => {
            panic!("You forgot to put a ';' at the end of the line")
        }
        Err(r) => Err(r),
    }
}

fn use_item_exp(s: Span) -> IResult<Span, crate::components::language::use_lang::UseItem> {
    use crate::components::language::use_lang::UseItem;
    // Parse: Ident (as Ident)?
    let res = (
        terminated(variable_recognizer, multispace0),
        opt(preceded(
            pair(tag("as"), multispace0),
            terminated(variable_recognizer, multispace0),
        )),
    )
        .parse(s);
    match res {
        Ok((s, ((name, _), alias_opt))) => {
            let alias = alias_opt.map(|(alias_name, _)| alias_name);
            Ok((s, UseItem { name, alias }))
        }
        Err(r) => Err(r),
    }
}

fn use_items_selector(
    s: Span,
) -> IResult<Span, crate::components::language::use_lang::UseSelector> {
    use crate::components::language::use_lang::UseSelector;
    let res = (
        pair(tag("{"), multispace0),
        separated_list0(pair(tag(","), multispace0), use_item_exp),
        opt(pair(tag(","), multispace0)),
        pair(tag("}"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_, items, _, _))) => Ok((s, UseSelector::Items(items))),
        Err(r) => Err(r),
    }
}

fn use_selector_exp(s: Span) -> IResult<Span, crate::components::language::use_lang::UseSelector> {
    use crate::components::language::use_lang::UseSelector;
    alt((
        map(terminated(tag("*"), multispace0), |_| UseSelector::Wildcard),
        use_items_selector,
    ))
    .parse(s)
}

fn use_module_directive(s: Span) -> IResult<Span, Vec<Lang>> {
    use crate::components::language::use_lang::UseSelector;

    // Parse: use FirstSeg :: ...
    let res = (
        terminated(tag("use"), multispace0),
        terminated(variable_recognizer, multispace0),
        pair(tag("::"), multispace0),
    )
        .parse(s);

    let (s, (use_kw, (first_seg, _), _)) = res?;

    let mut path = vec![first_seg];
    let mut current_s = s;
    let selector;

    loop {
        match use_selector_exp(current_s.clone()) {
            Ok((s2, sel)) => {
                selector = sel;
                current_s = s2;
                break;
            }
            Err(_) => match terminated(variable_recognizer, multispace0).parse(current_s.clone()) {
                Ok((s2, (seg, _))) => {
                    let colon_res: IResult<Span, (Span, Span)> =
                        pair(tag("::"), multispace0).parse(s2.clone());
                    match colon_res {
                        Ok((s3, _)) => {
                            path.push(seg);
                            current_s = s3;
                        }
                        Err(_) => {
                            use crate::components::language::use_lang::UseItem;
                            selector = UseSelector::Items(vec![UseItem {
                                name: seg,
                                alias: None,
                            }]);
                            current_s = s2;
                            break;
                        }
                    }
                }
                Err(e) => return Err(e),
            },
        }
    }

    let res = terminated(tag(";"), multispace0).parse(current_s);
    let (s, _) = res?;

    Ok((
        s,
        vec![Lang::UseModule {
            module_path: path,
            selector,
            help_data: use_kw.into(),
        }],
    ))
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
            vec![Lang::Use {
                lang: Box::new(lib),
                members: Box::new(members),
                help_data: us.into(),
            }],
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

fn head_lang(lang: &Lang) -> Option<Lang> {
    match lang {
        Lang::Variable { .. } => Some(lang.clone()),
        Lang::Operator {
            operator: Op::Pipe(_) | Op::Dot(_),
            rhs,
            ..
        } => head_lang(rhs),
        _ => None,
    }
}

fn implicit_mutate(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (parse_elements, terminated(tag("!;"), multispace0)).parse(s);
    match res {
        Ok((s, (expr, excl))) => match head_lang(&expr) {
            Some(lhs) => Ok((
                s,
                vec![Lang::Assign {
                    identifier: Box::new(lhs),
                    expression: Box::new(expr),
                    help_data: excl.into(),
                }],
            )),
            None => {
                push_parse_error(SyntaxError::MutationTargetNotAssignable(
                    expr.clone().into(),
                ));
                Err(nom::Err::Error(nom::error::Error::new(
                    s,
                    nom::error::ErrorKind::Tag,
                )))
            }
        },
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
            Ok((
                s,
                vec![Lang::Signature {
                    identifier: var2,
                    target_type: typ,
                    help_data: at.into(),
                    is_extern: false,
                    extern_r_name: None,
                }],
            ))
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
            Ok((
                s,
                vec![Lang::Signature {
                    identifier: var2,
                    target_type: typ,
                    help_data: at.into(),
                    is_extern: false,
                    extern_r_name: None,
                }],
            ))
        }
        Ok((_s, (_, _, _, _, _))) => todo!(),
        Err(r) => Err(r),
    }
}

fn signature_extern(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        tag("@extern"),
        multispace1,
        opt((
            take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '.'),
            tag("::"),
        )),
        alt((variable_recognizer, custom_operators)),
        terminated(tag(":"), multispace0),
        ltype,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (at, _, pkg_prefix, (name, h), _col, typ, _))) => {
            let r_name = pkg_prefix.map(|(pkg, _)| format!("{}::{}", pkg, name));
            let var2 = Var::from_name(&name).set_help_data(h).set_type(typ.clone());
            Ok((
                s,
                vec![Lang::Signature {
                    identifier: var2,
                    target_type: typ,
                    help_data: at.into(),
                    is_extern: true,
                    extern_r_name: r_name,
                }],
            ))
        }
        Err(r) => Err(r),
    }
}

pub fn signature(s: Span) -> IResult<Span, Vec<Lang>> {
    alt((signature_extern, signature_opaque, signature_variable)).parse(s)
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
            vec![Lang::ForLoop {
                identifier: Var::from_name(&var_str),
                expression: Box::new(iterator),
                body: Box::new(scop),
                help_data: _for.into(),
            }],
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
            vec![Lang::WhileLoop {
                condition: Box::new(condition),
                body: Box::new(scop),
                help_data: _while.into(),
            }],
        )),
        Err(r) => Err(r),
    }
}

fn loop_loop(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        terminated(tag("loop"), multispace0),
        scope,
        terminated(tag(";"), multispace0),
    )
        .parse(s);
    match res {
        Ok((s, (_loop, scop, _semi))) => Ok((
            s,
            vec![Lang::Loop {
                body: Box::new(scop),
                help_data: _loop.into(),
            }],
        )),
        Err(r) => Err(r),
    }
}

fn test_block(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("Test"), multispace0), scope).parse(s);
    //parse_block).parse(s);

    match res {
        Ok((s, (tst, body))) => Ok((
            s,
            vec![Lang::TestBlock {
                value: Box::new(body),
                help_data: tst.into(),
            }],
        )),
        Err(r) => Err(r),
    }
}

// main
pub fn base_parse(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        opt(multispace0),
        many0(alt((
            alt((
                library,
                break_exp,
                next_exp,
                use_exp,
                test_block,
                while_loop,
                loop_loop,
                for_loop,
                import_from_exp,
                signature,
                tests,
                import_module,
                use_module_directive,
                import_type,
            )),
            alt((
                import_var,
                mod_imp,
                comment,
                typeconstructor_exp,
                type_exp,
                type_instead_of_let_exp,
                opaque_exp,
                let_tuple_exp,
                let_exp,
                module,
                assign,
                return_stmt,
                implicit_mutate,
                stmt_exp,
            )),
        ))),
        opt(parse_elements),
    )
        .parse(s);
    match res {
        // Case 1: nothing → empty vec (truly empty, distinct from `...`)
        Ok((s, (_, v, None))) if v.is_empty() => Ok((s, vec![])),
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
        Ok((remaining, v)) => {
            if !remaining.fragment().is_empty() {
                let element = remaining
                    .fragment()
                    .lines()
                    .next()
                    .unwrap_or("")
                    .trim()
                    .to_string();
                let line = remaining.location_line();
                let help_data = HelpData::from(remaining);
                push_parse_error(SyntaxError::UnknownElement {
                    element,
                    line,
                    help_data,
                });
            }
            ParseResult::new(Lang::Lines {
                value: v.clone(),
                help_data: v.into(),
            })
        }
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
    // Drain any parse errors collected during parsing (e.g. ForgottenSemicolon, FunctionTypeSyntax)
    let _ = take_parse_errors();
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

    // ==================== Let vs Type Alias Tests ====================

    #[test]
    fn test_let_instead_of_type() {
        let res = parse("let MyType <- int;".into());
        assert!(
            res.has_errors(),
            "let with PascalCase should produce a syntax error"
        );
        assert_eq!(res.errors.len(), 1, "Should have exactly one error");
        match &res.errors[0] {
            SyntaxError::LetInsteadOfType { name, .. } => {
                assert_eq!(name, "MyType", "Error should reference the type name");
            }
            _ => panic!("Expected LetInsteadOfType error"),
        }
    }

    #[test]
    fn test_let_instead_of_type_with_pub() {
        let res = parse("@pub let MyAlias <- num;".into());
        assert!(
            res.has_errors(),
            "@pub let with PascalCase should produce a syntax error"
        );
        assert!(res
            .errors
            .iter()
            .any(|e| matches!(e, SyntaxError::LetInsteadOfType { .. })));
    }

    #[test]
    fn test_let_with_lowercase_is_fine() {
        let res = parse("let my_var <- 42;".into());
        assert!(
            !res.has_errors(),
            "let with snake_case should not produce a syntax error"
        );
    }

    #[test]
    fn test_type_instead_of_let() {
        let res = parse("type my_var <- 42;".into());
        assert!(
            res.has_errors(),
            "type with snake_case should produce a syntax error"
        );
        assert_eq!(res.errors.len(), 1, "Should have exactly one error");
        match &res.errors[0] {
            SyntaxError::TypeInsteadOfLet { name, .. } => {
                assert_eq!(name, "my_var", "Error should reference the variable name");
            }
            _ => panic!("Expected TypeInsteadOfLet error"),
        }
    }

    #[test]
    fn test_type_instead_of_let_with_pub() {
        let res = parse("@pub type my_binding <- num;".into());
        assert!(
            res.has_errors(),
            "@pub type with snake_case should produce a syntax error"
        );
        assert!(res
            .errors
            .iter()
            .any(|e| matches!(e, SyntaxError::TypeInsteadOfLet { .. })));
    }

    #[test]
    fn test_type_with_pascalcase_is_fine() {
        let res = parse("type MyAlias <- int;".into());
        assert!(
            !res.has_errors(),
            "type with PascalCase should not produce a syntax error"
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
            if let Lang::Variable { name, .. } = var.as_ref() {
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
            if let Lang::Variable { name, .. } = var.as_ref() {
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
            if let Lang::Variable { name, .. } = var.as_ref() {
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
            if let Lang::Variable { name, .. } = var.as_ref() {
                assert_eq!(name, "a");
            } else {
                panic!("Expected Variable 'a'");
            }
        } else {
            panic!("Expected Let");
        }
        // Third Let should bind 'c'
        if let Lang::Let { variable: var, .. } = &res[2] {
            if let Lang::Variable { name, .. } = var.as_ref() {
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

    #[test]
    fn test_fn_function_type_syntax_error() {
        let res = parse("let f: fn(a: int) -> int <- fn(x: int): int { x };".into());
        assert!(
            res.has_errors(),
            "Using 'fn(...)' in type position should produce a syntax error"
        );
        let fn_type_error = res
            .errors
            .iter()
            .any(|e| matches!(e, SyntaxError::FunctionTypeSyntax(_)));
        assert!(
            fn_type_error,
            "Expected FunctionTypeSyntax error, got: {:?}",
            res.errors
        );
    }

    #[test]
    fn test_use_wildcard_parses() {
        use crate::components::language::use_lang::UseSelector;
        let lang = parse2("use Math::*;".into()).expect("parse failed");
        match lang {
            Lang::UseModule {
                module_path,
                selector,
                ..
            } => {
                assert_eq!(module_path, vec!["Math".to_string()]);
                assert_eq!(selector, UseSelector::Wildcard);
            }
            other => panic!("Expected UseModule, got {:?}", other),
        }
    }

    #[test]
    fn test_use_items_parses() {
        use crate::components::language::use_lang::{UseItem, UseSelector};
        let lang = parse2("use Math::{pi, sin as s};".into()).expect("parse failed");
        match lang {
            Lang::UseModule {
                module_path,
                selector,
                ..
            } => {
                assert_eq!(module_path, vec!["Math".to_string()]);
                assert_eq!(
                    selector,
                    UseSelector::Items(vec![
                        UseItem {
                            name: "pi".to_string(),
                            alias: None
                        },
                        UseItem {
                            name: "sin".to_string(),
                            alias: Some("s".to_string())
                        },
                    ])
                );
            }
            other => panic!("Expected UseModule, got {:?}", other),
        }
    }

    #[test]
    fn test_use_nested_path_parses() {
        use crate::components::language::use_lang::UseSelector;
        let lang = parse2("use Aa::Bb::Cc::*;".into()).expect("parse failed");
        match lang {
            Lang::UseModule {
                module_path,
                selector,
                ..
            } => {
                assert_eq!(
                    module_path,
                    vec!["Aa".to_string(), "Bb".to_string(), "Cc".to_string()]
                );
                assert_eq!(selector, UseSelector::Wildcard);
            }
            other => panic!("Expected UseModule, got {:?}", other),
        }
    }

    #[test]
    fn test_implicit_mutate_simple_variable() {
        let res = implicit_mutate("x!;".into()).unwrap().1;
        assert_eq!(res.len(), 1);
        match &res[0] {
            Lang::Assign {
                identifier,
                expression,
                ..
            } => {
                assert!(matches!(identifier.as_ref(), Lang::Variable { .. }));
                assert!(matches!(expression.as_ref(), Lang::Variable { .. }));
            }
            other => panic!("Expected Assign, got {:?}", other),
        }
    }

    #[test]
    fn test_implicit_mutate_pipeline() {
        let res = implicit_mutate("x |> f()!;".into()).unwrap().1;
        assert_eq!(res.len(), 1);
        match &res[0] {
            Lang::Assign {
                identifier,
                expression,
                ..
            } => {
                assert!(matches!(identifier.as_ref(), Lang::Variable { .. }));
                assert!(matches!(expression.as_ref(), Lang::Operator { .. }));
            }
            other => panic!("Expected Assign, got {:?}", other),
        }
    }

    #[test]
    fn test_implicit_mutate_ufc() {
        let res = implicit_mutate("obj.method()!;".into()).unwrap().1;
        assert_eq!(res.len(), 1);
        match &res[0] {
            Lang::Assign { identifier, .. } => match identifier.as_ref() {
                Lang::Variable { name, .. } => assert_eq!(name, "obj"),
                other => panic!("Expected Variable identifier, got {:?}", other),
            },
            other => panic!("Expected Assign, got {:?}", other),
        }
    }

    #[test]
    fn test_implicit_mutate_pipeline_chained() {
        let res = implicit_mutate("x |> f() |> g()!;".into()).unwrap().1;
        assert_eq!(res.len(), 1);
        match &res[0] {
            Lang::Assign { identifier, .. } => match identifier.as_ref() {
                Lang::Variable { name, .. } => assert_eq!(name, "x"),
                other => panic!("Expected Variable identifier, got {:?}", other),
            },
            other => panic!("Expected Assign, got {:?}", other),
        }
    }

    #[test]
    fn test_implicit_mutate_ufc_pipeline() {
        let res = implicit_mutate("shape.scale(2) |> rotate(90)!;".into())
            .unwrap()
            .1;
        assert_eq!(res.len(), 1);
        match &res[0] {
            Lang::Assign { identifier, .. } => match identifier.as_ref() {
                Lang::Variable { name, .. } => assert_eq!(name, "shape"),
                other => panic!("Expected Variable identifier, got {:?}", other),
            },
            other => panic!("Expected Assign, got {:?}", other),
        }
    }

    #[test]
    fn test_implicit_mutate_invalid_literal_fails() {
        // 3!; — literal, not assignable: parser should fail (return Err)
        let res = implicit_mutate("3!;".into());
        assert!(res.is_err(), "Literal mutation should fail to parse");
    }
}
