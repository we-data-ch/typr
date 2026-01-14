use nom::IResult;
use crate::components::context::config::Config;
use crate::language::Lang;
use crate::elements::parse_elements;
use nom::character::complete::multispace0;
use nom::sequence::terminated;
use nom::bytes::complete::tag;
use crate::elements::variable;
use crate::parsing::types::type_alias;
use crate::parsing::types::ltype;
use crate::r#type::r#type::Type;
use crate::lang::var::Var;
use nom::combinator::opt;
use nom::sequence::delimited;
use crate::elements::tag_exp;
use nom::character::complete::not_line_ending;
use nom::character::complete::line_ending;
use crate::elements::variable_exp;
use nom::branch::alt;
use nom::sequence::preceded;
use nom::multi::many0;
use nom::Parser;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;
use crate::elements::single_element;
use crate::elements::scope;
use crate::lang::operators::custom_op;
use crate::lang::operators::Op;
use crate::elements::return_exp;
use crate::elements::chars;
use crate::elements::vector;
use crate::elements::break_exp;
//use crate::elements::elements;
use crate::elements::variable2;
use crate::elements::Case;
use std::ops::Deref;
use crate::language::ModulePosition;
use crate::elements::variable_recognizer;

type Span<'a> = LocatedSpan<&'a str, String>;

fn pattern_var(s: Span) -> IResult<Span, (Vec<Lang>, Option<String>)> {
    let res = alt((tag_exp, variable2)).parse(s);
    match res {
        Ok((s, Lang::Tag(name, val, _h)))
            => {
                if let Lang::Variable(name2, perm, mutopa, typ, h) = *val {
                    Ok((s, 
                        (vec![Lang::Variable(name2.to_string(), perm, mutopa, typ, h.clone())],
                        Some(name.to_string()))))
                } else {
                    Ok((s, (vec![], Some(name.to_string()))))
                }
            } ,
        Ok((s, Lang::Variable(name, perm, mutopa, typ, h)))
            => Ok((s, (vec![Lang::Variable(name, perm, mutopa, typ, h.clone())], None))),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn single_parse(s: Span) -> IResult<Span, Lang> {
    let res = (
        parse_elements,
        terminated(tag(";"), multispace0) 
    ).parse(s);
    match res {
        Ok((s, (exp, _))) => Ok((s, exp)),
        Err(r) => Err(r)
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
          ).parse(s);
    match res {
        Ok((s, (_let, (pat_var, None), typ, _eq, Lang::Function(params, ty, body, h)))) 
            if params.len() > 0 => {
                let newvar = Var::from_language(pat_var[0].clone()).unwrap().set_type(params[0].1.clone());
                Ok((s, vec![Lang::Let(Box::new(newvar.to_language()),
                    typ.unwrap_or(Type::Empty(HelpData::default())),
                Box::new(Lang::Function(params, ty, body, h)), _let.into())]))
            },
        Ok((s, (_let, (pat_var, None), typ, _eq, body))) => {
                Ok((s, 
                    vec![
                    Lang::Let(
                        Box::new(pat_var[0].clone()),
                        typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                        Box::new(body), _let.into())]))
                }
        Ok((s, (_let, (pat_var, Some(_)), typ, eq, body))) => {
            if pat_var.len() == 1 {
                Ok((s, 
                    vec![
                    Lang::Let(
                        Box::new(pat_var[0].clone()),
                        typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                        Box::new(Lang::Operator(Op::Dollar(HelpData::default()), 
                                                Box::new(Lang::Number(0.0, eq.into())),
                        Box::new(body), pat_var.into())), _let.into())]))

            } else {
                Ok((s,
                pat_var.iter().map(|x| {
                    Lang::Let(Box::new(x.clone()), typ.clone().unwrap_or(Type::Empty(HelpData::default())), Box::new(body.clone()), HelpData::default())
                }).collect::<Vec<_>>()
                   ))
            }
        },
        Err(r) => Err(r),
    }
}

fn let_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        opt(terminated(tag("pub"), multispace0)),
        base_let_exp
                    ).parse(s);
    match res {
        Ok((s, (None, le))) => Ok((s, le)),
        Ok((s, (Some(_pu), le))) => {
            let new_le = le.iter().map(|x| {
                match x {
                    Lang::Let(var, typ, body, h) 
                        => {
                        let vari = Var::from_language(var.deref().clone())
                            .unwrap()
                            .set_permission(true)
                            .to_language();
                        Lang::Let(Box::new(vari),
                                    typ.clone(),
                                    body.clone(), h.clone())
                        }
                    lan => lan.clone()
                }
            }).collect();
            Ok((s, new_le))
        },
        Err(r) => Err(r),
    }
}

fn base_type_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("type"), multispace0),
            type_alias,
            equality_operator,
            ltype,
            terminated(tag(";"), multispace0) 
          ).parse(s);
    match res {
        Ok((s, (_ty, Type::Alias(name, params, _, h), _eq, ty, _))) 
            => {
                let h2 = if params.len() > 0 { params[0].clone().into()} else { HelpData::default() };
                let vari = Var::from_name(&name)
                    .set_type(Type::Params(params.clone(), h2))
                    .to_language();
                Ok((s, Lang::Alias(
                                Box::new(vari),
                                params, ty, h)))
            },
        Ok((s, (_ty, _, _eq, _ty2, _)))
            => {
                Ok((s, Lang::Empty(_ty.into())))
            },
        Err(r) => Err(r),
    }
}

fn type_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(terminated(tag("pub"), multispace0)),
                base_type_exp
                    ).parse(s);
    match res {
        Ok((s, (Some(_pu), ali))) => Ok((s, vec![ali])),
        Ok((s, (None, Lang::Alias(var, params, typ, h)))) 
            => {
                let vari = Var::from_language(var.deref().clone())
                    .unwrap().set_permission(false)
                    .to_language();
                Ok((s, vec![Lang::Alias(
                            Box::new(vari),
                            params,
                            typ, h)]))
            },
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn base_opaque_exp(s: Span) -> IResult<Span, Lang> {
    let res = (terminated(tag("opaque"), multispace0),
            type_alias,
            terminated(tag("="), multispace0),
            ltype,
            terminated(tag(";"), multispace0) 
          ).parse(s);
    match res {
        Ok((s, (_ty, Type::Alias(name, params, _, h), _eq, ty, _))) 
            => {
                let vari = Var::from_name(&name)
                        .set_type(Type::Params(params.clone(), params.clone().into()))
                        .set_opacity(true)
                        .to_language();
                Ok((s, Lang::Alias(
                            Box::new(vari),
                            params, ty, h)))
            }
        Ok((s, (_ty, _, _eq, _ty2, _)))
            => Ok((s, Lang::Empty(_ty.into()))),
        Err(r) => Err(r),
    }
}

fn opaque_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(terminated(tag("pub"), multispace0)),
                base_opaque_exp).parse(s);
    match res {
        Ok((s, (Some(_pu), Lang::Alias(var, params, typ, h)))) 
            => {
                let vari = Var::from_language(var.deref().clone())
                    .unwrap()
                    .set_opacity(true)
                    .to_language();
                Ok((s, vec![Lang::Alias(Box::new(vari), params, typ, h)]))
            }
        Ok((s, (None, Lang::Alias(var, params, typ, h)))) 
            => {
                let vari = Var::from_language(var.deref().clone())
                    .unwrap()
                    .set_permission(false)
                    .set_opacity(true)
                    .to_language();
                Ok((s, vec![Lang::Alias(
                                Box::new(vari),
                                params,
                                typ, h)]))
            }
        Err(r) => Err(r),
        _ => todo!()
    }
}

pub fn module(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("module"), multispace0),
        terminated(variable_exp, multispace0),
        terminated(tag("{"), multispace0),
        base_parse,
        terminated(tag("}"), multispace0),
        terminated(tag(";"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (modu, (name, _), _op, v, _cl, _dv))) => 
            Ok((s, vec![Lang::Module(name, v, ModulePosition::Internal, Config::default(), modu.into())])),
        Err(r) => Err(r),
    }
}


fn assign(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
            variable,
            alt((
                terminated(tag("="), multispace0),
                terminated(tag("<-"), multispace0))),
            parse_elements,
            terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, ((var, _), _eq, exp, _pv))) 
            => Ok((s, vec![Lang::Assign(Box::new(var.clone()), Box::new(exp), var.into())])),
        Err(r) => Err(r)
    } 
}

fn comment(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
            tag("#"),
            not_line_ending,
            opt(line_ending),
            multispace0).parse(s);
    match res {
        Ok((s, (_hashtag, txt, _, _))) 
            => Ok((s, vec![Lang::Comment(txt.to_string(), _hashtag.into())])),
        Err(r) => Err(r)
    }
}

pub fn simple_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        parse_elements,
        terminated(tag(";"), multispace0)
                    ).parse(s);
    match res {
        Ok((s, (lang, _sc))) => {
            Ok((s, vec![lang]))
        },
        Err(r) => Err(r)
    }
}

fn mod_imp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("mod"), multispace0),
            terminated(variable_exp, multispace0),
            terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (_mod, (name, _), _sc))) 
            => Ok((s, vec![Lang::ModuleImport(name.to_string(), _mod.into())])),
        Err(r) => Err(r)
    }
}

fn import_var(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("use"), multispace0),
                variable,
                terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (_use, (lang, case), _sc))) => {
            let res = match case {
                Case::Maj => Var::from_language(lang).unwrap().to_alias_lang(),
                _ => Var::from_language(lang).unwrap().to_let()
            };
            Ok((s, vec![res]))
        },
        Err(r) => Err(r),
    }
}

fn import_type(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("use"), multispace0),
            type_alias,
            terminated(tag(";"), multispace0)).parse(s);

    match res {
        Ok((s, (_use, alias, _sc))) => Ok((s, vec![Lang::Import(alias, _use.into())])),
        Err(r) => Err(r)
    }
}

fn tests(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("Test"),
                delimited(tag("["), base_parse, tag("]"))).parse(s);
    match res {
        Ok((s, (_t, body))) => Ok((s, vec![Lang::Test(body, _t.into())])),
        Err(r) => Err(r)
    }
}

fn library(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("library("), 
               variable_exp,
               tag(")"),
               opt(tag(";")),
               multispace0).parse(s);

    match res {
        Ok((s, (_lib, (var, h), _cl, Some(_col), _))) 
            => Ok((s, vec![Lang::Library(var, h.clone())])),
        Ok((_, (_lib, _var, _cl, None, _))) 
            => panic!("You forgot to put a ';' at the end of the line"),
        Err(r) => Err(r)
    }
}

fn use_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("use("),
                chars,
                tag(", "),
                alt((vector, chars)),
                terminated(tag(");"), multispace0)).parse(s);
    match res {
        Ok((s, (us, lib, _, members, _))) 
            => Ok((s, vec![Lang::Use(Box::new(lib), Box::new(members), us.into())])),
        Err(r) => Err(r)
    }
}

fn custom_operators(s: Span) -> IResult<Span, (String, HelpData)> {
    let res = custom_op.parse(s);
    match res {
        Ok((s, co)) => Ok((s, (co.clone().to_string(), co.into()))),
        Err(r) => Err(r)
    }
}

fn signature_variable(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("@"),
                alt((variable_recognizer, custom_operators)),
                terminated(tag(":"), multispace0),
                ltype, 
                terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (at, (name, h), _col, typ, _))) 
            => {
                let var2 = Var::from_name(&name).set_help_data(h).set_type(typ.clone()).set_permission(false);
                Ok((s, vec![Lang::Signature(var2, typ, at.into())]))
            },
        Err(r) => Err(r)
    }
}

fn signature_opaque(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("@"),
                type_alias,
                terminated(tag(":"), multispace0),
                ltype, 
                terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (at, Type::Alias(name, _params, _, h), _, typ, _))) 
            => {
                let var2 = Var::from_name(&name)
                    .set_help_data(h.clone())
                    .set_type(typ.clone());
                Ok((s, vec![Lang::Signature(var2, typ, at.into())]))
            },
        Ok((_s, (_, _, _, _, _))) => todo!(),
        Err(r) => Err(r)
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
            terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (_for, _op, (var_str, _h), _in, iterator, _cl, scop, _semi))) 
            => Ok((s, 
                   vec![Lang::ForLoop(Var::from_name(&var_str),
                       Box::new(iterator),
                       Box::new(scop),
                       _for.into())])),
        Err(r) => Err(r)
    }
}

fn while_loop(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
            terminated(tag("while"), multispace0),
            terminated(tag("("), multispace0),
            terminated(single_element, multispace0),
            terminated(tag(")"), multispace0),
            scope,
            terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (_while, _op, condition, _cl, scop, _semi))) 
            => Ok((s, 
                   vec![Lang::WhileLoop(Box::new(condition),
                       Box::new(scop),
                       _while.into())])),
        Err(r) => Err(r)
    }
}

fn test_block(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("Test"), multispace0),
     scope).parse(s);
     //parse_block).parse(s);

    match res {
        Ok((s, (tst, body))) 
            => Ok((s, vec![Lang::TestBlock(Box::new(body), tst.into())])),
        Err(r) => Err(r)
    }
}


// main
pub fn base_parse(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(multispace0),
        many0(alt((library, break_exp, use_exp, test_block, while_loop, for_loop, signature, tests, import_type, import_var, mod_imp, comment, type_exp, opaque_exp, let_exp, module, assign, simple_exp))),
        opt(alt((return_exp, parse_elements)))).parse(s);
    match res {
        Ok((s, (_, v, Some(exp)))) => {
            let mut new_v = v.iter().flatten().cloned().collect::<Vec<_>>();
            new_v.push(exp);
            Ok((s, new_v))
        },
        Ok((s, (_, v, None))) => Ok((s, v.iter().flatten().cloned().collect())),
        Err(r) => Err(r)
    }
}

pub fn parse(s: Span) -> Lang {
    let res = base_parse(s.clone());
    match res {
        Ok((_, v)) => Lang::Lines(v.clone(), v.into()),
        Err(_) => panic!("Can't parse string {}", s)
    }
}

//pub fn parse2(s: Span) -> Result<Lang, String> {
    //let res = library(s.clone());
    //match res {
        //Ok((_, v)) => Ok(v[0].clone()),
        //Err(_) => Err(format!("Can't parse string {}", s))
    //}
//}

pub fn parse2(s: Span) -> Result<Lang, String> {
    let res = base_parse(s.clone());
    match res {
        Ok((_, v)) => Ok(v[0].clone()),
        Err(_) => Err(format!("Can't parse string {}", s))
    }
}

// main test
#[cfg(test)]
mod tesus {
    use super::*;

    #[test]
    fn test_type_exp2() {
        let res = type_exp("type Mat<M, N, T> = [M, [N, T]];".into()).unwrap().0;
        assert_eq!(res, "alias(var('Mat'), [M, N, T], [M, [N, T]])".into());
    }

    #[test]
    fn test_assign1() {
        let res = assign("a <- 12;".into()).unwrap().1;
        assert_eq!("Assign", res[0].simple_print(),
        "The expression 'a <- 12;' should be identified as an assignation");
    }

    #[test]
    fn test_assign2() {
        let res = assign("a.b() <- 12;".into()).unwrap().1;
        assert_eq!("Assign", res[0].simple_print(),
        "The expression 'a.b() <- 12;' should be identified as an assignation");
    }

    #[test]
    fn test_assign3() {
        let res = assign("a$b <- 12;".into()).unwrap().1;
        assert_eq!("Assign", res[0].simple_print(),
        "The expression 'a$b <- 12;' should be identified as an assignation");
    } 

}
