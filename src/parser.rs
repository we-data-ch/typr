use nom::IResult;
use crate::language::Lang;
use crate::elements::parse_elements;
use nom::character::complete::multispace0;
use nom::sequence::terminated;
use nom::bytes::complete::tag;
use crate::elements::variable;
use crate::types::type_alias;
use crate::types::ltype;
use crate::r#type::Type;
use crate::var::Var;
use nom::combinator::opt;
use nom::sequence::delimited;
use crate::elements::tag_exp;
use nom::character::complete::not_line_ending;
use nom::character::complete::line_ending;
use crate::elements::bang_exp;
use crate::adt::Adt;
use crate::elements::variable_exp;
use nom::branch::alt;
use nom::sequence::preceded;
use nom::multi::many0;
use nom::Parser;
use nom_locate::LocatedSpan;
use crate::help_data::HelpData;
use crate::builder;

type Span<'a> = LocatedSpan<&'a str, String>;

fn pattern_var(s: Span) -> IResult<Span, (Vec<Lang>, Option<String>)> {
    let res = alt((tag_exp, variable)).parse(s);
    match res {
        Ok((s, Lang::Tag(name, val, _h)))
            => {
                if let Lang::Variable(name2, path, perm, mutopa, typ, h) = *val {
                    Ok((s, 
                        (vec![Lang::Variable(name2.to_string(), path, perm, mutopa, typ, h.clone())],
                        Some(name.to_string()))))
                } else {
                    Ok((s, (vec![], Some(name.to_string()))))
                }
            } ,
        Ok((s, Lang::Variable(name, path, perm, mutopa, typ, h)))
            => Ok((s, (vec![Lang::Variable(name, path, perm, mutopa, typ, h.clone())], None))),
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
        Ok((s, (_let, (pat_var, None), typ, _eq, Lang::Function(ki, params, ty, body, h)))) 
            if params.len() > 0 => {
                let newvar = Var::from_language(pat_var[0].clone()).unwrap().set_type(params[0].1.clone()).set_permission(false);
                Ok((s, vec![Lang::Let(newvar, typ.unwrap_or(Type::Empty(HelpData::default())),
                Box::new(Lang::Function(ki, params, ty, body, h)), _let.into())]))
            },
        Ok((s, (_let, (pat_var, None), typ, _eq, body))) => {
                Ok((s, 
                    vec![
                    Lang::Let(
                        Var::from_language(pat_var[0].clone()).unwrap().set_permission(false),
                        typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                        Box::new(body), _let.into())]))
                }
        Ok((s, (_let, (pat_var, Some(_)), typ, eq, body))) => {
            if pat_var.len() == 1 {
                Ok((s, 
                    vec![
                    Lang::Let(
                        Var::from_language(pat_var[0].clone()).unwrap().set_permission(false),
                        typ.clone().unwrap_or(Type::Empty(HelpData::default())),
                        Box::new(Lang::Chain(Box::new(Lang::Number(0.0, eq.into())),
                        Box::new(body), pat_var.into())), _let.into())]))

            } else {
                Ok((s,
                pat_var.iter().map(|x| {
                    Lang::Let(Var::from_language(x.clone()).unwrap().set_permission(false), typ.clone().unwrap_or(Type::Empty(HelpData::default())), Box::new(body.clone()), HelpData::default())
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
                        => Lang::Let(var.clone().set_permission(true),
                                    typ.clone(),
                                    body.clone(), h.clone()),
                    lan => lan.clone()
                }
            }).collect();
            Ok((s, new_le))
        },
        Err(r) => Err(r),
    }
}

fn base_mut_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("mut"), multispace0),
            pattern_var,
            opt(preceded(terminated(tag(":"), multispace0), ltype)),
            equality_operator,
            single_parse,
          ).parse(s);
    match res {
        Ok((s, (_met, (var, None), typ, _eq, Lang::Function(ki, params, ty, body, h)))) 
            if params.len() > 0 => {
                let newvar = Var::from_language(var[0].clone())
                    .unwrap()
                    .set_type(params[0].1.clone())
                    .set_mutability(true);
                Ok((s, Lang::Let(newvar, typ.unwrap_or(Type::Empty(HelpData::default())),
                Box::new(Lang::Function(ki, params, ty, body, h.clone())), h)))
            },
        Ok((s, (_let, (var, _), typ, _eq, body))) => {
            Ok((s, Lang::Let(
                        Var::from_language(var[0].clone())
                            .unwrap()
                            .set_mutability(true),
                            typ.unwrap_or(Type::Empty(HelpData::default())), Box::new(body),
                            _let.into())))
        },
        Err(r) => Err(r)
    }
}

fn mut_exp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (
        opt(terminated(tag("pub"), multispace0)),
        base_mut_exp).parse(s);
    match res {
        Ok((s, (None, le))) => Ok((s, vec![le])),
        Ok((s, (Some(_pu), Lang::Let(var, typ, body, h)))) 
            => Ok((s, vec![Lang::Let(
                    var.clone().set_permission(true),
                    typ.clone(),
                    body.clone(),
                    h.clone())] 
                   )),
        Err(r) => Err(r),
        _ => todo!()
    }
}

fn base_type_exp(s: Span) -> IResult<Span, Lang> {
    let res = (
            terminated(tag("type"), multispace0),
            type_alias,
            terminated(tag("="), multispace0),
            ltype,
            terminated(tag(";"), multispace0) 
          ).parse(s);
    match res {
        Ok((s, (_ty, Type::Alias(name, params, path, h), _eq, ty, _))) 
            => {
                let h2 = if params.len() > 0 { params[0].clone().into()} else { HelpData::default() };
                Ok((s, Lang::Alias(
                                Var::from_name(&name)
                                    .set_type(Type::Params(params.clone(), h2))
                                    .add_path(path),
                                params, ty, h)))
            },
        Ok((s, (_ty, _, _eq, _ty2, _)))
            => {
                dbg!(&_ty);
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
            => Ok((s, vec![Lang::Alias(
                        var.set_permission(false),
                        params,
                        typ, h)])),
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
        Ok((s, (_ty, Type::Alias(name, params, path, h), _eq, ty, _))) 
            => Ok((s, Lang::Alias(
                        Var::from_name(&name)
                            .set_type(Type::Params(params.clone(), params.clone().into()))
                            .add_path(path)
                            .set_opacity(true),
                        params, ty, h))),
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
            => Ok((s, vec![Lang::Alias(var.set_opacity(true), params, typ, h)])),
        Ok((s, (None, Lang::Alias(var, params, typ, h)))) 
            => Ok((s, vec![Lang::Alias(
                        var.set_permission(false).set_opacity(true),
                        params,
                        typ, h)])),
        Err(r) => Err(r),
        _ => todo!()
    }
}

pub fn module(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("module"), multispace0),
        terminated(variable_exp, multispace0),
        terminated(tag("{"), multispace0),
        parse_exp,
        terminated(tag("}"), multispace0),
        terminated(tag(";"), multispace0)
          ).parse(s);
    match res {
        Ok((s, (modu, (name, _), _op, Lang::Sequence(v, _h), _cl, _dv))) => 
            Ok((s, vec![Lang::Module(name, v, modu.into())])),
        Err(r) => Err(r),
        _ => todo!()
    }
}

pub fn return_exp(s: Span) -> IResult<Span, Lang> {
    let res = terminated(delimited(tag("return "), parse_elements, tag(";")), multispace0).parse(s);
    match res {
        Ok((s, el)) 
            => Ok((s, Lang::Return(Box::new(el.clone()), el.into()))),
        Err(r) => Err(r)
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
        Ok((s, (var, _eq, exp, _pv))) 
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

fn bangs_exp(s: Span) -> IResult<Span,Vec<Lang>> {
    let res = bang_exp(s);
    match res {
        Ok((s, exp)) => Ok((s, vec![exp])),
        Err(r) => Err(r)
    }
}

fn mod_imp(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("mod"), multispace0),
            terminated(variable_exp, multispace0),
            terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (_mod, (name, _), _sc))) 
            => Ok((s, vec![Lang::ModImp(name.to_string(), _mod.into())])),
        Err(r) => Err(r)
    }
}

fn import_var(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (terminated(tag("use"), multispace0),
                variable,
                terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (_use, Lang::Variable(name, path, perm, mutop, typ, h), _sc))) => {
            let var1 =  Lang::Variable(name.clone(), path.clone(), perm.clone(), mutop.clone(), typ.clone(), h.clone());
            let var2 =  Lang::Variable(name.clone(), "".into(), perm.clone(), mutop.clone(), typ.clone(), h.clone());
            let shortcut = Lang::Let(Var::from_language(var2).unwrap(), Type::Any(HelpData::default()), Box::new(var1), h.clone());
            Ok((s, vec![shortcut]))
        }
        Err(r) => Err(r),
        _ => todo!()
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

fn signature_variable(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("@"),
                variable_exp,
                terminated(tag(":"), multispace0),
                ltype, 
                terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (at, (name, h), _col, typ, _))) 
            => {
                let var2 = Var::from_name(&name).set_help_data(h).set_type(typ.clone());
                Ok((s, vec![Lang::Signature(var2, typ, at.into())]))
            },
        Err(r) => Err(r)
    }
}

fn signature_opaque(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (tag("@"),
                type_alias,
                terminated(tag(";"), multispace0)).parse(s);
    match res {
        Ok((s, (at, Type::Alias(name, params, _, h), _))) 
            => {
                let var2 = Var::from_name(&name)
                    .set_help_data(h.clone())
                    .set_type(Type::Params(params, h.clone()));
                Ok((s, vec![Lang::Signature(var2, Type::Empty(h), at.into())]))
            },
        Ok((_s, (_, _, _))) => todo!(),
        Err(r) => Err(r)
    }
}

fn signature(s: Span) -> IResult<Span, Vec<Lang>> {
    alt((signature_opaque, signature_variable)).parse(s)
}

// main
fn base_parse(s: Span) -> IResult<Span, Vec<Lang>> {
    let res = (opt(multispace0),
        many0(alt((signature, library, tests, import_type, import_var, mod_imp, comment, type_exp, mut_exp, opaque_exp, let_exp, module, assign, bangs_exp))),
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

pub fn parse_exp(s: Span) -> IResult<Span, Lang> {
    let res = base_parse(s);
    match res {
        Ok((s, v)) => Ok((s, Lang::Sequence(v.clone(), v.into()))),
        Err(r) => Err(r)
    }
}

// main
pub fn parse(s: Span) -> IResult<Span, Adt> {
    let res = base_parse(s);
    match res {
        Ok((s, v)) => Ok((s, Adt(v.clone()))),
        Err(r) => Err(r)
    }
}

// main test
#[cfg(test)]
mod tesus {
    use super::*;
    use crate::builder;

    #[test]
    fn test_type_exp0() {
        let res = type_exp("type Mat = [2, [2, num]];".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_type_exp1() {
        let res = type_exp("type Num = num;".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_type_exp2() {
        let res = type_exp("type Mat<M, N, T> = [M, [N, T]];".into()).unwrap().0;
        assert_eq!(res, "alias(var('Mat'), [M, N, T], [M, [N, T]])".into());
    }

    #[test]
    fn test_type_exp3() {
        let res = type_exp("type Point = {x: num, y: num};".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_type_exp4() {
        let res = type_exp("type Nn = Un | Deux;".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_let1() {
        let res = let_exp("let hello: num = 4 ;".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_let2() {
        let res = let_exp("let a : Num<num> = 3;".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_let3() {
        let res = let_exp("let a : num = 3;".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_let4() {
        let res = let_exp("let a : Num<2> = 3;".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_let5() {
        let res = let_exp("let ma: Mat<num, num> = [[0, 0], [0, 0]];".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_let6() {
        let res = let_exp("let f = fn(a: num, b: bool): num {...};".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_let7() {
        let res = let_exp("let f <- fn(): bool { true };".into()).unwrap().1;
        assert_eq!(res, vec![])
    }

    #[test]
    fn test_suite1() {
        let res = parse("let a = 5; let b = 6; b;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_suite2() {
        let res = parse("type Nu = num;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_suite3() {
        let res = parse("type nu = num; type bo = bool;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }
    #[test]
    fn test_suite4() {
        let res = parse("type B = num; let a: Re<num> = record {b: true, t: 10}; a;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_suite5() {
        let res = parse("type C = num; let a: Re<num> = 5; type B<T> = num; b;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_suite6() {
        let res = parse("let z: Mat<2, 2, num> = 8; type Mat<M, N, T> = [M, [N, T]]; let a: Mat<2, 2, num> = [[2, 2], [2, 2]]; a;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_parse_chars() {
        let res = parse("let p1: chars = \"hey\";".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_base_parse1() {
        let res = base_parse("let a = 5;".into()).unwrap().1;
        assert_eq!(res, vec![builder::empty_lang()]);
    }

    #[test]
    fn test_base_parse2() {
        let res = base_let_exp("let a = 5;".into()).unwrap().1;
        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_base_parse3() {
        let res = base_mut_exp("mut a <- 5;".into()).unwrap().1;
        if let Lang::Let(var, _, _, _) = res {
            assert!(var.is_mutable())
        } else {
            assert!(false)
        }
    }

    #[test]
    fn test_base_parse4() {
        let res = parse("module Person { let nom = 'Jean'; }".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_base_parse5() {
        let res = parse("let a: Combo = 4;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_module1() {
        let res = module("module add { let a = 5; };".into()).unwrap().1;
        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_module2() {
        let res = module("module Add { let a = 5; }".into()).unwrap().1;
        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_module5() {
        let res = parse_exp("module Add { let a = 5; }".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang())
    }

    #[test]
    fn test_parse_module() {
        let res = parse("module Add { let a = 5; }".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_option_type1() {
        let res = parse("let a: Option<T> = Some(7);".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_pattern_var1() {
        let res = pattern_var("Some(n)".into()).unwrap().1.0;
        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_pattern_var2() {
        let res = base_let_exp("let Some(n) = Some(5);".into()).unwrap().1;
        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_func1() {
        let res = base_let_exp("let pres <- fn(a: {name: chars, age: num}): num { ... };".into()).unwrap().1;
        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_classic_let() {
        let res = parse("let duck = record { wing: Wings, name: 'Daffy'};".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_let_func1() {
        let res = parse("let map <- fn(a: [N, T], f: (T) -> U): [N, U] { ... };".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_let_func2() {
        let res = parse("let append <- fn(a: [#N, T], b: T): [#N+1, T] { ... };".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_assignement1() {
        let res = parse("Mod::a <- 4;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_alias_alias() {
        let res = parse("type Truc = Machin;".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_parse_range1() {
        let res = parse("1:a".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_parse_range2() {
        let res = parse("a:3".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_parse_range3() {
        let res = parse("1:a:3".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_parse_op1() {
        let res = parse("let less_than_3 = fn(n: int): bool { 3 >= n };".into()).unwrap().1;
        assert_eq!(res.0, vec![])
    }

    #[test]
    fn test_ret0() {
        let res = return_exp("return a + 1;".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_base_type_exp() {
        let res = base_type_exp("type Combo = .Truc(int) | .Wow(int);".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }
    
    #[test]
    fn test_type_record() {
        let res = base_type_exp("type Plot = { x: [#N, num], y: [#N, num], t: char };".into()).unwrap().1;
        assert_eq!(res, builder::empty_lang());
    }

    #[test]
    fn test_let_seq() {
        let res = parse("let seq <- fn(a: #I, b: #J, c: #K): [#J-#I/#K, int] { ... };".into()).unwrap().1;
        assert_eq!(res.0, vec![]);
    }

    #[test]
    fn test_let_mat() {
        let res = parse("let a: Mat<3, 2> <- [[1, 2], [4, 5], [7, 8]];".into()).unwrap().1;
        assert_eq!(res.0, vec![]);
    }

    #[test]
    fn test_module_import0() {
        let res = mod_imp("mod calcul;".into()).unwrap().1;
        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_module_import1() {
        let res = parse("mod calcul;".into()).unwrap().1;
        assert_eq!(res.0, vec![]);
    }

    #[test]
    fn test_signature0() {
        let res = signature("@data: int;".into()).unwrap().1;
        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_signature1() {
        let res = parse("@data: int;".into()).unwrap().1;
        assert_eq!(res.0, vec![]);
    }

}
