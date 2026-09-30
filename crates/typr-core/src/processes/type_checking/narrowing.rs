//! Condition narrowing (`refined_types_plan.md`, Phase 9, §24).
//!
//! Inside `if (length(x) == 5) { .. }` the checker knows more about `x` than
//! its declared type says: the branch is entered only when the condition
//! holds, so `x` can be treated as `[5, int]` there without any runtime
//! check (the condition *is* the check). A condition is read as a list of
//! facts `(variable, measure, interval)` and each fact is intersected with
//! what the variable already is, through the same `apply_refinements` as an
//! annotation. Nothing is narrowed when the intersection is empty (dead
//! branch) or the base does not support the measure (`x > 0` on a vector).
//!
//! Recognised, on either side of the comparison:
//! * `length(x) <op> n`  ->  measure `Length`
//! * `x <op> c`          ->  measure `Value` (scalars only)
//! * `a && b` (then-branch) and `a || b` (else-branch), `!c`
//!
//! The else-branch uses the negated comparison; `==` says nothing when
//! false, and neither does `!=` when true.

use crate::components::context::Context;
use crate::components::language::operators::Op;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::refinement::{Interval, Measure, RefinementSet};
use crate::components::r#type::Type;
use crate::processes::type_checking::type_arithmetic::apply_refinements;
use crate::processes::type_checking::type_comparison::reduce_type;

type Fact = (String, Measure, Interval);

#[derive(Clone, Copy)]
enum Cmp {
    Eq,
    NotEq,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Cmp {
    fn of(op: &Op) -> Option<Cmp> {
        Some(match op {
            Op::Eq(_) => Cmp::Eq,
            Op::NotEq(_) => Cmp::NotEq,
            Op::LesserThan(_) => Cmp::Lt,
            Op::LesserOrEqual(_) => Cmp::Le,
            Op::GreaterThan(_) => Cmp::Gt,
            Op::GreaterOrEqual(_) => Cmp::Ge,
            _ => return None,
        })
    }

    /// `c <op> x` read as `x <flipped op> c`.
    fn flip(self) -> Cmp {
        match self {
            Cmp::Lt => Cmp::Gt,
            Cmp::Le => Cmp::Ge,
            Cmp::Gt => Cmp::Lt,
            Cmp::Ge => Cmp::Le,
            c => c,
        }
    }

    fn negate(self) -> Cmp {
        match self {
            Cmp::Eq => Cmp::NotEq,
            Cmp::NotEq => Cmp::Eq,
            Cmp::Lt => Cmp::Ge,
            Cmp::Le => Cmp::Gt,
            Cmp::Gt => Cmp::Le,
            Cmp::Ge => Cmp::Lt,
        }
    }

    /// What `x <op> c` tells about `x`, if anything.
    fn interval(self, c: f64) -> Option<Interval> {
        match self {
            Cmp::Eq => Some(Interval::point(c)),
            Cmp::NotEq => None,
            Cmp::Lt => Some(Interval::less_than(c)),
            Cmp::Le => Some(Interval::at_most(c)),
            Cmp::Gt => Some(Interval::greater_than(c)),
            Cmp::Ge => Some(Interval::at_least(c)),
        }
    }
}

fn literal(l: &Lang) -> Option<f64> {
    match l {
        Lang::Integer { value, .. } => Some(*value as f64),
        Lang::Number { value, .. } => Some(*value),
        _ => None,
    }
}

/// `x` or `length(x)` with `x` a plain variable.
fn subject(l: &Lang) -> Option<(String, Measure)> {
    match l {
        Lang::Variable { name, .. } => Some((name.clone(), Measure::Value)),
        Lang::FunctionApp { identifier, arguments, .. } if arguments.len() == 1 => match (&**identifier, &arguments[0]) {
            (Lang::Variable { name: f, .. }, Lang::Variable { name, .. }) if f == "length" => {
                Some((name.clone(), Measure::Length))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Facts that hold when `cond` evaluates to `holds`.
fn facts(cond: &Lang, holds: bool, out: &mut Vec<Fact>) {
    match cond {
        // `(c)` parses as a one-expression scope.
        Lang::Scope { body, .. } if body.len() == 1 => facts(&body[0], holds, out),
        Lang::Not { value, .. } => facts(value, !holds, out),
        Lang::Operator { operator: Op::And(_) | Op::And2(_), lhs, rhs, .. } if holds => {
            facts(lhs, true, out);
            facts(rhs, true, out);
        }
        Lang::Operator { operator: Op::Or(_) | Op::Or2(_), lhs, rhs, .. } if !holds => {
            facts(lhs, false, out);
            facts(rhs, false, out);
        }
        // `&&` / `||` reach the checker as a call to the operator's function.
        Lang::FunctionApp { identifier, arguments, .. } if arguments.len() == 2 => {
            let Lang::Variable { name, .. } = &**identifier else { return };
            let wanted = match name.trim_matches('`') {
                "&&" | "&" if holds => true,
                "||" | "|" if !holds => false,
                _ => return,
            };
            facts(&arguments[0], wanted, out);
            facts(&arguments[1], wanted, out);
        }
        // `Lang::Operator` stores the *left* operand in `rhs`.
        Lang::Operator { operator, lhs: right, rhs: left, .. } => {
            let Some(cmp) = Cmp::of(operator) else { return };
            let (lhs, rhs) = (left, right);
            let (subj, c, cmp) = match (subject(lhs), literal(rhs), literal(lhs), subject(rhs)) {
                (Some(s), Some(c), _, _) => (s, c, cmp),
                (_, _, Some(c), Some(s)) => (s, c, cmp.flip()),
                _ => return,
            };
            let cmp = if holds { cmp } else { cmp.negate() };
            if let Some(iv) = cmp.interval(c) {
                out.push((subj.0, subj.1, iv));
            }
        }
        _ => {}
    }
}

/// `context` as it is inside the branch entered when `cond` is `holds`.
pub fn narrow(context: &Context, cond: &Lang, holds: bool) -> Context {
    let mut found = Vec::new();
    facts(cond, holds, &mut found);
    // One narrowing per variable, from the meet of all its facts
    // (`length(v) > 1 && length(v) < 3` is a single `length(v) == 2`).
    let mut per_variable: Vec<(String, RefinementSet)> = Vec::new();
    for (name, measure, iv) in found {
        let fact = RefinementSet::empty().with(measure, iv);
        match per_variable.iter_mut().find(|(n, _)| *n == name) {
            Some((_, set)) => *set = set.meet(&fact),
            None => per_variable.push((name, fact)),
        }
    }
    per_variable
        .into_iter()
        .fold(context.clone(), |ctx, (name, set)| narrow_variable(ctx, &name, set))
}

fn narrow_variable(context: Context, name: &str, facts: RefinementSet) -> Context {
    // Only a variable with a single visible binding: with overloads there is
    // no telling which one the condition talks about.
    let mut entries = context.typing_context.entries_named(name);
    if entries.len() != 1 {
        return context;
    }
    let (var, declared) = entries.remove(0);
    let reduced = reduce_type(&context, &declared);
    if !matches!(reduced.unrefined(), Type::Vec(..) | Type::Integer(..) | Type::Number(..)) {
        return context;
    }
    let mut set = reduced.refinements_of().meet(&facts);
    // A length is an integer: `1 < length(x) < 3` means `length(x) == 2`.
    if let Some(len) = set.get(Measure::Length).map(|iv| iv.to_integral()) {
        set = set.with(Measure::Length, len);
    }
    let h = reduced.get_help_data();
    match apply_refinements(reduced.unrefined().clone(), set, h) {
        Type::Failed(..) => context,
        narrowed => {
            let snapshot = context.clone();
            context.replace_or_push_var_type(var, narrowed, &snapshot)
        }
    }
}
