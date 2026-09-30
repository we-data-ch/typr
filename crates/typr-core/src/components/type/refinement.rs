//! Refinement algebra: properties normalised as *intervals over measures*.
//!
//! A refinement such as `length(5)` or `(> 0)` is not kept as a bare
//! predicate. It is folded into an [`Interval`] on a [`Measure`] (the length
//! of a vector, the value of a scalar), and a [`RefinementSet`] holds at most
//! one interval per measure. That gives, by construction:
//!
//! * commutativity / associativity / idempotence of `&` (interval meet),
//! * `contradicts` = empty interval, `implies` = interval inclusion,
//! * a canonical, deterministic `Hash`/`Eq` (the set is kept sorted), which
//!   the subtype cache relies on.
//!
//! Nothing here knows about `Type`; see `type_arithmetic` for the wiring.

use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::tint::Tint;
use crate::components::r#type::Type;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

/// A finite `f64` with a total order and a canonical `Hash`/`Eq`
/// (so `-0.0 == 0.0`). Mirrors what `Tnum` does for its payload.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Num(f64);

impl Num {
    pub fn new(v: f64) -> Self {
        // Normalise -0.0 so that equality and hashing agree.
        Num(if v == 0.0 { 0.0 } else { v })
    }

    pub fn get(&self) -> f64 {
        self.0
    }
}

impl PartialEq for Num {
    fn eq(&self, other: &Self) -> bool {
        self.0.total_cmp(&other.0) == Ordering::Equal
    }
}
impl Eq for Num {}

impl PartialOrd for Num {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Num {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}
impl Hash for Num {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

impl fmt::Display for Num {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// What a property talks about. Extensible (`Nchar`, `Nrow`, `Ncol`, ...).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Measure {
    /// The value itself (`(> 0)` on `int`/`num`; every element for a vector).
    Value,
    /// `length(x)`.
    Length,
}

impl fmt::Display for Measure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Measure::Value => write!(f, "value"),
            Measure::Length => write!(f, "length"),
        }
    }
}

/// One end of an interval. On the lower end `Open(c)` means `> c`; on the
/// upper end it means `< c`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Bound {
    Unbounded,
    Open(Num),
    Closed(Num),
}

impl Bound {
    fn value(&self) -> Option<Num> {
        match self {
            Bound::Unbounded => None,
            Bound::Open(n) | Bound::Closed(n) => Some(*n),
        }
    }

    /// The tighter of two *lower* bounds.
    fn max_lower(self, other: Bound) -> Bound {
        match (self, other) {
            (Bound::Unbounded, b) | (b, Bound::Unbounded) => b,
            (a, b) => match a.value().cmp(&b.value()) {
                Ordering::Greater => a,
                Ordering::Less => b,
                // Same constant: the open one excludes more.
                Ordering::Equal => match a {
                    Bound::Open(_) => a,
                    _ => b,
                },
            },
        }
    }

    /// The tighter of two *upper* bounds.
    fn min_upper(self, other: Bound) -> Bound {
        match (self, other) {
            (Bound::Unbounded, b) | (b, Bound::Unbounded) => b,
            (a, b) => match a.value().cmp(&b.value()) {
                Ordering::Less => a,
                Ordering::Greater => b,
                Ordering::Equal => match a {
                    Bound::Open(_) => a,
                    _ => b,
                },
            },
        }
    }
}

/// A (possibly unbounded) interval of reals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Interval {
    lo: Bound,
    hi: Bound,
}

impl Interval {
    pub fn new(lo: Bound, hi: Bound) -> Self {
        Interval { lo, hi }
    }

    pub fn full() -> Self {
        Interval::new(Bound::Unbounded, Bound::Unbounded)
    }

    /// `[c, c]`
    pub fn point(c: f64) -> Self {
        let n = Num::new(c);
        Interval::new(Bound::Closed(n), Bound::Closed(n))
    }

    /// `(c, +inf)`
    pub fn greater_than(c: f64) -> Self {
        Interval::new(Bound::Open(Num::new(c)), Bound::Unbounded)
    }

    /// `[c, +inf)`
    pub fn at_least(c: f64) -> Self {
        Interval::new(Bound::Closed(Num::new(c)), Bound::Unbounded)
    }

    /// `(-inf, c)`
    pub fn less_than(c: f64) -> Self {
        Interval::new(Bound::Unbounded, Bound::Open(Num::new(c)))
    }

    /// `(-inf, c]`
    pub fn at_most(c: f64) -> Self {
        Interval::new(Bound::Unbounded, Bound::Closed(Num::new(c)))
    }

    pub fn lo(&self) -> Bound {
        self.lo
    }

    pub fn hi(&self) -> Bound {
        self.hi
    }

    pub fn is_full(&self) -> bool {
        *self == Interval::full()
    }

    /// The single value of a degenerate `[c, c]` interval.
    pub fn as_point(&self) -> Option<f64> {
        match (self.lo, self.hi) {
            (Bound::Closed(a), Bound::Closed(b)) if a == b => Some(a.get()),
            _ => None,
        }
    }

    pub fn is_empty(&self) -> bool {
        match (self.lo.value(), self.hi.value()) {
            (Some(l), Some(h)) => match l.cmp(&h) {
                Ordering::Greater => true,
                Ordering::Equal => {
                    !(matches!(self.lo, Bound::Closed(_)) && matches!(self.hi, Bound::Closed(_)))
                }
                Ordering::Less => false,
            },
            _ => false,
        }
    }

    /// Intersection (`&`).
    pub fn meet(&self, other: &Interval) -> Interval {
        Interval::new(self.lo.max_lower(other.lo), self.hi.min_upper(other.hi))
    }

    /// `self ⊆ other`: every value allowed by `self` is allowed by `other`.
    /// An empty `self` implies everything.
    pub fn implies(&self, other: &Interval) -> bool {
        self.is_empty() || self.meet(other) == *self
    }

    /// Some value satisfies both.
    pub fn compatible(&self, other: &Interval) -> bool {
        !self.meet(other).is_empty()
    }

    /// No value satisfies both.
    pub fn contradicts(&self, other: &Interval) -> bool {
        !self.compatible(other)
    }

    /// Tighten the bounds to integers: `(> 0)` becomes `[1, +inf)`,
    /// `(< 1)` becomes `(-inf, 0]`. Used when the base type is `int`, so that
    /// `int & (> 0) & (< 1)` is detected as empty.
    pub fn to_integral(&self) -> Interval {
        let lo = match self.lo {
            Bound::Unbounded => Bound::Unbounded,
            Bound::Closed(n) => Bound::Closed(Num::new(n.get().ceil())),
            Bound::Open(n) => Bound::Closed(Num::new(n.get().floor() + 1.0)),
        };
        let hi = match self.hi {
            Bound::Unbounded => Bound::Unbounded,
            Bound::Closed(n) => Bound::Closed(Num::new(n.get().floor())),
            Bound::Open(n) => Bound::Closed(Num::new(n.get().ceil() - 1.0)),
        };
        Interval::new(lo, hi)
    }
}

impl Measure {
    /// Renders an interval on this measure in surface syntax, one property
    /// per bound: `length(5)`, `(> 0) & (< 10)`.
    pub fn display(&self, iv: &Interval) -> String {
        if let Some(p) = iv.as_point() {
            return match self {
                Measure::Length => format!("length({})", p),
                Measure::Value => format!("(== {})", Num::new(p)),
            };
        }
        let mut parts = Vec::new();
        let (lo_op, hi_op) = match self {
            Measure::Value => (("(> ", "(>= "), ("(< ", "(<= ")),
            Measure::Length => (("length(> ", "length(>= "), ("length(< ", "length(<= ")),
        };
        match iv.lo {
            Bound::Open(n) => parts.push(format!("{}{})", lo_op.0, n)),
            Bound::Closed(n) => parts.push(format!("{}{})", lo_op.1, n)),
            Bound::Unbounded => {}
        }
        match iv.hi {
            Bound::Open(n) => parts.push(format!("{}{})", hi_op.0, n)),
            Bound::Closed(n) => parts.push(format!("{}{})", hi_op.1, n)),
            Bound::Unbounded => {}
        }
        parts.join(" & ")
    }
}

/// Surface form of a single property, as the parser produces it.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Refinement {
    /// `length(5)`
    Length(i32),
    /// `(> c)`
    Gt(Num),
    /// `(< c)`
    Lt(Num),
    /// Every other bound: `(>= c)`, `(<= c)`, `length(> n)`, `length(<= n)`...
    Range(Measure, Interval),
}

impl Refinement {
    pub fn measure(&self) -> Measure {
        match self {
            Refinement::Length(_) => Measure::Length,
            Refinement::Gt(_) | Refinement::Lt(_) => Measure::Value,
            Refinement::Range(m, _) => *m,
        }
    }

    pub fn interval(&self) -> Interval {
        match self {
            Refinement::Length(n) => Interval::point(*n as f64),
            Refinement::Gt(c) => Interval::greater_than(c.get()),
            Refinement::Lt(c) => Interval::less_than(c.get()),
            Refinement::Range(_, iv) => *iv,
        }
    }
}

impl fmt::Display for Refinement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Refinement::Length(n) => write!(f, "length({})", n),
            Refinement::Gt(c) => write!(f, "(> {})", c),
            Refinement::Lt(c) => write!(f, "(< {})", c),
            Refinement::Range(m, iv) => write!(f, "{}", m.display(iv)),
        }
    }
}

/// The length of a vector, i.e. the [`Measure::Length`] value stored on
/// `Type::Vec`. A numeric length is an [`Interval`] (`[5, T]` is `[5, 5]`,
/// an unsized `[T]` is the full interval); a length that cannot be decided
/// numerically (`#N`, `#N + 1`) keeps its index expression symbolic.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Length {
    Range(Interval),
    Sym(Box<Type>),
}

impl Length {
    /// No known length (`[int]`).
    pub fn unknown() -> Self {
        Length::Range(Interval::full())
    }

    pub fn known(n: i32) -> Self {
        Length::Range(Interval::point(n as f64))
    }

    /// Reads a length index as written in a type: an integer literal, `any`
    /// (unsized), or an index expression kept as-is.
    pub fn from_type(t: &Type) -> Self {
        match t {
            Type::Integer(Tint::Val(n), _) => Length::known(*n),
            Type::Any(_) => Length::unknown(),
            other => Length::Sym(Box::new(other.clone())),
        }
    }

    /// The length as an index type. A numeric range that is neither a point
    /// nor unbounded has no index form and reads as unsized.
    pub fn to_type(&self, help: HelpData) -> Type {
        match self {
            Length::Range(iv) => match iv.as_point() {
                Some(n) => Type::Integer(Tint::Val(n as i32), help),
                None => Type::Any(help),
            },
            Length::Sym(t) => (**t).clone(),
        }
    }

    /// The length as an interval on [`Measure::Length`]; `None` when symbolic.
    pub fn interval(&self) -> Option<Interval> {
        match self {
            Length::Range(iv) => Some(*iv),
            Length::Sym(_) => None,
        }
    }

    /// The exact length when it is a known literal.
    pub fn as_known(&self) -> Option<i32> {
        match self {
            Length::Range(iv) => iv.as_point().map(|n| n as i32),
            Length::Sym(_) => None,
        }
    }

    /// A numeric range that is neither a point nor unbounded (`length(> 0)`):
    /// the only lengths with no index form, printed as a refinement.
    pub fn as_proper_range(&self) -> Option<Interval> {
        match self {
            Length::Range(iv) if iv.as_point().is_none() && !iv.is_full() => Some(*iv),
            _ => None,
        }
    }

    /// The surface property for [`Length::as_proper_range`] (`length(> 0)`).
    pub fn range_property(&self) -> Option<String> {
        self.as_proper_range().map(|iv| Measure::Length.display(&iv))
    }

    /// An interval every length denoted by `self` lies in. Exact for a
    /// range; for a symbolic length it is a sound lower bound (a length is
    /// never negative, `#N + 2` is at least 2) with no upper bound.
    pub fn bounds(&self) -> Interval {
        match self {
            Length::Range(iv) => *iv,
            Length::Sym(t) => Interval::at_least(sym_lower_bound(t).max(0.0)),
        }
    }

    /// `self ⊆ other`, decided on the bounds. A symbolic length only implies
    /// another symbolic one when they are the same expression.
    pub fn implies(&self, other: &Length) -> bool {
        match (self, other) {
            (Length::Sym(a), Length::Sym(b)) => a == b,
            (_, Length::Sym(_)) => false,
            (_, Length::Range(iv)) => self.bounds().implies(iv),
        }
    }

    /// No length can satisfy both.
    pub fn contradicts(&self, other: &Length) -> bool {
        match (self, other) {
            (Length::Sym(_), Length::Sym(_)) => false,
            _ => self.bounds().contradicts(&other.bounds()),
        }
    }

    /// Intersect with `iv`. `None` when the result has no representation:
    /// a symbolic length narrowed by a range it does not already satisfy
    /// stays a `Refined` property. `Some(Err)` is a contradiction.
    pub fn meet_interval(&self, iv: &Interval) -> Option<Result<Length, ()>> {
        match self {
            Length::Range(cur) => {
                // Kept as written (`> 0`, not `>= 1`): the bound is what the
                // runtime check and the printer show. Emptiness is judged on
                // integers, lengths being whole numbers.
                let m = cur.meet(iv);
                Some(if m.to_integral().is_empty() { Err(()) } else { Ok(Length::Range(m)) })
            }
            Length::Sym(_) if self.bounds().implies(iv) => Some(Ok(self.clone())),
            Length::Sym(_) if self.bounds().contradicts(iv) => Some(Err(())),
            Length::Sym(_) => None,
        }
    }
}

/// Lower bound of an index expression read as a length: literals and sums
/// are exact, an index generic is only known to be a length (>= 0).
fn sym_lower_bound(t: &Type) -> f64 {
    use crate::components::r#type::type_operator::TypeOperator;
    match t {
        Type::Integer(Tint::Val(n), _) => *n as f64,
        Type::Operator(TypeOperator::Addition, a, b, _) => sym_lower_bound(a).max(0.0) + sym_lower_bound(b).max(0.0),
        Type::Operator(TypeOperator::Multiplication, a, b, _) => sym_lower_bound(a).max(0.0) * sym_lower_bound(b).max(0.0),
        _ => 0.0,
    }
}

/// A conjunction of properties: at most one non-full interval per measure,
/// kept sorted by measure so that `Hash`/`Eq` are canonical. (A `Vec` rather
/// than a `BTreeMap` so it round-trips through non-string-keyed formats.)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct RefinementSet(Vec<(Measure, Interval)>);

impl RefinementSet {
    pub fn empty() -> Self {
        RefinementSet(Vec::new())
    }

    pub fn single(r: &Refinement) -> Self {
        RefinementSet::empty().with(r.measure(), r.interval())
    }

    pub fn is_trivial(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, m: Measure) -> Option<&Interval> {
        self.0.iter().find(|(k, _)| *k == m).map(|(_, iv)| iv)
    }

    pub fn iter(&self) -> impl Iterator<Item = &(Measure, Interval)> {
        self.0.iter()
    }

    /// Add a constraint on `m`, intersecting with what is already there.
    pub fn with(mut self, m: Measure, iv: Interval) -> Self {
        match self.0.iter_mut().find(|(k, _)| *k == m) {
            Some((_, cur)) => *cur = cur.meet(&iv),
            None => {
                if !iv.is_full() {
                    self.0.push((m, iv));
                    self.0.sort_by_key(|(k, _)| *k);
                }
            }
        }
        self
    }

    /// Intersection (`&`) of two sets.
    pub fn meet(&self, other: &RefinementSet) -> RefinementSet {
        other
            .0
            .iter()
            .fold(self.clone(), |acc, (m, iv)| acc.with(*m, *iv))
    }

    /// Some measure has an empty interval: no value can satisfy the set.
    /// `integral` rounds bounds to integers first (base type `int`, and
    /// always for `Length`).
    pub fn is_empty(&self, integral: bool) -> bool {
        self.0.iter().any(|(m, iv)| {
            let iv = if integral || *m == Measure::Length { iv.to_integral() } else { *iv };
            iv.is_empty()
        })
    }

    /// `self ⊆ other`: every measure constrained by `other` is constrained at
    /// least as tightly by `self`.
    pub fn implies(&self, other: &RefinementSet) -> bool {
        other.0.iter().all(|(m, want)| match self.get(*m) {
            Some(have) => have.implies(want),
            None => want.is_full(),
        })
    }

    /// Some value satisfies both sets.
    pub fn compatible(&self, integral: bool) -> bool {
        !self.is_empty(integral)
    }

    /// No value satisfies both `self` and `other`.
    pub fn contradicts(&self, other: &RefinementSet, integral: bool) -> bool {
        self.meet(other).is_empty(integral)
    }
}

impl fmt::Display for RefinementSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parts: Vec<String> = self.0.iter().map(|(m, iv)| m.display(iv)).collect();
        write!(f, "{}", parts.join(" & "))
    }
}

/// A runtime check the type checker decided is needed at one boundary
/// (Phase 5, D5). Keyed by the source span of the checked expression; the
/// transpiler only reads the table and never reasons about refinements.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RefinementObligation {
    pub file: String,
    pub start: usize,
    pub end: usize,
    pub set: RefinementSet,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn gt(c: f64) -> RefinementSet {
        RefinementSet::single(&Refinement::Gt(Num::new(c)))
    }
    fn lt(c: f64) -> RefinementSet {
        RefinementSet::single(&Refinement::Lt(Num::new(c)))
    }
    fn len(n: i32) -> RefinementSet {
        RefinementSet::single(&Refinement::Length(n))
    }

    #[test]
    fn meet_is_commutative_associative_idempotent() {
        let (a, b, c) = (gt(0.0), lt(10.0), gt(3.0));
        assert_eq!(a.meet(&b), b.meet(&a));
        assert_eq!(a.meet(&b).meet(&c), a.meet(&b.meet(&c)));
        assert_eq!(a.meet(&a), a);
        assert_eq!(len(5).meet(&len(5)), len(5));
    }

    #[test]
    fn contradictions() {
        assert!(gt(10.0).contradicts(&lt(5.0), false));
        assert!(len(5).contradicts(&len(10), false));
        assert!(!gt(0.0).contradicts(&lt(10.0), false));
        // (> 5) & (< 5) is empty even over the reals
        assert!(gt(5.0).contradicts(&lt(5.0), false));
    }

    #[test]
    fn integer_rounding_detects_gaps() {
        // (> 0) & (< 1): fine over num, empty over int
        let s = gt(0.0).meet(&lt(1.0));
        assert!(!s.is_empty(false));
        assert!(s.is_empty(true));
    }

    #[test]
    fn implication_is_interval_inclusion() {
        assert!(gt(5.0).implies(&gt(0.0)));
        assert!(!gt(0.0).implies(&gt(5.0)));
        assert!(gt(0.0).implies(&RefinementSet::empty()));
        assert!(!RefinementSet::empty().implies(&gt(0.0)));
        assert!(len(5).implies(&RefinementSet::empty()));
        // (> 0) & (< 10) implies (< 20)
        assert!(gt(0.0).meet(&lt(10.0)).implies(&lt(20.0)));
    }

    #[test]
    fn open_closed_at_same_constant() {
        let open = Interval::greater_than(3.0);
        let closed = Interval::at_least(3.0);
        assert!(open.implies(&closed));
        assert!(!closed.implies(&open));
        assert_eq!(open.meet(&closed), open);
    }

    #[test]
    fn range_properties_share_the_interval_model() {
        let len_pos = RefinementSet::single(&Refinement::Range(Measure::Length, Interval::greater_than(0.0)));
        // a point length implies a range that contains it, and only that
        assert!(len(5).implies(&len_pos));
        assert!(!len(0).implies(&len_pos));
        assert!(len(0).contradicts(&len_pos, true));
        assert!(!len_pos.implies(&len(5)));
        // `length(> 0)` and `length(< 1)` leave no integer length
        let none = len_pos.meet(&RefinementSet::single(&Refinement::Range(Measure::Length, Interval::less_than(1.0))));
        assert!(none.is_empty(true));
        assert_eq!(len_pos.to_string(), "length(> 0)");
        let ge = RefinementSet::single(&Refinement::Range(Measure::Value, Interval::at_least(0.0)));
        assert_eq!(ge.to_string(), "(>= 0)");
        assert!(gt(0.0).implies(&ge) && !ge.implies(&gt(0.0)));
    }

    #[test]
    fn negative_zero_is_canonical() {
        assert_eq!(Num::new(-0.0), Num::new(0.0));
        assert_eq!(gt(-0.0), gt(0.0));
    }

    #[test]
    fn display_uses_surface_syntax() {
        assert_eq!(len(5).to_string(), "length(5)");
        assert_eq!(gt(0.0).to_string(), "(> 0)");
        assert_eq!(gt(0.0).meet(&lt(10.0)).to_string(), "(> 0) & (< 10)");
    }
}

#[cfg(test)]
mod type_tests {
    use super::*;
    use crate::components::error_message::help_data::HelpData;
    use crate::components::r#type::tint::Tint;
    use crate::components::r#type::type_system::TypeSystem;
    use crate::components::r#type::vector_type::VecType;
    use crate::components::r#type::Type;
    use crate::utils::builder;
    use std::collections::HashSet;

    fn refined(base: Type, rs: RefinementSet) -> Type {
        Type::Refined(Box::new(base), rs, HelpData::default())
    }
    fn int_lit(n: i32) -> Type {
        Type::Integer(Tint::Val(n), HelpData::default())
    }
    fn vec_of_len(n: Option<i32>) -> Type {
        let index = match n {
            Some(n) => int_lit(n),
            None => builder::integer_type_default(),
        };
        Type::vec(VecType::S3, index, builder::integer_type_default(), HelpData::default())
    }

    #[test]
    fn refined_equality_and_hash_ignore_construction_order() {
        let a = refined(
            builder::integer_type_default(),
            RefinementSet::single(&Refinement::Gt(Num::new(0.0))).meet(&RefinementSet::single(&Refinement::Lt(Num::new(9.0)))),
        );
        let b = refined(
            builder::integer_type_default(),
            RefinementSet::single(&Refinement::Lt(Num::new(9.0))).meet(&RefinementSet::single(&Refinement::Gt(Num::new(0.0)))),
        );
        assert_eq!(a, b);
        let set: HashSet<Type> = [a, b].into_iter().collect();
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn refinements_are_read_back_from_structure() {
        assert_eq!(vec_of_len(Some(5)).refinements_of(), RefinementSet::single(&Refinement::Length(5)));
        assert!(vec_of_len(None).refinements_of().is_trivial());
        assert_eq!(
            int_lit(3).refinements_of().get(Measure::Value),
            Some(&Interval::point(3.0))
        );
        let r = refined(vec_of_len(Some(2)), RefinementSet::single(&Refinement::Gt(Num::new(0.0))));
        let got = r.refinements_of();
        assert_eq!(got.get(Measure::Length), Some(&Interval::point(2.0)));
        assert_eq!(got.get(Measure::Value), Some(&Interval::greater_than(0.0)));
    }

    #[test]
    fn refined_prints_and_dispatches_like_its_base() {
        let r = refined(
            builder::integer_type_default(),
            RefinementSet::single(&Refinement::Gt(Num::new(0.0))),
        );
        assert_eq!(r.pretty(), "int & (> 0)");
        assert_eq!(r.to_category(), builder::integer_type_default().to_category());
        assert_eq!(r.unrefined(), &builder::integer_type_default());
    }
}
