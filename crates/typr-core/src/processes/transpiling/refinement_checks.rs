//! Runtime side of refined types (`refined_types_plan.md`, D5/D6).
//!
//! The type checker decides, at each boundary, which refinements it could not
//! prove and records them as obligations in the `Context`
//! (`refinement_check::coerce_to`). This module only reads that table and
//! wraps the transpiled expression in the prelude helpers of `std.R`:
//!
//! ```r
//! a <- typr_refine_length(read_coordinates(), 2L, "main.ty:12")
//! ```
//!
//! The helpers return their argument, so the wrap works in any expression
//! position. Nothing is emitted for a boundary without an obligation.

use crate::components::context::Context;
use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::refinement::{Bound, Interval, Measure, RefinementSet};
use crate::processes::transpiling::escape_r_string;

/// Wrap `r_expr`, the transpilation of the expression at `expr_h`, in the
/// checks the type checker required there. `r_expr` is returned unchanged
/// when there is no obligation.
pub fn wrap_obligation(context: &Context, r_expr: String, expr_h: &HelpData) -> String {
    match context.refinement_obligation_at(expr_h) {
        Some(set) => wrap_set(r_expr, set, &format_loc(expr_h)),
        None => r_expr,
    }
}

pub fn wrap_set(r_expr: String, set: &RefinementSet, loc: &str) -> String {
    set.iter()
        .fold(r_expr, |acc, (measure, iv)| wrap_measure(acc, *measure, iv, loc))
}

fn wrap_measure(x: String, measure: Measure, iv: &Interval, loc: &str) -> String {
    let loc = escape_r_string(loc);
    match (measure, iv.as_point()) {
        (Measure::Length, Some(n)) => format!("typr_refine_length({}, {}L, {})", x, n as i64, loc),
        // Only point lengths can be written today; a range would need its own
        // helper, and dropping the check silently is not an option.
        (Measure::Length, None) => format!("typr_refine_length_range({}, {}, {})", x, bound_args(iv), loc),
        (Measure::Value, _) => format!("typr_refine_value({}, {}, {})", x, bound_args(iv), loc),
    }
}

/// `lo, lo_open, hi, hi_open` as R arguments (`-Inf`/`Inf` when unbounded).
fn bound_args(iv: &Interval) -> String {
    let (lo, lo_open) = match iv.lo() {
        Bound::Unbounded => ("-Inf".to_string(), false),
        Bound::Open(c) => (r_number(c.get()), true),
        Bound::Closed(c) => (r_number(c.get()), false),
    };
    let (hi, hi_open) = match iv.hi() {
        Bound::Unbounded => ("Inf".to_string(), false),
        Bound::Open(c) => (r_number(c.get()), true),
        Bound::Closed(c) => (r_number(c.get()), false),
    };
    format!(
        "{}, {}, {}, {}",
        lo,
        lo_open.to_string().to_uppercase(),
        hi,
        hi_open.to_string().to_uppercase()
    )
}

fn r_number(v: f64) -> String {
    if v.fract() == 0.0 && v.abs() < 1e9 {
        format!("{}", v as i64)
    } else {
        format!("{}", v)
    }
}

fn format_loc(loc: &HelpData) -> String {
    let file = loc.get_file_name();
    if file.is_empty() {
        return "<unknown>".to_string();
    }
    let line = loc.get_file_data().and_then(|(_, content)| {
        let offset = loc.get_offset().min(content.len());
        content.get(..offset).map(|prefix| prefix.matches('\n').count() + 1)
    });
    match line {
        Some(line) => format!("{}:{}", file, line),
        None => format!("{}:offset {}", file, loc.get_offset()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::r#type::refinement::Refinement;

    #[test]
    fn length_and_value_forms() {
        let set = RefinementSet::single(&Refinement::Length(3));
        assert_eq!(
            wrap_set("x".into(), &set, "m.ty:1"),
            "typr_refine_length(x, 3L, \"m.ty:1\")"
        );
        let set = RefinementSet::single(&Refinement::Gt(crate::components::r#type::refinement::Num::new(0.0)));
        assert_eq!(
            wrap_set("x".into(), &set, "m.ty:1"),
            "typr_refine_value(x, 0, TRUE, Inf, FALSE, \"m.ty:1\")"
        );
    }
}
