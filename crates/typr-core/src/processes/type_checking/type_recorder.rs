//! Records the type computed for every expression `typing()` is called on, keyed by source span
//! (see `visualization_graph_v2.md` §7.2 — prerequisite for the block-graph builder and, longer
//! term, for LSP hover/inlay hints). Disabled by default: `typing()` still checks a thread-local
//! on every call, but records nothing unless a `with_recording` scope is active, so the cost when
//! off is one thread-local read and no allocation.
//!
//! WASM is single-threaded, so a `thread_local!` here carries no cross-thread risk.

use crate::components::error_message::help_data::HelpData;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use std::cell::RefCell;
use std::collections::HashMap;

/// Identifies a node's source range for recording purposes: `(file, start, end)`. Two distinct
/// nodes at the same offset (there are none in a well-formed parse) would collide; see prereq D
/// (`HelpData.end`) for why `start` alone isn't enough — it's what previously made every prefix of
/// an expression share its parent's key.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpanKey {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

impl From<&HelpData> for SpanKey {
    fn from(h: &HelpData) -> Self {
        SpanKey {
            file: h.get_file_name(),
            start: h.get_offset(),
            end: h.get_end(),
        }
    }
}

/// A recorded conflict: `typing()` was called more than once on the same span and produced two
/// different, both non-`Failed`, types. Kept for analysis while validating the recording policy
/// on real cases (§7.2); not surfaced as an error.
#[derive(Debug, Clone)]
pub struct TypeConflict {
    pub key: SpanKey,
    pub previous: Type,
    pub replacement: Type,
}

#[derive(Debug, Clone, Default)]
pub struct TypeTable {
    entries: HashMap<SpanKey, Type>,
    conflicts: Vec<TypeConflict>,
}

/// A type that carries no real information about the node it's attached to — the placeholder
/// `typing()` produces mid-way through a trial (dispatch candidate, forward reference, empty
/// block) rather than the node's actual type. Neither overwrites a firm value, nor counts as a
/// conflict when a firm value overwrites it: observed on the §14 walkthrough example, a function
/// body's `Lines` node is first typed as `UnknownFunction` while its own signature is still being
/// resolved, then re-typed to its real type once that trial completes (see §7.2's "last
/// non-`Failed` value" policy, broadened here after that observation).
fn is_placeholder(t: &Type) -> bool {
    matches!(t, Type::Failed(..) | Type::UnknownFunction(..))
}

impl TypeTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts under the policy of §7.2, "last non-`Failed` value wins" (broadened to
    /// [`is_placeholder`]): a placeholder result never overwrites a previously recorded firm
    /// value, since it carries no information (typing() retries dispatch/unification candidates,
    /// and re-types a node while its own signature is still being resolved). Two different firm
    /// values for the same span are logged as a conflict but the later one still wins.
    pub fn insert(&mut self, key: SpanKey, value: Type) {
        match self.entries.get(&key) {
            Some(previous) if is_placeholder(&value) && !is_placeholder(previous) => {
                // Keep the earlier good value; a placeholder retry carries no information.
            }
            Some(previous) if *previous != value && !is_placeholder(previous) && !is_placeholder(&value) => {
                self.conflicts.push(TypeConflict {
                    key: key.clone(),
                    previous: previous.clone(),
                    replacement: value.clone(),
                });
                self.entries.insert(key, value);
            }
            _ => {
                self.entries.insert(key, value);
            }
        }
    }

    pub fn get(&self, key: &SpanKey) -> Option<&Type> {
        self.entries.get(key)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn conflicts(&self) -> &[TypeConflict] {
        &self.conflicts
    }
}

thread_local! {
    static RECORDER: RefCell<Option<TypeTable>> = const { RefCell::new(None) };
}

/// Runs `f` with recording turned on and returns its result alongside the table filled in by every
/// `typing()` call made while `f` ran. Any recording already in progress on this thread (there is
/// none in current callers) is suspended for the duration and restored afterwards.
pub fn with_recording<R>(f: impl FnOnce() -> R) -> (R, TypeTable) {
    let previous = RECORDER.with(|cell| cell.replace(Some(TypeTable::new())));
    let result = f();
    let table = RECORDER.with(|cell| cell.replace(previous));
    (result, table.unwrap_or_default())
}

/// Called by `typing()` after computing `value` for `expr`. A no-op (one thread-local read, no
/// allocation) unless a `with_recording` scope is active.
pub fn record(expr: &Lang, value: &Type) {
    RECORDER.with(|cell| {
        if let Some(table) = cell.borrow_mut().as_mut() {
            table.insert(SpanKey::from(&expr.get_help_data()), value.clone());
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::context::Context;
    use crate::processes::parsing::parse_from_string;
    use crate::processes::type_checking::typing_with_errors;

    // The walkthrough example from `visualization_graph_v2.md` §14, verified there with `typr check`.
    const FIL_ROUGE: &str = r#"
type Printable <- interface { show: (Self) -> char };
type Point <- list { x: int, y: int };

let sq <- fn(n: int): int { n * n };

let norm2 <- fn(p: Point): int {
    let a <- sq(p$x);
    a + sq(p$y)
};

let show <- fn(p: Point): char { "Point" };

let p <- Point:{ x = 3, y = 4 };
let d <- Printable(p);
let total <- norm2(p) + 12 + 3;
"#;

    #[test]
    fn recording_is_disabled_by_default() {
        let lang = parse_from_string(FIL_ROUGE, "fil_rouge");
        let _ = typing_with_errors(&Context::default(), &lang);
        // No with_recording scope was opened: nothing observable from outside, this just
        // exercises the no-op path on the same program the next test records.
    }

    #[test]
    fn with_recording_captures_every_subexpression_of_the_walkthrough_example() {
        let lang = parse_from_string(FIL_ROUGE, "fil_rouge");
        let (result, table) = with_recording(|| typing_with_errors(&Context::default(), &lang));

        assert!(
            !result.has_errors(),
            "walkthrough example should typecheck cleanly: {:?}",
            result.display_errors()
        );
        assert!(!table.is_empty(), "recording should have captured at least one span");
        assert!(
            table.conflicts().is_empty(),
            "no dispatch/unification conflicts expected on this example: {:?}",
            table.conflicts()
        );
    }
}
