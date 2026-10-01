//! Lexical scope tracking during the `Lang` walk (spec §3.3, §7.1 step 2). A "frame" is pushed
//! for every name-binding position (the `Program` root, each `Function`); a name found in the
//! current boundary's own frames is local, one found further out is a capture crossing that
//! boundary.
//!
//! Simplification (documented, not yet needed by any real case): a capture only ever attaches to
//! the *nearest* enclosing boundary, even when the definition lives further out still (e.g. a
//! `Function` nested inside another `Function`, itself referencing something from `Program`).
//! Full bubbling through every crossed boundary (spec §3.3's "les dépendances remontent
//! naturellement") is deferred until a real nested case needs it.

use crate::model::PortRef;
use std::collections::HashMap;

use crate::key::BlockKey;

struct Frame {
    bindings: HashMap<String, PortRef>,
    /// `Some(key)` if this frame is a capture boundary (the block `key` owns captures crossing
    /// into it); `None` for a plain nested scope (e.g. an `if` branch) that shares its parent's
    /// boundary.
    boundary: Option<BlockKey>,
}

#[derive(Default)]
pub struct ScopeStack {
    frames: Vec<Frame>,
}

impl ScopeStack {
    pub fn new() -> Self {
        ScopeStack { frames: Vec::new() }
    }

    pub fn push_boundary(&mut self, key: BlockKey) {
        self.frames.push(Frame {
            bindings: HashMap::new(),
            boundary: Some(key),
        });
    }

    pub fn push_plain(&mut self) {
        self.frames.push(Frame {
            bindings: HashMap::new(),
            boundary: None,
        });
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn bind(&mut self, name: &str, port: PortRef) {
        if let Some(frame) = self.frames.last_mut() {
            frame.bindings.insert(name.to_string(), port);
        }
    }

    fn nearest_boundary_index(&self) -> Option<usize> {
        self.frames.iter().rposition(|f| f.boundary.is_some())
    }

    pub fn nearest_boundary(&self) -> Option<BlockKey> {
        self.frames.iter().rev().find_map(|f| f.boundary.clone())
    }

    /// Looks a name up from the innermost frame outward. Returns the bound `PortRef` and whether
    /// it was found without crossing the nearest boundary (`true`) or beyond it (`false`, a
    /// capture).
    pub fn resolve(&self, name: &str) -> Option<(PortRef, bool)> {
        let boundary_idx = self.nearest_boundary_index();
        for (idx, frame) in self.frames.iter().enumerate().rev() {
            if let Some(port) = frame.bindings.get(name) {
                let is_local = boundary_idx.is_none_or(|b| idx >= b);
                return Some((port.clone(), is_local));
            }
        }
        None
    }
}
