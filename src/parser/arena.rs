//! Parser AST arena contracts.
//!
//! Phase 3 provides deterministic node allocation accounting with explicit
//! limit failures for parser-side AST builders.

/// Opaque AST node identifier for arena allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AstNodeId(usize);

impl AstNodeId {
    /// Creates a node id from a raw index.
    pub const fn new(index: usize) -> Self {
        Self(index)
    }

    /// Returns the raw arena index.
    pub const fn index(self) -> usize {
        self.0
    }
}

/// Arena-level errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArenaError {
    /// Allocation would exceed configured node limit.
    NodeLimitExceeded { limit: usize, attempted: usize },
}

/// Lightweight node allocation counter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstArena {
    max_nodes: usize,
    allocated_nodes: usize,
}

impl AstArena {
    /// Creates an arena with a maximum node count.
    pub fn new(max_nodes: usize) -> Self {
        Self {
            max_nodes,
            allocated_nodes: 0,
        }
    }

    /// Attempts to reserve one node id.
    pub fn allocate(&mut self) -> Result<AstNodeId, ArenaError> {
        if self.allocated_nodes >= self.max_nodes {
            return Err(ArenaError::NodeLimitExceeded {
                limit: self.max_nodes,
                attempted: self.allocated_nodes.saturating_add(1),
            });
        }

        let id = AstNodeId::new(self.allocated_nodes);
        self.allocated_nodes += 1;
        Ok(id)
    }

    /// Returns configured max nodes.
    pub const fn max_nodes(&self) -> usize {
        self.max_nodes
    }

    /// Returns allocated node count.
    pub const fn allocated_nodes(&self) -> usize {
        self.allocated_nodes
    }

    /// Returns remaining node capacity.
    pub const fn remaining_capacity(&self) -> usize {
        self.max_nodes.saturating_sub(self.allocated_nodes)
    }

    /// Clears allocation counter.
    pub fn reset(&mut self) {
        self.allocated_nodes = 0;
    }
}
