/// Graph cyclicity and Directed Acyclic Graph (DAG) analysis.
///
/// This module provides efficient algorithms for detecting cycles in graphs,
/// which is fundamental for topological sorting, deadlock detection, and
/// validating graph properties.
///
/// ## Algorithms
///
/// | Problem | Algorithm | Function | Complexity |
/// |---------|-----------|----------|------------|
/// | Cycle detection (directed) | Kahn's algorithm | isAcyclic, isCyclic | O(V + E) |
/// | Cycle detection (undirected) | DFS | isAcyclic, isCyclic | O(V + E) |
///
/// ## Applications
///
/// - Dependency resolution: Detect circular dependencies in package managers
/// - Deadlock detection: Resource allocation graphs in operating systems
/// - Build systems: Detect circular dependencies in Makefiles
/// - Course prerequisites: Validate prerequisite chains aren't circular
module Yog.Properties.Cyclicity

open Yog.Model

/// Checks if the graph is a Directed Acyclic Graph (DAG) or has no cycles if undirected.
///
/// For directed graphs, a cycle exists if there is a path from a node back to itself.
/// For undirected graphs, a cycle exists if there is a path of length >= 3
/// from a node back to itself, or a self-loop.
///
/// Time Complexity: O(V + E)
let isAcyclic (graph: Graph<'n, 'e>) : bool = Yog.Traversal.isAcyclic graph

/// Checks if the graph contains at least one cycle.
///
/// The logical opposite of isAcyclic.
///
/// Time Complexity: O(V + E)
let isCyclic (graph: Graph<'n, 'e>) : bool = not (Yog.Traversal.isAcyclic graph)
