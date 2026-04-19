/// Utility types for pathfinding algorithms.
/// 
/// Provides the `Path<'e>` type which represents a computed path through a graph
/// along with its total weight/cost.
module Yog.Pathfinding.Utils

open Yog.Model

/// Represents a computed path through a graph along with its total weight.
/// 
/// ## Type Parameters
/// - `'e`: The edge weight type
/// 
/// ## Fields
/// - `Nodes`: List of NodeIds from source to target (inclusive)
/// - `TotalWeight`: Sum of edge weights along the path
/// 
/// ## Example
/// 
///     // Path from node 0 to node 3 with total distance 42
///     let path = { Nodes = [0; 2; 3]; TotalWeight = 42 }
/// 
type Path<'e> =
    { /// Ordered list of node IDs from source to target (inclusive).
      Nodes: NodeId list
      /// Total weight/cost of traversing this path.
      TotalWeight: 'e }
