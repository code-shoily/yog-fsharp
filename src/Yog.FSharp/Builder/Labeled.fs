namespace Yog.Builder

open Yog.Model

/// A persistent builder for graphs that use arbitrary labels instead of integer node IDs.
///
/// ## Type Parameters
/// - `'Label`: The type used to identify nodes (must support comparison)
/// - `'EdgeData`: The type stored on edges
///
/// ## Fields
/// - `Graph`: The underlying graph structure with internal IDs
/// - `LabelToId`: Bidirectional mapping from labels to integer IDs
/// - `NextId`: Next available internal node ID
type LabeledBuilder<'Label, 'EdgeData when 'Label: comparison> =
    {
        /// The underlying graph structure.
        Graph: Graph<'Label, 'EdgeData>
        /// Mapping from label to internal node ID.
        LabelToId: Map<'Label, NodeId>
        /// Next available node ID.
        NextId: NodeId
    }

/// Labeled graph builder for constructing graphs with custom node labels.
///
/// Provides a type-safe way to build graphs using arbitrary labels (strings, custom types, etc.)
/// instead of integer node IDs. The builder maintains an internal mapping from labels to IDs.
///
/// ## When to Use
/// - Working with domain-specific identifiers (URLs, names, UUIDs)
/// - Building graphs from external data with natural keys
/// - When node identity is meaningful outside the graph structure
/// - Incremental graph construction with unknown nodes
///
/// ## Comparison with Direct Graph Construction
/// | Aspect | LabeledBuilder | Direct Graph API |
/// |--------|---------------|------------------|
/// | Node reference | Labels ('L) | Integer IDs |
/// | Auto-create nodes | Yes | No |
/// | Lookup cost | Map lookup | Direct |
/// | Use case | Domain modeling | Algorithm implementation |
///
/// ## Example
///
///     open Yog.Builder
///
///     // Build a social graph with string labels
///     let socialGraph =
///         Labeled.directed<string, int>()
///         |> Labeled.addEdge "Alice" "Bob" 5
///         |> Labeled.addEdge "Bob" "Charlie" 3
///         |> Labeled.addEdge "Alice" "Charlie" 10
///         |> Labeled.toGraph
///
module Labeled =

    /// Creates a new empty labeled graph builder.
    ///
    /// ## Parameters
    /// - `kind`: Directed or Undirected
    ///
    /// ## Returns
    /// A fresh LabeledBuilder with no nodes or edges.
    let create kind : LabeledBuilder<'Label, 'EdgeData> =
        { Graph = empty kind
          LabelToId = Map.empty
          NextId = 0 }

    /// Convenience: Creates a directed labeled builder.
    ///
    /// ## Example
    ///
    ///     let builder = Labeled.directed<string, int>()
    ///
    let directed<'L, 'E when 'L: comparison> () : LabeledBuilder<'L, 'E> = create Directed

    /// Convenience: Creates an undirected labeled builder.
    ///
    /// ## Example
    ///
    ///     let builder = Labeled.undirected<string, int>()
    ///
    let undirected<'L, 'E when 'L: comparison> () : LabeledBuilder<'L, 'E> = create Undirected

    /// Idempotently ensures a node exists for a label.
    ///
    /// If the label already exists, returns the existing ID.
    /// If the label is new, creates a node and assigns a new ID.
    ///
    /// ## Returns
    /// Tuple of (updated builder, node ID)
    let ensureNode (label: 'Label) (builder: LabeledBuilder<'Label, 'E>) =
        match Map.tryFind label builder.LabelToId with
        | Some id -> builder, id
        | None ->
            let id = builder.NextId
            let newGraph = addNode id label builder.Graph
            let newMapping = Map.add label id builder.LabelToId

            { builder with
                Graph = newGraph
                LabelToId = newMapping
                NextId = id + 1 },
            id

    /// Explicitly adds a node by label.
    ///
    /// ## Parameters
    /// - `label`: The label for the new node
    /// - `builder`: The builder to modify
    ///
    /// ## Returns
    /// Updated builder with the node added (if new).
    let addNode (label: 'Label) (builder: LabeledBuilder<'Label, 'E>) =
        let b, _ = ensureNode label builder
        b

    /// Adds an edge between two labels. Automatically creates nodes if missing.
    ///
    /// ## Parameters
    /// - `fromLabel`: Source node label
    /// - `toLabel`: Target node label
    /// - `weight`: Edge weight/data
    /// - `builder`: The builder to modify
    ///
    /// ## Example
    ///
    ///     let builder =
    ///         Labeled.directed<string, int>()
    ///         |> Labeled.addEdge "A" "B" 10
    ///
    let addEdge (fromLabel: 'Label) (toLabel: 'Label) (weight: 'E) (builder: LabeledBuilder<'Label, 'E>) =
        let b1, srcId = ensureNode fromLabel builder
        let b2, dstId = ensureNode toLabel b1
        let newGraph = addEdge srcId dstId weight b2.Graph
        { b2 with Graph = newGraph }

    /// Adds a simple edge with weight 1 (for integer weighted graphs).
    ///
    /// ## Example
    ///
    ///     let builder =
    ///         Labeled.directed<string, int>()
    ///         |> Labeled.addSimpleEdge "A" "B"
    ///
    let addSimpleEdge (fromLabel: 'Label) (toLabel: 'Label) (builder: LabeledBuilder<'Label, int>) =
        addEdge fromLabel toLabel 1 builder

    /// Looks up the internal node ID for a given label.
    ///
    /// ## Returns
    /// `Some id` if the label exists, `None` otherwise.
    let getId (label: 'Label) (builder: LabeledBuilder<'Label, 'E>) = Map.tryFind label builder.LabelToId

    /// Converts the builder to a standard detached Graph.
    ///
    /// The returned graph uses internal integer IDs but preserves labels as node data.
    let toGraph (builder: LabeledBuilder<'Label, 'E>) = builder.Graph

    /// Creates a builder from a list of edge triples (source, target, weight).
    ///
    /// ## Example
    ///
    ///     let edges = [("A", "B", 1); ("B", "C", 2)]
    ///     let builder = Labeled.fromList Directed edges
    ///
    let fromList kind (edges: ('L * 'L * 'E) list) =
        edges |> List.fold (fun b (src, dst, w) -> addEdge src dst w b) (create kind)

    /// Returns all labels currently in the registry.
    let allLabels (builder: LabeledBuilder<'L, 'E>) =
        builder.LabelToId |> Map.toList |> List.map fst

    /// Internal helper: Maps (NodeId * 'E) list to ('L * 'E) list
    let private mapIdsToLabels (edges: (NodeId * 'E) list) (graph: Graph<'L, 'E>) =
        edges
        |> List.choose (fun (id, weight) ->
            match Map.tryFind id graph.Nodes with
            | Some label -> Some(label, weight)
            | None -> None)

    /// Gets successors using labels.
    ///
    /// ## Returns
    /// `Some list` of (label, weight) if the node exists, `None` otherwise.
    let successors (label: 'Label) (builder: LabeledBuilder<'Label, 'E>) =
        getId label builder
        |> Option.map (fun id -> successors id builder.Graph |> mapIdsToLabels <| builder.Graph)

    /// Gets predecessors using labels.
    ///
    /// ## Returns
    /// `Some list` of (label, weight) if the node exists, `None` otherwise.
    let predecessors (label: 'Label) (builder: LabeledBuilder<'Label, 'E>) =
        getId label builder
        |> Option.map (fun id -> predecessors id builder.Graph |> mapIdsToLabels <| builder.Graph)
