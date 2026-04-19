namespace Yog.Builder

open Yog.Model

/// Represents a pending transition to be applied during sync.
type Transition<'n, 'e> =
    /// Add a node with given ID and label.
    | AddNode of id: NodeId * label: 'n
    /// Add an edge between two node IDs with given weight.
    | AddEdge of fromId: NodeId * toId: NodeId * weight: 'e
    /// Remove an edge between two node IDs.
    | RemoveEdge of fromId: NodeId * toId: NodeId
    /// Remove a node by ID.
    | RemoveNode of id: NodeId

/// A Live Builder for incremental graph construction.
/// 
/// Tracks label-to-ID mappings and pending transitions.
/// Changes are applied via `sync` to a target graph.
/// 
/// ## Type Parameters
/// - `'n`: Node label type (must support comparison)
/// - `'e`: Edge weight type
type LiveBuilder<'n, 'e when 'n: comparison> =
    { /// Mapping from labels to internal node IDs.
      Registry: Map<'n, NodeId>
      /// Next available node ID.
      NextId: NodeId
      /// Queued transitions waiting to be applied.
      Pending: Transition<'n, 'e> list }

/// Live graph builder for incremental graph construction.
/// 
/// The Live builder queues changes (additions and removals) that can be applied
/// to a graph in batches via `sync`. This is useful for:
/// - Building graphs incrementally from streaming data
/// - Delaying expensive graph operations until necessary
/// - Tracking pending changes before commit
/// 
/// ## Comparison with LabeledBuilder
/// | Aspect | LiveBuilder | LabeledBuilder |
/// |--------|-------------|----------------|
/// | Changes | Queued/batched | Immediate |
/// | Sync required | Yes | No |
/// | Remove operations | Yes | No |
/// | Use case | Streaming/mutable | Static construction |
/// 
/// ## Example
/// 
///     open Yog.Builder
///     
///     // Queue multiple changes
///     let builder, graph =
///         Live.create<string, int>()
///         |> Live.addEdge "Alice" "Bob" 5
///         |> Live.addEdge "Bob" "Charlie" 3
///         |> Live.removeEdge "Alice" "Bob"
///         |> Live.sync (Yog.Model.empty Directed)
///     
///     // graph now contains only Bob -> Charlie
/// 
/// ## Transition Types
/// - `AddNode`: Create a new node with given ID and label
/// - `AddEdge`: Create an edge between node IDs
/// - `RemoveEdge`: Remove an edge between node IDs
/// - `RemoveNode`: Remove a node and all its edges
module Live =

    /// Creates a new empty live builder.
    /// 
    /// ## Example
    /// 
    ///     let builder = Live.create<string, int>()
    /// 
    let create<'n, 'e when 'n: comparison> () : LiveBuilder<'n, 'e> =
        { Registry = Map.empty
          NextId = 0
          Pending = [] }

    /// Internal: Gets or creates a node ID for a label, queuing an AddNode transition if new.
    let private ensureNode label builder =
        match Map.tryFind label builder.Registry with
        | Some id -> builder, id
        | None ->
            let id = builder.NextId
            let transition = AddNode(id, label)

            { builder with
                Registry = Map.add label id builder.Registry
                NextId = id + 1
                Pending = transition :: builder.Pending },
            id

    /// Queues an edge addition between two labels.
    /// 
    /// Creates nodes automatically if they don't exist.
    /// 
    /// ## Example
    /// 
    ///     let builder =
    ///         Live.create<string, int>()
    ///         |> Live.addEdge "A" "B" 10
    /// 
    let addEdge fromLabel toLabel weight builder =
        let b1, srcId = ensureNode fromLabel builder
        let b2, dstId = ensureNode toLabel b1
        let transition = AddEdge(srcId, dstId, weight)
        { b2 with Pending = transition :: b2.Pending }

    /// Queues a simple edge with weight 1.
    let addSimpleEdge fromLabel toLabel builder = addEdge fromLabel toLabel 1 builder

    /// Queues an edge removal.
    /// 
    /// Silently ignored if either label doesn't exist.
    let removeEdge fromLabel toLabel builder =
        match Map.tryFind fromLabel builder.Registry, Map.tryFind toLabel builder.Registry with
        | Some src, Some dst ->
            let transition = RemoveEdge(src, dst)
            { builder with Pending = transition :: builder.Pending }
        | _ -> builder

    /// Queues a node removal and removes it from the label registry.
    /// 
    /// Silently ignored if the label doesn't exist.
    let removeNode label builder =
        match Map.tryFind label builder.Registry with
        | Some id ->
            let transition = RemoveNode(id)

            { builder with
                Registry = Map.remove label builder.Registry
                Pending = transition :: builder.Pending }
        | None -> builder

    /// Synchronizes pending changes to the provided graph.
    /// 
    /// Returns the updated builder (with cleared queue) and the updated graph.
    /// 
    /// ## Parameters
    /// - `builder`: The LiveBuilder with pending changes
    /// - `graph`: The target graph to apply changes to
    /// 
    /// ## Returns
    /// Tuple of (builder with cleared pending, updated graph)
    /// 
    /// ## Example
    /// 
    ///     let builder, updatedGraph =
    ///         myBuilder
    ///         |> Live.sync existingGraph
    /// 
    let sync (builder: LiveBuilder<'n, 'e>) (graph: Graph<'n, 'e>) =
        if List.isEmpty builder.Pending then
            builder, graph
        else
            // Transitions are prepended, so reverse to maintain insertion order
            let transitions = List.rev builder.Pending

            let updatedGraph =
                (graph, transitions)
                ||> List.fold (fun g t ->
                    match t with
                    | AddNode (id, label) -> Yog.Model.addNode id label g
                    | AddEdge (src, dst, w) -> Yog.Model.addEdge src dst w g
                    | RemoveEdge (src, dst) -> Yog.Model.removeEdge src dst g
                    | RemoveNode (id) -> Yog.Model.removeNode id g)

            { builder with Pending = [] }, updatedGraph

    /// Discards all pending changes without applying them.
    /// 
    /// ## Example
    /// 
    ///     let cleanBuilder = Live.purgePending builder
    /// 
    let purgePending builder = { builder with Pending = [] }

    /// Looks up the NodeId for a label.
    /// 
    /// ## Returns
    /// `Some id` if the label is registered, `None` otherwise.
    let getId label builder = Map.tryFind label builder.Registry

    /// Returns all registered labels.
    let allLabels builder =
        builder.Registry |> Map.toList |> List.map fst

    /// Total count of pending changes.
    let pendingCount builder = List.length builder.Pending

    /// Migrate from a static LabeledBuilder to a LiveBuilder.
    /// 
    /// Preserves all labels and IDs but starts with an empty pending queue.
    /// 
    /// ## Example
    /// 
    ///     let labeled = Labeled.directed<string, int>() |> Labeled.addEdge "A" "B" 1
    ///     let live = Live.fromLabeled labeled
    /// 
    let fromLabeled (labeled: LabeledBuilder<'n, 'e>) : LiveBuilder<'n, 'e> =
        { Registry = labeled.LabelToId
          NextId = labeled.NextId
          Pending = [] }
