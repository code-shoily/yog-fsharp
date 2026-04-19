/// Core graph data structures and basic operations for the yog library.
///
/// This module defines the fundamental `Graph` type and provides all basic operations
/// for creating and manipulating graphs. The graph uses an adjacency list representation
/// with dual indexing (both outgoing and incoming edges) for efficient traversal in both
/// directions.
module Yog.Model

/// Unique identifier for a node in the graph.
type NodeId = int

/// The type of graph: directed or undirected.
type GraphType =
    /// A directed graph where edges have a direction from source to destination.
    | Directed
    /// An undirected graph where edges are bidirectional.
    | Undirected

/// A simple graph data structure that can be directed or undirected.
/// - `'nodeData`: The type of data stored at each node
/// - `'edgeData`: The type of data (usually weight) stored on each edge
type Graph<'nodeData, 'edgeData> =
    { Kind: GraphType
      Nodes: Map<NodeId, 'nodeData>
      OutEdges: Map<NodeId, Map<NodeId, 'edgeData>>
      InEdges: Map<NodeId, Map<NodeId, 'edgeData>> }

/// Creates a new empty graph of the specified type.
///
/// ## Example
///
///     let graph = Model.empty Directed
///
/// ## Parameters
/// - `graphType`: The type of the graph.
/// ## Returns
/// A new empty graph.
let empty graphType : Graph<'n, 'e> =
    { Kind = graphType
      Nodes = Map.empty
      OutEdges = Map.empty
      InEdges = Map.empty }

/// Adds a node to the graph with the given ID and data.
/// If a node with this ID already exists, its data will be replaced.
///
/// ## Example
///
///     graph
///     |> addNode 1 "Node A"
///     |> addNode 2 "Node B"
///
/// ## Parameters
/// - `id`: The unique identifier for the node.
/// - `data`: The data to store at the node.
/// - `graph`: The graph to add the node to.
/// ## Returns
/// A new graph with the node added.
let addNode (id: NodeId) (data: 'n) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    { graph with
        Nodes = graph.Nodes |> Map.add id data }

/// Helper function to add a directed edge using Map.change
let private doAddDirectedEdge src dst weight (graph: Graph<'n, 'e>) =
    let addOrUpdate key mapOpt =
        match mapOpt with
        | Some m -> Some(Map.add key weight m)
        | None -> Some(Map.ofList [ (key, weight) ])

    { graph with
        OutEdges = graph.OutEdges |> Map.change src (addOrUpdate dst)
        InEdges = graph.InEdges |> Map.change dst (addOrUpdate src) }

/// Adds an edge to the graph with the given weight.
///
/// For directed graphs, adds a single edge from src to dst.
/// For undirected graphs, adds edges in both directions.
///
/// ## Example
///
///     graph
///     |> addEdge 1 2 10
///
/// **Note:** If `src` or `dst` have not been added via `addNode`,
/// an `ArgumentException` is thrown. To automatically create missing
/// endpoints when adding an edge, use `addEdgeEnsured` or
/// `addEdgeEnsuredWith`.
/// ## Parameters
/// - `src`: The source node ID.
/// - `dst`: The destination node ID.
/// - `weight`: The weight of the edge.
/// - `graph`: The graph to add the edge to.
/// ## Returns
/// A new graph with the edge added.
let addEdge (src: NodeId) (dst: NodeId) (weight: 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    if not (graph.Nodes |> Map.containsKey src) then
        invalidArg "src" (sprintf "Source node %d does not exist in the graph." src)

    if not (graph.Nodes |> Map.containsKey dst) then
        invalidArg "dst" (sprintf "Destination node %d does not exist in the graph." dst)

    let g = doAddDirectedEdge src dst weight graph

    match g.Kind with
    | Directed -> g
    | Undirected -> doAddDirectedEdge dst src weight g

/// Adds a node only if it doesn't already exist.
let private ensureNode id data (graph: Graph<'n, 'e>) =
    if graph.Nodes |> Map.containsKey id then
        graph
    else
        addNode id data graph

/// Like `addEdge`, but ensures both endpoint nodes exist first.
///
/// If `src` or `dst` is not already in the graph, it is created with
/// the supplied default data before the edge is added. Nodes
/// that already exist are left unchanged.
///
/// ## Example
///
///     // Nodes 1 and 2 are created automatically with data "unknown"
///     empty Directed
///     |> addEdgeEnsured 1 2 10 "unknown" "unknown"
///
///     // Existing nodes keep their data; only missing ones get the default
///     empty Directed
///     |> addNode 1 "Alice"
///     |> addEdgeEnsured 1 2 5 "anon" "anon"
///     // Node 1 is still "Alice", node 2 is "anon"
///
/// ## Parameters
/// - `src`: The source node ID.
/// - `dst`: The destination node ID.
/// - `weight`: The weight of the edge.
/// - `srcDefault`: The default data for the source node if it doesn't exist.
/// - `dstDefault`: The default data for the destination node if it doesn't exist.
/// - `graph`: The graph to add the edge to.
/// ## Returns
/// A new graph with the edge added and endpoints ensured.
let addEdgeEnsured (src: NodeId) (dst: NodeId) (weight: 'e) (srcDefault: 'n) (dstDefault: 'n) (graph: Graph<'n, 'e>) =
    graph
    |> ensureNode src srcDefault
    |> ensureNode dst dstDefault
    |> addEdge src dst weight

/// Like `addEdge`, but ensures both endpoint nodes exist first by generating
/// default data via a callback.
/// ## Parameters
/// - `src`: The source node ID.
/// - `dst`: The destination node ID.
/// - `weight`: The weight of the edge.
/// - `createDefault`: A callback that takes a NodeId and returns the default data for that node.
/// - `graph`: The graph to add the edge to.
/// ## Returns
/// A new graph with the edge added and endpoints ensured.
let addEdgeEnsuredWith (src: NodeId) (dst: NodeId) (weight: 'e) (createDefault: NodeId -> 'n) (graph: Graph<'n, 'e>) =
    let ensureNodeWith id (g: Graph<'n, 'e>) =
        if g.Nodes |> Map.containsKey id then
            g
        else
            addNode id (createDefault id) g

    graph |> ensureNodeWith src |> ensureNodeWith dst |> addEdge src dst weight

/// Gets nodes you can travel TO (Successors).
/// ## Parameters
/// - `id`: The node ID to get successors for.
/// - `graph`: The graph.
/// ## Returns
/// A list of successor node IDs and edge weights.
let successors (id: NodeId) (graph: Graph<'n, 'e>) : list<NodeId * 'e> =
    graph.OutEdges
    |> Map.tryFind id
    |> Option.map Map.toList
    |> Option.defaultValue []

/// Gets nodes you came FROM (Predecessors).
/// ## Parameters
/// - `id`: The node ID to get predecessors for.
/// - `graph`: The graph.
/// ## Returns
/// A list of predecessor node IDs and edge weights.
let predecessors (id: NodeId) (graph: Graph<'n, 'e>) : list<NodeId * 'e> =
    graph.InEdges
    |> Map.tryFind id
    |> Option.map Map.toList
    |> Option.defaultValue []

/// Returns just the NodeIds of predecessors (without edge weights).
/// ## Parameters
/// - `node`: The node ID to get predecessors for.
/// - `graph`: The graph.
/// ## Returns
/// A list of predecessor node IDs.
let predecessorIds (node: NodeId) (graph: Graph<'n, 'e>) : NodeId list = predecessors node graph |> List.map fst

/// Gets everyone connected to the node, regardless of direction.
///
/// For undirected graphs, this is equivalent to `successors`.
/// For directed graphs, this combines both outgoing and incoming edges
/// without duplicates.
/// ## Parameters
/// - `id`: The node ID to get neighbors for.
/// - `graph`: The graph.
/// ## Returns
/// A list of connected node IDs and edge weights.
let neighbors (id: NodeId) (graph: Graph<'n, 'e>) : list<NodeId * 'e> =
    match graph.Kind with
    | Undirected -> successors id graph
    | Directed ->
        let outgoing = successors id graph
        let incoming = predecessors id graph
        let outIds = outgoing |> List.map fst |> Set.ofList

        (outgoing, incoming)
        ||> List.fold (fun acc ((inId, _) as inc) -> if outIds |> Set.contains inId then acc else inc :: acc)

/// Returns all node IDs in the graph.
/// This includes all nodes, even isolated nodes with no edges.
/// ## Parameters
/// - `graph`: The graph.
/// ## Returns
/// A list of all node IDs.
let allNodes (graph: Graph<'n, 'e>) : list<NodeId> =
    graph.Nodes |> Map.toList |> List.map fst

/// Returns the number of nodes in the graph (graph order).
/// **Time Complexity:** O(1)
/// ## Parameters
/// - `graph`: The graph.
/// ## Returns
/// The number of nodes.
let order (graph: Graph<'n, 'e>) : int = graph.Nodes.Count

/// Returns the number of nodes in the graph.
/// Equivalent to `order`.
/// **Time Complexity:** O(1)
let nodeCount = order

/// Returns the number of edges in the graph.
///
/// For undirected graphs, each edge is counted once (the pair {u, v}).
/// For directed graphs, each directed edge (u -> v) is counted once.
/// **Time Complexity:** O(V)
/// ## Parameters
/// - `graph`: The graph.
/// ## Returns
/// The number of edges.
let edgeCount (graph: Graph<'n, 'e>) : int =
    let count = graph.OutEdges |> Map.fold (fun acc _ targets -> acc + targets.Count) 0

    match graph.Kind with
    | Directed -> count
    | Undirected -> count / 2

/// Returns just the NodeIds of successors (without edge weights).
/// Convenient for traversal algorithms that only need the IDs.
/// ## Parameters
/// - `id`: The node ID to get successors for.
/// - `graph`: The graph.
/// ## Returns
/// A list of successor node IDs.
let successorIds (id: NodeId) (graph: Graph<'n, 'e>) : list<NodeId> = successors id graph |> List.map fst

/// Removes a node and all its connected edges (incoming and outgoing).
///
/// ## Example
///
///     let graph =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addNode 3 "C"
///       |> addEdge 1 2 10
///       |> addEdge 2 3 20
///
///     let graph = removeNode 2 graph
///     // Node 2 is removed, along with edges 1->2 and 2->3
///
/// **Time Complexity:** O(deg(v)) - proportional to the number of edges
/// connected to the node, not the whole graph.
/// ## Parameters
/// - `id`: The ID of the node to remove.
/// - `graph`: The graph to remove the node from.
/// ## Returns
/// A new graph with the node and its connections removed.
let removeNode (id: NodeId) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let targets = successors id graph |> List.map fst
    let sources = predecessors id graph |> List.map fst

    let newNodes = graph.Nodes |> Map.remove id
    let newOut = graph.OutEdges |> Map.remove id
    let newIn = graph.InEdges |> Map.remove id

    // Clean up incoming references to this node (removes the key entirely if map is empty)
    let newInCleaned =
        targets
        |> List.fold
            (fun acc targetId ->
                acc
                |> Map.change targetId (Option.map (Map.remove id) >> Option.filter (fun m -> not m.IsEmpty)))
            newIn

    // Clean up outgoing references to this node
    let newOutCleaned =
        sources
        |> List.fold
            (fun acc sourceId ->
                acc
                |> Map.change sourceId (Option.map (Map.remove id) >> Option.filter (fun m -> not m.IsEmpty)))
            newOut

    { graph with
        Nodes = newNodes
        OutEdges = newOutCleaned
        InEdges = newInCleaned }

/// Helper for removing directed edge
let private doRemoveDirectedEdge src dst (graph: Graph<'n, 'e>) =
    let newOut =
        graph.OutEdges
        |> Map.change src (Option.map (Map.remove dst) >> Option.filter (fun m -> not m.IsEmpty))

    let newIn =
        graph.InEdges
        |> Map.change dst (Option.map (Map.remove src) >> Option.filter (fun m -> not m.IsEmpty))

    { graph with
        OutEdges = newOut
        InEdges = newIn }

/// Removes an edge from src to dst.
/// For undirected graphs, removes edges in both directions.
///
/// ## Example
///
///     // Directed graph - removes single directed edge
///     let graph =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addEdge 1 2 10
///       |> removeEdge 1 2
///     // Edge 1->2 is removed
///
/// **Time Complexity:** O(1)
/// ## Parameters
/// - `src`: The source node ID.
/// - `dst`: The destination node ID.
/// - `graph`: The graph to remove the edge from.
/// ## Returns
/// A new graph with the edge removed.
let removeEdge (src: NodeId) (dst: NodeId) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let g = doRemoveDirectedEdge src dst graph

    match g.Kind with
    | Directed -> g
    | Undirected -> doRemoveDirectedEdge dst src g

/// Helper for adding edge with combine function
let private doAddDirectedCombine src dst weight combine (graph: Graph<'n, 'e>) =
    let addOrCombine key mapOpt =
        match mapOpt with
        | Some m ->
            let newWeight =
                match Map.tryFind key m with
                | Some existing -> combine existing weight
                | None -> weight

            Some(Map.add key newWeight m)
        | None -> Some(Map.ofList [ (key, weight) ])

    { graph with
        OutEdges = graph.OutEdges |> Map.change src (addOrCombine dst)
        InEdges = graph.InEdges |> Map.change dst (addOrCombine src) }

/// Adds an edge, but if an edge already exists between `src` and `dst`,
/// it combines the new weight with the existing one using `combine`.
///
/// The combine function receives `(existingWeight, newWeight)` and should
/// return the combined weight.
///
/// ## Example
///
///     let graph =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addEdge 1 2 10
///       |> addEdgeWithCombine 1 2 5 (+)
///     // Edge 1->2 now has weight 15 (10 + 5)
///
/// **Time Complexity:** O(1)
///
/// <h2>Use Cases</h2>
/// <ul>
/// <li>**Edge contraction** in graph algorithms (Stoer-Wagner min-cut)</li>
/// <li>**Multi-graph support** (adding parallel edges with combined weights)</li>
/// <li>**Incremental graph building** (accumulating weights from multiple sources)</li>
/// </ul>
/// ## Parameters
/// - `src`: The source node ID.
/// - `dst`: The destination node ID.
/// - `weight`: The weight of the edge.
/// - `combine`: A function to combine existing and new weights.
/// - `graph`: The graph to add the edge to.
/// ## Returns
/// A new graph with the edge added or weights combined.
let addEdgeWithCombine (src: NodeId) (dst: NodeId) (weight: 'e) (combine: 'e -> 'e -> 'e) (graph: Graph<'n, 'e>) =
    if not (graph.Nodes |> Map.containsKey src) then
        invalidArg "src" (sprintf "Source node %d does not exist in the graph." src)

    if not (graph.Nodes |> Map.containsKey dst) then
        invalidArg "dst" (sprintf "Destination node %d does not exist in the graph." dst)

    let g = doAddDirectedCombine src dst weight combine graph

    match g.Kind with
    | Directed -> g
    | Undirected -> doAddDirectedCombine dst src weight combine g
