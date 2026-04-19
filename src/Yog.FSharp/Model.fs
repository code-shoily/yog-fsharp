/// <summary>
/// Core graph data structures and basic operations for the yog library.
///
/// This module defines the fundamental <c>Graph</c> type and provides all basic operations
/// for creating and manipulating graphs. The graph uses an adjacency list representation
/// with dual indexing (both outgoing and incoming edges) for efficient traversal in both
/// directions.
/// </summary>
module Yog.Model

/// <summary>
/// Unique identifier for a node in the graph.
/// </summary>
type NodeId = int

/// <summary>
/// The type of graph: directed or undirected.
/// </summary>
type GraphType =
    /// <summary>
    /// A directed graph where edges have a direction from source to destination.
    /// </summary>
    | Directed
    /// <summary>
    /// An undirected graph where edges are bidirectional.
    /// </summary>
    | Undirected

/// <summary>
/// A simple graph data structure that can be directed or undirected.
/// </summary>
/// <typeparam name="'nodeData">The type of data stored at each node</typeparam>
/// <typeparam name="'edgeData">The type of data (usually weight) stored on each edge</typeparam>
type Graph<'nodeData, 'edgeData> =
    { Kind: GraphType
      Nodes: Map<NodeId, 'nodeData>
      OutEdges: Map<NodeId, Map<NodeId, 'edgeData>>
      InEdges: Map<NodeId, Map<NodeId, 'edgeData>> }

/// <summary>
/// Creates a new empty graph of the specified type.
///
/// **Example:**
///
///     let graph = Model.empty Directed
///
/// </summary>
/// <param name="graphType">The type of the graph.</param>
/// <returns>A new empty graph.</returns>
let empty graphType : Graph<'n, 'e> =
    { Kind = graphType
      Nodes = Map.empty
      OutEdges = Map.empty
      InEdges = Map.empty }

/// <summary>
/// Adds a node to the graph with the given ID and data.
/// If a node with this ID already exists, its data will be replaced.
///
/// **Example:**
///
///     graph
///     |> addNode 1 "Node A"
///     |> addNode 2 "Node B"
///
/// </summary>
/// <param name="id">The unique identifier for the node.</param>
/// <param name="data">The data to store at the node.</param>
/// <param name="graph">The graph to add the node to.</param>
/// <returns>A new graph with the node added.</returns>
let addNode (id: NodeId) (data: 'n) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    { graph with Nodes = graph.Nodes |> Map.add id data }

/// <summary>
/// Helper function to add a directed edge using Map.change
/// </summary>
let private doAddDirectedEdge src dst weight (graph: Graph<'n, 'e>) =
    let addOrUpdate key mapOpt =
        match mapOpt with
        | Some m -> Some(Map.add key weight m)
        | None -> Some(Map.ofList [ (key, weight) ])

    { graph with
        OutEdges = graph.OutEdges |> Map.change src (addOrUpdate dst)
        InEdges = graph.InEdges |> Map.change dst (addOrUpdate src) }

/// <summary>
/// Adds an edge to the graph with the given weight.
///
/// For directed graphs, adds a single edge from src to dst.
/// For undirected graphs, adds edges in both directions.
///
/// **Example:**
///
///     graph
///     |> addEdge 1 2 10
///
/// </summary>
/// <remarks>
/// <strong>Note:</strong> If <c>src</c> or <c>dst</c> have not been added via <c>addNode</c>,
/// an <c>ArgumentException</c> is thrown. To automatically create missing
/// endpoints when adding an edge, use <c>addEdgeEnsured</c> or
/// <c>addEdgeEnsuredWith</c>.
/// </remarks>
/// <param name="src">The source node ID.</param>
/// <param name="dst">The destination node ID.</param>
/// <param name="weight">The weight of the edge.</param>
/// <param name="graph">The graph to add the edge to.</param>
/// <returns>A new graph with the edge added.</returns>
let addEdge (src: NodeId) (dst: NodeId) (weight: 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    if not (graph.Nodes |> Map.containsKey src) then
        invalidArg "src" (sprintf "Source node %d does not exist in the graph." src)
    if not (graph.Nodes |> Map.containsKey dst) then
        invalidArg "dst" (sprintf "Destination node %d does not exist in the graph." dst)

    let g = doAddDirectedEdge src dst weight graph

    match g.Kind with
    | Directed -> g
    | Undirected -> doAddDirectedEdge dst src weight g

/// <summary>
/// Adds a node only if it doesn't already exist.
/// </summary>
let private ensureNode id data (graph: Graph<'n, 'e>) =
    if graph.Nodes |> Map.containsKey id then
        graph
    else
        addNode id data graph

/// <summary>
/// Like <c>addEdge</c>, but ensures both endpoint nodes exist first.
///
/// If <c>src</c> or <c>dst</c> is not already in the graph, it is created with
/// the supplied default data before the edge is added. Nodes
/// that already exist are left unchanged.
///
/// **Example:**
///
///     // Nodes 1 and 2 are created automatically with data "unknown"
///     empty Directed
///     |> addEdgeEnsured 1 2 10 "unknown" "unknown"
///
///
///     // Existing nodes keep their data; only missing ones get the default
///     empty Directed
///     |> addNode 1 "Alice"
///     |> addEdgeEnsured 1 2 5 "anon" "anon"
///     // Node 1 is still "Alice", node 2 is "anon"
///
/// </summary>
/// <param name="src">The source node ID.</param>
/// <param name="dst">The destination node ID.</param>
/// <param name="weight">The weight of the edge.</param>
/// <param name="srcDefault">The default data for the source node if it doesn't exist.</param>
/// <param name="dstDefault">The default data for the destination node if it doesn't exist.</param>
/// <param name="graph">The graph to add the edge to.</param>
/// <returns>A new graph with the edge added and endpoints ensured.</returns>
let addEdgeEnsured (src: NodeId) (dst: NodeId) (weight: 'e) (srcDefault: 'n) (dstDefault: 'n) (graph: Graph<'n, 'e>) =
    graph
    |> ensureNode src srcDefault
    |> ensureNode dst dstDefault
    |> addEdge src dst weight

/// <summary>
/// Like <c>addEdge</c>, but ensures both endpoint nodes exist first by generating
/// default data via a callback.
/// </summary>
/// <param name="src">The source node ID.</param>
/// <param name="dst">The destination node ID.</param>
/// <param name="weight">The weight of the edge.</param>
/// <param name="createDefault">A callback that takes a NodeId and returns the default data for that node.</param>
/// <param name="graph">The graph to add the edge to.</param>
/// <returns>A new graph with the edge added and endpoints ensured.</returns>
let addEdgeEnsuredWith (src: NodeId) (dst: NodeId) (weight: 'e) (createDefault: NodeId -> 'n) (graph: Graph<'n, 'e>) =
    let ensureNodeWith id (g: Graph<'n, 'e>) =
        if g.Nodes |> Map.containsKey id then
            g
        else
            addNode id (createDefault id) g

    graph
    |> ensureNodeWith src
    |> ensureNodeWith dst
    |> addEdge src dst weight

/// <summary>
/// Gets nodes you can travel TO (Successors).
/// </summary>
/// <param name="id">The node ID to get successors for.</param>
/// <param name="graph">The graph.</param>
/// <returns>A list of successor node IDs and edge weights.</returns>
let successors (id: NodeId) (graph: Graph<'n, 'e>) : list<NodeId * 'e> =
    graph.OutEdges
    |> Map.tryFind id
    |> Option.map Map.toList
    |> Option.defaultValue []

/// <summary>
/// Gets nodes you came FROM (Predecessors).
/// </summary>
/// <param name="id">The node ID to get predecessors for.</param>
/// <param name="graph">The graph.</param>
/// <returns>A list of predecessor node IDs and edge weights.</returns>
let predecessors (id: NodeId) (graph: Graph<'n, 'e>) : list<NodeId * 'e> =
    graph.InEdges
    |> Map.tryFind id
    |> Option.map Map.toList
    |> Option.defaultValue []

/// <summary>
/// Returns just the NodeIds of predecessors (without edge weights).
/// </summary>
/// <param name="node">The node ID to get predecessors for.</param>
/// <param name="graph">The graph.</param>
/// <returns>A list of predecessor node IDs.</returns>
let predecessorIds (node: NodeId) (graph: Graph<'n, 'e>) : NodeId list = predecessors node graph |> List.map fst

/// <summary>
/// Gets everyone connected to the node, regardless of direction.
///
/// For undirected graphs, this is equivalent to <c>successors</c>.
/// For directed graphs, this combines both outgoing and incoming edges
/// without duplicates.
/// </summary>
/// <param name="id">The node ID to get neighbors for.</param>
/// <param name="graph">The graph.</param>
/// <returns>A list of connected node IDs and edge weights.</returns>
let neighbors (id: NodeId) (graph: Graph<'n, 'e>) : list<NodeId * 'e> =
    match graph.Kind with
    | Undirected -> successors id graph
    | Directed ->
        let outgoing = successors id graph
        let incoming = predecessors id graph
        let outIds = outgoing |> List.map fst |> Set.ofList

        (outgoing, incoming)
        ||> List.fold (fun acc ((inId, _) as inc) ->
            if outIds |> Set.contains inId then
                acc
            else
                inc :: acc)

/// <summary>
/// Returns all node IDs in the graph.
/// This includes all nodes, even isolated nodes with no edges.
/// </summary>
/// <param name="graph">The graph.</param>
/// <returns>A list of all node IDs.</returns>
let allNodes (graph: Graph<'n, 'e>) : list<NodeId> =
    graph.Nodes |> Map.toList |> List.map fst

/// <summary>
/// Returns the number of nodes in the graph (graph order).
/// </summary>
/// <remarks>
/// <strong>Time Complexity:</strong> O(1)
/// </remarks>
/// <param name="graph">The graph.</param>
/// <returns>The number of nodes.</returns>
let order (graph: Graph<'n, 'e>) : int = graph.Nodes.Count

/// <summary>
/// Returns the number of nodes in the graph.
/// Equivalent to <c>order</c>.
/// </summary>
/// <remarks>
/// <strong>Time Complexity:</strong> O(1)
/// </remarks>
let nodeCount = order

/// <summary>
/// Returns the number of edges in the graph.
///
/// For undirected graphs, each edge is counted once (the pair {u, v}).
/// For directed graphs, each directed edge (u -> v) is counted once.
/// </summary>
/// <remarks>
/// <strong>Time Complexity:</strong> O(V)
/// </remarks>
/// <param name="graph">The graph.</param>
/// <returns>The number of edges.</returns>
let edgeCount (graph: Graph<'n, 'e>) : int =
    let count =
        graph.OutEdges
        |> Map.fold (fun acc _ targets -> acc + targets.Count) 0

    match graph.Kind with
    | Directed -> count
    | Undirected -> count / 2

/// <summary>
/// Returns just the NodeIds of successors (without edge weights).
/// Convenient for traversal algorithms that only need the IDs.
/// </summary>
/// <param name="id">The node ID to get successors for.</param>
/// <param name="graph">The graph.</param>
/// <returns>A list of successor node IDs.</returns>
let successorIds (id: NodeId) (graph: Graph<'n, 'e>) : list<NodeId> = successors id graph |> List.map fst

/// <summary>
/// Removes a node and all its connected edges (incoming and outgoing).
///
/// **Example:**
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
/// </summary>
/// <remarks>
/// <strong>Time Complexity:</strong> O(deg(v)) - proportional to the number of edges
/// connected to the node, not the whole graph.
/// </remarks>
/// <param name="id">The ID of the node to remove.</param>
/// <param name="graph">The graph to remove the node from.</param>
/// <returns>A new graph with the node and its connections removed.</returns>
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
                |> Map.change
                    targetId
                    (Option.map (Map.remove id)
                     >> Option.filter (fun m -> not m.IsEmpty)))
            newIn

    // Clean up outgoing references to this node
    let newOutCleaned =
        sources
        |> List.fold
            (fun acc sourceId ->
                acc
                |> Map.change
                    sourceId
                    (Option.map (Map.remove id)
                     >> Option.filter (fun m -> not m.IsEmpty)))
            newOut

    { graph with
        Nodes = newNodes
        OutEdges = newOutCleaned
        InEdges = newInCleaned }

/// <summary>
/// Helper for removing directed edge
/// </summary>
let private doRemoveDirectedEdge src dst (graph: Graph<'n, 'e>) =
    let newOut =
        graph.OutEdges
        |> Map.change
            src
            (Option.map (Map.remove dst)
             >> Option.filter (fun m -> not m.IsEmpty))

    let newIn =
        graph.InEdges
        |> Map.change
            dst
            (Option.map (Map.remove src)
             >> Option.filter (fun m -> not m.IsEmpty))

    { graph with
        OutEdges = newOut
        InEdges = newIn }

/// <summary>
/// Removes an edge from src to dst.
/// For undirected graphs, removes edges in both directions.
///
/// **Example:**
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
/// </summary>
/// <remarks>
/// <strong>Time Complexity:</strong> O(1)
/// </remarks>
/// <param name="src">The source node ID.</param>
/// <param name="dst">The destination node ID.</param>
/// <param name="graph">The graph to remove the edge from.</param>
/// <returns>A new graph with the edge removed.</returns>
let removeEdge (src: NodeId) (dst: NodeId) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let g = doRemoveDirectedEdge src dst graph

    match g.Kind with
    | Directed -> g
    | Undirected -> doRemoveDirectedEdge dst src g

/// <summary>
/// Helper for adding edge with combine function
/// </summary>
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
        OutEdges =
            graph.OutEdges
            |> Map.change src (addOrCombine dst)
        InEdges = graph.InEdges |> Map.change dst (addOrCombine src) }

/// <summary>
/// Adds an edge, but if an edge already exists between <c>src</c> and <c>dst</c>,
/// it combines the new weight with the existing one using <c>combine</c>.
///
/// The combine function receives <c>(existingWeight, newWeight)</c> and should
/// return the combined weight.
///
/// **Example:**
///
///     let graph =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addEdge 1 2 10
///       |> addEdgeWithCombine 1 2 5 (+)
///     // Edge 1->2 now has weight 15 (10 + 5)
///
/// </summary>
/// <remarks>
/// <strong>Time Complexity:</strong> O(1)
///
/// <h2>Use Cases</h2>
/// <ul>
/// <li><strong>Edge contraction</strong> in graph algorithms (Stoer-Wagner min-cut)</li>
/// <li><strong>Multi-graph support</strong> (adding parallel edges with combined weights)</li>
/// <li><strong>Incremental graph building</strong> (accumulating weights from multiple sources)</li>
/// </ul>
/// </remarks>
/// <param name="src">The source node ID.</param>
/// <param name="dst">The destination node ID.</param>
/// <param name="weight">The weight of the edge.</param>
/// <param name="combine">A function to combine existing and new weights.</param>
/// <param name="graph">The graph to add the edge to.</param>
/// <returns>A new graph with the edge added or weights combined.</returns>
let addEdgeWithCombine (src: NodeId) (dst: NodeId) (weight: 'e) (combine: 'e -> 'e -> 'e) (graph: Graph<'n, 'e>) =
    if not (graph.Nodes |> Map.containsKey src) then
        invalidArg "src" (sprintf "Source node %d does not exist in the graph." src)
    if not (graph.Nodes |> Map.containsKey dst) then
        invalidArg "dst" (sprintf "Destination node %d does not exist in the graph." dst)

    let g = doAddDirectedCombine src dst weight combine graph

    match g.Kind with
    | Directed -> g
    | Undirected -> doAddDirectedCombine dst src weight combine g
