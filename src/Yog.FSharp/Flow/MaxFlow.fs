/// Maximum flow and minimum cut algorithms for network flow problems.
///
/// Provides the Edmonds-Karp algorithm for computing maximum flow in a flow network,
/// and the max-flow min-cut theorem for extracting minimum cuts.
///
/// ## When to Use
/// - Network flow optimization problems
/// - Finding bottlenecks in transportation networks
/// - Bipartite matching (via max flow)
/// - Image segmentation (via min cut)
/// - Project selection problems
///
/// ## Key Concepts
///
/// ### Flow Network
/// A directed graph where each edge has a capacity and each edge receives a flow.
/// The amount of flow on an edge cannot exceed its capacity.
///
/// ### Maximum Flow
/// The maximum amount of flow that can be sent from a source node to a sink node
/// without violating capacity constraints.
///
/// ### Minimum Cut
/// A partition of nodes into two sets (S, T) where source ∈ S and sink ∈ T,
/// minimizing the sum of capacities of edges from S to T.
///
/// ## Complexity
/// - **Time**: O(V × E²) for Edmonds-Karp
/// - **Space**: O(V + E) for residual graph
///
/// ## Max-Flow Min-Cut Theorem
/// The maximum flow value equals the capacity of the minimum cut.
/// This fundamental result connects optimization on flows to graph partitioning.
module Yog.Flow.MaxFlow

open System.Collections.Generic
open Yog.Model

/// Result of a max flow computation.
///
/// Contains both the maximum flow value and information needed to extract the minimum cut.
///
/// ## Type Parameters
/// - `'e`: The flow/capacity type (int, float, etc.)
type MaxFlowResult<'e> =
    { /// The maximum flow value from source to sink.
      MaxFlow: 'e
      /// The residual graph after flow computation (contains remaining capacities).
      /// Edges with positive residual capacity can admit more flow.
      ResidualGraph: Graph<unit, 'e>
      /// The source node used in the computation.
      Source: NodeId
      /// The sink node used in the computation.
      Sink: NodeId }



/// Represents a minimum cut in the network.
///
/// A cut partitions the nodes into two sets: those reachable from the source
/// in the residual graph (source side) and the rest (sink side).
///
/// ## Key Property
/// The capacity of this cut equals the maximum flow value (max-flow min-cut theorem).
type MinCut =
    { /// Nodes reachable from source in the residual graph (source side of cut).
      SourceSide: Set<NodeId>
      /// Nodes NOT reachable from source in the residual graph (sink side of cut).
      SinkSide: Set<NodeId> }

/// Finds the maximum flow using the Edmonds-Karp algorithm.
///
/// Edmonds-Karp is a specific implementation of the Ford-Fulkerson method
/// that uses BFS to find the shortest augmenting path.
///
/// ## Type Parameters
/// - `'e`: The flow/capacity type
///
/// ## Parameters
/// - `zero`: Identity element (0 for numeric types)
/// - `add`: Addition function for accumulating flow
/// - `subtract`: Subtraction function for updating residual capacities
/// - `compare`: Comparison function for weights
/// - `minVal`: Minimum function for finding bottleneck capacity
/// - `source`: Source node ID (flow originates here)
/// - `sink`: Sink node ID (flow terminates here)
/// - `graph`: Input graph with edge capacities
///
/// ## Returns
/// `MaxFlowResult` containing maximum flow value and residual graph.
///
/// ## Algorithm
/// 1. Initialize residual graph with original capacities
/// 2. While there exists an augmenting path from source to sink:
///    a. Find shortest augmenting path using BFS
///    b. Compute bottleneck capacity (minimum residual capacity on path)
///    c. Augment flow along path
///    d. Update residual capacities
/// 3. Return total flow and final residual graph
///
/// ## Complexity
/// - **Time**: O(V × E²) - polynomial, unlike basic Ford-Fulkerson
/// - **Space**: O(V + E)
///
/// ## Example
/// ```fsharp
/// // Simple flow network
/// let graph =
///     empty Directed
///     |> addNode 0 () |> addNode 1 () |> addNode 2 ()
///     |> addEdge 0 1 10  // Capacity 10 from 0 to 1
///     |> addEdge 1 2 5   // Capacity 5 from 1 to 2
///     |> addEdge 0 2 3   // Capacity 3 from 0 to 2
///
/// let result = edmondsKarp 0 (+) (-) compare min 0 2 graph
/// // result.MaxFlow = 8 (5 through 0->1->2, 3 through 0->2)
/// ```
///
/// ## Applications
/// - **Transportation**: Maximize goods transported through a network
/// - **Network reliability**: Find minimum edges to cut to disconnect network
/// - **Bipartite matching**: Model as flow with capacity 1 edges
/// - **Image segmentation**: Min cut separates foreground from background
let edmondsKarp
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (subtract: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (minVal: 'e -> 'e -> 'e)
    (source: NodeId)
    (sink: NodeId)
    (graph: Graph<'n, 'e>)
    : MaxFlowResult<'e> =
    if source = sink then
        { MaxFlow = zero
          ResidualGraph = Yog.Model.empty Directed
          Source = source
          Sink = sink }
    else
        // 1. Build Adjacency List and flat Residuals Dictionary
        let residuals = Dictionary<NodeId * NodeId, 'e>()
        let adjList = Dictionary<NodeId, HashSet<NodeId>>()

        let addEdge u v cap =
            residuals.[(u, v)] <- cap

            // Ensure bidirectional adjacency for BFS
            let mutable uNeighbors = null

            if not (adjList.TryGetValue(u, &uNeighbors)) then
                uNeighbors <- HashSet<NodeId>()
                adjList.[u] <- uNeighbors

            uNeighbors.Add(v) |> ignore

            let mutable vNeighbors = null

            if not (adjList.TryGetValue(v, &vNeighbors)) then
                vNeighbors <- HashSet<NodeId>()
                adjList.[v] <- vNeighbors

            vNeighbors.Add(u) |> ignore

            // Initialize backward capacity to zero if it doesn't exist
            if not (residuals.ContainsKey((v, u))) then
                residuals.[(v, u)] <- zero

        // Populate initial graph state
        for u in allNodes graph do
            for (v, cap) in successors u graph do
                addEdge u v cap

        let mutable totalFlow = zero
        let mutable pathFound = true

        // 2. Ford-Fulkerson loop using BFS (Edmonds-Karp)
        // 2. Ford-Fulkerson loop using BFS (Edmonds-Karp)
        while pathFound do
            // Dictionary stores: neighbor -> (parent, capacity_to_neighbor)
            let parents = Dictionary<NodeId, NodeId * 'e>()
            let q = Queue<NodeId>()

            q.Enqueue(source)
            // We use a dummy parent for the source to mark it as visited
            // (We'll use a NodeId that doesn't exist, like -1)
            let dummyParent = -1
            parents.[source] <- (dummyParent, zero)

            let mutable reachedSink = false

            while q.Count > 0 && not reachedSink do
                let curr = q.Dequeue()

                let mutable neighbors = null

                if adjList.TryGetValue(curr, &neighbors) then
                    for next in neighbors do
                        if not (parents.ContainsKey(next)) then
                            let cap = residuals.[(curr, next)]

                            if compare cap zero > 0 then
                                parents.[next] <- (curr, cap)
                                q.Enqueue(next)
                                if next = sink then reachedSink <- true

            if not reachedSink then
                pathFound <- false
            else
                // --- Path Reconstruction ---
                // 1. Find the bottleneck capacity along the path
                let mutable bottleneck = zero
                let mutable backtrack = sink
                let mutable isFirst = true

                while backtrack <> source do
                    let (p, cap) = parents.[backtrack]

                    if isFirst then
                        bottleneck <- cap
                        isFirst <- false
                    else
                        bottleneck <- minVal bottleneck cap

                    backtrack <- p

                // 2. Augment flow: update residuals for forward and backward edges
                let mutable augmentNode = sink

                while augmentNode <> source do
                    let (p, _) = parents.[augmentNode]
                    let forwardKey = (p, augmentNode)
                    let backwardKey = (augmentNode, p)

                    residuals.[forwardKey] <- subtract residuals.[forwardKey] bottleneck
                    residuals.[backwardKey] <- add residuals.[backwardKey] bottleneck
                    augmentNode <- p

                totalFlow <- add totalFlow bottleneck

        // 3. Rebuild Residual Graph for the result
        let mutGraph = Yog.Model.empty Directed

        // Add all original nodes (as unit/Nil data)
        let graphWithNodes =
            allNodes graph
            |> List.fold (fun acc n -> addNode n () acc) mutGraph

        // Add all updated edges from the residuals
        let finalGraph =
            residuals
            |> Seq.fold
                (fun acc kvp ->
                    let (u, v) = kvp.Key
                    let cap = kvp.Value
                    // Explicitly call the Model's addEdge to avoid name shadowing
                    Yog.Model.addEdge u v cap acc)
                graphWithNodes

        { MaxFlow = totalFlow
          ResidualGraph = finalGraph
          Source = source
          Sink = sink }

/// Extracts the minimum cut from a max flow result.
///
/// Uses the max-flow min-cut theorem, identifying nodes reachable from source
/// in the final residual graph.
///
/// ## Parameters
/// - `zero`: Identity element for flow type
/// - `compare`: Comparison function for weights
/// - `result`: The `MaxFlowResult` from a max flow computation
///
/// ## Returns
/// `MinCut` containing source and sink side node sets.
///
/// ## Algorithm
/// 1. Perform BFS from source in residual graph
/// 2. Follow edges with positive residual capacity
/// 3. All visited nodes form the source side
/// 4. Unvisited nodes form the sink side
///
/// ## Example
/// ```fsharp
/// let flowResult = edmondsKarpInt 0 5 graph
/// let cut = minCut 0 compare flowResult
///
/// // cut.SourceSide = nodes reachable from 0 in residual graph
/// // cut.SinkSide = remaining nodes
/// ```
///
/// ## Property
/// The sum of capacities of edges from SourceSide to SinkSide equals
/// the maximum flow value (max-flow min-cut theorem).
let minCut (zero: 'e) (compare: 'e -> 'e -> int) (result: MaxFlowResult<'e>) : MinCut =
    let reachable = HashSet<NodeId>()
    let q = Queue<NodeId>()

    q.Enqueue(result.Source)
    reachable.Add(result.Source) |> ignore

    // BFS to find all nodes reachable via positive capacity
    while q.Count > 0 do
        let current = q.Dequeue()

        for (next, cap) in successors current result.ResidualGraph do
            if compare cap zero > 0 && reachable.Add(next) then
                q.Enqueue(next)

    let sourceSide = reachable |> Set.ofSeq
    let allNodesSet = allNodes result.ResidualGraph |> Set.ofList
    let sinkSide = Set.difference allNodesSet sourceSide

    { SourceSide = sourceSide
      SinkSide = sinkSide }

// -----------------------------------------------------------------------------
// CONVENIENCE WRAPPER FOR INTEGER CAPACITIES
// -----------------------------------------------------------------------------

/// Finds maximum flow with integer capacities.
///
/// ## Parameters
/// - `source`: Source node ID
/// - `sink`: Sink node ID
/// - `graph`: Input graph with integer capacities
///
/// ## Returns
/// `MaxFlowResult<int>` with maximum flow and residual graph.
///
/// ## Example
/// ```fsharp
/// let result = edmondsKarpInt 0 5 myGraph
/// printfn "Maximum flow: %d" result.MaxFlow
/// ```
let edmondsKarpInt (source: NodeId) (sink: NodeId) (graph: Graph<'n, int>) : MaxFlowResult<int> =
    edmondsKarp 0 (+) (-) compare min source sink graph
