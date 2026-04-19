/// Floyd-Warshall algorithm for all-pairs shortest paths in weighted graphs.
/// 
/// The Floyd-Warshall algorithm finds the shortest paths between all pairs of nodes
/// in a single execution. It uses dynamic programming to iteratively improve shortest
/// path estimates by considering each node as a potential intermediate vertex.
/// 
/// ## Algorithm
/// 
/// | Algorithm | Function | Complexity | Best For |
/// |-----------|----------|------------|----------|
/// | Floyd-Warshall | floydWarshall | O(V³) | Dense graphs, all-pairs paths |
/// 
/// ## Key Concepts
/// 
/// - **Dynamic Programming**: Builds solution from smaller subproblems
/// - **K-Intermediate Nodes**: After k iterations, paths use only nodes {1,...,k} as intermediates
/// - **Path Reconstruction**: Predecessor matrix allows full path recovery
/// - **Transitive Closure**: Can be adapted for reachability (boolean weights)
/// 
/// ## The DP Recurrence
/// 
/// dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])
/// 
/// For each intermediate node k, check if going through k improves the path from i to j.
/// 
/// ## Comparison with Running Dijkstra V Times
/// 
/// | Approach | Complexity | Best For |
/// |----------|------------|----------|
/// | Floyd-Warshall | O(V³) | Dense graphs (E ≈ V²) |
/// | V × Dijkstra | O(V(V+E) log V) | Sparse graphs |
/// | Johnson's | O(V² log V + VE) | Sparse graphs with negative weights |
/// 
/// **Rule of thumb**: Use Floyd-Warshall when E > V × log V (fairly dense)
/// 
/// ## Negative Cycles
/// 
/// The algorithm can detect negative cycles: after completion, if any node has
/// dist[node][node] < 0, a negative cycle exists.
/// 
/// ## Use Cases
/// 
/// - All-pairs routing: Precompute distances for fast lookup
/// - Transitive closure: Reachability queries in databases
/// - Centrality metrics: Closeness and betweenness calculations
/// - Graph analysis: Detecting negative cycles
/// 
/// ## History
/// 
/// Published independently by Robert Floyd (1962), Stephen Warshall (1962),
/// and Bernard Roy (1959). Floyd's version included path reconstruction.
module Yog.Pathfinding.FloydWarshall

open System.Collections.Generic
open Yog.Model

/// Detects if there's a negative cycle by checking if any node has negative distance to itself.
/// 
/// ## Parameters
/// - `zero`: The zero/neutral element for the weight type
/// - `compare`: Comparison function for weights
/// - `distances`: Current distance map from Floyd-Warshall
/// - `nodes`: List of node IDs to check
/// 
/// ## Returns
/// `true` if a negative cycle is detected, `false` otherwise.
/// 
/// ## Algorithm
/// After Floyd-Warshall completes, if any node has dist[node][node] < 0,
/// there's a negative cycle reachable from that node.
let detectNegativeCycle
    (zero: 'e)
    (compare: 'e -> 'e -> int)
    (distances: Map<NodeId * NodeId, 'e>)
    (nodes: NodeId list)
    : bool =
    nodes
    |> List.exists (fun i ->
        match distances |> Map.tryFind (i, i) with
        | Some dist -> compare dist zero < 0
        | None -> false)

/// Computes shortest paths between all pairs of nodes using Floyd-Warshall.
/// 
/// ## Type Parameters
/// - `'e`: The edge weight type (must support zero, addition, comparison)
/// 
/// ## Parameters
/// - `zero`: The identity element for addition (e.g., 0, 0.0)
/// - `add`: Addition function for combining path weights
/// - `compare`: Comparison function returning int (< 0, 0, > 0)
/// - `graph`: Input graph
/// 
/// ## Returns
/// - `Ok distances`: Map from (source, target) pairs to shortest distance
/// - `Error ()`: If a negative cycle is detected
/// 
/// ## Algorithm
/// Dynamic programming approach with relaxation:
/// 
/// dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j]) for all k
/// 
/// ## Example
/// 
///     // Compute all-pairs shortest paths
///     let result = floydWarshall 0 (+) compare graph
///     
///     match result with
///     | Ok distances ->
///         // Get distance from node 0 to node 3
///         let d03 = distances |> Map.tryFind (0, 3)
///     | Error () ->
///         printfn "Graph contains a negative cycle!"
/// 
/// ## Warning
/// Returns `Error` if the graph contains a negative cycle. In this case,
/// shortest paths are undefined (they can be infinitely negative).
let floydWarshall
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (graph: Graph<'n, 'e>)
    : Result<Map<NodeId * NodeId, 'e>, unit> =
    let nodes = allNodes graph

    // Using a mutable dictionary internally for blistering O(V³) performance
    let distances = Dictionary<NodeId * NodeId, 'e>()

    // 1. Initialize distances
    for i in nodes do
        for j in nodes do
            if i = j then
                // Self edges: check if there's a negative self-loop explicitly added, otherwise 0
                match
                    graph.OutEdges |> Map.tryFind i
                    |> Option.bind (Map.tryFind j)
                    with
                | Some w when compare w zero < 0 -> distances.[(i, j)] <- w
                | _ -> distances.[(i, j)] <- zero
            else
                match
                    graph.OutEdges |> Map.tryFind i
                    |> Option.bind (Map.tryFind j)
                    with
                | Some w -> distances.[(i, j)] <- w
                | None -> () // Leave unassigned to represent "infinity"

    // 2. Floyd-Warshall triple loop
    for k in nodes do
        for i in nodes do
            let mutable distIK = zero

            // Optimization: Only run the inner loop if a path from i to k actually exists
            if distances.TryGetValue((i, k), &distIK) then
                for j in nodes do
                    let mutable distKJ = zero

                    if distances.TryGetValue((k, j), &distKJ) then
                        let newDist = add distIK distKJ
                        let mutable currentDist = zero

                        // Update if no path existed yet (infinity), or if we found a shorter path
                        if
                            not (distances.TryGetValue((i, j), &currentDist))
                            || compare newDist currentDist < 0
                        then
                            distances.[(i, j)] <- newDist

    // 3. Check for negative cycles
    let mutable hasNegativeCycle = false

    for i in nodes do
        let mutable selfDist = zero

        if
            distances.TryGetValue((i, i), &selfDist)
            && compare selfDist zero < 0
        then
            hasNegativeCycle <- true

    if hasNegativeCycle then
        Error()
    else
        // Convert the fast mutable Dictionary back to a pure F# Map for the caller
        let mapSeq =
            distances
            |> Seq.map (fun kvp -> kvp.Key, kvp.Value)

        Ok(Map.ofSeq mapSeq)

// -----------------------------------------------------------------------------
// CONVENIENCE WRAPPERS FOR COMMON TYPES
// -----------------------------------------------------------------------------

/// Computes all-pairs shortest paths with integer weights.
/// 
/// ## Parameters
/// - `graph`: Input graph with int edge weights
/// 
/// ## Returns
/// - `Ok distances`: Map of shortest distances
/// - `Error ()`: If negative cycle detected
/// 
/// ## Example
/// 
///     let result = floydWarshallInt graph
///     // Access distance from 0 to 5
///     match result with
///     | Ok d -> d |> Map.tryFind (0, 5)
///     | Error () -> None
/// 
let floydWarshallInt (graph: Graph<'n, int>) : Result<Map<NodeId * NodeId, int>, unit> =
    floydWarshall 0 (+) compare graph

/// Computes all-pairs shortest paths with float weights.
/// 
/// ## Parameters
/// - `graph`: Input graph with float edge weights
/// 
/// ## Returns
/// - `Ok distances`: Map of shortest distances
/// - `Error ()`: If negative cycle detected
/// 
/// ## Note
/// Uses `compare` for float comparison. NaN values may cause unexpected behavior.
let floydWarshallFloat (graph: Graph<'n, float>) : Result<Map<NodeId * NodeId, float>, unit> =
    floydWarshall 0.0 (+) compare graph
