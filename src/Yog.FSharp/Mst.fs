/// Minimum Spanning Tree (MST) algorithms for finding optimal network connections.
///
/// A [Minimum Spanning Tree](https://en.wikipedia.org/wiki/Minimum_spanning_tree) connects all nodes
/// in a weighted undirected graph with the minimum possible total edge weight. MSTs have
/// applications in network design, clustering, and optimization problems.
///
/// ## Available Algorithms
///
/// | Algorithm | Function | Best For |
/// |-----------|----------|----------|
/// | [Kruskal's](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm) | `kruskal/2` | Sparse graphs, edge lists |
///
/// ## Properties of MSTs
///
/// - Connects all nodes with exactly `V - 1` edges (for a graph with V nodes)
/// - Contains no cycles
/// - Minimizes the sum of edge weights
/// - May not be unique if multiple edges have the same weight
///
/// ## Example Use Cases
///
/// - **Network Design**: Minimizing cable length to connect buildings
/// - **Cluster Analysis**: Hierarchical clustering via MST
/// - **Approximation**: Traveling Salesman Problem approximations
/// - **Image Segmentation**: Computer vision applications
///
/// ## References
///
/// - [Wikipedia: Minimum Spanning Tree](https://en.wikipedia.org/wiki/Minimum_spanning_tree)
/// - [CP-Algorithms: MST](https://cp-algorithms.com/graph/mst_kruskal.html)
module Yog.Mst

open System.Collections.Generic
open Yog.Model

/// Represents an edge in the minimum spanning tree.
type Edge<'e> =
    { From: NodeId
      To: NodeId
      Weight: 'e }

/// Finds the Minimum Spanning Tree (MST) using Kruskal's algorithm.
///
/// Returns a list of edges that form the MST. The total weight of these edges
/// is minimized while ensuring all nodes are connected.
///
/// **Time Complexity:** O(E log E) where E is the number of edges
///
/// ## Algorithm
///
/// 1. Sort all edges by weight in ascending order
/// 2. Iterate through sorted edges, adding each edge that doesn't form a cycle
/// 3. Use Union-Find to efficiently detect cycles
///
/// ## Example
///
/// ```fsharp
/// let mstEdges = kruskal compare graph
/// // => [{ From = 1; To = 2; Weight = 5 }; { From = 2; To = 3; Weight = 3 }; ...]
/// ```
///
/// ## Use Cases
///
/// - Network design (minimum cost to connect all nodes)
/// - Approximation algorithms for NP-hard problems
/// - Cluster analysis (single-linkage clustering)
///
/// ## Comparison with Prim's Algorithm
///
/// - **Kruskal's:** Processes edges globally, works well for sparse graphs
/// - **Prim's:** Grows from a starting node, works well for dense graphs
///
/// Both produce optimal MSTs; choose based on graph density and implementation convenience.
let kruskal (compare: 'e -> 'e -> int) (graph: Graph<'n, 'e>) : Edge<'e> list =
    let edges =
        graph.OutEdges
        |> Map.fold
            (fun acc src targets ->
                targets
                |> Map.fold
                    (fun innerAcc dst weight ->
                        if graph.Kind = Undirected && src > dst then
                            innerAcc
                        else
                            { From = src
                              To = dst
                              Weight = weight }
                            :: innerAcc)
                    acc)
            []
        |> List.sortWith (fun a b -> compare a.Weight b.Weight)

    let rec doKruskal edgesList dsu acc =
        match edgesList with
        | [] -> List.rev acc
        | edge :: rest ->
            let dsu1, rootFrom = DisjointSet.find edge.From dsu
            let dsu2, rootTo = DisjointSet.find edge.To dsu1

            if rootFrom = rootTo then
                doKruskal rest dsu2 acc
            else
                let nextDsu = DisjointSet.union edge.From edge.To dsu2
                doKruskal rest nextDsu (edge :: acc)

    doKruskal edges DisjointSet.empty []

/// Finds the Minimum Spanning Tree (MST) using Prim's algorithm.
///
/// Returns a list of edges that form the MST. Unlike Kruskal's which processes
/// all edges globally, Prim's grows the MST from a starting node by repeatedly
/// adding the minimum-weight edge that connects a visited node to an unvisited node.
///
/// **Time Complexity:** O(E log V) where E is the number of edges and V is the number of vertices
///
/// **Disconnected Graphs:** For disconnected graphs, Prim's only returns edges
/// for the connected component containing the starting node (the first node in the graph).
/// Use Kruskal's if you need a minimum spanning forest that covers all components.
///
/// ## Algorithm
///
/// 1. Start from an arbitrary node (first node in the graph)
/// 2. Use a priority queue to track minimum-weight edges from visited to unvisited nodes
/// 3. Repeatedly extract the minimum edge, add it to the MST, and update the frontier
///
/// ## Example
///
/// ```fsharp
/// let mstEdges = prim compare graph
/// // => [{ From = 1; To = 2; Weight = 5 }; { From = 2; To = 3; Weight = 3 }; ...]
/// ```
///
/// ## Use Cases
///
/// - Dense graphs where E ≈ V² (better cache locality than Kruskal's)
/// - Incremental MST construction (can start from a specific node)
/// - When you need the MST rooted at a specific node
///
/// ## Comparison with Kruskal's Algorithm
///
/// - **Prim's:** Grows from a node, uses priority queue, O(E log V), single component only
/// - **Kruskal's:** Processes edges by weight, uses Union-Find, O(E log E), handles all components
///
/// For dense connected graphs, Prim's is often faster. For sparse graphs or graphs with
/// multiple components, Kruskal's is preferred.
let prim (compare: 'e -> 'e -> int) (graph: Graph<'n, 'e>) : Edge<'e> list =
    match allNodes graph with
    | [] -> []
    | start :: _ ->
        let comparer =
            { new IComparer<'e> with
                member _.Compare(a, b) = compare a b }

        let pq = PriorityQueue<Edge<'e>, 'e>(comparer)
        let visited = HashSet<NodeId>()

        visited.Add(start) |> ignore

        for (dst, weight) in successors start graph do
            pq.Enqueue(
                { From = start
                  To = dst
                  Weight = weight },
                weight
            )

        let mutable mstEdges = []

        while pq.Count > 0 do
            let edge = pq.Dequeue()

            if visited.Add(edge.To) then
                mstEdges <- edge :: mstEdges

                for (dst, weight) in successors edge.To graph do
                    if not (visited.Contains(dst)) then
                        pq.Enqueue(
                            { From = edge.To
                              To = dst
                              Weight = weight },
                            weight
                        )

        List.rev mstEdges
