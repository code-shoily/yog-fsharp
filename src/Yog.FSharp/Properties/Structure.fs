/// Structural properties of graphs.
///
/// This module provides checks for various graph classes and regularities.
///
/// ## Algorithms
///
/// | Problem | Function | Complexity |
/// |---------|----------|------------|
/// | Tree check | `isTree` | O(V + E) |
/// | Arborescence check | `isArborescence` | O(V + E) |
/// | Forest check | `isForest` | O(V + E) |
/// | Branching check | `isBranching` | O(V + E) |
/// | Complete graph check | `isComplete` | O(V) |
/// | Regular graph check | `isRegular` | O(V) |
/// | Connected check | `isConnected` | O(V + E) |
/// | Strongly connected check | `isStronglyConnected` | O(V + E) |
/// | Weakly connected check | `isWeaklyConnected` | O(V + E) |
/// | Chordal check | `isChordal` | O(V + E) |
///
/// ## Key Concepts
///
/// - **Tree**: Connected acyclic undirected graph.
/// - **Arborescence**: Directed tree with a unique root.
/// - **Complete Graph (Kn)**: Every pair of distinct vertices is connected by an edge.
/// - **Regular Graph**: Every vertex has the same degree k.
module Yog.Properties.Structure

open System.Collections.Generic
open Yog.Model

/// Degree of a node in a graph. For undirected graphs this is the number of
/// neighbors. For directed graphs this returns the total degree
/// (in-degree + out-degree).
let private degree (id: NodeId) (graph: Graph<'n, 'e>) : int =
    match graph.Kind with
    | Undirected -> (neighbors id graph).Length
    | Directed -> (successors id graph).Length + (predecessors id graph).Length

/// Checks if the graph is connected.
///
/// For undirected graphs, every node is reachable from every other node.
/// For directed graphs, this checks for strong connectivity.
///
/// **Time Complexity:** O(V + E)
let rec isConnected (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Undirected ->
        match Yog.Connectivity.connectedComponents graph with
        | [ _ ] -> true
        | [] -> true
        | _ -> false
    | Directed -> isStronglyConnected graph

/// Checks if a directed graph is strongly connected.
///
/// For undirected graphs, this delegates to `isConnected`.
///
/// **Time Complexity:** O(V + E)
and isStronglyConnected (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Undirected -> isConnected graph
    | Directed ->
        match Yog.Connectivity.stronglyConnectedComponents graph with
        | [ _ ] -> true
        | [] -> true
        | _ -> false

/// Checks if a directed graph is weakly connected.
///
/// For undirected graphs, this delegates to `isConnected`.
///
/// **Time Complexity:** O(V + E)
let isWeaklyConnected (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Undirected -> isConnected graph
    | Directed ->
        match Yog.Connectivity.weaklyConnectedComponents graph with
        | [ _ ] -> true
        | [] -> true
        | _ -> false

/// Checks if the graph is a tree (connected and acyclic).
/// Works for undirected graphs; directed graphs return `false`.
///
/// **Time Complexity:** O(V + E)
let isTree (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Undirected ->
        let n = nodeCount graph
        let e = edgeCount graph
        n > 0 && e = n - 1 && isConnected graph
    | Directed -> false

/// Checks if the graph is an arborescence (directed tree with a single root).
///
/// A directed graph is an arborescence iff:
/// - It has n nodes and n-1 edges
/// - Exactly one node has in-degree 0 (the root)
/// - All other nodes have in-degree exactly 1
///
/// **Time Complexity:** O(V + E)
let isArborescence (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Directed ->
        let n = nodeCount graph

        if n > 0 && edgeCount graph = n - 1 then
            let mutable roots = 0
            let mutable validNonRoots = 0

            for node in allNodes graph do
                let inDeg = (predecessors node graph).Length

                match inDeg with
                | 0 -> roots <- roots + 1
                | 1 -> validNonRoots <- validNonRoots + 1
                | _ -> ()

            roots = 1 && validNonRoots = n - 1
        else
            false
    | Undirected -> false

/// Finds the root of an arborescence, or `None` if the graph is not an
/// arborescence.
let arborescenceRoot (graph: Graph<'n, 'e>) : NodeId option =
    if isArborescence graph then
        allNodes graph |> List.tryFind (fun node -> (predecessors node graph).Length = 0)
    else
        None

/// Checks if the graph is a forest (a loopless undirected graph consisting
/// entirely of disjoint trees).
///
/// A disconnected graph with multiple trees evaluates to `true`.
///
/// **Time Complexity:** O(V + E)
let isForest (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Undirected ->
        let n = nodeCount graph

        if n = 0 then
            true
        else
            let e = edgeCount graph
            let c = (Yog.Connectivity.connectedComponents graph).Length
            e = n - c
    | Directed -> false

/// Checks if a directed graph is a branching (a directed forest).
///
/// Evaluates to `true` if every node has an in-degree of 1 or 0, and the graph
/// contains no directed cycles.
///
/// **Time Complexity:** O(V + E)
let isBranching (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Directed ->
        let validInDegrees =
            allNodes graph |> List.forall (fun node -> (predecessors node graph).Length <= 1)

        validInDegrees && Yog.Properties.Cyclicity.isAcyclic graph
    | Undirected -> false

/// Checks if the graph is complete (every pair of distinct nodes is connected).
///
/// **Time Complexity:** O(V)
let isComplete (graph: Graph<'n, 'e>) : bool =
    let n = nodeCount graph

    if n <= 1 then
        true
    else
        let e = edgeCount graph

        let expected =
            match graph.Kind with
            | Undirected -> n * (n - 1) / 2
            | Directed -> n * (n - 1)

        e = expected && not (allNodes graph |> List.exists (fun u -> hasEdge u u graph))

/// Checks if the graph is k-regular (every node has degree exactly k).
///
/// For undirected graphs, every node must have exactly k neighbors.
/// For directed graphs, every node must have in-degree = out-degree = k.
///
/// **Time Complexity:** O(V)
let isRegular (k: int) (graph: Graph<'n, 'e>) : bool =
    let nodes = allNodes graph

    if nodes.IsEmpty then
        true
    else
        match graph.Kind with
        | Undirected -> nodes |> List.forall (fun u -> (neighbors u graph).Length = k)
        | Directed ->
            nodes
            |> List.forall (fun u ->
                (successors u graph).Length = k && (predecessors u graph).Length = k)

/// Returns the minimum degree of the graph.
///
/// Isolated vertices have degree 0. Returns 0 for an empty graph.
///
/// **Time Complexity:** O(V)
let minimumDegree (graph: Graph<'n, 'e>) : int =
    let nodes = allNodes graph

    if nodes.IsEmpty then
        0
    else
        nodes |> List.map (fun u -> degree u graph) |> List.min



// =============================================================================
// Chordal graph helpers (Maximum Cardinality Search)
// =============================================================================

/// Pop a node from the bucket with the highest non-empty weight.
let private popMaxWeightNode (buckets: Dictionary<int, HashSet<NodeId>>) (maxWeight: int) : NodeId * int =
    let rec loop w =
        if w < 0 then
            invalidOp "Bucket queue empty - no more nodes to process"
        else
            match buckets.TryGetValue w with
            | false, _ -> loop (w - 1)
            | true, set ->
                if set.Count = 0 then
                    loop (w - 1)
                else
                    let node = set |> Seq.head
                    set.Remove(node) |> ignore
                    (node, w)

    loop maxWeight

/// Maximum Cardinality Search ordering.
let private mcsOrdering (graph: Graph<'n, 'e>) : NodeId list =
    let nodes = allNodes graph
    let n = nodes.Length

    if n = 0 then
        []
    else
        let weights = Dictionary<NodeId, int>()
        let remaining = HashSet<NodeId>(nodes)
        let buckets = Dictionary<int, HashSet<NodeId>>()
        buckets.[0] <- HashSet<NodeId>(nodes)

        for node in nodes do
            weights.[node] <- 0

        let rec loop order maxWeight =
            if remaining.Count = 0 then
                List.rev order
            else
                let v, newMaxWeight = popMaxWeightNode buckets maxWeight

                remaining.Remove(v) |> ignore
                buckets.[newMaxWeight].Remove(v) |> ignore

                let mutable updatedMaxWeight = newMaxWeight

                for u in neighborIds v graph do
                    if remaining.Contains(u) then
                        let oldWeight = weights.[u]
                        let newWeight = oldWeight + 1
                        weights.[u] <- newWeight

                        if not (buckets.ContainsKey oldWeight) then
                            buckets.[oldWeight] <- HashSet<NodeId>()

                        if not (buckets.ContainsKey newWeight) then
                            buckets.[newWeight] <- HashSet<NodeId>()

                        buckets.[oldWeight].Remove(u) |> ignore
                        buckets.[newWeight].Add(u) |> ignore

                        if newWeight > updatedMaxWeight then
                            updatedMaxWeight <- newWeight

                loop (v :: order) updatedMaxWeight

        loop [] 0

/// Checks whether the given set of nodes forms a clique in the graph.
let private isClique (graph: Graph<'n, 'e>) (nodes: NodeId list) : bool =
    let rec checkPairs remaining =
        match remaining with
        | [] -> true
        | u :: rest ->
            rest |> List.forall (fun v -> hasEdge u v graph) && checkPairs rest

    checkPairs nodes

/// Checks whether the given ordering is a Perfect Elimination Ordering.
let private isPeo (graph: Graph<'n, 'e>) (order: NodeId list) : bool =
    let pos = order |> List.mapi (fun i v -> (v, i)) |> Map.ofList

    order
    |> List.forall (fun v ->
        let earlierNeighbors =
            neighborIds v graph
            |> List.filter (fun u -> pos.[u] < pos.[v])

        isClique graph earlierNeighbors)

/// Checks if the graph is chordal using Maximum Cardinality Search.
///
/// A chordal graph is one in which every cycle of length four or more has a
/// chord. Equivalently, a graph is chordal iff it has a Perfect Elimination
/// Ordering.
///
/// **Time Complexity:** O(V + E)
let isChordal (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Undirected -> isPeo graph (mcsOrdering graph)
    | Directed -> false
