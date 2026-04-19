/// Graph transformations and mappings - functor operations on graphs.
///
/// This module provides operations that transform graphs while preserving their structure.
/// These are useful for adapting graph data types, creating derived graphs, and
/// preparing graphs for specific algorithms.
///
/// ## Available Transformations
///
/// | Transformation | Function | Complexity | Use Case |
/// |----------------|----------|------------|----------|
/// | Transpose | `transpose/1` | O(1) | Reverse edge directions |
/// | Map Nodes | `map_nodes/2` | O(V) | Transform node data |
/// | Map Edges | `map_edges/2` | O(E) | Transform edge weights |
/// | Filter Nodes | `filter_nodes/2` | O(V) | Subgraph extraction |
/// | Filter Edges | `filter_edges/2` | O(E) | Remove unwanted edges |
///
/// ## The O(1) Transpose Operation
///
/// Due to yog's dual-map representation (storing both outgoing and incoming edges),
/// transposing a graph is a single pointer swap - dramatically faster than O(E)
/// implementations in traditional adjacency list libraries.
///
/// ## Functor Laws
///
/// The mapping operations satisfy functor laws:
/// - Identity: `mapNodes g identity = g`
/// - Composition: `mapNodes (mapNodes g f) h = mapNodes g (h << f)`
///
/// ## Use Cases
///
/// - **Kosaraju's Algorithm**: Requires transposed graph for SCC finding
/// - **Type Conversion**: Changing node/edge data types for algorithm requirements
/// - **Subgraph Extraction**: Working with portions of large graphs
/// - **Weight Normalization**: Preprocessing edge weights
module Yog.Transform

open Yog.Model

/// Reverses the direction of every edge in the graph (graph transpose).
///
/// Due to the dual-map representation (storing both OutEdges and InEdges),
/// this is an **O(1) operation** - just a pointer swap! This is dramatically
/// faster than most graph libraries where transpose is O(E).
///
/// **Time Complexity:** O(1)
///
/// **Property:** `transpose(transpose(G)) = G`
///
/// ## Example
///
///     let graph =
///       empty Directed
///       |> addEdge 1 2 10
///       |> addEdge 2 3 20
///
///     let reversed = transpose graph
///     // Now has edges: 2->1 and 3->2
///
/// ## Use Cases
///
/// - Computing strongly connected components (Kosaraju's algorithm)
/// - Finding all nodes that can reach a target node
/// - Reversing dependencies in a DAG
let transpose (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    { graph with
        OutEdges = graph.InEdges
        InEdges = graph.OutEdges }

/// Transforms node data using a function, preserving graph structure.
///
/// This is a functor operation - it applies a function to every node's data
/// while keeping all edges and the graph structure unchanged.
///
/// **Time Complexity:** O(V) where V is the number of nodes
///
/// **Functor Law:** `mapNodes (mapNodes g f) h = mapNodes g (h << f)`
///
/// ## Example
///
///     let graph =
///       empty Directed
///       |> addNode 1 "alice"
///       |> addNode 2 "bob"
///
///     let uppercased = mapNodes (fun s -> s.ToUpper()) graph
///     // Nodes now contain "ALICE" and "BOB"
///
/// ## Type Changes
///
/// Can change the node data type:
///
///     // Convert string node data to integers
///     mapNodes graph (fun s ->
///       match System.Int32.TryParse(s) with
///       | true, n -> n
///       | false, _ -> 0
///     )
///
let mapNodes (f: 'n -> 'm) (graph: Graph<'n, 'e>) : Graph<'m, 'e> =
    let newNodes = graph.Nodes |> Map.map (fun _ -> f)

    // Notice we cannot use { graph with ... } here because the generic type
    // changes from Graph<'n, 'e> to Graph<'m, 'e>. We must build it explicitly.
    { Kind = graph.Kind
      Nodes = newNodes
      OutEdges = graph.OutEdges
      InEdges = graph.InEdges }

/// Transforms edge weights using a function, preserving graph structure.
///
/// This is a functor operation - it applies a function to every edge's weight/data
/// while keeping all nodes and the graph topology unchanged.
///
/// **Time Complexity:** O(E) where E is the number of edges
///
/// **Functor Law:** `mapEdges (mapEdges g f) h = mapEdges g (h << f)`
///
/// ## Example
///
///     let graph =
///       empty Directed
///       |> addEdge 1 2 10
///       |> addEdge 2 3 20
///
///     // Double all weights
///     let doubled = mapEdges (fun w -> w * 2) graph
///     // Edges now have weights 20 and 40
///
/// ## Type Changes
///
/// Can change the edge weight type:
///
///     // Convert integer weights to floats
///     mapEdges float graph
///
///     // Convert weights to labels
///     mapEdges (fun w -> if w < 10 then "short" else "long") graph
///
let mapEdges (f: 'e -> 'f) (graph: Graph<'n, 'e>) : Graph<'n, 'f> =
    let mapOuter outerMap =
        outerMap |> Map.map (fun _ innerMap -> innerMap |> Map.map (fun _ -> f))

    // The same generic type change rule applies here for 'e -> 'f
    { Kind = graph.Kind
      Nodes = graph.Nodes
      OutEdges = mapOuter graph.OutEdges
      InEdges = mapOuter graph.InEdges }

/// Filters nodes by a predicate, automatically pruning connected edges.
///
/// Returns a new graph containing only nodes whose data satisfies the predicate.
/// All edges connected to removed nodes (both incoming and outgoing) are
/// automatically removed to maintain graph consistency.
///
/// **Time Complexity:** O(V + E) where V is nodes and E is edges
///
/// ## Example
///
///     let graph =
///       empty Directed
///       |> addNode 1 "apple"
///       |> addNode 2 "banana"
///       |> addNode 3 "apricot"
///       |> addEdge 1 2 1
///       |> addEdge 2 3 2
///
///     // Keep only nodes starting with 'a'
///     let filtered = filterNodes (fun s -> s.StartsWith("a")) graph
///     // Result has nodes 1 and 3, edge 1->2 is removed (node 2 gone)
///
/// ## Use Cases
///
/// - Extract subgraphs based on node properties
/// - Remove inactive/disabled nodes from a network
/// - Filter by node importance/centrality
let filterNodes (predicate: 'n -> bool) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let keptNodes = graph.Nodes |> Map.filter (fun _ -> predicate)
    let keptIds = keptNodes.Keys |> Set.ofSeq

    let pruneEdges outerMap =
        outerMap
        |> Map.filter (fun src _ -> Set.contains src keptIds)
        |> Map.map (fun _ innerMap -> innerMap |> Map.filter (fun dst _ -> Set.contains dst keptIds))
        |> Map.filter (fun _ innerMap -> not innerMap.IsEmpty)

    { graph with
        Nodes = keptNodes
        OutEdges = pruneEdges graph.OutEdges
        InEdges = pruneEdges graph.InEdges }

/// Filters edges by a predicate, preserving all nodes.
///
/// Returns a new graph with the same nodes but only the edges where the
/// predicate returns `true`. The predicate receives `(src, dst, weight)`.
///
/// **Time Complexity:** O(E) where E is the number of edges
///
/// ## Example
///
///     let graph =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addNode 3 "C"
///       |> addEdge 1 2 5
///       |> addEdge 1 3 15
///       |> addEdge 2 3 3
///
///     // Keep only edges with weight >= 10
///     let heavy = filterEdges (fun _src _dst w -> w >= 10) graph
///     // Result: edges [1->3 (15)], edges 1->2 and 2->3 removed
///
/// ## Use Cases
///
/// - Pruning low-weight edges in weighted networks
/// - Removing self-loops: `filterEdges (fun s d _ -> s <> d) graph`
/// - Threshold-based graph sparsification
let filterEdges (predicate: NodeId -> NodeId -> 'e -> bool) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let filterOuter outerMap =
        outerMap
        |> Map.map (fun src innerMap -> innerMap |> Map.filter (predicate src))
        |> Map.filter (fun _ innerMap -> not innerMap.IsEmpty)

    { graph with
        OutEdges = filterOuter graph.OutEdges
        InEdges = filterOuter graph.InEdges }

/// Creates the complement of a graph.
///
/// The complement contains the same nodes but connects all pairs of nodes
/// that are **not** connected in the original graph, and removes all edges
/// that **are** present. Each new edge gets the supplied `defaultWeight`.
///
/// Self-loops are never added in the complement.
///
/// **Time Complexity:** O(V² + E)
///
/// ## Example
///
///     let graph =
///       empty Undirected
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addNode 3 "C"
///       |> addEdge 1 2 1
///
///     let comp = complement 1 graph
///     // Original: 1-2 connected, 1-3 and 2-3 not
///     // Complement: 1-3 and 2-3 connected, 1-2 not
///
/// ## Use Cases
///
/// - Finding independent sets (cliques in the complement)
/// - Graph coloring via complement analysis
/// - Testing graph density (sparse ↔ dense complement)
let complement (defaultWeight: 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let nodeIds = graph.Nodes |> Map.toList |> List.map fst

    let initGraph =
        { graph with
            OutEdges = Map.empty
            InEdges = Map.empty }

    nodeIds
    |> List.fold
        (fun g src ->
            nodeIds
            |> List.fold
                (fun acc dst ->
                    if src = dst then
                        acc
                    else
                        let hasEdge =
                            match graph.OutEdges |> Map.tryFind src with
                            | Some inner -> inner |> Map.containsKey dst
                            | None -> false

                        if hasEdge then acc else addEdge src dst defaultWeight acc)
                g)
        initGraph

// --- Helper functions for merge ---
let private mapOverwrite m1 m2 =
    (m1, m2) ||> Map.fold (fun acc k v -> Map.add k v acc)

let private mergeOuter outer1 outer2 =
    (outer1, outer2)
    ||> Map.fold (fun acc src inner2 ->
        match Map.tryFind src acc with
        | Some inner1 -> Map.add src (mapOverwrite inner1 inner2) acc
        | None -> Map.add src inner2 acc)
// ----------------------------------

/// Combines two graphs, with the second graph's data taking precedence on conflicts.
///
/// Merges nodes, OutEdges, and InEdges from both graphs. When a node exists in
/// both graphs, the node data from `other` overwrites `base`. When the same edge
/// exists in both graphs, the edge weight from `other` overwrites `base`.
///
/// Importantly, edges from different nodes are combined - if `base` has edges
/// 1->2 and 1->3, and `other` has edges 1->4 and 1->5, the result will have
/// all four edges from node 1.
///
/// The resulting graph uses the `Kind` (Directed/Undirected) from the base graph.
///
/// **Time Complexity:** O(V + E) for both graphs combined
///
/// ## Example
///
///     let base =
///       empty Directed
///       |> addNode 1 "Original"
///       |> addEdge 1 2 10
///       |> addEdge 1 3 15
///
///     let other =
///       empty Directed
///       |> addNode 1 "Updated"
///       |> addEdge 1 4 20
///       |> addEdge 2 3 25
///
///     let merged = merge base other
///     // Node 1 has "Updated" (from other)
///     // Node 1 has edges to: 2, 3, and 4 (all edges combined)
///     // Node 2 has edge to: 3
///
/// ## Use Cases
///
/// - Combining disjoint subgraphs
/// - Applying updates/patches to a graph
/// - Building graphs incrementally from multiple sources
let merge (baseGraph: Graph<'n, 'e>) (other: Graph<'n, 'e>) : Graph<'n, 'e> =
    { Kind = baseGraph.Kind
      Nodes = mapOverwrite baseGraph.Nodes other.Nodes
      OutEdges = mergeOuter baseGraph.OutEdges other.OutEdges
      InEdges = mergeOuter baseGraph.InEdges other.InEdges }

/// Extracts a subgraph containing only the specified nodes and their connecting edges.
///
/// Returns a new graph with only the nodes whose IDs are in the provided list,
/// along with any edges that connect nodes within this subset. Nodes not in the
/// list are removed, and all edges touching removed nodes are pruned.
///
/// **Time Complexity:** O(V + E) where V is nodes and E is edges
///
/// ## Example
///
///     let graph =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addNode 3 "C"
///       |> addNode 4 "D"
///       |> addEdge 1 2 10
///       |> addEdge 2 3 20
///       |> addEdge 3 4 30
///
///     // Extract only nodes 2 and 3
///     let sub = subgraph [2; 3] graph
///     // Result has nodes 2, 3 and edge 2->3
///     // Edges 1->2 and 3->4 are removed (endpoints outside subgraph)
///
/// ## Use Cases
///
/// - Extracting connected components found by algorithms
/// - Analyzing k-hop neighborhoods around specific nodes
/// - Working with strongly connected components (extract each SCC)
/// - Removing nodes found by some criteria (keep the inverse set)
/// - Visualizing specific portions of large graphs
///
/// ## Comparison with `filterNodes`
///
/// - `filterNodes` - Filters by predicate on node data (e.g., "keep active users")
/// - `subgraph` - Filters by explicit node IDs (e.g., "keep nodes [1, 5, 7]")
let subgraph (ids: NodeId list) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let idSet = Set.ofList ids

    let keptNodes = graph.Nodes |> Map.filter (fun id _ -> Set.contains id idSet)

    let prune outerMap =
        outerMap
        |> Map.filter (fun src _ -> Set.contains src idSet)
        |> Map.map (fun _ innerMap -> innerMap |> Map.filter (fun dst _ -> Set.contains dst idSet))
        |> Map.filter (fun _ innerMap -> not innerMap.IsEmpty)

    { graph with
        Nodes = keptNodes
        OutEdges = prune graph.OutEdges
        InEdges = prune graph.InEdges }

/// Contracts an edge by merging node `b` into node `a`.
///
/// Node `b` is removed from the graph, and all edges connected to `b` are
/// redirected to `a`. If both `a` and `b` had edges to the same neighbor,
/// their weights are combined using `combineWeights`.
///
/// Self-loops (edges from a node to itself) are removed during contraction.
///
/// **Important for undirected graphs:** Since undirected edges are stored
/// bidirectionally, each logical edge is processed twice during contraction,
/// causing weights to be combined twice. For example, if edge weights represent
/// capacities, this effectively doubles them. Consider dividing weights by 2
/// or using a custom combine function if this behavior is undesired.
///
/// **Time Complexity:** O(deg(a) + deg(b)) - proportional to the combined
/// degree of both nodes.
///
/// ## Example
///
///     let graph =
///       empty Undirected
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addNode 3 "C"
///       |> addEdge 1 2 5
///       |> addEdge 2 3 10
///
///     let contracted = contract 1 2 (+) graph
///     // Result: nodes [1, 3], edge 1-3 with weight 10
///     // Node 2 is merged into node 1
///
/// ## Combining Weights
///
/// When both `a` and `b` have edges to the same neighbor `c`:
///
///     // Before: a-[5]->c, b-[10]->c
///     let contracted = contract a b (+) graph
///     // After: a-[15]->c (5 + 10)
///
/// ## Use Cases
///
/// - **Stoer-Wagner algorithm** for minimum cut
/// - **Graph simplification** by merging strongly connected nodes
/// - **Community detection** by contracting nodes in the same community
/// - **Karger's algorithm** for minimum cut (randomized)
let contract (a: NodeId) (b: NodeId) (combineWeights: 'e -> 'e -> 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let bOut = graph.OutEdges |> Map.tryFind b |> Option.defaultValue Map.empty

    let g1 =
        bOut
        |> Map.fold
            (fun acc neighbor weight ->
                if neighbor = a || neighbor = b then
                    acc
                else
                    addEdgeWithCombine a neighbor weight combineWeights acc)
            graph

    let g2 =
        match g1.Kind with
        | Undirected -> g1
        | Directed ->
            let bIn = g1.InEdges |> Map.tryFind b |> Option.defaultValue Map.empty

            bIn
            |> Map.fold
                (fun acc neighbor weight ->
                    if neighbor = a || neighbor = b then
                        acc
                    else
                        addEdgeWithCombine neighbor a weight combineWeights acc)
                g1

    removeNode b g2

/// Converts an undirected graph to a directed graph.
///
/// Since Yog internally stores undirected edges as bidirectional directed edges,
/// this is essentially free — it just changes the `Kind` flag. The resulting
/// directed graph has two directed edges (A→B and B→A) for each original
/// undirected edge.
///
/// If the graph is already directed, it is returned unchanged.
///
/// **Time Complexity:** O(1)
///
/// ## Example
///
///     let undirected =
///       empty Undirected
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addEdge 1 2 10
///
///     let directed = toDirected undirected
///     // Has edges: 1->2 and 2->1 (both with weight 10)
///
let toDirected (graph: Graph<'n, 'e>) : Graph<'n, 'e> = { graph with Kind = Directed }

/// Converts a directed graph to an undirected graph.
///
/// For each directed edge A→B, ensures B→A also exists. If both A→B and B→A
/// already exist with different weights, the `resolve` function decides which
/// weight to keep.
///
/// If the graph is already undirected, it is returned unchanged.
///
/// **Time Complexity:** O(E) where E is the number of edges
///
/// ## Example
///
///     let directed =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addEdge 1 2 10
///       |> addEdge 2 1 20
///
///     // When both directions exist, keep the smaller weight
///     let undirected = toUndirected min directed
///     // Edge 1-2 has weight 10 (min of 10 and 20)
///
///     // One-directional edges get mirrored automatically
///     let directed =
///       empty Directed
///       |> addEdge 1 2 5
///
///     let undirected = toUndirected min directed
///     // Edge exists in both directions with weight 5
///
let toUndirected (resolve: 'e -> 'e -> 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    match graph.Kind with
    | Undirected -> graph
    | Directed ->
        let symmetricOut =
            graph.OutEdges
            |> Map.fold
                (fun accOuter src inner ->
                    inner
                    |> Map.fold
                        (fun acc dst weight ->
                            let dstInner = acc |> Map.tryFind dst |> Option.defaultValue Map.empty

                            let updatedInner =
                                match dstInner |> Map.tryFind src with
                                | Some existing -> dstInner |> Map.add src (resolve existing weight)
                                | None -> dstInner |> Map.add src weight

                            acc |> Map.add dst updatedInner)
                        accOuter)
                graph.OutEdges

        { graph with
            Kind = Undirected
            OutEdges = symmetricOut
            InEdges = symmetricOut }
