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

/// Updates a specific node's data using an updater function.
/// Similar to Map.add but with default value if node doesn't exist.
let updateNode (id: NodeId) (defaultVal: 'n) (updater: 'n -> 'n) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let newNodes =
        graph.Nodes
        |> Map.change id (fun maybeData ->
            match maybeData with
            | Some data -> Some(updater data)
            | None -> Some defaultVal)

    { graph with Nodes = newNodes }

/// Helper for updating a directed edge.
let private doUpdateDirectedEdge src dst defaultVal fn (graph: Graph<'n, 'e>) =
    let updateFn mapOpt =
        match mapOpt with
        | Some m ->
            let newW =
                match Map.tryFind dst m with
                | Some w -> fn w
                | None -> defaultVal

            Some(Map.add dst newW m)
        | None -> Some(Map.ofList [ (dst, defaultVal) ])

    let updateInFn mapOpt =
        match mapOpt with
        | Some m ->
            let newW =
                match Map.tryFind src m with
                | Some w -> fn w
                | None -> defaultVal

            Some(Map.add src newW m)
        | None -> Some(Map.ofList [ (src, defaultVal) ])

    { graph with
        OutEdges = graph.OutEdges |> Map.change src updateFn
        InEdges = graph.InEdges |> Map.change dst updateInFn }

/// Updates a specific edge's weight/metadata safely.
/// Properly handles undirected graphs by updating both directions.
let updateEdge (src: NodeId) (dst: NodeId) (defaultVal: 'e) (fn: 'e -> 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let hasSrc = graph.Nodes |> Map.containsKey src
    let hasDst = graph.Nodes |> Map.containsKey dst

    match hasSrc, hasDst with
    | true, true ->
        let g = doUpdateDirectedEdge src dst defaultVal fn graph

        match g.Kind with
        | Directed -> g
        | Undirected ->
            if src = dst then
                g
            else
                doUpdateDirectedEdge dst src defaultVal fn g
    | _ -> graph

/// Helper for DAG transitive closure.
let private doTransitiveClosureDag
    (graph: Graph<'n, 'e>)
    (sorted: NodeId list)
    (mergeFn: 'e -> 'e -> 'e)
    : Graph<'n, 'e> =
    let reachabilityMap =
        (Map.empty, List.rev sorted)
        ||> List.fold (fun acc node ->
            let edges = graph.OutEdges |> Map.tryFind node |> Option.defaultValue Map.empty

            let reachableFromNode =
                (edges, edges)
                ||> Map.fold (fun reachableAcc child wNodeChild ->
                    let childReachable = acc |> Map.tryFind child |> Option.defaultValue Map.empty
                    let mappedChild = childReachable |> Map.map (fun _ w -> mergeFn wNodeChild w)

                    (reachableAcc, mappedChild)
                    ||> Map.fold (fun combinedAcc k v ->
                        match Map.tryFind k combinedAcc with
                        | Some existing -> Map.add k (mergeFn existing v) combinedAcc
                        | None -> Map.add k v combinedAcc))

            Map.add node reachableFromNode acc)

    (graph, reachabilityMap)
    ||> Map.fold (fun gAcc src targets -> (gAcc, targets) ||> Map.fold (fun gInner dst w -> addEdge src dst w gInner))

/// Helper for general transitive closure.
let private doWeightedReachability
    (graph: Graph<'n, 'e>)
    (queue: (NodeId * 'e) list)
    (visited: Map<NodeId, 'e>)
    (updates: Map<NodeId, int>)
    (mergeFn: 'e -> 'e -> 'e)
    (maxUpdates: int)
    : Map<NodeId, 'e> =
    let rec loop q vis upds =
        match q with
        | [] -> vis
        | (current, weightToCurrent) :: rest ->
            match Map.tryFind current vis with
            | Some prevWeight ->
                let merged = mergeFn prevWeight weightToCurrent

                if merged = prevWeight then
                    loop rest vis upds
                else
                    let updateCount = Map.tryFind current upds |> Option.defaultValue 0

                    if updateCount >= maxUpdates then
                        loop rest vis upds
                    else
                        let newVis = Map.add current merged vis
                        let newUpds = Map.add current (updateCount + 1) upds
                        let successors = successors current graph
                        let nextSteps = successors |> List.map (fun (dst, w) -> (dst, mergeFn merged w))
                        loop (rest @ nextSteps) newVis newUpds
            | None ->
                let newVis = Map.add current weightToCurrent vis
                let newUpds = Map.add current 1 upds
                let successors = successors current graph

                let nextSteps =
                    successors |> List.map (fun (dst, w) -> (dst, mergeFn weightToCurrent w))

                loop (rest @ nextSteps) newVis newUpds

    loop queue visited updates

/// Finds all reachable nodes with their aggregated weights.
let private findAllReachableWeighted
    (graph: Graph<'n, 'e>)
    (start: NodeId)
    (mergeFn: 'e -> 'e -> 'e)
    : Map<NodeId, 'e> =
    let succs = successors start graph
    let maxUpdates = nodeCount graph
    doWeightedReachability graph succs Map.empty Map.empty mergeFn maxUpdates

/// Helper to compute general transitive closure (for graphs with cycles).
let private doTransitiveClosureGeneral (graph: Graph<'n, 'e>) (mergeFn: 'e -> 'e -> 'e) : Graph<'n, 'e> =
    (graph, allNodes graph)
    ||> List.fold (fun accGraph startNode ->
        let reachable = findAllReachableWeighted graph startNode mergeFn

        (accGraph, reachable)
        ||> Map.fold (fun innerGraph targetNode weight -> addEdge startNode targetNode weight innerGraph))

/// Computes the transitive closure of a graph.
/// Adds edges between all pairs of nodes where a path exists in the original graph.
let transitiveClosure (mergeFn: 'e -> 'e -> 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    match Yog.Traversal.topologicalSort graph with
    | Ok sorted -> doTransitiveClosureDag graph sorted mergeFn
    | Error _ -> doTransitiveClosureGeneral graph mergeFn

/// Computes the transitive reduction of a graph.
/// Removes all edges that are redundant (indirectly reachable).
let transitiveReduction (mergeFn: 'e -> 'e -> 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let reachGraph = transitiveClosure mergeFn graph

    (graph, graph.OutEdges)
    ||> Map.fold (fun gAcc u targets ->
        (gAcc, targets)
        ||> Map.fold (fun gInner v _ ->
            let isRedundant =
                (false, targets)
                ||> Map.fold (fun foundRedundant w _ ->
                    if foundRedundant then
                        true
                    elif w = v then
                        false
                    else
                        match Map.tryFind w reachGraph.OutEdges with
                        | Some wTargets -> Map.containsKey v wTargets
                        | None -> false)

            if isRedundant then removeEdge u v gInner else gInner))

/// Maps node data using a function that also accepts the Node ID.
let mapNodesIndexed (f: NodeId -> 'n -> 'm) (graph: Graph<'n, 'e>) : Graph<'m, 'e> =
    let newNodes = graph.Nodes |> Map.map f

    { Nodes = newNodes
      OutEdges = graph.OutEdges
      InEdges = graph.InEdges
      Kind = graph.Kind }

/// Maps edge weights using a function that also accepts the source and destination Node IDs.
let mapEdgesIndexed (f: NodeId -> NodeId -> 'e -> 'f) (graph: Graph<'n, 'e>) : Graph<'n, 'f> =
    let mapOuter outerMap =
        outerMap
        |> Map.map (fun src innerMap -> innerMap |> Map.map (fun dst weight -> f src dst weight))

    { Nodes = graph.Nodes
      OutEdges = mapOuter graph.OutEdges
      InEdges = mapOuter graph.InEdges
      Kind = graph.Kind }

/// Adds a self-loop (an edge from a node to itself) for every node in the graph if it doesn't already exist.
let addSelfLoops (defaultWeight: 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    (graph, allNodes graph)
    ||> List.fold (fun gAcc node ->
        if hasEdge node node gAcc then
            gAcc
        else
            addEdge node node defaultWeight gAcc)

/// Removes all self-loops (edges from a node to itself) from the graph.
let removeSelfLoops (graph: Graph<'n, 'e>) : Graph<'n, 'e> = filterEdges (fun u v _ -> u <> v) graph

/// Relabels all node IDs in the graph using a mapping function.
/// Updates all node identifiers and edge references (source and destination) to maintain consistency.
let relabelNodes (f: NodeId -> NodeId) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    // 1. Add all nodes
    let graphWithNodes =
        (empty graph.Kind, graph.Nodes)
        ||> Map.fold (fun acc id data -> addNode (f id) data acc)

    // 2. Add all edges
    let isDirected = graph.Kind = Directed

    let edges =
        graph.OutEdges
        |> Map.fold
            (fun accOuter u dests ->
                (accOuter, dests)
                ||> Map.fold (fun acc v weight ->
                    if isDirected || u <= v then
                        addEdge (f u) (f v) weight acc
                    else
                        acc))
            graphWithNodes

    edges

/// Normalizes all node IDs to a continuous range of integers 0..n-1.
/// The mapping is deterministic, based on the sorted order of existing node IDs.
let normalizeNodeIds (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let sortedNodes = graph.Nodes.Keys |> Seq.sort |> Seq.toList
    let mapping = sortedNodes |> List.mapi (fun idx id -> (id, idx)) |> Map.ofList
    relabelNodes (fun id -> Map.find id mapping) graph

/// Mode specifying which edges to follow when constructing an ego graph.
type EgoMode =
    /// Follow only outgoing edges (successors).
    | Successors
    /// Follow both outgoing and incoming edges (neighbors).
    | Neighbors

/// Helper for ego graph BFS traversal.
let private egoBfs (graph: Graph<'n, 'e>) (startNode: NodeId) (radius: int) (mode: EgoMode) : Set<NodeId> =
    let rec doEgoBfs (queue: NodeId list) (dist: int) (visited: Set<NodeId>) =
        if List.isEmpty queue || dist >= radius then
            visited
        else
            let nextQueue =
                queue
                |> List.collect (fun current ->
                    let neighbors =
                        match graph.Kind, mode with
                        | Undirected, _ -> successorIds current graph
                        | Directed, Neighbors -> neighborIds current graph
                        | Directed, Successors -> successorIds current graph

                    neighbors)
                |> List.filter (fun n -> not (Set.contains n visited))
                |> List.distinct

            let newVisited = (visited, nextQueue) ||> List.fold (fun acc n -> Set.add n acc)
            doEgoBfs nextQueue (dist + 1) newVisited

    doEgoBfs [ startNode ] 0 (Set.singleton startNode)

/// Returns the ego graph of a node within a given radius.
/// An ego graph is the subgraph induced by the node (the "ego") and its neighbors (the "alters") within distance `radius`.
let egoGraph (node: NodeId) (radius: int) (mode: EgoMode) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    if radius < 0 then
        invalidArg "radius" "Radius must be non-negative."

    let ids = egoBfs graph node radius mode |> Set.toList
    subgraph ids graph

/// Contracts nodes according to a partition map, producing a quotient graph.
/// Each block (supernode) in the partition becomes a single node in the new graph.
/// Edges between nodes in different blocks become edges between supernodes.
/// Nodes not present in `partition` are treated as singleton blocks (their block ID is themselves).
let quotientGraph
    (partition: Map<NodeId, NodeId>)
    (combineWeight: 'e -> 'e -> 'e)
    (combineData: 'n -> 'n -> 'n)
    (graph: Graph<'n, 'e>)
    : Graph<'n, 'e> =

    let blockFor node =
        Map.tryFind node partition |> Option.defaultValue node

    // 1. Group node data by supernode
    let blockNodes =
        (Map.empty, graph.Nodes)
        ||> Map.fold (fun acc node data ->
            let block = blockFor node

            match Map.tryFind block acc with
            | Some existing -> Map.add block (combineData existing data) acc
            | None -> Map.add block data acc)

    // 2. Aggregate edge weights between different supernodes
    let blockEdges =
        let edgesList =
            graph.OutEdges
            |> Map.fold
                (fun accOuter u dests ->
                    dests
                    |> Map.fold
                        (fun accInner v weight ->
                            if graph.Kind = Directed || u <= v then
                                (u, v, weight) :: accInner
                            else
                                accInner)
                        accOuter)
                []

        (Map.empty, edgesList)
        ||> List.fold (fun acc (u, v, weight) ->
            let bu = blockFor u
            let bv = blockFor v

            if bu = bv then
                acc
            else
                let edgeKey =
                    match graph.Kind with
                    | Directed -> (bu, bv)
                    | Undirected -> if bu <= bv then (bu, bv) else (bv, bu)

                match Map.tryFind edgeKey acc with
                | Some existing -> Map.add edgeKey (combineWeight existing weight) acc
                | None -> Map.add edgeKey weight acc)

    // 3. Build the quotient graph
    let mutable newGraph =
        { Nodes = blockNodes
          OutEdges = Map.empty
          InEdges = Map.empty
          Kind = graph.Kind }

    for KeyValue((fromBlock, toBlock), weight) in blockEdges do
        newGraph <- addEdge fromBlock toBlock weight newGraph

    newGraph
