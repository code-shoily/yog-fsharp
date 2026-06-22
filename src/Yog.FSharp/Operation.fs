/// Graph operations - set-theoretic operations, composition, and structural comparison.
///
/// This module implements binary operations that treat graphs as sets of nodes and edges,
/// following NetworkX's "Graph as a Set" philosophy. These operations allow you to combine,
/// compare, and analyze structural differences between graphs.
///
/// ## Set-Theoretic Operations
///
/// | Function | Description | Complexity |
/// |----------|-------------|------------|
/// | `union` | All nodes and edges from both graphs | O(V₁ + V₂ + E₁ + E₂) |
/// | `intersection` | Only nodes and edges common to both | O(V + E) |
/// | `difference` | Nodes/edges in first but not second | O(V + E) |
/// | `symmetricDifference` | Nodes/edges unique to each graph | O(V₁ + V₂ + E₁ + E₂) |
///
/// ## Composition & Joins
///
/// | Function | Description | Complexity |
/// |----------|-------------|------------|
/// | `disjointUnion` | Combine with automatic ID re-indexing | O(V₁ + V₂ + E₁ + E₂) |
/// | `cartesianProduct` | Multiply graphs (grids, hypercubes) | O(V₁ × V₂ + E₁ × V₂ + E₂ × V₁) |
/// | `tensorProduct` | Kronecker/direct product | O(V₁ × V₂ + E₁ × E₂) |
/// | `strongProduct` | Union of Cartesian and tensor products | O(V₁ × V₂ + E₁ × V₂ + E₂ × V₁ + E₁ × E₂) |
/// | `lexicographicProduct` | Lexicographic/composition product | O(V₁ × V₂ + E₁ × V₂² + V₁ × E₂) |
/// | `compose` | Merge overlapping graphs (same as `union`) | O(V₁ + V₂ + E₁ + E₂) |
/// | `lineGraph` | Edges become nodes | O(E²) |
/// | `power` | k-th power (connect nodes within distance k) | O(V × (V + E)) |
///
/// ## Structural Comparison
///
/// | Function | Description | Complexity |
/// |----------|-------------|------------|
/// | `isSubgraph` | Check if first is subset of second | O(Vₚ + Eₚ) |
/// | `isomorphic` | Check if graphs are structurally identical | O(V log V + E) fast checks; exponential worst case |
module Yog.Operation

open Yog.Model
open Yog.Transform

// =============================================================================
// Private Helpers
// =============================================================================

/// Returns the set of node IDs in a graph.
let private nodeSet (graph: Graph<'n, 'e>) : Set<NodeId> =
    graph.Nodes |> Map.toSeq |> Seq.map fst |> Set.ofSeq

/// Checks if an edge exists from `src` to `dst` in the graph.
let private hasEdge (src: NodeId) (dst: NodeId) (graph: Graph<'n, 'e>) : bool =
    graph.OutEdges
    |> Map.tryFind src
    |> Option.map (Map.containsKey dst)
    |> Option.defaultValue false

/// Out-degree of a node (number of outgoing edges).
let private outDegree (id: NodeId) (graph: Graph<'n, 'e>) : int =
    graph.OutEdges
    |> Map.tryFind id
    |> Option.map (fun m -> m.Count)
    |> Option.defaultValue 0

/// In-degree of a node (number of incoming edges).
let private inDegree (id: NodeId) (graph: Graph<'n, 'e>) : int =
    graph.InEdges
    |> Map.tryFind id
    |> Option.map (fun m -> m.Count)
    |> Option.defaultValue 0

/// Total degree of a node. For undirected graphs this equals the out-degree.
let private totalDegree (id: NodeId) (graph: Graph<'n, 'e>) : int =
    match graph.Kind with
    | Directed -> outDegree id graph + inDegree id graph
    | Undirected -> outDegree id graph

/// Returns all directed edges as `(src, dst, weight)` triples.
/// For undirected graphs each logical edge appears once with `src <= dst`.
let private normalizedEdges (graph: Graph<'n, 'e>) : (NodeId * NodeId * 'e) list =
    graph.OutEdges
    |> Map.toList
    |> List.collect (fun (src, dests) ->
        dests
        |> Map.toList
        |> List.choose (fun (dst, weight) ->
            match graph.Kind with
            | Directed -> Some(src, dst, weight)
            | Undirected -> if src <= dst then Some(src, dst, weight) else None))

/// Adds a directed edge without checking node existence. Used internally for
/// building result graphs where nodes are already known to exist.
let private addDirectedEdgeUnchecked (src: NodeId) (dst: NodeId) (weight: 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let updateOuter key mapOpt =
        match mapOpt with
        | Some m -> Some(Map.add key weight m)
        | None -> Some(Map.ofList [ (key, weight) ])

    { graph with
        OutEdges = graph.OutEdges |> Map.change src (updateOuter dst)
        InEdges = graph.InEdges |> Map.change dst (updateOuter src) }

/// Adds an edge, mirroring it for undirected graphs.
let private addEdgeUnchecked (src: NodeId) (dst: NodeId) (weight: 'e) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let g = addDirectedEdgeUnchecked src dst weight graph

    match g.Kind with
    | Directed -> g
    | Undirected -> addDirectedEdgeUnchecked dst src weight g

/// Adds a node only if it does not already exist.
let private ensureNode (id: NodeId) (data: 'n) (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    if graph.Nodes |> Map.containsKey id then
        graph
    else
        { graph with
            Nodes = graph.Nodes |> Map.add id data }

/// Merges two outer edge maps. Values from `other` overwrite `base` on conflict.
let private mergeOuter
    (baseOuter: Map<NodeId, Map<NodeId, 'e>>)
    (otherOuter: Map<NodeId, Map<NodeId, 'e>>)
    : Map<NodeId, Map<NodeId, 'e>> =
    (baseOuter, otherOuter)
    ||> Map.fold (fun acc src inner2 ->
        match Map.tryFind src acc with
        | Some inner1 -> Map.add src (inner2 |> Map.fold (fun i k v -> Map.add k v i) inner1) acc
        | None -> Map.add src inner2 acc)

// =============================================================================
// Set-Theoretic Operations
// =============================================================================

/// Returns a graph containing all nodes and edges from both input graphs.
///
/// Node data and edge weights from `other` take precedence on conflicts.
/// The resulting graph inherits the kind (`Directed` or `Undirected`) from `base`.
///
/// **Time Complexity:** O(V₁ + V₂ + E₁ + E₂)
///
/// ## Example
///
///     let g1 = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 1
///     let g2 = empty Undirected |> addNode 2 "B" |> addNode 3 "C" |> addEdge 2 3 1
///     let combined = union g1 g2 // nodes 1, 2, 3; edges 1-2, 2-3
///
/// ## Parameters
/// - `base`: The base graph whose kind is preserved.
/// - `other`: The graph to merge into the base graph.
/// ## Returns
/// A new graph containing the union of both graphs.
let union (baseGraph: Graph<'n, 'e>) (other: Graph<'n, 'e>) : Graph<'n, 'e> = Yog.Transform.merge baseGraph other

/// Returns a graph containing only nodes and edges that exist in both input graphs.
///
/// For directed graphs, a directed edge must exist in both graphs to be kept.
/// For undirected graphs, an undirected edge must exist in both graphs.
/// The resulting graph has the kind of `first` and keeps weights from `first`.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
///     let g1 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addNode 3 "" |> addEdge 1 2 1 |> addEdge 2 3 1
///     let g2 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addNode 3 "" |> addEdge 1 2 1
///     let common = intersection g1 g2 // nodes 1, 2, 3; only edge 1-2
let intersection (first: Graph<'n, 'e>) (second: Graph<'n, 'e>) : Graph<'n, 'e> =
    let commonIds = Set.intersect (nodeSet first) (nodeSet second)

    let keptNodes = first.Nodes |> Map.filter (fun id _ -> Set.contains id commonIds)

    let keepEdge src dst _ = hasEdge src dst second

    let filterOuter outerMap =
        outerMap
        |> Map.filter (fun src _ -> Set.contains src commonIds)
        |> Map.map (fun src innerMap -> innerMap |> Map.filter (fun dst w -> keepEdge src dst w))
        |> Map.filter (fun _ innerMap -> not innerMap.IsEmpty)

    { first with
        Nodes = keptNodes
        OutEdges = filterOuter first.OutEdges
        InEdges = filterOuter first.InEdges }

/// Returns a graph containing nodes and edges that exist in the first graph
/// but not in the second.
///
/// Any node that appears in `second` is removed from the result, along with all
/// its incident edges. Of the remaining nodes, only edges that do not appear in
/// `second` are kept. Weights are taken from `first`.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
///     let g1 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
///     let g2 = empty Undirected |> addNode 3 ""
///     let diff = difference g1 g2 // nodes 1, 2; edge 1-2
let difference (first: Graph<'n, 'e>) (second: Graph<'n, 'e>) : Graph<'n, 'e> =
    let secondIds = nodeSet second
    let keptIds = Set.difference (nodeSet first) secondIds

    let keptNodes = first.Nodes |> Map.filter (fun id _ -> Set.contains id keptIds)

    let keepEdge src dst _ = not (hasEdge src dst second)

    let filterOuter outerMap =
        outerMap
        |> Map.filter (fun src _ -> Set.contains src keptIds)
        |> Map.map (fun src innerMap -> innerMap |> Map.filter (fun dst w -> keepEdge src dst w))
        |> Map.filter (fun _ innerMap -> not innerMap.IsEmpty)

    { first with
        Nodes = keptNodes
        OutEdges = filterOuter first.OutEdges
        InEdges = filterOuter first.InEdges }

/// Returns a graph containing nodes and edges that are unique to each input graph.
///
/// The result is equivalent to the union of `difference(first, second)` and
/// `difference(second, first)`. Nodes that appear in both graphs are removed,
/// along with all their incident edges.
///
/// The resulting graph inherits the kind of `first`.
///
/// **Time Complexity:** O(V₁ + V₂ + E₁ + E₂)
///
/// ## Example
///
///     let g1 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
///     let g2 = empty Undirected |> addNode 2 "" |> addNode 3 "" |> addEdge 2 3 1
///     let sym = symmetricDifference g1 g2 // only node 1 (no incident edges)
let symmetricDifference (first: Graph<'n, 'e>) (second: Graph<'n, 'e>) : Graph<'n, 'e> =
    let firstOnly = Set.difference (nodeSet first) (nodeSet second)
    let secondOnly = Set.difference (nodeSet second) (nodeSet first)

    let nodes =
        let firstPart = first.Nodes |> Map.filter (fun id _ -> Set.contains id firstOnly)
        let secondPart = second.Nodes |> Map.filter (fun id _ -> Set.contains id secondOnly)
        secondPart |> Map.fold (fun acc id data -> Map.add id data acc) firstPart

    let mutable result =
        { first with
            Nodes = nodes
            OutEdges = Map.empty
            InEdges = Map.empty }

    // Add edges within first-only nodes.
    for (src, dst, weight) in normalizedEdges first do
        if Set.contains src firstOnly && Set.contains dst firstOnly then
            result <- addEdgeUnchecked src dst weight result

    // Add edges within second-only nodes.
    for (src, dst, weight) in normalizedEdges second do
        if Set.contains src secondOnly && Set.contains dst secondOnly then
            result <- addEdgeUnchecked src dst weight result

    result

/// Computes the disjoint union of two graphs.
///
/// Unlike a simple union, this function guarantees that nodes from `first` and
/// `second` remain distinct by shifting the node IDs of `second` so that they do
/// not collide with IDs from `first`. The resulting graph uses the kind of `first`.
///
/// The shift amount is `maxId(first) + 1` (or `0` if `first` is empty). A node
/// with ID `id` from `second` will appear in the result with ID `id + shift`.
///
/// **Time Complexity:** O(V₁ + V₂ + E₁ + E₂)
///
/// ## Example
///
///     let g1 = empty Directed |> addNode 0 "A"
///     let g2 = empty Directed |> addNode 0 "B"
///     let combined = disjointUnion g1 g2
///     // combined has nodes 0 and 1 (node 0 from g2 shifted to 1)
let disjointUnion (first: Graph<'n, 'e>) (second: Graph<'n, 'e>) : Graph<'n, 'e> =
    let maxId =
        if first.Nodes.IsEmpty then
            -1
        else
            first.Nodes |> Map.toList |> List.map fst |> List.max

    let shift = maxId + 1

    let shiftedNodes =
        second.Nodes
        |> Map.toList
        |> List.map (fun (id, data) -> (id + shift, data))
        |> Map.ofList

    let shiftOuter outerMap =
        outerMap
        |> Map.toList
        |> List.map (fun (src, innerMap) ->
            let shiftedInner =
                innerMap
                |> Map.toList
                |> List.map (fun (dst, weight) -> (dst + shift, weight))
                |> Map.ofList

            (src + shift, shiftedInner))
        |> Map.ofList

    { first with
        Nodes = first.Nodes |> Map.fold (fun acc id data -> Map.add id data acc) shiftedNodes
        OutEdges = mergeOuter first.OutEdges (shiftOuter second.OutEdges)
        InEdges = mergeOuter first.InEdges (shiftOuter second.InEdges) }

// =============================================================================
// Graph Products
// =============================================================================

/// Builds the node index maps used by product operations.
/// Returns `(uIndexMap, vIndexMap, secondOrder)`.
let private productIndexMaps (first: Graph<'n, 'e>) (second: Graph<'m, 'f>) =
    let firstNodes = first.Nodes |> Map.toList |> List.map fst
    let secondNodes = second.Nodes |> Map.toList |> List.map fst
    let secondOrder = List.length secondNodes

    let uMap = firstNodes |> List.mapi (fun i u -> (u, i)) |> Map.ofList
    let vMap = secondNodes |> List.mapi (fun i v -> (v, i)) |> Map.ofList
    (uMap, vMap, secondOrder)

/// Computes the integer ID of a product node.
let private productNodeId (uIdx: int) (vIdx: int) (secondOrder: int) : NodeId = uIdx * secondOrder + vIdx

/// Adds all Cartesian-product nodes to a result graph.
let private addProductNodes
    (first: Graph<'n, 'e>)
    (second: Graph<'m, 'f>)
    (uMap: Map<NodeId, int>)
    (vMap: Map<NodeId, int>)
    (secondOrder: int)
    (graph: Graph<'n * 'm, 'a>)
    : Graph<'n * 'm, 'a> =
    List.fold
        (fun gAcc (u, uData) ->
            let uIdx = uMap.[u]

            List.fold
                (fun g (v, vData) ->
                    let vIdx = vMap.[v]
                    let id = productNodeId uIdx vIdx secondOrder
                    ensureNode id (uData, vData) g)
                gAcc
                (second.Nodes |> Map.toList))
        graph
        (first.Nodes |> Map.toList)

/// Adds vertical Cartesian edges (edges derived from the second graph).
let private addProductVerticalEdges
    (first: Graph<'n, 'e>)
    (second: Graph<'m, 'f>)
    (uMap: Map<NodeId, int>)
    (vMap: Map<NodeId, int>)
    (secondOrder: int)
    (defaultFirst: 'e)
    (graph: Graph<'n * 'm, 'e * 'f>)
    : Graph<'n * 'm, 'e * 'f> =
    List.fold
        (fun gAcc (u, _) ->
            let uIdx = uMap.[u]

            List.fold
                (fun g (v, vSucc, weightSecond) ->
                    let vIdx = vMap.[v]
                    let vSuccIdx = vMap.[vSucc]
                    let src = productNodeId uIdx vIdx secondOrder
                    let dst = productNodeId uIdx vSuccIdx secondOrder
                    addEdgeUnchecked src dst (defaultFirst, weightSecond) g)
                gAcc
                (normalizedEdges second))
        graph
        (first.Nodes |> Map.toList)

/// Adds horizontal Cartesian edges (edges derived from the first graph).
let private addProductHorizontalEdges
    (first: Graph<'n, 'e>)
    (second: Graph<'m, 'f>)
    (uMap: Map<NodeId, int>)
    (vMap: Map<NodeId, int>)
    (secondOrder: int)
    (defaultSecond: 'f)
    (graph: Graph<'n * 'm, 'e * 'f>)
    : Graph<'n * 'm, 'e * 'f> =
    List.fold
        (fun gAcc (v, _) ->
            let vIdx = vMap.[v]

            List.fold
                (fun g (u, uSucc, weightFirst) ->
                    let uIdx = uMap.[u]
                    let uSuccIdx = uMap.[uSucc]
                    let src = productNodeId uIdx vIdx secondOrder
                    let dst = productNodeId uSuccIdx vIdx secondOrder
                    addEdgeUnchecked src dst (weightFirst, defaultSecond) g)
                gAcc
                (normalizedEdges first))
        graph
        (second.Nodes |> Map.toList)

/// Adds tensor-product edges (edges derived from both graphs simultaneously).
let private addTensorEdges
    (first: Graph<'n, 'e>)
    (second: Graph<'m, 'f>)
    (uMap: Map<NodeId, int>)
    (vMap: Map<NodeId, int>)
    (secondOrder: int)
    (graph: Graph<'n * 'm, 'e * 'f>)
    : Graph<'n * 'm, 'e * 'f> =
    List.fold
        (fun gAcc (u, uSucc, weightFirst) ->
            let uIdx = uMap.[u]
            let uSuccIdx = uMap.[uSucc]

            List.fold
                (fun g (v, vSucc, weightSecond) ->
                    let vIdx = vMap.[v]
                    let vSuccIdx = vMap.[vSucc]
                    let src = productNodeId uIdx vIdx secondOrder
                    let dst = productNodeId uSuccIdx vSuccIdx secondOrder
                    addEdgeUnchecked src dst (weightFirst, weightSecond) g)
                gAcc
                (second.OutEdges
                 |> Map.toList
                 |> List.collect (fun (src, dests) -> dests |> Map.toList |> List.map (fun (dst, w) -> (src, dst, w)))))
        graph
        (normalizedEdges first)

/// Adds lexicographic-product horizontal edges.
let private addLexicographicHorizontalEdges
    (first: Graph<'n, 'e>)
    (second: Graph<'m, 'f>)
    (uMap: Map<NodeId, int>)
    (vMap: Map<NodeId, int>)
    (secondOrder: int)
    (defaultFirst: 'f)
    (graph: Graph<'n * 'm, 'e * 'f>)
    : Graph<'n * 'm, 'e * 'f> =
    let secondNodeIds = second.Nodes |> Map.toList |> List.map fst

    List.fold
        (fun gAcc (u, uSucc, weightFirst) ->
            let uIdx = uMap.[u]
            let uSuccIdx = uMap.[uSucc]

            List.fold
                (fun gInner u2 ->
                    let u2Idx = vMap.[u2]
                    let src = productNodeId uIdx u2Idx secondOrder

                    List.fold
                        (fun gEdge v2 ->
                            let v2Idx = vMap.[v2]
                            let dst = productNodeId uSuccIdx v2Idx secondOrder
                            addEdgeUnchecked src dst (weightFirst, defaultFirst) gEdge)
                        gInner
                        secondNodeIds)
                gAcc
                secondNodeIds)
        graph
        (normalizedEdges first)

/// Returns the Cartesian product of two graphs.
///
/// Creates a new graph where each node represents a pair of nodes from the input
/// graphs. Useful for generating grids, hypercubes, and other complex structures.
///
/// > **Performance Warning:** The size of the resulting graph grows quadratically.
/// > It contains V₁ × V₂ nodes and E₁ × V₂ + E₂ × V₁ edges.
///
/// **Time Complexity:** O(V₁ × V₂ + E₁ × V₂ + E₂ × V₁)
///
/// ## Parameters
/// - `defaultFirst`: Default edge data for edges derived from `first`.
/// - `defaultSecond`: Default edge data for edges derived from `second`.
/// - `first`: First input graph.
/// - `second`: Second input graph.
/// ## Returns
/// A new graph representing the Cartesian product.
let cartesianProduct
    (defaultFirst: 'e)
    (defaultSecond: 'f)
    (first: Graph<'n, 'e>)
    (second: Graph<'m, 'f>)
    : Graph<'n * 'm, 'e * 'f> =
    let uMap, vMap, secondOrder = productIndexMaps first second

    empty first.Kind
    |> addProductNodes first second uMap vMap secondOrder
    |> addProductVerticalEdges first second uMap vMap secondOrder defaultFirst
    |> addProductHorizontalEdges first second uMap vMap secondOrder defaultSecond

/// Returns the Tensor product (also known as Kronecker or direct product) of two graphs.
///
/// The Tensor product G₁ × G₂ has node set V(G₁) × V(G₂). An edge exists between
/// (u₁, u₂) and (v₁, v₂) iff u₁ → v₁ is an edge in G₁ and u₂ → v₂ is an edge in G₂.
/// Edge weights are pairs `(weightFirst, weightSecond)`.
///
/// > **Performance Warning:** The resulting graph contains V₁ × V₂ nodes and
/// > E₁ × E₂ edges.
///
/// **Time Complexity:** O(V₁ × V₂ + E₁ × E₂)
let tensorProduct (first: Graph<'n, 'e>) (second: Graph<'m, 'f>) : Graph<'n * 'm, 'e * 'f> =
    let uMap, vMap, secondOrder = productIndexMaps first second

    empty first.Kind
    |> addProductNodes first second uMap vMap secondOrder
    |> addTensorEdges first second uMap vMap secondOrder

/// Returns the Strong product of two graphs.
///
/// The Strong product G₁ ⊠ G₂ contains the union of Cartesian and Tensor product
/// edges. Edge weights are pairs as documented in `cartesianProduct` and
/// `tensorProduct`.
///
/// **Time Complexity:** O(V₁ × V₂ + E₁ × V₂ + E₂ × V₁ + E₁ × E₂)
let strongProduct
    (defaultFirst: 'e)
    (defaultSecond: 'f)
    (first: Graph<'n, 'e>)
    (second: Graph<'m, 'f>)
    : Graph<'n * 'm, 'e * 'f> =
    let uMap, vMap, secondOrder = productIndexMaps first second

    empty first.Kind
    |> addProductNodes first second uMap vMap secondOrder
    |> addProductVerticalEdges first second uMap vMap secondOrder defaultFirst
    |> addProductHorizontalEdges first second uMap vMap secondOrder defaultSecond
    |> addTensorEdges first second uMap vMap secondOrder

/// Returns the Lexicographic product (composition) of two graphs.
///
/// The Lexicographic product G₁[G₂] has node set V(G₁) × V(G₂). An edge exists
/// between (u₁, u₂) and (v₁, v₂) iff either u₁ → v₁ in G₁ (for all u₂, v₂), or
/// u₁ = v₁ and u₂ → v₂ in G₂.
///
/// **Time Complexity:** O(V₁ × V₂ + E₁ × V₂² + V₁ × E₂)
let lexicographicProduct
    (defaultFirst: 'f)
    (defaultSecond: 'e)
    (first: Graph<'n, 'e>)
    (second: Graph<'m, 'f>)
    : Graph<'n * 'm, 'e * 'f> =
    let uMap, vMap, secondOrder = productIndexMaps first second

    empty first.Kind
    |> addProductNodes first second uMap vMap secondOrder
    |> addProductVerticalEdges first second uMap vMap secondOrder defaultSecond
    |> addLexicographicHorizontalEdges first second uMap vMap secondOrder defaultFirst

/// Composes two graphs by merging overlapping nodes and combining their edges.
///
/// This is equivalent to `union` — both graphs are merged together with `other`'s
/// data taking precedence on conflicts.
///
/// **Time Complexity:** O(V₁ + V₂ + E₁ + E₂)
let compose (first: Graph<'n, 'e>) (second: Graph<'n, 'e>) : Graph<'n, 'e> = union first second

// =============================================================================
// Line Graph & Power
// =============================================================================

/// Bijective pairing of two non-negative integers into a single integer.
/// Used to encode edge endpoints as line-graph node IDs.
let private cantorPair (a: int) (b: int) : int = (a + b) * (a + b + 1) / 2 + b

/// Node ID for a line-graph node representing edge `(u, v)`.
let private lineGraphNodeId (kind: GraphType) (u: NodeId) (v: NodeId) : NodeId =
    match kind with
    | Directed -> cantorPair u v
    | Undirected ->
        let a, b = min u v, max u v
        cantorPair a b

/// Returns the line graph of a graph.
///
/// The line graph L(G) is a graph where each node represents an edge of G, and two
/// nodes are adjacent iff their corresponding edges share a common endpoint.
///
/// For directed graphs, two edges (u, v) and (x, y) are adjacent iff v = x.
/// For undirected graphs, two edges share an endpoint.
///
/// Line-graph node IDs are derived from the original endpoints via a pairing
/// function. The node data is the weight of the represented edge; edges in the
/// line graph carry `defaultWeight`.
///
/// > **Performance Warning:** This operation has O(E²) time complexity and is not
/// > recommended for very dense or large graphs.
///
/// ## Parameters
/// - `defaultWeight`: Weight for edges in the line graph.
/// - `graph`: The input graph.
/// ## Returns
/// The line graph of the input graph.
let lineGraph (defaultWeight: 'lw) (graph: Graph<'n, 'e>) : Graph<'e, 'lw> =
    let edges = normalizedEdges graph

    // Create nodes for each edge.
    let mutable result = empty graph.Kind

    for (u, v, weight) in edges do
        let id = lineGraphNodeId graph.Kind u v
        result <- ensureNode id weight result

    // Connect line-graph nodes.
    match graph.Kind with
    | Directed ->
        for (u, v, _) in edges do
            let srcId = lineGraphNodeId Directed u v

            for (y, _) in successors v graph do
                let dstId = lineGraphNodeId Directed v y

                if srcId <> dstId then
                    result <- addEdgeUnchecked srcId dstId defaultWeight result

        result
    | Undirected ->
        // For each original node, connect all pairs of incident edges.
        for nodeId in allNodes graph do
            let incident =
                neighbors nodeId graph
                |> List.map (fun (succ, _) ->
                    let a, b = min nodeId succ, max nodeId succ
                    lineGraphNodeId Undirected a b)
                |> List.sort
                |> List.distinct

            let rec connectPairs edges acc =
                match edges with
                | [] -> acc
                | e1 :: rest ->
                    let newAcc =
                        rest
                        |> List.fold
                            (fun a e2 ->
                                if e1 <> e2 then
                                    addEdgeUnchecked e1 e2 defaultWeight a
                                else
                                    a)
                            acc

                    connectPairs rest newAcc

            result <- connectPairs incident result

        result

/// Returns the k-th power of a graph.
///
/// The k-th power of a graph G, denoted G^k, is a graph where two nodes are
/// adjacent iff their distance in G is at most `k`. Self-loops are never added.
///
/// > **Performance Warning:** Computing the k-th power requires running BFS from
/// > every node. The resulting graph can approach a complete graph.
///
/// **Time Complexity:** O(V × (V + E)) in the worst case.
///
/// ## Parameters
/// - `k`: The distance threshold (must be >= 1).
/// - `defaultWeight`: Weight for newly created edges.
/// - `graph`: The input graph.
/// ## Returns
/// The k-th power of the input graph.
let power (k: int) (defaultWeight: 'w) (graph: Graph<'n, 'e>) : Graph<'n, 'w> =
    if k <= 1 then
        mapNodes (fun _ -> Unchecked.defaultof<'n>) graph
        |> mapEdges (fun _ -> defaultWeight)
    else
        let mutable result = mapEdges (fun _ -> defaultWeight) graph

        for src in allNodes graph do
            let reachable =
                Yog.Traversal.foldWalk
                    src
                    Yog.Traversal.BreadthFirst
                    []
                    (fun acc nodeId meta ->
                        if meta.Depth <= k then
                            (Yog.Traversal.Continue, nodeId :: acc)
                        else
                            (Yog.Traversal.Continue, acc))
                    graph

            for dst in reachable do
                if src <> dst && not (hasEdge src dst result) then
                    result <- addEdgeUnchecked src dst defaultWeight result

        result

// =============================================================================
// Structural Comparison
// =============================================================================

/// Checks if the first graph is a subgraph of the second graph.
///
/// Returns `true` if all nodes and edges in `potential` exist in `container`.
/// Node data is not compared; only the existence of nodes and edges matters.
///
/// **Time Complexity:** O(Vₚ + Eₚ)
///
/// ## Example
///
///     let container = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addNode 3 "" |> addEdge 1 2 1 |> addEdge 2 3 1
///     let potential = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
///     isSubgraph potential container // true
let isSubgraph (potential: Graph<'n, 'e>) (container: Graph<'n, 'e>) : bool =
    let containerNodes = nodeSet container

    let allNodesPresent =
        potential.Nodes |> Map.forall (fun id _ -> Set.contains id containerNodes)

    if not allNodesPresent then
        false
    else
        potential.OutEdges
        |> Map.forall (fun src innerMap -> innerMap |> Map.forall (fun dst _ -> hasEdge src dst container))

/// Checks if two graphs are isomorphic (structurally identical).
///
/// Two graphs are isomorphic if there exists a bijection between their node sets
/// that preserves adjacency. This implementation uses degree sequence comparison
/// and backtracking.
///
/// > **Performance Warning:** Graph isomorphism is computationally hard. While
/// > fast heuristics are included, the worst-case backtracking complexity is
/// > exponential. Avoid using this on graphs with more than a few dozen nodes,
/// > especially highly symmetric graphs.
///
/// **Time Complexity:** O(V log V + E) for fast checks; exponential worst case.
let isomorphic (first: Graph<'n, 'e>) (second: Graph<'n, 'e>) : bool =
    if order first <> order second then
        false
    else if edgeCount first <> edgeCount second then
        false
    else if first.Kind <> second.Kind then
        false
    else
        let degreeOf graph id =
            match graph.Kind with
            | Directed -> (inDegree id graph, outDegree id graph)
            | Undirected -> (totalDegree id graph, 0)

        let firstDegrees =
            first.Nodes
            |> Map.toList
            |> List.map (fun (id, _) -> degreeOf first id)
            |> List.sort

        let secondDegrees =
            second.Nodes
            |> Map.toList
            |> List.map (fun (id, _) -> degreeOf second id)
            |> List.sort

        if firstDegrees <> secondDegrees then
            false
        else
            let firstNodes =
                first.Nodes
                |> Map.toList
                |> List.map fst
                |> List.sortByDescending (fun id ->
                    match first.Kind with
                    | Directed -> inDegree id first + outDegree id first
                    | Undirected -> totalDegree id first)

            let secondNodes = second.Nodes |> Map.toList |> List.map fst

            let rec mappingValid mapping src candidate =
                let srcSuccs = first.OutEdges |> Map.tryFind src |> Option.defaultValue Map.empty

                let candSuccs =
                    second.OutEdges |> Map.tryFind candidate |> Option.defaultValue Map.empty

                let consistentOut =
                    mapping
                    |> Map.forall (fun s c -> Map.containsKey s srcSuccs = Map.containsKey c candSuccs)

                let srcPreds = first.InEdges |> Map.tryFind src |> Option.defaultValue Map.empty

                let candPreds =
                    second.InEdges |> Map.tryFind candidate |> Option.defaultValue Map.empty

                let consistentIn =
                    mapping
                    |> Map.forall (fun s c -> Map.containsKey s srcPreds = Map.containsKey c candPreds)

                consistentOut && consistentIn

            let rec tryMapping remaining available mapping =
                match remaining with
                | [] -> true
                | src :: rest ->
                    let srcDeg = degreeOf first src

                    let candidates =
                        available |> List.filter (fun cand -> degreeOf second cand = srcDeg)

                    candidates
                    |> List.exists (fun candidate ->
                        if mappingValid mapping src candidate then
                            let newMapping = Map.add src candidate mapping
                            let newAvailable = available |> List.filter (fun n -> n <> candidate)
                            tryMapping rest newAvailable newMapping
                        else
                            false)

            tryMapping firstNodes secondNodes Map.empty
