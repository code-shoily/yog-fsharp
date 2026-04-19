namespace Yog.Multi

open Yog.Model

/// Unique identifier for edges in a multigraph.
type EdgeId = int

/// A multigraph that can hold multiple (parallel) edges between the same pair of nodes.
///
/// ## Type Parameters
/// - `'n`: Node data type (must support equality)
/// - `'e`: Edge data type (must support equality)
///
/// ## Fields
/// - `Kind`: Directed or Undirected
/// - `Nodes`: Map from NodeId to node data
/// - `Edges`: Map from EdgeId to (source, target, data)
/// - `OutEdgeIds`: Map from node to its outgoing edge IDs
/// - `InEdgeIds`: Map from node to its incoming edge IDs
/// - `NextEdgeId`: Counter for generating new edge IDs
type MultiGraph<'n, 'e when 'n: equality and 'e: equality> =
    { Kind: GraphType
      Nodes: Map<NodeId, 'n>
      Edges: Map<EdgeId, NodeId * NodeId * 'e>
      OutEdgeIds: Map<NodeId, EdgeId list>
      InEdgeIds: Map<NodeId, EdgeId list>
      NextEdgeId: EdgeId }

/// Metadata for fold-style walks.
type WalkMetadata =
    {
        /// Current depth in the traversal.
        Depth: int
        /// Parent node and edge ID (if any).
        Parent: (NodeId * EdgeId) option
    }

/// Control flow for walk operations.
type WalkControl =
    | Continue
    | Stop
    | Halt

/// Multigraph data structure supporting parallel edges.
///
/// A multigraph allows multiple edges between the same pair of nodes,
/// useful for modeling scenarios like:
/// - Multi-modal transportation (different routes between cities)
/// - Redundant network connections
/// - Temporal graphs (same edge at different times)
/// - Multiple relationship types (without using hypergraphs)
///
/// ## Comparison with Simple Graph
/// | Aspect | MultiGraph | Graph |
/// |--------|-----------|-------|
/// | Parallel edges | Allowed | Combined/Not allowed |
/// | Edge identity | EdgeId per edge | No identity |
/// | Storage | Map<EdgeId, Edge> | Nested maps |
/// | Use case | Multi-modal, redundant | Standard algorithms |
///
/// ## Operations
/// - `addEdge`: Creates new edge, returns EdgeId
/// - `toSimpleGraph`: Collapse parallel edges using merge function
/// - `toSimpleGraphMinEdges`: Keep minimum weight edge
/// - `toSimpleGraphSumEdges`: Sum parallel edge weights
///
/// ## Example
///
///     open Yog.Multi
///
///     // Model multiple flights between cities
///     let multiGraph =
///         Model.empty Directed
///         |> Model.addNode 0 "NYC"
///         |> Model.addNode 1 "LON"
///         |> fun g ->
///             let g1, _ = Model.addEdge 0 1 (Flight("BA112", 420)) g
///             let g2, _ = Model.addEdge 0 1 (Flight("VS003", 400)) g1
///             g2
///
///     // Collapse to simple graph with cheapest flight
///     let simple = Model.toSimpleGraphMinEdges compare multiGraph
///
module Model =

    /// Creates an empty multigraph.
    ///
    /// ## Parameters
    /// - `kind`: Directed or Undirected
    ///
    /// ## Example
    ///
    ///     let graph = Model.empty Directed
    ///
    let empty kind : MultiGraph<'n, 'e> =
        { Kind = kind
          Nodes = Map.empty
          Edges = Map.empty
          OutEdgeIds = Map.empty
          InEdgeIds = Map.empty
          NextEdgeId = 0 }

    /// Adds a node to the multigraph.
    ///
    /// ## Example
    ///
    ///     let graph = Model.empty Directed |> Model.addNode 0 "A"
    ///
    let addNode id data (graph: MultiGraph<'n, 'e>) =
        { graph with
            Nodes = Map.add id data graph.Nodes }

    /// Adds an edge between two nodes.
    ///
    /// ## Returns
    /// Tuple of (updated graph, new EdgeId)
    ///
    /// ## Example
    ///
    ///     let graph, edgeId = Model.addEdge 0 1 "weight" graph
    ///
    let addEdge src dst data (graph: MultiGraph<'n, 'e>) =
        let eid = graph.NextEdgeId
        let newEdges = Map.add eid (src, dst, data) graph.Edges

        let updateMap id mapping =
            let existing = Map.tryFind id mapping |> Option.defaultValue []
            Map.add id (eid :: existing) mapping

        let out1 = updateMap src graph.OutEdgeIds
        let in1 = updateMap dst graph.InEdgeIds

        let out2, in2 =
            match graph.Kind with
            | Directed -> out1, in1
            | Undirected -> updateMap dst out1, updateMap src in1

        { graph with
            Edges = newEdges
            OutEdgeIds = out2
            InEdgeIds = in2
            NextEdgeId = eid + 1 },
        eid

    /// Gets successors with their EdgeIds.
    ///
    /// ## Returns
    /// List of (target node, EdgeId, edge data)
    let successors id (graph: MultiGraph<'n, 'e>) =
        Map.tryFind id graph.OutEdgeIds
        |> Option.defaultValue []
        |> List.choose (fun eid ->
            match Map.tryFind eid graph.Edges with
            | Some(src, dst, data) when src = id -> Some(dst, eid, data)
            | Some(src, dst, data) when dst = id && graph.Kind = Undirected -> Some(src, eid, data)
            | _ -> None)

    /// Collapses the multigraph into a simple Graph by combining parallel edges.
    ///
    /// ## Parameters
    /// - `combineFn`: Function to merge edge weights when multiple edges exist
    /// - `graph`: The multigraph to collapse
    ///
    /// ## Returns
    /// A simple Graph with combined edges.
    ///
    /// ## Example
    ///
    ///     // Sum parallel edge weights
    ///     let simple = Model.toSimpleGraph (+) multiGraph
    ///
    ///     // Keep maximum weight
    ///     let simple = Model.toSimpleGraph max multiGraph
    ///
    let toSimpleGraph (combineFn: 'e -> 'e -> 'e) (graph: MultiGraph<'n, 'e>) : Graph<'n, 'e> =
        let mutable (simple: Graph<'n, 'e>) = Yog.Model.empty graph.Kind

        for kvp in graph.Nodes do
            simple <- Yog.Model.addNode kvp.Key kvp.Value simple

        for kvp in graph.Edges do
            let (src, dst, data) = kvp.Value

            let existingEdge = Map.tryFind src simple.OutEdges |> Option.bind (Map.tryFind dst)

            match existingEdge with
            | Some existingWeight ->
                let merged = combineFn existingWeight data
                simple <- Yog.Model.addEdge src dst merged simple
            | None -> simple <- Yog.Model.addEdge src dst data simple

        simple

    /// Collapses the multigraph by keeping the minimum weight.
    ///
    /// ## Parameters
    /// - `compareFn`: Comparison function for edge weights
    let toSimpleGraphMinEdges (compareFn: 'e -> 'e -> int) (graph: MultiGraph<'n, 'e>) : Graph<'n, 'e> =
        let minFn (a: 'e) (b: 'e) = if compareFn a b > 0 then b else a
        toSimpleGraph minFn graph

    /// Collapses the multigraph by summing weights.
    ///
    /// ## Parameters
    /// - `addFn`: Addition function for edge weights
    let toSimpleGraphSumEdges (addFn: 'e -> 'e -> 'e) (graph: MultiGraph<'n, 'e>) : Graph<'n, 'e> =
        toSimpleGraph addFn graph

/// Traversal operations for multigraphs.
module Traversal =
    open Model

    /// Breadth-first search traversal.
    ///
    /// ## Returns
    /// List of visited node IDs in BFS order.
    let bfs (source: NodeId) (graph: MultiGraph<'n, 'e>) =
        let rec loop (queue: NodeId list) visitedNodes usedEdges acc =
            match queue with
            | [] -> List.rev acc
            | curr :: rest ->
                let mutable nextQueue = rest
                let mutable vNodes = visitedNodes
                let mutable uEdges = usedEdges

                for (dst, eid, _) in successors curr graph do
                    if not (Set.contains eid uEdges) && not (Set.contains dst vNodes) then
                        vNodes <- Set.add dst vNodes
                        uEdges <- Set.add eid uEdges
                        nextQueue <- nextQueue @ [ dst ]

                loop nextQueue vNodes uEdges (curr :: acc)

        loop [ source ] (Set.singleton source) Set.empty []

/// Eulerian circuit algorithms for multigraphs.
///
/// An Eulerian circuit is a trail that visits every edge exactly once
/// and returns to the starting vertex.
module Eulerian =
    open Model

    let private allEvenDegree (graph: MultiGraph<'n, 'e>) =
        graph.Nodes.Keys |> Seq.forall (fun n -> (successors n graph).Length % 2 = 0)

    let private runHierholzer (start: NodeId) (graph: MultiGraph<'n, 'e>) =
        let rec solve current available path =
            let options =
                successors current graph
                |> List.tryFind (fun (_, eid, _) -> Set.contains eid available)

            match options with
            | None -> available, path
            | Some(next, eid, _) ->
                let av2, built = solve next (Set.remove eid available) path
                av2, eid :: built

        let allEids = graph.Edges |> Map.toSeq |> Seq.map fst |> Set.ofSeq

        let _, path = solve start allEids []

        if List.isEmpty path then None else Some path

    /// Finds an Eulerian circuit if one exists.
    ///
    /// ## Conditions
    /// - Graph must be connected
    /// - All vertices must have even degree (for undirected)
    /// - For directed: in-degree = out-degree for all vertices
    ///
    /// ## Returns
    /// `Some edgeIdList` forming the circuit, or `None` if no circuit exists.
    ///
    /// ## Example
    ///
    ///     match Eulerian.findEulerianCircuit graph with
    ///     | Some path -> printfn "Eulerian circuit: %A" path
    ///     | None -> printfn "No Eulerian circuit exists"
    ///
    /// ## Use Cases
    /// - Route planning (visit every road exactly once)
    /// - DNA sequencing (de Bruijn graphs)
    /// - Chinese Postman Problem
    let findEulerianCircuit (graph: MultiGraph<'n, 'e>) =
        if graph.Nodes.Count = 0 then
            None
        else
            match graph.Nodes |> Map.toSeq |> Seq.tryHead with
            | Some(start, _) -> runHierholzer start graph
            | None -> None
