module Yog.FSharp.Tests.AlgorithmPropertyTests


open Xunit
open Yog.Model
open Yog.Connectivity
open Yog.Centrality
open Yog.Pathfinding.Dijkstra
open Yog.Pathfinding.BellmanFord
open Yog.Traversal

// ============================================================================
// HELPERS
// ============================================================================

/// Check if all edges in a path exist in the graph
let rec isValidPath (graph: Graph<'n, int>) (path: NodeId list) : bool =
    match path with
    | []
    | [ _ ] -> true
    | first :: second :: rest ->
        let edgeExists = successors first graph |> List.exists (fun (id, _) -> id = second)

        edgeExists && isValidPath graph (second :: rest)

/// Calculate total weight of a path
let rec calculatePathWeight (graph: Graph<'n, int>) (path: NodeId list) : int =
    match path with
    | []
    | [ _ ] -> 0
    | first :: second :: rest ->
        let edgeWeight =
            successors first graph
            |> List.tryFind (fun (id, _) -> id = second)
            |> Option.map snd
            |> Option.defaultValue 0

        edgeWeight + calculatePathWeight graph (second :: rest)

/// Check if node b is reachable from node a via BFS
let isReachable (graph: Graph<'n, 'e>) (from: NodeId) (target: NodeId) : bool =
    let visited = walk from BreadthFirst graph
    List.contains target visited

// ============================================================================
// CATEGORY 1: ALGORITHM CROSS-VALIDATION
// ============================================================================

// ----------------------------------------------------------------------------
// SCC: Tarjan vs Kosaraju - Both should find same strongly connected components
// ----------------------------------------------------------------------------

[<Fact>]
let ``SCC Tarjan equals Kosaraju`` () =
    // Example graph with known SCC structure
    // Graph with 2 SCCs: {0, 1} and {2}
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 1
        |> addEdge 1 0 1
        |> addEdge 1 2 1

    let tarjan = stronglyConnectedComponents graph
    let kosaraju' = kosaraju graph

    // Convert to sets for comparison (order doesn't matter)
    let tarjanSets = tarjan |> List.map Set.ofList |> Set.ofList

    let kosarajuSets = kosaraju' |> List.map Set.ofList |> Set.ofList

    // Both algorithms should find the same components
    Assert.Equal<Set<Set<NodeId>>>(tarjanSets, kosarajuSets)

// ----------------------------------------------------------------------------
// Pathfinding: Bellman-Ford vs Dijkstra on non-negative graphs
// ----------------------------------------------------------------------------

[<Fact>]
let ``Bellman-Ford equals Dijkstra on non-negative graphs`` () =
    // Small graph with non-negative weights
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 5
        |> addEdge 1 2 3
        |> addEdge 0 2 10

    let dijkstraResult = shortestPath 0 (+) compare 0 2 graph

    let bellmanResult = bellmanFord 0 (+) compare 0 2 graph

    // Both should find same path weight
    match dijkstraResult, bellmanResult with
    | Some dPath, ShortestPath bPath -> Assert.Equal(dPath.TotalWeight, bPath.TotalWeight)
    | None, NoPath -> ()
    | _ -> Assert.True(false, "Dijkstra and Bellman-Ford disagree on path existence!")

// ============================================================================
// CATEGORY 2: PATHFINDING CORRECTNESS
// ============================================================================

// ----------------------------------------------------------------------------
// Property: Dijkstra path is valid and connects start to goal
// ----------------------------------------------------------------------------

[<Fact>]
let ``Dijkstra path is valid`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addNode 3 3
        |> addEdge 0 1 1
        |> addEdge 1 2 2
        |> addEdge 2 3 3

    match shortestPath 0 (+) compare 0 3 graph with
    | Some path ->
        // Path should start at start node
        Assert.Equal(0, List.head path.Nodes)

        // Path should end at goal node
        Assert.Equal(3, List.last path.Nodes)

        // All edges in path should exist
        Assert.True(isValidPath graph path.Nodes)

        // Weight should match actual path weight
        let calculated = calculatePathWeight graph path.Nodes
        Assert.Equal(path.TotalWeight, calculated)
    | None -> Assert.True(false, "Path should exist!")

// ----------------------------------------------------------------------------
// Property: No path should return None and BFS should confirm
// ----------------------------------------------------------------------------

[<Fact>]
let ``Dijkstra no-path confirmed by BFS`` () =
    // Disconnected graph
    let graph = empty Directed |> addNode 0 0 |> addNode 1 1

    // No edge between them
    let dijkstraResult = shortestPath 0 (+) compare 0 1 graph
    let bfsReachable = isReachable graph 0 1

    match dijkstraResult with
    | None -> Assert.False(bfsReachable, "BFS should also confirm no path")
    | Some _ -> Assert.True(false, "Should not find path in disconnected graph")

// ----------------------------------------------------------------------------
// Property: Undirected path symmetry
// ----------------------------------------------------------------------------

[<Fact>]
let ``undirected path is symmetric`` () =
    let graph =
        empty Undirected
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 3
        |> addEdge 1 2 4

    let forward = shortestPath 0 (+) compare 0 2 graph
    let backward = shortestPath 0 (+) compare 2 0 graph

    match forward, backward with
    | Some fPath, Some bPath ->
        // Weights should be equal for undirected graph
        Assert.Equal(fPath.TotalWeight, bPath.TotalWeight)
    | None, None -> ()
    | _ -> Assert.True(false, "Symmetric paths should both exist or both not exist!")

// ----------------------------------------------------------------------------
// Property: Triangle inequality (path via intermediate >= direct path)
// ----------------------------------------------------------------------------

[<Fact>]
let ``triangle inequality holds`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 5
        |> addEdge 1 2 3
        |> addEdge 0 2 7

    let direct = shortestPath 0 (+) compare 0 2 graph
    let via1Part1 = shortestPath 0 (+) compare 0 1 graph
    let via1Part2 = shortestPath 0 (+) compare 1 2 graph

    match direct, via1Part1, via1Part2 with
    | Some d, Some p1, Some p2 ->
        let viaWeight = p1.TotalWeight + p2.TotalWeight
        // Direct path should be <= path via intermediate node
        Assert.True(d.TotalWeight <= viaWeight)
    | _ -> ()

// ============================================================================
// CATEGORY 3: COMPLEX INVARIANTS
// ============================================================================

// ----------------------------------------------------------------------------
// Property: SCC partition - components are disjoint and cover all nodes
// ----------------------------------------------------------------------------

[<Fact>]
let ``SCC components partition the graph`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addNode 3 3
        |> addEdge 0 1 1
        |> addEdge 1 0 1
        |> addEdge 2 3 1
        |> addEdge 3 2 1

    let components = stronglyConnectedComponents graph

    // Flatten all components
    let allNodesInComponents = components |> List.collect id |> Set.ofList

    let allGraphNodes = allNodes graph |> Set.ofList

    // Components should cover all nodes
    Assert.Equal<Set<NodeId>>(allGraphNodes, allNodesInComponents)

    // Components should be disjoint (no duplicates when flattened)
    let totalCount = components |> List.sumBy List.length
    Assert.Equal(Set.count allNodesInComponents, totalCount)

// ----------------------------------------------------------------------------
// Property: Bridge removal increases connected components
// ----------------------------------------------------------------------------

[<Fact>]
let ``bridge removal disconnects graph`` () =
    // Graph with a known bridge
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 1
        |> addEdge 1 0 1
        // Bridge from 1 to 2
        |> addEdge 1 2 1

    let result = analyze graph

    // Should find the bridge
    Assert.NotEmpty(result.Bridges)

    // Removing a bridge should disconnect the graph
    let bridge = List.head result.Bridges
    let (src, dst) = bridge

    let graphNoBridge = removeEdge src dst graph

    // After removing bridge, node 2 should not be reachable from node 0
    let stillReachable = isReachable graphNoBridge 0 2
    Assert.False(stillReachable)

// ----------------------------------------------------------------------------
// Property: Degree centrality correctness
// ----------------------------------------------------------------------------

[<Fact>]
let ``degree centrality is correct`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 1
        |> addEdge 0 2 1
        |> addEdge 1 2 1

    let outDegrees = degree OutDegree graph

    // Node 0 has 2 outgoing edges
    let degree0 = Map.find 0 outDegrees
    Assert.True(degree0 > 0.0)

    // Node 1 has 1 outgoing edge
    let degree1 = Map.find 1 outDegrees
    Assert.True(degree1 > 0.0)

    // Node 2 has 0 outgoing edges
    let degree2 = Map.find 2 outDegrees
    Assert.Equal(0.0, degree2)

// ----------------------------------------------------------------------------
// Property: Betweenness centrality is non-negative
// ----------------------------------------------------------------------------

[<Fact>]
let ``betweenness centrality is non-negative`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 1
        |> addEdge 1 2 1

    let betweennessScores = betweennessInt graph

    // All betweenness scores should be non-negative
    betweennessScores
    |> Map.iter (fun _ score -> Assert.True(score >= 0.0, "Betweenness should be non-negative"))

// ----------------------------------------------------------------------------
// Property: Closeness centrality in valid range
// ----------------------------------------------------------------------------

[<Fact>]
let ``closeness centrality in valid range`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 1
        |> addEdge 1 2 1

    let closenessScores = closenessInt graph

    // All closeness scores should be in [0, 1] range
    closenessScores
    |> Map.iter (fun _ score -> Assert.True(score >= 0.0 && score <= 1.0, "Closeness should be in [0, 1]"))
