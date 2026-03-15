/// Comprehensive tests for Minimum Spanning Tree algorithms.
///
/// Covers:
/// - Kruskal's algorithm
/// - Prim's algorithm
/// - Edge cases and various graph structures
module Yog.FSharp.Tests.MSTTests

open Xunit
open Yog.Model
open Yog.Mst

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

let makeUndirectedWeightedGraph (edges: (NodeId * NodeId * int) list) : Graph<unit, int> =
    let allNodes =
        edges
        |> List.collect (fun (u, v, _) -> [ u; v ])
        |> List.distinct

    let g = empty Undirected

    let gWithNodes =
        allNodes
        |> List.fold (fun acc n -> addNode n () acc) g

    edges
    |> List.fold (fun acc (u, v, w) -> addEdge u v w acc) gWithNodes

let totalWeight (edges: Edge<int> list) : int = edges |> List.sumBy (fun e -> e.Weight)

let edgeSet (edges: Edge<'e> list) : Set<(NodeId * NodeId)> =
    edges
    |> List.map (fun e -> (min e.From e.To, max e.From e.To))
    |> Set.ofList

// =============================================================================
// KRUSKAL'S ALGORITHM TESTS
// =============================================================================

module KruskalTests =
    [<Fact>]
    let ``kruskal - simple triangle`` () =
        // Triangle: 0-1 (1), 1-2 (2), 0-2 (3)
        // MST should pick 0-1 and 1-2, total weight 3
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (1, 2, 2)
                                          (0, 2, 3) ]

        let result = kruskal compare graph

        Assert.Equal(2, result.Length) // n-1 edges
        Assert.Equal(3, totalWeight result) // 1 + 2 = 3

    [<Fact>]
    let ``kruskal - square with diagonal`` () =
        // Square: 0-1 (1), 1-2 (2), 2-3 (3), 0-3 (4), diagonal 0-2 (5)
        // MST: 0-1, 1-2, 2-3 = 1+2+3 = 6
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (1, 2, 2)
                                          (2, 3, 3)
                                          (0, 3, 4)
                                          (0, 2, 5) ]

        let result = kruskal compare graph

        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``kruskal - complete graph K4`` () =
        // All edges, weights: 0-1 (1), 0-2 (2), 0-3 (3), 1-2 (4), 1-3 (5), 2-3 (6)
        // MST picks: 0-1, 0-2, 0-3 = 1+2+3 = 6
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (0, 2, 2)
                                          (0, 3, 3)
                                          (1, 2, 4)
                                          (1, 3, 5)
                                          (2, 3, 6) ]

        let result = kruskal compare graph

        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``kruskal - already a tree`` () =
        // Already a tree: 0-1 (5), 1-2 (3), 1-3 (2)
        // MST is the tree itself
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 5)
                                          (1, 2, 3)
                                          (1, 3, 2) ]

        let result = kruskal compare graph

        Assert.Equal(3, result.Length)
        Assert.Equal(10, totalWeight result)

    [<Fact>]
    let ``kruskal - single edge`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 42) ]
        let result = kruskal compare graph

        Assert.Equal(1, result.Length)
        Assert.Equal(0, result.[0].From)
        Assert.Equal(1, result.[0].To)
        Assert.Equal(42, result.[0].Weight)

    [<Fact>]
    let ``kruskal - empty graph`` () =
        let graph = empty Undirected
        let result = kruskal compare graph
        Assert.Empty(result)

    [<Fact>]
    let ``kruskal - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = kruskal compare graph
        Assert.Empty(result)

    [<Fact>]
    let ``kruskal - two nodes`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 100) ]
        let result = kruskal compare graph

        Assert.Equal(1, result.Length)
        Assert.Equal(100, result.[0].Weight)

    [<Fact>]
    let ``kruskal - disconnected graph gives forest`` () =
        // Two components: {0,1} and {2,3}
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (2, 3, 2) ]

        let result = kruskal compare graph

        // Should create a minimum spanning forest with both edges
        Assert.Equal(2, result.Length)
        Assert.Equal(3, totalWeight result)

    [<Fact>]
    let ``kruskal - equal weights`` () =
        // All edges have weight 1
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (0, 2, 1)
                                          (1, 2, 1) ]

        let result = kruskal compare graph

        Assert.Equal(2, result.Length)
        Assert.Equal(2, totalWeight result)

    [<Fact>]
    let ``kruskal - large weights`` () =
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1000000)
                                          (0, 2, 2000000)
                                          (1, 2, 3000000) ]

        let result = kruskal compare graph

        Assert.Equal(2, result.Length)
        Assert.Equal(3000000, totalWeight result)

// =============================================================================
// PRIM'S ALGORITHM TESTS
// =============================================================================

module PrimTests =
    [<Fact>]
    let ``prim - simple triangle`` () =
        // Triangle: 0-1 (1), 1-2 (2), 0-2 (3)
        // MST should pick 0-1 and 1-2, total weight 3
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (1, 2, 2)
                                          (0, 2, 3) ]

        let result = prim compare graph

        Assert.Equal(2, result.Length)
        Assert.Equal(3, totalWeight result)

    [<Fact>]
    let ``prim - square with diagonal`` () =
        // Square: 0-1 (1), 1-2 (2), 2-3 (3), 0-3 (4), diagonal 0-2 (5)
        // MST: 0-1, 1-2, 2-3 = 1+2+3 = 6
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (1, 2, 2)
                                          (2, 3, 3)
                                          (0, 3, 4)
                                          (0, 2, 5) ]

        let result = prim compare graph

        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``prim - complete graph K4`` () =
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (0, 2, 2)
                                          (0, 3, 3)
                                          (1, 2, 4)
                                          (1, 3, 5)
                                          (2, 3, 6) ]

        let result = prim compare graph

        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``prim - already a tree`` () =
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 5)
                                          (1, 2, 3)
                                          (1, 3, 2) ]

        let result = prim compare graph

        Assert.Equal(3, result.Length)
        Assert.Equal(10, totalWeight result)

    [<Fact>]
    let ``prim - single edge`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 42) ]
        let result = prim compare graph

        Assert.Equal(1, result.Length)
        Assert.Equal(42, result.[0].Weight)

    [<Fact>]
    let ``prim - empty graph`` () =
        let graph = empty Undirected
        let result = prim compare graph
        Assert.Empty(result)

    [<Fact>]
    let ``prim - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = prim compare graph
        Assert.Empty(result)

    [<Fact>]
    let ``prim - star graph`` () =
        // Star: center 0 connected to 1,2,3,4 with increasing weights
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (0, 2, 2)
                                          (0, 3, 3)
                                          (0, 4, 4) ]

        let result = prim compare graph

        Assert.Equal(4, result.Length)
        Assert.Equal(10, totalWeight result)

    [<Fact>]
    let ``prim - path graph`` () =
        // Path: 0-1 (5), 1-2 (4), 2-3 (3), 3-4 (2)
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 5)
                                          (1, 2, 4)
                                          (2, 3, 3)
                                          (3, 4, 2) ]

        let result = prim compare graph

        Assert.Equal(4, result.Length)
        Assert.Equal(14, totalWeight result)

    [<Fact>]
    let ``prim - disconnected graph returns only first component`` () =
        // Two components: {0,1} and {2,3}
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (2, 3, 2) ]

        let result = prim compare graph

        // Prim's starts from first node (0) and only spans that component
        // Unlike Kruskal which returns a spanning forest
        Assert.Equal(1, result.Length)
        Assert.Equal(1, totalWeight result)

        // Verify the edge is in the {0,1} component
        let edges = edgeSet result
        Assert.True(edges.Contains((0, 1)))

// =============================================================================
// KRUSKAL VS PRIM COMPARISON TESTS
// =============================================================================

module ComparisonTests =
    [<Fact>]
    let ``kruskal and prim give same total weight`` () =
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 4)
                                          (0, 2, 3)
                                          (0, 3, 1)
                                          (1, 2, 2)
                                          (1, 3, 5)
                                          (2, 3, 6) ]

        let kruskalResult = kruskal compare graph
        let primResult = prim compare graph

        // Both should produce MSTs with same total weight
        Assert.Equal(totalWeight kruskalResult, totalWeight primResult)

    [<Fact>]
    let ``kruskal and prim produce same number of edges`` () =
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 10)
                                          (0, 2, 20)
                                          (0, 3, 30)
                                          (1, 2, 5)
                                          (1, 3, 15)
                                          (2, 3, 25) ]

        let kruskalResult = kruskal compare graph
        let primResult = prim compare graph

        Assert.Equal(kruskalResult.Length, primResult.Length)
        Assert.Equal(3, kruskalResult.Length)

    [<Fact>]
    let ``both algorithms handle line graph`` () =
        // Simple line: 0-1-2-3-4
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (1, 2, 2)
                                          (2, 3, 3)
                                          (3, 4, 4) ]

        let kruskalResult = kruskal compare graph
        let primResult = prim compare graph

        // Line is already a tree, both should return all edges
        Assert.Equal(4, kruskalResult.Length)
        Assert.Equal(4, primResult.Length)
        Assert.Equal(10, totalWeight kruskalResult)
        Assert.Equal(10, totalWeight primResult)

    [<Fact>]
    let ``both algorithms handle cycle`` () =
        // Cycle: 0-1-2-0
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 1)
                                          (1, 2, 2)
                                          (0, 2, 3) ]

        let kruskalResult = kruskal compare graph
        let primResult = prim compare graph

        // Should drop the heaviest edge (0-2 with weight 3)
        Assert.Equal(2, kruskalResult.Length)
        Assert.Equal(2, primResult.Length)
        Assert.Equal(3, totalWeight kruskalResult)
        Assert.Equal(3, totalWeight primResult)

// =============================================================================
// EDGE CASE TESTS
// =============================================================================

module EdgeCaseTests =
    [<Fact>]
    let ``MST with zero weight edges`` () =
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 0)
                                          (1, 2, 0)
                                          (0, 2, 5) ]

        let result = kruskal compare graph

        // Should prefer zero-weight edges
        Assert.Equal(2, result.Length)
        Assert.Equal(0, totalWeight result)

    [<Fact>]
    let ``MST with negative weights`` () =
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, -5)
                                          (1, 2, -3)
                                          (0, 2, 10) ]

        let result = kruskal compare graph

        // Should include negative weight edges
        Assert.Equal(2, result.Length)
        Assert.Equal(-8, totalWeight result)

    [<Fact>]
    let ``MST on graph with many equal weight edges`` () =
        // Complete graph with all edges weight 5
        let graph =
            makeUndirectedWeightedGraph [ (0, 1, 5)
                                          (0, 2, 5)
                                          (0, 3, 5)
                                          (1, 2, 5)
                                          (1, 3, 5)
                                          (2, 3, 5) ]

        let result = kruskal compare graph

        Assert.Equal(3, result.Length)
        Assert.Equal(15, totalWeight result)

    [<Fact>]
    let ``MST handles float weights`` () =
        let graph =
            empty Undirected
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 1.5
            |> addEdge 1 2 2.5
            |> addEdge 0 2 4.0

        let result = kruskal compare graph

        Assert.Equal(2, result.Length)
    // Should pick 0-1 (1.5) and 1-2 (2.5), total 4.0

    [<Fact>]
    let ``MST on large complete graph`` () =
        // K10 - complete graph with 10 nodes
        let mutable edges = []

        for i in 0..9 do
            for j in i + 1 .. 9 do
                edges <- (i, j, i * 10 + j) :: edges

        let graph = makeUndirectedWeightedGraph edges
        let result = kruskal compare graph

        // MST should have n-1 = 9 edges
        Assert.Equal(9, result.Length)
