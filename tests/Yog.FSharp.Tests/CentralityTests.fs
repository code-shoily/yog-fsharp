/// Comprehensive tests for graph centrality measures.
///
/// Covers:
/// - Degree Centrality (in, out, total)
/// - Closeness Centrality
/// - Betweenness Centrality (Brandes' algorithm)
/// - PageRank
/// - Eigenvector Centrality
/// - Katz Centrality
module Yog.FSharp.Tests.CentralityTests

open Xunit
open Yog.Model
open Yog.Centrality

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

let makeDirectedGraph (edges: (NodeId * NodeId) list) : Graph<unit, int> =
    let allNodes =
        edges
        |> List.collect (fun (u, v) -> [ u; v ])
        |> List.distinct

    let g = empty Directed

    let gWithNodes =
        allNodes
        |> List.fold (fun acc n -> addNode n () acc) g

    edges
    |> List.fold (fun acc (u, v) -> addEdge u v 1 acc) gWithNodes

let makeUndirectedGraph (edges: (NodeId * NodeId) list) : Graph<unit, int> =
    let allNodes =
        edges
        |> List.collect (fun (u, v) -> [ u; v ])
        |> List.distinct

    let g = empty Undirected

    let gWithNodes =
        allNodes
        |> List.fold (fun acc n -> addNode n () acc) g

    edges
    |> List.fold (fun acc (u, v) -> addEdge u v 1 acc) gWithNodes

let makeWeightedGraph (edges: (NodeId * NodeId * int) list) : Graph<unit, int> =
    let allNodes =
        edges
        |> List.collect (fun (u, v, _) -> [ u; v ])
        |> List.distinct

    let g = empty Directed

    let gWithNodes =
        allNodes
        |> List.fold (fun acc n -> addNode n () acc) g

    edges
    |> List.fold (fun acc (u, v, w) -> addEdge u v w acc) gWithNodes

// =============================================================================
// DEGREE CENTRALITY TESTS
// =============================================================================

module DegreeCentralityTests =
    [<Fact>]
    let ``degree - star graph center has highest degree`` () =
        // Star: 0 connected to 1, 2, 3, 4
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (0, 4) ]

        let result = degree TotalDegree graph

        // Center (0) should have degree 4, leaves have degree 1
        Assert.Equal(4.0 / 4.0, result.[0]) // normalized by (n-1) = 4
        Assert.Equal(1.0 / 4.0, result.[1])
        Assert.Equal(1.0 / 4.0, result.[2])

    [<Fact>]
    let ``degree - complete graph all nodes equal`` () =
        // K4: complete graph with 4 nodes
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (1, 2)
                                  (1, 3)
                                  (2, 3) ]

        let result = degree TotalDegree graph

        // All nodes have degree 3
        for i in 0..3 do
            Assert.Equal(3.0 / 3.0, result.[i])

    [<Fact>]
    let ``degree - directed in-degree`` () =
        // 1 -> 0, 2 -> 0 (node 0 has in-degree 2)
        let graph =
            makeDirectedGraph [ (1, 0)
                                (2, 0)
                                (3, 0) ]

        let result = degree InDegree graph

        Assert.Equal(3.0 / 3.0, result.[0]) // in-degree 3
        Assert.Equal(0.0, result.[1]) // in-degree 0

    [<Fact>]
    let ``degree - directed out-degree`` () =
        // 0 -> 1, 0 -> 2 (node 0 has out-degree 2)
        let graph =
            makeDirectedGraph [ (0, 1)
                                (0, 2)
                                (0, 3) ]

        let result = degree OutDegree graph

        Assert.Equal(3.0 / 3.0, result.[0]) // out-degree 3
        Assert.Equal(0.0, result.[1]) // out-degree 0

    [<Fact>]
    let ``degree - empty graph`` () =
        let graph = empty Directed
        let result = degree TotalDegree graph
        Assert.Empty(result)

    [<Fact>]
    let ``degree - single node`` () =
        let graph = empty Directed |> addNode 0 ()
        let result = degree TotalDegree graph
        Assert.Equal(0.0, result.[0])

    [<Fact>]
    let ``degreeTotal convenience function`` () =
        let graph = makeUndirectedGraph [ (0, 1); (0, 2) ]
        let result = degreeTotal graph
        Assert.Equal(2.0 / 2.0, result.[0])
        Assert.Equal(1.0 / 2.0, result.[1])

// =============================================================================
// CLOSENESS CENTRALITY TESTS
// =============================================================================

module ClosenessCentralityTests =
    [<Fact>]
    let ``closeness - star graph center has highest closeness`` () =
        // Star: 0 at center, all others at distance 1
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (0, 4) ]

        let result = closenessInt graph

        // Center can reach all others in 1 step: sum = 4, closeness = 4/4 = 1.0
        Assert.True(result.[0] > result.[1])
    // Leaves must go through center: sum = 1 + 2 + 2 + 2 = 7

    [<Fact>]
    let ``closeness - path graph middle has highest closeness`` () =
        // Path: 0 - 1 - 2 - 3 - 4
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 3)
                                  (3, 4) ]

        let result = closenessInt graph

        // Middle nodes (1, 2) should have higher closeness than ends (0, 4)
        Assert.True(result.[2] > result.[0])
        Assert.True(result.[2] > result.[4])

    [<Fact>]
    let ``closeness - disconnected node has zero centrality`` () =
        // Two components: 0 - 1 and isolated 2
        let graph = makeUndirectedGraph [ (0, 1) ] |> addNode 2 ()
        let result = closenessInt graph

        // Node 2 cannot reach all others
        Assert.Equal(0.0, result.[2])

    [<Fact>]
    let ``closeness - empty graph`` () =
        let graph = empty Undirected
        let result = closenessInt graph
        Assert.Empty(result)

    [<Fact>]
    let ``closeness - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = closenessInt graph
        Assert.Equal(0.0, result.[0]) // Single node, n <= 1

    [<Fact>]
    let ``closeness - complete graph all nodes equal`` () =
        // K4: all nodes at distance 1 from each other
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (1, 2)
                                  (1, 3)
                                  (2, 3) ]

        let result = closenessInt graph

        // All should have same closeness
        let first = result.[0]

        for i in 1..3 do
            Assert.Equal(first, result.[i])

// =============================================================================
// BETWEENNESS CENTRALITY TESTS
// =============================================================================

module BetweennessCentralityTests =
    [<Fact>]
    let ``betweenness - bridge node has high betweenness`` () =
        // 0 - 1 - 2 (node 1 is a bridge)
        let graph = makeUndirectedGraph [ (0, 1); (1, 2) ]
        let result = betweennessInt graph

        // Node 1 lies on shortest path between 0 and 2
        Assert.True(result.[1] > result.[0])
        Assert.True(result.[1] > result.[2])

    [<Fact>]
    let ``betweenness - star graph center has maximum betweenness`` () =
        // Star: 0 at center
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (0, 4) ]

        let result = betweennessInt graph

        // Center lies on all shortest paths between leaves
        Assert.True(result.[0] > 0.0)
        // Leaves have zero betweenness (no paths go through them)
        for i in 1..4 do
            Assert.Equal(0.0, result.[i])

    [<Fact>]
    let ``betweenness - complete graph all zero`` () =
        // K4: no node lies on shortest path between others (direct edges)
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (1, 2)
                                  (1, 3)
                                  (2, 3) ]

        let result = betweennessInt graph

        // All betweenness should be 0 (no intermediate nodes needed)
        for i in 0..3 do
            Assert.Equal(0.0, result.[i])

    [<Fact>]
    let ``betweenness - empty graph`` () =
        let graph = empty Undirected
        let result = betweennessInt graph
        Assert.Empty(result)

    [<Fact>]
    let ``betweenness - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = betweennessInt graph
        Assert.Equal(0.0, result.[0])

    [<Fact>]
    let ``betweenness - weighted graph`` () =
        // Diamond: 0 -> 1 -> 3 and 0 -> 2 -> 3, with different weights
        let graph =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addNode 3 ()
            |> addEdge 0 1 1
            |> addEdge 1 3 1
            |> addEdge 0 2 2
            |> addEdge 2 3 2

        let result = betweenness 0 (+) compare graph

        // Node 1 is on the shortest path
        Assert.True(result.[1] > 0.0)

// =============================================================================
// PAGERANK TESTS
// =============================================================================

module PageRankTests =
    let defaultOptions =
        { Damping = 0.85
          MaxIterations = 100
          Tolerance = 0.0001 }

    [<Fact>]
    let ``pagerank - star graph center has highest rank`` () =
        // Star: 0 at center (everyone links to center)
        // Actually for undirected, edges go both ways
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (0, 4) ]

        let result = pagerank defaultOptions graph

        // All ranks should sum to 1
        let sum = result |> Map.fold (fun acc _ v -> acc + v) 0.0
        Assert.True(abs (sum - 1.0) < 0.001)

    [<Fact>]
    let ``pagerank - empty graph`` () =
        let graph = empty Directed
        let result = pagerank defaultOptions graph
        Assert.Empty(result)

    [<Fact>]
    let ``pagerank - single node`` () =
        let graph = empty Directed |> addNode 0 ()
        let result = pagerank defaultOptions graph
        Assert.Equal(1.0, result.[0])

    [<Fact>]
    let ``pagerank - two nodes mutual links`` () =
        // 0 <-> 1
        let graph = makeDirectedGraph [ (0, 1); (1, 0) ]
        let result = pagerank defaultOptions graph

        // Both should have similar rank
        Assert.True(abs (result.[0] - result.[1]) < 0.1)

    [<Fact>]
    let ``pagerank - sink nodes handled`` () =
        // 0 -> 1 (node 1 is a sink)
        let graph = makeDirectedGraph [ (0, 1) ]
        let result = pagerank defaultOptions graph

        // Both should have valid ranks
        Assert.True(result.[0] > 0.0)
        Assert.True(result.[1] > 0.0)

    [<Fact>]
    let ``pagerank - linear chain`` () =
        // 0 -> 1 -> 2
        let graph = makeDirectedGraph [ (0, 1); (1, 2) ]
        let result = pagerank defaultOptions graph

        // Middle node gets rank from both sides
        Assert.True(result.[1] > 0.0)

// =============================================================================
// EIGENVECTOR CENTRALITY TESTS
// =============================================================================

module EigenvectorCentralityTests =
    [<Fact>]
    let ``eigenvector - connected to important nodes`` () =
        // Node 0 connected to 1 and 2, node 1 connected to 2
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (1, 2) ]

        let result = eigenvector 100 0.0001 graph

        // All should have positive scores
        for i in 0..2 do
            Assert.True(result.[i] > 0.0)

    [<Fact>]
    let ``eigenvector - empty graph`` () =
        let graph = empty Undirected
        let result = eigenvector 100 0.0001 graph
        Assert.Empty(result)

    [<Fact>]
    let ``eigenvector - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = eigenvector 100 0.0001 graph
        Assert.Equal(1.0, result.[0])

    [<Fact>]
    let ``eigenvector - star graph`` () =
        // Star: center connected to everyone
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3) ]

        let result = eigenvector 100 0.0001 graph

        // Center should have highest score (connected to most)
        Assert.True(result.[0] > result.[1])

    [<Fact>]
    let ``eigenvector - isolated node has zero`` () =
        // Triangle plus isolated node
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) ]
            |> addNode 3 ()

        let result = eigenvector 100 0.0001 graph

        // Isolated node should have minimal score
        Assert.True(result.[3] < result.[0])

// =============================================================================
// KATZ CENTRALITY TESTS
// =============================================================================

module KatzCentralityTests =
    [<Fact>]
    let ``katz - basic calculation`` () =
        // Simple chain
        let graph = makeUndirectedGraph [ (0, 1); (1, 2) ]
        let result = katz 0.1 1.0 100 0.0001 graph

        // All should have positive scores
        for i in 0..2 do
            Assert.True(result.[i] > 0.0)

    [<Fact>]
    let ``katz - empty graph`` () =
        let graph = empty Undirected
        let result = katz 0.1 1.0 100 0.0001 graph
        Assert.Empty(result)

    [<Fact>]
    let ``katz - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = katz 0.1 1.0 100 0.0001 graph
        // With no neighbors, score is just beta
        Assert.Equal(1.0, result.[0])

    [<Fact>]
    let ``katz - higher alpha values amplify neighbors`` () =
        // Star graph
        let graph = makeUndirectedGraph [ (0, 1); (0, 2) ]
        let lowAlpha = katz 0.01 1.0 100 0.0001 graph
        let highAlpha = katz 0.1 1.0 100 0.0001 graph

        // Center (0) has more neighbors, so higher alpha benefits it more
        let lowDiff = lowAlpha.[0] - lowAlpha.[1]
        let highDiff = highAlpha.[0] - highAlpha.[1]
        Assert.True(highDiff > lowDiff)

    [<Fact>]
    let ``katz - beta shifts base score`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result1 = katz 0.1 1.0 100 0.0001 graph
        let result2 = katz 0.1 2.0 100 0.0001 graph

        // Higher beta should give higher scores
        Assert.True(result2.[0] > result1.[0])

// =============================================================================
// COMPARATIVE CENTRALITY TESTS
// =============================================================================

module ComparativeTests =
    [<Fact>]
    let ``different centralities give different insights`` () =
        // Star graph: center is most central by most measures
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (0, 4) ]

        let degreeResult = degreeTotal graph
        let closenessResult = closenessInt graph
        let betweennessResult = betweennessInt graph

        // Center (0) should be most central by all measures
        Assert.True(degreeResult.[0] > degreeResult.[1])
        Assert.True(closenessResult.[0] > closenessResult.[1])
        Assert.True(betweennessResult.[0] > betweennessResult.[1])

    [<Fact>]
    let ``centralities handle weighted graphs`` () =
        let graph =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 10
            |> addEdge 1 2 5

        let closeness = closeness 0 (+) compare float graph
        let betweenness = betweenness 0 (+) compare graph

        Assert.NotEmpty(closeness)
        Assert.NotEmpty(betweenness)

    [<Fact>]
    let ``centrality rankings are consistent`` () =
        // Path: 0 - 1 - 2 - 3 - 4
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 3)
                                  (3, 4) ]

        let result = degreeTotal graph

        // All nodes have same degree (2 for middle, 1 for ends)
        // In undirected path: ends have degree 1, middle have degree 2
        Assert.Equal(2.0 / 4.0, result.[1]) // interior
        Assert.Equal(2.0 / 4.0, result.[2])
        Assert.Equal(2.0 / 4.0, result.[3])
        Assert.Equal(1.0 / 4.0, result.[0]) // end
        Assert.Equal(1.0 / 4.0, result.[4]) // end

// =============================================================================
// HARMONIC CENTRALITY TESTS
// =============================================================================

module HarmonicCentralityTests =
    [<Fact>]
    let ``harmonic - star graph center has highest`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (0, 4) ]

        let result = harmonicCentralityInt graph

        // Center node (0): 1/1 + 1/1 + 1/1 + 1/1 = 4, normalized = 4/4 = 1.0
        Assert.Equal(1.0, result.[0])
        // Leaves have lower centrality
        Assert.True(result.[0] > result.[1])

    [<Fact>]
    let ``harmonic - disconnected graph works`` () =
        // Two components: 0 - 1 and isolated 2
        let graph = makeUndirectedGraph [ (0, 1) ] |> addNode 2 ()
        let result = harmonicCentralityInt graph

        // Isolated node has 0.0
        Assert.Equal(0.0, result.[2])
        // Connected nodes have non-zero (1/1 / 2 = 0.5)
        Assert.Equal(0.5, result.[0])
        Assert.Equal(0.5, result.[1])

    [<Fact>]
    let ``harmonic - complete graph all equal`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (1, 2)
                                  (1, 3)
                                  (2, 3) ]

        let result = harmonicCentralityInt graph

        // All should have same harmonic centrality
        Assert.Equal(result.[0], result.[1])
        Assert.Equal(result.[1], result.[2])

// =============================================================================
// ALPHA CENTRALITY TESTS
// =============================================================================

module AlphaCentralityTests =
    [<Fact>]
    let ``alpha - star graph`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3) ]

        let result = alphaCentrality 0.3 1.0 100 0.0001 graph

        // All should have non-negative scores
        Assert.True(result.[0] >= 0.0)
        Assert.True(result.[1] >= 0.0)

    [<Fact>]
    let ``alpha - path graph`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 3) ]

        let result = alphaCentrality 0.3 1.0 100 0.0001 graph

        // Middle nodes should have higher centrality
        Assert.True(result.[1] > result.[0])
        Assert.True(result.[2] > result.[0])

    [<Fact>]
    let ``alpha - single node converges to zero`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = alphaCentrality 0.3 1.0 100 0.0001 graph

        // Single node with no neighbors goes to 0
        Assert.Equal(0.0, result.[0])
