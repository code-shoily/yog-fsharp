/// Comprehensive tests for graph connectivity algorithms.
///
/// Covers:
/// - Strongly Connected Components (Tarjan's algorithm)
/// - Kosaraju's SCC algorithm
/// - Bridges (cut edges)
/// - Articulation points (cut vertices)
module Yog.FSharp.Tests.ConnectivityTests

open Xunit
open Yog.Model
open Yog.Connectivity

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

let makeDirectedGraph (edges: (NodeId * NodeId) list) : Graph<unit, int> =
    let allNodes = edges |> List.collect (fun (u, v) -> [ u; v ]) |> List.distinct

    let g = empty Directed

    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g

    edges |> List.fold (fun acc (u, v) -> addEdge u v 1 acc) gWithNodes

let makeUndirectedGraph (edges: (NodeId * NodeId) list) : Graph<unit, int> =
    let allNodes = edges |> List.collect (fun (u, v) -> [ u; v ]) |> List.distinct

    let g = empty Undirected

    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g

    edges |> List.fold (fun acc (u, v) -> addEdge u v 1 acc) gWithNodes

// =============================================================================
// STRONGLY CONNECTED COMPONENTS (TARJAN'S) TESTS
// =============================================================================

module SCCTests =
    [<Fact>]
    let ``SCC - empty graph`` () =
        let graph = empty Directed
        let result = stronglyConnectedComponents graph
        Assert.Equal(0, result.Length)

    [<Fact>]
    let ``SCC - single node no edges`` () =
        let graph = empty Directed |> addNode 0 ()
        let result = stronglyConnectedComponents graph
        Assert.Equal(1, result.Length)
        Assert.Equal<int list>([ 0 ], result.[0])

    [<Fact>]
    let ``SCC - linear chain each node separate`` () =
        // 0 -> 1 -> 2
        let graph = makeDirectedGraph [ (0, 1); (1, 2) ]
        let result = stronglyConnectedComponents graph
        Assert.Equal(3, result.Length) // Each node is its own SCC

    [<Fact>]
    let ``SCC - simple cycle single component`` () =
        // 0 -> 1 -> 2 -> 0
        let graph = makeDirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        let result = stronglyConnectedComponents graph
        Assert.Equal(1, result.Length)
        Assert.Equal(3, result.[0].Length)

    [<Fact>]
    let ``SCC - self loop`` () =
        let graph = empty Directed |> addNode 0 () |> addEdge 0 0 1
        let result = stronglyConnectedComponents graph
        Assert.Equal(1, result.Length)
        Assert.Equal<int list>([ 0 ], result.[0])

    [<Fact>]
    let ``SCC - two node cycle`` () =
        // 0 <-> 1
        let graph = makeDirectedGraph [ (0, 1); (1, 0) ]
        let result = stronglyConnectedComponents graph
        Assert.Equal(1, result.Length)
        Assert.Equal(2, result.[0].Length)

    [<Fact>]
    let ``SCC - two separate cycles`` () =
        // Cycle 1: 0 <-> 1
        // Cycle 2: 2 <-> 3
        let graph = makeDirectedGraph [ (0, 1); (1, 0); (2, 3); (3, 2) ]

        let result = stronglyConnectedComponents graph
        Assert.Equal(2, result.Length)
        // Each cycle should have 2 nodes
        Assert.True(result |> List.forall (fun comp -> comp.Length = 2))

    [<Fact>]
    let ``SCC - mixed cycle and non-cycle`` () =
        // Cycle: 0 -> 1 -> 2 -> 0
        // Non-cycle: 2 -> 3
        let graph = makeDirectedGraph [ (0, 1); (1, 2); (2, 0); (2, 3) ]

        let result = stronglyConnectedComponents graph
        Assert.Equal(2, result.Length)

        let sizes = result |> List.map (fun c -> c.Length) |> List.sort

        Assert.Equal<int list>([ 1; 3 ], sizes)

    [<Fact>]
    let ``SCC - Kosaraju's classic example`` () =
        // Two SCCs: {0,1,2} and {3,4}
        //   0 -> 1 -> 2
        //   ^         |
        //   |         v
        //   +-------- 3 -> 4
        //                  |
        //                  v
        //                  3 (cycle)
        let graph = makeDirectedGraph [ (0, 1); (1, 2); (2, 0); (2, 3); (3, 4); (4, 3) ]

        let result = stronglyConnectedComponents graph
        Assert.Equal(2, result.Length)

        let sizes = result |> List.map (fun c -> c.Length) |> List.sort

        Assert.Equal<int list>([ 2; 3 ], sizes)

    [<Fact>]
    let ``SCC - diamond with bottom cycle`` () =
        //      0
        //     / \
        //    1   2
        //     \ /
        //      3 <-> 1 (cycle between 1 and 3)
        let graph = makeDirectedGraph [ (0, 1); (0, 2); (1, 3); (2, 3); (3, 1) ]

        let result = stronglyConnectedComponents graph
        Assert.Equal(3, result.Length) // {0}, {2}, {1,3}

        let sizes = result |> List.map (fun c -> c.Length) |> List.sort

        Assert.Equal<int list>([ 1; 1; 2 ], sizes)

    [<Fact>]
    let ``SCC - complete directed graph`` () =
        // All nodes connected both ways
        let graph =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 1
            |> addEdge 1 0 1
            |> addEdge 0 2 1
            |> addEdge 2 0 1
            |> addEdge 1 2 1
            |> addEdge 2 1 1

        let result = stronglyConnectedComponents graph
        Assert.Equal(1, result.Length)
        Assert.Equal(3, result.[0].Length)

    [<Fact>]
    let ``SCC - disconnected components`` () =
        // Component 1: 0 -> 1
        // Component 2: 2 -> 3
        let graph = makeDirectedGraph [ (0, 1); (2, 3) ]
        let result = stronglyConnectedComponents graph
        Assert.Equal(4, result.Length) // All separate

    [<Fact>]
    let ``SCC - large cycle`` () =
        // 0 -> 1 -> 2 -> 3 -> 4 -> 0
        let edges = [ (0, 1); (1, 2); (2, 3); (3, 4); (4, 0) ]

        let graph = makeDirectedGraph edges
        let result = stronglyConnectedComponents graph
        Assert.Equal(1, result.Length)
        Assert.Equal(5, result.[0].Length)

// =============================================================================
// KOSARAJU'S SCC ALGORITHM TESTS
// =============================================================================

module KosarajuTests =
    [<Fact>]
    let ``Kosaraju - simple cycle`` () =
        let graph = makeDirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        let result = kosaraju graph
        Assert.Equal(1, result.Length)
        Assert.Equal(3, result.[0].Length)

    [<Fact>]
    let ``Kosaraju - matches Tarjan results`` () =
        let graph = makeDirectedGraph [ (0, 1); (1, 2); (2, 0); (2, 3); (3, 4); (4, 3) ]

        let tarjanResult = stronglyConnectedComponents graph
        let kosarajuResult = kosaraju graph
        // Both should find same number of SCCs
        Assert.Equal(tarjanResult.Length, kosarajuResult.Length)
        // Both should find same component sizes
        let tarjanSizes = tarjanResult |> List.map (fun c -> c.Length) |> List.sort

        let kosarajuSizes = kosarajuResult |> List.map (fun c -> c.Length) |> List.sort

        Assert.Equal<int list>(tarjanSizes, kosarajuSizes)

    [<Fact>]
    let ``Kosaraju - empty graph`` () =
        let graph = empty Directed
        let result = kosaraju graph
        Assert.Equal(0, result.Length)

    [<Fact>]
    let ``Kosaraju - single node`` () =
        let graph = empty Directed |> addNode 0 ()
        let result = kosaraju graph
        Assert.Equal(1, result.Length)

    [<Fact>]
    let ``Kosaraju - linear chain`` () =
        let graph = makeDirectedGraph [ (0, 1); (1, 2); (2, 3) ]

        let result = kosaraju graph
        Assert.Equal(4, result.Length)

// =============================================================================
// BRIDGES (CUT EDGES) TESTS
// =============================================================================

module BridgesTests =
    [<Fact>]
    let ``Bridges - empty graph`` () =
        let graph = empty Undirected
        let result = analyze graph
        Assert.Equal(0, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - single edge is bridge`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result = analyze graph
        Assert.Equal(1, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - triangle has no bridges`` () =
        // 0 - 1
        // |   |
        // 2 --+
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        let result = analyze graph
        Assert.Equal(0, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - chain has all edges as bridges`` () =
        // 0 - 1 - 2 - 3
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ]

        let result = analyze graph
        Assert.Equal(3, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - two triangles connected by bridge`` () =
        // Triangle 1: 0-1-2-0
        // Bridge: 2-3
        // Triangle 2: 3-4-5-3
        let graph =
            makeUndirectedGraph
                [ (0, 1)
                  (1, 2)
                  (2, 0) // Triangle 1
                  (2, 3) // Bridge
                  (3, 4)
                  (4, 5)
                  (5, 3) ] // Triangle 2

        let result = analyze graph
        Assert.Equal(1, result.Bridges.Length)
        // The bridge should be (2,3) or (3,2)
        let bridge = result.Bridges.[0]

        Assert.True((fst bridge = 2 && snd bridge = 3) || (fst bridge = 3 && snd bridge = 2))

    [<Fact>]
    let ``Bridges - star graph all edges are bridges`` () =
        // Center node 0 connected to 1, 2, 3
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (0, 3) ]

        let result = analyze graph
        Assert.Equal(3, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - self loop is not a bridge`` () =
        let graph = empty Undirected |> addNode 0 () |> addEdge 0 0 1
        let result = analyze graph
        Assert.Equal(0, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - disconnected graphs`` () =
        // Two separate edges: 0-1 and 2-3
        let graph = makeUndirectedGraph [ (0, 1); (2, 3) ]
        let result = analyze graph
        Assert.Equal(2, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - diamond with bridges`` () =
        //   0
        //   |
        //   1 -- 2
        //   |
        //   3
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (1, 3) ]

        let result = analyze graph
        // All three edges are bridges
        Assert.Equal(3, result.Bridges.Length)

// =============================================================================
// ARTICULATION POINTS (CUT VERTICES) TESTS
// =============================================================================

module ArticulationPointsTests =
    [<Fact>]
    let ``Articulation points - empty graph`` () =
        let graph = empty Undirected
        let result = analyze graph
        Assert.Equal(0, result.ArticulationPoints.Length)

    [<Fact>]
    let ``Articulation points - single edge`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result = analyze graph
        // Neither endpoint is an articulation point in a 2-node graph
        Assert.Equal(0, result.ArticulationPoints.Length)

    [<Fact>]
    let ``Articulation points - triangle`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        let result = analyze graph
        Assert.Equal(0, result.ArticulationPoints.Length)

    [<Fact>]
    let ``Articulation points - chain`` () =
        // 0 - 1 - 2 - 3
        // Articulation points: 1, 2
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ]

        let result = analyze graph
        Assert.Equal(2, result.ArticulationPoints.Length)
        Assert.True(result.ArticulationPoints |> List.contains 1)
        Assert.True(result.ArticulationPoints |> List.contains 2)

    [<Fact>]
    let ``Articulation points - star center`` () =
        // Center 0 connected to 1, 2, 3
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (0, 3) ]

        let result = analyze graph
        Assert.Equal(1, result.ArticulationPoints.Length)
        Assert.Equal(0, result.ArticulationPoints.[0])

    [<Fact>]
    let ``Articulation points - two triangles connected`` () =
        // Triangle 1: 0-1-2-0
        // Connection: 2-3
        // Triangle 2: 3-4-5-3
        let graph =
            makeUndirectedGraph [ (0, 1); (1, 2); (2, 0); (2, 3); (3, 4); (4, 5); (5, 3) ]

        let result = analyze graph
        // Articulation points: 2 and 3
        Assert.Equal(2, result.ArticulationPoints.Length)
        Assert.True(result.ArticulationPoints |> List.contains 2)
        Assert.True(result.ArticulationPoints |> List.contains 3)

    [<Fact>]
    let ``Articulation points - complete graph`` () =
        // Complete graph has no articulation points
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (1, 2) ]

        let result = analyze graph
        Assert.Equal(0, result.ArticulationPoints.Length)

    [<Fact>]
    let ``Articulation points - tree root`` () =
        //      0
        //     /|\
        //    1 2 3
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (0, 3) ]

        let result = analyze graph
        Assert.Equal(1, result.ArticulationPoints.Length)
        Assert.Equal(0, result.ArticulationPoints.[0])

    [<Fact>]
    let ``Articulation points - binary tree`` () =
        //      0
        //     / \
        //    1   2
        //   / \
        //  3   4
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (1, 3); (1, 4) ]

        let result = analyze graph
        // Articulation points: 0, 1
        Assert.Equal(2, result.ArticulationPoints.Length)
        Assert.True(result.ArticulationPoints |> List.contains 0)
        Assert.True(result.ArticulationPoints |> List.contains 1)

    [<Fact>]
    let ``Articulation points - disconnected graph`` () =
        // Two separate components: 0-1 and 2-3-4
        let graph = makeUndirectedGraph [ (0, 1); (2, 3); (3, 4) ]

        let result = analyze graph
        // Only node 3 is an articulation point
        Assert.Equal(1, result.ArticulationPoints.Length)
        Assert.Equal(3, result.ArticulationPoints.[0])

// =============================================================================
// COMBINED CONNECTIVITY TESTS
// =============================================================================

module CombinedTests =
    [<Fact>]
    let ``Connectivity - bridges imply articulation points`` () =
        // If a graph has a bridge, at least one endpoint is usually an articulation point
        // Exception: graphs with only 2 nodes
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ]

        let result = analyze graph
        // Has 3 bridges
        Assert.Equal(3, result.Bridges.Length)
        // Has 2 articulation points (1, 2)
        Assert.Equal(2, result.ArticulationPoints.Length)

    [<Fact>]
    let ``Connectivity - cycle has no bridges or articulation points`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3); (3, 0) ]

        let result = analyze graph
        Assert.Equal(0, result.Bridges.Length)
        Assert.Equal(0, result.ArticulationPoints.Length)

    [<Fact>]
    let ``Connectivity - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = analyze graph
        Assert.Equal(0, result.Bridges.Length)
        Assert.Equal(0, result.ArticulationPoints.Length)

    [<Fact>]
    let ``SCC vs connectivity - directed cycle`` () =
        // Directed cycle
        let directedGraph = makeDirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        let sccResult = stronglyConnectedComponents directedGraph
        Assert.Equal(1, sccResult.Length) // All in one SCC

        // Same structure as undirected
        let undirectedGraph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        let connResult = analyze undirectedGraph
        Assert.Equal(0, connResult.Bridges.Length)
        Assert.Equal(0, connResult.ArticulationPoints.Length)

    [<Fact>]
    let ``Large graph - SCC performance`` () =
        // Create a large cycle
        let edges = [ 0..98 ] |> List.map (fun i -> (i, i + 1))
        let edgesWithCycle = edges @ [ (99, 0) ]
        let graph = makeDirectedGraph edgesWithCycle
        let result = stronglyConnectedComponents graph
        Assert.Equal(1, result.Length)
        Assert.Equal(100, result.[0].Length)

    [<Fact>]
    let ``Large graph - many disconnected nodes`` () =
        // Many isolated nodes
        let graph = [ 0..99 ] |> List.fold (fun g n -> addNode n () g) (empty Directed)

        let result = stronglyConnectedComponents graph
        Assert.Equal(100, result.Length)
