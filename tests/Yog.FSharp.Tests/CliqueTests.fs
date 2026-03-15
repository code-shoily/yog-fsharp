/// Comprehensive tests for clique finding algorithms.
///
/// Covers:
/// - Maximum clique (Bron-Kerbosch with pivoting)
/// - All maximal cliques
/// - k-cliques
module Yog.FSharp.Tests.CliqueTests

open Xunit
open Yog.Model
open Yog.Properties.Clique

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

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

let addAllNodes nodes graph =
    nodes
    |> List.fold (fun acc n -> addNode n () acc) graph

// =============================================================================
// MAX CLIQUE TESTS
// =============================================================================

module MaxCliqueTests =
    [<Fact>]
    let ``maxClique - empty graph`` () =
        let graph = empty Undirected
        let result = maxClique graph
        Assert.Empty(result)

    [<Fact>]
    let ``maxClique - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = maxClique graph
        Assert.Equal(1, result.Count)
        Assert.True(result.Contains 0)

    [<Fact>]
    let ``maxClique - single edge`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result = maxClique graph
        // Edge forms a clique of size 2
        Assert.Equal(2, result.Count)
        Assert.True(result.Contains 0)
        Assert.True(result.Contains 1)

    [<Fact>]
    let ``maxClique - triangle`` () =
        // 0-1-2-0 forms a triangle (clique of size 3)
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) ]

        let result = maxClique graph
        Assert.Equal(3, result.Count)
        Assert.True(result.Contains 0)
        Assert.True(result.Contains 1)
        Assert.True(result.Contains 2)

    [<Fact>]
    let ``maxClique - square no diagonal`` () =
        // 0-1-2-3-0 is a cycle, no clique larger than 2
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 3)
                                  (3, 0) ]

        let result = maxClique graph
        Assert.Equal(2, result.Count) // Any edge

    [<Fact>]
    let ``maxClique - complete graph K4`` () =
        // All 4 nodes connected to each other
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (1, 2)
                                  (1, 3)
                                  (2, 3) ]

        let result = maxClique graph
        Assert.Equal(4, result.Count)

    [<Fact>]
    let ``maxClique - kite graph`` () =
        // Triangle 0-1-2 with node 3 connected to 1 and 2
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) // Triangle
                                  (1, 3)
                                  (2, 3) ] // Node 3 connected to 1 and 2

        let result = maxClique graph
        // Max clique is the triangle 0-1-2 or 1-2-3 (both size 3)
        Assert.Equal(3, result.Count)

    [<Fact>]
    let ``maxClique - two cliques sharing edge`` () =
        // Triangle 0-1-2 and triangle 1-2-3 sharing edge 1-2
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) // First triangle
                                  (1, 3)
                                  (2, 3) ] // Second triangle with shared edge

        let result = maxClique graph
        Assert.Equal(3, result.Count)

    [<Fact>]
    let ``maxClique - path graph`` () =
        // Path: 0-1-2-3
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 3) ]

        let result = maxClique graph
        // Max clique is just an edge (size 2)
        Assert.Equal(2, result.Count)

    [<Fact>]
    let ``maxClique - star graph`` () =
        // Star: center 0, leaves 1,2,3,4
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (0, 4) ]

        let result = maxClique graph
        // Max clique is just an edge (center + one leaf)
        Assert.Equal(2, result.Count)

    [<Fact>]
    let ``maxClique - isolated nodes`` () =
        let graph =
            empty Undirected
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()

        let result = maxClique graph
        // Max clique is size 1 (any single node)
        Assert.Equal(1, result.Count)

// =============================================================================
// ALL MAXIMAL CLIQUES TESTS
// =============================================================================

module AllMaximalCliquesTests =
    [<Fact>]
    let ``allMaximalCliques - empty graph`` () =
        let graph = empty Undirected
        let result = allMaximalCliques graph
        // Implementation may return empty set for empty graph
        // or a list with one empty set - both are valid
        Assert.True(result.Length <= 1)

    [<Fact>]
    let ``allMaximalCliques - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = allMaximalCliques graph
        Assert.Equal(1, result.Length)
        Assert.Equal(1, result.[0].Count)
        Assert.True(result.[0].Contains 0)

    [<Fact>]
    let ``allMaximalCliques - single edge`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result = allMaximalCliques graph
        // One maximal clique: {0, 1}
        Assert.Equal(1, result.Length)
        Assert.Equal(2, result.[0].Count)

    [<Fact>]
    let ``allMaximalCliques - triangle`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) ]

        let result = allMaximalCliques graph
        // One maximal clique: the triangle itself
        Assert.Equal(1, result.Length)
        Assert.Equal(3, result.[0].Count)

    [<Fact>]
    let ``allMaximalCliques - square no diagonal`` () =
        // Cycle of 4
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 3)
                                  (3, 0) ]

        let result = allMaximalCliques graph
        // Four maximal cliques, each is an edge
        Assert.Equal(4, result.Length)

        for clique in result do
            Assert.Equal(2, clique.Count)

    [<Fact>]
    let ``allMaximalCliques - path of three edges`` () =
        // 0-1-2-3
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 3) ]

        let result = allMaximalCliques graph
        // Three maximal cliques: {0,1}, {1,2}, {2,3}
        Assert.Equal(3, result.Length)

    [<Fact>]
    let ``allMaximalCliques - two triangles sharing edge`` () =
        // Triangle 0-1-2 and triangle 1-2-3
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) // First triangle
                                  (1, 3)
                                  (2, 3) ] // Second triangle

        let result = allMaximalCliques graph
        // Two maximal cliques: {0,1,2} and {1,2,3}
        Assert.Equal(2, result.Length)

        for clique in result do
            Assert.Equal(3, clique.Count)

    [<Fact>]
    let ``allMaximalCliques - complete graph K4`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (1, 2)
                                  (1, 3)
                                  (2, 3) ]

        let result = allMaximalCliques graph
        // One maximal clique: the whole graph
        Assert.Equal(1, result.Length)
        Assert.Equal(4, result.[0].Count)

// =============================================================================
// K-CLIQUES TESTS
// =============================================================================

module KCliquesTests =
    [<Fact>]
    let ``kCliques - k=0 returns empty`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2) ]
        let result = kCliques 0 graph
        Assert.Empty(result)

    [<Fact>]
    let ``kCliques - k=1 returns all nodes`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result = kCliques 1 graph
        Assert.Equal(2, result.Length)

    [<Fact>]
    let ``kCliques - k=2 returns all edges`` () =
        // Triangle has 3 edges = 3 cliques of size 2
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) ]

        let result = kCliques 2 graph
        Assert.Equal(3, result.Length)

    [<Fact>]
    let ``kCliques - k=3 in triangle`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) ]

        let result = kCliques 3 graph
        // One triangle
        Assert.Equal(1, result.Length)
        Assert.Equal(3, result.[0].Count)

    [<Fact>]
    let ``kCliques - k larger than any clique`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2) ] // Path, max clique size 2
        let result = kCliques 3 graph
        Assert.Empty(result)

    [<Fact>]
    let ``kCliques - complete graph K4, k=3`` () =
        // K4 has 4 choose 3 = 4 triangles
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (1, 2)
                                  (1, 3)
                                  (2, 3) ]

        let result = kCliques 3 graph
        Assert.Equal(4, result.Length)

    [<Fact>]
    let ``kCliques - complete graph K4, k=4`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (1, 2)
                                  (1, 3)
                                  (2, 3) ]

        let result = kCliques 4 graph
        // One clique of size 4 (the whole graph)
        Assert.Equal(1, result.Length)

    [<Fact>]
    let ``kCliques - two disjoint triangles`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) // First triangle
                                  (3, 4)
                                  (4, 5)
                                  (5, 3) ] // Second triangle

        let result = kCliques 3 graph
        // Two triangles
        Assert.Equal(2, result.Length)

    [<Fact>]
    let ``kCliques - square with one diagonal`` () =
        // Square 0-1-2-3-0 with diagonal 0-2
        // Creates two triangles: 0-1-2 and 0-2-3
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 3)
                                  (3, 0)
                                  (0, 2) ]

        let result = kCliques 3 graph
        Assert.Equal(2, result.Length)

    [<Fact>]
    let ``kCliques - negative k returns empty`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result = kCliques -1 graph
        Assert.Empty(result)

// =============================================================================
// EDGE CASE TESTS
// =============================================================================

module EdgeCaseTests =
    [<Fact>]
    let ``cliques in directed graph treated as undirected`` () =
        // Clique detection works on adjacency, direction doesn't matter
        let graph =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 1
            |> addEdge 1 0 1
            |> addEdge 1 2 1
            |> addEdge 2 1 1
            |> addEdge 0 2 1
            |> addEdge 2 0 1

        let result = maxClique graph
        Assert.Equal(3, result.Count)

    [<Fact>]
    let ``self loops don't affect cliques`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2) ]
        let graphWithLoop = addEdge 0 0 1 graph

        let result = maxClique graphWithLoop
        // Self loop doesn't create larger clique
        Assert.True(result.Count <= 2)

    [<Fact>]
    let ``clique algorithms handle isolated nodes`` () =
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (1, 2)
                                  (2, 0) ] // Triangle
            |> addNode 3 () // Isolated node

        let maxResult = maxClique graph
        let allResult = allMaximalCliques graph
        let kResult = kCliques 1 graph

        // Max clique is still the triangle
        Assert.Equal(3, maxResult.Count)
        // All maximal cliques include triangle and isolated nodes
        Assert.True(allResult.Length >= 2)
        // k=1 should include all 4 nodes
        Assert.Equal(4, kResult.Length)

    let makeUndirectedWeightedGraph edges =
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

    [<Fact>]
    let ``large clique detection`` () =
        // Create a 10-node complete graph (K10)
        let mutable edges = []

        for i in 0..9 do
            for j in i + 1 .. 9 do
                edges <- (i, j) :: edges

        let graph = makeUndirectedWeightedGraph edges

        let result = maxClique graph
        Assert.Equal(10, result.Count)

        let kResult = kCliques 5 graph
        // C(10,5) = 252 cliques of size 5
        Assert.Equal(252, kResult.Length)

    [<Fact>]
    let ``wheel graph cliques`` () =
        // Wheel: center 0, rim 1-2-3-4-1
        let graph =
            makeUndirectedGraph [ (0, 1)
                                  (0, 2)
                                  (0, 3)
                                  (0, 4) // Spokes
                                  (1, 2)
                                  (2, 3)
                                  (3, 4)
                                  (4, 1) ] // Rim

        let maxResult = maxClique graph
        // Max clique is triangle (center + two adjacent rim nodes)
        Assert.Equal(3, maxResult.Count)

        // Number of triangles = number of rim edges = 4
        let triangles = kCliques 3 graph
        Assert.Equal(4, triangles.Length)

// Helper for large clique test
let makeUndirectedWeightedGraph edges =
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
