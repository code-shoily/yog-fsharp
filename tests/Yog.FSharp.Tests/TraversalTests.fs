/// Comprehensive tests for graph traversal algorithms.
///
/// Covers:
/// - BFS (Breadth-First Search)
/// - DFS (Depth-First Search)
/// - walk and walkUntil variants
/// - Topological sorting
/// - Cycle detection
module Yog.FSharp.Tests.TraversalTests

open Xunit
open Yog.Model
open Yog.Traversal

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/// Helper to create a simple graph from edge list
let makeGraph (edges: (NodeId * NodeId) list) (kind: GraphType) : Graph<unit, int> =
    let allNodes =
        edges
        |> List.collect (fun (u, v) -> [ u; v ])
        |> List.distinct

    let g = empty kind

    let gWithNodes =
        allNodes
        |> List.fold (fun acc n -> addNode n () acc) g

    edges
    |> List.fold (fun acc (u, v) -> addEdge u v 1 acc) gWithNodes

// =============================================================================
// BFS (BREADTH-FIRST SEARCH) TESTS
// =============================================================================

module BFSTests =
    [<Fact>]
    let ``BFS - linear path`` () =
        // 0 -> 1 -> 2
        let graph = makeGraph [ (0, 1); (1, 2) ] Directed
        let result = walk 0 BreadthFirst graph
        Assert.Equal<int list>([ 0; 1; 2 ], result)

    [<Fact>]
    let ``BFS - tree visits level by level`` () =
        //     0
        //    / \
        //   1   2
        //  / \
        // 3   4
        let graph = makeGraph [ (0, 1); (0, 2); (1, 3); (1, 4) ] Directed
        let result = walk 0 BreadthFirst graph
        Assert.Equal<int list>([ 0; 1; 2; 3; 4 ], result)

    [<Fact>]
    let ``BFS - with cycle does not loop infinitely`` () =
        // 0 -> 1 -> 2 -> 0 (cycle)
        let graph = makeGraph [ (0, 1); (1, 2); (2, 0) ] Directed
        let result = walk 0 BreadthFirst graph
        Assert.Equal(3, result.Length)
        Assert.Equal<int list>([ 0; 1; 2 ], result)

    [<Fact>]
    let ``BFS - isolated node`` () =
        let graph = makeGraph [] Directed |> addNode 0 ()
        let result = walk 0 BreadthFirst graph
        Assert.Equal<int list>([ 0 ], result)

    [<Fact>]
    let ``BFS - nonexistent start node`` () =
        let graph = empty Directed
        let result = walk 99 BreadthFirst graph
        Assert.Equal<int list>([ 99 ], result)

    [<Fact>]
    let ``BFS - undirected graph`` () =
        // 0 - 1 - 2 (undirected)
        let graph = makeGraph [ (0, 1); (1, 2) ] Undirected
        let result = walk 1 BreadthFirst graph
        Assert.Equal<int list>([ 1; 0; 2 ], result)

    [<Fact>]
    let ``BFS - diamond pattern`` () =
        //   0
        //  / \
        // 1   2
        //  \ /
        //   3
        let graph = makeGraph [ (0, 1); (0, 2); (1, 3); (2, 3) ] Directed
        let result = walk 0 BreadthFirst graph
        // BFS visits level by level: 0, then {1,2}, then {3}
        Assert.Equal(4, result.Length)
        Assert.Equal(0, result.[0])
        Assert.Equal(3, result.[3]) // Node 3 is last

    [<Fact>]
    let ``BFS - disconnected component not visited`` () =
        // Component 1: 0 -> 1
        // Component 2: 2 -> 3 (isolated)
        let graph = makeGraph [ (0, 1); (2, 3) ] Directed
        let result = walk 0 BreadthFirst graph
        Assert.Equal<int list>([ 0; 1 ], result)
        Assert.False(result |> List.contains 2)

    [<Fact>]
    let ``BFS - empty graph`` () =
        let graph = empty Directed
        let result = walk 0 BreadthFirst graph
        Assert.Equal<int list>([ 0 ], result)

    [<Fact>]
    let ``BFS - self loop`` () =
        let graph = empty Directed |> addNode 0 () |> addEdge 0 0 1
        let result = walk 0 BreadthFirst graph
        Assert.Equal<int list>([ 0 ], result)

    [<Fact>]
    let ``BFS - complex graph`` () =
        // More complex structure
        let edges =
            [ (0, 1)
              (0, 2)
              (1, 3)
              (2, 3)
              (3, 4)
              (3, 5) ]

        let graph = makeGraph edges Directed
        let result = walk 0 BreadthFirst graph
        Assert.Equal(6, result.Length)
        // All nodes should be visited
        Assert.True(
            [ 0; 1; 2; 3; 4; 5 ]
            |> List.forall (fun n -> result |> List.contains n)
        )

// =============================================================================
// DFS (DEPTH-FIRST SEARCH) TESTS
// =============================================================================

module DFSTests =
    [<Fact>]
    let ``DFS - linear path`` () =
        // 0 -> 1 -> 2
        let graph = makeGraph [ (0, 1); (1, 2) ] Directed
        let result = walk 0 DepthFirst graph
        Assert.Equal<int list>([ 0; 1; 2 ], result)

    [<Fact>]
    let ``DFS - tree goes deep first`` () =
        //     0
        //    / \
        //   1   2
        //  / \
        // 3   4
        let graph = makeGraph [ (0, 1); (0, 2); (1, 3); (1, 4) ] Directed
        let result = walk 0 DepthFirst graph
        // DFS goes deep: 0 -> 1 -> 3 -> 4 -> 2
        Assert.Equal<int list>([ 0; 1; 3; 4; 2 ], result)

    [<Fact>]
    let ``DFS - with cycle does not loop infinitely`` () =
        // 0 -> 1 -> 2 -> 0 (cycle)
        let graph = makeGraph [ (0, 1); (1, 2); (2, 0) ] Directed
        let result = walk 0 DepthFirst graph
        Assert.Equal(3, result.Length)
        Assert.Equal<int list>([ 0; 1; 2 ], result)

    [<Fact>]
    let ``DFS - isolated node`` () =
        let graph = makeGraph [] Directed |> addNode 0 ()
        let result = walk 0 DepthFirst graph
        Assert.Equal<int list>([ 0 ], result)

    [<Fact>]
    let ``DFS - diamond visits node once`` () =
        //   0
        //  / \
        // 1   2
        //  \ /
        //   3
        let graph = makeGraph [ (0, 1); (0, 2); (1, 3); (2, 3) ] Directed
        let result = walk 0 DepthFirst graph
        Assert.Equal(4, result.Length)
        Assert.Equal<int list>([ 0; 1; 3; 2 ], result)

    [<Fact>]
    let ``DFS - undirected graph`` () =
        let graph = makeGraph [ (0, 1); (1, 2) ] Undirected
        let result = walk 0 DepthFirst graph
        Assert.Equal(3, result.Length)
        Assert.True(result |> List.contains 0)
        Assert.True(result |> List.contains 1)
        Assert.True(result |> List.contains 2)

    [<Fact>]
    let ``DFS - complex branching`` () =
        //      0
        //     /|\
        //    1 2 3
        //   /| |
        //  4 5 6
        let edges =
            [ (0, 1)
              (0, 2)
              (0, 3)
              (1, 4)
              (1, 5)
              (2, 6) ]

        let graph = makeGraph edges Directed
        let result = walk 0 DepthFirst graph
        Assert.Equal(7, result.Length)
        Assert.Equal(0, result.[0]) // Starts at 0

    [<Fact>]
    let ``DFS - no duplicates with multiple paths`` () =
        let edges =
            [ (0, 1)
              (0, 2)
              (1, 3)
              (2, 3)
              (3, 4) ]

        let graph = makeGraph edges Directed
        let result = walk 0 DepthFirst graph
        let uniqueCount = result |> List.distinct |> List.length
        Assert.Equal(result.Length, uniqueCount)

// =============================================================================
// WALK UNTIL TESTS
// =============================================================================

module WalkUntilTests =
    [<Fact>]
    let ``walkUntil - BFS stops at target`` () =
        // 0 -> 1 -> 2 -> 3
        let graph = makeGraph [ (0, 1); (1, 2); (2, 3) ] Directed
        let result = walkUntil 0 BreadthFirst (fun id -> id = 2) graph
        Assert.Equal<int list>([ 0; 1; 2 ], result)

    [<Fact>]
    let ``walkUntil - DFS stops at target`` () =
        // 0 -> 1 -> 2 -> 3
        let graph = makeGraph [ (0, 1); (1, 2); (2, 3) ] Directed
        let result = walkUntil 0 DepthFirst (fun id -> id = 2) graph
        Assert.Equal<int list>([ 0; 1; 2 ], result)

    [<Fact>]
    let ``walkUntil - never stops visits all`` () =
        let graph = makeGraph [ (0, 1); (1, 2) ] Directed
        let result = walkUntil 0 BreadthFirst (fun _ -> false) graph
        Assert.Equal<int list>([ 0; 1; 2 ], result)

    [<Fact>]
    let ``walkUntil - stops at start`` () =
        let graph = makeGraph [ (0, 1); (1, 2) ] Directed
        let result = walkUntil 0 BreadthFirst (fun id -> id = 0) graph
        Assert.Equal<int list>([ 0 ], result)

    [<Fact>]
    let ``walkUntil - target not in graph`` () =
        let graph = makeGraph [ (0, 1); (1, 2) ] Directed
        let result = walkUntil 0 BreadthFirst (fun id -> id = 99) graph
        Assert.Equal<int list>([ 0; 1; 2 ], result)

    [<Fact>]
    let ``walkUntil - stops early in large graph`` () =
        let edges =
            [ (0, 1)
              (0, 2)
              (1, 3)
              (2, 4)
              (3, 5)
              (4, 6) ]

        let graph = makeGraph edges Directed
        let result = walkUntil 0 BreadthFirst (fun id -> id = 3) graph
        Assert.True(result.Length < 7) // Should not visit all nodes
        Assert.True(result |> List.contains 3)

// =============================================================================
// TOPOLOGICAL SORT TESTS
// =============================================================================

module TopologicalSortTests =
    [<Fact>]
    let ``topologicalSort - simple chain`` () =
        // 0 -> 1 -> 2
        let graph = makeGraph [ (0, 1); (1, 2) ] Directed
        let result = topologicalSort graph

        match result with
        | Ok sorted ->
            Assert.Equal(3, sorted.Length)
            let idx0 = sorted |> List.findIndex ((=) 0)
            let idx1 = sorted |> List.findIndex ((=) 1)
            let idx2 = sorted |> List.findIndex ((=) 2)
            Assert.True(idx0 < idx1)
            Assert.True(idx1 < idx2)
        | Error _ -> Assert.True(false, "Should succeed")

    [<Fact>]
    let ``topologicalSort - DAG with multiple paths`` () =
        //   0
        //  / \
        // 1   2
        //  \ /
        //   3
        let graph = makeGraph [ (0, 1); (0, 2); (1, 3); (2, 3) ] Directed
        let result = topologicalSort graph

        match result with
        | Ok sorted ->
            Assert.Equal(4, sorted.Length)
            Assert.Equal(0, sorted.[0]) // 0 must be first
            Assert.Equal(3, sorted.[3]) // 3 must be last
        | Error _ -> Assert.True(false, "Should succeed on DAG")

    [<Fact>]
    let ``topologicalSort - detects cycle`` () =
        // 0 -> 1 -> 2 -> 0 (cycle)
        let graph = makeGraph [ (0, 1); (1, 2); (2, 0) ] Directed
        let result = topologicalSort graph

        match result with
        | Ok _ -> Assert.True(false, "Should detect cycle")
        | Error _ -> Assert.True(true)

    [<Fact>]
    let ``topologicalSort - single node`` () =
        let graph = empty Directed |> addNode 0 ()
        let result = topologicalSort graph

        match result with
        | Ok sorted -> Assert.Equal<int list>([ 0 ], sorted)
        | Error _ -> Assert.True(false, "Single node should work")

    [<Fact>]
    let ``topologicalSort - disconnected DAG`` () =
        // Component 1: 0 -> 1
        // Component 2: 2 -> 3
        let graph = makeGraph [ (0, 1); (2, 3) ] Directed
        let result = topologicalSort graph

        match result with
        | Ok sorted ->
            Assert.Equal(4, sorted.Length)
            let idx0 = sorted |> List.findIndex ((=) 0)
            let idx1 = sorted |> List.findIndex ((=) 1)
            let idx2 = sorted |> List.findIndex ((=) 2)
            let idx3 = sorted |> List.findIndex ((=) 3)
            Assert.True(idx0 < idx1)
            Assert.True(idx2 < idx3)
        | Error _ -> Assert.True(false, "Should succeed")

    [<Fact>]
    let ``topologicalSort - empty graph`` () =
        let graph = empty Directed
        let result = topologicalSort graph

        match result with
        | Ok sorted -> Assert.Equal(0, sorted.Length)
        | Error _ -> Assert.True(false, "Empty graph should succeed")

    [<Fact>]
    let ``topologicalSort - complex DAG`` () =
        //      0
        //     /|\
        //    1 2 3
        //     \|/
        //      4
        let edges =
            [ (0, 1)
              (0, 2)
              (0, 3)
              (1, 4)
              (2, 4)
              (3, 4) ]

        let graph = makeGraph edges Directed
        let result = topologicalSort graph

        match result with
        | Ok sorted ->
            Assert.Equal(5, sorted.Length)
            Assert.Equal(0, sorted.[0])
            Assert.Equal(4, sorted.[4])
        | Error _ -> Assert.True(false, "Should succeed")

    [<Fact>]
    let ``topologicalSort - self loop is cycle`` () =
        let graph = empty Directed |> addNode 0 () |> addEdge 0 0 1
        let result = topologicalSort graph

        match result with
        | Ok _ -> Assert.True(false, "Self loop is a cycle")
        | Error _ -> Assert.True(true)

// =============================================================================
// LEXICOGRAPHICAL TOPOLOGICAL SORT TESTS
// =============================================================================

module LexicographicalTopologicalSortTests =
    [<Fact>]
    let ``lexicographicalTopologicalSort - orders by value when possible`` () =
        //   2
        //  / \
        // 0   1
        //  \ /
        //   3
        let graph =
            empty Directed
            |> addNode 2 2 // Value 2
            |> addNode 0 0 // Value 0
            |> addNode 1 1 // Value 1
            |> addNode 3 3 // Value 3
            |> addEdge 2 0 1
            |> addEdge 2 1 1
            |> addEdge 0 3 1
            |> addEdge 1 3 1

        let result = lexicographicalTopologicalSort compare graph

        match result with
        | Ok sorted ->
            Assert.Equal(4, sorted.Length)
            Assert.Equal(2, sorted.[0]) // 2 is the only source
            // 0 and 1 both depend on 2, lexicographically 0 < 1
            let idx0 = sorted |> List.findIndex ((=) 0)
            let idx1 = sorted |> List.findIndex ((=) 1)
            Assert.True(idx0 < idx1)
            Assert.Equal(3, sorted.[3]) // 3 is the only sink
        | Error _ -> Assert.True(false, "Should succeed")

    [<Fact>]
    let ``lexicographicalTopologicalSort - detects cycle`` () =
        let graph = makeGraph [ (0, 1); (1, 2); (2, 0) ] Directed
        let result = lexicographicalTopologicalSort compare graph

        match result with
        | Ok _ -> Assert.True(false, "Should detect cycle")
        | Error _ -> Assert.True(true)

// =============================================================================
// CYCLE DETECTION TESTS
// =============================================================================

module CyclicityTests =
    [<Fact>]
    let ``isCyclic - simple cycle`` () =
        let graph = makeGraph [ (0, 1); (1, 2); (2, 0) ] Directed
        Assert.True(isCyclic graph)

    [<Fact>]
    let ``isCyclic - DAG is not cyclic`` () =
        let graph = makeGraph [ (0, 1); (1, 2) ] Directed
        Assert.False(isCyclic graph)

    [<Fact>]
    let ``isCyclic - self loop`` () =
        let graph = empty Directed |> addNode 0 () |> addEdge 0 0 1
        Assert.True(isCyclic graph)

    [<Fact>]
    let ``isCyclic - empty graph`` () =
        let graph = empty Directed
        Assert.False(isCyclic graph)

    [<Fact>]
    let ``isCyclic - single node`` () =
        let graph = empty Directed |> addNode 0 ()
        Assert.False(isCyclic graph)

    [<Fact>]
    let ``isCyclic - complex with cycle`` () =
        // 0 -> 1 -> 2
        //      ^    |
        //      |    v
        //      4 <- 3
        let edges =
            [ (0, 1)
              (1, 2)
              (2, 3)
              (3, 4)
              (4, 1) ]

        let graph = makeGraph edges Directed
        Assert.True(isCyclic graph)

    [<Fact>]
    let ``isAcyclic - DAG`` () =
        let graph = makeGraph [ (0, 1); (1, 2) ] Directed
        Assert.True(isAcyclic graph)

    [<Fact>]
    let ``isAcyclic - with cycle`` () =
        let graph = makeGraph [ (0, 1); (1, 2); (2, 0) ] Directed
        Assert.False(isAcyclic graph)

// =============================================================================
// EDGE CASES AND STRESS TESTS
// =============================================================================

module EdgeCasesTests =
    [<Fact>]
    let ``BFS and DFS give same node set`` () =
        let graph = makeGraph [ (0, 1); (0, 2); (1, 3); (2, 4) ] Directed
        let bfsResult = walk 0 BreadthFirst graph |> set
        let dfsResult = walk 0 DepthFirst graph |> set
        Assert.Equal<Set<int>>(bfsResult, dfsResult)

    [<Fact>]
    let ``traversal on undirected equals bidirectional directed`` () =
        let undirected = makeGraph [ (0, 1); (1, 2) ] Undirected

        let directed =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 1
            |> addEdge 1 0 1
            |> addEdge 1 2 1
            |> addEdge 2 1 1

        let undirectedResult = walk 0 BreadthFirst undirected |> set
        let directedResult = walk 0 BreadthFirst directed |> set
        Assert.Equal<Set<int>>(undirectedResult, directedResult)

    [<Fact>]
    let ``large linear chain BFS`` () =
        let edges = [ 0..98 ] |> List.map (fun i -> (i, i + 1))
        let graph = makeGraph edges Directed
        let result = walk 0 BreadthFirst graph
        Assert.Equal(100, result.Length)
        Assert.Equal(0, result.[0])
        Assert.Equal(99, result.[99])

    [<Fact>]
    let ``large linear chain DFS`` () =
        let edges = [ 0..98 ] |> List.map (fun i -> (i, i + 1))
        let graph = makeGraph edges Directed
        let result = walk 0 DepthFirst graph
        Assert.Equal(100, result.Length)
        Assert.Equal(0, result.[0])
        Assert.Equal(99, result.[99])

// =============================================================================
// IMPLICIT TRAVERSAL TESTS
// =============================================================================

module ImplicitTraversalTests =
    [<Fact>]
    let ``implicitFold - simple path`` () =
        let successors n = if n < 5 then [ n + 1 ] else []

        let result =
            implicitFold 0 BreadthFirst [] successors (fun acc n _ -> Continue, n :: acc)

        let sorted = result |> List.sort
        Assert.Equal<int list>([ 0; 1; 2; 3; 4; 5 ], sorted)

    [<Fact>]
    let ``implicitFoldBy - distinct elements by key`` () =
        // State is (value, flag), we dedupe by value so visiting both (1, true) and (1, false) counts only once
        let successors (v, _) =
            if v < 3 then
                [ (v + 1, true); (v + 1, false) ]
            else
                []

        let result =
            implicitFoldBy (0, true) BreadthFirst [] successors fst (fun acc (v, f) _ -> Continue, (v, f) :: acc)

        let visitedKeys = result |> List.map fst |> List.sort
        Assert.Equal<int list>([ 0; 1; 2; 3 ], visitedKeys)

    [<Fact>]
    let ``implicitDijkstra - basic`` () =
        // Walk chain 0 -> 1 -> 2 -> 3 -> 4 -> 5, cost 10 each step
        // The folder receives (acc, node, cost_so_far); at node 5, cost = 10*5 = 50
        let successors n = if n < 5 then [ (n + 1, 10) ] else []
        let total = implicitDijkstra 0 0 successors (fun acc _n cost -> Continue, cost)
        Assert.Equal(50, total)
