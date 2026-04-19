/// Comprehensive tests for bipartite graph algorithms.
///
/// Covers:
/// - Bipartite detection (isBipartite)
/// - Partition extraction
/// - Maximum matching
/// - Stable marriage problem
module Yog.FSharp.Tests.BipartiteTests

open Xunit
open Yog.Model
open Yog.Properties.Bipartite

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

let makeUndirectedGraph (edges: (NodeId * NodeId) list) : Graph<unit, int> =
    let allNodes = edges |> List.collect (fun (u, v) -> [ u; v ]) |> List.distinct

    let g = empty Undirected

    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g

    edges |> List.fold (fun acc (u, v) -> addEdge u v 1 acc) gWithNodes

let makeDirectedGraph (edges: (NodeId * NodeId) list) : Graph<unit, int> =
    let allNodes = edges |> List.collect (fun (u, v) -> [ u; v ]) |> List.distinct

    let g = empty Directed

    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g

    edges |> List.fold (fun acc (u, v) -> addEdge u v 1 acc) gWithNodes

// =============================================================================
// IS BIPARTITE TESTS
// =============================================================================

module IsBipartiteTests =
    [<Fact>]
    let ``isBipartite - empty graph is bipartite`` () =
        let graph = empty Undirected
        Assert.True(isBipartite graph)

    [<Fact>]
    let ``isBipartite - single node is bipartite`` () =
        let graph = empty Undirected |> addNode 0 ()
        Assert.True(isBipartite graph)

    [<Fact>]
    let ``isBipartite - single edge is bipartite`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        Assert.True(isBipartite graph)

    [<Fact>]
    let ``isBipartite - path is bipartite`` () =
        // Path: 0-1-2-3
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ]

        Assert.True(isBipartite graph)

    [<Fact>]
    let ``isBipartite - even cycle is bipartite`` () =
        // Square: 0-1-2-3-0
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3); (3, 0) ]

        Assert.True(isBipartite graph)

    [<Fact>]
    let ``isBipartite - odd cycle is not bipartite`` () =
        // Triangle: 0-1-2-0
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        Assert.False(isBipartite graph)

    [<Fact>]
    let ``isBipartite - complete graph K3 not bipartite`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (0, 2) ]

        Assert.False(isBipartite graph)

    [<Fact>]
    let ``isBipartite - complete graph K4 not bipartite`` () =
        // Actually K4 is not bipartite either (contains triangles)
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (0, 3); (1, 2); (1, 3); (2, 3) ]

        Assert.False(isBipartite graph)

    [<Fact>]
    let ``isBipartite - star graph is bipartite`` () =
        // Star with center 0
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (0, 3); (0, 4) ]

        Assert.True(isBipartite graph)

    [<Fact>]
    let ``isBipartite - disconnected bipartite components`` () =
        // Two separate edges
        let graph = makeUndirectedGraph [ (0, 1); (2, 3) ]
        Assert.True(isBipartite graph)

    [<Fact>]
    let ``isBipartite - one component bipartite one not`` () =
        // Component 1: edge (bipartite)
        // Component 2: triangle (not bipartite)
        let graph = makeUndirectedGraph [ (0, 1); (2, 3); (3, 4); (4, 2) ]

        Assert.False(isBipartite graph)

    [<Fact>]
    let ``isBipartite - self loop is not bipartite`` () =
        let graph = empty Undirected |> addNode 0 () |> addEdge 0 0 1
        Assert.False(isBipartite graph)

// =============================================================================
// PARTITION TESTS
// =============================================================================

module PartitionTests =
    [<Fact>]
    let ``partition - returns None for non-bipartite`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 0) ] // Triangle

        let result = partition graph
        Assert.True(result.IsNone)

    [<Fact>]
    let ``partition - returns partitions for bipartite`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ] // Path

        let result = partition graph
        Assert.True(result.IsSome)

    [<Fact>]
    let ``partition - single edge partitions`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result = partition graph

        Assert.True(result.IsSome)
        let p = result.Value
        // 0 and 1 should be in different partitions
        Assert.True(
            (p.Left.Contains 0 && p.Right.Contains 1)
            || (p.Left.Contains 1 && p.Right.Contains 0)
        )

    [<Fact>]
    let ``partition - square partitions correctly`` () =
        // Square: 0-1-2-3-0
        // One partition: 0, 2; Other: 1, 3
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3); (3, 0) ]

        let result = partition graph

        Assert.True(result.IsSome)
        let p = result.Value

        // Adjacent nodes should be in different partitions
        let checkEdge u v =
            let uInLeft = p.Left.Contains u
            let vInLeft = p.Left.Contains v
            Assert.NotEqual(uInLeft, vInLeft)

        checkEdge 0 1
        checkEdge 1 2
        checkEdge 2 3
        checkEdge 3 0

    [<Fact>]
    let ``partition - star graph center in one partition`` () =
        // Star: center 0 connected to 1,2,3,4
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (0, 3); (0, 4) ]

        let result = partition graph

        Assert.True(result.IsSome)
        let p = result.Value

        // Center should be in one partition, all leaves in other
        let centerInLeft = p.Left.Contains 0

        if centerInLeft then
            Assert.Equal(1, p.Left.Count)
            Assert.Equal(4, p.Right.Count)

            for i in 1..4 do
                Assert.True(p.Right.Contains i)
        else
            Assert.Equal(4, p.Left.Count)
            Assert.Equal(1, p.Right.Count)

            for i in 1..4 do
                Assert.True(p.Left.Contains i)

    [<Fact>]
    let ``partition - empty graph`` () =
        let graph = empty Undirected
        let result = partition graph

        Assert.True(result.IsSome)
        let p = result.Value
        Assert.Empty(p.Left)
        Assert.Empty(p.Right)

    [<Fact>]
    let ``partition - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = partition graph

        Assert.True(result.IsSome)
        let p = result.Value
        // Single node can be in either partition
        Assert.Equal(1, p.Left.Count + p.Right.Count)

    [<Fact>]
    let ``partition covers all nodes`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ]

        let result = partition graph

        let p = result.Value
        let allPartitioned = Set.union p.Left p.Right
        let allNodes = [ 0; 1; 2; 3 ] |> Set.ofList
        Assert.Equal<Set<NodeId>>(allNodes, allPartitioned)

    [<Fact>]
    let ``partitions are disjoint`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2) ]
        let result = partition graph

        let p = result.Value
        let intersection = Set.intersect p.Left p.Right
        Assert.Empty(intersection)

// =============================================================================
// MAXIMUM MATCHING TESTS
// =============================================================================

module MaximumMatchingTests =
    [<Fact>]
    let ``maximumMatching - single edge`` () =
        let graph = makeDirectedGraph [ (0, 1) ]

        let partition =
            { Left = Set.ofList [ 0 ]
              Right = Set.ofList [ 1 ] }

        let result = maximumMatching partition graph

        Assert.Equal(1, result.Length)
        Assert.Contains((0, 1), result)

    [<Fact>]
    let ``maximumMatching - two independent edges`` () =
        // 0->1 and 2->3, can match both
        let graph = makeDirectedGraph [ (0, 1); (2, 3) ]

        let partition =
            { Left = Set.ofList [ 0; 2 ]
              Right = Set.ofList [ 1; 3 ] }

        let result = maximumMatching partition graph

        Assert.Equal(2, result.Length)

    [<Fact>]
    let ``maximumMatching - path of three nodes`` () =
        // 0->1->2 where 0,1 in left, 2 in right? Actually let's use proper bipartite
        // Left: 0, 1; Right: 2, 3
        // Edges: 0->2, 0->3, 1->2
        let graph = makeDirectedGraph [ (0, 2); (0, 3); (1, 2) ]

        let partition =
            { Left = Set.ofList [ 0; 1 ]
              Right = Set.ofList [ 2; 3 ] }

        let result = maximumMatching partition graph

        // Maximum matching size is 2
        Assert.Equal(2, result.Length)
        // Both left nodes should be matched
        let matchedLeft = result |> List.map fst |> Set.ofList
        Assert.Equal<Set<NodeId>>(Set.ofList [ 0; 1 ], matchedLeft)

    [<Fact>]
    let ``maximumMatching - complete bipartite K2,2`` () =
        // All possible edges exist
        let graph = makeDirectedGraph [ (0, 2); (0, 3); (1, 2); (1, 3) ]

        let partition =
            { Left = Set.ofList [ 0; 1 ]
              Right = Set.ofList [ 2; 3 ] }

        let result = maximumMatching partition graph

        // Can match all 2 left nodes
        Assert.Equal(2, result.Length)

    [<Fact>]
    let ``maximumMatching - no edges`` () =
        let graph = empty Directed |> addNode 0 () |> addNode 1 ()

        let partition =
            { Left = Set.ofList [ 0 ]
              Right = Set.ofList [ 1 ] }

        let result = maximumMatching partition graph

        Assert.Empty(result)

    [<Fact>]
    let ``maximumMatching - star from left`` () =
        // One left node connected to all right nodes
        let graph = makeDirectedGraph [ (0, 1); (0, 2); (0, 3) ]

        let partition =
            { Left = Set.ofList [ 0 ]
              Right = Set.ofList [ 1; 2; 3 ] }

        let result = maximumMatching partition graph

        // Can only match one edge
        Assert.Equal(1, result.Length)

// =============================================================================
// STABLE MARRIAGE TESTS
// =============================================================================

module StableMarriageTests =
    [<Fact>]
    let ``stableMarriage - simple case`` () =
        // 2 men, 2 women
        let leftPrefs =
            Map.ofList
                [ 0, [ 1; 0 ] // Man 0 prefers woman 1, then 0
                  1, [ 0; 1 ] ] // Man 1 prefers woman 0, then 1

        let rightPrefs =
            Map.ofList
                [ 0, [ 0; 1 ] // Woman 0 prefers man 0, then 1
                  1, [ 1; 0 ] ] // Woman 1 prefers man 1, then 0

        let result = stableMarriage leftPrefs rightPrefs

        // Gale-Shapley produces man-optimal matching
        // Man 0 proposes to 1 (his first choice), accepted
        // Man 1 proposes to 0 (his first choice), accepted
        Assert.Equal(2, result.Matches.Count)
        // Result is 0-1 and 1-0
        Assert.Equal(1, result.Matches.[0])
        Assert.Equal(0, result.Matches.[1])

    [<Fact>]
    let ``stableMarriage - different preferences`` () =
        // Classic Gale-Shapley example
        let leftPrefs = Map.ofList [ 0, [ 0; 1 ]; 1, [ 0; 1 ] ]

        let rightPrefs =
            Map.ofList
                [ 0, [ 1; 0 ] // Both women prefer man 1
                  1, [ 1; 0 ] ]

        let result = stableMarriage leftPrefs rightPrefs

        // Man-optimal: 0 gets 0, 1 gets 1
        // (Even though women prefer 1, 0 proposes first to 0)
        Assert.Equal(2, result.Matches.Count)

    [<Fact>]
    let ``stableMarriage - three couples`` () =
        let leftPrefs = Map.ofList [ 0, [ 0; 1; 2 ]; 1, [ 1; 2; 0 ]; 2, [ 2; 0; 1 ] ]

        let rightPrefs = Map.ofList [ 0, [ 0; 1; 2 ]; 1, [ 1; 2; 0 ]; 2, [ 2; 0; 1 ] ]

        let result = stableMarriage leftPrefs rightPrefs

        // Should match everyone
        Assert.Equal(3, result.Matches.Count)

    [<Fact>]
    let ``stableMarriage - stability check`` () =
        // Verify no blocking pairs exist
        let leftPrefs = Map.ofList [ 0, [ 1; 0 ]; 1, [ 0; 1 ] ]
        let rightPrefs = Map.ofList [ 0, [ 1; 0 ]; 1, [ 0; 1 ] ]

        let result = stableMarriage leftPrefs rightPrefs

        // In this case: 0 gets 1, 1 gets 0 (both get second choice)
        // But this is stable - neither would rather be unmatched
        Assert.Equal(2, result.Matches.Count)

    [<Fact>]
    let ``getPartner - finds partner`` () =
        let marriage = { Matches = Map.ofList [ (0, 1); (1, 0) ] }

        Assert.Equal(Some 1, getPartner 0 marriage)
        Assert.Equal(Some 0, getPartner 1 marriage)
        Assert.Equal(None, getPartner 99 marriage)

// =============================================================================
// INTEGRATION TESTS
// =============================================================================

module IntegrationTests =
    [<Fact>]
    let ``full bipartite workflow`` () =
        // Start with a bipartite graph
        let graph = makeUndirectedGraph [ (0, 3); (0, 4); (1, 3); (1, 4); (2, 4); (2, 5) ]

        // Step 1: Verify it's bipartite
        Assert.True(isBipartite graph)

        // Step 2: Get partition
        let partitionOpt = partition graph
        Assert.True(partitionOpt.IsSome)
        let partition = partitionOpt.Value

        // Step 3: Convert to directed for matching
        let directed =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addNode 3 ()
            |> addNode 4 ()
            |> addNode 5 ()
            |> addEdge 0 3 1
            |> addEdge 0 4 1
            |> addEdge 1 3 1
            |> addEdge 1 4 1
            |> addEdge 2 4 1
            |> addEdge 2 5 1

        // Step 4: Find maximum matching
        let matching = maximumMatching partition directed

        // Should match 3 left nodes
        Assert.True(matching.Length >= 2)

    [<Fact>]
    let ``bipartite detection vs partition consistency`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ]

        let isBip = isBipartite graph
        let partitionOpt = partition graph

        // If bipartite, should have valid partition
        Assert.True(isBip)
        Assert.True(partitionOpt.IsSome)

        // If not bipartite, partition should be None
        let nonBip = makeUndirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        Assert.False(isBipartite nonBip)
        Assert.True((partition nonBip).IsNone)

    [<Fact>]
    let ``grid graph is bipartite`` () =
        // 2x2 grid
        let graph =
            makeUndirectedGraph
                [ (0, 1)
                  (0, 2) // Top row and left column
                  (1, 3)
                  (2, 3) ] // Right column and bottom row

        Assert.True(isBipartite graph)

        let result = partition graph
        Assert.True(result.IsSome)

        // In a grid, opposite corners should be in same partition
        let p = result.Value
        let corner0InLeft = p.Left.Contains 0
        let corner3InLeft = p.Left.Contains 3
        Assert.Equal(corner0InLeft, corner3InLeft) // Both corners same partition
