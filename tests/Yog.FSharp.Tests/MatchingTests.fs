/// Comprehensive tests for graph matching algorithms.
///
/// Covers:
/// - Hopcroft-Karp algorithm (maximum bipartite matching)
/// - Hungarian algorithm (min/max weight bipartite perfect matching)
/// - Edmonds' Blossom algorithm (maximum matching in general graphs)
module Yog.FSharp.Tests.MatchingTests

open System
open Xunit
open Yog.Model
open Yog.Properties.Matching

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

let makeUndirectedGraph (edges: (NodeId * NodeId) list) : Graph<unit, float> =
    let allNodes = edges |> List.collect (fun (u, v) -> [ u; v ]) |> List.distinct
    let g = empty Undirected
    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g
    edges |> List.fold (fun acc (u, v) -> addEdge u v 1.0 acc) gWithNodes

let makeWeightedBipartiteGraph (edges: (NodeId * NodeId * float) list) : Graph<unit, float> =
    let allNodes = edges |> List.collect (fun (u, v, _) -> [ u; v ]) |> List.distinct
    let g = empty Undirected
    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g
    edges |> List.fold (fun acc (u, v, w) -> addEdge u v w acc) gWithNodes

// =============================================================================
// HOPCROFT-KARP TESTS
// =============================================================================

module HopcroftKarpTests =
    [<Fact>]
    let ``hopcroftKarp - empty graph returns empty matching`` () =
        let graph = empty Undirected: Graph<unit, float>
        let matching = hopcroftKarp graph
        Assert.True(matching.IsEmpty)

    [<Fact>]
    let ``hopcroftKarp - simple path graph`` () =
        // Path: 1 - 2 - 3 - 4
        let graph = makeUndirectedGraph [ (1, 2); (2, 3); (3, 4) ]
        let matching = hopcroftKarp graph
        // Max matching size should be 2 pairs (4 map entries)
        Assert.Equal(4, matching.Count)
        Assert.True(matching.ContainsKey(1))
        Assert.True(matching.ContainsKey(2))
        Assert.True(matching.ContainsKey(3))
        Assert.True(matching.ContainsKey(4))

        let m1 = matching.[1]
        let m4 = matching.[4]
        Assert.Equal(2, m1)
        Assert.Equal(3, m4)

    [<Fact>]
    let ``hopcroftKarp - complete bipartite K_2_2`` () =
        // L: 1, 2; R: 3, 4
        let graph = makeUndirectedGraph [ (1, 3); (1, 4); (2, 3); (2, 4) ]
        let matching = hopcroftKarp graph
        Assert.Equal(4, matching.Count)

        // Every node must be matched
        for n in [ 1; 2; 3; 4 ] do
            Assert.True(matching.ContainsKey(n))

    [<Fact>]
    let ``hopcroftKarp - star graph`` () =
        // Center: 1, leaves: 2, 3, 4
        let graph = makeUndirectedGraph [ (1, 2); (1, 3); (1, 4) ]
        let matching = hopcroftKarp graph
        // Max matching size should be 1 pair (2 entries in map)
        Assert.Equal(2, matching.Count)
        Assert.True(matching.ContainsKey(1))
        let matchedLeaf = matching.[1]
        Assert.Contains(matchedLeaf, [ 2; 3; 4 ])
        Assert.Equal(1, matching.[matchedLeaf])

    [<Fact>]
    let ``hopcroftKarp - non-bipartite graph throws ArgumentException`` () =
        // Triangle: 1 - 2 - 3 - 1
        let graph = makeUndirectedGraph [ (1, 2); (2, 3); (3, 1) ]

        Assert.Throws<ArgumentException>(fun () -> hopcroftKarp graph |> ignore)
        |> ignore

// =============================================================================
// HUNGARIAN ALGORITHM TESTS
// =============================================================================

module HungarianTests =
    [<Fact>]
    let ``hungarian - min weight perfect matching`` () =
        // L: 1, 2, 3; R: 4, 5, 6
        // Complete bipartite graph
        let graph =
            makeWeightedBipartiteGraph
                [ (1, 4, 10.0)
                  (1, 5, 19.0)
                  (1, 6, 8.0)
                  (2, 4, 15.0)
                  (2, 5, 17.0)
                  (2, 6, 12.0)
                  (3, 4, 8.0)
                  (3, 5, 18.0)
                  (3, 6, 9.0) ]

        let (cost, matching) = hungarian Min graph
        Assert.Equal(33.0, cost)
        Assert.Equal(6, matching.Count)

        Assert.Equal(6, matching.[1])
        Assert.Equal(5, matching.[2])
        Assert.Equal(4, matching.[3])

    [<Fact>]
    let ``hungarian - max weight perfect matching`` () =
        // L: 1, 2, 3; R: 4, 5, 6
        let graph =
            makeWeightedBipartiteGraph
                [ (1, 4, 10.0)
                  (1, 5, 19.0)
                  (1, 6, 8.0)
                  (2, 4, 15.0)
                  (2, 5, 17.0)
                  (2, 6, 12.0)
                  (3, 4, 8.0)
                  (3, 5, 18.0)
                  (3, 6, 9.0) ]

        let (cost, matching) = hungarian Max graph
        // Max weight matching: (1, 5) => 19, (2, 4) => 15, (3, 6) => 9 => total 43
        Assert.Equal(43.0, cost)
        Assert.Equal(6, matching.Count)
        Assert.Equal(5, matching.[1])
        Assert.Equal(4, matching.[2])
        Assert.Equal(6, matching.[3])

    [<Fact>]
    let ``hungarian - rectangular partition min weight matching`` () =
        // L: 1, 2; R: 3, 4, 5. Needs dummy nodes.
        let graph =
            makeWeightedBipartiteGraph
                [ (1, 3, 10.0)
                  (1, 4, 5.0)
                  (1, 5, 20.0)
                  (2, 3, 15.0)
                  (2, 4, 12.0)
                  (2, 5, 8.0) ]

        let (cost, matching) = hungarian Min graph
        // Best matches: 1->4 (5.0), 2->5 (8.0). Dummy node matches to 3 at 0 cost.
        // Total cost should be 13.0
        Assert.Equal(13.0, cost)
        // Real nodes matched: 1->4, 2->5 (which means matching has 4 entries)
        Assert.Equal(4, matching.Count)
        Assert.Equal(4, matching.[1])
        Assert.Equal(5, matching.[2])

// =============================================================================
// EDMONDS' BLOSSOM TESTS
// =============================================================================

module BlossomTests =
    [<Fact>]
    let ``blossom - empty graph returns empty matching`` () =
        let graph = empty Undirected: Graph<unit, float>
        let matching = blossomMaximumMatching graph
        Assert.True(matching.IsEmpty)

    [<Fact>]
    let ``blossom - triangle odd cycle`` () =
        // Odd cycle 1-2-3-1
        let graph = makeUndirectedGraph [ (1, 2); (2, 3); (3, 1) ]
        let matching = blossomMaximumMatching graph
        // Max cardinality is 1 pair (2 map entries)
        Assert.Equal(2, matching.Count)

        let m1 = matching.TryFind(1)
        let m2 = matching.TryFind(2)
        let m3 = matching.TryFind(3)
        // Exactly one pair is matched
        let matches = [ m1; m2; m3 ] |> List.filter Option.isSome
        Assert.Equal(2, matches.Length)

    [<Fact>]
    let ``blossom - square even cycle`` () =
        // Even cycle 1-2-3-4-1
        let graph = makeUndirectedGraph [ (1, 2); (2, 3); (3, 4); (4, 1) ]
        let matching = blossomMaximumMatching graph
        // Max cardinality is 2 pairs (4 map entries)
        Assert.Equal(4, matching.Count)

        for n in [ 1; 2; 3; 4 ] do
            Assert.True(matching.ContainsKey(n))

    [<Fact>]
    let ``blossom - petersen graph is perfect matching`` () =
        // Petersen graph has 10 nodes and a perfect matching of size 5
        let edges =
            [ (0, 1)
              (1, 2)
              (2, 3)
              (3, 4)
              (4, 0) // Outer cycle
              (5, 7)
              (7, 9)
              (9, 6)
              (6, 8)
              (8, 5) // Inner star
              (0, 5)
              (1, 6)
              (2, 7)
              (3, 8)
              (4, 9) ] // Spokes

        let graph = makeUndirectedGraph edges
        let matching = blossomMaximumMatching graph
        Assert.Equal(10, matching.Count)

        for n in 0..9 do
            Assert.True(matching.ContainsKey(n))


// =============================================================================
// MATCHING PROPERTY TESTS
// =============================================================================

module MatchingPropertyTests =
    open Hedgehog
    open Hedgehog.FSharp
    open Yog.Properties.Bipartite

    /// Generates a random bipartite graph with disjoint left/right partitions.
    let bipartiteGraphGen: Gen<Graph<unit, float>> =
        gen {
            let! leftSize = Gen.int32 (Range.linear 0 8)
            let! rightSize = Gen.int32 (Range.linear 0 8)
            let left = [ 0 .. leftSize - 1 ]
            let right = [ leftSize .. leftSize + rightSize - 1 ]

            let g =
                (empty Undirected)
                |> fun g -> left |> List.fold (fun acc n -> addNode n () acc) g
                |> fun g -> right |> List.fold (fun acc n -> addNode n () acc) g

            let possibleEdges =
                [ for u in left do
                      for v in right -> (u, v) ]

            let! includeEdge = Gen.array (Range.constant possibleEdges.Length possibleEdges.Length) Gen.bool

            let chosen =
                possibleEdges
                |> List.zip (includeEdge |> Array.toList)
                |> List.filter fst
                |> List.map snd

            return chosen |> List.fold (fun acc (u, v) -> addEdge u v 1.0 acc) g
        }

    let private toUnorderedEdges (m: Map<NodeId, NodeId>) =
        m
        |> Map.toSeq
        |> Seq.map (fun (u, v) -> if u < v then (u, v) else (v, u))
        |> Set.ofSeq

    let private isVertexDisjoint (m: Map<NodeId, NodeId>) =
        let edges = toUnorderedEdges m |> Set.toList
        let endpoints = edges |> List.collect (fun (u, v) -> [ u; v ])
        List.length endpoints = (List.length edges * 2)

    let private allMatchedEdgesExist (graph: Graph<unit, float>) (m: Map<NodeId, NodeId>) =
        toUnorderedEdges m |> Set.forall (fun (u, v) -> hasEdge u v graph)

    [<Fact>]
    let ``blossom matching is vertex-disjoint and edges exist`` () =
        property {
            let! g = bipartiteGraphGen
            let m = blossomMaximumMatching g
            return isVertexDisjoint m && allMatchedEdgesExist g m
        }
        |> Property.checkBool

    [<Fact>]
    let ``hopcroft-karp matching is vertex-disjoint and edges exist`` () =
        property {
            let! g = bipartiteGraphGen
            let m = hopcroftKarp g
            return isVertexDisjoint m && allMatchedEdgesExist g m
        }
        |> Property.checkBool

    [<Fact>]
    let ``blossom and hopcroft-karp agree on matching size for bipartite graphs`` () =
        property {
            let! g = bipartiteGraphGen
            let blossom = blossomMaximumMatching g
            let hk = hopcroftKarp g
            return blossom.Count = hk.Count
        }
        |> Property.checkBool
