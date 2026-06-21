/// Comprehensive tests for Minimum Spanning Tree algorithms.
///
/// Covers:
/// - Kruskal's algorithm
/// - Prim's algorithm
/// - Borůvka's algorithm
/// - Edmonds' algorithm (Minimum Spanning Arborescence)
/// - Wilson's algorithm (Uniform Spanning Tree)
/// - Edge cases and various graph structures
module Yog.FSharp.Tests.MSTTests

open Xunit
open Yog
open Yog.Model
open Yog.Mst

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

let makeUndirectedWeightedGraph (edges: (NodeId * NodeId * int) list) : Graph<unit, int> =
    let allNodes = edges |> List.collect (fun (u, v, _) -> [ u; v ]) |> List.distinct
    let g = empty Undirected
    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g
    edges |> List.fold (fun acc (u, v, w) -> addEdge u v w acc) gWithNodes

let makeDirectedWeightedGraph (edges: (NodeId * NodeId * int) list) : Graph<unit, int> =
    let allNodes = edges |> List.collect (fun (u, v, _) -> [ u; v ]) |> List.distinct
    let g = empty Directed
    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g
    edges |> List.fold (fun acc (u, v, w) -> addEdge u v w acc) gWithNodes

let totalWeight (edges: Edge<int> list) : int = edges |> List.sumBy (fun e -> e.Weight)

let edgeSet (edges: Edge<'e> list) : Set<(NodeId * NodeId)> =
    edges |> List.map (fun e -> (min e.From e.To, max e.From e.To)) |> Set.ofList

let runKruskal (graph: Graph<'n, int>) : Edge<int> list =
    match kruskalInt graph with
    | Ok res -> res.Edges
    | Error msg -> failwith msg

let runPrim (graph: Graph<'n, int>) : Edge<int> list =
    match primInt graph with
    | Ok res -> res.Edges
    | Error msg -> failwith msg

let runBoruvka (graph: Graph<'n, int>) : Edge<int> list =
    match boruvkaInt graph with
    | Ok res -> res.Edges
    | Error msg -> failwith msg

let runEdmonds (root: NodeId) (graph: Graph<'n, int>) : Edge<int> list =
    match edmondsInt root graph with
    | Ok res -> res.Edges
    | Error msg -> failwith msg

let runWilson (seed: int option) (graph: Graph<'n, int>) : Edge<int> list =
    match wilsonInt seed graph with
    | Ok res -> res.Edges
    | Error msg -> failwith msg

// =============================================================================
// KRUSKAL'S ALGORITHM TESTS
// =============================================================================

module KruskalTests =
    [<Fact>]
    let ``kruskal - simple triangle`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (1, 2, 2); (0, 2, 3) ]
        let result = runKruskal graph
        Assert.Equal(2, result.Length)
        Assert.Equal(3, totalWeight result)

    [<Fact>]
    let ``kruskal - square with diagonal`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (1, 2, 2); (2, 3, 3); (0, 3, 4); (0, 2, 5) ]
        let result = runKruskal graph
        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``kruskal - complete graph K4`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (0, 2, 2); (0, 3, 3); (1, 2, 4); (1, 3, 5); (2, 3, 6) ]
        let result = runKruskal graph
        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``kruskal - already a tree`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 5); (1, 2, 3); (1, 3, 2) ]
        let result = runKruskal graph
        Assert.Equal(3, result.Length)
        Assert.Equal(10, totalWeight result)

    [<Fact>]
    let ``kruskal - single edge`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 42) ]
        let result = runKruskal graph
        Assert.Equal(1, result.Length)
        Assert.Equal(0, result.[0].From)
        Assert.Equal(1, result.[0].To)
        Assert.Equal(42, result.[0].Weight)

    [<Fact>]
    let ``kruskal - empty graph`` () =
        let graph = empty Undirected
        let result = runKruskal graph
        Assert.Empty(result)

    [<Fact>]
    let ``kruskal - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = runKruskal graph
        Assert.Empty(result)

    [<Fact>]
    let ``kruskal - two nodes`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 100) ]
        let result = runKruskal graph
        Assert.Equal(1, result.Length)
        Assert.Equal(100, result.[0].Weight)

    [<Fact>]
    let ``kruskal - disconnected graph gives forest`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (2, 3, 2) ]
        let result = runKruskal graph
        Assert.Equal(2, result.Length)
        Assert.Equal(3, totalWeight result)

    [<Fact>]
    let ``kruskal - equal weights`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (0, 2, 1); (1, 2, 1) ]
        let result = runKruskal graph
        Assert.Equal(2, result.Length)
        Assert.Equal(2, totalWeight result)

    [<Fact>]
    let ``kruskal - large weights`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1000000); (0, 2, 2000000); (1, 2, 3000000) ]
        let result = runKruskal graph
        Assert.Equal(2, result.Length)
        Assert.Equal(3000000, totalWeight result)

// =============================================================================
// PRIM'S ALGORITHM TESTS
// =============================================================================

module PrimTests =
    [<Fact>]
    let ``prim - simple triangle`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (1, 2, 2); (0, 2, 3) ]
        let result = runPrim graph
        Assert.Equal(2, result.Length)
        Assert.Equal(3, totalWeight result)

    [<Fact>]
    let ``prim - square with diagonal`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (1, 2, 2); (2, 3, 3); (0, 3, 4); (0, 2, 5) ]
        let result = runPrim graph
        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``prim - complete graph K4`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (0, 2, 2); (0, 3, 3); (1, 2, 4); (1, 3, 5); (2, 3, 6) ]
        let result = runPrim graph
        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``prim - already a tree`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 5); (1, 2, 3); (1, 3, 2) ]
        let result = runPrim graph
        Assert.Equal(3, result.Length)
        Assert.Equal(10, totalWeight result)

    [<Fact>]
    let ``prim - single edge`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 42) ]
        let result = runPrim graph
        Assert.Equal(1, result.Length)
        Assert.Equal(42, result.[0].Weight)

    [<Fact>]
    let ``prim - empty graph`` () =
        let graph = empty Undirected
        let result = runPrim graph
        Assert.Empty(result)

    [<Fact>]
    let ``prim - single node`` () =
        let graph = empty Undirected |> addNode 0 ()
        let result = runPrim graph
        Assert.Empty(result)

    [<Fact>]
    let ``prim - star graph`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (0, 2, 2); (0, 3, 3); (0, 4, 4) ]
        let result = runPrim graph
        Assert.Equal(4, result.Length)
        Assert.Equal(10, totalWeight result)

    [<Fact>]
    let ``prim - path graph`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 5); (1, 2, 4); (2, 3, 3); (3, 4, 2) ]
        let result = runPrim graph
        Assert.Equal(4, result.Length)
        Assert.Equal(14, totalWeight result)

    [<Fact>]
    let ``prim - disconnected graph returns only first component`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (2, 3, 2) ]
        let result = runPrim graph
        Assert.Equal(1, result.Length)
        Assert.Equal(1, totalWeight result)
        let edges = edgeSet result
        Assert.True(edges.Contains((0, 1)))

// =============================================================================
// BORUVKA'S ALGORITHM TESTS
// =============================================================================

module BoruvkaTests =
    [<Fact>]
    let ``boruvka - simple triangle`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (1, 2, 2); (0, 2, 3) ]
        let result = runBoruvka graph
        Assert.Equal(2, result.Length)
        Assert.Equal(3, totalWeight result)

    [<Fact>]
    let ``boruvka - square with diagonal`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (1, 2, 2); (2, 3, 3); (0, 3, 4); (0, 2, 5) ]
        let result = runBoruvka graph
        Assert.Equal(3, result.Length)
        Assert.Equal(6, totalWeight result)

    [<Fact>]
    let ``boruvka - already a tree`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 5); (1, 2, 3); (1, 3, 2) ]
        let result = runBoruvka graph
        Assert.Equal(3, result.Length)
        Assert.Equal(10, totalWeight result)

// =============================================================================
// EDMONDS' ALGORITHM TESTS
// =============================================================================

module EdmondsTests =
    [<Fact>]
    let ``edmonds - simple MSA`` () =
        let graph = makeDirectedWeightedGraph [ (0, 1, 10); (0, 2, 20); (1, 2, 5) ]
        let result = runEdmonds 0 graph
        Assert.Equal(2, result.Length)
        Assert.Equal(15, totalWeight result)

    [<Fact>]
    let ``edmonds - with cycle`` () =
        let graph = makeDirectedWeightedGraph [ (0, 1, 10); (0, 2, 20); (1, 2, 5); (2, 1, 3) ]
        let result = runEdmonds 0 graph
        Assert.Equal(2, result.Length)
        Assert.Equal(15, totalWeight result)

    [<Fact>]
    let ``edmonds - unreachable returns error`` () =
        let graph = makeDirectedWeightedGraph [ (0, 1, 10); (2, 3, 5) ]
        let res = edmondsInt 0 graph
        match res with
        | Error msg -> Assert.Contains("No arborescence exists", msg)
        | Ok _ -> failwith "Expected failure"

// =============================================================================
// WILSON'S ALGORITHM TESTS
// =============================================================================

module WilsonTests =
    [<Fact>]
    let ``wilson - simple triangle`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 1); (1, 2, 2); (0, 2, 3) ]
        let result1 = runWilson (Some 42) graph
        Assert.Equal(2, result1.Length)
        let result2 = runWilson (Some 42) graph
        Assert.Equal<Edge<int> list>(result1, result2)

// =============================================================================
// KRUSKAL VS PRIM VS BORUVKA COMPARISON TESTS
// =============================================================================

module ComparisonTests =
    [<Fact>]
    let ``kruskal prim and boruvka give same total weight`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 4); (0, 2, 3); (0, 3, 1); (1, 2, 2); (1, 3, 5); (2, 3, 6) ]
        let kruskalResult = runKruskal graph
        let primResult = runPrim graph
        let boruvkaResult = runBoruvka graph
        Assert.Equal(totalWeight kruskalResult, totalWeight primResult)
        Assert.Equal(totalWeight kruskalResult, totalWeight boruvkaResult)

    [<Fact>]
    let ``algorithms produce same number of edges`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 10); (0, 2, 20); (0, 3, 30); (1, 2, 5); (1, 3, 15); (2, 3, 25) ]
        let kruskalResult = runKruskal graph
        let primResult = runPrim graph
        let boruvkaResult = runBoruvka graph
        Assert.Equal(kruskalResult.Length, primResult.Length)
        Assert.Equal(kruskalResult.Length, boruvkaResult.Length)
        Assert.Equal(3, kruskalResult.Length)

// =============================================================================
// EDGE CASE TESTS
// =============================================================================

module EdgeCaseTests =
    [<Fact>]
    let ``MST with zero weight edges`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, 0); (1, 2, 0); (0, 2, 5) ]
        let result = runKruskal graph
        Assert.Equal(2, result.Length)
        Assert.Equal(0, totalWeight result)

    [<Fact>]
    let ``MST with negative weights`` () =
        let graph = makeUndirectedWeightedGraph [ (0, 1, -5); (1, 2, -3); (0, 2, 10) ]
        let result = runKruskal graph
        Assert.Equal(2, result.Length)
        Assert.Equal(-8, totalWeight result)

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
        let result =
            match kruskalFloat graph with
            | Ok res -> res.Edges
            | Error msg -> failwith msg
        Assert.Equal(2, result.Length)
