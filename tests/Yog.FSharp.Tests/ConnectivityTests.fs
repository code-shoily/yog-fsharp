/// Comprehensive tests for graph connectivity algorithms.
module Yog.FSharp.Tests.ConnectivityTests

open Xunit
open Yog
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

let runAnalyze (graph: Graph<'n, 'e>) : ConnectivityResults =
    match analyze graph with
    | Ok res -> res
    | Error msg -> failwith msg

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
        let graph = makeDirectedGraph [ (0, 1); (1, 2) ]
        let result = stronglyConnectedComponents graph
        Assert.Equal(3, result.Length)

    [<Fact>]
    let ``SCC - simple cycle single component`` () =
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
        Assert.Equal(tarjanResult.Length, kosarajuResult.Length)
        let tarjanSizes = tarjanResult |> List.map (fun c -> c.Length) |> List.sort
        let kosarajuSizes = kosarajuResult |> List.map (fun c -> c.Length) |> List.sort
        Assert.Equal<int list>(tarjanSizes, kosarajuSizes)

// =============================================================================
// BRIDGES (CUT EDGES) TESTS
// =============================================================================

module BridgesTests =
    [<Fact>]
    let ``Bridges - empty graph`` () =
        let graph = empty Undirected
        let result = runAnalyze graph
        Assert.Equal(0, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - single edge is bridge`` () =
        let graph = makeUndirectedGraph [ (0, 1) ]
        let result = runAnalyze graph
        Assert.Equal(1, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - triangle has no bridges`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 0) ]
        let result = runAnalyze graph
        Assert.Equal(0, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - chain has all edges as bridges`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ]
        let result = runAnalyze graph
        Assert.Equal(3, result.Bridges.Length)

    [<Fact>]
    let ``Bridges - analyze on directed returns Error`` () =
        let graph = makeDirectedGraph [ (0, 1) ]
        let res = analyze graph

        match res with
        | Error msg -> Assert.Contains("requires an undirected graph", msg)
        | Ok _ -> failwith "Expected directed graph analyze call to fail"

// =============================================================================
// ARTICULATION POINTS (CUT VERTICES) TESTS
// =============================================================================

module ArticulationPointsTests =
    [<Fact>]
    let ``Articulation points - empty graph`` () =
        let graph = empty Undirected
        let result = runAnalyze graph
        Assert.Equal(0, result.ArticulationPoints.Length)

    [<Fact>]
    let ``Articulation points - chain`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 3) ]
        let result = runAnalyze graph
        Assert.Equal(2, result.ArticulationPoints.Length)
        Assert.True(result.ArticulationPoints |> List.contains 1)
        Assert.True(result.ArticulationPoints |> List.contains 2)

// =============================================================================
// CONNECTED & WEAKLY CONNECTED COMPONENTS TESTS
// =============================================================================

module ConnectedComponentsTests =
    [<Fact>]
    let ``Connected components - simple undirected`` () =
        let graph = makeUndirectedGraph [ (0, 1); (2, 3) ]
        let comps = connectedComponents graph
        Assert.Equal(2, comps.Length)
        let sizes = comps |> List.map (fun c -> c.Length) |> List.sort
        Assert.Equal<int list>([ 2; 2 ], sizes)

    [<Fact>]
    let ``Weakly connected components - directed`` () =
        let graph = makeDirectedGraph [ (0, 1); (2, 1) ] // 0 -> 1 <- 2
        let comps = weaklyConnectedComponents graph
        Assert.Equal(1, comps.Length)
        Assert.Equal(3, comps.[0].Length)

// =============================================================================
// K-CORE DECOMPOSITION TESTS
// =============================================================================

module KCoreTests =
    [<Fact>]
    let ``k-core and degeneracy of a triangle`` () =
        let graph = makeUndirectedGraph [ (0, 1); (1, 2); (2, 0) ]

        let cores =
            match coreNumbers graph with
            | Ok c -> c
            | Error msg -> failwith msg

        Assert.Equal(2, Map.find 0 cores)
        Assert.Equal(2, Map.find 1 cores)
        Assert.Equal(2, Map.find 2 cores)

        let degen =
            match degeneracy graph with
            | Ok d -> d
            | Error msg -> failwith msg

        Assert.Equal(2, degen)

        let k2Core =
            match kCore graph 2 with
            | Ok g -> g
            | Error msg -> failwith msg

        Assert.Equal(3, nodeCount k2Core)

        let k3Core =
            match kCore graph 3 with
            | Ok g -> g
            | Error msg -> failwith msg

        Assert.Equal(0, nodeCount k3Core)

    [<Fact>]
    let ``shell decomposition of a star`` () =
        let graph = makeUndirectedGraph [ (0, 1); (0, 2); (0, 3) ]

        let shells =
            match shellDecomposition graph with
            | Ok s -> s
            | Error msg -> failwith msg
        // Star has center with degree 3 and leaves with degree 1.
        // Pruning leaves (deg 1 < 2) removes them, then center's degree becomes 0 < 2, so center also pruned.
        // Therefore, core numbers are all 1.
        Assert.Equal(1, shells.Count)
        Assert.Equal(4, (Map.find 1 shells).Length)

module ReachabilityTests =
    open Yog.Connectivity.Reachability

    [<Fact>]
    let ``Reachability - counts descendants for acyclic graph`` () =
        let graph = makeDirectedGraph [ (1, 2); (2, 3); (1, 4) ]
        let counts = counts graph Descendants
        Assert.Equal(3, Map.find 1 counts)
        Assert.Equal(1, Map.find 2 counts)
        Assert.Equal(0, Map.find 3 counts)
        Assert.Equal(0, Map.find 4 counts)

    [<Fact>]
    let ``Reachability - counts ancestors for acyclic graph`` () =
        let graph = makeDirectedGraph [ (1, 2); (2, 3); (1, 4) ]
        let counts = counts graph Ancestors
        Assert.Equal(0, Map.find 1 counts)
        Assert.Equal(1, Map.find 2 counts)
        Assert.Equal(2, Map.find 3 counts)
        Assert.Equal(1, Map.find 4 counts)

    [<Fact>]
    let ``Reachability - cyclic graphs via condensation`` () =
        let graph =
            empty Directed
            |> addNode 1 ()
            |> addNode 2 ()
            |> addNode 3 ()
            |> addEdge 1 2 1
            |> addEdge 2 1 1
            |> addEdge 2 3 1

        let c = counts graph Descendants
        Assert.Equal(2, Map.find 1 c)
        Assert.Equal(2, Map.find 2 c)
        Assert.Equal(0, Map.find 3 c)

    [<Fact>]
    let ``Reachability - HLL estimates for acyclic graph`` () =
        let graph = makeDirectedGraph [ (1, 2); (2, 3) ]
        let est = countsEstimate graph Descendants
        Assert.True(Map.find 1 est >= 1)
        Assert.Equal(0, Map.find 3 est)
