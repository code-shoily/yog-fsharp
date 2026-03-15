module Yog.FSharp.Tests.AggressivePropertyTests


open Xunit
open Yog.Model
open Yog.Transform

// ============================================================================
// EDGE CASE: Empty Graphs
// ============================================================================

[<Fact>]
let ``empty graph has zero edges and nodes`` () =
    let directed = empty Directed
    let undirected = empty Undirected

    Assert.Equal(0, edgeCount directed)
    Assert.Equal(0, edgeCount undirected)
    Assert.Equal(0, order directed)
    Assert.Equal(0, order undirected)

[<Fact>]
let ``empty graph transpose is empty`` () =
    let directed = empty Directed
    let transposed = transpose directed

    Assert.Equal(0, order transposed)
    Assert.Equal(0, edgeCount transposed)

// ============================================================================
// EDGE CASE: Self-Loops
// ============================================================================

[<Fact>]
let ``self loop in directed graph`` () =
    let graph = empty Directed |> addNode 0 0 |> addEdge 0 0 10

    // Should have 1 edge
    Assert.Equal(1, edgeCount graph)

    // Node 0 should be its own successor
    let succs = successors 0 graph
    Assert.Equal(1, List.length succs)
    Assert.True(succs |> List.exists (fun (id, _) -> id = 0))

[<Fact>]
let ``self loop in undirected graph`` () =
    let graph = empty Undirected |> addNode 0 0 |> addEdge 0 0 10

    // Should have at least 1 edge representation
    let succs = successors 0 graph
    Assert.True(List.length succs >= 1)

// ============================================================================
// EDGE CASE: Multiple Edges Between Same Nodes
// ============================================================================

[<Fact>]
let ``adding same edge twice replaces weight`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addEdge 0 1 10
        |> addEdge 0 1 20 // Replace weight

    Assert.Equal(1, edgeCount graph)

    let succs = successors 0 graph
    Assert.Equal(1, List.length succs)

    // Weight should be the latest (20)
    match List.tryHead succs with
    | Some (_, weight) -> Assert.Equal(20, weight)
    | None -> Assert.True(false, "Should have successor")

// ============================================================================
// EDGE CASE: Remove Edge That Doesn't Exist
// ============================================================================

[<Fact>]
let ``removing nonexistent edge is no-op`` () =
    let graph = empty Directed |> addNode 0 0 |> addNode 1 1

    // No edge exists, try to remove it
    let removed = removeEdge 0 1 graph

    // Should be a no-op
    Assert.Equal(edgeCount graph, edgeCount removed)
    Assert.Equal(order graph, order removed)

// ============================================================================
// EDGE CASE: Undirected Edge Removal - Fixed in F# Version!
// ============================================================================

[<Fact>]
let ``undirected edge removal is symmetric - F# version is fixed!`` () =
    // This test PASSES in F# (would fail in Gleam v3.x)
    // The F# version correctly removes BOTH directions

    let graph =
        empty Undirected
        |> addNode 0 0
        |> addNode 1 1
        |> addEdge 0 1 10

    // add_edge created BOTH directions
    Assert.Equal(1, successors 0 graph |> List.length)
    Assert.Equal(1, successors 1 graph |> List.length)

    // Remove edge - F# version removes BOTH directions (fixed!)
    let removed = removeEdge 0 1 graph

    // Verify BOTH directions removed
    Assert.Equal<(NodeId * int) list>([], successors 0 removed)
    Assert.Equal<(NodeId * int) list>([], successors 1 removed)
    Assert.Equal(0, edgeCount removed)

// ============================================================================
// EDGE CASE: Filter All Nodes
// ============================================================================

[<Fact>]
let ``filtering all nodes produces empty graph`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addEdge 0 1 10

    // Filter out all nodes
    let emptyFiltered = filterNodes (fun _ -> false) graph

    Assert.Equal(0, order emptyFiltered)
    Assert.Equal(0, edgeCount emptyFiltered)

// ============================================================================
// EDGE CASE: Transpose on Graph with Self-Loop
// ============================================================================

[<Fact>]
let ``transpose preserves self loop`` () =
    let graph = empty Directed |> addNode 0 0 |> addEdge 0 0 10

    let transposed = transpose graph

    // Self-loop should remain a self-loop
    let succs = successors 0 transposed
    Assert.Equal(1, List.length succs)
    Assert.True(succs |> List.exists (fun (id, _) -> id = 0))

// ============================================================================
// EDGE CASE: Isolated Nodes
// ============================================================================

[<Fact>]
let ``isolated node has no edges`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 10

    // Node 2 is isolated
    Assert.Equal(3, order graph)
    Assert.Equal(1, edgeCount graph)

    let succs = successors 2 graph
    let preds = predecessors 2 graph

    Assert.Equal(0, List.length succs)
    Assert.Equal(0, List.length preds)
