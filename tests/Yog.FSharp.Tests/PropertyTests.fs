module Yog.FSharp.Tests.PropertyTests


open Xunit
open Yog.Model
open Yog.Transform
open Yog.Traversal

// ============================================================================
// PROPERTY 1: Transpose is Involutive
// ============================================================================

[<Fact>]
let ``transpose is involutive`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 10
        |> addEdge 1 2 20

    let once = transpose graph
    let twice = transpose once

    Assert.Equal<Graph<int, int>>(graph, twice)

// ============================================================================
// PROPERTY 2: Edge Count Consistency
// ============================================================================

[<Fact>]
let ``edge count matches actual edges for directed`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 10
        |> addEdge 1 2 20

    let actualCount =
        allNodes graph |> List.sumBy (fun id -> successors id graph |> List.length)

    Assert.Equal(2, edgeCount graph)
    Assert.Equal(2, actualCount)

[<Fact>]
let ``edge count matches actual edges for undirected`` () =
    let graph = empty Undirected |> addNode 0 0 |> addNode 1 1 |> addEdge 0 1 10

    // Each undirected edge counted once, but appears in successor lists twice
    Assert.Equal(1, edgeCount graph)

// ============================================================================
// PROPERTY 3: Undirected Graph Symmetry
// ============================================================================

[<Fact>]
let ``undirected graphs have symmetric edges`` () =
    let graph =
        empty Undirected
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 10
        |> addEdge 1 2 20

    // Every edge should appear in both directions
    Assert.True(successors 0 graph |> List.exists (fun (id, w) -> id = 1 && w = 10))

    Assert.True(successors 1 graph |> List.exists (fun (id, w) -> id = 0 && w = 10))

    Assert.True(successors 1 graph |> List.exists (fun (id, w) -> id = 2 && w = 20))

    Assert.True(successors 2 graph |> List.exists (fun (id, w) -> id = 1 && w = 20))

// ============================================================================
// PROPERTY 4: Neighbors == Successors for Undirected
// ============================================================================

[<Fact>]
let ``neighbors equals successors for undirected graphs`` () =
    let graph = empty Undirected |> addNode 0 0 |> addNode 1 1 |> addEdge 0 1 10

    let nbrs = neighbors 0 graph |> List.sort
    let succs = successors 0 graph |> List.sort

    Assert.Equal<(NodeId * int) list>(nbrs, succs)

// ============================================================================
// PROPERTY 5: mapNodes Preserves Structure
// ============================================================================

[<Fact>]
let ``mapNodes preserves graph structure`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 10
        |> addEdge 1 2 20

    let mapped = mapNodes (fun data -> data + 1) graph

    // Same order and edge count
    Assert.Equal(order graph, order mapped)
    Assert.Equal(edgeCount graph, edgeCount mapped)

    // All edges preserved
    Assert.Equal<(NodeId * int) list>([ 1, 10 ], successors 0 mapped)
    Assert.Equal<(NodeId * int) list>([ 2, 20 ], successors 1 mapped)

// ============================================================================
// PROPERTY 6: mapEdges Preserves Structure
// ============================================================================

[<Fact>]
let ``mapEdges preserves graph structure`` () =
    let graph = empty Directed |> addNode 0 0 |> addNode 1 1 |> addEdge 0 1 10

    let mapped = mapEdges (fun weight -> weight * 2) graph

    // Same order and edge count
    Assert.Equal(order graph, order mapped)
    Assert.Equal(edgeCount graph, edgeCount mapped)

    // Edge destinations preserved, weights changed
    Assert.Equal<(NodeId * int) list>([ 1, 20 ], successors 0 mapped)

// ============================================================================
// PROPERTY 6.5: Cyclicity checks
// ============================================================================

[<Fact>]
let ``isCyclic detects cycles correctly`` () =
    // Acyclic directed
    let g1 = empty Directed |> addNode 0 0 |> addNode 1 1 |> addEdge 0 1 10

    Assert.False(Yog.Properties.Cyclicity.isCyclic g1)

    // Cyclic directed
    let g2 =
        empty Directed |> addNode 0 0 |> addNode 1 1 |> addEdge 0 1 10 |> addEdge 1 0 10

    Assert.True(Yog.Properties.Cyclicity.isCyclic g2)

    // Acyclic undirected (tree)
    let g3 =
        empty Undirected
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 10
        |> addEdge 1 2 20

    Assert.False(Yog.Properties.Cyclicity.isCyclic g3)

    // Cyclic undirected
    let g4 =
        empty Undirected
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 10
        |> addEdge 1 2 20
        |> addEdge 2 0 30

    Assert.True(Yog.Properties.Cyclicity.isCyclic g4)

// ============================================================================
// PROPERTY 7: filterNodes Removes Incident Edges
// ============================================================================

[<Fact>]
let ``filterNodes removes incident edges`` () =
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addNode 3 3
        |> addEdge 0 1 10
        |> addEdge 1 2 20
        |> addEdge 2 3 30

    // Keep only even-valued nodes (0 and 2)
    let filtered = filterNodes (fun data -> data % 2 = 0) graph

    // Should have only nodes 0 and 2
    Assert.Equal(2, order filtered)
    Assert.True(Map.containsKey 0 filtered.Nodes)
    Assert.True(Map.containsKey 2 filtered.Nodes)

    // Edge from 0 to 1 should be gone (1 removed)
    // Edge from 1 to 2 should be gone (1 removed)
    // Edge from 2 to 3 should be gone (3 removed)
    Assert.Equal(0, edgeCount filtered)

// ============================================================================
// PROPERTY 8: toUndirected Creates Symmetry
// ============================================================================

[<Fact>]
let ``toUndirected creates symmetric edges`` () =
    let graph = empty Directed |> addNode 0 0 |> addNode 1 1 |> addEdge 0 1 10

    let undirected = toUndirected max graph

    Assert.Equal(Undirected, undirected.Kind)

    // Both directions should exist
    Assert.True(successors 0 undirected |> List.exists (fun (id, w) -> id = 1 && w = 10))

    Assert.True(successors 1 undirected |> List.exists (fun (id, w) -> id = 0 && w = 10))

// ============================================================================
// PROPERTY 9: Add/Remove Edge (Directed)
// ============================================================================

[<Fact>]
let ``add and remove edge directed`` () =
    let graph = empty Directed |> addNode 0 0 |> addNode 1 1 |> addEdge 0 1 10

    Assert.Equal(1, edgeCount graph)
    Assert.Equal<(NodeId * int) list>([ 1, 10 ], successors 0 graph)

    let removed = removeEdge 0 1 graph

    Assert.Equal(0, edgeCount removed)
    Assert.Equal<(NodeId * int) list>([], successors 0 removed)

// ============================================================================
// PROPERTY 10: Add/Remove Edge (Undirected) - F# Version is Fixed!
// ============================================================================

[<Fact>]
let ``add and remove edge undirected - symmetric behavior - FIXED`` () =
    let graph = empty Undirected |> addNode 0 0 |> addNode 1 1 |> addEdge 0 1 10 // Creates BOTH directions

    // Verify both directions exist
    Assert.True(successors 0 graph |> List.exists (fun (id, _) -> id = 1))

    Assert.True(successors 1 graph |> List.exists (fun (id, _) -> id = 0))

    let removed = removeEdge 0 1 graph // F# version removes BOTH directions!

    // Verify BOTH directions are gone (F# version is fixed!)
    Assert.Equal<(NodeId * int) list>([], successors 0 removed)
    Assert.Equal<(NodeId * int) list>([], successors 1 removed)
    Assert.Equal(0, edgeCount removed)

// ============================================================================
// PROPERTY 11: BFS No Duplicates
// ============================================================================

[<Fact>]
let ``BFS produces no duplicates`` () =
    // Linear chain: 0 -> 1 -> 2 -> 3
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addNode 3 3
        |> addEdge 0 1 1
        |> addEdge 1 2 1
        |> addEdge 2 3 1

    let visited = walk 0 BreadthFirst graph
    let uniqueCount = visited |> List.distinct |> List.length

    Assert.Equal(List.length visited, uniqueCount)

// ============================================================================
// PROPERTY 12: DFS No Duplicates on Cycles
// ============================================================================

[<Fact>]
let ``DFS produces no duplicates even with cycles`` () =
    // Cycle: 0 -> 1 -> 2 -> 0
    let graph =
        empty Directed
        |> addNode 0 0
        |> addNode 1 1
        |> addNode 2 2
        |> addEdge 0 1 1
        |> addEdge 1 2 1
        |> addEdge 2 0 1 // Creates cycle

    let visited = walk 0 DepthFirst graph
    let uniqueCount = visited |> List.distinct |> List.length

    Assert.Equal(List.length visited, uniqueCount)
