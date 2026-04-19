module Yog.FSharp.Tests.TransformTests


open Xunit
open Yog.Model
open Yog.Transform

// ============================================================================
// TRANSPOSE
// ============================================================================

[<Fact>]
let ``transpose empty graph`` () =
    let graph = empty Directed
    let transposed = transpose graph

    Assert.Equal(0, edgeCount transposed)
    Assert.Equal<Map<NodeId, string>>(graph.Nodes, transposed.Nodes)

[<Fact>]
let ``transpose single edge`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 10

    let transposed = transpose graph

    // Original: 1 -> 2
    // Transposed: 2 -> 1
    Assert.Empty(successors 1 transposed)
    Assert.Equal<(NodeId * int) list>([ 1, 10 ], successors 2 transposed)
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], predecessors 1 transposed)

[<Fact>]
let ``transpose multiple edges`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 2 3 20
        |> addEdge 1 3 30

    let transposed = transpose graph

    // Original: 1->2, 2->3, 1->3
    // Transposed: 2->1, 3->2, 3->1
    Assert.Empty(successors 1 transposed)
    Assert.Equal<(NodeId * int) list>([ 1, 10 ], successors 2 transposed)
    Assert.Equal(2, successors 3 transposed |> List.length)

[<Fact>]
let ``transpose cycle`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 1
        |> addEdge 2 3 2
        |> addEdge 3 1 3

    let transposed = transpose graph

    // Cycle reverses: 1->2->3->1 becomes 1->3->2->1
    Assert.Equal<(NodeId * int) list>([ 3, 3 ], successors 1 transposed)
    Assert.Equal<(NodeId * int) list>([ 1, 1 ], successors 2 transposed)
    Assert.Equal<(NodeId * int) list>([ 2, 2 ], successors 3 transposed)

[<Fact>]
let ``transpose is involutive (transpose twice is identity)`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 10

    let doubleTransposed = graph |> transpose |> transpose

    Assert.Equal<Graph<string, int>>(graph, doubleTransposed)

[<Fact>]
let ``transpose preserves nodes`` () =
    let graph =
        empty Directed |> addNode 1 "Node A" |> addNode 2 "Node B" |> addEdge 1 2 10

    let transposed = transpose graph

    Assert.Equal<Map<NodeId, string>>(graph.Nodes, transposed.Nodes)

// ============================================================================
// MAP NODES
// ============================================================================

[<Fact>]
let ``mapNodes empty graph`` () =
    let graph = empty Directed
    let mapped = mapNodes (fun (s: string) -> s.ToUpper()) graph

    Assert.Equal(0, order mapped)

[<Fact>]
let ``mapNodes transforms all nodes`` () =
    let graph =
        empty Directed |> addNode 1 "alice" |> addNode 2 "bob" |> addNode 3 "carol"

    let mapped = mapNodes (fun (s: string) -> s.ToUpper()) graph

    let nodes: Map<NodeId, string> = mapped.Nodes
    Assert.Equal("ALICE", nodes.[1])
    Assert.Equal("BOB", nodes.[2])
    Assert.Equal("CAROL", nodes.[3])

[<Fact>]
let ``mapNodes preserves structure`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 2 3 20

    let mapped = mapNodes (fun data -> data + "_") graph

    // Same order and edge count
    Assert.Equal(order graph, order mapped)
    Assert.Equal(edgeCount graph, edgeCount mapped)

    // Edges preserved (nodes 1 and 2, edge to 2 with weight 10)
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 mapped)
    Assert.Equal<(NodeId * int) list>([ 3, 20 ], successors 2 mapped)

// ============================================================================
// MAP EDGES
// ============================================================================

[<Fact>]
let ``mapEdges empty graph`` () =
    let graph = empty Directed
    let mapped = mapEdges (fun w -> w * 2) graph

    Assert.Equal(0, edgeCount mapped)

[<Fact>]
let ``mapEdges transforms all weights`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 10

    let mapped = mapEdges (fun w -> w * 2) graph

    Assert.Equal<(NodeId * int) list>([ 2, 20 ], successors 1 mapped)

[<Fact>]
let ``mapEdges preserves structure`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 1 3 20

    let mapped = mapEdges (fun w -> w + 1) graph

    Assert.Equal(order graph, order mapped)
    Assert.Equal(edgeCount graph, edgeCount mapped)

    let succs = successors 1 mapped |> List.sortBy fst
    Assert.Equal<(NodeId * int) list>([ 2, 11; 3, 21 ], succs)

// ============================================================================
// FILTER NODES
// ============================================================================

[<Fact>]
let ``filterNodes keeps matching nodes`` () =
    let graph =
        empty Directed |> addNode 1 "apple" |> addNode 2 "banana" |> addNode 3 "apricot"

    let filtered = filterNodes (fun (s: string) -> s.StartsWith("a")) graph

    Assert.Equal(2, order filtered)
    Assert.True(Map.containsKey 1 filtered.Nodes) // "apple"
    Assert.True(Map.containsKey 3 filtered.Nodes) // "apricot"
    Assert.False(Map.containsKey 2 filtered.Nodes) // "banana"

[<Fact>]
let ``filterNodes removes incident edges`` () =
    let graph =
        empty Directed
        |> addNode 1 0
        |> addNode 2 1
        |> addNode 3 2
        |> addNode 4 3
        |> addEdge 1 2 10
        |> addEdge 2 3 20
        |> addEdge 3 4 30

    // Keep only even-valued nodes
    let filtered = filterNodes (fun data -> data % 2 = 0) graph

    // Should have nodes 1 and 3 (IDs 1 and 3)
    Assert.Equal(2, order filtered)

    // All edges should be gone (they all involve odd nodes)
    Assert.Equal(0, edgeCount filtered)

[<Fact>]
let ``filterNodes preserves edges between kept nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 2 3 20

    // Keep only A and B
    let filtered = filterNodes (fun s -> s = "A" || s = "B") graph

    Assert.Equal(2, order filtered)
    Assert.Equal(1, edgeCount filtered)
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 filtered)

// ============================================================================
// FILTER EDGES
// ============================================================================

[<Fact>]
let ``filterEdges keeps matching edges`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 5
        |> addEdge 1 3 15
        |> addEdge 2 3 3

    // Keep only edges with weight >= 10
    let filtered = filterEdges (fun _ _ w -> w >= 10) graph

    Assert.Equal(3, order filtered) // All nodes preserved
    Assert.Equal(1, edgeCount filtered)
    Assert.Equal<(NodeId * int) list>([ 3, 15 ], successors 1 filtered)

[<Fact>]
let ``filterEdges removes self-loops`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 1 10
        |> addEdge 1 2 20
        |> addEdge 2 2 30

    // Remove self-loops
    let filtered = filterEdges (fun src dst _ -> src <> dst) graph

    Assert.Equal(1, edgeCount filtered)
    Assert.Equal<(NodeId * int) list>([ 2, 20 ], successors 1 filtered)

[<Fact>]
let ``filterEdges by source node`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 1 3 20
        |> addEdge 2 3 30

    // Keep only edges from node 1
    let filtered = filterEdges (fun src _ _ -> src = 1) graph

    Assert.Equal(2, edgeCount filtered)
    Assert.Equal(2, successors 1 filtered |> List.length)
    Assert.Empty(successors 2 filtered)

// ============================================================================
// COMPLEMENT
// ============================================================================

[<Fact>]
let ``complement of empty graph is empty`` () =
    let graph = empty Directed
    let comp = complement 1 graph

    Assert.Equal(0, edgeCount comp)

[<Fact>]
let ``complement creates missing edges`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 1

    let comp = complement 0 graph // default weight = 0

    // Original: 1-2 connected, 1-3 and 2-3 not
    // Complement: 1-3 and 2-3 connected, 1-2 not
    Assert.Equal(2, edgeCount comp)

    Assert.True(successors 1 comp |> List.exists (fun (id, _) -> id = 3))

    Assert.True(successors 2 comp |> List.exists (fun (id, _) -> id = 3))

[<Fact>]
let ``complement has no self-loops`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B"

    let comp = complement 1 graph

    // Should not have edge from node to itself
    Assert.True(successors 1 comp |> List.forall (fun (id, _) -> id <> 1))

    Assert.True(successors 2 comp |> List.forall (fun (id, _) -> id <> 2))

[<Fact>]
let ``complement of complete graph is empty`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 1
        |> addEdge 1 3 1
        |> addEdge 2 3 1

    let comp = complement 0 graph

    Assert.Equal(0, edgeCount comp)

// ============================================================================
// MERGE
// ============================================================================

[<Fact>]
let ``merge combines nodes from both graphs`` () =
    let baseGraph = empty Directed |> addNode 1 "BaseA" |> addNode 2 "BaseB"

    let other = empty Directed |> addNode 2 "OtherB" |> addNode 3 "OtherC"

    let merged = merge baseGraph other

    Assert.Equal(3, order merged)
    // Node 2 should have value from 'other' (it overwrites)
    let mergedNodes: Map<NodeId, string> = merged.Nodes
    Assert.Equal("OtherB", mergedNodes.[2])

[<Fact>]
let ``merge combines edges from both graphs`` () =
    let baseGraph =
        empty Directed |> addNode 1 "" |> addNode 2 "" |> addNode 3 "" |> addEdge 1 2 10

    let other =
        empty Directed
        |> addNode 2 ""
        |> addNode 3 ""
        |> addNode 4 ""
        |> addEdge 2 3 20
        |> addEdge 3 4 30

    let merged = merge baseGraph other

    // Should have all edges combined
    Assert.Equal(3, edgeCount merged)
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 merged)
    Assert.Equal<(NodeId * int) list>([ 3, 20 ], successors 2 merged)
    Assert.Equal<(NodeId * int) list>([ 4, 30 ], successors 3 merged)

[<Fact>]
let ``merge uses base graph kind`` () =
    let baseGraph = empty Directed
    let other = empty Undirected

    let merged = merge baseGraph other

    Assert.Equal(Directed, merged.Kind)

// ============================================================================
// SUBGRAPH
// ============================================================================

[<Fact>]
let ``subgraph keeps specified nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addNode 4 "D"

    let sub = subgraph [ 1; 3 ] graph

    Assert.Equal(2, order sub)
    Assert.True(Map.containsKey 1 sub.Nodes)
    Assert.True(Map.containsKey 3 sub.Nodes)

[<Fact>]
let ``subgraph keeps edges within subset`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addNode 4 "D"
        |> addEdge 1 2 10
        |> addEdge 2 3 20
        |> addEdge 3 4 30

    // Extract only nodes 2 and 3
    let sub = subgraph [ 2; 3 ] graph

    Assert.Equal(2, order sub)
    Assert.Equal(1, edgeCount sub)
    Assert.Equal<(NodeId * int) list>([ 3, 20 ], successors 2 sub)

[<Fact>]
let ``subgraph removes edges to outside nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 2 3 20

    let sub = subgraph [ 1 ] graph // Keep only node 1

    Assert.Equal(1, order sub)
    Assert.Equal(0, edgeCount sub)

// ============================================================================
// CONTRACT
// ============================================================================

[<Fact>]
let ``contract merges nodes`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 5
        |> addEdge 2 3 10

    let contracted = contract 1 2 (+) graph // Merge 2 into 1

    Assert.Equal(2, order contracted)
    Assert.False(Map.containsKey 2 contracted.Nodes)
    Assert.True(Map.containsKey 1 contracted.Nodes)
    Assert.True(Map.containsKey 3 contracted.Nodes)

[<Fact>]
let ``contract redirects edges`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 5
        |> addEdge 2 3 10

    let contracted = contract 1 2 (+) graph // Merge 2 into 1

    // Edge 2-3 (weight 10) should become 1-3
    Assert.Equal(1, edgeCount contracted)
    Assert.Equal<(NodeId * int) list>([ 3, 10 ], successors 1 contracted)

[<Fact>]
let ``contract combines parallel edges`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 3 5 // 1 is connected to 3
        |> addEdge 2 3 10 // 2 is also connected to 3

    let contracted = contract 1 2 (+) graph // Merge 2 into 1

    // Both edges should be combined: 5 + 10 = 15
    Assert.Equal(1, edgeCount contracted)
    Assert.Equal<(NodeId * int) list>([ 1, 15 ], successors 3 contracted)

[<Fact>]
let ``contract removes self-loops`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 5

    let contracted = contract 1 2 (+) graph // Merge 2 into 1

    // No self-loop should be created
    Assert.True(successors 1 contracted |> List.forall (fun (id, _) -> id <> 1))

// ============================================================================
// TO DIRECTED / TO UNDIRECTED
// ============================================================================

[<Fact>]
let ``toDirected changes kind`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 10

    let directed = toDirected graph

    Assert.Equal(Directed, directed.Kind)

[<Fact>]
let ``toDirected preserves edges`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 10

    let directed = toDirected graph

    // Still has both directions (since undirected stores bidirectionally)
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 directed)
    Assert.Equal<(NodeId * int) list>([ 1, 10 ], successors 2 directed)

[<Fact>]
let ``toUndirected makes edges symmetric`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 10

    let undirected = toUndirected max graph

    Assert.Equal(Undirected, undirected.Kind)
    // Both directions should exist now
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 undirected)
    Assert.Equal<(NodeId * int) list>([ 1, 10 ], successors 2 undirected)

[<Fact>]
let ``toUndirected resolves weight conflicts`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10
        |> addEdge 2 1 20

    // Use max to resolve conflict
    let undirected = toUndirected max graph

    // Both directions should have weight 20 (max of 10 and 20)
    Assert.Equal<(NodeId * int) list>([ 2, 20 ], successors 1 undirected)
    Assert.Equal<(NodeId * int) list>([ 1, 20 ], successors 2 undirected)
