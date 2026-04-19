module Yog.FSharp.Tests.ModelTests


open Xunit
open Yog.Model

// ============================================================================
// GRAPH CREATION
// ============================================================================

[<Fact>]
let ``empty creates directed graph`` () =
    let graph = empty Directed
    Assert.Equal(Directed, graph.Kind)
    Assert.Equal(0, graph.Nodes.Count)
    Assert.Equal(0, graph.OutEdges.Count)
    Assert.Equal(0, graph.InEdges.Count)

[<Fact>]
let ``empty creates undirected graph`` () =
    let graph = empty Undirected
    Assert.Equal(Undirected, graph.Kind)

// ============================================================================
// NODE OPERATIONS
// ============================================================================

[<Fact>]
let ``addNode adds single node`` () =
    let graph = empty Directed |> addNode 1 "A"

    Assert.Equal(1, order graph)
    Assert.True(Map.containsKey 1 graph.Nodes)
    Assert.Equal("A", graph.Nodes.[1])

[<Fact>]
let ``addNode adds multiple nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"

    Assert.Equal(3, order graph)
    Assert.True(Map.containsKey 1 graph.Nodes)
    Assert.True(Map.containsKey 2 graph.Nodes)
    Assert.True(Map.containsKey 3 graph.Nodes)

[<Fact>]
let ``addNode updates existing node`` () =
    let graph =
        empty Directed
        |> addNode 1 "Original"
        |> addNode 1 "Updated"

    Assert.Equal(1, order graph)
    Assert.Equal("Updated", graph.Nodes.[1])

[<Fact>]
let ``removeNode removes node and its edges`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 2 3 20

    let removed = removeNode 2 graph

    // Node 2 should be gone
    Assert.Equal(2, order removed)
    Assert.False(Map.containsKey 2 removed.Nodes)

    // Edges involving node 2 should be gone
    Assert.Equal(0, edgeCount removed)
    Assert.Empty(successors 1 removed)
    Assert.Empty(successors 3 removed)

[<Fact>]
let ``removeNode handles isolated node`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B"

    let removed = removeNode 1 graph

    Assert.Equal(1, order removed)
    Assert.False(Map.containsKey 1 removed.Nodes)
    Assert.True(Map.containsKey 2 removed.Nodes)

[<Fact>]
let ``removeNode handles nonexistent node`` () =
    let graph = empty Directed |> addNode 1 "A"

    // Should not throw
    let removed = removeNode 999 graph

    Assert.Equal(1, order removed)
    Assert.True(Map.containsKey 1 removed.Nodes)

[<Fact>]
let ``allNodes returns all node IDs`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"

    let nodes = allNodes graph |> Set.ofList

    Assert.Equal<Set<NodeId>>(Set.ofList [ 1; 2; 3 ], nodes)

[<Fact>]
let ``order returns correct node count`` () =
    let graph = empty Directed
    Assert.Equal(0, order graph)

    let graph2 = graph |> addNode 1 "A"
    Assert.Equal(1, order graph2)

[<Fact>]
let ``nodeCount equals order`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B"

    Assert.Equal(order graph, nodeCount graph)

// ============================================================================
// EDGE OPERATIONS - DIRECTED
// ============================================================================

[<Fact>]
let ``addEdge adds directed edge`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10

    Assert.Equal(1, edgeCount graph)
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 graph)
    Assert.Equal<(NodeId * int) list>([], successors 2 graph)
    Assert.Equal<(NodeId * int) list>([ 1, 10 ], predecessors 2 graph)

[<Fact>]
let ``addEdge throws ArgumentException if src or dst are missing`` () =
    let graph = empty Directed |> addNode 1 "A"
    
    // Missing dst
    let ex1 = Assert.Throws<System.ArgumentException>(fun () -> addEdge 1 2 10 graph |> ignore)
    Assert.Contains("Destination node 2 does not exist", ex1.Message)

    // Missing src
    let ex2 = Assert.Throws<System.ArgumentException>(fun () -> addEdge 2 1 10 graph |> ignore)
    Assert.Contains("Source node 2 does not exist", ex2.Message)

[<Fact>]
let ``addEdge updates existing edge weight`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10
        |> addEdge 1 2 20

    Assert.Equal(1, edgeCount graph)
    Assert.Equal<(NodeId * int) list>([ 2, 20 ], successors 1 graph)

[<Fact>]
let ``addEdge creates multiple outgoing edges`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 1 3 20

    Assert.Equal(2, edgeCount graph)
    let succs = successors 1 graph |> List.sortBy fst
    Assert.Equal<(NodeId * int) list>([ 2, 10; 3, 20 ], succs)

[<Fact>]
let ``removeEdge removes directed edge`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10

    let removed = removeEdge 1 2 graph

    Assert.Equal(0, edgeCount removed)
    Assert.Empty(successors 1 removed)
    Assert.Empty(predecessors 2 removed)

[<Fact>]
let ``removeEdge handles nonexistent edge`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B"

    // Should not throw
    let removed = removeEdge 1 2 graph

    Assert.Equal(0, edgeCount removed)

[<Fact>]
let ``removeEdge only removes specified direction`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10
        |> addEdge 2 1 20

    let removed = removeEdge 1 2 graph

    Assert.Equal(1, edgeCount removed)
    Assert.Empty(successors 1 removed)
    Assert.Equal<(NodeId * int) list>([ 1, 20 ], successors 2 removed)

[<Fact>]
let ``edgeCount returns correct edge count for directed`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 2 3 20

    Assert.Equal(2, edgeCount graph)

// ============================================================================
// EDGE OPERATIONS - UNDIRECTED
// ============================================================================

[<Fact>]
let ``addEdge creates symmetric edges for undirected`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10

    Assert.Equal(1, edgeCount graph)
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 graph)
    Assert.Equal<(NodeId * int) list>([ 1, 10 ], successors 2 graph)

[<Fact>]
let ``removeEdge removes both directions for undirected`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10

    let removed = removeEdge 1 2 graph

    Assert.Equal(0, edgeCount removed)
    Assert.Empty(successors 1 removed)
    Assert.Empty(successors 2 removed)

[<Fact>]
let ``edgeCount counts undirected edges once`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 2 3 20

    // Should count each undirected edge once, not twice
    Assert.Equal(2, edgeCount graph)

// ============================================================================
// addEdgeWithCombine
// ============================================================================

[<Fact>]
let ``addEdgeWithCombine combines weights with existing edge`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10
        |> addEdgeWithCombine 1 2 5 (+)

    Assert.Equal(1, edgeCount graph)
    Assert.Equal<(NodeId * int) list>([ 2, 15 ], successors 1 graph) // 10 + 5 = 15

[<Fact>]
let ``addEdgeWithCombine creates new edge if none exists`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdgeWithCombine 1 2 10 (+)

    Assert.Equal(1, edgeCount graph)
    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 graph)

[<Fact>]
let ``addEdgeWithCombine uses custom combine function`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10
        |> addEdgeWithCombine 1 2 5 max

    Assert.Equal<(NodeId * int) list>([ 2, 10 ], successors 1 graph) // max(10, 5) = 10

[<Fact>]
let ``addEdgeWithCombine works for undirected graphs`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10
        |> addEdgeWithCombine 1 2 5 (+)

    Assert.Equal(1, edgeCount graph)
    // Both directions should have combined weight
    Assert.Equal<(NodeId * int) list>([ 2, 15 ], successors 1 graph)
    Assert.Equal<(NodeId * int) list>([ 1, 15 ], successors 2 graph)

// ============================================================================
// addEdgeEnsured
// ============================================================================

[<Fact>]
let ``addEdgeEnsured creates missing nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "Existing"
        |> addEdgeEnsured 1 2 10 "Default" "Default"

    Assert.Equal(2, order graph)
    Assert.Equal("Existing", graph.Nodes.[1])
    Assert.Equal("Default", graph.Nodes.[2])
    Assert.Equal(1, edgeCount graph)

[<Fact>]
let ``addEdgeEnsured preserves existing node data`` () =
    let graph =
        empty Directed
        |> addNode 1 "Alice"
        |> addNode 2 "Bob"
        |> addEdgeEnsured 1 2 10 "Unknown" "Unknown"

    Assert.Equal("Alice", graph.Nodes.[1])
    Assert.Equal("Bob", graph.Nodes.[2])

[<Fact>]
let ``addEdgeEnsuredWith uses callback for missing nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "Existing"
        |> addEdgeEnsuredWith 1 2 10 (fun id -> sprintf "Generated_%d" id)

    Assert.Equal(2, order graph)
    Assert.Equal("Existing", graph.Nodes.[1])
    Assert.Equal("Generated_2", graph.Nodes.[2])
    Assert.Equal(1, edgeCount graph)

// ============================================================================
// SUCCESSORS / PREDECESSORS / NEIGHBORS
// ============================================================================

[<Fact>]
let ``successors returns empty for node with no outgoing edges`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 2 1 10 // Edge goes TO 1, not FROM 1

    Assert.Empty(successors 1 graph)

[<Fact>]
let ``predecessors returns empty for node with no incoming edges`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10

    Assert.Empty(predecessors 1 graph)

[<Fact>]
let ``predecessors returns correct predecessors`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 3 2 20

    let preds = predecessors 2 graph |> List.sortBy fst
    Assert.Equal<(NodeId * int) list>([ 1, 10; 3, 20 ], preds)

[<Fact>]
let ``successorIds returns only node IDs`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 1 3 20

    let ids = successorIds 1 graph |> List.sort
    Assert.Equal<NodeId list>([ 2; 3 ], ids)

[<Fact>]
let ``predecessorIds returns only node IDs`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 2 1 10
        |> addEdge 3 1 20

    let ids = predecessorIds 1 graph |> List.sort
    Assert.Equal<NodeId list>([ 2; 3 ], ids)

[<Fact>]
let ``neighbors equals successors for undirected`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10
        |> addEdge 1 3 20

    let nbrs = neighbors 1 graph |> List.sortBy fst
    let succs = successors 1 graph |> List.sortBy fst

    Assert.Equal<(NodeId * int) list>(nbrs, succs)

[<Fact>]
let ``neighbors combines incoming and outgoing for directed`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 10 // 1 -> 2
        |> addEdge 3 1 20 // 3 -> 1

    let nbrs = neighbors 1 graph |> List.sortBy fst

    // Should have both 2 (successor) and 3 (predecessor)
    Assert.Equal<(NodeId * int) list>([ 2, 10; 3, 20 ], nbrs)

[<Fact>]
let ``neighbors does not duplicate bidirectional edges`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10
        |> addEdge 2 1 20

    let nbrs = neighbors 1 graph

    // Should appear only once, even though there's an edge in each direction
    Assert.Equal(1, List.length nbrs)
