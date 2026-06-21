/// Comprehensive tests for MultiGraph operations.
module Yog.FSharp.Tests.MultiGraphTests

open Xunit
open Yog.Model
open Yog.Multi

// =============================================================================
// MODEL TESTS
// =============================================================================

module ModelTests =
    [<Fact>]
    let ``empty creates empty multigraph`` () =
        let g = Model.empty Directed
        Assert.Equal(0, g.Nodes.Count)
        Assert.Equal(0, g.Edges.Count)
        Assert.Equal(0, g.NextEdgeId)
        Assert.Equal(Directed, g.Kind)

    [<Fact>]
    let ``directed and undirected helpers`` () =
        let g1 = Model.directed<string, int>()
        Assert.Equal(Directed, g1.Kind)
        let g2 = Model.undirected<string, int>()
        Assert.Equal(Undirected, g2.Kind)

    [<Fact>]
    let ``addNode adds a node`` () =
        let g = Model.empty Directed |> Model.addNode 0 "A"
        Assert.Equal(1, g.Nodes.Count)
        Assert.True(Map.containsKey 0 g.Nodes)
        Assert.Equal("A", g.Nodes.[0])

    [<Fact>]
    let ``addNode replaces existing node data`` () =
        let g = Model.empty Directed |> Model.addNode 0 "A" |> Model.addNode 0 "B"
        Assert.Equal(1, g.Nodes.Count)
        Assert.Equal("B", g.Nodes.[0])

    [<Fact>]
    let ``order and allNodes query`` () =
        let g = Model.empty Directed |> Model.addNode 0 "A" |> Model.addNode 1 "B"
        Assert.Equal(2, Model.order g)
        Assert.Equal<int list>([ 0; 1 ], Model.allNodes g |> List.sort)

    [<Fact>]
    let ``addEdge creates edge and returns EdgeId`` () =
        let g, eid =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g

        Assert.Equal(0, eid)
        Assert.Equal(1, g.Edges.Count)
        Assert.True(Map.containsKey 0 g.Edges)
        let (src, dst, weight) = g.Edges.[0]
        Assert.Equal(0, src)
        Assert.Equal(1, dst)
        Assert.Equal(10, weight)

    [<Fact>]
    let ``addEdge increments EdgeId`` () =
        let g1, eid1 =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g

        let g2, eid2 = Model.addEdge 0 1 20 g1
        Assert.Equal(0, eid1)
        Assert.Equal(1, eid2)
        Assert.Equal(2, g2.Edges.Count)

    [<Fact>]
    let ``addEdge creates parallel edges`` () =
        let g1, _ =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g

        let g2, _ = Model.addEdge 0 1 20 g1
        Assert.Equal(2, g2.Edges.Count)
        let weights = g2.Edges |> Map.toList |> List.map (fun (_, (_, _, w)) -> w) |> Set.ofList
        Assert.Equal<Set<int>>(Set.ofList [ 10; 20 ], weights)

    [<Fact>]
    let ``addEdge updates OutEdgeIds and InEdgeIds`` () =
        let g, eid =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g

        Assert.True(Map.containsKey 0 g.OutEdgeIds)
        Assert.Contains(eid, g.OutEdgeIds.[0])
        Assert.True(Map.containsKey 1 g.InEdgeIds)
        Assert.Contains(eid, g.InEdgeIds.[1])

    [<Fact>]
    let ``addEdge undirected creates both directions`` () =
        let g, eid =
            Model.empty Undirected
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        Assert.Contains(eid, g.OutEdgeIds.[0])
        Assert.Contains(eid, g.OutEdgeIds.[1])
        Assert.Contains(eid, g.InEdgeIds.[0])
        Assert.Contains(eid, g.InEdgeIds.[1])

    [<Fact>]
    let ``removeEdge removes edge from indexes`` () =
        let g1, eid =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g

        let g2 = Model.removeEdge eid g1
        Assert.Equal(0, g2.Edges.Count)
        Assert.False(Model.hasEdge g2 eid)
        Assert.Empty(g2.OutEdgeIds.[0])

    [<Fact>]
    let ``removeNode deletes node and connected edges`` () =
        let g1, eid1 =
            Model.empty Undirected
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        let g2 = Model.removeNode 0 g1
        Assert.Equal(1, Model.order g2)
        Assert.Equal(0, Model.size g2)

    [<Fact>]
    let ``edgesBetween and edgeCount query`` () =
        let g1, _ =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        let g2, _ = Model.addEdge 0 1 20 g1
        Assert.Equal(2, Model.edgeCount g2 0 1)
        let edges = Model.edgesBetween g2 0 1 |> List.map snd
        Assert.Equal<int list>([ 10; 20 ], edges)

    [<Fact>]
    let ``successors and predecessors query`` () =
        let g1, _ =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        let succs = Model.successors 0 g1
        Assert.Equal(1, succs.Length)
        let preds = Model.predecessors 1 g1
        Assert.Equal(1, preds.Length)

    [<Fact>]
    let ``outDegree, inDegree, and degree query`` () =
        let g1, _ =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        Assert.Equal(1, Model.outDegree 0 g1)
        Assert.Equal(1, Model.inDegree 1 g1)
        Assert.Equal(1, Model.degree 0 g1)

// =============================================================================
// TO SIMPLE GRAPH TESTS
// =============================================================================

module ToSimpleGraphTests =
    [<Fact>]
    let ``toSimpleGraph combines parallel edges`` () =
        let g1, _ =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g

        let g2, _ = Model.addEdge 0 1 20 g1
        let simple = Model.toSimpleGraph (+) g2
        Assert.Equal(1, edgeCount simple)
        let succs = successors 0 simple
        Assert.Equal(1, succs.Length)
        Assert.Equal(1, fst succs.[0])
        Assert.Equal(30, snd succs.[0])

    [<Fact>]
    let ``toSimpleGraphDefault keeps first edge`` () =
        let g1, _ =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        let g2, _ = Model.addEdge 0 1 20 g1
        let simple = Model.toSimpleGraphDefault g2
        let succs = successors 0 simple
        Assert.Equal(10, snd succs.[0])

    [<Fact>]
    let ``toSimpleGraphMaxEdges keeps maximum`` () =
        let g1, _ =
            Model.empty Directed
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        let g2, _ = Model.addEdge 0 1 20 g1
        let simple = Model.toSimpleGraphMaxEdges compare g2
        let succs = successors 0 simple
        Assert.Equal(20, snd succs.[0])

// =============================================================================
// TRAVERSAL TESTS
// =============================================================================

module TraversalTests =
    [<Fact>]
    let ``bfs on single node`` () =
        let g = Model.directed<string, int>() |> Model.addNode 0 "A"
        Assert.Equal<NodeId list>([ 0 ], Traversal.bfs 0 g)

    [<Fact>]
    let ``bfs traverses all reachable nodes`` () =
        let g =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> Model.addNode 3 "D"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 0 2 2 g)
            |> fun g -> fst (Model.addEdge 1 3 3 g)
        let result = Traversal.bfs 0 g
        Assert.Equal(4, result.Length)
        Assert.Contains(0, result)
        Assert.Contains(1, result)
        Assert.Contains(2, result)
        Assert.Contains(3, result)

    [<Fact>]
    let ``bfs with parallel edges`` () =
        let g =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 0 1 2 g)
        let result = Traversal.bfs 0 g
        Assert.Equal<NodeId list>([ 0; 1 ], result)

    [<Fact>]
    let ``dfs on single node`` () =
        let g = Model.directed<string, int>() |> Model.addNode 0 "A"
        Assert.Equal<NodeId list>([ 0 ], Traversal.dfs 0 g)

    [<Fact>]
    let ``dfs returns pre-order traversal`` () =
        let g =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> Model.addNode 3 "D"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 0 2 2 g)
            |> fun g -> fst (Model.addEdge 1 3 3 g)
        let result = Traversal.dfs 0 g
        Assert.Equal(0, List.head result)
        Assert.Equal(4, result.Length)
        Assert.Contains(0, result)
        Assert.Contains(1, result)
        Assert.Contains(2, result)
        Assert.Contains(3, result)

    [<Fact>]
    let ``foldWalk accumulates with Continue`` () =
        let g =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 0 2 2 g)
        let result = Traversal.foldWalk 0 [] (fun acc node _meta -> Continue, node :: acc) g
        Assert.Equal(3, result.Length)
        Assert.Contains(0, result)
        Assert.Contains(1, result)
        Assert.Contains(2, result)

    [<Fact>]
    let ``foldWalk stops with Halt`` () =
        let g =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 1 2 2 g)
        let result = Traversal.foldWalk 0 [] (fun acc node _meta -> if node = 1 then Halt, acc else Continue, node :: acc) g
        Assert.DoesNotContain(2, result)

    [<Fact>]
    let ``foldWalk skips successors with Stop`` () =
        let g =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> Model.addNode 3 "D"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 0 2 2 g)
            |> fun g -> fst (Model.addEdge 2 3 3 g)
        let result = Traversal.foldWalk 0 [] (fun acc node _meta -> if node = 2 then Stop, node :: acc else Continue, node :: acc) g
        Assert.Contains(0, result)
        Assert.Contains(1, result)
        Assert.Contains(2, result)
        Assert.DoesNotContain(3, result)

    [<Fact>]
    let ``foldWalk provides depth and parent metadata`` () =
        let g, e1 =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        let g, e2 = Model.addEdge 1 2 20 g
        let g = Model.addNode 2 "C" g
        
        let depths, parents =
            Traversal.foldWalk 0 (Map.empty, Map.empty) (fun (dAcc, pAcc) node meta ->
                let nextD = Map.add node meta.Depth dAcc
                let nextP =
                    match meta.Parent with
                    | None -> pAcc
                    | Some (p, e) -> Map.add node (p, e) pAcc
                Continue, (nextD, nextP)
            ) g

        Assert.Equal(0, depths.[0])
        Assert.Equal(1, depths.[1])
        Assert.Equal(2, depths.[2])
        Assert.Equal((0, e1), parents.[1])
        Assert.Equal((1, e2), parents.[2])

    [<Fact>]
    let ``foldWalk traverses all parallel edges`` () =
        let g, e1 =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> Model.addEdge 0 1 10 g
        let g, e2 = Model.addEdge 0 1 20 g

        let edgesUsed =
            Traversal.foldWalk 0 [] (fun acc _node meta ->
                match meta.Parent with
                | None -> Continue, acc
                | Some (_, e) -> Continue, e :: acc
            ) g

        Assert.Equal(2, edgesUsed.Length)
        Assert.Contains(e1, edgesUsed)
        Assert.Contains(e2, edgesUsed)

// =============================================================================
// EULERIAN PATH & CIRCUIT TESTS
// =============================================================================

module EulerianTests =
    [<Fact>]
    let ``empty graph checks`` () =
        let g = Model.directed<string, int>()
        Assert.False(Eulerian.hasEulerianCircuit g)
        Assert.False(Eulerian.hasEulerianPath g)
        Assert.True((Eulerian.findEulerianCircuit g).IsNone)
        Assert.True((Eulerian.findEulerianPath g).IsNone)

    [<Fact>]
    let ``directed single edge checks`` () =
        let g =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
        Assert.False(Eulerian.hasEulerianCircuit g)
        Assert.True(Eulerian.hasEulerianPath g)
        Assert.True((Eulerian.findEulerianCircuit g).IsNone)
        
        let path = Eulerian.findEulerianPath g
        Assert.True(path.IsSome)
        Assert.Equal(1, path.Value.Length)

    [<Fact>]
    let ``directed cycle checks`` () =
        let g =
            Model.directed<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 1 2 2 g)
            |> fun g -> fst (Model.addEdge 2 0 3 g)
        Assert.True(Eulerian.hasEulerianCircuit g)
        Assert.True(Eulerian.hasEulerianPath g)
        
        let circuit = Eulerian.findEulerianCircuit g
        Assert.True(circuit.IsSome)
        Assert.Equal(3, circuit.Value.Length)

    [<Fact>]
    let ``undirected triangle checks`` () =
        let g =
            Model.undirected<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 1 2 2 g)
            |> fun g -> fst (Model.addEdge 2 0 3 g)
        Assert.True(Eulerian.hasEulerianCircuit g)
        
        let circuit = Eulerian.findEulerianCircuit g
        Assert.True(circuit.IsSome)
        Assert.Equal(3, circuit.Value.Length)

    [<Fact>]
    let ``undirected path checks`` () =
        let g =
            Model.undirected<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 1 2 2 g)
        Assert.False(Eulerian.hasEulerianCircuit g)
        Assert.True(Eulerian.hasEulerianPath g)
        
        let path = Eulerian.findEulerianPath g
        Assert.True(path.IsSome)
        Assert.Equal(2, path.Value.Length)

    [<Fact>]
    let ``undirected parallel edges checks`` () =
        let g =
            Model.undirected<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 0 1 2 g)
        Assert.True(Eulerian.hasEulerianCircuit g)
        
        let circuit = Eulerian.findEulerianCircuit g
        Assert.True(circuit.IsSome)
        Assert.Equal(2, circuit.Value.Length)

    [<Fact>]
    let ``undirected triangle plus isolated node has circuit`` () =
        let g =
            Model.undirected<string, int>()
            |> Model.addNode 0 "A"
            |> Model.addNode 1 "B"
            |> Model.addNode 2 "C"
            |> Model.addNode 9 "Z"
            |> fun g -> fst (Model.addEdge 0 1 1 g)
            |> fun g -> fst (Model.addEdge 1 2 2 g)
            |> fun g -> fst (Model.addEdge 2 0 3 g)
        Assert.True(Eulerian.hasEulerianCircuit g)
        
        let circuit = Eulerian.findEulerianCircuit g
        Assert.True(circuit.IsSome)
        Assert.Equal(3, circuit.Value.Length)

    [<Fact>]
    let ``self-loop checks`` () =
        let g =
            Model.undirected<string, int>()
            |> Model.addNode 0 "U"
            |> Model.addNode 1 "V"
            |> fun g -> fst (Model.addEdge 0 0 1 g) // self loop at 0
            |> fun g -> fst (Model.addEdge 0 1 2 g)
        Assert.False(Eulerian.hasEulerianCircuit g)
        Assert.True(Eulerian.hasEulerianPath g)
        
        let path = Eulerian.findEulerianPath g
        Assert.True(path.IsSome)
        Assert.Equal(2, path.Value.Length)

