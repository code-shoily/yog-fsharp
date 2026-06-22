module Yog.FSharp.Tests.OperationTests

open Xunit
open Yog.Model
open Yog.Operation

// ============================================================================
// UNION
// ============================================================================

[<Fact>]
let ``union combines nodes and edges`` () =
    let g1 = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 1
    let g2 = empty Undirected |> addNode 2 "B" |> addNode 3 "C" |> addEdge 2 3 1
    let combined = union g1 g2

    Assert.Equal(3, order combined)
    Assert.Equal(2, edgeCount combined)
    Assert.True(hasEdge 1 2 combined)
    Assert.True(hasEdge 2 3 combined)

[<Fact>]
let ``union preserves kind of base graph`` () =
    let g1 = empty Directed |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let g2 = empty Undirected |> addNode 2 "" |> addNode 3 "" |> addEdge 2 3 1
    let combined = union g1 g2

    Assert.Equal(Directed, combined.Kind)

// ============================================================================
// INTERSECTION
// ============================================================================

[<Fact>]
let ``intersection keeps common nodes and edges`` () =
    let g1 =
        empty Undirected
        |> addNode 1 ""
        |> addNode 2 ""
        |> addNode 3 ""
        |> addEdge 1 2 1
        |> addEdge 2 3 1

    let g2 =
        empty Undirected
        |> addNode 1 ""
        |> addNode 2 ""
        |> addNode 3 ""
        |> addEdge 1 2 1

    let common = intersection g1 g2

    Assert.Equal(3, order common)
    Assert.Equal(1, edgeCount common)
    Assert.True(hasEdge 1 2 common)
    Assert.False(hasEdge 2 3 common)

[<Fact>]
let ``intersection of disjoint graphs is empty`` () =
    let g1 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let g2 = empty Undirected |> addNode 3 "" |> addNode 4 "" |> addEdge 3 4 1
    let common = intersection g1 g2

    Assert.Equal(0, order common)
    Assert.Equal(0, edgeCount common)

// ============================================================================
// DIFFERENCE
// ============================================================================

[<Fact>]
let ``difference removes shared nodes and edges`` () =
    let g1 =
        empty Undirected
        |> addNode 1 ""
        |> addNode 2 ""
        |> addNode 3 ""
        |> addEdge 1 2 1
        |> addEdge 2 3 1

    let g2 = empty Undirected |> addNode 3 ""
    let diff = difference g1 g2

    Assert.Equal(2, order diff)
    Assert.Equal(1, edgeCount diff)
    Assert.True(hasEdge 1 2 diff)
    Assert.False(hasNode 3 diff)

[<Fact>]
let ``difference removes edges present in second graph`` () =
    let g1 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let g2 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let diff = difference g1 g2

    Assert.Equal(0, order diff)
    Assert.Equal(0, edgeCount diff)

// ============================================================================
// SYMMETRIC DIFFERENCE
// ============================================================================

[<Fact>]
let ``symmetricDifference keeps unique nodes and edges`` () =
    let g1 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let g2 = empty Undirected |> addNode 2 "" |> addNode 3 "" |> addEdge 2 3 1
    let sym = symmetricDifference g1 g2

    Assert.Equal(2, order sym)
    Assert.True(hasNode 1 sym)
    Assert.False(hasNode 2 sym)
    Assert.True(hasNode 3 sym)

[<Fact>]
let ``symmetricDifference of identical graphs is empty`` () =
    let g1 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let g2 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let sym = symmetricDifference g1 g2

    Assert.Equal(0, order sym)

// ============================================================================
// DISJOINT UNION
// ============================================================================

[<Fact>]
let ``disjointUnion shifts second graph IDs`` () =
    let g1 = empty Directed |> addNode 0 "A"
    let g2 = empty Directed |> addNode 0 "B" |> addNode 1 "C" |> addEdge 0 1 1
    let combined = disjointUnion g1 g2

    Assert.Equal(3, order combined)
    Assert.Equal("A", combined.Nodes.[0])
    Assert.Equal("B", combined.Nodes.[1])
    Assert.Equal("C", combined.Nodes.[2])
    Assert.True(hasEdge 1 2 combined)

[<Fact>]
let ``disjointUnion of empty first graph keeps second graph IDs`` () =
    let g1 = empty Directed
    let g2 = empty Directed |> addNode 0 "A" |> addNode 1 "B" |> addEdge 0 1 1
    let combined = disjointUnion g1 g2

    Assert.Equal(2, order combined)
    Assert.True(hasNode 0 combined)
    Assert.True(hasNode 1 combined)

// ============================================================================
// CARTESIAN PRODUCT
// ============================================================================

[<Fact>]
let ``cartesianProduct of two edges makes a square`` () =
    let g1 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 10
    let g2 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 20
    let product = cartesianProduct 0 0 g1 g2

    Assert.Equal(4, order product)
    // 4 nodes, 4 edges for C2 x C2 (a square)
    Assert.Equal(4, edgeCount product)

[<Fact>]
let ``cartesianProduct node data is a pair`` () =
    let g1 = empty Undirected |> addNode 0 "A" |> addNode 1 "B"
    let g2 = empty Undirected |> addNode 0 "X" |> addNode 1 "Y"
    let product = cartesianProduct 0 0 g1 g2

    Assert.Equal(("A", "X"), product.Nodes.[0])
    Assert.Equal(("A", "Y"), product.Nodes.[1])
    Assert.Equal(("B", "X"), product.Nodes.[2])
    Assert.Equal(("B", "Y"), product.Nodes.[3])

// ============================================================================
// TENSOR PRODUCT
// ============================================================================

[<Fact>]
let ``tensorProduct of two edges has tensor edges`` () =
    let g1 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 10
    let g2 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 20
    let product = tensorProduct g1 g2

    Assert.Equal(4, order product)
    Assert.Equal(2, edgeCount product)

[<Fact>]
let ``tensorProduct edge weights are pairs`` () =
    let g1 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 10
    let g2 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 20
    let product = tensorProduct g1 g2

    Assert.True(
        product.OutEdges
        |> Map.values
        |> Seq.collect Map.values
        |> Seq.exists ((=) (10, 20))
    )

// ============================================================================
// STRONG PRODUCT
// ============================================================================

[<Fact>]
let ``strongProduct contains cartesian and tensor edges`` () =
    let g1 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 10
    let g2 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 20
    let product = strongProduct 0 0 g1 g2

    Assert.Equal(4, order product)
    // 4 cartesian edges + 2 tensor edges = 6 for undirected C2 x C2 strong product
    Assert.Equal(6, edgeCount product)

// ============================================================================
// LEXICOGRAPHIC PRODUCT
// ============================================================================

[<Fact>]
let ``lexicographicProduct of two edges has correct order`` () =
    let g1 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 10
    let g2 = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 20
    let product = lexicographicProduct 0 0 g1 g2

    Assert.Equal(4, order product)
    // horizontal edges: 2 (from g1 edge) * 2*2 = 8, vertical: 2 nodes in g1 * 1 edge in g2 = 2 => 10 undirected => 5 logical
    Assert.Equal(6, edgeCount product)

// ============================================================================
// COMPOSE
// ============================================================================

[<Fact>]
let ``compose behaves like union`` () =
    let g1 = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let g2 = empty Undirected |> addNode 2 "" |> addNode 3 "" |> addEdge 2 3 1
    let composed = compose g1 g2
    let merged = union g1 g2

    Assert.Equal<Graph<string, int>>(merged, composed)

// ============================================================================
// LINE GRAPH
// ============================================================================

[<Fact>]
let ``lineGraph of path has correct structure`` () =
    let path =
        empty Undirected
        |> addNode 0 ""
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 0 1 10
        |> addEdge 1 2 20

    let lg = lineGraph 1 path

    Assert.Equal(2, order lg)
    Assert.Equal(1, edgeCount lg)

[<Fact>]
let ``lineGraph node data equals original edge weight`` () =
    let path =
        empty Undirected
        |> addNode 0 ""
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 0 1 10
        |> addEdge 1 2 20

    let lg = lineGraph 1 path

    let weights = lg.Nodes |> Map.toList |> List.map snd |> Set.ofList
    Assert.Equal<Set<int>>(Set.ofList [ 10; 20 ], weights)

[<Fact>]
let ``lineGraph of directed path follows head-tail`` () =
    let path =
        empty Directed
        |> addNode 0 ""
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 0 1 1
        |> addEdge 1 2 1

    let lg = lineGraph 1 path

    Assert.Equal(2, order lg)
    Assert.Equal(1, edgeCount lg)

// ============================================================================
// POWER
// ============================================================================

[<Fact>]
let ``power of path connects distant nodes`` () =
    let path =
        empty Undirected
        |> addNode 0 ""
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 0 1 1
        |> addEdge 1 2 1

    let p2 = power 2 1 path

    Assert.Equal(3, order p2)
    Assert.True(hasEdge 0 2 p2)
    Assert.Equal(3, edgeCount p2)

[<Fact>]
let ``power with k 1 returns original edges`` () =
    let path = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 1

    let p1 = power 1 1 path

    Assert.Equal(1, edgeCount p1)
    Assert.True(hasEdge 0 1 p1)

// ============================================================================
// SUBGRAPH
// ============================================================================

[<Fact>]
let ``isSubgraph returns true for actual subgraph`` () =
    let container =
        empty Undirected
        |> addNode 1 ""
        |> addNode 2 ""
        |> addNode 3 ""
        |> addEdge 1 2 1
        |> addEdge 2 3 1

    let potential = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1

    Assert.True(isSubgraph potential container)

[<Fact>]
let ``isSubgraph returns false when edge is missing`` () =
    let container = empty Undirected |> addNode 1 "" |> addNode 2 ""
    let potential = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1

    Assert.False(isSubgraph potential container)

// ============================================================================
// ISOMORPHISM
// ============================================================================

[<Fact>]
let ``isomorphic detects identical triangles`` () =
    let g1 =
        empty Undirected
        |> addNode 0 ""
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 0 1 1
        |> addEdge 1 2 1
        |> addEdge 2 0 1

    let g2 =
        empty Undirected
        |> addNode 10 ""
        |> addNode 20 ""
        |> addNode 30 ""
        |> addEdge 10 20 1
        |> addEdge 20 30 1
        |> addEdge 30 10 1

    Assert.True(isomorphic g1 g2)

[<Fact>]
let ``isomorphic rejects triangle vs path`` () =
    let triangle =
        empty Undirected
        |> addNode 0 ""
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 0 1 1
        |> addEdge 1 2 1
        |> addEdge 2 0 1

    let path =
        empty Undirected
        |> addNode 0 ""
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 0 1 1
        |> addEdge 1 2 1

    Assert.False(isomorphic triangle path)

[<Fact>]
let ``isomorphic rejects different sized graphs`` () =
    let g1 = empty Undirected |> addNode 0 ""
    let g2 = empty Undirected |> addNode 0 "" |> addNode 1 ""

    Assert.False(isomorphic g1 g2)

[<Fact>]
let ``isomorphic rejects different kinds`` () =
    let directed = empty Directed |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 1
    let undirected = empty Undirected |> addNode 0 "" |> addNode 1 "" |> addEdge 0 1 1

    Assert.False(isomorphic directed undirected)
