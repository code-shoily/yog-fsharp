module Yog.FSharp.Tests.GeneratorsTests


open Xunit
open Yog.Model
open Yog.Generators

// ============================================================================
// CLASSIC GENERATORS - COMPLETE
// ============================================================================

[<Fact>]
let ``complete graph has correct number of nodes`` () =
    let g = Classic.complete 5 Undirected
    Assert.Equal(5, order g)

[<Fact>]
let ``complete undirected has correct edge count`` () =
    let n = 5
    let g = Classic.complete n Undirected
    // K_n has n(n-1)/2 edges
    let expectedEdges = n * (n - 1) / 2
    Assert.Equal(expectedEdges, edgeCount g)

[<Fact>]
let ``complete directed has correct edge count`` () =
    let n = 5
    let g = Classic.complete n Directed
    // K_n directed has n(n-1) edges (no self-loops)
    let expectedEdges = n * (n - 1)
    Assert.Equal(expectedEdges, edgeCount g)

[<Fact>]
let ``complete graph has all nodes connected`` () =
    let g = Classic.complete 4 Undirected
    // Every node should have degree 3
    for i in 0..3 do
        Assert.Equal(3, successors i g |> List.length)

[<Fact>]
let ``complete graph has unit weights`` () =
    let g = Classic.complete 4 Undirected

    for i in 0..3 do
        for (_, w) in successors i g do
            Assert.Equal(1, w)

[<Fact>]
let ``complete empty graph`` () =
    let g = Classic.complete 0 Undirected
    Assert.Equal(0, order g)
    Assert.Equal(0, edgeCount g)

[<Fact>]
let ``complete single node`` () =
    let g = Classic.complete 1 Undirected
    Assert.Equal(1, order g)
    Assert.Equal(0, edgeCount g)

// ============================================================================
// CLASSIC GENERATORS - CYCLE
// ============================================================================

[<Fact>]
let ``cycle graph has correct number of nodes`` () =
    let g = Classic.cycle 5 Undirected
    Assert.Equal(5, order g)

[<Fact>]
let ``cycle graph has correct edge count`` () =
    let g = Classic.cycle 5 Undirected
    // C_n has n edges
    Assert.Equal(5, edgeCount g)

[<Fact>]
let ``cycle graph forms ring`` () =
    let g = Classic.cycle 5 Undirected
    // Each node should have degree 2
    for i in 0..4 do
        Assert.Equal(2, successors i g |> List.length)

[<Fact>]
let ``cycle graph connections`` () =
    let g = Classic.cycle 5 Undirected
    // Node 0 should connect to 1 and 4
    let nbrs = neighbors 0 g |> List.map fst |> Set.ofList
    Assert.Equal<Set<NodeId>>(Set.ofList [ 1; 4 ], nbrs)

[<Fact>]
let ``cycle too small returns empty`` () =
    let g = Classic.cycle 2 Undirected
    Assert.Equal(0, order g)

[<Fact>]
let ``cycle directed`` () =
    let g = Classic.cycle 5 Directed
    // In directed cycle, each node has 1 successor
    for i in 0..4 do
        Assert.Equal(1, successors i g |> List.length)
    // Node i should connect to (i+1) % 5
    Assert.Equal<(NodeId * int) list>([ 1, 1 ], successors 0 g)
    Assert.Equal<(NodeId * int) list>([ 0, 1 ], successors 4 g)

// ============================================================================
// CLASSIC GENERATORS - PATH
// ============================================================================

[<Fact>]
let ``path graph has correct number of nodes`` () =
    let g = Classic.path 5 Undirected
    Assert.Equal(5, order g)

[<Fact>]
let ``path graph has correct edge count`` () =
    let g = Classic.path 5 Undirected
    // P_n has n-1 edges
    Assert.Equal(4, edgeCount g)

[<Fact>]
let ``path graph end nodes have degree 1`` () =
    let g = Classic.path 5 Undirected
    // End nodes (0 and 4) have degree 1
    Assert.Equal(1, successors 0 g |> List.length)
    Assert.Equal(1, successors 4 g |> List.length)

[<Fact>]
let ``path graph interior nodes have degree 2`` () =
    let g = Classic.path 5 Undirected
    // Interior nodes have degree 2
    for i in 1..3 do
        Assert.Equal(2, successors i g |> List.length)

[<Fact>]
let ``path graph connections`` () =
    let g = Classic.path 4 Undirected
    // Check linear connections
    Assert.True(
        neighbors 0 g
        |> List.exists (fun (id, _) -> id = 1)
    )

    Assert.True(
        neighbors 1 g
        |> List.exists (fun (id, _) -> id = 0)
    )

    Assert.True(
        neighbors 1 g
        |> List.exists (fun (id, _) -> id = 2)
    )

    Assert.True(
        neighbors 2 g
        |> List.exists (fun (id, _) -> id = 3)
    )

[<Fact>]
let ``path empty graph`` () =
    let g = Classic.path 0 Undirected
    Assert.Equal(0, order g)

[<Fact>]
let ``path single node`` () =
    let g = Classic.path 1 Undirected
    Assert.Equal(1, order g)
    Assert.Equal(0, edgeCount g)

// ============================================================================
// CLASSIC GENERATORS - STAR
// ============================================================================

[<Fact>]
let ``star graph has correct number of nodes`` () =
    let g = Classic.star 5 Undirected
    Assert.Equal(5, order g)

[<Fact>]
let ``star graph has correct edge count`` () =
    let g = Classic.star 5 Undirected
    // S_n has n-1 edges
    Assert.Equal(4, edgeCount g)

[<Fact>]
let ``star graph hub has correct degree`` () =
    let g = Classic.star 5 Undirected
    // Hub (node 0) has degree n-1
    Assert.Equal(4, successors 0 g |> List.length)

[<Fact>]
let ``star graph leaves have degree 1`` () =
    let g = Classic.star 5 Undirected
    // Leaves have degree 1
    for i in 1..4 do
        Assert.Equal(1, successors i g |> List.length)

[<Fact>]
let ``star graph connections`` () =
    let g = Classic.star 5 Undirected
    // All leaves connect to hub
    for i in 1..4 do
        Assert.True(
            neighbors i g
            |> List.exists (fun (id, _) -> id = 0)
        )

[<Fact>]
let ``star minimum graph`` () =
    let g = Classic.star 2 Undirected
    Assert.Equal(2, order g)
    Assert.Equal(1, edgeCount g)

// ============================================================================
// CLASSIC GENERATORS - WHEEL
// ============================================================================

[<Fact>]
let ``wheel graph has correct number of nodes`` () =
    let g = Classic.wheel 5 Undirected
    Assert.Equal(5, order g)

[<Fact>]
let ``wheel graph has correct edge count`` () =
    let n = 5
    let g = Classic.wheel n Undirected
    // W_n has 2(n-1) edges: (n-1) spokes + (n-1) rim edges
    Assert.Equal(2 * (n - 1), edgeCount g)

[<Fact>]
let ``wheel graph hub has correct degree`` () =
    let n = 5
    let g = Classic.wheel n Undirected
    // Hub connects to all rim nodes
    Assert.Equal(n - 1, successors 0 g |> List.length)

[<Fact>]
let ``wheel graph rim nodes have degree 3`` () =
    let g = Classic.wheel 5 Undirected
    // Rim nodes connect to hub and 2 neighbors
    for i in 1..4 do
        Assert.Equal(3, successors i g |> List.length)

[<Fact>]
let ``wheel too small returns empty`` () =
    let g = Classic.wheel 3 Undirected
    Assert.Equal(0, order g)

// ============================================================================
// CLASSIC GENERATORS - GRID2D
// ============================================================================

[<Fact>]
let ``grid2D has correct number of nodes`` () =
    let rows, cols = 3, 4
    let g = Classic.grid2D rows cols Undirected
    Assert.Equal(rows * cols, order g)

[<Fact>]
let ``grid2D has correct edge count`` () =
    let rows, cols = 3, 4
    let g = Classic.grid2D rows cols Undirected
    // m x n grid has (m-1)*n + m*(n-1) edges
    let expected = (rows - 1) * cols + rows * (cols - 1)
    Assert.Equal(expected, edgeCount g)

[<Fact>]
let ``grid2D corner nodes have degree 2`` () =
    let g = Classic.grid2D 3 4 Undirected
    // Corners: (0,0)=0, (0,3)=3, (2,0)=8, (2,3)=11
    Assert.Equal(2, successors 0 g |> List.length)
    Assert.Equal(2, successors 3 g |> List.length)
    Assert.Equal(2, successors 8 g |> List.length)
    Assert.Equal(2, successors 11 g |> List.length)

[<Fact>]
let ``grid2D edge nodes have degree 3`` () =
    let g = Classic.grid2D 3 4 Undirected
    // Edge node (not corner): node 1 (0,1)
    Assert.Equal(3, successors 1 g |> List.length)

[<Fact>]
let ``grid2D interior nodes have degree 4`` () =
    let g = Classic.grid2D 3 4 Undirected
    // Interior node: node 5 (1,1)
    Assert.Equal(4, successors 5 g |> List.length)

[<Fact>]
let ``grid2D 1xN is a path`` () =
    let g = Classic.grid2D 1 5 Undirected
    // 1x5 grid is just a path with 5 nodes
    Assert.Equal(5, order g)
    Assert.Equal(4, edgeCount g)

[<Fact>]
let ``grid2D directed`` () =
    let g = Classic.grid2D 2 2 Directed
    // grid2D creates directed edges in one direction only
    // So 2x2 directed has same 4 edges as undirected
    Assert.Equal(4, edgeCount g)

// ============================================================================
// RANDOM GENERATORS - ERDOS-RENYI
// ============================================================================

[<Fact>]
let ``erdosRenyiGnp generates correct number of nodes`` () =
    let g = Random.erdosRenyiGnp 100 0.5 Undirected
    Assert.Equal(100, order g)

[<Fact>]
let ``erdosRenyiGnp p=0 generates no edges`` () =
    let g = Random.erdosRenyiGnp 10 0.0 Undirected
    Assert.Equal(0, edgeCount g)

[<Fact>]
let ``erdosRenyiGnp p=1 generates complete graph`` () =
    let n = 5
    let g = Random.erdosRenyiGnp n 1.0 Undirected
    let expectedEdges = n * (n - 1) / 2
    Assert.Equal(expectedEdges, edgeCount g)

[<Fact>]
let ``erdosRenyiGnp directed respects direction`` () =
    let n = 10
    let g = Random.erdosRenyiGnp n 1.0 Directed
    // Directed complete has n(n-1) edges
    let expectedEdges = n * (n - 1)
    Assert.Equal(expectedEdges, edgeCount g)

// ============================================================================
// RANDOM GENERATORS - BARABASI-ALBERT
// ============================================================================

[<Fact>]
let ``barabasiAlbert generates correct number of nodes`` () =
    let g = Random.barabasiAlbert 100 3 Undirected
    Assert.Equal(100, order g)

[<Fact>]
let ``barabasiAlbert returns empty if n < m`` () =
    let g = Random.barabasiAlbert 5 10 Undirected
    Assert.Equal(0, order g)

[<Fact>]
let ``barabasiAlbert small graph`` () =
    // With n=5, m=2, should start with complete graph of 2 nodes
    let g = Random.barabasiAlbert 5 2 Undirected
    Assert.Equal(5, order g)

[<Fact>]
let ``barabasiAlbert creates scale_free structure`` () =
    // Larger test to verify preferential attachment
    let g = Random.barabasiAlbert 50 3 Undirected
    Assert.Equal(50, order g)
    // Should have edges (approximately)
    Assert.True(edgeCount g > 0)

// ============================================================================
// RANDOM GENERATORS - WATTS-STROGATZ
// ============================================================================

[<Fact>]
let ``wattsStrogatz generates correct number of nodes`` () =
    let g = Random.wattsStrogatz 100 6 0.1 Undirected
    Assert.Equal(100, order g)

[<Fact>]
let ``wattsStrogatz returns empty if n < 3`` () =
    let g = Random.wattsStrogatz 2 2 0.1 Undirected
    Assert.Equal(0, order g)

[<Fact>]
let ``wattsStrogatz returns empty if k >= n`` () =
    let g = Random.wattsStrogatz 5 5 0.1 Undirected
    Assert.Equal(0, order g)

[<Fact>]
let ``wattsStrogatz p=0 creates regular lattice`` () =
    let n, k = 10, 4
    let g = Random.wattsStrogatz n k 0.0 Undirected
    // Regular lattice has n * k / 2 edges
    Assert.Equal(n * k / 2, edgeCount g)
    // Each node has degree k
    for i in 0 .. n - 1 do
        Assert.Equal(k, successors i g |> List.length)

[<Fact>]
let ``wattsStrogatz p=1 creates random graph`` () =
    // With p=1, all edges are rewired (no regular structure)
    let g = Random.wattsStrogatz 20 4 1.0 Undirected
    Assert.Equal(20, order g)
    // Still has correct number of edges
    Assert.Equal(40, edgeCount g)

// ============================================================================
// CLASSIC GENERATORS - COMPLETE BIPARTITE
// ============================================================================

[<Fact>]
let ``completeBipartite has correct number of nodes`` () =
    let g = Classic.completeBipartite 3 4 Undirected
    Assert.Equal(7, order g)

[<Fact>]
let ``completeBipartite has correct edge count`` () =
    let m, n = 3, 4
    let g = Classic.completeBipartite m n Undirected
    // K_{m,n} has m * n edges
    Assert.Equal(m * n, edgeCount g)

[<Fact>]
let ``completeBipartite partitions are correct`` () =
    let g = Classic.completeBipartite 3 4 Undirected
    // Left partition (0,1,2) should only connect to right partition (3,4,5,6)
    for i in 0..2 do
        let nbrs = successors i g |> List.map fst
        Assert.All(nbrs, (fun n -> Assert.True(n >= 3 && n <= 6)))

[<Fact>]
let ``completeBipartite directed`` () =
    let g = Classic.completeBipartite 2 3 Directed
    Assert.Equal(6, edgeCount g)

// ============================================================================
// CLASSIC GENERATORS - EMPTY
// ============================================================================

[<Fact>]
let ``emptyGraph has correct number of nodes`` () =
    let g = Classic.emptyGraph 10 Undirected
    Assert.Equal(10, order g)

[<Fact>]
let ``emptyGraph has no edges`` () =
    let g = Classic.emptyGraph 20 Undirected
    Assert.Equal(0, edgeCount g)

// ============================================================================
// CLASSIC GENERATORS - BINARY TREE
// ============================================================================

[<Fact>]
let ``binaryTree has correct number of nodes`` () =
    let g = Classic.binaryTree 3 Undirected
    // 2^(depth+1) - 1 = 2^4 - 1 = 15
    Assert.Equal(15, order g)

[<Fact>]
let ``binaryTree has correct edge count`` () =
    let g = Classic.binaryTree 3 Undirected
    // Binary tree has n-1 edges
    Assert.Equal(14, edgeCount g)

[<Fact>]
let ``binaryTree root has two children`` () =
    let g = Classic.binaryTree 3 Undirected
    // Root (node 0) should have 2 children (1 and 2)
    let children = successors 0 g |> List.map fst |> Set.ofList
    Assert.Equal<Set<NodeId>>(Set.ofList [ 1; 2 ], children)

[<Fact>]
let ``binaryTree depth 0 is single node`` () =
    let g = Classic.binaryTree 0 Undirected
    Assert.Equal(1, order g)
    Assert.Equal(0, edgeCount g)

[<Fact>]
let ``binaryTree negative depth returns empty`` () =
    let g = Classic.binaryTree -1 Undirected
    Assert.Equal(0, order g)

// ============================================================================
// CLASSIC GENERATORS - PETERSEN
// ============================================================================

[<Fact>]
let ``petersenGraph has 10 nodes`` () =
    let g = Classic.petersenGraph Undirected
    Assert.Equal(10, order g)

[<Fact>]
let ``petersenGraph has 15 edges`` () =
    let g = Classic.petersenGraph Undirected
    Assert.Equal(15, edgeCount g)

[<Fact>]
let ``petersenGraph is 3-regular`` () =
    let g = Classic.petersenGraph Undirected
    // Every node has degree 3
    for i in 0..9 do
        Assert.Equal(3, successors i g |> List.length)

// ============================================================================
// RANDOM GENERATORS - ERDOS-RENYI GNM
// ============================================================================

[<Fact>]
let ``erdosRenyiGnm generates correct number of nodes`` () =
    let g = Random.erdosRenyiGnm 100 50 Undirected
    Assert.Equal(100, order g)

[<Fact>]
let ``erdosRenyiGnm generates exactly m edges`` () =
    let g = Random.erdosRenyiGnm 50 100 Undirected
    Assert.Equal(100, edgeCount g)

[<Fact>]
let ``erdosRenyiGnm m=0 generates no edges`` () =
    let g = Random.erdosRenyiGnm 10 0 Undirected
    Assert.Equal(0, edgeCount g)

[<Fact>]
let ``erdosRenyiGnm caps at max edges`` () =
    let n = 5
    let maxEdges = n * (n - 1) / 2
    let g = Random.erdosRenyiGnm n 1000 Undirected
    // Should cap at maxEdges
    Assert.Equal(maxEdges, edgeCount g)

// ============================================================================
// RANDOM GENERATORS - RANDOM TREE
// ============================================================================

[<Fact>]
let ``randomTree generates correct number of nodes`` () =
    let g = Random.randomTree 50 Undirected
    Assert.Equal(50, order g)

[<Fact>]
let ``randomTree has n-1 edges`` () =
    let n = 50
    let g = Random.randomTree n Undirected
    Assert.Equal(n - 1, edgeCount g)

[<Fact>]
let ``randomTree single node`` () =
    let g = Random.randomTree 1 Undirected
    Assert.Equal(1, order g)
    Assert.Equal(0, edgeCount g)

[<Fact>]
let ``randomTree is connected`` () =
    // Use traversal to check connectivity
    let g = Random.randomTree 20 Undirected
    let visited = Yog.Traversal.walk 0 Yog.Traversal.BreadthFirst g
    Assert.Equal(20, visited.Length)
