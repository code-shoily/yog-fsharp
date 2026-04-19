/// Graph generators for creating common graph structures and random network models.
///
/// This module provides both deterministic and stochastic graph generators, useful for:
/// - Testing graph algorithms with known structures
/// - Modeling real-world networks
/// - Benchmarking and performance analysis
/// - Generating synthetic datasets
///
/// ## Module Structure
///
/// | Module | Type | Generators |
/// |--------|------|------------|
/// | `Classic` | Deterministic | Complete, Cycle, Path, Star, Wheel, Grid, Binary Tree, Bipartite, Petersen |
/// | `Random` | Stochastic | Erdős-Rényi (G(n,p) and G(n,m)), Barabási-Albert, Watts-Strogatz, Random Tree |
///
/// ## Quick Start
///
///     open Yog.Generators
///     open Yog.Model
///
///     // Classic deterministic graphs
///     let cycle = Classic.cycle 5 Undirected           // C5 cycle graph
///     let complete = Classic.complete 4 Undirected     // K4 complete graph
///     let grid = Classic.grid2D 3 4 Undirected         // 3x4 grid lattice
///     let tree = Classic.binaryTree 3 Undirected       // Depth-3 binary tree
///     let petersen = Classic.petersenGraph Undirected  // Famous Petersen graph
///
///     // Random network models
///     let sparse = Random.erdosRenyiGnp 100 0.05 Undirected      // Sparse random
///     let exact = Random.erdosRenyiGnm 50 100 Undirected         // Exactly 100 edges
///     let scaleFree = Random.barabasiAlbert 1000 3 Undirected    // Scale-free network
///     let smallWorld = Random.wattsStrogatz 100 6 0.1 Undirected // Small-world network
///     let randTree = Random.randomTree 50 Undirected             // Random spanning tree
///
/// ## Classic Generators
///
/// Deterministic generators produce identical graphs given the same parameters:
///
/// - **complete** - K_n: Every node connects to every other (O(n²))
/// - **cycle** - C_n: Nodes form a ring (O(n))
/// - **path** - P_n: Linear chain of nodes (O(n))
/// - **star** - S_n: Hub connected to all others (O(n))
/// - **wheel** - W_n: Cycle with central hub (O(n))
/// - **grid2D** - m×n lattice mesh (O(mn))
/// - **completeBipartite** - K_{m,n}: Two complete partitions (O(mn))
/// - **binaryTree** - Complete binary tree (O(2^depth))
/// - **petersenGraph** - Famous 3-regular graph (O(1))
/// - **emptyGraph** - n isolated nodes (O(n))
///
/// ## Random Generators
///
/// Stochastic generators use randomness to model real-world networks:
///
/// - **erdosRenyiGnp** - G(n,p): Each edge independently with probability p
/// - **erdosRenyiGnm** - G(n,m): Exactly m random edges
/// - **barabasiAlbert** - Scale-free via preferential attachment (power-law degree distribution)
/// - **wattsStrogatz** - Small-world via ring lattice + rewiring (high clustering, short paths)
/// - **randomTree** - Uniformly random spanning tree
///
/// ## References
///
/// - [Wikipedia: Graph Generators](https://en.wikipedia.org/wiki/Graph_theory#Graph_generators)
/// - [NetworkX Documentation](https://networkx.org/documentation/stable/reference/generators.html)
/// - [Erdős-Rényi Model](https://en.wikipedia.org/wiki/Erd%C5%91s%E2%80%93R%C3%A9nyi_model)
/// - [Barabási-Albert Model](https://en.wikipedia.org/wiki/Barab%C3%A1si%E2%80%93Albert_model)
/// - [Watts-Strogatz Model](https://en.wikipedia.org/wiki/Watts%E2%80%93Strogatz_model)
module Yog.Generators

open System
open System.Collections.Generic
open Yog.Model

module Internal =
    let createNodes n (graph: Graph<unit, int>) =
        let mutable g = graph

        for i in 0 .. n - 1 do
            g <- addNode i () g

        g

/// Deterministic graph generators for classic graph structures.
module Classic =
    open Internal

    /// Generates a complete graph K_n where every node connects to every other.
    ///
    /// In a complete graph with n nodes, there are n(n-1)/2 edges for undirected
    /// graphs and n(n-1) edges for directed graphs. All edges have unit weight (1).
    ///
    /// **Time Complexity:** O(n²)
    ///
    /// ## Example
    ///
    ///     let k5 = Classic.complete 5 Undirected
    ///     // K5 has 5 nodes and 10 edges
    ///
    /// ## Use Cases
    ///
    /// - Testing algorithms on dense graphs
    /// - Maximum connectivity scenarios
    /// - Clique detection benchmarks
    let complete n kind =
        let mutable g = createNodes n (empty kind)

        for i in 0 .. n - 1 do
            let startJ = if kind = Undirected then i + 1 else 0

            for j in startJ .. n - 1 do
                if i <> j then
                    g <- addEdge i j 1 g

        g

    /// Generates a cycle graph C_n where nodes form a ring.
    ///
    /// A cycle graph connects n nodes in a circular pattern:
    /// 0-1-2-...-(n-1)-0. Each node has degree 2.
    ///
    /// Returns an empty graph if n < 3 (cycles require at least 3 nodes).
    ///
    /// **Time Complexity:** O(n)
    ///
    /// ## Example
    ///
    ///     let c5 = Classic.cycle 5 Undirected
    ///     // C5: 0-1-2-3-4-0 (a pentagon)
    ///
    /// ## Use Cases
    ///
    /// - Ring network topologies
    /// - Circular dependency testing
    /// - Hamiltonian cycle benchmarks
    let cycle n kind =
        if n < 3 then
            empty kind
        else
            let mutable g = createNodes n (empty kind)

            for i in 0 .. n - 1 do
                g <- addEdge i ((i + 1) % n) 1 g

            g

    /// Generates a path graph P_n (linear chain).
    ///
    /// A path graph connects n nodes in a linear sequence:
    /// 0-1-2-...-(n-1). End nodes have degree 1, interior nodes have degree 2.
    ///
    /// **Time Complexity:** O(n)
    ///
    /// ## Example
    ///
    ///     let p4 = Classic.path 4 Undirected
    ///     // P4: 0-1-2-3
    ///
    /// ## Use Cases
    ///
    /// - Linear network topologies
    /// - Chain processing pipelines
    /// - Pathfinding algorithm tests
    let path n kind =
        let mutable g = createNodes n (empty kind)

        for i in 0 .. n - 2 do
            g <- addEdge i (i + 1) 1 g

        g

    /// Generates a star graph S_n where node 0 is the hub.
    ///
    /// A star graph has one central node (0) connected to all other nodes.
    /// The hub has degree n-1, all other nodes have degree 1.
    ///
    /// **Time Complexity:** O(n)
    ///
    /// ## Example
    ///
    ///     let s5 = Classic.star 5 Undirected
    ///     // S5: hub 0 connected to nodes 1, 2, 3, 4
    ///
    /// ## Use Cases
    ///
    /// - Hub-and-spoke network topologies
    /// - Centralized architecture modeling
    /// - Broadcast/multicast scenarios
    let star n kind =
        let mutable g = createNodes n (empty kind)

        for i in 1 .. n - 1 do
            g <- addEdge 0 i 1 g

        g

    /// Generates a wheel graph W_n (cycle with a central hub).
    ///
    /// A wheel graph combines a star and a cycle: node 0 is the hub,
    /// and nodes 1..(n-1) form a cycle.
    ///
    /// Returns an empty graph if n < 4 (wheels require at least 4 nodes).
    ///
    /// **Time Complexity:** O(n)
    ///
    /// ## Example
    ///
    ///     let w6 = Classic.wheel 6 Undirected
    ///     // W6: hub 0 connected to cycle 1-2-3-4-5-1
    ///
    /// ## Use Cases
    ///
    /// - Hybrid network topologies
    /// - Fault-tolerant network design
    /// - Routing algorithm benchmarks
    let wheel n kind =
        if n < 4 then
            empty kind
        else
            let mutable g = star n kind

            for i in 1 .. n - 1 do
                let next = if i = n - 1 then 1 else i + 1
                g <- addEdge i next 1 g

            g

    /// Generates a 2D grid graph (lattice) of rows x cols.
    ///
    /// Creates a rectangular grid where each node is connected to its
    /// orthogonal neighbors (up, down, left, right). Nodes are numbered
    /// row by row: node at (r, c) has ID = r * cols + c.
    ///
    /// **Time Complexity:** O(rows * cols)
    ///
    /// ## Example
    ///
    ///     let grid = Classic.grid2D 3 4 Undirected
    ///     // 3x4 grid with 12 nodes
    ///     // Node numbering: 0-1-2-3
    ///     //                 | | | |
    ///     //                 4-5-6-7
    ///     //                 | | | |
    ///     //                 8-9-10-11
    ///
    /// ## Use Cases
    ///
    /// - Mesh network topologies
    /// - Spatial/grid-based algorithms
    /// - Image processing graph models
    /// - Game board representations
    let grid2D rows cols kind =
        let mutable g = createNodes (rows * cols) (empty kind)

        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                let node = r * cols + c

                if c < cols - 1 then
                    g <- addEdge node (node + 1) 1 g

                if r < rows - 1 then
                    g <- addEdge node (node + cols) 1 g

        g

    /// Generates a complete bipartite graph K_{m,n}.
    ///
    /// A complete bipartite graph has two disjoint sets of nodes (left and right partitions),
    /// where every node in the left partition connects to every node in the right partition.
    /// Left partition: nodes 0..(m-1), Right partition: nodes m..(m+n-1).
    ///
    /// **Time Complexity:** O(mn)
    ///
    /// ## Example
    ///
    ///     let k33 = Classic.completeBipartite 3 3 Undirected
    ///     // K_{3,3}: 3 nodes in each partition, 9 edges
    ///
    /// ## Use Cases
    ///
    /// - Matching problems (job assignment, pairing)
    /// - Bipartite graph algorithms
    /// - Network flow modeling
    let completeBipartite m n kind =
        let total = m + n
        let mutable g = createNodes total (empty kind)

        for left in 0 .. m - 1 do
            for right in m .. total - 1 do
                g <- addEdge left right 1 g

        g

    /// Generates an empty graph with n nodes and no edges.
    ///
    /// **Time Complexity:** O(n)
    ///
    /// ## Example
    ///
    ///     let isolated = Classic.emptyGraph 10 Undirected
    ///     // 10 isolated nodes, no edges
    ///
    /// ## Use Cases
    ///
    /// - Starting point for custom graph construction
    /// - Independent set problems
    /// - Testing disconnected components
    let emptyGraph n kind = createNodes n (empty kind)

    /// Generates a complete binary tree of given depth.
    ///
    /// Node 0 is the root. For node i: left child is 2i+1, right child is 2i+2.
    /// Total nodes: 2^(depth+1) - 1. All edges have unit weight (1).
    ///
    /// **Time Complexity:** O(2^depth)
    ///
    /// ## Example
    ///
    ///     let tree = Classic.binaryTree 3 Undirected
    ///     // Complete binary tree with depth 3, total 15 nodes
    ///
    /// ## Use Cases
    ///
    /// - Hierarchical structures
    /// - Binary search tree modeling
    /// - Heap data structure visualization
    /// - Tournament brackets
    let binaryTree depth kind =
        if depth < 0 then
            empty kind
        else
            let rec pow b e = if e = 0 then 1 else b * pow b (e - 1)
            let n = pow 2 (depth + 1) - 1
            let mutable g = createNodes n (empty kind)

            for i in 0 .. n - 1 do
                let leftChild = 2 * i + 1
                let rightChild = 2 * i + 2

                if leftChild < n then
                    g <- addEdge i leftChild 1 g

                if rightChild < n then
                    g <- addEdge i rightChild 1 g

            g

    /// Generates the Petersen graph.
    ///
    /// The [Petersen graph](https://en.wikipedia.org/wiki/Petersen_graph) is a famous
    /// undirected graph with 10 nodes and 15 edges. It is often used as a counterexample
    /// in graph theory due to its unique properties.
    ///
    /// **Time Complexity:** O(1)
    ///
    /// ## Example
    ///
    ///     let petersen = Classic.petersenGraph Undirected
    ///     // 10 nodes, 15 edges
    ///
    /// ## Properties
    ///
    /// - 3-regular (every node has degree 3)
    /// - Diameter 2
    /// - Not planar
    /// - Not Hamiltonian
    ///
    /// ## Use Cases
    ///
    /// - Graph theory counterexamples
    /// - Algorithm testing
    /// - Theoretical research
    let petersenGraph kind =
        let mutable g = createNodes 10 (empty kind)
        // Outer pentagon: 0-1-2-3-4-0
        g <- addEdge 0 1 1 g
        g <- addEdge 1 2 1 g
        g <- addEdge 2 3 1 g
        g <- addEdge 3 4 1 g
        g <- addEdge 4 0 1 g
        // Inner pentagram: 5-7-9-6-8-5
        g <- addEdge 5 7 1 g
        g <- addEdge 7 9 1 g
        g <- addEdge 9 6 1 g
        g <- addEdge 6 8 1 g
        g <- addEdge 8 5 1 g
        // Connect outer to inner (spokes)
        g <- addEdge 0 5 1 g
        g <- addEdge 1 6 1 g
        g <- addEdge 2 7 1 g
        g <- addEdge 3 8 1 g
        g <- addEdge 4 9 1 g
        g


/// Random graph generators for stochastic network models.
module Random =
    open Internal
    let private rng = Random()

    /// Erdős-Rényi G(n, p) model: each edge exists with probability p.
    ///
    /// Generates a random graph where each possible edge is included
    /// independently with probability p. For undirected graphs, each
    /// unordered pair is considered once.
    ///
    /// **Time Complexity:** O(n²)
    ///
    /// ## Parameters
    ///
    /// - `n`: Number of nodes
    /// - `p`: Edge probability (0.0 to 1.0)
    /// - `kind`: Directed or Undirected
    ///
    /// ## Example
    ///
    ///     // Sparse random graph
    ///     let sparse = Random.erdosRenyiGnp 100 0.05 Undirected
    ///
    ///     // Dense random graph
    ///     let dense = Random.erdosRenyiGnp 50 0.8 Directed
    ///
    /// ## Properties
    ///
    /// - Expected number of edges: p * n(n-1)/2 (undirected) or p * n(n-1) (directed)
    /// - Phase transition at p = 1/n (giant component emerges)
    ///
    /// ## Use Cases
    ///
    /// - Random network modeling
    /// - Percolation studies
    /// - Average-case algorithm analysis
    let erdosRenyiGnp n (p: float) kind =
        let mutable g = createNodes n (empty kind)

        for i in 0 .. n - 1 do
            let startJ = if kind = Undirected then i + 1 else 0

            for j in startJ .. n - 1 do
                if i <> j && rng.NextDouble() < p then
                    g <- addEdge i j 1 g

        g

    /// Erdős-Rényi G(n, m) model: exactly m edges are added uniformly at random.
    ///
    /// Unlike G(n, p) which includes each edge independently with probability p,
    /// G(n, m) guarantees exactly m edges in the resulting graph.
    ///
    /// **Time Complexity:** O(m) expected
    ///
    /// ## Parameters
    ///
    /// - `n`: Number of nodes
    /// - `m`: Exact number of edges to add
    /// - `kind`: Directed or Undirected
    ///
    /// ## Example
    ///
    ///     // Random graph with 50 nodes and exactly 100 edges
    ///     let graph = Random.erdosRenyiGnm 50 100 Undirected
    ///
    /// ## Properties
    ///
    /// - Exactly m edges (unlike G(n,p) which has expected m edges)
    /// - Uniform distribution over all graphs with n nodes and m edges
    ///
    /// ## Use Cases
    ///
    /// - Fixed edge count requirements
    /// - Random graph benchmarking
    /// - Testing with specific densities
    let erdosRenyiGnm n m kind =
        let maxEdges = if kind = Undirected then n * (n - 1) / 2 else n * (n - 1)

        let actualM = min m maxEdges
        let mutable g = createNodes n (empty kind)
        let mutable addedEdges = HashSet<NodeId * NodeId>()

        while addedEdges.Count < actualM do
            let i = rng.Next(n)
            let j = rng.Next(n)

            if i <> j then
                let edge = if kind = Undirected && i > j then (j, i) else (i, j)

                if addedEdges.Add(edge) then
                    g <- addEdge (fst edge) (snd edge) 1 g

        g

    /// Barabási-Albert model: creates scale-free networks via preferential attachment.
    ///
    /// Generates a random graph with a power-law degree distribution (scale-free).
    /// New nodes preferentially attach to existing high-degree nodes ("rich get richer").
    ///
    /// **Time Complexity:** O(n * m * average_degree)
    ///
    /// ## Parameters
    ///
    /// - `n`: Total number of nodes
    /// - `m`: Number of edges each new node creates (must be < n)
    /// - `kind`: Directed or Undirected
    ///
    /// ## Example
    ///
    ///     // Scale-free network with 1000 nodes, each connecting to 3 existing nodes
    ///     let ba = Random.barabasiAlbert 1000 3 Undirected
    ///
    /// ## Properties
    ///
    /// - Power-law degree distribution: P(k) ~ k^(-3)
    /// - Hub nodes with very high degree
    /// - Small-world properties
    ///
    /// ## Use Cases
    ///
    /// - Social network modeling
    /// - Citation network analysis
    /// - Web graph simulation
    let barabasiAlbert n m kind =
        if n < m || m < 1 then
            empty kind
        else
            let m0 = max m 2
            let mutable g = Classic.complete m0 kind

            for newNode in m0 .. n - 1 do
                g <- addNode newNode () g
                let mutable pool = []

                for node in allNodes g do
                    if node <> newNode then
                        let deg =
                            if kind = Undirected then
                                (neighbors node g).Length
                            else
                                (successors node g).Length

                        for _ in 1 .. max deg 1 do
                            pool <- node :: pool

                let targets =
                    pool |> List.sortBy (fun _ -> rng.Next()) |> List.distinct |> List.truncate m

                for t in targets do
                    g <- addEdge newNode t 1 g

            g

    /// Watts-Strogatz model: small-world network (ring lattice + rewiring).
    ///
    /// Generates a graph with both high clustering (like regular lattices)
    /// and short path lengths (like random graphs). Starts with a ring
    /// lattice and rewires edges with probability p.
    ///
    /// **Time Complexity:** O(n * k)
    ///
    /// ## Parameters
    ///
    /// - `n`: Number of nodes (must be >= 3)
    /// - `k`: Each node connects to k nearest neighbors (must be even, < n)
    /// - `p`: Rewiring probability (0.0 = regular lattice, 1.0 = random)
    /// - `kind`: Directed or Undirected
    ///
    /// ## Example
    ///
    ///     // Small-world network: 100 nodes, 6 neighbors each, 10% rewiring
    ///     let sw = Random.wattsStrogatz 100 6 0.1 Undirected
    ///
    /// ## Properties
    ///
    /// - High clustering coefficient
    /// - Short average path length
    /// - p=0: regular lattice, p=1: random graph
    ///
    /// ## Use Cases
    ///
    /// - Social network modeling (six degrees of separation)
    /// - Neural network topology
    /// - Epidemic spread modeling
    let wattsStrogatz n k (p: float) kind =
        if n < 3 || k < 2 || k >= n then
            empty kind
        else
            let mutable g = createNodes n (empty kind)
            let halfK = k / 2

            for i in 0 .. n - 1 do
                for offset in 1..halfK do
                    if rng.NextDouble() >= p then
                        g <- addEdge i ((i + offset) % n) 1 g
                    else
                        let mutable target = rng.Next(n)

                        while target = i || (successors i g |> List.exists (fun (id, _) -> id = target)) do
                            target <- rng.Next(n)

                        g <- addEdge i target 1 g

            g

    /// Generates a uniformly random tree on n nodes.
    ///
    /// Creates a tree by starting with node 0 and repeatedly connecting
    /// new nodes to random nodes already in the tree. This produces a
    /// uniform distribution over all labeled trees.
    ///
    /// **Time Complexity:** O(n²) expected
    ///
    /// ## Example
    ///
    ///     let tree = Random.randomTree 50 Undirected
    ///     // Random tree with 50 nodes, 49 edges
    ///
    /// ## Properties
    ///
    /// - Exactly n-1 edges (tree property)
    /// - Connected
    /// - Acyclic
    /// - Uniform distribution over all labeled trees
    ///
    /// ## Use Cases
    ///
    /// - Random spanning tree generation
    /// - Tree algorithm testing
    /// - Network topology generation
    /// - Phylogenetic tree simulation
    let randomTree n kind =
        if n < 2 then
            createNodes n (empty kind)
        else
            let mutable g = createNodes n (empty kind)
            let inTree = HashSet<NodeId>([ 0 ])

            for nextNode in 1 .. n - 1 do
                let treeList = inTree |> Seq.toArray
                let parent = treeList.[rng.Next(treeList.Length)]
                g <- addEdge parent nextNode 1 g
                inTree.Add(nextNode) |> ignore

            g
