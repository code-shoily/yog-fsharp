# Algorithmic Invariants Catalog

This document lists the algorithmic invariants (hypotheses) verified by `Yog.FSharp`'s testing suite. We use property-based testing (Hedgehog) and extensive unit-test cases to ensure these properties hold across a wide range of edge cases, including sparse, dense, disconnected, cyclic, directed, and undirected graphs.

**Location**: Tests are located in `tests/Yog.FSharp.Tests/`.

**Framework**: Property-based tests use [Hedgehog](https://hedgehog.qa/); oracle comparisons against NetworkX/libgraph are intentionally excluded and will be added later with our own reference implementations.

---

## Connectivity and Components

- **SCC Partitioning**: Strongly Connected Components must partition the set of all nodes exactly. Every node belongs to one and only one SCC.
- **SCC Algorithm Agreement**: Tarjan and Kosaraju produce the same set of strongly connected components.
- **Bridge Removal**: Removing a discovered bridge from a connected undirected graph disconnects the graph.
- **MST Algorithm Agreement**: Kruskal, Prim, and Borůvka algorithms must produce the same total weight for connected undirected graphs with non-negative weights.
- **MST of a Tree**: The MST of a tree is the tree itself (same edge count and total weight).
- **MST Directed Rejection**: MST algorithms return an error when given a directed graph.

## Pathfinding and Flow

- **Dijkstra-BFS Agreement**: Dijkstra agrees with BFS on the shortest path distance for unweighted graphs.
- **Algorithm Consistency**: Dijkstra, Bellman-Ford, and A* (with zero heuristic) must agree on shortest path weights for non-negative weights.
- **Negative Cycle Detection**: Bellman-Ford correctly identifies graphs containing reachable negative cycles.
- **Bidirectional Correctness**: Bidirectional Dijkstra yields the same path distance as standard Dijkstra.
- **Floyd-Warshall Agreement**: Floyd-Warshall agrees with repeated Dijkstra runs for all-pairs shortest paths.
- **Floyd-Warshall Triangle Inequality**: Floyd-Warshall distances satisfy `d(u, w) ≤ d(u, v) + d(v, w)`.
- **Max-Flow Min-Cut Theorem**: The maximum flow value equals the minimum cut capacity.
- **Flow Conservation**: At every node except source and sink, total in-flow equals total out-flow.
- **Max-Flow Non-Negativity**: Maximum flow is always non-negative.
- **Max-Flow Source Bound**: Maximum flow cannot exceed the sum of outgoing capacities from the source.
- **Max-Flow Algorithm Agreement**: Edmonds-Karp and Dinic produce identical max-flow values.
- **Residual Graph Separation**: After computing max flow, the sink is unreachable from the source in the residual graph.

## Centrality Measures

- **Star Graph Centrality**: In a star graph with at least three nodes, the center node has strictly higher degree centrality than any leaf node.
- **Betweenness Non-Negativity**: All betweenness centrality scores are non-negative.
- **Closeness Range**: All closeness centrality scores lie in `[0, 1]`.
- **PageRank Unity**: The sum of PageRank scores across all nodes equals exactly 1.0.
- **Eigenvector Centrality**: Non-negative for connected graphs (where defined).

## Matching

- **Bipartite Matching Partition**: Maximum matching edges only connect vertices between left and right partitions.
- **Hopcroft-Karp Bipartite Matching**: Maximum matching size matches naive augmenting paths algorithm.
- **Hungarian Cost Optimality**: Hungarian algorithm produces a perfect matching with optimal (min or max) total cost.
- **Edmonds' Blossom Alternating Paths**: Blossom maximum matching finds the absolute maximum cardinality matching on general graphs, resolving odd cycles via contraction.

## Graph Operations & Transformations

- **Transpose Involutivity**: `transpose (transpose G) == G`.
- **Undirected Symmetry**: Undirected graphs have symmetric successor/predecessor lists.
- **Edge Count Consistency**: `edgeCount G` equals the length of `allEdges G`.
- **Neighbors Equals Successors (Undirected)**: For undirected graphs, `neighbors u` and `successors u` return the same adjacency list.
- **Map-Nodes Identity**: `mapNodes id G == G`.
- **Map-Nodes Topology Preservation**: Node and edge counts are preserved under node mapping.
- **Map-Edges Identity**: `mapEdges id G == G`.
- **Map-Edges Topology Preservation**: Node and edge counts are preserved under edge-weight mapping.
- **Filter-Nodes Consistency**: Filtering nodes removes associated edges and preserves the structure of the remaining subgraph.
- **Complement Invariants**: Complement of complement (minus self-loops) equals the original graph without self-loops.
- **Union**: Union contains all nodes and edges from both graphs.
- **Intersection Idempotence**: Intersection of a graph with itself equals the graph.
- **Difference Annihilation**: Difference of a graph with itself is empty.
- **Symmetric Difference Commutativity**: `symmetricDifference G1 G2 == symmetricDifference G2 G1`.
- **Isomorphism Reflexivity**: A graph is isomorphic to itself.
- **Power Identity**: Power of a graph to 1 preserves the node set.
- **Line Graph Node Count**: The line graph has exactly as many nodes as the original graph has edges.
- **Cartesian Product Order**: `order (cartesianProduct G1 G2) == order G1 * order G2`.
- **Tensor Product Order**: `order (tensorProduct G1 G2) == order G1 * order G2`.
- **Strong Product Order**: `order (strongProduct G1 G2) == order G1 * order G2`.
- **Lexicographic Product Order**: `order (lexicographicProduct G1 G2) == order G1 * order G2`.

## Traversal

- **BFS Uniqueness**: BFS visits each reachable node exactly once.
- **DFS-BFS Node Agreement**: DFS and BFS visit the same set of reachable nodes.
- **Cyclicity Consistency**: A graph is either cyclic or acyclic (exclusive or).
- **DAGs from Increasing Edges**: A directed graph built only from smaller to larger node IDs is acyclic.
- **Topological Order**: For any edge $(u,v)$ in a DAG, $u$ appears before $v$ in topological sort.

## Structural Properties

- **Tree Characterization**: Generated random trees are acyclic and have exactly $V - 1$ edges.
- **Arborescence Validity**: Directed trees have a unique root with in-degree 0, others in-degree 1.
- **Complete Graph**: $K_n$ has $n$ nodes and $n(n-1)/2$ edges; it is complete and $(n-1)$-regular.
- **Cycle Graph**: $C_n$ has $n$ nodes and $n$ edges.
- **Path Graph**: $P_n$ has diameter $n - 1$.
- **Bipartite Complete Graphs**: Complete bipartite graphs $K_{m,n}$ are bipartite.
- **Bipartite Partition**: The two partitions are disjoint and together cover all nodes.
- **Planarity Consistency**: K5 and K3,3 are correctly classified as non-planar, while grids are planar. Kuratowski witnesses accurately isolate $K_5$ or $K_{3,3}$ subdivisions.
- **WL Hashing Isomorphism**: WL signatures match for isomorphic graphs and differ for non-isomorphic graphs.
- **Tree Decomposition Valid**: Correctly identifies valid decompositions by checking vertex/edge coverage and running intersection.

## Generators

- **Erdős-Rényi G(n, p)**: Generates exactly $n$ nodes; with $p = 1$ yields a complete graph.
- **Erdős-Rényi G(n, m)**: Generates exactly $n$ nodes and at most $m$ edges.
- **Random Tree**: Generates exactly $n$ nodes and $n - 1$ edges.
- **Random Regular**: Generates $n$ nodes with the requested degree $d$ when feasible.
- **Barabási-Albert**: Generates exactly $n$ nodes via preferential attachment.
- **SBM / DCSBM / HSBM**: Generate the requested number of nodes and valid community assignments.
- **Reproducibility**: Random generators with the same seed produce identical graphs.

## Health / Distance Metrics

- **Path Diameter**: The diameter of $P_n$ is $n - 1$.
- **Complete Graph Diameter/Radius**: For $K_n$ ($n \ge 2$), diameter equals radius and both equal 1.
- **Regular Graph Assortativity**: $d$-regular graphs have assortativity 0.
- **Star Graph Assortativity**: Star graphs have negative assortativity.
- **Average Path Length**: For complete graphs $K_n$ ($n \ge 2$), average path length is 1.0.

## Disjoint Set

- **Reflexivity**: Every element is connected to itself.
- **Symmetry**: `connected x y == connected y x`.
- **Transitivity**: If `x` is connected to `y` and `y` to `z`, then `x` is connected to `z`.
- **Union Count**: Union of two distinct sets reduces the total set count by exactly 1.
- **Partitioning**: `toLists` partitions all elements into disjoint, internally connected sets.

## I/O Roundtrips

- **TGF**: Serialize-then-parse preserves node count and edge count.
- **Adjacency List**: Serialize-then-parse preserves node count and edge count.
- **Adjacency Matrix**: `toMatrix`/`fromMatrix` preserves node count and edge count for non-zero weights.
- **GraphML / GDF**: Round-trips preserve kind, order, and edge count for supported typed graphs.

## Pairing Heap (Internal Priority Queue)

- **Heap Sort**: Popping all elements returns a sorted list (min-heap or max-heap with custom comparison).
- **Peek/Pop Consistency**: `peek` returns the same value as the first popped element.
- **Size Invariant**: Size is maintained across random push/pop sequences.
- **Merge Invariant**: Merging two heaps preserves overall order and size.
