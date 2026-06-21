# Algorithmic Invariants Catalog

This document lists the algorithmic invariants (hypotheses) verified by `Yog.FSharp`'s testing suite. We use property-based testing and extensive unit test cases to ensure these properties hold across all possible edge cases, including sparse, dense, disconnected, and cyclic graphs.

**Location**: Tests are located in `tests/Yog.FSharp.Tests/`.

## Connectivity and Components

- **SCC Partitioning**: Strongly Connected Components must partition the set of all nodes exactly. Every node belongs to one and only one SCC.
- **MST Algorithm Agreement**: Kruskal, Prim, and Borůvka algorithms must produce the same total weight for connected undirected graphs.

## Pathfinding and Flow

- **Dijkstra-BFS Agreement**: Dijkstra agrees with BFS on the shortest path distance for unweighted graphs.
- **Algorithm Consistency**: Dijkstra, Bellman-Ford, and A* (with zero heuristic) must agree on shortest path weights for non-negative weights.
- **Negative Cycle Detection**: Bellman-Ford correctly identifies graphs containing reachable negative cycles.
- **Bidirectional Correctness**: Bidirectional Dijkstra yields the same path distance as standard Dijkstra.
- **Floyd-Warshall Agreement**: Floyd-Warshall agrees with repeated Dijkstra runs for all-pairs shortest paths.
- **Max-Flow Min-Cut Theorem**: The maximum flow value equals the minimum cut capacity.
- **Flow Conservation**: At every node except source and sink, total in-flow equals total out-flow.

## Centrality Measures

- **Star Graph Centrality**: In a star graph, the center node has strictly higher centrality (Degree, Closeness, Betweenness, PageRank, Eigenvector) than any leaf node.
- **PageRank Unity**: The sum of PageRank scores across all nodes equals exactly 1.0.

## Matching

- **Bipartite Matching Partition**: Maximum matching edges only connect vertices between left and right partitions.
- **Hopcroft-Karp Bipartite Matching**: Maximum matching size matches naive augmenting paths algorithm.
- **Hungarian Cost Optimality**: Hungarian algorithm produces a perfect matching with optimal (min or max) total cost.
- **Edmonds' Blossom Alternating Paths**: Blossom maximum matching finds the absolute maximum cardinality matching on general graphs, resolving odd cycles via contraction.

## Graph Operations & Transformations

- **Transpose Involutivity**: `transpose (transpose G) == G`.
- **Map-Nodes Identity**: `mapNodes id G == G`.
- **Map-Nodes Topology Preservation**: Node and edge counts are preserved under node mapping.
- **Map-Edges Identity**: `mapEdges id G == G`.
- **Filter-Nodes Consistency**: Filtering removes associated edges and preserves remaining structure.
- **Complement Invariants**: Complement of complement (minus self-loops) equals original.
- **Union**: Union contains all nodes and edges from both graphs.
- **Intersection Idempotence**: Intersection of a graph with itself equals the graph.
- **Difference Annihilation**: Difference of a graph with itself is empty.
- **Isomorphism Reflexivity**: A graph is isomorphic to itself.
- **Power Identity**: Power of a graph to 1 equals the graph.
- **Line Graph Node Count**: Equals edge count of original graph.

## Traversal

- **BFS Uniqueness**: BFS visits each reachable node exactly once.
- **DFS-BFS Node Agreement**: DFS and BFS visit the same set of reachable nodes.
- **Cyclicity Consistency**: A graph is either cyclic or acyclic (exclusive or).
- **Topological Order**: For any edge $(u,v)$ in a DAG, $u$ appears before $v$ in topological sort.

## Structural Properties

- **Tree Characterization**: Trees are connected, acyclic, and have exactly $V-1$ edges.
- **Arborescence Validity**: Directed trees have a unique root with in-degree 0, others in-degree 1.
- **Complete Graph**: $K_n$ is complete and $(n-1)$-regular.
- **Planarity Consistency**: K5 and K3,3 are correctly classified as non-planar, while grids are planar. Kuratowski witnesses accurately isolate $K_5$ or $K_{3,3}$ subdivisions.
- **WL Hashing Isomorphism**: WL signatures match for isomorphic graphs and differ for non-isomorphic graphs.
- **Tree Decomposition Valid**: Correctly identifies valid decompositions by checking vertex/edge coverage and running intersection.
