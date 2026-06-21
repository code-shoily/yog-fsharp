# Algorithm Catalog

Complete reference of all algorithms implemented in Yog.FSharp, organized by category.

## Pathfinding

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Dijkstra | `Yog.Pathfinding.Dijkstra` | Single-source shortest path (non-negative weights) | O((V+E) log V) | O(V) |
| A* | `Yog.Pathfinding.AStar` | Heuristic-guided shortest path | O((V+E) log V) | O(V) |
| Bellman-Ford | `Yog.Pathfinding.BellmanFord` | Shortest path with negative weights, cycle detection | O(VE) | O(V) |
| Floyd-Warshall | `Yog.Pathfinding.FloydWarshall` | All-pairs shortest paths | O(V³) | O(V²) |
| Johnson's | `Yog.Pathfinding.Johnson` | All-pairs shortest paths in sparse graphs | O(V² log V + VE) | O(V²) |
| Bidirectional Dijkstra | `Yog.Pathfinding.Bidirectional` | Faster single-pair shortest path | O((V+E) log V) | O(V) |
| Bidirectional BFS | `Yog.Pathfinding.Bidirectional` | Unweighted shortest path | O(V+E) | O(V) |
| Yen's K-Shortest | `Yog.Pathfinding.Yen` | k shortest loopless paths | O(k·N·(E+V log V)) | O(kV) |
| Widest Path | `Yog.Pathfinding` | Maximum bottleneck capacity path | O((V+E) log V) | O(V) |
| Chinese Postman | `Yog.Pathfinding.ChinesePostman` | Shortest route visiting every edge | O(V³) | O(V²) |
| LCA (Binary Lifting) | `Yog.Pathfinding.Lca` | Lowest common ancestor in trees | O(V log V) preprocess, O(log V) query | O(V log V) |
| Distance Matrix | `Yog.Pathfinding.DistanceMatrix` | Matrix-based distance operations | O(V²) | O(V²) |

## Flow & Cuts

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Edmonds-Karp | `Yog.Flow.MaxFlow` | Maximum flow (BFS augmenting paths) | O(VE²) | O(V+E) |
| Dinic's | `Yog.Flow.MaxFlow` | Maximum flow (blocking flow) | O(V²E) | O(V+E) |
| Stoer-Wagner | `Yog.Flow.MinCut` | Global minimum cut | O(V³) | O(V²) |

## Spanning Tree

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Kruskal's | `Yog.Mst` | MST via edge sorting | O(E log E) | O(V) |
| Prim's | `Yog.Mst` | MST via vertex growing | O(E log V) | O(V) |
| Borůvka's | `Yog.Mst` | Parallel MST | O(E log V) | O(V) |

## Matching

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Maximum Matching | `Yog.Properties.Bipartite` | Maximum bipartite matching | O(VE) | O(V) |
| Gale-Shapley | `Yog.Properties.Bipartite` | Stable marriage matching | O(V²) | O(V) |

## Connectivity & Components

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Tarjan's SCC | `Yog.Connectivity` | Strongly connected components | O(V+E) | O(V) |
| Kosaraju's SCC | `Yog.Connectivity` | Strongly connected components (two-pass) | O(V+E) | O(V) |
| Connected Components | `Yog.Connectivity` | Undirected connected components | O(V+E) | O(V) |
| Weakly Connected Components | `Yog.Connectivity` | Directed graph components (ignore direction) | O(V+E) | O(V) |
| Tarjan's Bridges / Articulation | `Yog.Connectivity` | Bridge edges and articulation points | O(V+E) | O(V) |

## Centrality Measures

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Degree Centrality | `Yog.Centrality` | Simple connectivity importance | O(V+E) | O(V) |
| Closeness Centrality | `Yog.Centrality` | Distance-based importance | O(VE + V² log V) | O(V) |
| Harmonic Centrality | `Yog.Centrality` | Distance-based (handles infinite) | O(VE + V² log V) | O(V) |
| Betweenness Centrality | `Yog.Centrality` | Bridge/gatekeeper detection | O(VE) | O(V²) |
| PageRank | `Yog.Centrality` | Link-quality importance | O(k(V+E)) | O(V) |
| Eigenvector Centrality | `Yog.Centrality` | Influence from neighbors | O(k(V+E)) | O(V) |
| Katz Centrality | `Yog.Centrality` | Attenuated influence propagation | O(k(V+E)) | O(V) |
| Alpha Centrality | `Yog.Centrality` | External influence model | O(k(V+E)) | O(V) |

## Structural Checks

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Tree Check | `Yog.Properties.Structure` | Verify if graph is a tree | O(V+E) | O(V) |
| Arborescence Check | `Yog.Properties.Structure` | Verify directed tree with root | O(V+E) | O(V) |
| Forest Check | `Yog.Properties.Structure` | Verify disjoint trees | O(V+E) | O(V) |
| Branching Check | `Yog.Properties.Structure` | Verify directed forest | O(V+E) | O(V) |
| Complete Graph Check | `Yog.Properties.Structure` | Verify every node is connected | O(V) | O(1) |
| Regular Graph Check | `Yog.Properties.Structure` | Verify k-regularity | O(V) | O(1) |
| Chordal Check | `Yog.Properties.Structure` | Verify Perfect Elimination Ordering | O(V+E) | O(V) |
| Cycle / Acyclic Checks | `Yog.Properties.Cyclicity` | Verify presence of cycles | O(V+E) | O(V) |
| Eulerian Path / Circuit | `Yog.Properties.Eulerian` | Eulerian path/circuit detection & Hierholzer | O(V+E) | O(V) |
| Clique (Bron-Kerbosch) | `Yog.Properties.Clique` | Finding maximal cliques | O(3^(V/3)) | O(V²) |
| Graph Coloring | `Yog.Properties.Coloring` | Greedy (Welsh-Powell) and exact coloring | O(V log V + E) | O(V) |
| LR Planarity Test | `Yog.Properties.Planarity` | Exact planarity check and combinatorial embedding | O(V²) | O(V) |
| Tree Decomposition | `Yog.Properties.TreeDecomposition` | Struct validation of tree decompositions | O(V+E) | O(V) |
| Weisfeiler-Lehman | `Yog.Properties.WeisfeilerLehman` | Graph hashing for isomorphism verification | O(k(V+E)) | O(V) |
