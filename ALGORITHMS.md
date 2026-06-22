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
| Hopcroft-Karp | `Yog.Properties.Matching` | Maximum bipartite matching | O(E√V) | O(V+E) |
| Hungarian (Kuhn-Munkres) | `Yog.Properties.Matching` | Min/max weight perfect matching (bipartite) | O(V³) | O(V²) |
| Edmonds' Blossom | `Yog.Properties.Matching` | Maximum general matching (non-bipartite) | O(V²E) | O(V+E) |
| Maximum Matching | `Yog.Properties.Bipartite` | Naive maximum bipartite matching | O(VE) | O(V) |
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

## Random Graph Generation

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Erdős-Rényi (GNP) | `Yog.Generators.Random` | Fixed probability per edge | O(V²) | O(V+E) |
| Erdős-Rényi (GNM) | `Yog.Generators.Random` | Fixed number of edges | O(V²) | O(V+E) |
| Barabási-Albert | `Yog.Generators.Random` | Preferential attachment | O(VE) | O(V+E) |
| Watts-Strogatz | `Yog.Generators.Random` | Small-world networks | O(V²) | O(V+E) |
| Random Tree | `Yog.Generators.Random` | Uniform random tree | O(V) | O(V) |
| Random Regular | `Yog.Generators.Random` | Fixed-degree random graph | O(VD) | O(V+E) |
| SBM | `Yog.Generators.Random` | Stochastic Block Model | O(V²) | O(V+E) |
| DCSBM | `Yog.Generators.Random` | Degree-Corrected SBM | O(V²) | O(V+E) |
| HSBM | `Yog.Generators.Random` | Hierarchical SBM | O(V²) | O(V+E) |
| Configuration Model | `Yog.Generators.Random` | Given degree sequence | O(V+E) | O(V+E) |
| Power Law Graph | `Yog.Generators.Random` | Scale-free network | O(VE) | O(V+E) |
| Kronecker | `Yog.Generators.Random` | Recursive matrix product | O(V+E) | O(V+E) |
| R-MAT | `Yog.Generators.Random` | Recursive matrix model | O(E log V) | O(V+E) |
| Geometric | `Yog.Generators.Random` | Distance-threshold graph | O(V²) | O(V²) |
| Waxman | `Yog.Generators.Random` | Probabilistic distance graph | O(V²) | O(V²) |

## Classic Graph Generators

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Complete Graph | `Yog.Generators.Classic` | Kₙ generator | O(V²) | O(V²) |
| Cycle Graph | `Yog.Generators.Classic` | Cₙ generator | O(V) | O(V) |
| Path Graph | `Yog.Generators.Classic` | Pₙ generator | O(V) | O(V) |
| Star Graph | `Yog.Generators.Classic` | Sₙ generator | O(V) | O(V) |
| Wheel Graph | `Yog.Generators.Classic` | Wₙ generator | O(V) | O(V) |
| Complete Bipartite | `Yog.Generators.Classic` | Kₘ,ₙ generator | O(m·n) | O(m+n) |
| Binary Tree | `Yog.Generators.Classic` | Full binary tree | O(V) | O(V) |
| K-ary Tree | `Yog.Generators.Classic` | Full k-ary tree | O(V) | O(V) |
| Complete K-ary | `Yog.Generators.Classic` | Complete k-ary tree | O(V) | O(V) |
| Caterpillar | `Yog.Generators.Classic` | Spine with leaves | O(V) | O(V) |
| Grid 2D | `Yog.Generators.Classic` | Rectangular lattice | O(V) | O(V) |
| Petersen Graph | `Yog.Generators.Classic` | Famous 10-node graph | O(1) | O(1) |
| Empty Graph | `Yog.Generators.Classic` | N isolated nodes | O(V) | O(V) |
| Hypercube | `Yog.Generators.Classic` | Qₙ generator | O(V log V) | O(V log V) |
| Ladder | `Yog.Generators.Classic` | Ladder graph | O(V) | O(V) |
| Circular Ladder | `Yog.Generators.Classic` | Prism graph | O(V) | O(V) |
| Möbius Ladder | `Yog.Generators.Classic` | Möbius-Kantor variant | O(V) | O(V) |
| Friendship | `Yog.Generators.Classic` | Windmill Fₙ | O(V) | O(V) |
| Windmill | `Yog.Generators.Classic` | Generalized windmill | O(V) | O(V) |
| Book Graph | `Yog.Generators.Classic` | Stacked triangles | O(V) | O(V) |
| Crown Graph | `Yog.Generators.Classic` | Sₙ⁻ generator | O(V²) | O(V²) |
| Lollipop | `Yog.Generators.Classic` | Kₘ connected to Pₙ | O(m+n) | O(m+n) |
| Barbell | `Yog.Generators.Classic` | Two cliques + path | O(m+n) | O(m+n) |
| Turán Graph | `Yog.Generators.Classic` | T(n,r) extremal graph | O(V²) | O(V²) |
| Platonic Solids | `Yog.Generators.Classic` | Tetrahedron, Cube, Octahedron, Dodecahedron, icosahedron | O(1) | O(1) |
| Tutte Graph | `Yog.Generators.Classic` | Non-Hamiltonian polyhedral | O(1) | O(1) |
| Sedgewick Maze | `Yog.Generators.Classic` | Classic 8-node maze | O(1) | O(1) |

## Maze Generation

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Binary Tree | `Yog.Generators.Maze` | Simplest, fastest | O(N) | O(1) |
| Sidewinder | `Yog.Generators.Maze` | Vertical corridors | O(N) | O(cols) |
| Recursive Backtracker | `Yog.Generators.Maze` | Classic "roguelike" passages | O(N) | O(N) |
| Hunt-and-Kill | `Yog.Generators.Maze` | Organic, winding | O(N²) | O(1) |
| Aldous-Broder | `Yog.Generators.Maze` | Uniform spanning tree | O(N²) | O(N) |
| Wilson's | `Yog.Generators.Maze` | Efficient uniform tree | O(N) avg | O(N) |
| Kruskal's | `Yog.Generators.Maze` | Balanced, randomized | O(N log N) | O(N) |
| Prim's (Simplified) | `Yog.Generators.Maze` | Radial, many dead ends | O(N log N) | O(N) |
| Prim's (True) | `Yog.Generators.Maze` | True Prim maze | O(N log N) | O(N) |
| Eller's | `Yog.Generators.Maze` | Infinite height potential | O(N) | O(cols) |
| Recursive Division | `Yog.Generators.Maze` | Fractal, room-based | O(N log N) | O(log N) |
 
## Approximation Algorithms

| Algorithm | Module | Purpose | Time Complexity | Space Complexity |
|-----------|--------|---------|-----------------|------------------|
| Vertex Cover | `Yog.Approximate` | 2-approximation for minimum vertex cover | O(V+E) | O(V) |
| Maximum Clique | `Yog.Approximate` | Greedy maximum clique approximation | O(V²) | O(V) |
| Maximum Independent Set | `Yog.Approximate` | Greedy independent set approximation | O(V²) | O(V) |
| TSP (Metric) | `Yog.Approximate` | 2-approximation for Metric Traveling Salesperson | O(V²) | O(V²) |
| Graph Coloring | `Yog.Approximate` | Greedy graph coloring approximation | O(V log V + E) | O(V) |
