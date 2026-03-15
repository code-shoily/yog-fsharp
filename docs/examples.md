# Examples

Real-world examples demonstrating how to use Yog.FSharp for various graph problems.

## Running Examples

All examples are literate F# scripts that you can run directly:

```bash
# Run an example script
dotnet fsi docs/examples/gps-navigation.fsx
```

Or reference in your F# Interactive session:

```fsharp
#load "docs/examples/gps-navigation.fsx"
```

The examples use literate programming - documentation is embedded in the `.fsx` files using `(**` and `*)` comment blocks.

---

## Pathfinding & Traversal

| Example | Description | Demonstrates |
|:---|:---|:---|
| [GPS Navigation with A*](examples/gps-navigation.html) | Find the fastest route between locations using heuristic-based search. | A* algorithm, heuristics, route planning |
| [Dijkstra's Shortest Path](examples/dijkstra-shortest-path.html) | Find the fastest delivery route in a weighted road network. | Dijkstra's algorithm, single-source distances |
| [Bellman-Ford with Negative Weights](examples/bellman-ford.html) | Shortest paths with negative weights and negative cycle detection. | Bellman-Ford, negative cycle detection |
| [City Distance Matrix with Floyd-Warshall](examples/city-distance-matrix.html) | Compute shortest distances between all pairs of cities in a network. | All-pairs shortest paths, distance matrices |
| [Cave Path Counting](examples/cave-path-counting.html) | Count all valid paths through a cave system with complex visit rules. | Custom DFS, backtracking, state-space search |

## Graph Properties & Algorithms

| Example | Description | Demonstrates |
|:---|:---|:---|
| [The Seven Bridges of Königsberg](examples/bridges-of-konigsberg.html) | The classic problem that founded graph theory. | Eulerian paths/circuits, degree analysis |
| [Task Scheduling with Topological Sort](examples/task-scheduling.html) | Determine the correct execution order for tasks with dependencies. | Topological sorting on DAGs, cycle detection |
| [Deterministic Task Ordering](examples/task-ordering.html) | Alphabetically earliest execution order for tasks with multiple valid orderings. | Lexicographical topological sort, stable resolution |
| [Social Network Analysis with SCC](examples/social-network-analysis.html) | Find communities in a social network using strongly connected components. | Tarjan's & Kosaraju's SCC algorithms |
| [Network Cable Layout Optimization](examples/network-cable-layout.html) | Find the minimum cost to connect all buildings using Kruskal's algorithm. | Minimum Spanning Trees (MST), cost optimization |
| [Centrality Analysis](examples/centrality-analysis.html) | Identify the most important nodes using degree, closeness, betweenness, PageRank, and eigenvector centrality. | All 5 centrality measures |
| [Bridges & Articulation Points](examples/bridges-and-articulation.html) | Find critical infrastructure whose failure disconnects the network. | Tarjan's bridge-finding algorithm |
| [Clique Finding](examples/clique-finding.html) | Find tightly-knit groups using the Bron-Kerbosch algorithm. | Maximum clique, all maximal cliques, k-cliques |
| [DAG Algorithms](examples/dag-algorithms.html) | Critical path analysis and transitive closure on a task dependency DAG. | Longest path, transitive closure, topological sort |

## Flow & Matching

| Example | Description | Demonstrates |
|:---|:---|:---|
| [Job Matching with Maximum Flow](examples/job-matching.html) | Solve a bipartite matching problem using maximum flow algorithms. | Edmonds-Karp, bipartite matching, network modeling |
| [Network Bandwidth Allocation](examples/network-bandwidth.html) | Maximize throughput and identify bottlenecks in a computer network. | Edmonds-Karp, Max-Flow Min-Cut Theorem |
| [Global Minimum Cut](examples/global-min-cut.html) | Find the "natural" split point of a graph that disconnects it with minimum cost. | Stoer-Wagner algorithm, network reliability |
| [Medical Residency Matching](examples/medical-residency.html) | Solve the stable marriage problem to match residents to hospitals. | Gale-Shapley algorithm, stable matching |

## Classic Graph Generators

| Example | Description | Demonstrates |
|:---|:---|:---|
| [Complete Graphs](examples/complete-graphs.html) | Generate $K_n$ graphs where every node connects to every other. | `Classic.complete` |
| [Cycle Graphs](examples/cycle-graphs.html) | Generate $C_n$ graphs forming a single closed loop. | `Classic.cycle` |
| [Path Graphs](examples/path-graphs.html) | Generate $P_n$ graphs forming a linear chain. | `Classic.path` |
| [Star Graphs](examples/star-graphs.html) | Generate $S_n$ graphs with a central hub node. | `Classic.star` |
| [Wheel Graphs](examples/wheel-graphs.html) | Generate $W_n$ graphs by adding a hub to a cycle. | `Classic.wheel` |
| [2D Grid Graphs](examples/grid-graphs.html) | Generate rectangular lattice meshes. | `Classic.grid2D` |
| [Bipartite Graphs](examples/bipartite-graphs.html) | Generate complete bipartite graphs $K_{m,n}$. | `Classic.completeBipartite` |
| [Empty Graphs](examples/empty-graphs.html) | Generate $n$ isolated nodes with no edges. | `Classic.emptyGraph` |
| [Binary Trees](examples/binary-trees.html) | Generate complete binary trees of a given depth. | `Classic.binaryTree` |
| [The Petersen Graph](examples/petersen-graph.html) | Generate the famous 10-node, 15-edge cubic graph. | `Classic.petersenGraph` |

## Stochastic Network Models

| Example | Description | Demonstrates |
|:---|:---|:---|
| [Erdős-Rényi $G(n,p)$ Graphs](examples/erdos-renyi-graphs.html) | Random graphs where each edge exists with probability $p$. | `Random.erdosRenyiGnp` |
| [Erdős-Rényi $G(n,m)$ Graphs](examples/erdos-renyi-gnm-graphs.html) | Random graphs with exactly $m$ edges added uniformly. | `Random.erdosRenyiGnm` |
| [Barabási-Albert Networks](examples/barabasi-albert-networks.html) | Scale-free networks generated via preferential attachment. | `Random.barabasiAlbert` |
| [Watts-Strogatz Networks](examples/watts-strogatz-networks.html) | Small-world networks with high clustering and short paths. | `Random.wattsStrogatz` |
| [Random Spanning Trees](examples/random-trees.html) | Uniformly random tree structure on $n$ nodes. | `Random.randomTree` |

## IO & Serialization

| Example | Description | Demonstrates |
|:---|:---|:---|
| [DOT Graph Rendering](examples/render-dot.html) | Export graphs to Graphviz DOT format for professional visualization. | Yog.IO.Dot, visual customization, Graphviz |
| [JSON Data Export](examples/render-json.html) | Export graph data to JSON for web APIs and frontend visualization. | Yog.IO.Json, indented serialization, data interchange |
| [GraphML Serialization](examples/render-graphml.html) | Export and import graphs in GraphML format for Gephi, yEd, and Cytoscape. | Yog.IO.GraphML, round-trip, custom attributes |

Check the [GitHub repository](https://github.com/code-shoily/yog-fsharp) for the latest examples!
