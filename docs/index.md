# Yog.FSharp

```text
                    ♯
                   /|\
                  / | \
                 /  |  \
                Y   |   O--------G
               /    |    \      /
              /     |     \    /
             /      |      \  /
            যো------+-------গ
           / \      |      / \
          /   \     |     /   \
         /     \    |    /     \
        ✦       ✦   |   ✦       ✦

```

> **Note**: This is a work in progress and is being actively developed. API design is subject to change.

[![NuGet Version](https://img.shields.io/nuget/v/Yog.FSharp)](https://www.nuget.org/packages/Yog.FSharp/)
[![NuGet Downloads](https://img.shields.io/nuget/dt/Yog.FSharp)](https://www.nuget.org/packages/Yog.FSharp/)
[![Documentation](https://img.shields.io/badge/docs-online-blue)](https://code-shoily.github.io/yog-fsharp)

A comprehensive graph algorithm library for F#, providing functional APIs for graph construction, analysis, and visualization.

**[Full Documentation & API Reference](https://code-shoily.github.io/yog-fsharp)** | **[Elixir Version](https://github.com/code-shoily/yog_ex)** | **[Complete Algorithm Catalog](ALGORITHMS.md)** | **[Algorithmic Invariants Catalog](PROPERTIES.md)**

## Installation

```sh
dotnet add package Yog.FSharp
```

## Quick Start

```fsharp
open Yog.Model
open Yog.Pathfinding.Dijkstra

// Create a directed graph
let graph =
    empty Directed
    |> addNode 1 "Start"
    |> addNode 2 "Middle"
    |> addNode 3 "End"
    |> addEdge 1 2 5
    |> addEdge 2 3 3
    |> addEdge 1 3 10

// Find shortest path
match shortestPathInt 1 3 graph with
| Some path ->
    printfn "Found path with weight: %d" path.TotalWeight
    printfn "Path: %A" path.Nodes
| None ->
    printfn "No path found"
```

## Features

### Core Data Structures
- **Graph**: Directed and undirected graphs with generic node and edge data
- **MultiGraph**: Support for parallel edges between nodes
- **DAG**: Type-safe directed acyclic graphs with cycle prevention

### Pathfinding Algorithms
- **Dijkstra**: Single-source shortest path (non-negative weights)
- **A\***: Heuristic-guided shortest path search
- **Bellman-Ford**: Shortest path with negative weights and cycle detection
- **Floyd-Warshall**: All-pairs shortest paths
- **Implicit Variants**: State-space search without explicit graph construction

### Flow & Optimization
- **Edmonds-Karp**: Maximum flow algorithm
- **Stoer-Wagner**: Global minimum cut
- **Kruskal's MST**: Minimum spanning tree with Union-Find

### Graph Traversal
- **BFS & DFS**: Breadth-first and depth-first search
- **Early Termination**: Stop traversal when goal is found
- **Implicit Traversal**: Explore state spaces without building full graph

### Graph Properties & Analysis
- **Connectivity**: Bridge and articulation point detection, strongly connected components (Tarjan's and Kosaraju's)
- **Centrality**: Degree, betweenness, and closeness centrality measures
- **Cycles**: Cycle detection and analysis
- **Eulerian**: Eulerian path/circuit detection and finding (Hierholzer's)
- **Bipartite**: Bipartite detection, stable marriage (Gale-Shapley)
- **Matching**: Hopcroft-Karp maximum bipartite matching, Hungarian optimal assignment (min/max weight perfect matching), and Edmonds' Blossom maximum general matching
- **Cliques**: Maximum clique detection (Bron-Kerbosch)
- **Planarity**: Exact Left-Right planarity checks, combinatorial planar embedding, and Kuratowski witness ($K_5$/$K_{3,3}$ subdivision) identification
- **WL Hashing**: Weisfeiler-Lehman graph hashing for isomorphism and structure verification
- **Tree Decomposition**: Struct representation and validation (vertex coverage, edge coverage, running intersection)

### Graph Transformations
- **Transpose**: O(1) reverse of all edges
- **Map/Filter**: Functor operations on nodes and edges (`mapNodes`, `mapNodesIndexed`, `mapEdges`, `mapEdgesIndexed`, `filterNodes`, `filterEdges`)
- **Self Loops**: Add or remove self-loops (`addSelfLoops`, `removeSelfLoops`)
- **Relabeling**: Map node IDs using a function or normalize to `0..N-1` (`relabelNodes`, `normalizeNodeIds`)
- **Ego Graph**: Extract subgraphs of neighbors within a given radius (`egoGraph`)
- **Quotient Graph**: Condense a graph by grouping nodes into partition blocks (`quotientGraph`)
- **Contract**: Merge node $b$ into node $a$, combining edge weights (`contract`)
- **Subgraph**: Extract induced subgraphs by a subset of nodes


### Graph Operations
- **Set Operations**: Union, intersection, difference, symmetric difference, disjoint union
- **Graph Products**: Cartesian, tensor, strong, and lexicographic products
- **Composition**: Compose/merge graphs
- **Line Graph**: Convert edges to nodes
- **Graph Power**: Connect nodes within distance k
- **Structural Comparison**: Subgraph and isomorphism checks

### Graph Generators
**Classic Deterministic Graphs:**
- Complete (K_n), Cycle (C_n), Path (P_n), Star (S_n), Wheel (W_n)
- Grid2D, Binary Tree, Complete Bipartite, Petersen Graph, Empty Graph
- Hypercube (Q_n), Ladder, Circular Ladder (Prism), Möbius Ladder
- Friendship (F_n), Windmill, Book, Crown, Lollipop, Barbell, Turán
- Platonic Solids (Tetrahedron, Cube, Octahedron, Dodecahedron, Icosahedron)
- Tutte Graph, Sedgewick Maze

**Random Network Models:**
- Erdős-Rényi: G(n,p) and G(n,m) random graphs
- Barabási-Albert: Scale-free networks with preferential attachment
- Watts-Strogatz: Small-world networks
- Random Trees: Uniformly random spanning trees
- Random Regular, SBM, DCSBM, HSBM, Configuration Model
- Power Law Graph, Kronecker, R-MAT, Geometric, Waxman

**Maze Generators:**
- Binary Tree, Sidewinder, Recursive Backtracker (DFS), Hunt-and-Kill
- Aldous-Broder, Wilson's, Kruskal's, Simplified/True Prim's
- Eller's, Growing Tree, Recursive Division

### Graph Builders
- **Labeled Builder**: Use custom labels (strings, UUIDs) instead of integer IDs
- **Live Builder**: Interactive graph construction
- **Grid Builder**: Specialized builder for grid/lattice graphs

### Visualization & Export
- **DOT (Graphviz)**: Export for visualization with Graphviz tools
- **GraphML**: XML format for Gephi, yEd, Cytoscape, NetworkX
- **GDF**: Lightweight text format for Gephi and data interchange
- **JSON**: Data interchange and web applications
- **Mermaid**: Embed diagrams in markdown documents
- **TGF (Trivial Graph Format)**: Simple line-based format parser and serializer
- **Adjacency/Edge List**: Compact text-based lists parser and serializer
- **Adjacency Matrix**: Matrix representation parser and serializer

### Advanced Features
- **Disjoint Set (Union-Find)**: Path compression and union by rank
- **Topological Sorting**: Kahn's algorithm with lexicographical variant
- **DAG Algorithms**: Longest path, reachability analysis

## Algorithm Selection Guide

| Algorithm | Use When | Time Complexity |
| --------- | -------- | --------------- |
| **Dijkstra** | Non-negative weights, single shortest path | O((V+E) log V) |
| **A\*** | Non-negative weights + good heuristic | O((V+E) log V) |
| **Bellman-Ford** | Negative weights OR cycle detection needed | O(VE) |
| **Floyd-Warshall** | All-pairs shortest paths, distance matrices | O(V³) |
| **Edmonds-Karp** | Maximum flow, bipartite matching | O(VE²) |
| **BFS/DFS** | Unweighted graphs, exploring reachability | O(V+E) |
| **Kruskal's MST** | Finding minimum spanning tree | O(E log E) |
| **Stoer-Wagner** | Global minimum cut, graph partitioning | O(V³) |
| **Tarjan's SCC** | Finding strongly connected components | O(V+E) |
| **Hierholzer** | Eulerian paths/circuits, route planning | O(V+E) |
| **Topological Sort** | Ordering tasks with dependencies | O(V+E) |
| **Gale-Shapley** | Stable matching, college admissions | O(n²) |
| **Hopcroft-Karp** | Maximum bipartite matching | O(E√V) |
| **Hungarian** | Minimum/maximum weight bipartite matching | O(V³) |
| **Edmonds' Blossom** | Maximum general (non-bipartite) matching | O(V²E) |
| **Implicit Search** | Pathfinding/Traversal on on-demand graphs | O((V+E) log V) |
| **LR Planarity Test** | Determining if undirected graph is planar | O(V²) |
| **WL Hashing** | Generating graph signature to verify isomorphism | O(k(V+E)) |

## Usage Examples

### Building Graphs with Labels

```fsharp
open Yog.Builder

let socialNetwork =
    Labeled.undirected<string, int>()
    |> Labeled.addEdge "Alice" "Bob" 5
    |> Labeled.addEdge "Bob" "Charlie" 3
    |> Labeled.addEdge "Charlie" "Alice" 2
    |> Labeled.toGraph
```

### Generating Random Networks

```fsharp
open Yog.Generators

// Erdős-Rényi random graph
let randomGraph = Random.erdosRenyiGnp 100 0.05 Undirected

// Scale-free network (power-law distribution)
let scaleFree = Random.barabasiAlbert 1000 3 Undirected

// Small-world network
let smallWorld = Random.wattsStrogatz 100 6 0.1 Undirected
```

### Type-Safe DAG Operations

```fsharp
open Yog.Dag

// Create a DAG with cycle detection
let dag =
    Model.empty
    |> Model.addNode 1 "Task A"
    |> Model.addNode 2 "Task B"
    |> Model.addEdge 1 2 ()
    |> Result.get

// Topological sort always succeeds on DAGs
let sorted = Algorithms.topologicalSort dag
```

### Exporting for Visualization

```fsharp
open Yog.IO

// Export to Graphviz DOT
let dotOutput = Dot.render Dot.defaultOptions graph
File.WriteAllText("graph.dot", dotOutput)

// Export to GraphML for Gephi/yEd
let graphml = GraphML.serialize graph
File.WriteAllText("graph.graphml", graphml)

// Export to GDF for Gephi (lightweight text format)
let gdf = Gdf.serialize graph
File.WriteAllText("graph.gdf", gdf)

// Export to Mermaid for markdown
let mermaid = Mermaid.render Mermaid.defaultOptions graph
printfn "```mermaid\n%s\n```" mermaid

// Trivial Graph Format (TGF)
let tgfString = Tgf.serialize Tgf.defaultOptions graph

// Adjacency List
let adjList = List.serialize false " " graph
```

## Real-World Use Cases

- **GPS Navigation**: Shortest path routing with A* and heuristics
- **Network Analysis**: Social network centrality and community detection
- **Task Scheduling**: Topological sorting for dependency resolution
- **Flow Networks**: Maximum flow for network capacity planning
- **Matching Problems**: Stable marriage and bipartite matching
- **Circuit Design**: Eulerian path finding for circuit optimization
- **Bayesian Networks**: DAG operations for probabilistic inference

## Project Status

**Version:** 0.5.0 (Pre-release) - [Changelog](CHANGELOG.md)

This is an F# port of the [Gleam Yog](https://github.com/code-shoily/yog) library. While not a 1:1 port, it captures the spirit and functional API of the original while adding F#-specific enhancements.

**Stability:** The library is actively developed and APIs may change before 1.0. Feedback and contributions are welcome!

## Documentation

- [Full Documentation](https://code-shoily.github.io/yog-fsharp)
- [Getting Started Guide](https://code-shoily.github.io/yog-fsharp/tutorials/getting-started.html)
- [Examples](https://code-shoily.github.io/yog-fsharp/examples.html) - 37+ real-world examples
- [API Reference](https://code-shoily.github.io/yog-fsharp/reference/index.html)

## Contributing

Contributions are welcome! Please create an issue or a PR for any bugfix or feature request.

## License

MIT License - see [LICENSE](LICENSE) for details.

## AI Assistance

Parts of this project were developed with the assistance of AI coding tools. All AI-generated code has been reviewed, tested, and validated by the maintainer.

---

**Yog.FSharp** - Graph algorithms for F#
