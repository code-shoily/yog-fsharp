# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- GDF (GUESS Graph Format) export support in `Yog.IO.Gdf` module
- `render-gdf.fsx` example demonstrating GDF serialization
- Enhanced README with comprehensive feature documentation
- Documentation for Graph Generators (Classic and Random)
- Documentation for Graph Builders (Labeled, Live, Grid)
- Documentation for Visualization & Export formats
- Comprehensive test suite for Network Simplex algorithm verification
- **Gleam vs F# comparison document** - Detailed side-by-side feature comparison (docs/GLEAM_FSHARP_COMPARISON.md)
- `addEdgeEnsuredWith` function for dynamically creating missing endpoints using a callback

### Changed
- Improved README structure with usage examples and real-world use cases
- Updated documentation to accurately reflect all implemented features
- Enhanced DOCS.md with complete example listing
- **Network Simplex algorithm marked as EXPERIMENTAL** due to incomplete pivot implementation
- `addEdge` and `addEdgeWithCombine` now throw an exception if source or destination nodes do not exist instead of creating "ghost nodes"
- `addEdgeEnsured` now takes separate default values for source and destination nodes

### Fixed
- Fixed documentation deployment URLs with proper trailing slash handling

### Known Issues
- **Network Simplex**: Pivot loop implementation is incomplete, causing false `Infeasible` results
  for valid minimum cost flow problems. The algorithm skeleton is present but core pivot logic
  is missing. See `NETWORK_SIMPLEX_ISSUES.md` for details. Alternative: Use successive shortest
  paths or wait for complete implementation in future release.

## [0.5.0] - 2025-03-15

### Added
- **Core Data Structures**
  - `Graph<'n, 'e>` - Directed and undirected graphs with generic node and edge data
  - `MultiGraph<'n, 'e>` - Support for parallel edges between nodes
  - `Dag<'n, 'e>` - Type-safe directed acyclic graphs with cycle prevention

- **Pathfinding Algorithms**
  - Dijkstra's algorithm for single-source shortest paths
  - A* algorithm with heuristic-guided search
  - Bellman-Ford algorithm with negative weight support
  - Floyd-Warshall algorithm for all-pairs shortest paths
  - Distance matrix utilities
  - Implicit pathfinding variants for state-space search

- **Flow & Optimization**
  - Edmonds-Karp maximum flow algorithm
  - Stoer-Wagner global minimum cut
  - Network Simplex minimum cost flow optimization
  - Kruskal's minimum spanning tree with Union-Find

- **Graph Traversal**
  - Breadth-first search (BFS)
  - Depth-first search (DFS)
  - Early termination support
  - Implicit traversal for on-demand graph exploration

- **Graph Properties & Analysis**
  - Connectivity analysis (bridges, articulation points)
  - Strongly connected components (Tarjan's and Kosaraju's algorithms)
  - Centrality measures (degree, betweenness, closeness)
  - Cycle detection
  - Eulerian path/circuit detection (Hierholzer's algorithm)
  - Bipartite graph detection and maximum matching
  - Stable marriage problem (Gale-Shapley algorithm)
  - Maximum clique detection (Bron-Kerbosch algorithm)

- **Graph Transformations**
  - O(1) transpose operation
  - Map/filter operations for nodes and edges
  - Subgraph extraction

- **Graph Generators**
  - **Classic Generators**: Complete, Cycle, Path, Star, Wheel, Grid2D, Binary Tree, Complete Bipartite, Petersen Graph, Empty Graph
  - **Random Generators**: Erdős-Rényi (G(n,p) and G(n,m)), Barabási-Albert, Watts-Strogatz, Random Trees

- **Graph Builders**
  - Labeled builder for custom node labels
  - Live builder for interactive construction
  - Grid builder for lattice graphs

- **Visualization & Export**
  - DOT (Graphviz) format export
  - GraphML serialization and deserialization
  - JSON export
  - Mermaid diagram generation

- **Advanced Features**
  - Disjoint Set (Union-Find) with path compression
  - Topological sorting (Kahn's algorithm)
  - DAG-specific algorithms (longest path, transitive closure)

- **Documentation**
  - 37+ comprehensive examples covering all major features
  - API documentation generated with FSDocs
  - GitHub Pages deployment at https://code-shoily.github.io/yog-fsharp
  - Getting started guide and tutorials

### Infrastructure
- .NET 10.0 target framework
- GitHub Actions CI/CD pipeline
- Automated documentation deployment
- NuGet package publishing
- SourceLink support for debugging

## [0.1.0] - 2025-03-09

### Added
- Initial project structure
- Basic graph data structure
- Core pathfinding algorithms
- Initial test suite

---

## Release Notes

### Version 0.5.0

This is the first public release of Yog.FSharp, an F# port of the Gleam Yog library. While not a 1:1 port, it captures the spirit and functional API of the original while adding F#-specific enhancements.

**Highlights:**
- Comprehensive graph algorithm library with 20+ algorithms
- Multiple pathfinding algorithms including A* and implicit search
- Flow algorithms and network optimization
- Rich graph generation capabilities (14+ generators)
- Multiple export formats for visualization
- Type-safe DAG wrapper preventing cycles
- 37+ documented examples

**What's Next:**
- API stabilization for 1.0.0 release
- Performance optimizations
- Additional algorithms based on user feedback
- Expanded test coverage

---

[Unreleased]: https://github.com/code-shoily/yog-fsharp/compare/v0.5.0...HEAD
[0.5.0]: https://github.com/code-shoily/yog-fsharp/releases/tag/v0.5.0
[0.1.0]: https://github.com/code-shoily/yog-fsharp/commits/main
