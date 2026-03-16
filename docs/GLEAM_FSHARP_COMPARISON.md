# Yog: Gleam vs F# Implementation Comparison

This document compares the Gleam and F# implementations of the Yog graph algorithm library.

## Quick Summary

| Aspect | Gleam | F# |
|--------|-------|-----|
| **Repository** | [code-shoily/yog](https://github.com/code-shoily/yog) | [code-shoily/yog-fsharp](https://github.com/code-shoily/yog-fsharp) |
| **Language** | Gleam (BEAM/Erlang VM) | F# (.NET) |
| **Package** | [hex.pm/packages/yog](https://hex.pm/packages/yog) | [nuget.org/packages/Yog.FSharp](https://www.nuget.org/packages/Yog.FSharp/) |
| **Documentation** | [HexDocs](https://hexdocs.pm/yog/) | [GitHub Pages](https://code-shoily.github.io/yog-fsharp) |
| **Status** | Stable, Production Ready | 0.5.0 Pre-release |
| **Total Algorithms** | 50+ | 50+ |
| **Lines of Code** | ~10,000 | ~8,500 |

## Core Data Structures

| Feature | Gleam | F# | Notes |
|---------|-------|-----|-------|
| **Graph<'n, 'e>** | ✅ | ✅ | Directed/Undirected with generic node/edge data |
| **MultiGraph** | ✅ | ✅ | Parallel edges between nodes |
| **DAG (Directed Acyclic Graph)** | ✅ | ✅ | Type-safe wrapper with cycle prevention |
| **Disjoint Set (Union-Find)** | ✅ | ✅ | Path compression and union by rank |

## Pathfinding Algorithms

| Algorithm | Gleam | F# | Complexity |
|-----------|-------|-----|------------|
| **Dijkstra** | ✅ | ✅ | O((V+E) log V) |
| **A\*** | ✅ | ✅ | O((V+E) log V) |
| **Bellman-Ford** | ✅ | ✅ | O(VE) |
| **Floyd-Warshall** | ✅ | ✅ | O(V³) |
| **Distance Matrix** | ✅ | ✅ | All-pairs distances |
| **Implicit Pathfinding** | ✅ | ✅ | State-space search |

**Status**: ✅ Feature parity - All algorithms present in both

## Graph Traversal

| Algorithm | Gleam | F# | Notes |
|-----------|-------|-----|-------|
| **BFS** | ✅ | ✅ | Breadth-first search |
| **DFS** | ✅ | ✅ | Depth-first search |
| **Early Termination** | ✅ | ✅ | Stop on goal found |
| **Implicit Traversal** | ✅ | ✅ | On-demand graph exploration |
| **Topological Sort** | ✅ | ✅ | Kahn's algorithm |
| **Lexicographical Topo Sort** | ✅ | ✅ | Stable ordering |

**Status**: ✅ Feature parity

## Flow & Optimization

| Algorithm | Gleam | F# | Status |
|-----------|-------|-----|--------|
| **Edmonds-Karp** (Max Flow) | ✅ | ✅ | Both fully functional |
| **Min Cut from Max Flow** | ✅ | ✅ | Both fully functional |
| **Stoer-Wagner** (Global Min Cut) | ✅ | ✅ | Both fully functional |
| **Network Simplex** (Min Cost Flow) | ✅ ✅ | ⚠️ ❌ | **Gleam: Complete (930 LOC)**, **F#: Incomplete (349 LOC)** |

**Status**: ⚠️ **F# Network Simplex is incomplete** - pivot logic missing

### Network Simplex Details

| Component | Gleam | F# |
|-----------|-------|-----|
| Initial state setup | ✅ | ✅ |
| Demand validation | ✅ | ✅ |
| Entering edge selection | ✅ | ✅ |
| **find_cycle** | ✅ | ❌ |
| **find_leaving_edge** | ✅ | ❌ |
| **augment_flow** | ✅ | ❌ |
| **Tree updates** | ✅ | ❌ |
| **Potential updates** | ✅ | ❌ |
| Tests pass | ✅ | ❌ |

## Graph Properties & Analysis

| Feature | Gleam | F# | Notes |
|---------|-------|-----|-------|
| **Connectivity** | ✅ | ✅ | |
| - Bridges | ✅ | ✅ | Tarjan's algorithm |
| - Articulation Points | ✅ | ✅ | Tarjan's algorithm |
| - Strong Components (SCC) | ✅ | ✅ | Tarjan's & Kosaraju's |
| **Cyclicity** | ✅ | ✅ | Cycle detection |
| **Eulerian Paths/Circuits** | ✅ | ✅ | Hierholzer's algorithm |
| **Bipartite Graphs** | ✅ | ✅ | Detection & max matching |
| **Stable Marriage** | ✅ | ✅ | Gale-Shapley algorithm |
| **Cliques** | ✅ | ✅ | Bron-Kerbosch algorithm |

**Status**: ✅ Feature parity

## Centrality Measures

| Measure | Gleam | F# | Notes |
|---------|-------|-----|-------|
| **Degree Centrality** | ✅ | ✅ | |
| **Betweenness Centrality** | ✅ | ✅ | Int & Float variants |
| **Closeness Centrality** | ✅ | ✅ | Int & Float variants |
| **Harmonic Centrality** | ✅ | ✅ | Int & Float variants |
| **PageRank** | ✅ | ✅ | Iterative algorithm |
| **Eigenvector Centrality** | ✅ | ✅ | Power iteration |
| **Katz Centrality** | ✅ | ✅ | |
| **Alpha Centrality** | ✅ | ✅ | |

**Status**: ✅ Feature parity - All 8 centrality measures in both

## Minimum Spanning Trees

| Algorithm | Gleam | F# | Notes |
|-----------|-------|-----|-------|
| **Kruskal's MST** | ✅ | ✅ | O(E log E) |
| **Prim's MST** | ✅ | ✅ | O(E log V) |

**Status**: ✅ Feature parity

## Graph Generators

### Classic Deterministic Graphs

| Generator | Gleam | F# | Description |
|-----------|-------|-----|-------------|
| **Complete (K_n)** | ✅ | ✅ | Every node connected |
| **Cycle (C_n)** | ✅ | ✅ | Ring structure |
| **Path (P_n)** | ✅ | ✅ | Linear chain |
| **Star (S_n)** | ✅ | ✅ | Hub with spokes |
| **Wheel (W_n)** | ✅ | ✅ | Cycle + center hub |
| **Grid 2D** | ✅ | ✅ | Rectangular lattice |
| **Binary Tree** | ✅ | ✅ | Complete binary tree |
| **Complete Bipartite** | ✅ | ✅ | K_{m,n} |
| **Petersen Graph** | ✅ | ✅ | Famous 10-node graph |
| **Empty Graph** | ✅ | ✅ | Isolated nodes |

**Status**: ✅ Feature parity - All 10 classic generators

### Random Network Models

| Generator | Gleam | F# | Description |
|-----------|-------|-----|-------------|
| **Erdős-Rényi G(n,p)** | ✅ | ✅ | Edge probability p |
| **Erdős-Rényi G(n,m)** | ✅ | ✅ | Exactly m edges |
| **Barabási-Albert** | ✅ | ✅ | Scale-free networks |
| **Watts-Strogatz** | ✅ | ✅ | Small-world networks |
| **Random Trees** | ✅ | ✅ | Uniformly random spanning trees |

**Status**: ✅ Feature parity - All 5 random generators

## Graph Builders

| Builder | Gleam | F# | Use Case |
|---------|-------|-----|----------|
| **Labeled Builder** | ✅ | ✅ | Use custom labels instead of IDs |
| **Live Builder** | ✅ | ✅ | Interactive construction |
| **Grid Builder** | ✅ | ✅ | Lattice/grid graphs |

**Status**: ✅ Feature parity

## Graph Transformations

| Operation | Gleam | F# | Notes |
|-----------|-------|-----|-------|
| **Transpose** | ✅ | ✅ | O(1) edge reversal |
| **Map Nodes** | ✅ | ✅ | Transform node data |
| **Map Edges** | ✅ | ✅ | Transform edge data |
| **Filter Nodes** | ✅ | ✅ | Remove nodes by predicate |
| **Filter Edges** | ✅ | ✅ | Remove edges by predicate |
| **Subgraph** | ✅ | ✅ | Extract by node set |
| **Merge** | ✅ | ✅ | Combine graphs |
| **Contract Edges** | ✅ | ❌ | **Gleam only** |

**Status**: ⚠️ F# missing edge contraction

## Visualization & I/O

| Format | Gleam | F# | Purpose |
|--------|-------|-----|---------|
| **DOT (Graphviz)** | ✅ | ✅ | Professional visualization |
| **JSON** | ✅ | ✅ | Web APIs, data interchange |
| **Mermaid** | ✅ | ✅ | Markdown diagrams |
| **GraphML** | ❌ | ✅ | **F# only** - Gephi, yEd, Cytoscape |
| **GDF** | ❌ | ✅ | **F# only** - Gephi lightweight format |

**Status**: ⚠️ F# has 2 additional export formats (GraphML, GDF)

## DAG-Specific Algorithms

| Feature | Gleam | F# | Notes |
|---------|-------|-----|-------|
| **Type-safe DAG wrapper** | ✅ | ✅ | Prevents cycles at compile time |
| **Longest Path** | ✅ | ✅ | Critical path analysis |
| **Topological Sort** | ✅ | ✅ | Guaranteed success on DAG |
| **Transitive Closure** | ✅ | ✅ | Reachability matrix |
| **Transitive Reduction** | ✅ | ✅ | Minimal equivalent DAG |

**Status**: ✅ Feature parity

## MultiGraph Support

| Feature | Gleam | F# | Notes |
|---------|-------|-----|-------|
| **Parallel Edges** | ✅ | ✅ | Multiple edges between nodes |
| **Edge IDs** | ✅ | ✅ | Unique identification |
| **Eulerian for MultiGraphs** | ✅ | ✅ | Specialized implementation |
| **MultiGraph Traversal** | ✅ | ✅ | BFS/DFS with edge IDs |

**Status**: ✅ Feature parity

## Performance Optimizations

| Feature | Gleam | F# |
|---------|-------|-----|
| **Pairing Heap (Priority Queue)** | ✅ | ❌ |
| **Two-List Queue (BFS)** | ✅ | ❌ |
| **Mutable Arrays for Hot Paths** | Limited | ✅ |
| **O(1) Transpose** | ✅ | ✅ |

## Testing & Quality

| Aspect | Gleam | F# |
|--------|-------|-----|
| **Unit Tests** | ✅ Extensive | ✅ Extensive |
| **Property-Based Tests** | ✅ qcheck | ✅ FsCheck |
| **Example Count** | 25+ | 37+ |
| **Documentation Coverage** | ✅ Complete | ✅ Complete |
| **CI/CD** | ✅ GitHub Actions | ✅ GitHub Actions |

## Platform & Ecosystem

| Aspect | Gleam | F# |
|--------|-------|-----|
| **Runtime** | BEAM/Erlang VM | .NET CLR |
| **Target Platforms** | Erlang, JavaScript | Windows, Linux, macOS |
| **Concurrency Model** | Actor model (OTP) | async/await, Tasks |
| **Package Manager** | Hex | NuGet |
| **Interactive** | Gleam REPL | F# Interactive (FSI) |

## Unique Features

### Gleam Only
- ✅ **Complete Network Simplex** - Full min cost flow implementation
- ✅ **Edge Contraction** - Graph transformation
- ✅ **Pairing Heap** - Custom priority queue
- ✅ **Two-List Queue** - Optimized BFS queue

### F# Only
- ✅ **GraphML Export/Import** - XML graph format
- ✅ **GDF Export** - Gephi lightweight format
- ✅ **More Examples** (37 vs 25)
- ✅ **.NET Integration** - Seamless with C#/VB.NET

## Migration Guide

### Gleam → F#
**Mostly straightforward**, but watch for:
- ⚠️ Network Simplex is incomplete in F# (use Gleam or wait for update)
- ✅ All other algorithms are functionally equivalent
- ✅ F# has additional export formats (GraphML, GDF)

### F# → Gleam
**Easy migration**, but note:
- ❌ No GraphML/GDF support in Gleam
- ✅ Network Simplex works correctly in Gleam
- ✅ All core algorithms present

## Version History

| Version | Gleam | F# |
|---------|-------|-----|
| **Latest** | 0.6.0 | 0.5.0 |
| **First Release** | 2024 | 2025 |
| **Stability** | Stable | Pre-release |

## Recommendations

### Choose Gleam If:
- ✅ You need **min cost flow** (Network Simplex) now
- ✅ Building BEAM/Erlang applications
- ✅ Want battle-tested, production-ready code
- ✅ Prefer functional programming on Erlang VM

### Choose F# If:
- ✅ Working in **.NET ecosystem**
- ✅ Need **GraphML/GDF** export formats
- ✅ Want seamless C# interop
- ✅ Can wait for Network Simplex completion or use alternatives
- ✅ Prefer statically typed .NET with excellent tooling

## Future Roadmap

### F# Planned
- [ ] Complete Network Simplex implementation
- [ ] Edge contraction transformation
- [ ] Performance benchmarks vs Gleam
- [ ] More random graph models

### Both
- [ ] Additional centrality measures
- [ ] Graph isomorphism detection
- [ ] Community detection algorithms
- [ ] Graph coloring algorithms

## Contributing

Both implementations welcome contributions!

- **Gleam**: [github.com/code-shoily/yog](https://github.com/code-shoily/yog)
- **F#**: [github.com/code-shoily/yog-fsharp](https://github.com/code-shoily/yog-fsharp)

## Summary

Both implementations are **high-quality, feature-rich graph libraries** with excellent documentation and test coverage. The choice between them primarily depends on your platform (.NET vs BEAM) rather than algorithmic capabilities, with the notable exception of Network Simplex being incomplete in F#.

**Algorithm Coverage**: ~98% feature parity
**Quality**: Both production-ready (except F# Network Simplex)
**Documentation**: Excellent in both
**Community**: Active maintenance in both

---

**Last Updated**: March 2025
**Gleam Version**: 0.6.0
**F# Version**: 0.5.0
