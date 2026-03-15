# Yog.FSharp

```text
                    ★
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

[![NuGet Version](https://img.shields.io/nuget/v/Yog.FSharp)](https://www.nuget.org/packages/Yog.FSharp/)
[![NuGet Downloads](https://img.shields.io/nuget/dt/Yog.FSharp)](https://www.nuget.org/packages/Yog.FSharp/)

A graph algorithm library for F#, providing implementations of classic graph algorithms with a functional API.

## Why Yog?

In many Indic languages, **Yog** (pronounced like "yoke") translates to "Union," "Addition," or "Connection." It stems from the ancient root *yuj*, meaning to join or to fasten together.

In the world of computer science, this is the literal definition of Graph Theory. A graph is nothing more than the union of independent points through purposeful connections.

## Features

- **Graph Data Structures**: Directed and undirected graphs with generic node and edge data
- **Pathfinding Algorithms**: Dijkstra, A*, Bellman-Ford, Floyd-Warshall, and **Implicit Variants** (state-space search)
- **Maximum Flow**: Edmonds-Karp algorithm
- **Graph Traversal**: BFS and DFS with early termination and **Implicit Variants**
- **Graph Transformations**: Transpose (O(1)!), map, filter, subgraph extraction
- **Minimum Spanning Tree**: Kruskal's algorithm with Union-Find
- **Minimum Cut**: Stoer-Wagner algorithm for global min-cut
- **Topological Sorting**: Kahn's algorithm with lexicographical variant
- **Strongly Connected Components**: Tarjan's and Kosaraju's algorithms
- **Maximum Clique**: Bron-Kerbosch algorithm for maximal cliques
- **Connectivity**: Bridge and articulation point detection
- **Eulerian Paths & Circuits**: Detection and finding using Hierholzer's algorithm
- **Bipartite Graphs**: Detection, maximum matching, and stable marriage (Gale-Shapley)
- **Minimum Cost Flow (MCF)**: Global optimization using the **Network Simplex** algorithm
- **Disjoint Set (Union-Find)**: With path compression and union by rank
- **Graph Centrality**: Degree, betweenness, and closeness centrality measures

## Installation

Add Yog.FSharp to your project:

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

## Algorithm Selection Guide

| Algorithm | Use When | Time Complexity |
| --------- | -------- | --------------- |
| **Dijkstra** | Non-negative weights, single shortest path | O((V+E) log V) |
| **A*** | Non-negative weights + good heuristic | O((V+E) log V) |
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
| **Network Simplex** | Global minimum cost flow optimization | O(E) pivots |
| **Implicit Search** | Pathfinding/Traversal on on-demand graphs | O((V+E) log V) |

## Project Status

This is an F# port of the [Gleam Yog](https://github.com/code-shoily/yog) library. While not a 1:1 port, it captures the spirit and functional API of the original. Some features from the Gleam version (graph generators, visualization, DAG wrapper) are not yet implemented.

## AI Assistance

Parts of this project were developed with the assistance of AI coding tools. All AI-generated code has been reviewed, tested, and validated by the maintainer.

---

**Yog.FSharp** - Graph algorithms for F#
