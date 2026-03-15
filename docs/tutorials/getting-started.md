# Getting Started with Yog.FSharp

This guide will walk you through installing Yog.FSharp and using it to solve common graph problems.

## Installation

Add Yog.FSharp to your F# project:

```bash
dotnet add package Yog.FSharp
```

Or add it manually to your `.fsproj` file:

```xml
<ItemGroup>
  <PackageReference Include="Yog.FSharp" Version="0.1.0-alpha.1" />
</ItemGroup>
```

## Basic Concepts

### Graphs

Yog.FSharp provides two types of graphs:
- **Directed**: Edges have a direction (A → B)
- **Undirected**: Edges are bidirectional (A ↔ B)

Both types support generic node data and edge weights.

### Creating a Graph

```fsharp
open Yog.Model

// Create an empty directed graph
let directedGraph = empty Directed

// Create an empty undirected graph
let undirectedGraph = empty Undirected
```

### Adding Nodes and Edges

```fsharp
open Yog.Model

let socialNetwork =
    empty Undirected
    |> addNode 1 "Alice"
    |> addNode 2 "Bob"
    |> addNode 3 "Charlie"
    |> addEdge 1 2 1  // Alice ↔ Bob (friendship)
    |> addEdge 2 3 1  // Bob ↔ Charlie (friendship)
    |> addEdge 1 3 1  // Alice ↔ Charlie (friendship)
```

## Common Tasks

### 1. Finding Shortest Paths

Use Dijkstra's algorithm for non-negative weights:

```fsharp
open Yog.Pathfinding.Dijkstra

let roadNetwork =
    empty Directed
    |> addNode 0 "Home"
    |> addNode 1 "Coffee Shop"
    |> addNode 2 "Work"
    |> addEdge 0 1 10  // 10 minutes
    |> addEdge 1 2 15  // 15 minutes
    |> addEdge 0 2 30  // Direct route: 30 minutes

match shortestPathInt 0 2 roadNetwork with
| Some path ->
    printfn "Fastest route: %d minutes" path.TotalWeight
    printfn "Via: %A" path.Nodes  // [0; 1; 2]
| None ->
    printfn "No route found"
```

### 2. Graph Traversal

Explore nodes using BFS or DFS:

```fsharp
open Yog.Traversal

let maze =
    empty Undirected
    |> addNode 1 "Start"
    |> addNode 2 "Junction"
    |> addNode 3 "Dead End"
    |> addNode 4 "Exit"
    |> addEdge 1 2 1
    |> addEdge 2 3 1
    |> addEdge 2 4 1

// Find all reachable nodes from start
let reachable = walkAll BreadthFirst 1 maze
printfn "Reachable: %A" reachable  // [1; 2; 3; 4]

// Find path to exit
match walkUntil BreadthFirst 1 (fun id -> id = 4) maze with
| Some exitId -> printfn "Found exit at node %d" exitId
| None -> printfn "No path to exit"
```

### 3. Checking Graph Properties

```fsharp
open Yog.Properties.Bipartite
open Yog.Properties.Cyclicity

let taskGraph =
    empty Directed
    |> addEdge 1 2 1  // Task 1 → Task 2
    |> addEdge 2 3 1  // Task 2 → Task 3
    |> addEdge 3 4 1  // Task 3 → Task 4

// Check if graph is acyclic (DAG)
if isAcyclic taskGraph then
    printfn "Valid task ordering (no circular dependencies)"
else
    printfn "Circular dependency detected!"

// Check if graph is bipartite
let collaborationGraph =
    empty Undirected
    |> addEdge 1 2 1
    |> addEdge 2 3 1

match partition collaborationGraph with
| Some (setA, setB) ->
    printfn "Bipartite! Set A: %A, Set B: %A" setA setB
| None ->
    printfn "Not bipartite (contains odd cycle)"
```

## Next Steps

- **[Examples](../examples.html)** - See real-world use cases
- **[Tutorials](index.html)** - Deep dive into specific algorithms
- **[API Reference](../reference/index.html)** - Complete API documentation

## Common Patterns

### Builder Pattern for Labeled Graphs

Use the Labeled builder when working with string/custom identifiers:

```fsharp
open Yog.Builder.Labeled

let socialGraph =
    directed()
    |> addEdge "Alice" "Bob" 1
    |> addEdge "Bob" "Charlie" 1
    |> addEdge "Alice" "Charlie" 1
    |> toGraph

// Get internal ID for a label
match getId "Alice" socialGraph with
| Some id -> printfn "Alice's ID: %d" id
| None -> printfn "Alice not found"
```

### Grid Graphs for Pathfinding

Create grid-based graphs from 2D arrays:

```fsharp
open Yog.Builder.Grid

let maze = array2D [
    ["."; "."; "#"; "."]
    ["."; "#"; "#"; "."]
    ["."; "."; "."; "."]
]

let grid =
    fromArray2D maze Directed rook (avoiding "#")

let graph = toGraph grid
let start = coordToId 0 0 grid.Cols  // Top-left
let goal = coordToId 2 3 grid.Cols   // Bottom-right

// Use with pathfinding...
```
