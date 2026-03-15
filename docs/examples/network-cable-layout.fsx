(**
# Network Cable Layout Optimization

This example demonstrates using Kruskal's Minimum Spanning Tree (MST) algorithm
to find the cheapest way to connect all buildings in a network with cables.

## Problem

Given multiple buildings and the cost to run cables between them, find the
minimum cost to connect all buildings so they can communicate. This is a
classic MST problem.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Mst

(**
## Modeling the Network

We model the network as an undirected graph where:
- Nodes represent buildings
- Edge weights represent cable installation costs (in dollars)
*)

let buildings =
    empty Undirected
    |> addNode 1 "Building A"
    |> addNode 2 "Building B"
    |> addNode 3 "Building C"
    |> addNode 4 "Building D"
    |> addEdge 1 2 100  // A ↔ B: $100
    |> addEdge 1 3 150  // A ↔ C: $150
    |> addEdge 2 3 50   // B ↔ C: $50
    |> addEdge 2 4 200  // B ↔ D: $200
    |> addEdge 3 4 100  // C ↔ D: $100

(**
## Finding Minimum Cable Cost

Use Kruskal's algorithm to find the minimum spanning tree:
*)

printfn "=== Network Cable Layout Optimization ==="
printfn ""
printfn "Buildings to connect:"
printfn "  - Building A (node 1)"
printfn "  - Building B (node 2)"
printfn "  - Building C (node 3)"
printfn "  - Building D (node 4)"
printfn ""
printfn "Possible cable connections:"
printfn "  A ↔ B: $100"
printfn "  A ↔ C: $150"
printfn "  B ↔ C: $50"
printfn "  B ↔ D: $200"
printfn "  C ↔ D: $100"
printfn ""

// Find MST using Kruskal's algorithm
let cables = kruskal compare buildings

let totalCost = cables |> List.sumBy (fun edge -> edge.Weight)

printfn "=== Optimal Cable Layout ==="
printfn "Cables to install:"
cables
|> List.iter (fun edge ->
    printfn "  Building %d ↔ Building %d: $%d" edge.From edge.To edge.Weight)

printfn ""
printfn "Total cable cost: $%d" totalCost
printfn ""
printfn "This is the minimum cost to connect all buildings!"

(**
## Why This Works

Kruskal's algorithm:

1. _Sorts edges_ by weight (cost) from lowest to highest
2. _Greedily adds_ the cheapest edge that doesn't create a cycle
3. _Continues_ until all nodes are connected

For our network:
- First adds B ↔ C ($50) - cheapest connection
- Then adds A ↔ B ($100) - connects A to the network
- Finally adds C ↔ D ($100) - connects D
- Skips A ↔ C ($150) - would create a cycle
- Skips B ↔ D ($200) - would create a cycle

Total: $250 (instead of $600 if we installed all cables)

## Output

```
=== Network Cable Layout Optimization ===

Buildings to connect:
  - Building A (node 1)
  - Building B (node 2)
  - Building C (node 3)
  - Building D (node 4)

Possible cable connections:
  A ↔ B: $100
  A ↔ C: $150
  B ↔ C: $50
  B ↔ D: $200
  C ↔ D: $100

=== Optimal Cable Layout ===
Cables to install:
  Building B ↔ Building C: $50
  Building A ↔ Building B: $100
  Building C ↔ Building D: $100

Total cable cost: $250

This is the minimum cost to connect all buildings!
```

## Use Cases

Minimum Spanning Trees are used for:

1. _Network Design_: Minimize infrastructure costs (cables, pipes, roads)
2. _Cluster Analysis_: Find natural groupings in data
3. _Image Segmentation_: Partition images efficiently
4. _Approximation Algorithms_: TSP and other optimization problems
5. _Circuit Design_: Minimize wire length in electronics

## Running This Example

```bash
dotnet fsi docs/examples/network-cable-layout.fsx
```
*)
