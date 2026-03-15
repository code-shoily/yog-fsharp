(**
# Dijkstra's Shortest Path

This example uses Dijkstra's algorithm to find the shortest delivery route
in a city road network with travel times as weights.

## Problem

A delivery driver needs to find the fastest route from the Warehouse (node 0)
to the Customer (node 5) through a network of one-way streets.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Pathfinding.Dijkstra

// Road network: nodes are intersections, edges are one-way roads with travel time (minutes)
let roads =
    empty Directed
    |> addNode 0 "Warehouse" |> addNode 1 "Main St"   |> addNode 2 "Park Ave"
    |> addNode 3 "Broadway"  |> addNode 4 "Oak Lane"   |> addNode 5 "Customer"
    |> addEdge 0 1 4   // Warehouse -> Main St: 4 min
    |> addEdge 0 2 2   // Warehouse -> Park Ave: 2 min
    |> addEdge 1 3 5   // Main St -> Broadway: 5 min
    |> addEdge 2 1 1   // Park Ave -> Main St: 1 min
    |> addEdge 2 4 10  // Park Ave -> Oak Lane: 10 min
    |> addEdge 3 5 2   // Broadway -> Customer: 2 min
    |> addEdge 4 5 3   // Oak Lane -> Customer: 3 min

(**
## Finding the Shortest Path
*)

printfn "=== Delivery Route Optimization ==="

match shortestPathInt 0 5 roads with
| Some path ->
    printfn "Fastest route: %A" path.Nodes
    printfn "Total travel time: %d minutes" path.TotalWeight
| None ->
    printfn "No route found!"

// Also compute distances from warehouse to all intersections
printfn "\nDistances from Warehouse:"
let distances = singleSourceDistancesInt 0 roads
for kvp in distances do
    let name = roads.Nodes.TryFind kvp.Key |> Option.defaultValue "?"
    printfn "  %s (node %d): %d min" name kvp.Key kvp.Value

(**
## Output

```
=== Delivery Route Optimization ===
Fastest route: [0; 2; 1; 3; 5]
Total travel time: 10 minutes

Distances from Warehouse:
  Warehouse (node 0): 0 min
  Main St (node 1): 3 min
  Park Ave (node 2): 2 min
  Broadway (node 3): 8 min
  Oak Lane (node 4): 12 min
  Customer (node 5): 10 min
```

## Why Dijkstra?

Dijkstra is the go-to algorithm when all edge weights are non-negative.
It's faster than Bellman-Ford ($O((V+E) \log V)$ vs $O(VE)$) but cannot
handle negative weights.
*)
