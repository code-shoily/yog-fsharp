(**
# City Distance Matrix with Floyd-Warshall

This example demonstrates using the Floyd-Warshall algorithm to compute
shortest distances between all pairs of cities. This creates a "distance matrix"
useful for trip planning, logistics, and navigation.

## Problem

Given a network of cities with direct distances, calculate the shortest
distance between every pair of cities, even those without direct connections.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Pathfinding.FloydWarshall

(**
## Modeling a City Network

We model cities as an undirected graph where:
- Nodes represent cities
- Edge weights represent distances in kilometers
*)

let cities =
    empty Undirected
    |> addNode 1 "London"
    |> addNode 2 "Paris"
    |> addNode 3 "Berlin"
    |> addNode 4 "Rome"
    |> addEdge 1 2 344    // London ↔ Paris: 344 km
    |> addEdge 2 3 878    // Paris ↔ Berlin: 878 km
    |> addEdge 3 4 1184   // Berlin ↔ Rome: 1184 km
    |> addEdge 2 4 1105   // Paris ↔ Rome: 1105 km

(**
## Computing All-Pairs Shortest Paths

Use Floyd-Warshall to compute the distance matrix:
*)

printfn "=== City Distance Matrix ==="
printfn ""
printfn "Direct connections:"
printfn "  London ↔ Paris: 344 km"
printfn "  Paris ↔ Berlin: 878 km"
printfn "  Berlin ↔ Rome: 1184 km"
printfn "  Paris ↔ Rome: 1105 km"
printfn ""

match floydWarshallInt cities with
| Ok distances ->
    printfn "=== Shortest Distances Between All Cities ==="
    printfn ""

    // Create a list of all city pairs to display
    let cityPairs = [
        (1, "London"); (2, "Paris"); (3, "Berlin"); (4, "Rome")
    ]

    for (fromId, fromName) in cityPairs do
        for (toId, toName) in cityPairs do
            if fromId < toId then
                match distances |> Map.tryFind (fromId, toId) with
                | Some dist ->
                    printfn "%s to %s: %d km" fromName toName dist
                | None ->
                    printfn "%s to %s: No path" fromName toName

    printfn ""
    printfn "=== Interesting Insights ==="
    printfn ""

    // London to Rome
    match distances |> Map.tryFind (1, 4) with
    | Some dist ->
        printfn "London to Rome shortest distance: %d km" dist
        printfn "  Direct path not available, but can go through Paris!"
        printfn "  Route: London → Paris → Rome"
    | None ->
        printfn "No path from London to Rome"

    // London to Berlin
    match distances |> Map.tryFind (1, 3) with
    | Some dist ->
        printfn ""
        printfn "London to Berlin shortest distance: %d km" dist
        printfn "  Route: London → Paris → Berlin"
    | None ->
        printfn ""
        printfn "No path from London to Berlin"

| Error () ->
    printfn "Error: Negative cycle detected in the graph!"

(**
## Why Floyd-Warshall?

Floyd-Warshall is ideal when you need:

1. _All-pairs shortest paths_: Distance between every pair of cities
2. _Dense graphs_: When you need many pairwise distances
3. _Distance matrices_: Tabular representation of distances
4. _Negative weights_: Can handle negative edges (detects negative cycles)

For single-source shortest paths (one starting point), use Dijkstra or Bellman-Ford instead.

## Output

```
=== City Distance Matrix ===

Direct connections:
  London ↔ Paris: 344 km
  Paris ↔ Berlin: 878 km
  Berlin ↔ Rome: 1184 km
  Paris ↔ Rome: 1105 km

=== Shortest Distances Between All Cities ===

London to Paris: 344 km
London to Berlin: 1222 km
London to Rome: 1449 km
Paris to Berlin: 878 km
Paris to Rome: 1105 km
Berlin to Rome: 1184 km

=== Interesting Insights ===

London to Rome shortest distance: 1449 km
  Direct path not available, but can go through Paris!
  Route: London → Paris → Rome

London to Berlin shortest distance: 1222 km
  Route: London → Paris → Berlin
```

## Complexity

- _Time_: O(V³) where V is the number of cities
- _Space_: O(V²) for the distance matrix
- _Best for_: Small to medium graphs (< 1000 nodes)

For larger graphs with sparse connections, use Dijkstra's algorithm
run from each node instead.

## Use Cases

Floyd-Warshall is used for:

1. _Route Planning_: Pre-compute all distances for quick lookups
2. _Network Analysis_: Find bottlenecks and critical paths
3. _Transitive Closure_: Determine reachability between all nodes
4. _Game AI_: Pathfinding tables for navigation
5. _Traffic Routing_: Optimize delivery routes

## Running This Example

```bash
dotnet fsi docs/examples/city-distance-matrix.fsx
```
*)
