(**
# GPS Navigation with A* Algorithm

This example demonstrates using the A* pathfinding algorithm with a heuristic function
for efficient route planning, similar to how GPS navigation systems work.

## Problem

Find the fastest route from Home to Office through a network of locations,
using travel time as the edge weight and Euclidean distance as a heuristic.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Pathfinding.AStar

// A graph of locations with coordinates and travel times (minutes)
let locations =
    empty Undirected
    |> addNode 1 (0, 0)   // Home: (x, y)
    |> addNode 2 (5, 2)   // Coffee shop
    |> addNode 3 (2, 8)   // Park
    |> addNode 4 (10, 10) // Office
    |> addEdge 1 2 10     // Home → Coffee: 10 min
    |> addEdge 2 3 15     // Coffee → Park: 15 min
    |> addEdge 3 4 20     // Park → Office: 20 min
    |> addEdge 2 4 25     // Coffee → Office (direct): 25 min

(**
## Heuristic Function

The A* algorithm uses a heuristic to estimate the remaining distance to the goal.
For this example, we use a simplified distance estimate based on known coordinates.

In a real GPS system, you'd calculate actual Euclidean distance between coordinates.
*)

let heuristic (fromId: int) (toId: int) : int =
    // Simplified heuristic - estimates distance to goal (Office = node 4)
    if toId = 4 then
        match fromId with
        | 1 -> 14  // Estimate from Home
        | 2 -> 8   // Estimate from Coffee Shop
        | 3 -> 5   // Estimate from Park
        | 4 -> 0   // Already at Office
        | _ -> 0
    else
        0

(**
## Finding the Route

Use A* with the heuristic to find the optimal path:
*)

match aStarInt heuristic 1 4 locations with
| Some path ->
    printfn "=== GPS Navigation Result ==="
    printfn "Fastest route takes %d minutes" path.TotalWeight
    printfn "Route: %A" path.Nodes
    printfn ""
    printfn "Directions:"
    printfn "1. Start at Home (node 1)"
    printfn "2. Go to Coffee Shop (node 2) - 10 min"
    printfn "3. Continue to Office (node 4) - 25 min"
    printfn "Total: 35 minutes via Coffee Shop"
| None ->
    printfn "No route to office found!"

(**
## Output

```
=== GPS Navigation Result ===
Fastest route takes 35 minutes
Route: [1; 2; 4]

Directions:
1. Start at Home (node 1)
2. Go to Coffee Shop (node 2) - 10 min
3. Continue to Office (node 4) - 25 min
Total: 35 minutes via Coffee Shop
```

## Why A* is Better Than Dijkstra

For this specific problem, A* explores fewer nodes than Dijkstra because:
1. The heuristic guides it toward the goal (Office)
2. It avoids exploring paths that move away from the destination
3. With a good heuristic, A* is often 2-3x faster than Dijkstra

However, if you need shortest paths to **all** destinations, Dijkstra is better!
*)
