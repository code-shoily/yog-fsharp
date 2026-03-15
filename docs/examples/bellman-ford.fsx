(**
# Bellman-Ford: Shortest Paths with Negative Weights

This example demonstrates the Bellman-Ford algorithm for finding shortest paths
in graphs that may contain negative edge weights, such as currency exchange networks.

## Problem

Given exchange rates between currencies, find the most profitable conversion path.
We model rates as $-\log(\text{rate})$ so that shortest path = most profitable path.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Pathfinding.BellmanFord

// Currency exchange: edges are -log(rate), so shortest path = best exchange
// 0=USD, 1=EUR, 2=GBP, 3=JPY
let exchange =
    empty Directed
    |> addNode 0 "USD" |> addNode 1 "EUR" |> addNode 2 "GBP" |> addNode 3 "JPY"
    |> addEdge 0 1 -1   // USD->EUR favorable
    |> addEdge 1 2 -2   // EUR->GBP favorable
    |> addEdge 2 3  5   // GBP->JPY costly
    |> addEdge 0 3  4   // USD->JPY direct (costly)

(**
## Finding the Best Conversion Path
*)

printfn "=== Currency Exchange Optimization ==="

match bellmanFordInt 0 3 exchange with
| ShortestPath path ->
    printfn "Best path: %A" path.Nodes
    printfn "Total cost: %d" path.TotalWeight
| NegativeCycle ->
    printfn "Arbitrage opportunity detected! (negative cycle)"
| NoPath ->
    printfn "No conversion path exists."

// Also test a graph with a negative cycle (arbitrage)
let arbitrage =
    empty Directed
    |> addNode 0 "A" |> addNode 1 "B" |> addNode 2 "C"
    |> addEdge 0 1 1
    |> addEdge 1 2 -3
    |> addEdge 2 0 1

printfn "\n=== Arbitrage Detection ==="
match bellmanFordInt 0 2 arbitrage with
| ShortestPath path ->
    printfn "Path: %A, cost: %d" path.Nodes path.TotalWeight
| NegativeCycle ->
    printfn "Negative cycle detected — arbitrage opportunity!"
| NoPath ->
    printfn "No path found."

(**
## Output

```
=== Currency Exchange Optimization ===
Best path: [0; 1; 2; 3]
Total cost: 2

=== Arbitrage Detection ===
Negative cycle detected — arbitrage opportunity!
```

## When to Use Bellman-Ford

Use Bellman-Ford instead of Dijkstra when your graph has **negative edge weights**.
Its $O(VE)$ complexity is slower, but it can also **detect negative cycles** — which
represent arbitrage opportunities in finance or contradictory constraints in scheduling.
*)
