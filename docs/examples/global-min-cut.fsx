(**
# Global Minimum Cut (Stoer-Wagner)
This example identifies the weakest point in a graph that partitions it into two sets with minimum total edge weight.

## Problem
Find the minimum weight cut that disconnects an undirected weighted graph. Unlike s-t cuts, this finds the overall "natural" split points in the graph.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Flow.MinCut

// Model a graph with two tightly connected clusters and one bridge
let graph: Graph<unit, int> =
    empty Undirected
    |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addNode 4 () |> addNode 5 ()
    |> addNode 6 () |> addNode 7 () |> addNode 8 () |> addNode 9 () |> addNode 10 ()
    // Cluster A (nodes 1-5)
    |> addEdge 1 2 10 |> addEdge 2 3 10 |> addEdge 3 4 10 |> addEdge 4 5 10 |> addEdge 5 1 10
    // Cluster B (nodes 6-10)
    |> addEdge 6 7 10 |> addEdge 7 8 10 |> addEdge 8 9 10 |> addEdge 9 10 10 |> addEdge 10 6 10
    // The Bridge (the global minimum cut)
    |> addEdge 1 6 1

printfn "=== Global Min-Cut Showcase ==="

// Run Stol-Wagner algorithm
let result = globalMinCut graph

printfn "Min cut weight: %d" result.Weight
printfn "Group A size:   %d" result.GroupASize
printfn "Group B size:   %d" result.GroupBSize
printfn "Product of group sizes: %d" (result.GroupASize * result.GroupBSize)

(**
## Output

```
=== Global Min-Cut Showcase ===
Min cut weight: 1
Group A size:   5
Group B size:   5
Product of group sizes: 25
```
*)
