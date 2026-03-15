(**
# Network Bandwidth Allocation (Max Flow)
This example demonstrates finding the maximum bandwidth in a network using the Edmonds-Karp algorithm and identifying bottlenecks using the min-cut theorem.

## Problem
Model a network of routers with specific bandwidth capacities and find the total capacity from a source to a destination.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Flow.MaxFlow

// Nodes: 0=Source, 1=RouterA, 2=RouterB, 3=RouterC, 4=RouterD, 5=Destination
let network: Graph<unit, int> =
    empty Directed
    |> addNode 0 () |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addNode 4 () |> addNode 5 ()
    |> addEdge 0 1 20 // Source -> Router A (20 Mbps)
    |> addEdge 0 2 30 // Source -> Router B (30 Mbps)
    |> addEdge 1 2 10 // Router A -> Router B (10 Mbps)
    |> addEdge 1 3 15 // Router A -> Router C (15 Mbps)
    |> addEdge 2 3 25 // Router B -> Router C (25 Mbps)
    |> addEdge 2 4 20 // Router B -> Router D (20 Mbps)
    |> addEdge 3 5 30 // Router C -> Destination (30 Mbps)
    |> addEdge 4 5 15 // Router D -> Destination (15 Mbps)

printfn "--- Network Bandwidth Allocation ---"

// 1. Calculate Maximum Flow
let result = edmondsKarpInt 0 5 network

printfn "Maximum bandwidth from source to destination: %d Mbps" result.MaxFlow

// 2. Identify the Bottleneck (Min-Cut)
let cut = minCut 0 compare result

printfn "\n=== Bottleneck Analysis ==="
printfn "Source-side nodes: %A" cut.SourceSide
printfn "Sink-side nodes:   %A" cut.SinkSide
printfn "\nThe edges crossing between these sets are the bottlenecks limiting capacity."

(**
## Output

```
--- Network Bandwidth Allocation ---
Maximum bandwidth from source to destination: 45 Mbps

=== Bottleneck Analysis ===
Source-side nodes: set [0; 1; 2; 3; 4]
Sink-side nodes:   set [5]

The edges crossing between these sets are the bottlenecks limiting capacity.
```
*)
