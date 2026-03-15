(**
# The Seven Bridges of Königsberg

The classic problem that founded graph theory! Leonhard Euler proved in 1736 that
it's impossible to cross all seven bridges of Königsberg exactly once.

## Historical Context

The city of Königsberg (now Kaliningrad) had four land masses connected by seven bridges.
The challenge: Can you walk through the city crossing each bridge exactly once?

Euler's insight: This is possible if and only if the graph has at most two nodes
with odd degree (an _Eulerian path_), or zero nodes with odd degree (an _Eulerian circuit_).
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Properties.Eulerian

printfn "=== The Seven Bridges of Königsberg ===\n"

(**
## The Graph

Four land masses (nodes) connected by seven bridges (edges).
Note: This is a multigraph (multiple edges between same nodes).
*)

let konigsberg =
    empty Undirected
    |> addNode 1 "Island A"
    |> addNode 2 "Land B"
    |> addNode 3 "Land C"
    |> addNode 4 "Land D"
    // Seven bridges (note: two between A-B, two between A-C)
    |> addEdge 1 2 1  // Bridge 1: A ↔ B
    |> addEdge 1 2 1  // Bridge 2: A ↔ B
    |> addEdge 1 3 1  // Bridge 3: A ↔ C
    |> addEdge 1 3 1  // Bridge 4: A ↔ C
    |> addEdge 1 4 1  // Bridge 5: A ↔ D
    |> addEdge 2 4 1  // Bridge 6: B ↔ D
    |> addEdge 3 4 1  // Bridge 7: C ↔ D

(**
## Analysis

Check if an Eulerian path or circuit exists:
*)

printfn "Graph analysis:"

if hasEulerianCircuit konigsberg then
    printfn "✓ Eulerian circuit exists (can start and end at same place)"
else
    printfn "✗ No Eulerian circuit (too many odd-degree nodes)"

if hasEulerianPath konigsberg then
    printfn "✓ Eulerian path exists!"
    match findEulerianPath konigsberg with
    | Some path ->
        printfn "Path found: %A" path
    | None ->
        printfn "Error finding path"
else
    printfn "✗ No Eulerian path (more than two odd-degree nodes)"

printfn "\nEuler concluded in 1736 that no such walk is possible."
printfn "All four nodes have odd degree (3 or 5 edges), violating the requirement."

(**
## A Solvable Example

Here's a simple triangle where an Eulerian circuit **does** exist:
*)

printfn "\n=== A Solvable Example (Triangle) ==="

let triangle =
    empty Undirected
    |> addEdge 1 2 1
    |> addEdge 2 3 1
    |> addEdge 3 1 1

match findEulerianCircuit triangle with
| Some circuit ->
    printfn "Successfully found circuit for a triangle:"
    printfn "Path: %A" circuit
    printfn "All nodes have even degree (2), so a circuit exists!"
| None ->
    printfn "Error calculating path"

(**
## Output

```
=== The Seven Bridges of Königsberg ===

Graph analysis:
✗ No Eulerian circuit (too many odd-degree nodes)
✗ No Eulerian path (more than two odd-degree nodes)

Euler concluded in 1736 that no such walk is possible.
All four nodes have odd degree (3 or 5 edges), violating the requirement.

=== A Solvable Example (Triangle) ===
Successfully found circuit for a triangle:
Path: [1; 2; 3; 1]
All nodes have even degree (2), so a circuit exists!
```

## Key Insight

**Euler's Theorem for Undirected Graphs:**

- **Eulerian Circuit** exists ⟺ All nodes have even degree
- **Eulerian Path** exists ⟺ Exactly 0 or 2 nodes have odd degree

This was the birth of graph theory and topology!
*)
