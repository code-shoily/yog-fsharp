(**
# Erdős-Rényi Random Graphs ($G(n,m)$)
In the $G(n,m)$ model, a graph is chosen uniformly at random from the collection of all graphs which have $n$ nodes and $m$ edges.

## Implementation
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

// Generate a random graph with 10 nodes and exactly 15 edges
let er = Random.erdosRenyiGnm 10 15 Undirected

(**
## Visual Representation

*(Note: Output will vary due to randomness)*

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 3
  1 --- 4
  1 --- 7
  2 --- 5
  2 --- 8
  3 --- 6
  4 --- 5
  4 --- 9
  5 --- 6
  6 --- 7
  7 --- 8
  8 --- 9
  9 --- 0
  2 --- 4
</div>
*)
