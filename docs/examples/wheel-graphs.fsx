(**
# Wheel Graphs ($W_n$)
A wheel graph is a graph formed by connecting a single vertex to all vertices of an $(n-1)$-cycle.

## Implementation
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let w6 = Classic.wheel 6 Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 2
  0 --- 3
  0 --- 4
  0 --- 5
  1 --- 2
  2 --- 3
  3 --- 4
  4 --- 5
  5 --- 1
</div>
*)
