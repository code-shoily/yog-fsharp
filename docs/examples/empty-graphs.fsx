(**
# Empty Graphs
An empty graph is a graph with $n$ vertices and no edges. It is often used as a starting point for building custom graphs.

## Implementation
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let empty5 = Classic.emptyGraph 5 Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0
  1
  2
  3
  4
</div>
*)
