(**
# Complete Bipartite Graphs ($K_{m,n}$)
A complete bipartite graph is a bipartite graph such that every pair of vertices from different sets are adjacent.

## Implementation
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let k32 = Classic.completeBipartite 3 2 Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0 --- 3
  0 --- 4
  1 --- 3
  1 --- 4
  2 --- 3
  2 --- 4
</div>
*)
