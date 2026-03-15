(**
# Star Graphs ($S_n$)
A star graph is a complete bipartite graph $K_{1,n-1}$. It consists of a central hub node connected to all other nodes.

*)

open Yog.Model
open Yog.Generators

let s6 = Classic.star 6 Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 2
  0 --- 3
  0 --- 4
  0 --- 5
</div>
*)
