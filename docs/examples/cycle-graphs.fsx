(**
# Cycle Graphs ($C_n$)
A cycle graph consists of a single cycle, or in other words, some number of vertices connected in a closed chain.

*)

open Yog.Model
open Yog.Generators

let c6 = Classic.cycle 6 Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0 --- 1
  1 --- 2
  2 --- 3
  3 --- 4
  4 --- 5
  5 --- 0
</div>
*)
