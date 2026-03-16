(**
# 2D Grid Graphs
A 2D grid graph, also known as a lattice graph, is a graph whose vertices correspond to the points in a 2D grid.

*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let grid = Classic.grid2D 3 3 Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 3
  1 --- 2
  1 --- 4
  2 --- 5
  3 --- 4
  3 --- 6
  4 --- 5
  4 --- 7
  5 --- 8
  6 --- 7
  7 --- 8
</div>
*)
