(**
# Path Graphs ($P_n$)
A path graph is a graph whose vertices can be listed in the order $v_1, v_2, \dots, v_n$ such that the edges are $\{v_i, v_{i+1}\}$ for $i = 1, 2, \dots, n-1$.

## Implementation
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let p5 = Classic.path 5 Undirected

(**
## Visual Representation

<div class="mermaid">
graph LR
  0 --- 1
  1 --- 2
  2 --- 3
  3 --- 4
</div>
*)
