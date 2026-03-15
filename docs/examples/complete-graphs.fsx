(**
# Complete Graphs ($K_n$)
A complete graph is a simple undirected graph in which every pair of distinct vertices is connected by a unique edge.
In a complete graph with $n$ nodes, there are $n(n-1)/2$ edges.

*)

open Yog.Model
open Yog.Generators

let k5 = Classic.complete 5 Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 2
  0 --- 3
  0 --- 4
  1 --- 2
  1 --- 3
  1 --- 4
  2 --- 3
  2 --- 4
  3 --- 4
</div>
*)
