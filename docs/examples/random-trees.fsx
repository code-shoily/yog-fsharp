(**
# Random Spanning Trees
A random spanning tree is a tree selected uniformly at random from all possible spanning trees of a complete graph.

*)

open Yog.Model
open Yog.Generators

let tree = Random.randomTree 10 Undirected

(**
## Visual Representation

*(Note: Output will vary due to randomness)*

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 2
  1 --- 3
  1 --- 7
  2 --- 4
  2 --- 5
  3 --- 6
  4 --- 8
  5 --- 9
</div>
*)
