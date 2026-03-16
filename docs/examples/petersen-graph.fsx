(**
# The Petersen Graph
The Petersen graph is an undirected graph with 10 vertices and 15 edges. It is a small graph that serves as a useful example and counterexample for many problems in graph theory.

*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let petersen = Classic.petersenGraph Undirected

(**
## Visual Representation

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 4
  0 --- 5
  1 --- 2
  1 --- 6
  2 --- 3
  2 --- 7
  3 --- 4
  3 --- 8
  4 --- 9
  5 --- 7
  5 --- 8
  6 --- 8
  6 --- 9
  7 --- 9
</div>
*)
