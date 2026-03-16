(**
# Barabási-Albert Networks (Scale-Free)
The Barabási-Albert model generates scale-free networks using preferential attachment, where new nodes prefer to connect to high-degree nodes.

*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let ba = Random.barabasiAlbert 12 2 Undirected

(**
## Visual Representation

*(Note: Output will vary due to randomness)*

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 2
  0 --- 3
  0 --- 4
  0 --- 5
  0 --- 6
  0 --- 7
  0 --- 8
  0 --- 9
  0 --- 10
  0 --- 11
  1 --- 2
  1 --- 3
  1 --- 4
  1 --- 5
  2 --- 6
  3 --- 11
  4 --- 7
  5 --- 9
  6 --- 8
  7 --- 10
</div>
*)
