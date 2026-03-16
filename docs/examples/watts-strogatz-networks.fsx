(**
# Watts-Strogatz Networks (Small-World)
The Watts-Strogatz model generates small-world networks with high clustering and short path lengths by rewiring edges of a regular ring lattice.

*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let ws = Random.wattsStrogatz 12 4 0.2 Undirected

(**
## Visual Representation

*(Note: Output will vary due to randomness)*

<div class="mermaid">
graph TD
  0 --- 1
  0 --- 2
  0 --- 10
  0 --- 11
  1 --- 2
  1 --- 3
  1 --- 11
  2 --- 3
  2 --- 4
  3 --- 4
  3 --- 5
  4 --- 5
  4 --- 6
  5 --- 6
  5 --- 7
  6 --- 7
  6 --- 8
  7 --- 8
  7 --- 9
  8 --- 9
  8 --- 10
  9 --- 10
  9 --- 11
  10 --- 11
</div>
*)
