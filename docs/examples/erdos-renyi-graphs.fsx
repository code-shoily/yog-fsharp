(**
# Erdős-Rényi Random Graphs ($G(n,p)$)
In the Erdős-Rényi model, each possible edge exists independently with probability $p$.

*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Generators

let er = Random.erdosRenyiGnp 10 0.3 Undirected

(**
## Visual Representation

*(Note: Output will vary due to randomness)*

<div class="mermaid">
graph TD
  0 --- 4
  0 --- 5
  0 --- 9
  1 --- 4
  1 --- 7
  1 --- 8
  2 --- 7
  3 --- 4
  3 --- 9
  4 --- 8
  5 --- 6
  5 --- 8
  6 --- 7
  7 --- 9
</div>
*)
