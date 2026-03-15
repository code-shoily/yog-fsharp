(**
# Rendering Graphs to JSON
This example demonstrates how to export Yog.FSharp graphs to JSON format for data interchange.

## Basic Usage
We'll create a simple graph and render it as JSON.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.IO.Json

// Create a sample graph
let graph =
    empty Directed
    |> addNode 1 "Node A"
    |> addNode 2 "Node B"
    |> addEdge 1 2 "connects"

(** ### JSON Output
The `Json.render` function produces an indented JSON string.
*)
printfn "--- JSON Output ---"
let json = render graph
printfn "%s" json

(**
## Output

```
--- JSON Output ---
{
  "kind": "Directed",
  "nodes": [
    {
      "id": 1,
      "data": "Node A"
    },
    {
      "id": 2,
      "data": "Node B"
    }
  ],
  "edges": [
    {
      "source": 1,
      "target": 2,
      "weight": "connects"
    }
  ]
}
```
*)
