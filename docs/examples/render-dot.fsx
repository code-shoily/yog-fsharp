(**
# Rendering Graphs to DOT
This example demonstrates how to export Yog.FSharp graphs to the DOT format for visualization with Graphviz.

## Basic Usage
We'll create a simple directed graph and export it to DOT syntax.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.IO.Dot
open Yog.Transform

// Create a sample graph
let graph =
    empty Directed
    |> addNode 1 "Start"
    |> addNode 2 "Middle"
    |> addNode 3 "End"
    |> addEdge 1 2 5
    |> addEdge 2 3 3
    |> addEdge 1 3 10

(** ### 1. Basic DOT Output
The simplest way to export a graph is using the default options.
*)
printfn "--- Basic DOT Output ---"
let dotBasic = render defaultOptions (graph |> mapEdges string)
printfn "%s" dotBasic

(** ### 2. Customizing Output
You can customize labels, shapes, and highlight specific paths.
*)
printfn "\n--- DOT with Highlighted Path ---"
// Highlight the shortest path (1 -> 2 -> 3)
let options = {
    defaultOptions with
        NodeLabel = fun id data -> $"{id}: {data}"
        HighlightedNodes = Set.ofList [1; 2; 3]
        HighlightedEdges = Set.ofList [(1, 2); (2, 3)]
        HighlightColor = "blue"
}

let dotHighlighted = render options (graph |> mapEdges string)
printfn "%s" dotHighlighted

(**
## Output

```
--- Basic DOT Output ---
digraph G {
  node [shape=ellipse];
  edge [fontname="Helvetica", fontsize=10];
  1 [label="1"];
  2 [label="2"];
  3 [label="3"];
  1 -> 2 [label="5"];
  1 -> 3 [label="10"];
  2 -> 3 [label="3"];
}

--- DOT with Highlighted Path ---
digraph G {
  node [shape=ellipse];
  edge [fontname="Helvetica", fontsize=10];
  1 [label="1: Start" fillcolor="blue", style=filled];
  2 [label="2: Middle" fillcolor="blue", style=filled];
  3 [label="3: End" fillcolor="blue", style=filled];
  1 -> 2 [label="5" color="blue", penwidth=2];
  1 -> 3 [label="10"];
  2 -> 3 [label="3" color="blue", penwidth=2];
}
```
*)
