(**
# Deterministic Task Ordering
This example demonstrates lexicographical topological sorting, which provides a deterministic and alphabetically sorted order for tasks with dependencies.

## Problem
In a build system or project plan, many topological orders may be valid. Sometimes we want the order to be stable and follow alphabetical naming conventions.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Traversal

// Model task dependencies (Pre-requisite, Step)
let dependencies = [
    ("C", "A"); ("C", "F"); ("A", "B"); ("A", "D"); ("B", "E"); ("D", "E"); ("F", "E")
]

let graph: Graph<string, unit> =
    dependencies
    |> List.fold (fun (g: Graph<string, unit>) (prereq, step) ->
        let pid = int (prereq.[0])
        let sid = int (step.[0])
        g
        |> addNode pid prereq
        |> addNode sid step
        |> addEdge pid sid ()
    ) (empty Directed)

printfn "=== Lexicographical Topological Sort ==="

// Sort alphabetically by node data (task names)
match lexicographicalTopologicalSort compare graph with
| Ok order ->
    let taskNames =
        order
        |> List.map (fun id -> graph.Nodes.[id])
        |> String.concat ""
    
    printfn "Task execution order: %s" taskNames
    printfn "(Expected: CABDFE)"
| Error () ->
    printfn "Circular dependency detected!"

(**
## Output

```
=== Lexicographical Topological Sort ===
Task execution order: CABDFE
(Expected: CABDFE)
```
*)
