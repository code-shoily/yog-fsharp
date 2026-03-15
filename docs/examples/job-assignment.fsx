(**
# Job Assignment with Bipartite Matching
This example demonstrates how to solve a job assignment problem using maximum bipartite matching.

## Problem
Given a set of workers and a set of tasks where not everyone can do every task, find the maximum number of assignments.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Properties.Bipartite

// Job Assignment Problem: 3 Workers, 3 Tasks
let graph =
    empty Undirected
    // Workers (Left Partition)
    |> addNode 1 "Alice"
    |> addNode 2 "Bob"
    |> addNode 3 "Charlie"
    // Tasks (Right Partition)
    |> addNode 4 "Programming"
    |> addNode 5 "Design"
    |> addNode 6 "Testing"
    // Alice can do Programming or Design
    |> addEdge 1 4 ()
    |> addEdge 1 5 ()
    // Bob can only do Programming
    |> addEdge 2 4 ()
    // Charlie can do Design or Testing
    |> addEdge 3 5 ()
    |> addEdge 3 6 ()

printfn "--- Bipartite Job Assignment ---"

// 1. Partition the graph into two independent sets
match partition graph with
| Some p ->
    // 2. Find maximum matching
    let matching = maximumMatching p graph
    printfn "Maximum assignments found: %d" matching.Length

    matching
    |> List.iter (fun (workerId, taskId) ->
        printfn "Worker %d -> Task %d" workerId taskId)
| None ->
    printfn "This graph is not bipartite!"

(**
## Output

```
--- Bipartite Job Assignment ---
Maximum assignments found: 3
Worker 2 -> Task 4
Worker 1 -> Task 5
Worker 3 -> Task 6
```
*)
