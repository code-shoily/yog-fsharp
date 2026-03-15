(**
# DAG Algorithms: Critical Path & Transitive Closure

This example demonstrates DAG-specific algorithms for project scheduling:
longest path (critical path) and transitive closure (indirect dependencies).

## Problem

A software project has tasks with durations and dependencies. Find the critical path
(minimum project duration) and all indirect dependencies.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Dag
open Yog.Dag.Model

(**
## Building the Task DAG

Tasks: 0=Design, 1=Frontend, 2=Backend, 3=Database, 4=Testing, 5=Deploy
Edge weights represent task durations in days.
*)

let taskGraph =
    Yog.Model.empty Directed
    |> Yog.Model.addNode 0 "Design"   |> Yog.Model.addNode 1 "Frontend"
    |> Yog.Model.addNode 2 "Backend"  |> Yog.Model.addNode 3 "Database"
    |> Yog.Model.addNode 4 "Testing"  |> Yog.Model.addNode 5 "Deploy"
    |> Yog.Model.addEdge 0 1 3  // Design -> Frontend: 3 days
    |> Yog.Model.addEdge 0 2 5  // Design -> Backend: 5 days
    |> Yog.Model.addEdge 0 3 2  // Design -> Database: 2 days
    |> Yog.Model.addEdge 1 4 2  // Frontend -> Testing: 2 days
    |> Yog.Model.addEdge 2 4 3  // Backend -> Testing: 3 days
    |> Yog.Model.addEdge 3 2 1  // Database -> Backend: 1 day
    |> Yog.Model.addEdge 4 5 1  // Testing -> Deploy: 1 day

let dag =
    match fromGraph taskGraph with
    | Ok d -> d
    | Error _ -> failwith "Graph has a cycle!"

(**
## Critical Path Analysis
*)

printfn "=== Project Schedule Analysis ==="

// Topological order
let order = Algorithms.topologicalSort dag
printfn "\nExecution order: %A" order

// Longest path (critical path)
let critical = Algorithms.longestPath dag
printfn "\nCritical path: %A" critical
printfn "These tasks determine the minimum project duration."

(**
## Transitive Closure
*)

let closure = Algorithms.transitiveClosure (+) dag
let closureGraph = toGraph closure

printfn "\nIndirect dependencies (transitive closure):"
for src in allNodes closureGraph do
    for (dst, weight) in successors src closureGraph do
        let srcName = closureGraph.Nodes.TryFind src |> Option.defaultValue "?"
        let dstName = closureGraph.Nodes.TryFind dst |> Option.defaultValue "?"
        printfn "  %s -> %s (total weight: %d)" srcName dstName weight

(**
## Output

```
=== Project Schedule Analysis ===

Execution order: [0; 1; 3; 2; 4; 5]

Critical path: [0; 2; 4; 5]
These tasks determine the minimum project duration.

Indirect dependencies (transitive closure):
  Design -> Frontend (total weight: 3)
  Design -> Backend (total weight: 8)
  Design -> Database (total weight: 2)
  Design -> Testing (total weight: 19)
  Design -> Deploy (total weight: 22)
  Frontend -> Testing (total weight: 2)
  Frontend -> Deploy (total weight: 3)
  Backend -> Testing (total weight: 3)
  Backend -> Deploy (total weight: 4)
  Database -> Backend (total weight: 1)
  Database -> Testing (total weight: 4)
  Database -> Deploy (total weight: 5)
  Testing -> Deploy (total weight: 1)
```

## Interpretation

- The **critical path** Design → Backend → Testing → Deploy takes 9 days minimum.
- The **transitive closure** reveals that Design indirectly affects all downstream tasks,
  with the longest chain accumulating 9 days of work.
*)
