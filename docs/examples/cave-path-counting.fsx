(**
# Cave Path Counting (Backtracking DFS)
This example demonstrates a complex path-finding problem on a graph that requires custom depth-first search (DFS) with backtracking.

## Problem
Count all possible paths from "start" to "end" in a cave system. 
- Large caves (UPPERCASE) can be visited multiple times.
- Small caves (lowercase) can only be visited once.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model

// Model the cave system
let caveGraph =
    empty Undirected
    |> addNode 0 "start"
    |> addNode 1 "A"
    |> addNode 2 "b"
    |> addNode 3 "c"
    |> addNode 4 "d"
    |> addNode 5 "end"
    |> addEdge 0 1 ()
    |> addEdge 0 2 ()
    |> addEdge 1 3 ()
    |> addEdge 1 2 ()
    |> addEdge 2 4 ()
    |> addEdge 1 5 ()
    |> addEdge 4 5 ()

// Helper to determine if a cave is small
let isSmall (name: string) = name.ToLower() = name

// Custom recursive DFS with backtracking
let rec countPaths current (visitedSmall: Set<string>) canRevisitOne =
    let caveName = caveGraph.Nodes.[current]
    
    if caveName = "end" then 1
    else
        successorIds current caveGraph
        |> List.fold (fun total neighborId ->
            let neighborName = caveGraph.Nodes.[neighborId]
            let isSmallCave = isSmall neighborName
            let alreadyVisited = Set.contains neighborName visitedSmall
            
            match neighborName, isSmallCave, alreadyVisited with
            | "start", _, _ -> total
            | _, false, _ -> 
                total + countPaths neighborId visitedSmall canRevisitOne
            | _, true, false ->
                let nextVisited = Set.add neighborName visitedSmall
                total + countPaths neighborId nextVisited canRevisitOne
            | _, true, true when canRevisitOne ->
                total + countPaths neighborId visitedSmall false
            | _ -> total
        ) 0

printfn "--- Cave Path Counting ---"

let totalPaths = countPaths 0 (Set.singleton "start") true
printfn "Found %d valid paths through the cave system." totalPaths

(**
## Output

```
--- Cave Path Counting ---
Found 33 valid paths through the cave system.
```
*)
