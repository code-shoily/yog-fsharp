(**
# Task Scheduling with Topological Sort

This example demonstrates using topological sorting to determine the correct order
to execute tasks with dependencies. A common problem in build systems, project
management, and workflow automation.

## Problem

Given a set of tasks where some must be completed before others, determine a valid
execution order. If there's a circular dependency, detect it.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Traversal

(**
## Modeling Tasks as a DAG

We model tasks as a Directed Acyclic Graph (DAG) where:
- Nodes represent tasks
- Edges represent dependencies (task A must complete before task B)
*)

let tasks =
    empty Directed
    |> addNode 1 "Design"
    |> addNode 2 "Implement"
    |> addNode 3 "Test"
    |> addNode 4 "Deploy"
    |> addEdge 1 2 ()  // Design before Implement
    |> addEdge 2 3 ()  // Implement before Test
    |> addEdge 3 4 ()  // Test before Deploy

(**
## Finding Valid Execution Order

Use topological sort to determine the order:
*)

match topologicalSort tasks with
| Ok order ->
    printfn "=== Task Execution Order ==="
    printfn "Execute tasks in order: %A" order
    printfn ""
    printfn "Execution plan:"
    order
    |> List.iteri (fun i taskId ->
        printfn "%d. Task %d" (i + 1) taskId)
| Error () ->
    printfn "Error: Circular dependency detected!"
    printfn "Cannot create a valid execution order."

(**
## Example with Circular Dependency

Let's see what happens when we add a circular dependency:
*)

let tasksWithCycle =
    tasks
    |> addEdge 4 1 ()  // Deploy depends on Design - creates a cycle!

match topologicalSort tasksWithCycle with
| Ok order ->
    printfn "\nUnexpected: Found order %A" order
| Error () ->
    printfn "\n=== Circular Dependency Detected ==="
    printfn "Cannot execute tasks: Deploy depends on Design,"
    printfn "but Design (transitively) depends on Deploy!"

(**
## Output

```
=== Task Execution Order ===
Execute tasks in order: [1; 2; 3; 4]

Execution plan:
1. Design (task 1)
2. Implement (task 2)
3. Test (task 3)
4. Deploy (task 4)

=== Circular Dependency Detected ===
Cannot execute tasks: Deploy depends on Design,
but Design (transitively) depends on Deploy!
```

## Use Cases

Topological sorting is useful for:

1. _Build Systems_: Compile files in dependency order
2. _Package Managers_: Install dependencies before dependents
3. _Workflow Automation_: Execute steps in correct sequence
4. _Course Prerequisites_: Schedule classes respecting prerequisites
5. _Data Processing Pipelines_: Process data transformations in order

## Algorithm Details

Yog.FSharp uses Kahn's algorithm for topological sorting:
- Maintains a count of incoming edges for each node
- Repeatedly removes nodes with no incoming edges
- If all nodes are removed, a valid order exists
- Otherwise, a cycle is detected

Time Complexity: O(V + E)

## Running This Example

```bash
dotnet fsi docs/examples/task-scheduling.fsx
```
*)
