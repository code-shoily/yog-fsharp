(**
# Job Matching with Maximum Flow

This example demonstrates solving a bipartite matching problem using the maximum flow algorithm.
We model job assignments as a flow network to find the optimal matching.

## Problem

We have 4 candidates and 4 jobs. Each candidate is qualified for certain jobs.
Find the maximum number of job assignments such that:
- Each candidate gets at most one job
- Each job is filled by at most one candidate
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Flow.MaxFlow

printfn "=== Job Matching with Max Flow ===\n"

(**
## Candidates and Qualifications

- **Alice (1)**: Qualified for Software Engineer (5), Data Analyst (6)
- **Bob (2)**: Qualified for Software Engineer (5), Project Manager (7)
- **Carol (3)**: Qualified for Data Analyst (6), Designer (8)
- **Dave (4)**: Qualified for Project Manager (7), Designer (8)

## Network Structure

We model this as a flow network:
```
Source (0) → Candidates (1-4) → Jobs (5-8) → Sink (9)
```

All edges have capacity 1 (each person can take only one job).
*)

printfn "Candidates and their qualifications:"
printfn "  Alice (1): Qualified for Software Engineer (5), Data Analyst (6)"
printfn "  Bob (2): Qualified for Software Engineer (5), Project Manager (7)"
printfn "  Carol (3): Qualified for Data Analyst (6), Designer (8)"
printfn "  Dave (4): Qualified for Project Manager (7), Designer (8)\n"

let network =
    empty Directed
    // Source to candidates (capacity 1 - each candidate can take one job)
    |> addEdge 0 1 1  // Source → Alice
    |> addEdge 0 2 1  // Source → Bob
    |> addEdge 0 3 1  // Source → Carol
    |> addEdge 0 4 1  // Source → Dave
    // Candidate qualifications (who can do which job)
    |> addEdge 1 5 1  // Alice → Software Engineer
    |> addEdge 1 6 1  // Alice → Data Analyst
    |> addEdge 2 5 1  // Bob → Software Engineer
    |> addEdge 2 7 1  // Bob → Project Manager
    |> addEdge 3 6 1  // Carol → Data Analyst
    |> addEdge 3 8 1  // Carol → Designer
    |> addEdge 4 7 1  // Dave → Project Manager
    |> addEdge 4 8 1  // Dave → Designer
    // Jobs to sink (capacity 1 - each job needs one person)
    |> addEdge 5 9 1  // Software Engineer → Sink
    |> addEdge 6 9 1  // Data Analyst → Sink
    |> addEdge 7 9 1  // Project Manager → Sink
    |> addEdge 8 9 1  // Designer → Sink

(**
## Finding Maximum Matching

Use Edmonds-Karp algorithm to find the maximum flow:
*)

let result = edmondsKarpInt 0 9 network

printfn "Maximum matching: %d positions filled" result.MaxFlow
printfn ""

(**
## Interpreting Results

The maximum flow tells us how many jobs we can fill.
To see the actual assignments, we'd inspect the residual graph:

- If there's flow from candidate i to job j in the residual graph,
  then candidate i is assigned to job j.
*)

if result.MaxFlow = 4 then
    printfn "✓ Perfect matching! All 4 jobs filled."
    printfn ""
    printfn "Possible assignment (one of many):"
    printfn "  Alice → Software Engineer"
    printfn "  Bob → Project Manager"
    printfn "  Carol → Data Analyst"
    printfn "  Dave → Designer"
else
    printfn "⚠ Could only fill %d out of 4 jobs" result.MaxFlow

(**
## Output

```
=== Job Matching with Max Flow ===

Candidates and their qualifications:
  Alice (1): Qualified for Software Engineer (5), Data Analyst (6)
  Bob (2): Qualified for Software Engineer (5), Project Manager (7)
  Carol (3): Qualified for Data Analyst (6), Designer (8)
  Dave (4): Qualified for Project Manager (7), Designer (8)

Maximum matching: 4 positions filled

✓ Perfect matching! All 4 jobs filled.

Possible assignment (one of many):
  Alice → Software Engineer
  Bob → Project Manager
  Carol → Data Analyst
  Dave → Designer
```

## Why This Works

The **Max-Flow Min-Cut Theorem** guarantees that the maximum flow equals
the size of the maximum matching in a bipartite graph.

By modeling the problem as a flow network with capacities of 1, the algorithm
finds the optimal assignment automatically!

## Alternative Approach

For bipartite matching specifically, you could also use:
- `Yog.Properties.Bipartite.maximumMatching` - specialized for this problem
- Hungarian algorithm - O(n³) for weighted bipartite matching
*)
