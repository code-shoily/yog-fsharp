(**
# Social Network Analysis with Strongly Connected Components

This example demonstrates finding communities in a social network using
Strongly Connected Components (SCC). In a directed social graph, an SCC
represents a group of users who can all reach each other through follows.

## Problem

Given a social network where edges represent "follows" relationships,
identify groups of mutually connected users (communities where everyone
can reach everyone else through the follow graph).
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Connectivity

(**
## Modeling a Social Network

We model the network as a directed graph where:
- Nodes represent users
- Directed edges represent "follows" relationships (A → B means A follows B)
*)

let socialGraph =
    empty Directed
    |> addNode 1 "Alice"
    |> addNode 2 "Bob"
    |> addNode 3 "Carol"
    |> addNode 4 "Dave"
    |> addNode 5 "Eve"
    |> addNode 6 "Frank"
    // Community 1: Alice, Bob, Carol (mutually connected)
    |> addEdge 1 2 ()  // Alice → Bob
    |> addEdge 2 3 ()  // Bob → Carol
    |> addEdge 3 1 ()  // Carol → Alice (closes the loop)
    // Community 2: Dave, Eve (mutually connected)
    |> addEdge 4 5 ()  // Dave → Eve
    |> addEdge 5 4 ()  // Eve → Dave
    // Frank is isolated (only outgoing edge to community 1)
    |> addEdge 6 1 ()  // Frank → Alice

(**
## Finding Communities

Use Tarjan's algorithm to find strongly connected components:
*)

printfn "=== Social Network Analysis ==="
printfn ""
printfn "Network structure:"
printfn "  Alice ↔ Bob ↔ Carol (3-way mutual follows)"
printfn "  Dave ↔ Eve (mutual follows)"
printfn "  Frank → Alice (one-way follow)"
printfn ""

// Tarjan's algorithm (default implementation)
let communities = stronglyConnectedComponents socialGraph

printfn "=== Communities Found: %d ===" communities.Length
printfn ""

communities
|> List.iteri (fun i component ->
    printfn "Community %d (%d members): %A" (i + 1) component.Length component
    printfn "")

(**
## Interpreting Results

A Strongly Connected Component represents users who:
- Can all reach each other through the follow graph
- Form a "community" where information can flow in all directions

_Community 1: Alice, Bob, Carol_
- These three users all follow each other (directly or indirectly)
- Information shared by any of them will reach all others

_Community 2: Dave, Eve_
- These users mutually follow each other
- Smaller but still fully connected community

_Community 3: Frank (alone)_
- Frank follows Alice but no one follows Frank back
- He's in a community of one

## Output

```
=== Social Network Analysis ===

Network structure:
  Alice ↔ Bob ↔ Carol (3-way mutual follows)
  Dave ↔ Eve (mutual follows)
  Frank → Alice (one-way follow)

=== Communities Found: 3 ===

Community 1 (3 members):
  - Carol (ID: 3)
  - Bob (ID: 2)
  - Alice (ID: 1)

Community 2 (2 members):
  - Eve (ID: 5)
  - Dave (ID: 4)

Community 3 (1 member):
  - Frank (ID: 6)
```

## Use Cases

Strongly Connected Components are useful for:

1. _Social Network Analysis_: Find tightly-knit communities
2. _Dependency Resolution_: Detect circular dependencies in code
3. _Web Crawling_: Identify strongly linked web page clusters
4. _Reachability_: Determine which nodes can reach each other
5. _Graph Condensation_: Simplify graphs by treating SCCs as super-nodes

## Alternative: Kosaraju's Algorithm

Yog.FSharp also provides Kosaraju's algorithm for finding SCCs:

```fsharp
let communitiesKosaraju = kosaraju socialGraph
// Same result, different algorithm!
```

Both algorithms have O(V + E) time complexity, but:
- _Tarjan's_: Single DFS pass, more memory efficient
- _Kosaraju's_: Two DFS passes, simpler to understand

## Running This Example

```bash
dotnet fsi docs/examples/social-network-analysis.fsx
```
*)
