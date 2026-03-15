(**
# Bridges and Articulation Points

This example finds critical infrastructure in a communication network — edges (bridges)
and nodes (articulation points) whose failure would disconnect the network.

## Problem

Given a computer network, identify the single points of failure:
- **Bridges**: links whose removal splits the network
- **Articulation Points**: routers whose failure disconnects the network
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Connectivity

// Network: 0=HQ, 1=DataCenter, 2=Lab, 3=Office, 4=Remote, 5=Backup
let network =
    empty Undirected
    |> addNode 0 "HQ"     |> addNode 1 "DataCenter" |> addNode 2 "Lab"
    |> addNode 3 "Office"  |> addNode 4 "Remote"     |> addNode 5 "Backup"
    |> addEdge 0 1 1  // HQ - DataCenter
    |> addEdge 1 2 1  // DataCenter - Lab
    |> addEdge 0 2 1  // HQ - Lab (redundant link)
    |> addEdge 1 3 1  // DataCenter - Office (bridge!)
    |> addEdge 3 4 1  // Office - Remote
    |> addEdge 3 5 1  // Office - Backup
    |> addEdge 4 5 1  // Remote - Backup (redundant)

(**
## Running the Analysis
*)

let results = analyze network

printfn "=== Network Vulnerability Analysis ==="
printfn "\nBridges (critical links):"
for (a, b) in results.Bridges do
    printfn "  Link %d <-> %d" a b

printfn "\nArticulation Points (critical routers):"
for node in results.ArticulationPoints do
    let name = network.Nodes.TryFind node |> Option.defaultValue "?"
    printfn "  %s (node %d)" name node

(**
## Output

```
=== Network Vulnerability Analysis ===

Bridges (critical links):
  Link 1 <-> 3

Articulation Points (critical routers):
  Office (node 3)
  DataCenter (node 1)
```

## Interpretation

- The link between **DataCenter** and **Office** is the only bridge — removing it
  splits the network into two disconnected components.
- **DataCenter** and **Office** are articulation points — if either goes down,
  some nodes become unreachable.
- The HQ-Lab-DataCenter triangle and Office-Remote-Backup triangle have redundancy,
  but the single link between the two clusters is a vulnerability.
*)
