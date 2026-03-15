(**
# Clique Finding with Bron-Kerbosch

This example finds tightly-knit groups in a social network using the Bron-Kerbosch algorithm.

## Problem

In a group of friends, find the largest clique (group where everyone knows everyone)
and enumerate all maximal cliques.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Properties.Clique

// Friend network: 0=Alice, 1=Bob, 2=Carol, 3=Dave, 4=Eve, 5=Frank
let friends =
    empty Undirected
    |> addNode 0 "Alice" |> addNode 1 "Bob"   |> addNode 2 "Carol"
    |> addNode 3 "Dave"  |> addNode 4 "Eve"    |> addNode 5 "Frank"
    // Triangle: Alice-Bob-Carol
    |> addEdge 0 1 1 |> addEdge 1 2 1 |> addEdge 0 2 1
    // Triangle: Bob-Dave-Eve
    |> addEdge 1 3 1 |> addEdge 3 4 1 |> addEdge 1 4 1
    // Frank knows only Dave
    |> addEdge 3 5 1

let names = Map.ofList [(0,"Alice");(1,"Bob");(2,"Carol");(3,"Dave");(4,"Eve");(5,"Frank")]
let showClique (c: Set<int>) =
    c |> Set.toList |> List.map (fun id -> names.[id]) |> String.concat ", "

(**
## Finding Cliques
*)

printfn "=== Clique Analysis ==="

// Maximum Clique
let maxC = maxClique friends
printfn "\nLargest clique: {%s} (size %d)" (showClique maxC) maxC.Count

// All Maximal Cliques
let allC = allMaximalCliques friends
printfn "\nAll maximal cliques:"
for c in allC do
    printfn "  {%s}" (showClique c)

// All triangles (3-cliques)
let triangles = kCliques 3 friends
printfn "\nTriangles (3-cliques): %d found" triangles.Length
for t in triangles do
    printfn "  {%s}" (showClique t)

(**
## Output

```
=== Clique Analysis ===

Largest clique: {Alice, Bob, Carol} (size 3)

All maximal cliques:
  {Alice, Bob, Carol}
  {Bob, Dave, Eve}
  {Dave, Frank}

Triangles (3-cliques): 2 found
  {Alice, Bob, Carol}
  {Bob, Dave, Eve}
```

## Interpretation

The network has two overlapping friend triangles connected through Bob.
Bob is the social bridge between the two tightly-knit groups.
Frank is on the periphery, connected only to Dave.
*)
