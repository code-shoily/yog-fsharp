(**
# Centrality Analysis of a Social Network

This example demonstrates how to identify the most important people in a social network
using multiple centrality measures: Degree, Closeness, Betweenness, PageRank, and Eigenvector.

## Problem

Given a small network of friends, determine who is the most "central" or influential
using different definitions of importance.
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Centrality

// Build a small social network
// 0=Alice, 1=Bob, 2=Carol, 3=Dave, 4=Eve
let network =
    empty Undirected
    |> addNode 0 "Alice" |> addNode 1 "Bob" |> addNode 2 "Carol"
    |> addNode 3 "Dave"  |> addNode 4 "Eve"
    |> addEdge 0 1 1  // Alice - Bob
    |> addEdge 0 2 1  // Alice - Carol
    |> addEdge 1 2 1  // Bob - Carol
    |> addEdge 1 3 1  // Bob - Dave
    |> addEdge 3 4 1  // Dave - Eve

let names = Map.ofList [(0,"Alice"); (1,"Bob"); (2,"Carol"); (3,"Dave"); (4,"Eve")]

let printRanking title (scores: Map<int, float>) =
    printfn "\n=== %s ===" title
    scores |> Map.toList |> List.sortByDescending snd
    |> List.iter (fun (id, score) -> printfn "  %s: %.4f" names.[id] score)

(**
## Running the Analysis
*)

// 1. Degree Centrality - who has the most connections?
degree TotalDegree network |> printRanking "Degree Centrality"

// 2. Closeness Centrality - who can reach everyone fastest?
closenessInt network |> printRanking "Closeness Centrality"

// 3. Betweenness Centrality - who sits on the most shortest paths?
betweennessInt network |> printRanking "Betweenness Centrality"

// 4. PageRank - who is connected to other important people?
pagerank defaultPageRankOptions network |> printRanking "PageRank"

// 5. Eigenvector Centrality - whose neighbors are themselves important?
eigenvector 100 0.0001 network |> printRanking "Eigenvector Centrality"

(**
## Output

```
=== Degree Centrality ===
  Bob: 0.7500
  Alice: 0.5000
  Carol: 0.5000
  Dave: 0.5000
  Eve: 0.2500

=== Closeness Centrality ===
  Bob: 0.8000
  Dave: 0.6667
  Alice: 0.5714
  Carol: 0.5714
  Eve: 0.4444

=== Betweenness Centrality ===
  Bob: 4.0000
  Dave: 3.0000
  Alice: 0.0000
  Carol: 0.0000
  Eve: 0.0000

=== PageRank ===
  Bob: 0.2834
  Dave: 0.2126
  Alice: 0.1918
  Carol: 0.1918
  Eve: 0.1204

=== Eigenvector Centrality ===
  Bob: 0.6037
  Alice: 0.4971
  Carol: 0.4971
  Dave: 0.3425
  Eve: 0.1547
```

## Interpretation

- **Degree**: Bob has the most connections (3 out of 4 possible).
- **Closeness**: Bob can reach everyone in the fewest hops.
- **Betweenness**: Bob is the critical bridge — remove him and the network fragments.
- **PageRank**: Bob is connected to other well-connected people.
- **Eigenvector**: Bob's neighbors are themselves important.

Bob is consistently the most central, but Dave's high betweenness reveals his role as the
sole bridge to Eve — a pattern not captured by degree alone.
*)
