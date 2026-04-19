/// Clique finding algorithms using the Bron-Kerbosch algorithm.
///
/// A clique is a subset of nodes where every pair of nodes is connected by an edge.
/// Cliques represent tightly-knit communities or fully-connected subgraphs.
///
/// ## Algorithms
///
/// | Problem | Algorithm | Function | Complexity |
/// |---------|-----------|----------|------------|
/// | Maximum clique | Bron-Kerbosch with pivot | maxClique | O(3^(n/3)) |
/// | All maximal cliques | Bron-Kerbosch | allMaximalCliques | O(3^(n/3)) |
/// | k-Cliques | Bron-Kerbosch with pruning | kCliques | O(3^(n/3)) |
///
/// ## Note
///
/// Finding the maximum clique is NP-hard. The O(3^(n/3)) bound is tight - there
/// exist graphs with exactly 3^(n/3) maximal cliques (Moon-Moser graphs).
///
/// ## Use Cases
///
/// - Social network analysis: Finding tightly-knit friend groups
/// - Bioinformatics: Protein interaction clusters
/// - Finance: Detecting collusion rings in trading
/// - Recommendation: Finding groups with similar preferences
module Yog.Properties.Clique

open System.Collections.Generic
open Yog.Model

/// Builds a lookup of adjacency sets for O(1) neighbor lookups.
let private buildAdjacency (graph: Graph<'n, 'e>) =
    let adj = Dictionary<NodeId, HashSet<NodeId>>()

    for u in allNodes graph do
        let neighbors = successorIds u graph |> HashSet
        adj.[u] <- neighbors

    adj

/// Bron-Kerbosch with pivoting to find the maximum clique.
let maxClique (graph: Graph<'n, 'e>) : Set<NodeId> =
    let adj = buildAdjacency graph
    let nodes = allNodes graph

    let rec solve (r: HashSet<NodeId>) (p: HashSet<NodeId>) (x: HashSet<NodeId>) =
        if p.Count = 0 && x.Count = 0 then
            Set.ofSeq r
        elif p.Count = 0 then
            Set.empty
        else
            // Pivot selection: pick u from P ∪ X that maximizes |P ∩ N(u)|
            let mutable pivot = -1
            let mutable maxOverlap = -1

            for u in Seq.append p x do
                let neighborsU = adj.[u]
                let mutable count = 0

                for n in neighborsU do
                    if p.Contains(n) then
                        count <- count + 1

                if count > maxOverlap then
                    maxOverlap <- count
                    pivot <- u

            let mutable bestR = Set.empty
            let candidates = HashSet(p)
            let pivotNeighbors = adj.[pivot]

            // Process nodes in P \ N(pivot)
            let pList = Seq.toList p

            for v in pList do
                if not (pivotNeighbors.Contains(v)) then
                    let vNeighbors = adj.[v]

                    // New R = R ∪ {v}
                    let nextR = HashSet(r)
                    nextR.Add(v) |> ignore

                    // New P = P ∩ N(v)
                    let nextP = HashSet<NodeId>()

                    for n in vNeighbors do
                        if p.Contains(n) then
                            nextP.Add(n) |> ignore

                    // New X = X ∩ N(v)
                    let nextX = HashSet<NodeId>()

                    for n in vNeighbors do
                        if x.Contains(n) then
                            nextX.Add(n) |> ignore

                    let resultR = solve nextR nextP nextX

                    if resultR.Count > bestR.Count then
                        bestR <- resultR

                    p.Remove(v) |> ignore
                    x.Add(v) |> ignore

            bestR

    solve (HashSet()) (HashSet(nodes)) (HashSet())

/// Finds all maximal cliques in an undirected graph.
let allMaximalCliques (graph: Graph<'n, 'e>) : Set<NodeId> list =
    let adj = buildAdjacency graph
    let nodes = allNodes graph
    let results = ResizeArray<Set<NodeId>>()

    let rec solve (r: HashSet<NodeId>) (p: HashSet<NodeId>) (x: HashSet<NodeId>) =
        if p.Count = 0 && x.Count = 0 then
            results.Add(Set.ofSeq r)
        else
            let pList = Seq.toList p

            for v in pList do
                let vNeighbors = adj.[v]

                let nextR = HashSet(r)
                nextR.Add(v) |> ignore

                let nextP = HashSet<NodeId>()

                for n in vNeighbors do
                    if p.Contains(n) then
                        nextP.Add(n) |> ignore

                let nextX = HashSet<NodeId>()

                for n in vNeighbors do
                    if x.Contains(n) then
                        nextX.Add(n) |> ignore

                solve nextR nextP nextX

                p.Remove(v) |> ignore
                x.Add(v) |> ignore

    solve (HashSet()) (HashSet(nodes)) (HashSet())
    Seq.toList results

/// Finds all cliques of exactly size k.
let kCliques (k: int) (graph: Graph<'n, 'e>) : Set<NodeId> list =
    if k <= 0 then
        []
    else
        let adj = buildAdjacency graph
        let nodes = allNodes graph
        // Replace: let results = mutable.List<Set<NodeId>>()
        let results = ResizeArray<Set<NodeId>>()

        let rec solve (r: HashSet<NodeId>) (p: HashSet<NodeId>) =
            if r.Count = k then
                results.Add(Set.ofSeq r)
            elif r.Count + p.Count >= k then
                let pList = Seq.toList p

                for v in pList do
                    let nextR = HashSet(r)
                    nextR.Add(v) |> ignore

                    let nextP = HashSet<NodeId>()

                    for n in adj.[v] do
                        if p.Contains(n) then
                            nextP.Add(n) |> ignore

                    solve nextR nextP
                    p.Remove(v) |> ignore

        solve (HashSet()) (HashSet(nodes))
        Seq.toList results
