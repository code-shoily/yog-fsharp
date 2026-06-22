/// Tree decomposition representation and validation.
module Yog.Properties.TreeDecomposition

open System.Collections.Generic
open Yog.Model

type TreeDecomposition<'tn, 'te> =
    { Bags: Map<int, Set<NodeId>>
      Tree: Graph<'tn, 'te>
      Width: int }

let private connectedInTree (tree: Graph<'tn, 'te>) (allowed: int list) : bool =
    match allowed with
    | [] -> true
    | [ _ ] -> true
    | start :: _ ->
        let allowedSet = Set.ofList allowed
        let visited = HashSet<int>()
        let queue = Queue<int>()

        queue.Enqueue(start)
        visited.Add(start) |> ignore

        while queue.Count > 0 do
            let current = queue.Dequeue()

            for nbr in successorIds current tree do
                if allowedSet.Contains(nbr) && visited.Add(nbr) then
                    queue.Enqueue(nbr)

        visited.Count = allowedSet.Count

/// Validates that a tree decomposition satisfies all three properties.
let isValid (td: TreeDecomposition<'tn, 'te>) (graph: Graph<'n, 'e>) : bool =
    let vertices = allNodes graph |> Set.ofList
    let bagIndices = td.Bags |> Map.keys |> Seq.toList

    let covered = td.Bags |> Map.values |> Seq.fold Set.union Set.empty

    let vertexCoverage = (covered = vertices)

    let edgeCoverage =
        if vertexCoverage then
            let allGraphEdges =
                allNodes graph
                |> List.collect (fun u ->
                    successorIds u graph
                    |> List.filter (fun v -> u <= v)
                    |> List.map (fun v -> (u, v)))

            allGraphEdges
            |> List.forall (fun (u, v) -> td.Bags |> Map.exists (fun _ bag -> bag.Contains(u) && bag.Contains(v)))
        else
            false

    let runningIntersection =
        if edgeCoverage then
            allNodes graph
            |> List.forall (fun v ->
                let containing = bagIndices |> List.filter (fun idx -> td.Bags.[idx].Contains(v))
                connectedInTree td.Tree containing)
        else
            false

    vertexCoverage && edgeCoverage && runningIntersection
