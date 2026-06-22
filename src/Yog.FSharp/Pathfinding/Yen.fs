namespace Yog.Pathfinding

open System.Collections.Generic
open Yog.Model
open Yog.Pathfinding.Utils
open Yog.Pathfinding.Dijkstra

module Yen =

    let private startsWith prefix list =
        let rec loop p l =
            match p, l with
            | [], _ -> true
            | ph :: pt, lh :: lt -> ph = lh && loop pt lt
            | _ -> false

        loop prefix list

    let private getEdgeWeight (graph: Graph<'n, 'e>) (u: NodeId) (v: NodeId) : 'e =
        match Map.tryFind u graph.OutEdges with
        | Some inner ->
            match Map.tryFind v inner with
            | Some w -> w
            | None -> failwithf "Edge %d -> %d not found" u v
        | None -> failwithf "Edge %d -> %d not found" u v

    let private buildPrefixWeights (nodes: NodeId list) (graph: Graph<'n, 'e>) (zero: 'e) (add: 'e -> 'e -> 'e) =
        let weights = Dictionary<int, 'e>()
        weights.[0] <- zero

        let rec loop nodesList idx =
            match nodesList with
            | []
            | [ _ ] -> ()
            | u :: v :: rest ->
                let w = getEdgeWeight graph u v
                let nextWeight = add weights.[idx] w
                weights.[idx + 1] <- nextWeight
                loop (v :: rest) (idx + 1)

        loop nodes 0
        weights

    let private generateCandidates
        (graph: Graph<'n, 'e>)
        (prevPath: Path<'e>)
        (paths: Path<'e> list)
        (seenPaths: HashSet<NodeId list>)
        (seenCandidates: HashSet<NodeId list>)
        (candidatesPq: PriorityQueue<Path<'e>, 'e>)
        (target: NodeId)
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        : unit =

        let prevNodes = prevPath.Nodes
        let maxSpurIndex = List.length prevNodes - 2

        if maxSpurIndex >= 0 then
            let prefixWeights = buildPrefixWeights prevNodes graph zero add

            for i in 0..maxSpurIndex do
                let spurNode = List.item i prevNodes
                let rootPath = List.take (i + 1) prevNodes
                let rootWeight = prefixWeights.[i]
                let rootNodes = List.take i prevNodes

                let edgesToRemove =
                    paths
                    |> List.fold
                        (fun acc p ->
                            if startsWith rootPath p.Nodes then
                                let nextNode = List.item (i + 1) p.Nodes
                                (spurNode, nextNode) :: acc
                            else
                                acc)
                        []

                let mutable modifiedGraph = graph

                for (u, v) in edgesToRemove do
                    modifiedGraph <- removeEdge u v modifiedGraph

                for node in rootNodes do
                    modifiedGraph <- removeNode node modifiedGraph

                match shortestPath zero add compare spurNode target modifiedGraph with
                | None -> ()
                | Some spurPath ->
                    let totalNodes = rootPath @ (List.tail spurPath.Nodes)
                    let totalWeight = add rootWeight spurPath.TotalWeight

                    if not (seenPaths.Contains totalNodes) && not (seenCandidates.Contains totalNodes) then
                        let newPath =
                            { Nodes = totalNodes
                              TotalWeight = totalWeight }

                        candidatesPq.Enqueue(newPath, totalWeight)
                        seenCandidates.Add(totalNodes) |> ignore

    /// Finds the k shortest loopless paths from start to goal.
    /// Time Complexity: O(k * V * (E + V log V))
    /// Returns Some of a list of paths sorted by total weight, shortest first,
    /// or None if no path exists at all.
    let kShortestPaths
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (start: NodeId)
        (goal: NodeId)
        (k: int)
        (graph: Graph<'n, 'e>)
        : Path<'e> list option =

        if k < 1 then
            None
        else
            match shortestPath zero add compare start goal graph with
            | None -> None
            | Some firstPath ->
                let paths = ref [ firstPath ]
                let seenPaths = HashSet<NodeId list>()
                seenPaths.Add(firstPath.Nodes) |> ignore

                let seenCandidates = HashSet<NodeId list>()

                let comparer =
                    { new IComparer<'e> with
                        member _.Compare(a, b) = compare a b }

                let candidatesPq = PriorityQueue<Path<'e>, 'e>(comparer)

                generateCandidates graph firstPath !paths seenPaths seenCandidates candidatesPq goal zero add compare

                let mutable remaining = k - 1
                let mutable stop = false

                while remaining > 0 && not stop do
                    if candidatesPq.Count = 0 then
                        stop <- true
                    else
                        let candidate = candidatesPq.Dequeue()

                        if not (seenPaths.Contains candidate.Nodes) then
                            seenPaths.Add(candidate.Nodes) |> ignore
                            paths := candidate :: !paths

                            generateCandidates
                                graph
                                candidate
                                !paths
                                seenPaths
                                seenCandidates
                                candidatesPq
                                goal
                                zero
                                add
                                compare

                            remaining <- remaining - 1

                Some(List.rev !paths)

    // Convenience Wrappers
    let kShortestPathsInt (start: NodeId) (goal: NodeId) (k: int) (graph: Graph<'n, int>) : Path<int> list option =
        kShortestPaths 0 (+) compare start goal k graph

    let kShortestPathsFloat
        (start: NodeId)
        (goal: NodeId)
        (k: int)
        (graph: Graph<'n, float>)
        : Path<float> list option =
        kShortestPaths 0.0 (+) compare start goal k graph
