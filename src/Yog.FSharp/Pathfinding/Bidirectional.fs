namespace Yog.Pathfinding

open System.Collections.Generic
open Yog.Model
open Yog.Pathfinding.Utils

module Bidirectional =

    // ============================================================
    // Bidirectional BFS (Unweighted)
    // ============================================================

    let rec private expandFrontierBFS
        (graph: Graph<'n, 'e>)
        (isForward: bool)
        (frontier: (NodeId * NodeId list) list)
        (visited: Map<NodeId, NodeId list>)
        (otherVisited: Map<NodeId, NodeId list>)
        (acc: (NodeId * NodeId list) list)
        (best: (int * NodeId list) option)
        : (NodeId * NodeId list) list * Map<NodeId, NodeId list> * (int * NodeId list) option =
        
        match frontier with
        | [] -> (acc, visited, best)
        | (node, path) :: rest ->
            let neighbors = 
                if isForward then
                    successorIds node graph
                else
                    predecessorIds node graph

            let mutable currentAcc = acc
            let mutable currentVisited = visited
            let mutable currentBest = best

            for neighbor in neighbors do
                if not (currentVisited.ContainsKey neighbor) then
                    let newPath = neighbor :: path
                    currentVisited <- Map.add neighbor newPath currentVisited
                    currentAcc <- (neighbor, newPath) :: currentAcc

                    match Map.tryFind neighbor otherVisited with
                    | Some otherPath ->
                        let fullPath =
                            if isForward then
                                (List.rev newPath) @ (List.tail otherPath)
                            else
                                (List.rev otherPath) @ (List.tail newPath)
                        let len = List.length fullPath - 1
                        match currentBest with
                        | None ->
                            currentBest <- Some (len, fullPath)
                        | Some (bestLen, _) ->
                            if len < bestLen then
                                currentBest <- Some (len, fullPath)
                    | None -> ()

            expandFrontierBFS graph isForward rest currentVisited otherVisited currentAcc currentBest

    let rec private doBfsLevel
        (graph: Graph<'n, 'e>)
        (fwdFrontier: (NodeId * NodeId list) list)
        (bwdFrontier: (NodeId * NodeId list) list)
        (visitedFwd: Map<NodeId, NodeId list>)
        (visitedBwd: Map<NodeId, NodeId list>)
        (depthFwd: int)
        (depthBwd: int)
        (best: (int * NodeId list) option)
        : Path<int> option =
        
        match best with
        | Some (bestLen, _) when depthFwd + depthBwd >= bestLen ->
            best |> Option.map (fun (len, path) -> { Nodes = path; TotalWeight = len })
        | _ ->
            if List.isEmpty fwdFrontier || List.isEmpty bwdFrontier then
                best |> Option.map (fun (len, path) -> { Nodes = path; TotalWeight = len })
            else
                if List.length fwdFrontier <= List.length bwdFrontier then
                    let (newFrontier, newVisitedFwd, newBest) =
                        expandFrontierBFS graph true fwdFrontier visitedFwd visitedBwd [] best
                    doBfsLevel graph newFrontier bwdFrontier newVisitedFwd visitedBwd (depthFwd + 1) depthBwd newBest
                else
                    let (newFrontier, newVisitedBwd, newBest) =
                        expandFrontierBFS graph false bwdFrontier visitedBwd visitedFwd [] best
                    doBfsLevel graph fwdFrontier newFrontier visitedFwd newVisitedBwd depthFwd (depthBwd + 1) newBest

    /// Finds the shortest path in an unweighted graph using bidirectional BFS.
    let shortestPathUnweighted (start: NodeId) (goal: NodeId) (graph: Graph<'n, 'e>) : Path<int> option =
        if start = goal then
            Some { Nodes = [ start ]; TotalWeight = 0 }
        elif not (graph.Nodes.ContainsKey start) || not (graph.Nodes.ContainsKey goal) then
            None
        else
            let fwdFrontier = [ (start, [ start ]) ]
            let bwdFrontier = [ (goal, [ goal ]) ]
            let visitedFwd = Map.empty |> Map.add start [ start ]
            let visitedBwd = Map.empty |> Map.add goal [ goal ]
            doBfsLevel graph fwdFrontier bwdFrontier visitedFwd visitedBwd 0 0 None

    // ============================================================
    // Bidirectional Dijkstra (Weighted)
    // ============================================================

    let private reconstructBidirectionalPath
        (meetingPoint: NodeId)
        (predFwd: Dictionary<NodeId, NodeId>)
        (predBwd: Dictionary<NodeId, NodeId>)
        : NodeId list =
        let rec toSource node acc =
            match predFwd.TryGetValue(node) with
            | true, parent -> toSource parent (parent :: acc)
            | false, _ -> acc
        
        let rec toTarget node acc =
            match predBwd.TryGetValue(node) with
            | true, child -> toTarget child (acc @ [ child ])
            | false, _ -> acc

        let fwdPath = toSource meetingPoint [ meetingPoint ]
        let bwdPath = toTarget meetingPoint []
        fwdPath @ bwdPath

    type private SearchSide =
        | FwdSide
        | BwdSide
        | NoSide

    /// Finds the shortest path in a weighted graph using bidirectional Dijkstra.
    let shortestPath
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (start: NodeId)
        (goal: NodeId)
        (graph: Graph<'n, 'e>)
        : Path<'e> option =
        if start = goal then
            Some { Nodes = [ start ]; TotalWeight = zero }
        elif not (graph.Nodes.ContainsKey start) || not (graph.Nodes.ContainsKey goal) then
            None
        else
            let comparer =
                { new IComparer<'e> with
                    member _.Compare(a, b) = compare a b }

            let pqFwd = PriorityQueue<NodeId * 'e, 'e>(comparer)
            let pqBwd = PriorityQueue<NodeId * 'e, 'e>(comparer)

            let distFwd = Dictionary<NodeId, 'e>()
            let distBwd = Dictionary<NodeId, 'e>()

            let predFwd = Dictionary<NodeId, NodeId>()
            let predBwd = Dictionary<NodeId, NodeId>()

            pqFwd.Enqueue((start, zero), zero)
            distFwd.[start] <- zero

            pqBwd.Enqueue((goal, zero), zero)
            distBwd.[goal] <- zero

            let mutable bestPath = None
            let mutable bestWeight = None

            let mutable doneSearch = false

            while (pqFwd.Count > 0 || pqBwd.Count > 0) && not doneSearch do
                // 1. Determine if we can check termination
                match bestWeight with
                | Some w ->
                    let fwdPossible =
                        if pqFwd.Count > 0 then
                            let mutable dummy = (0, zero)
                            let mutable minFwd = zero
                            pqFwd.TryPeek(&dummy, &minFwd) |> ignore
                            compare minFwd w < 0
                        else false
                    let bwdPossible =
                        if pqBwd.Count > 0 then
                            let mutable dummy = (0, zero)
                            let mutable minBwd = zero
                            pqBwd.TryPeek(&dummy, &minBwd) |> ignore
                            compare minBwd w < 0
                        else false
                    if not fwdPossible && not bwdPossible then
                        doneSearch <- true
                | None ->
                    if pqFwd.Count = 0 || pqBwd.Count = 0 then
                        doneSearch <- true

                if not doneSearch then
                    // Decide expansion side
                    let mutable dummyFwd = (0, zero)
                    let mutable minFwd = zero
                    let hasFwd = pqFwd.TryPeek(&dummyFwd, &minFwd)
                    
                    let mutable dummyBwd = (0, zero)
                    let mutable minBwd = zero
                    let hasBwd = pqBwd.TryPeek(&dummyBwd, &minBwd)

                    let side =
                        match hasFwd, hasBwd with
                        | true, true ->
                            if compare minFwd minBwd <= 0 then FwdSide else BwdSide
                        | true, false -> FwdSide
                        | false, true -> BwdSide
                        | false, false -> NoSide

                    match side with
                    | FwdSide ->
                        let (u, uDist) = pqFwd.Dequeue()
                        let mutable actualDist = zero
                        if distFwd.TryGetValue(u, &actualDist) && compare uDist actualDist = 0 then
                            for (v, weight) in successors u graph do
                                let newDistV = add uDist weight
                                let mutable currentBestV = zero
                                if not (distFwd.TryGetValue(v, &currentBestV)) || compare newDistV currentBestV < 0 then
                                    distFwd.[v] <- newDistV
                                    predFwd.[v] <- u
                                    pqFwd.Enqueue((v, newDistV), newDistV)

                                    // Check meeting point
                                    let mutable dOther = zero
                                    if distBwd.TryGetValue(v, &dOther) then
                                        let totalDist = add newDistV dOther
                                        match bestWeight with
                                        | None ->
                                            bestWeight <- Some totalDist
                                            bestPath <- Some (reconstructBidirectionalPath v predFwd predBwd)
                                        | Some w ->
                                            if compare totalDist w < 0 then
                                                bestWeight <- Some totalDist
                                                bestPath <- Some (reconstructBidirectionalPath v predFwd predBwd)
                    | BwdSide ->
                        let (v, vDist) = pqBwd.Dequeue()
                        let mutable actualDist = zero
                        if distBwd.TryGetValue(v, &actualDist) && compare vDist actualDist = 0 then
                            for (u, weight) in predecessors v graph do
                                let newDistU = add vDist weight
                                let mutable currentBestU = zero
                                if not (distBwd.TryGetValue(u, &currentBestU)) || compare newDistU currentBestU < 0 then
                                    distBwd.[u] <- newDistU
                                    predBwd.[u] <- v
                                    pqBwd.Enqueue((u, newDistU), newDistU)

                                    // Check meeting point
                                    let mutable dOther = zero
                                    if distFwd.TryGetValue(u, &dOther) then
                                        let totalDist = add dOther newDistU
                                        match bestWeight with
                                        | None ->
                                            bestWeight <- Some totalDist
                                            bestPath <- Some (reconstructBidirectionalPath u predFwd predBwd)
                                        | Some w ->
                                            if compare totalDist w < 0 then
                                                bestWeight <- Some totalDist
                                                bestPath <- Some (reconstructBidirectionalPath u predFwd predBwd)
                    | NoSide ->
                        doneSearch <- true

            match bestPath, bestWeight with
            | Some path, Some w -> Some { Nodes = path; TotalWeight = w }
            | _ -> None

    // Convenience Wrappers
    let shortestPathInt (start: NodeId) (goal: NodeId) (graph: Graph<'n, int>) : Path<int> option =
        shortestPath 0 (+) compare start goal graph

    let shortestPathFloat (start: NodeId) (goal: NodeId) (graph: Graph<'n, float>) : Path<float> option =
        shortestPath 0.0 (+) compare start goal graph
