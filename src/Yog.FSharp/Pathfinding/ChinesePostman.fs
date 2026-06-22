namespace Yog.Pathfinding

open System.Collections.Generic
open Yog.Model
open Yog.Properties
open Yog.Pathfinding.Dijkstra
open Yog.Multi

module ChinesePostman =

    let private connectedIgnoringIsolates (graph: Graph<'n, 'e>) : bool =
        match Yog.Connectivity.connectedComponents graph with
        | [] -> true
        | components ->
            let edgeComponents =
                components
                |> List.filter (fun comp -> comp |> List.exists (fun u -> (neighbors u graph).Length > 0))
                |> List.length

            edgeComponents <= 1

    let private totalEdgeWeight (graph: Graph<'n, 'e>) (zero: 'e) (add: 'e -> 'e -> 'e) : 'e =
        let mutable sum = zero
        let mutable seen = Set.empty

        for (u, v, w) in allEdges graph do
            let edgeKey = if u <= v then (u, v) else (v, u)

            if not (Set.contains edgeKey seen) then
                seen <- Set.add edgeKey seen
                sum <- add sum w

        sum

    let private popcount (x: int) =
        let rec count valAcc acc =
            if valAcc = 0 then
                acc
            else
                count (valAcc &&& (valAcc - 1)) (acc + 1)

        count x 0

    let rec private firstSetBit (mask: int) =
        if (mask &&& 1) = 1 then 0 else 1 + firstSetBit (mask >>> 1)

    let private bitSet (mask: int) (i: int) = (mask &&& (1 <<< i)) <> 0

    let private minimumWeightPerfectMatching
        (distances: Map<int * int, 'e>)
        (n: int)
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        : (int * int) list =

        let fullMask = (1 <<< n) - 1
        let costs = Dictionary<int, 'e>()
        let pairs = Dictionary<int, int * int>()
        costs.[0] <- zero

        for mask in 1..fullMask do
            if popcount mask % 2 = 0 then
                let i = firstSetBit mask
                let mutable bestCost = None
                let mutable bestJ = -1

                for j in (i + 1) .. (n - 1) do
                    if bitSet mask j then
                        let prev = mask ^^^ (1 <<< i) ^^^ (1 <<< j)

                        if costs.ContainsKey prev then
                            let cost = add costs.[prev] (Map.find (i, j) distances)

                            match bestCost with
                            | None ->
                                bestCost <- Some cost
                                bestJ <- j
                            | Some bestC ->
                                if compare cost bestC < 0 then
                                    bestCost <- Some cost
                                    bestJ <- j

                match bestCost with
                | Some cost ->
                    costs.[mask] <- cost
                    pairs.[mask] <- (i, bestJ)
                | None -> ()

        let rec reconstruct mask acc =
            if mask = 0 then
                acc
            else
                let (i, j) = pairs.[mask]
                let prev = mask ^^^ (1 <<< i) ^^^ (1 <<< j)
                reconstruct prev ((i, j) :: acc)

        reconstruct fullMask []

    let private edgeIdsToNodes (multi: MultiGraph<'n, 'e>) (edgeIds: EdgeId list) : NodeId list =
        match edgeIds with
        | [] -> []
        | [ first ] ->
            let (a, b, _) = Map.find first multi.Edges
            [ a; b ]
        | first :: second :: rest ->
            let (a, b, _) = Map.find first multi.Edges
            let (x, y, _) = Map.find second multi.Edges

            let prev, acc = if b = x || b = y then b, [ b; a ] else a, [ a; b ]

            let mutable currentPrev = prev
            let mutable currentAcc = acc

            for eid in (second :: rest) do
                let (u, v, _) = Map.find eid multi.Edges
                let next = if u = currentPrev then v else u
                currentAcc <- next :: currentAcc
                currentPrev <- next

            List.rev currentAcc

    let private duplicatePath
        (multi: MultiGraph<'n, 'e>)
        (pathNodes: NodeId list)
        (originalGraph: Graph<'n, 'e>)
        (zero: 'e)
        : MultiGraph<'n, 'e> =

        let rec loop currentMulti nodes =
            match nodes with
            | []
            | [ _ ] -> currentMulti
            | u :: v :: rest ->
                let w =
                    match Map.tryFind u originalGraph.OutEdges |> Option.bind (Map.tryFind v) with
                    | Some weight -> weight
                    | None -> zero

                let (m2, _) = Yog.Multi.Model.addEdge u v w currentMulti
                loop m2 (v :: rest)

        loop multi pathNodes

    /// Solves the Chinese Postman Problem for an undirected graph.
    /// Returns Some (closed_walk_nodes, total_weight) on success, or None if no solution exists.
    let chinesePostman
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (graph: Graph<'n, 'e>)
        : (NodeId list * 'e) option =

        if graph.Kind <> Undirected then
            None
        elif List.isEmpty (allNodes graph) then
            None
        elif not (connectedIgnoringIsolates graph) then
            None
        else
            let originalWeight = totalEdgeWeight graph zero add

            let oddVertices =
                allNodes graph |> List.filter (fun u -> (neighbors u graph).Length % 2 = 1)

            if List.isEmpty oddVertices then
                // Already Eulerian
                match Yog.Properties.Eulerian.findEulerianCircuit graph with
                | Some circuit -> Some(circuit, originalWeight)
                | None -> None
            else
                let n = List.length oddVertices
                let oddVerticesArray = List.toArray oddVertices
                let oddIndices = oddVerticesArray |> Array.mapi (fun i u -> i, u) |> Map.ofArray
                let indices = oddVerticesArray |> Array.mapi (fun i u -> u, i) |> Map.ofArray

                // Compute shortest distances between all pairs of odd vertices
                let mutable oddDistances = Map.empty

                for u in oddVertices do
                    let uDistances = singleSourceDistances zero add compare u graph
                    let uIdx = Map.find u indices

                    for v in oddVertices do
                        if u <> v then
                            let vIdx = Map.find v indices

                            match Map.tryFind v uDistances with
                            | Some dist -> oddDistances <- Map.add (uIdx, vIdx) dist oddDistances
                            | None -> ()

                // Find minimum weight perfect matching
                let matchingPairs = minimumWeightPerfectMatching oddDistances n zero add compare

                // Build multigraph and duplicate matched paths
                let mutable multi = Yog.Multi.Model.undirected<'n, 'e> ()

                for u in allNodes graph do
                    let data = Map.find u graph.Nodes
                    multi <- Yog.Multi.Model.addNode u data multi

                for (u, v, w) in allEdges graph do
                    let (m2, _) = Yog.Multi.Model.addEdge u v w multi
                    multi <- m2

                let mutable duplicationWeight = zero

                for (i, j) in matchingPairs do
                    let u = Map.find i oddIndices
                    let v = Map.find j oddIndices

                    match shortestPath zero add compare u v graph with
                    | Some path ->
                        multi <- duplicatePath multi path.Nodes graph zero
                        duplicationWeight <- add duplicationWeight path.TotalWeight
                    | None -> ()

                // Extract Eulerian circuit
                match Yog.Multi.Eulerian.findEulerianCircuit multi with
                | Some edgeIds ->
                    let circuit = edgeIdsToNodes multi edgeIds
                    Some(circuit, add originalWeight duplicationWeight)
                | None -> None

    // Convenience Wrappers
    let chinesePostmanInt (graph: Graph<'n, int>) : (NodeId list * int) option = chinesePostman 0 (+) compare graph

    let chinesePostmanFloat (graph: Graph<'n, float>) : (NodeId list * float) option =
        chinesePostman 0.0 (+) compare graph
