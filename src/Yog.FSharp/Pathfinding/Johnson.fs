namespace Yog.Pathfinding

open System.Collections.Generic
open Yog.Model
open Yog.Pathfinding.Utils
open Yog.Pathfinding.Dijkstra

module Johnson =

    /// Computes all-pairs shortest paths using Johnson's algorithm.
    /// Time Complexity: O(V² log V + VE)
    /// Returns Ok of a map from (source, target) to shortest distance,
    /// or Error () if a negative cycle is detected.
    let johnson
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (subtract: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (graph: Graph<'n, 'e>)
        : Result<Map<NodeId * NodeId, 'e>, unit> =

        let allNodesList = allNodes graph
        let nodeCount = allNodesList.Length

        // 1. Temporary source node ID
        let tempSource =
            if List.isEmpty allNodesList then
                0
            else
                (allNodesList |> List.max) + 1

        // 2. Build edges with virtual source
        let tempEdges = allNodesList |> List.map (fun v -> (tempSource, v, zero))

        let regularEdges =
            allNodesList
            |> List.collect (fun u -> successors u graph |> List.map (fun (v, w) -> (u, v, w)))

        let edges = tempEdges @ regularEdges

        // 3. Bellman-Ford to find potentials
        let distances = Dictionary<NodeId, 'e>()
        distances.[tempSource] <- zero

        let mutable anyChanged = true
        let mutable iteration = 0
        let totalVertices = nodeCount + 1

        while iteration < totalVertices - 1 && anyChanged do
            anyChanged <- false

            for (u, v, weight) in edges do
                let mutable uDist = zero

                if distances.TryGetValue(u, &uDist) then
                    let newDist = add uDist weight
                    let mutable vDist = zero

                    if not (distances.TryGetValue(v, &vDist)) || compare newDist vDist < 0 then
                        distances.[v] <- newDist
                        anyChanged <- true

            iteration <- iteration + 1

        // Check for negative cycles
        let mutable hasNegativeCycle = false

        for (u, v, weight) in edges do
            let mutable uDist = zero

            if distances.TryGetValue(u, &uDist) then
                let newDist = add uDist weight
                let mutable vDist = zero

                if distances.TryGetValue(v, &vDist) && compare newDist vDist < 0 then
                    hasNegativeCycle <- true

        if hasNegativeCycle then
            Error()
        else
            // Extract potentials
            let potentials =
                allNodesList
                |> List.map (fun v ->
                    let mutable dist = zero
                    if distances.TryGetValue(v, &dist) then v, dist else v, zero)
                |> Map.ofList

            // 4. Reweight the graph
            let reweightedOutEdges =
                graph.OutEdges
                |> Map.map (fun u innerMap ->
                    innerMap
                    |> Map.map (fun v w ->
                        let h_u = Map.find u potentials
                        let h_v = Map.find v potentials
                        subtract (add w h_u) h_v))

            let reweightedInEdges =
                graph.InEdges
                |> Map.map (fun u innerMap ->
                    innerMap
                    |> Map.map (fun v w ->
                        let h_u = Map.find u potentials
                        let h_v = Map.find v potentials
                        subtract (add w h_u) h_v))

            let reweightedGraph =
                { Kind = graph.Kind
                  Nodes = graph.Nodes
                  OutEdges = reweightedOutEdges
                  InEdges = reweightedInEdges }

            // 5. Run Dijkstra from each node
            let finalDistances = Dictionary<NodeId * NodeId, 'e>()

            for source in allNodesList do
                let singleSource = singleSourceDistances zero add compare source reweightedGraph
                let h_source = Map.find source potentials

                for kvp in singleSource do
                    let dest = kvp.Key
                    let distPrime = kvp.Value
                    let h_dest = Map.find dest potentials
                    // dist = dist' - h(source) + h(dest)
                    let adjustedDist = add (subtract distPrime h_source) h_dest
                    finalDistances.[(source, dest)] <- adjustedDist

            let mapSeq = finalDistances |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
            Ok(Map.ofSeq mapSeq)

    // Convenience Wrappers
    let johnsonInt (graph: Graph<'n, int>) : Result<Map<NodeId * NodeId, int>, unit> = johnson 0 (+) (-) compare graph

    let johnsonFloat (graph: Graph<'n, float>) : Result<Map<NodeId * NodeId, float>, unit> =
        johnson 0.0 (+) (-) compare graph
