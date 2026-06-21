namespace Yog

open System
open System.Collections.Generic
open Yog.Model

module Health =

    /// Computes the eccentricity of a node: the maximum distance to all other nodes.
    /// Returns None if the node cannot reach all other nodes.
    let eccentricity
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (node: NodeId)
        (graph: Graph<'n, 'e>)
        : 'e option =
        let numNodes = graph.Nodes.Count
        if numNodes <= 1 then
            Some zero
        else
            let distances = Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare node graph
            if distances.Count < numNodes then
                None
            else
                distances
                |> Map.values
                |> Seq.reduce (fun maxDist d -> if compare d maxDist > 0 then d else maxDist)
                |> Some

    /// The diameter is the maximum eccentricity (longest shortest path).
    /// Returns None if the graph is disconnected or empty.
    let diameter
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (graph: Graph<'n, 'e>)
        : 'e option =
        let nodes = allNodes graph
        let n = nodes.Length
        if n = 0 then
            None
        else
            let eccentricities =
                nodes
                |> List.map (fun node -> eccentricity zero add compare node graph)
                |> List.choose id
            if eccentricities.Length < n then
                None
            else
                eccentricities
                |> List.reduce (fun maxEcc ecc -> if compare ecc maxEcc > 0 then ecc else maxEcc)
                |> Some

    /// The radius is the minimum eccentricity.
    /// Returns None if the graph is disconnected or empty.
    let radius
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (graph: Graph<'n, 'e>)
        : 'e option =
        let nodes = allNodes graph
        let n = nodes.Length
        if n = 0 then
            None
        else
            let eccentricities =
                nodes
                |> List.map (fun node -> eccentricity zero add compare node graph)
                |> List.choose id
            if eccentricities.Length < n then
                None
            else
                eccentricities
                |> List.reduce (fun minEcc ecc -> if compare ecc minEcc < 0 then ecc else minEcc)
                |> Some

    /// Assortativity coefficient measures degree correlation.
    let assortativity (graph: Graph<'n, 'e>) : float =
        let nodes = allNodes graph
        let degrees = nodes |> List.map (fun node -> node, (successors node graph).Length) |> Map.ofList
        
        let mutable mCount = 0
        let mutable sumJk = 0.0
        let mutable sumJPlusK = 0.0
        let mutable sumJ2PlusK2 = 0.0
        
        for u in nodes do
            let succs = successors u graph
            let j = float (Map.find u degrees)
            for (v, _) in succs do
                let k = float (Map.find v degrees)
                mCount <- mCount + 1
                sumJk <- sumJk + j * k
                sumJPlusK <- sumJPlusK + (j + k)
                sumJ2PlusK2 <- sumJ2PlusK2 + (j * j + k * k)
                
        let m = float mCount
        if m = 0.0 then
            0.0
        else
            let term1 = sumJPlusK / 2.0
            let numerator = sumJk / m - ((term1 / m) ** 2.0)
            let denominator = sumJ2PlusK2 / (2.0 * m) - ((term1 / m) ** 2.0)
            if denominator > 0.0 then numerator / denominator else 0.0

    /// Average shortest path length across all node pairs.
    /// Returns None if the graph is disconnected or empty.
    let averagePathLength
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (toFloat: 'e -> float)
        (graph: Graph<'n, 'e>)
        : float option =
        let nodes = allNodes graph
        let numNodes = nodes.Length
        if numNodes <= 1 then
            None
        else
            let mutable disconnected = false
            let mutable totalDist = 0.0
            let mutable i = 0
            while i < numNodes && not disconnected do
                let u = nodes.[i]
                let distances = Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare u graph
                if distances.Count < numNodes then
                    disconnected <- true
                else
                    for dist in distances.Values do
                        totalDist <- totalDist + toFloat dist
                i <- i + 1
                
            if disconnected then
                None
            else
                let zeroDistances = float numNodes * toFloat zero
                let numPairs = float numNodes * float (numNodes - 1)
                Some ((totalDist - zeroDistances) / numPairs)

    /// Efficiency between two nodes (inverse of shortest path distance).
    let efficiency
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (toFloat: 'e -> float)
        (u: NodeId)
        (v: NodeId)
        (graph: Graph<'n, 'e>)
        : float =
        if u = v then
            0.0
        else
            let distances = Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare u graph
            match Map.tryFind v distances with
            | Some dist ->
                let val' = toFloat dist
                if val' = 0.0 then 0.0 else 1.0 / val'
            | None -> 0.0

    /// Global efficiency of the graph.
    let globalEfficiency
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (toFloat: 'e -> float)
        (graph: Graph<'n, 'e>)
        : float =
        let nodes = allNodes graph
        let n = nodes.Length
        if n <= 1 then
            0.0
        else
            let mutable total = 0.0
            for u in nodes do
                let distances = Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare u graph
                for kvp in distances do
                    if kvp.Key <> u then
                        let val' = toFloat kvp.Value
                        if val' <> 0.0 then
                            total <- total + (1.0 / val')
            total / (float n * float (n - 1))

    /// Local efficiency of a single node.
    let localEfficiency
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (toFloat: 'e -> float)
        (node: NodeId)
        (graph: Graph<'n, 'e>)
        : float =
        let neighbors = neighborIds node graph
        if neighbors.Length <= 1 then
            0.0
        else
            let sub = Yog.Transform.subgraph neighbors graph
            globalEfficiency zero add compare toFloat sub

    /// Average local efficiency over all nodes.
    let averageLocalEfficiency
        (zero: 'e)
        (add: 'e -> 'e -> 'e)
        (compare: 'e -> 'e -> int)
        (toFloat: 'e -> float)
        (graph: Graph<'n, 'e>)
        : float =
        let nodes = allNodes graph
        let n = nodes.Length
        if n = 0 then
            0.0
        else
            let mutable total = 0.0
            for node in nodes do
                total <- total + localEfficiency zero add compare toFloat node graph
            total / float n
