/// Centrality measures for identifying important nodes in graphs.
///
/// Provides degree, closeness, harmonic, betweenness, PageRank, eigenvector, Katz,
/// and alpha centrality measures. All functions return a Map<NodeId, float> mapping
/// nodes to their centrality scores.
///
/// ## Overview
///
/// | Measure | Function | Best For | Complexity |
/// |---------|----------|----------|------------|
/// | Degree | degree | Local connectivity | O(V + E) |
/// | Closeness | closeness | Distance to all others | O(V(V+E)logV) |
/// | Harmonic | harmonicCentrality | Disconnected graphs | O(V(V+E)logV) |
/// | Betweenness | betweenness | Bridge/gatekeeper detection | O(VE) |
/// | PageRank | pagerank | Link-quality importance | O(iterations×(V+E)) |
/// | Eigenvector | eigenvector | Neighbor importance | O(iterations×(V+E)) |
/// | Katz | katz | Attenuated paths | O(iterations×(V+E)) |
/// | Alpha | alphaCentrality | Directed influence | O(iterations×(V+E)) |
module Yog.Centrality

open System.Collections.Generic
open Yog.Model

/// A mapping of Node IDs to their calculated centrality scores.
type Centrality = Map<NodeId, float>

/// Specifies which edges to consider for directed graphs.
type DegreeMode =
    | InDegree
    | OutDegree
    | TotalDegree

/// Calculates the Degree Centrality for all nodes in the graph.
let degree (mode: DegreeMode) (graph: Graph<'n, 'e>) : Centrality =
    let n = nodeCount graph
    let factor = if n > 1 then float (n - 1) else 1.0

    allNodes graph
    |> List.map (fun id ->
        let count =
            match graph.Kind with
            | Undirected -> (neighbors id graph).Length
            | Directed ->
                match mode with
                | InDegree -> (predecessors id graph).Length
                | OutDegree -> (successors id graph).Length
                | TotalDegree ->
                    (successors id graph).Length
                    + (predecessors id graph).Length

        id, float count / factor)
    |> Map.ofList

/// Calculates Closeness Centrality for all nodes.
///
/// Closeness centrality measures how close a node is to all other nodes.
/// Formula: C(v) = (n - 1) / Σ d(v, u) for all u ≠ v
///
/// Note: In disconnected graphs, nodes that cannot reach all others get 0.0.
/// Consider using harmonicCentrality for disconnected graphs.
///
/// Time Complexity: O(V * (V + E) log V) using Dijkstra from each node
let closeness
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (toFloat: 'e -> float)
    (graph: Graph<'n, 'e>)
    : Centrality =
    let nodes = allNodes graph
    let n = nodes.Length

    if n <= 1 then
        nodes
        |> List.map (fun id -> id, 0.0)
        |> Map.ofList
    else
        nodes
        |> List.map (fun source ->
            let distances =
                Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare source graph

            if distances.Count < n then
                source, 0.0
            else
                let totalDistance =
                    distances
                    |> Map.fold (fun acc _ -> add acc) zero

                source, float (n - 1) / toFloat totalDistance)
        |> Map.ofList

/// Calculates Harmonic Centrality for all nodes.
///
/// Harmonic centrality is a variation of closeness that handles disconnected graphs
/// gracefully. It sums the reciprocals of shortest path distances.
///
/// Formula: H(v) = Σ (1 / d(v, u)) / (n - 1) for all reachable u ≠ v
///
/// Time Complexity: O(V * (V + E) log V)
let harmonicCentrality
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (toFloat: 'e -> float)
    (graph: Graph<'n, 'e>)
    : Centrality =
    let nodes = allNodes graph
    let n = nodes.Length

    if n <= 1 then
        nodes
        |> List.map (fun id -> id, 0.0)
        |> Map.ofList
    else
        let denominator = float (n - 1)

        nodes
        |> List.map (fun source ->
            let distances =
                Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare source graph

            let sumOfReciprocals =
                distances
                |> Map.fold
                    (fun sum node dist ->
                        if node = source then
                            sum
                        else
                            let d = toFloat dist

                            if d > 0.0 then
                                sum + (1.0 / d)
                            else
                                sum)
                    0.0

            source, sumOfReciprocals / denominator)
        |> Map.ofList

/// Calculates Betweenness Centrality using Brandes' Algorithm.
let betweenness (zero: 'e) (add: 'e -> 'e -> 'e) (compare: 'e -> 'e -> int) (graph: Graph<'n, 'e>) : Centrality =
    let nodes = allNodes graph
    let cb = Dictionary<NodeId, float>()

    for n in nodes do
        cb.[n] <- 0.0

    for s in nodes do
        // Discovery phase (Dijkstra-based for weighted)
        let stack = Stack<NodeId>()
        let preds = Dictionary<NodeId, NodeId list>()
        let sigma = Dictionary<NodeId, float>()
        let dists = Dictionary<NodeId, 'e>()

        for n in nodes do
            sigma.[n] <- 0.0
            preds.[n] <- []

        sigma.[s] <- 1.0
        dists.[s] <- zero

        let comparer =
            { new IComparer<'e> with
                member _.Compare(a, b) = compare a b }

        let pq = PriorityQueue<NodeId, 'e>(comparer)
        pq.Enqueue(s, zero)

        while pq.Count > 0 do
            let v = pq.Dequeue()

            // Skip stale entries - if we already found a better path to v, ignore this entry
            if compare dists.[v] dists.[v] = 0 then
                stack.Push(v)

                for (w, weight) in successors v graph do
                    let newDist = add dists.[v] weight

                    if not (dists.ContainsKey(w)) then
                        dists.[w] <- newDist
                        pq.Enqueue(w, newDist)
                        sigma.[w] <- sigma.[w] + sigma.[v]
                        preds.[w] <- [ v ]
                    else
                        let cmp = compare newDist dists.[w]

                        if cmp < 0 then
                            // Found a better path - update distance and re-enqueue
                            dists.[w] <- newDist
                            pq.Enqueue(w, newDist)
                            sigma.[w] <- sigma.[v]
                            preds.[w] <- [ v ]
                        elif cmp = 0 then
                            // Found an equally good path - accumulate sigma
                            sigma.[w] <- sigma.[w] + sigma.[v]
                            preds.[w] <- v :: preds.[w]

        // Accumulation phase
        let delta = Dictionary<NodeId, float>()

        for n in nodes do
            delta.[n] <- 0.0

        while stack.Count > 0 do
            let w = stack.Pop()

            for v in preds.[w] do
                delta.[v] <-
                    delta.[v]
                    + (sigma.[v] / sigma.[w]) * (1.0 + delta.[w])

            if w <> s then
                cb.[w] <- cb.[w] + delta.[w]

    let factor =
        if graph.Kind = Undirected then
            0.5
        else
            1.0

    cb
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value * factor)
    |> Map.ofSeq

/// PageRank options for convergence.
type PageRankOptions =
    { Damping: float
      MaxIterations: int
      Tolerance: float }

/// Calculates PageRank Centrality for all nodes.
let pagerank (options: PageRankOptions) (graph: Graph<'n, 'e>) : Centrality =
    let nodes = allNodes graph
    let n = nodes.Length
    let nFloat = float n
    let mutable ranks = Dictionary<NodeId, float>()

    for node in nodes do
        ranks.[node] <- 1.0 / nFloat

    let mutable converged = false
    let mutable iteration = 0

    while iteration < options.MaxIterations && not converged do
        let newRanks = Dictionary<NodeId, float>()

        // Handle sinks (nodes with no out-edges)
        let mutable sinkSum = 0.0

        for node in nodes do
            if (successors node graph).Length = 0 then
                sinkSum <- sinkSum + ranks.[node] / nFloat

        for node in nodes do
            let mutable rankSum = 0.0

            let inNodes =
                if graph.Kind = Undirected then
                    successors node graph
                else
                    predecessors node graph

            for (neighbor, _) in inNodes do
                let outDeg =
                    float (
                        if graph.Kind = Undirected then
                            (neighbors neighbor graph).Length
                        else
                            (successors neighbor graph).Length
                    )

                rankSum <- rankSum + ranks.[neighbor] / outDeg

            newRanks.[node] <-
                ((1.0 - options.Damping) / nFloat)
                + options.Damping * (sinkSum + rankSum)

        // Check convergence (L1 norm)
        let mutable diff = 0.0

        for node in nodes do
            diff <- diff + abs (newRanks.[node] - ranks.[node])

        ranks <- newRanks

        if diff < options.Tolerance then
            converged <- true

        iteration <- iteration + 1

    ranks
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    |> Map.ofSeq

/// Calculates Eigenvector Centrality for all nodes.
/// Node importance is based on the importance of its neighbors.
let eigenvector (maxIterations: int) (tolerance: float) (graph: Graph<'n, 'e>) : Map<NodeId, float> =
    let nodes = allNodes graph
    let n = nodes.Length

    if n <= 1 then
        nodes
        |> List.map (fun id -> id, 1.0)
        |> Map.ofList
    else
        let mutable scores = Dictionary<NodeId, float>()
        let startVal = 1.0 / float n

        for id in nodes do
            scores.[id] <- startVal

        let mutable iteration = 0
        let mutable converged = false

        while iteration < maxIterations && not converged do
            let nextScores = Dictionary<NodeId, float>()

            // 1. Compute new scores: x_v = Σ x_u for neighbors/predecessors u
            for node in nodes do
                let mutable sum = 0.0

                let inNodes =
                    if graph.Kind = Undirected then
                        successors node graph
                    else
                        predecessors node graph

                for (neighbor, _) in inNodes do
                    sum <- sum + scores.[neighbor]

                nextScores.[node] <- sum

            // 2. Calculate L2 Norm for normalization
            let mutable l2Norm = 0.0

            for s in nextScores.Values do
                l2Norm <- l2Norm + (s * s)

            l2Norm <- sqrt l2Norm

            // 3. Normalize and check L2 difference for convergence
            let mutable l2Diff = 0.0

            if l2Norm > 0.0 then
                for node in nodes do
                    let normalized = nextScores.[node] / l2Norm
                    let diff = normalized - scores.[node]
                    l2Diff <- l2Diff + (diff * diff)
                    nextScores.[node] <- normalized

            scores <- nextScores

            if sqrt l2Diff < tolerance then
                converged <- true

            iteration <- iteration + 1

        scores
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Map.ofSeq

/// Calculates Katz Centrality for all nodes.
///
/// Katz centrality adds an attenuation factor (alpha) to prevent infinite accumulation
/// in cycles, plus a constant term (beta) for base centrality.
///
/// Formula: C(v) = α * Σ C(u) + β for all neighbors u
///
/// Time Complexity: O(max_iterations * (V + E))
///
/// Parameters:
/// - alpha: Attenuation factor (must be < 1/largest_eigenvalue, typically 0.1-0.3)
/// - beta: Base centrality (typically 1.0)
let katz
    (alpha: float)
    (beta: float)
    (maxIterations: int)
    (tolerance: float)
    (graph: Graph<'n, 'e>)
    : Map<NodeId, float> =
    let nodes = allNodes graph
    let n = nodes.Length

    if n = 0 then
        Map.empty
    else
        let mutable scores = Dictionary<NodeId, float>()

        for id in nodes do
            scores.[id] <- beta

        let mutable iteration = 0
        let mutable converged = false

        while iteration < maxIterations && not converged do
            let nextScores = Dictionary<NodeId, float>()

            for node in nodes do
                let mutable neighborSum = 0.0

                let inNodes =
                    if graph.Kind = Undirected then
                        successors node graph
                    else
                        predecessors node graph

                for (neighbor, _) in inNodes do
                    neighborSum <- neighborSum + scores.[neighbor]

                nextScores.[node] <- (alpha * neighborSum) + beta

            // L1 Norm difference for convergence
            let mutable l1Diff = 0.0

            for node in nodes do
                l1Diff <- l1Diff + abs (nextScores.[node] - scores.[node])

            scores <- nextScores

            if l1Diff < tolerance then
                converged <- true

            iteration <- iteration + 1

        scores
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Map.ofSeq

/// Calculates Alpha Centrality for all nodes.
///
/// Alpha centrality is a generalization of Katz centrality for directed graphs.
/// Unlike Katz, it does not include a constant beta term.
///
/// Formula: C(v) = α * Σ C(u) for all predecessors u (or neighbors for undirected)
///
/// Time Complexity: O(max_iterations * (V + E))
///
/// Parameters:
/// - alpha: Attenuation factor (typically 0.1-0.5)
/// - initial: Initial centrality value for all nodes
let alphaCentrality
    (alpha: float)
    (initial: float)
    (maxIterations: int)
    (tolerance: float)
    (graph: Graph<'n, 'e>)
    : Map<NodeId, float> =
    let nodes = allNodes graph
    let n = nodes.Length

    if n = 0 then
        Map.empty
    else
        let mutable scores = Dictionary<NodeId, float>()

        for id in nodes do
            scores.[id] <- initial

        let mutable iteration = 0
        let mutable converged = false

        while iteration < maxIterations && not converged do
            let nextScores = Dictionary<NodeId, float>()

            for node in nodes do
                let mutable neighborSum = 0.0

                let inNodes =
                    if graph.Kind = Undirected then
                        successors node graph
                    else
                        predecessors node graph

                for (neighbor, _) in inNodes do
                    neighborSum <- neighborSum + scores.[neighbor]

                nextScores.[node] <- alpha * neighborSum

            // L1 Norm difference for convergence
            let mutable l1Diff = 0.0

            for node in nodes do
                l1Diff <- l1Diff + abs (nextScores.[node] - scores.[node])

            scores <- nextScores

            if l1Diff < tolerance then
                converged <- true

            iteration <- iteration + 1

        scores
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Map.ofSeq

// -----------------------------------------------------------------------------
// Convenience Wrappers
// -----------------------------------------------------------------------------

let degreeTotal graph = degree TotalDegree graph

let closenessInt graph = closeness 0 (+) compare float graph

let closenessFloat graph = closeness 0.0 (+) compare id graph

let harmonicCentralityInt graph = harmonicCentrality 0 (+) compare float graph

let harmonicCentralityFloat graph = harmonicCentrality 0.0 (+) compare id graph

let betweennessInt graph = betweenness 0 (+) compare graph

let betweennessFloat graph = betweenness 0.0 (+) compare graph

let defaultPageRankOptions =
    { Damping = 0.85
      MaxIterations = 100
      Tolerance = 0.0001 }
