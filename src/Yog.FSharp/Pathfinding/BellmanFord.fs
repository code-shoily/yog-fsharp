/// Bellman-Ford algorithm for single-source shortest paths with support for negative edge weights.
/// 
/// The Bellman-Ford algorithm finds shortest paths from a source node to all other nodes,
/// even when edges have negative weights. It can also detect negative cycles reachable
/// from the source, which make shortest paths undefined.
/// 
/// ## Algorithm
/// 
/// | Algorithm | Function | Complexity | Best For |
/// |-----------|----------|------------|----------|
/// | Bellman-Ford | bellmanFord | O(VE) | Negative weights, cycle detection |
/// | SPFA (Queue-optimized) | implicitBellmanFord | O(E) average | Sparse graphs with few negative edges |
/// 
/// ## Key Concepts
/// 
/// - **Relaxation**: Repeatedly improve distance estimates (V-1 passes)
/// - **Negative Cycle**: Cycle with total negative weight (no shortest path exists)
/// - **Shortest Path Tree**: Tree of shortest paths from source to all nodes
/// 
/// ## Why V-1 Relaxation Passes?
/// 
/// In a graph with V nodes, any shortest path has at most V-1 edges.
/// Each pass of Bellman-Ford relaxes all edges, propagating shortest
/// path information one hop further each time.
/// 
/// ## Comparison with Dijkstra
/// 
/// | Feature | Bellman-Ford | Dijkstra |
/// |---------|--------------|----------|
/// | Negative weights | ✓ Yes | ✗ No |
/// | Negative cycle detection | ✓ Yes | ✗ N/A |
/// | Time complexity | O(VE) | O((V+E) log V) |
/// | Data structure | Simple loops | Priority queue |
/// 
/// ## Use Cases
/// 
/// - Currency arbitrage: Detecting negative cycles in exchange rates
/// - Financial modeling: Cost calculations with credits/penalties
/// - Chemical reactions: Energy changes with positive and negative values
/// - Constraint solving: Difference constraints systems
/// 
/// ## History
/// 
/// Published independently by Richard Bellman (1958) and Lester Ford Jr. (1956).
/// The algorithm is a classic example of dynamic programming.
module Yog.Pathfinding.BellmanFord

open System.Collections.Generic
open Yog.Model
open Yog.Pathfinding.Utils

/// Result type for Bellman-Ford algorithm.
type BellmanFordResult<'e> =
    /// A shortest path was found successfully
    | ShortestPath of Path<'e>
    /// A negative cycle was detected (reachable from source)
    | NegativeCycle
    /// No path exists from start to goal
    | NoPath

/// Result type for implicit Bellman-Ford algorithm.
type ImplicitBellmanFordResult<'cost> =
    /// A shortest distance to goal was found
    | FoundGoal of 'cost
    /// A negative cycle was detected (reachable from start)
    | DetectedNegativeCycle
    /// No goal state was reached
    | NoGoal

/// Finds shortest path with support for negative edge weights using Bellman-Ford.
/// Time Complexity: O(VE)
let bellmanFord
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (start: NodeId)
    (goal: NodeId)
    (graph: Graph<'n, 'e>)
    : BellmanFordResult<'e> =
    let distances = Dictionary<NodeId, 'e>()
    let predecessors = Dictionary<NodeId, NodeId>()
    let allNodesList = allNodes graph
    let nodeCount = allNodesList.Length

    distances.[start] <- zero

    // V - 1 relaxation passes with early-exit optimization
    let mutable anyChanged = true
    let mutable iteration = 0

    while iteration < nodeCount - 1 && anyChanged do
        anyChanged <- false

        for u in allNodesList do
            let mutable uDist = zero

            if distances.TryGetValue(u, &uDist) then
                for (v, weight) in successors u graph do
                    let newDist = add uDist weight
                    let mutable vDist = zero

                    if
                        not (distances.TryGetValue(v, &vDist))
                        || compare newDist vDist < 0
                    then
                        distances.[v] <- newDist
                        predecessors.[v] <- u
                        anyChanged <- true

        iteration <- iteration + 1

    // Pass V: Check for negative weight cycles
    let mutable hasNegativeCycle = false

    for u in allNodesList do
        let mutable uDist = zero

        if
            not hasNegativeCycle
            && distances.TryGetValue(u, &uDist)
        then
            for (v, weight) in successors u graph do
                let newDist = add uDist weight
                let mutable vDist = zero

                if
                    distances.TryGetValue(v, &vDist)
                    && compare newDist vDist < 0
                then
                    hasNegativeCycle <- true

    if hasNegativeCycle then
        NegativeCycle
    else
        let mutable goalDist = zero

        if not (distances.TryGetValue(goal, &goalDist)) then
            NoPath
        else
            // Reconstruct path
            let mutable current = goal
            let mutable pathNodes = [ goal ]
            let mutable noPath = false

            while current <> start && not noPath do
                let mutable pred = 0

                if predecessors.TryGetValue(current, &pred) then
                    pathNodes <- pred :: pathNodes
                    current <- pred
                else
                    noPath <- true

            if noPath then
                NoPath
            else
                ShortestPath
                    { Nodes = pathNodes
                      TotalWeight = goalDist }

/// Finds shortest path in implicit graphs with support for negative edge weights.
/// Uses SPFA (Shortest Path Faster Algorithm) internally.
let implicitBellmanFord
    (zero: 'cost)
    (add: 'cost -> 'cost -> 'cost)
    (compare: 'cost -> 'cost -> int)
    (successors: 'state -> ('state * 'cost) list)
    (isGoal: 'state -> bool)
    (start: 'state)
    : ImplicitBellmanFordResult<'cost> =
    let q = Queue<'state>()
    let distances = Dictionary<'state, 'cost>()
    let relaxCounts = Dictionary<'state, int>()
    let inQueue = HashSet<'state>()

    q.Enqueue(start)
    distances.[start] <- zero
    relaxCounts.[start] <- 0
    inQueue.Add(start) |> ignore

    let mutable hasNegativeCycle = false

    while q.Count > 0 && not hasNegativeCycle do
        let current = q.Dequeue()
        inQueue.Remove(current) |> ignore

        let currentDist = distances.[current]

        for (nextState, edgeCost) in successors current do
            let newDist = add currentDist edgeCost
            let mutable prevDist = zero

            if
                not (distances.TryGetValue(nextState, &prevDist))
                || compare newDist prevDist < 0
            then
                distances.[nextState] <- newDist

                let mutable count = 0

                relaxCounts.TryGetValue(nextState, &count)
                |> ignore

                relaxCounts.[nextState] <- count + 1

                // If a node is relaxed more times than the number of discovered nodes, a negative cycle exists
                if relaxCounts.[nextState] > distances.Count then
                    hasNegativeCycle <- true
                elif not (inQueue.Contains(nextState)) then
                    q.Enqueue(nextState)
                    inQueue.Add(nextState) |> ignore

    if hasNegativeCycle then
        DetectedNegativeCycle
    else
        // Find the goal with the minimum distance
        let mutable minGoalDist = None

        for kvp in distances do
            if isGoal kvp.Key then
                match minGoalDist with
                | None -> minGoalDist <- Some kvp.Value
                | Some existing ->
                    if compare kvp.Value existing < 0 then
                        minGoalDist <- Some kvp.Value

        match minGoalDist with
        | Some dist -> FoundGoal dist
        | None -> NoGoal

/// Like `implicitBellmanFord`, but deduplicates visited states by a custom key.
let implicitBellmanFordBy
    (zero: 'cost)
    (add: 'cost -> 'cost -> 'cost)
    (compare: 'cost -> 'cost -> int)
    (successors: 'state -> ('state * 'cost) list)
    (keyFn: 'state -> 'key)
    (isGoal: 'state -> bool)
    (start: 'state)
    : ImplicitBellmanFordResult<'cost> =
    let q = Queue<'state>()
    let distances = Dictionary<'key, 'cost * 'state>()
    let relaxCounts = Dictionary<'key, int>()
    let inQueue = HashSet<'state>()

    let startKey = keyFn start
    q.Enqueue(start)
    distances.[startKey] <- (zero, start)
    relaxCounts.[startKey] <- 0
    inQueue.Add(start) |> ignore

    let mutable hasNegativeCycle = false

    while q.Count > 0 && not hasNegativeCycle do
        let current = q.Dequeue()
        let currentKey = keyFn current
        inQueue.Remove(current) |> ignore

        let (currentDist, _) = distances.[currentKey]

        for (nextState, edgeCost) in successors current do
            let nextKey = keyFn nextState
            let newDist = add currentDist edgeCost

            let mutable prevEntry = (zero, start) // Dummy initial value
            let hasPrev = distances.TryGetValue(nextKey, &prevEntry)
            let (prevDist, _) = prevEntry

            if not hasPrev || compare newDist prevDist < 0 then
                distances.[nextKey] <- (newDist, nextState)

                let mutable count = 0
                relaxCounts.TryGetValue(nextKey, &count) |> ignore
                relaxCounts.[nextKey] <- count + 1

                if relaxCounts.[nextKey] > distances.Count then
                    hasNegativeCycle <- true
                elif not (inQueue.Contains(nextState)) then
                    q.Enqueue(nextState)
                    inQueue.Add(nextState) |> ignore

    if hasNegativeCycle then
        DetectedNegativeCycle
    else
        let mutable minGoalDist = None

        for kvp in distances do
            let (dist, state) = kvp.Value

            if isGoal state then
                match minGoalDist with
                | None -> minGoalDist <- Some dist
                | Some existing ->
                    if compare dist existing < 0 then
                        minGoalDist <- Some dist

        match minGoalDist with
        | Some dist -> FoundGoal dist
        | None -> NoGoal

// -----------------------------------------------------------------------------
// CONVENIENCE WRAPPERS FOR COMMON TYPES
// -----------------------------------------------------------------------------

/// Finds shortest path with integer weights, handling negative edges.
let bellmanFordInt (start: NodeId) (goal: NodeId) (graph: Graph<'n, int>) : BellmanFordResult<int> =
    bellmanFord 0 (+) compare start goal graph

/// Finds shortest path with float weights, handling negative edges.
let bellmanFordFloat (start: NodeId) (goal: NodeId) (graph: Graph<'n, float>) : BellmanFordResult<float> =
    bellmanFord 0.0 (+) compare start goal graph
