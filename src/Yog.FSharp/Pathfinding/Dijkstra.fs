/// Dijkstra's algorithm for single-source shortest paths in graphs with non-negative edge weights.
/// 
/// Dijkstra's algorithm finds the shortest path from a source node to all other reachable
/// nodes in a graph. It works by maintaining a priority queue of nodes to visit,
/// always expanding the node with the smallest known distance.
/// 
/// ## Algorithm
/// 
/// | Algorithm | Function | Complexity | Best For |
/// |-----------|----------|------------|----------|
/// | Dijkstra (single-target) | shortestPath | O((V + E) log V) | One-to-one shortest path |
/// | Dijkstra (single-source) | singleSourceDistances | O((V + E) log V) | One-to-all shortest paths |
/// | Implicit Dijkstra | implicitDijkstra | O((V + E) log V) | Large/infinite graphs |
/// 
/// ## History
/// 
/// Edsger W. Dijkstra published this algorithm in 1959. The original paper described
/// it for finding the shortest path between two nodes, but it's commonly used for
/// single-source shortest paths to all nodes.
/// 
/// ## Use Cases
/// 
/// - Network routing: OSPF, IS-IS protocols use Dijkstra
/// - Map services: Shortest driving directions
/// - Social networks: Degrees of separation
/// - Game development: Shortest path on weighted grids
module Yog.Pathfinding.Dijkstra

open System.Collections.Generic
open Yog.Model
open Yog.Pathfinding.Utils

/// Finds the shortest path between two nodes using Dijkstra's algorithm.
/// Works with non-negative edge weights only.
/// Time Complexity: O((V + E) log V)
let shortestPath
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (start: NodeId)
    (goal: NodeId)
    (graph: Graph<'n, 'e>)
    : Path<'e> option =
    let comparer =
        { new IComparer<'e> with
            member _.Compare(a, b) = compare a b }

    let pq = PriorityQueue<'e * NodeId list, 'e>(comparer)
    let distances = Dictionary<NodeId, 'e>()

    pq.Enqueue((zero, [ start ]), zero)
    distances.[start] <- zero

    let mutable result = None
    let mutable found = false

    while pq.Count > 0 && not found do
        let (dist, path) = pq.Dequeue()
        let current = List.head path

        // Skip stale entries: if we already found a strictly better path to 'current', ignore this one
        let mutable bestKnown = zero

        if
            distances.TryGetValue(current, &bestKnown)
            && compare dist bestKnown > 0
        then
            ()
        elif current = goal then
            result <-
                Some
                    { Nodes = List.rev path
                      TotalWeight = dist }

            found <- true
        else
            for (nextId, weight) in successors current graph do
                let nextDist = add dist weight
                let mutable nextBestKnown = zero

                // If we haven't visited nextId, or we found a cheaper way to get there
                if
                    not (distances.TryGetValue(nextId, &nextBestKnown))
                    || compare nextDist nextBestKnown < 0
                then
                    distances.[nextId] <- nextDist
                    pq.Enqueue((nextDist, nextId :: path), nextDist)

    result

/// Computes shortest distances from a source node to all reachable nodes.
/// Returns a Map of each reachable node to its shortest distance.
let singleSourceDistances
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (source: NodeId)
    (graph: Graph<'n, 'e>)
    : Map<NodeId, 'e> =
    let comparer =
        { new IComparer<'e> with
            member _.Compare(a, b) = compare a b }

    let pq = PriorityQueue<'e * NodeId, 'e>(comparer)
    let distances = Dictionary<NodeId, 'e>()

    pq.Enqueue((zero, source), zero)
    distances.[source] <- zero

    while pq.Count > 0 do
        let (dist, current) = pq.Dequeue()

        let mutable bestKnown = zero

        if
            not (distances.TryGetValue(current, &bestKnown))
            || compare dist bestKnown <= 0
        then
            for (nextId, weight) in successors current graph do
                let nextDist = add dist weight
                let mutable nextBestKnown = zero

                if
                    not (distances.TryGetValue(nextId, &nextBestKnown))
                    || compare nextDist nextBestKnown < 0
                then
                    distances.[nextId] <- nextDist
                    pq.Enqueue((nextDist, nextId), nextDist)

    // Convert the mutable Dictionary into an immutable F# Map for the caller
    distances
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    |> Map.ofSeq

/// Finds the shortest path in an implicit graph using Dijkstra's algorithm.
let implicitDijkstra
    (zero: 'cost)
    (add: 'cost -> 'cost -> 'cost)
    (compare: 'cost -> 'cost -> int)
    (successors: 'state -> ('state * 'cost) list)
    (isGoal: 'state -> bool)
    (start: 'state)
    : 'cost option =
    let comparer =
        { new IComparer<'cost> with
            member _.Compare(a, b) = compare a b }

    let pq = PriorityQueue<'cost * 'state, 'cost>(comparer)
    let distances = Dictionary<'state, 'cost>()

    pq.Enqueue((zero, start), zero)
    distances.[start] <- zero

    let mutable result = None
    let mutable found = false

    while pq.Count > 0 && not found do
        let (dist, current) = pq.Dequeue()

        let mutable bestKnown = zero

        if
            distances.TryGetValue(current, &bestKnown)
            && compare dist bestKnown > 0
        then
            ()
        elif isGoal current then
            result <- Some dist
            found <- true
        else
            for (nextState, cost) in successors current do
                let nextDist = add dist cost
                let mutable nextBestKnown = zero

                if
                    not (distances.TryGetValue(nextState, &nextBestKnown))
                    || compare nextDist nextBestKnown < 0
                then
                    distances.[nextState] <- nextDist
                    pq.Enqueue((nextDist, nextState), nextDist)

    result

/// Like `implicitDijkstra`, but deduplicates visited states by a custom key.
let implicitDijkstraBy
    (zero: 'cost)
    (add: 'cost -> 'cost -> 'cost)
    (compare: 'cost -> 'cost -> int)
    (successors: 'state -> ('state * 'cost) list)
    (keyFn: 'state -> 'key)
    (isGoal: 'state -> bool)
    (start: 'state)
    : 'cost option =
    let comparer =
        { new IComparer<'cost> with
            member _.Compare(a, b) = compare a b }

    let pq = PriorityQueue<'cost * 'state, 'cost>(comparer)
    let distances = Dictionary<'key, 'cost>()

    let startKey = keyFn start
    pq.Enqueue((zero, start), zero)
    distances.[startKey] <- zero

    let mutable result = None
    let mutable found = false

    while pq.Count > 0 && not found do
        let (dist, current) = pq.Dequeue()
        let currentKey = keyFn current

        let mutable bestKnown = zero

        if
            distances.TryGetValue(currentKey, &bestKnown)
            && compare dist bestKnown > 0
        then
            ()
        elif isGoal current then
            result <- Some dist
            found <- true
        else
            for (nextState, cost) in successors current do
                let nextDist = add dist cost
                let nextKey = keyFn nextState
                let mutable nextBestKnown = zero

                if
                    not (distances.TryGetValue(nextKey, &nextBestKnown))
                    || compare nextDist nextBestKnown < 0
                then
                    distances.[nextKey] <- nextDist
                    pq.Enqueue((nextDist, nextState), nextDist)

    result

// -----------------------------------------------------------------------------
// CONVENIENCE WRAPPERS FOR COMMON TYPES
// -----------------------------------------------------------------------------

/// Finds the shortest path using integer weights.
let shortestPathInt (start: NodeId) (goal: NodeId) (graph: Graph<'n, int>) : Path<int> option =
    shortestPath 0 (+) compare start goal graph

/// Finds the shortest path using float weights.
let shortestPathFloat (start: NodeId) (goal: NodeId) (graph: Graph<'n, float>) : Path<float> option =
    shortestPath 0.0 (+) compare start goal graph

/// Computes shortest distances using integer weights.
let singleSourceDistancesInt (source: NodeId) (graph: Graph<'n, int>) : Map<NodeId, int> =
    singleSourceDistances 0 (+) compare source graph

/// Computes shortest distances using float weights.
let singleSourceDistancesFloat (source: NodeId) (graph: Graph<'n, float>) : Map<NodeId, float> =
    singleSourceDistances 0.0 (+) compare source graph
