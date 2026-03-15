/// A* (A-Star) search algorithm for optimal pathfinding with heuristic guidance.
///
/// A* is an informed search algorithm that finds the shortest path from a start node
/// to a goal node using a heuristic function to guide exploration. It combines the
/// completeness of Dijkstra's algorithm with the efficiency of greedy best-first search.
///
/// ## Algorithm
///
/// | Algorithm | Function | Complexity | Best For |
/// |-----------|----------|------------|----------|
/// | A* Search | aStar | O((V + E) log V) | Pathfinding with good heuristics |
/// | Implicit A* | implicitAStar | O((V + E) log V) | Large/infinite graphs generated on-demand |
///
/// ## Key Concepts
///
/// - **Evaluation Function**: f(n) = g(n) + h(n)
///   - g(n): Actual cost from start to node n
///   - h(n): Heuristic estimate from n to goal
///   - f(n): Estimated total cost through n
/// - **Admissible Heuristic**: h(n) ≤ actual cost (never overestimates)
/// - **Consistent Heuristic**: h(n) ≤ cost(n→n') + h(n') (triangle inequality)
///
/// ## When to Use A*
///
/// **Use A* when:**
/// - You have a specific goal node (not single-source to all)
/// - You can provide a good heuristic estimate
/// - The heuristic is admissible (underestimates)
///
/// **Use Dijkstra when:**
/// - No good heuristic available (h(n) = 0 reduces A* to Dijkstra)
/// - You need shortest paths to all nodes from a source
///
/// ## Heuristic Examples
///
/// | Domain | Heuristic | Admissible? |
/// |--------|-----------|-------------|
/// | Grid (4-way) | Manhattan distance | Yes |
/// | Grid (8-way) | Chebyshev distance | Yes |
/// | Geospatial | Haversine/great-circle | Yes |
/// | Road networks | Precomputed landmarks | Yes |
///
/// ## Use Cases
///
/// - Video games: NPC pathfinding on game maps
/// - GPS navigation: Route planning with distance estimates
/// - Robotics: Motion planning with obstacle avoidance
/// - Puzzle solving: Sliding puzzles, mazes, labyrinths
module Yog.Pathfinding.AStar

open System.Collections.Generic
open Yog.Model
open Yog.Pathfinding.Utils

/// Finds the shortest path using A* search with a heuristic function.
/// Time Complexity: O((V + E) log V)
let aStar
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (heuristic: NodeId -> NodeId -> 'e)
    (start: NodeId)
    (goal: NodeId)
    (graph: Graph<'n, 'e>)
    : Path<'e> option =
    // Create an IComparer wrapper for the fScore priority
    let comparer =
        { new IComparer<'e> with
            member _.Compare(a, b) = compare a b }

    // Priority queue element: (gScore, path). Priority: fScore
    let pq = PriorityQueue<'e * NodeId list, 'e>(comparer)

    // Dictionary tracks the best known gScore for each node
    let gScores = Dictionary<NodeId, 'e>()

    let initialFScore = heuristic start goal
    pq.Enqueue((zero, [ start ]), initialFScore)
    gScores.[start] <- zero

    let mutable result = None
    let mutable found = false

    while pq.Count > 0 && not found do
        let (dist, path) = pq.Dequeue()
        let current = List.head path

        // Check for staleness: if we already found a strictly better path to 'current', ignore this entry
        let mutable bestKnown = zero

        if
            gScores.TryGetValue(current, &bestKnown)
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
                    not (gScores.TryGetValue(nextId, &nextBestKnown))
                    || compare nextDist nextBestKnown < 0
                then
                    gScores.[nextId] <- nextDist

                    let fScore = add nextDist (heuristic nextId goal)
                    pq.Enqueue((nextDist, nextId :: path), fScore)

    result

/// Finds the shortest path in an implicit graph using A* search with a heuristic.
let implicitAStar
    (zero: 'cost)
    (add: 'cost -> 'cost -> 'cost)
    (compare: 'cost -> 'cost -> int)
    (heuristic: 'state -> 'cost)
    (successors: 'state -> ('state * 'cost) list)
    (isGoal: 'state -> bool)
    (start: 'state)
    : 'cost option =
    let comparer =
        { new IComparer<'cost> with
            member _.Compare(a, b) = compare a b }

    let pq = PriorityQueue<'cost * 'state, 'cost>(comparer)
    let gScores = Dictionary<'state, 'cost>()

    let initialFScore = heuristic start
    pq.Enqueue((zero, start), initialFScore)
    gScores.[start] <- zero

    let mutable result = None
    let mutable found = false

    while pq.Count > 0 && not found do
        let (dist, current) = pq.Dequeue()

        let mutable bestKnown = zero

        if
            gScores.TryGetValue(current, &bestKnown)
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
                    not (gScores.TryGetValue(nextState, &nextBestKnown))
                    || compare nextDist nextBestKnown < 0
                then
                    gScores.[nextState] <- nextDist
                    let fScore = add nextDist (heuristic nextState)
                    pq.Enqueue((nextDist, nextState), fScore)

    result

/// Like `implicitAStar`, but deduplicates visited states by a custom key.
let implicitAStarBy
    (zero: 'cost)
    (add: 'cost -> 'cost -> 'cost)
    (compare: 'cost -> 'cost -> int)
    (heuristic: 'state -> 'cost)
    (successors: 'state -> ('state * 'cost) list)
    (keyFn: 'state -> 'key)
    (isGoal: 'state -> bool)
    (start: 'state)
    : 'cost option =
    let comparer =
        { new IComparer<'cost> with
            member _.Compare(a, b) = compare a b }

    let pq = PriorityQueue<'cost * 'state, 'cost>(comparer)
    let gScores = Dictionary<'key, 'cost>()

    let startKey = keyFn start
    let initialFScore = heuristic start
    pq.Enqueue((zero, start), initialFScore)
    gScores.[startKey] <- zero

    let mutable result = None
    let mutable found = false

    while pq.Count > 0 && not found do
        let (dist, current) = pq.Dequeue()
        let currentKey = keyFn current

        let mutable bestKnown = zero

        if
            gScores.TryGetValue(currentKey, &bestKnown)
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
                    not (gScores.TryGetValue(nextKey, &nextBestKnown))
                    || compare nextDist nextBestKnown < 0
                then
                    gScores.[nextKey] <- nextDist
                    let fScore = add nextDist (heuristic nextState)
                    pq.Enqueue((nextDist, nextState), fScore)

    result

// -----------------------------------------------------------------------------
// CONVENIENCE WRAPPERS FOR COMMON TYPES
// -----------------------------------------------------------------------------

/// Finds the shortest path using A* with integer weights.
let aStarInt
    (heuristic: NodeId -> NodeId -> int)
    (start: NodeId)
    (goal: NodeId)
    (graph: Graph<'n, int>)
    : Path<int> option =
    aStar 0 (+) compare heuristic start goal graph

/// Finds the shortest path using A* with float weights.
let aStarFloat
    (heuristic: NodeId -> NodeId -> float)
    (start: NodeId)
    (goal: NodeId)
    (graph: Graph<'n, float>)
    : Path<float> option =
    aStar 0.0 (+) compare heuristic start goal graph
