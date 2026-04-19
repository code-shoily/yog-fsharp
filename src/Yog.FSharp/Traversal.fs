/// Graph traversal algorithms - systematic exploration of graph structure.
///
/// This module provides fundamental graph traversal algorithms for visiting nodes
/// in a specific order. Traversals are the foundation for most graph algorithms
/// including pathfinding, connectivity analysis, and cycle detection.
///
/// ## Traversal Orders
///
/// | Order | Strategy | Best For |
/// |-------|----------|----------|
/// | [BFS](https://en.wikipedia.org/wiki/Breadth-first_search) | Level-by-level | Shortest path (unweighted), finding neighbors |
/// | [DFS](https://en.wikipedia.org/wiki/Depth-first_search) | Deep exploration | Cycle detection, topological sort, connectivity |
///
/// ## Core Functions
///
/// - `walk` with `BreadthFirst`/`DepthFirst`: Simple traversals returning visited nodes in order
/// - `foldWalk`: Generic traversal with custom fold function and metadata
/// - `topologicalSort` / `lexicographicalTopologicalSort`: Ordering for DAGs
/// - `isCyclic` / `isAcyclic`: Cycle detection via Kahn's algorithm
/// - `implicitFold` / `implicitFoldBy` / `implicitDijkstra`: Traversals for implicit graphs
///
/// ## Walk Control
///
/// The `foldWalk` function provides fine-grained control:
/// - `Continue`: Explore this node's neighbors normally
/// - `Stop`: Skip this node's neighbors but continue traversal
/// - `Halt`: Stop the entire traversal immediately
///
/// ## Time Complexity
///
/// All traversals run in **O(V + E)** linear time, visiting each node and edge
/// at most once. Dijkstra-based traversals are **O((V + E) log V)**.
///
/// ## References
///
/// - [Wikipedia: Graph Traversal](https://en.wikipedia.org/wiki/Graph_traversal)
/// - [CP-Algorithms: DFS/BFS](https://cp-algorithms.com/graph/breadth-first-search.html)
/// - [Wikipedia: Topological Sorting](https://en.wikipedia.org/wiki/Topological_sorting)
module Yog.Traversal

open System.Collections.Generic
open Yog.Model

/// Traversal order for graph walking algorithms.
type Order =
    /// Breadth-First Search: visit all neighbors before going deeper.
    | BreadthFirst
    /// Depth-First Search: visit as deep as possible before backtracking.
    | DepthFirst

/// Control flow for foldWalk traversal.
type WalkControl =
    /// Continue exploring from this node's successors.
    | Continue
    /// Stop exploring from this node (but continue with other queued nodes).
    | Stop
    /// Halt the entire traversal immediately and return the accumulator.
    | Halt

/// Metadata provided during foldWalk / implicitFold traversal.
type WalkMetadata<'nid> =
    {
        /// Distance from the start node (number of edges traversed).
        Depth: int
        /// The parent node that led to this node (None for the start node).
        Parent: 'nid option
    }

// --- Internal Helper for Topological Sort (Kahn's Algorithm) ---

/// Performs a topological sort internally using Kahn's algorithm.
let private doTopologicalSort (graph: Graph<'n, 'e>) =
    let allNodesList = allNodes graph
    let totalNodes = allNodesList.Length

    // Calculate initial in-degrees
    let mutable inDegrees =
        allNodesList
        |> Seq.map (fun id ->
            let degree =
                graph.InEdges
                |> Map.tryFind id
                |> Option.map (fun m -> m.Count)
                |> Option.defaultValue 0

            id, degree)
        |> Map.ofSeq

    let queue = Queue<NodeId>()

    inDegrees
    |> Map.iter (fun id degree ->
        if degree = 0 then
            queue.Enqueue(id))

    let mutable sorted = []

    while queue.Count > 0 do
        let current = queue.Dequeue()
        sorted <- current :: sorted

        let successors = successorIds current graph

        for next in successors do
            let currentDegree = inDegrees |> Map.tryFind next |> Option.defaultValue 0

            let newDegree = currentDegree - 1
            inDegrees <- inDegrees |> Map.add next newDegree

            if newDegree = 0 then
                queue.Enqueue(next)

    if sorted.Length = totalNodes then
        Ok(List.rev sorted)
    else
        Error()

// --- Cycle Detection ---

/// Internally detects a cycle in an undirected graph using DFS.
let private hasUndirectedCycle (graph: Graph<'n, 'e>) =
    let visited = HashSet<NodeId>()

    let rec dfs node parentOpt =
        visited.Add(node) |> ignore

        let neighbors = successorIds node graph
        let mutable hasCycle = false

        use e = (neighbors :> seq<NodeId>).GetEnumerator()

        while e.MoveNext() && not hasCycle do
            let neighbor = e.Current

            if not (visited.Contains(neighbor)) then
                if dfs neighbor (Some node) then
                    hasCycle <- true
            elif parentOpt <> Some neighbor then
                hasCycle <- true

        hasCycle

    allNodes graph
    |> Seq.exists (fun node -> not (visited.Contains(node)) && dfs node None)

/// Determines if a graph contains any cycles.
///
/// For directed graphs, a cycle exists if there is a path from a node back to itself
/// (evaluated efficiently via Kahn's algorithm).
/// For undirected graphs, a cycle exists if there is a path of length >= 3 from a node back to itself,
/// or a self-loop.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
///     isCyclic graph
///     // => true  // Cycle detected
///
let isCyclic (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Directed ->
        match doTopologicalSort graph with
        | Ok _ -> false
        | Error _ -> true
    | Undirected -> hasUndirectedCycle graph

/// Determines if a graph is acyclic (contains no cycles).
///
/// This is the logical opposite of `isCyclic`. For directed graphs, returning
/// `true` means the graph is a Directed Acyclic Graph (DAG).
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
///     isAcyclic graph
///     // => true  // Valid DAG or undirected forest
///
let isAcyclic (graph: Graph<'n, 'e>) : bool = not (isCyclic graph)


// --- Fold Walk Implementations ---

/// Internal BFS implementation for foldWalk.
let private doFoldWalkBfs (graph: Graph<'n, 'e>) startId initial folder =
    let queue = Queue<NodeId * WalkMetadata<NodeId>>()
    let visited = HashSet<NodeId>()

    queue.Enqueue(startId, { Depth = 0; Parent = None })

    let mutable acc = initial
    let mutable halted = false

    while queue.Count > 0 && not halted do
        let (nodeId, metadata) = queue.Dequeue()

        if visited.Add(nodeId) then
            let (control, newAcc) = folder acc nodeId metadata
            acc <- newAcc

            match control with
            | Halt -> halted <- true
            | Stop -> ()
            | Continue ->
                let successors = successorIds nodeId graph

                for nextId in successors do
                    if not (visited.Contains(nextId)) then
                        queue.Enqueue(
                            nextId,
                            { Depth = metadata.Depth + 1
                              Parent = Some nodeId }
                        )

    acc

/// Internal DFS implementation for foldWalk.
let private doFoldWalkDfs (graph: Graph<'n, 'e>) startId initial folder =
    let stack = Stack<NodeId * WalkMetadata<NodeId>>()
    let visited = HashSet<NodeId>()

    stack.Push(startId, { Depth = 0; Parent = None })

    let mutable acc = initial
    let mutable halted = false

    while stack.Count > 0 && not halted do
        let (nodeId, metadata) = stack.Pop()

        if visited.Add(nodeId) then
            let (control, newAcc) = folder acc nodeId metadata
            acc <- newAcc

            match control with
            | Halt -> halted <- true
            | Stop -> ()
            | Continue ->
                // DFS typically pushes successors in reverse order to process them in listed order
                let successors = successorIds nodeId graph |> List.rev

                for nextId in successors do
                    if not (visited.Contains(nextId)) then
                        stack.Push(
                            nextId,
                            { Depth = metadata.Depth + 1
                              Parent = Some nodeId }
                        )

    acc

/// Folds over nodes during graph traversal, accumulating state with metadata.
///
/// This function combines traversal with state accumulation, providing metadata
/// about each visited node (depth and parent). The folder function controls the
/// traversal flow:
///
/// - `Continue`: Explore successors of the current node normally
/// - `Stop`: Skip successors of this node, but continue processing other queued nodes
/// - `Halt`: Stop the entire traversal immediately and return the accumulator
///
/// **Time Complexity:** O(V + E) for both BFS and DFS
///
/// ## Parameters
///
/// - `folder`: Called for each visited node with (accumulator, node_id, metadata).
///   Returns `(WalkControl, new_accumulator)`.
///
/// ## Examples
///
///     // Find all nodes within distance 3 from start
///     let nearby = foldWalk
///                    1
///                    BreadthFirst
///                    Map.empty
///                    (fun acc nodeId meta ->
///                       if meta.Depth <= 3 then
///                         (Continue, Map.add nodeId meta.Depth acc)
///                       else
///                         (Stop, acc))  // Don't explore beyond depth 3
///                    graph
///
///     // Stop immediately when target is found (like walkUntil)
///     let pathToTarget = foldWalk
///                          start
///                          BreadthFirst
///                          []
///                          (fun acc nodeId _meta ->
///                             let newAcc = nodeId :: acc
///                             if nodeId = target then
///                               (Halt, newAcc)   // Stop entire traversal
///                             else
///                               (Continue, newAcc))
///                          graph
///
///     // Build a parent map for path reconstruction
///     let parents = foldWalk
///                     start
///                     BreadthFirst
///                     Map.empty
///                     (fun acc nodeId meta ->
///                        let newAcc =
///                          match meta.Parent with
///                          | Some p -> Map.add nodeId p acc
///                          | None -> acc
///                        (Continue, newAcc))
///                     graph
///
/// ## Use Cases
///
/// - Finding nodes within a certain distance
/// - Building shortest path trees (parent pointers)
/// - Collecting nodes with custom filtering logic
/// - Computing statistics during traversal (depth distribution, etc.)
/// - BFS/DFS with early termination based on accumulated state
let foldWalk
    (start: NodeId)
    (order: Order)
    (initial: 'a)
    (folder: 'a -> NodeId -> WalkMetadata<NodeId> -> WalkControl * 'a)
    (graph: Graph<'n, 'e>)
    : 'a =
    match order with
    | BreadthFirst -> doFoldWalkBfs graph start initial folder
    | DepthFirst -> doFoldWalkDfs graph start initial folder

/// Walks the graph starting from the given node, visiting all reachable nodes.
///
/// Returns a list of NodeIds in the order they were visited.
/// Uses successors to follow directed paths.
///
/// ## Example
///
///     // BFS traversal
///     walk 1 BreadthFirst graph
///     // => [1; 2; 3; 4; 5]
///
///     // DFS traversal
///     walk 1 DepthFirst graph
///     // => [1; 2; 4; 5; 3]
///
let walk (startId: NodeId) (order: Order) (graph: Graph<'n, 'e>) : NodeId list =
    foldWalk startId order [] (fun acc nodeId _ -> (Continue, nodeId :: acc)) graph
    |> List.rev

/// Walks the graph but stops early when a condition is met.
///
/// Traverses the graph until `shouldStop` returns True for a node.
/// Returns all nodes visited including the one that stopped traversal.
///
/// ## Example
///
///     // Stop when we find node 5
///     walkUntil 1 BreadthFirst (fun node -> node = 5) graph
///
let walkUntil (startId: NodeId) (order: Order) (shouldStop: NodeId -> bool) (graph: Graph<'n, 'e>) : NodeId list =
    foldWalk
        startId
        order
        []
        (fun acc nodeId _ ->
            let newAcc = nodeId :: acc

            if shouldStop nodeId then
                (Halt, newAcc)
            else
                (Continue, newAcc))
        graph
    |> List.rev


// --- Implicit Graph Traversal ---

/// Internal BFS traversal engine for implicit graphs.
let private doImplicitBfsBy
    start
    (successors: 'nid -> 'nid list)
    (keyFn: 'nid -> 'key)
    initial
    (folder: 'a -> 'nid -> WalkMetadata<'nid> -> WalkControl * 'a)
    =
    let queue = Queue<'nid * WalkMetadata<'nid>>()
    let visited = HashSet<'key>()

    queue.Enqueue(start, { Depth = 0; Parent = None })

    let mutable acc = initial
    let mutable halted = false

    while queue.Count > 0 && not halted do
        let (node, metadata) = queue.Dequeue()
        let key = keyFn node

        if visited.Add(key) then
            let (control, newAcc) = folder acc node metadata
            acc <- newAcc

            match control with
            | Halt -> halted <- true
            | Stop -> ()
            | Continue ->
                for nextNode in successors node do
                    let nextKey = keyFn nextNode

                    if not (visited.Contains(nextKey)) then
                        queue.Enqueue(
                            nextNode,
                            { Depth = metadata.Depth + 1
                              Parent = Some node }
                        )

    acc

/// Internal DFS traversal engine for implicit graphs.
let private doImplicitDfsBy
    start
    (successors: 'nid -> 'nid list)
    (keyFn: 'nid -> 'key)
    initial
    (folder: 'a -> 'nid -> WalkMetadata<'nid> -> WalkControl * 'a)
    =
    let stack = Stack<'nid * WalkMetadata<'nid>>()
    let visited = HashSet<'key>()

    stack.Push(start, { Depth = 0; Parent = None })

    let mutable acc = initial
    let mutable halted = false

    while stack.Count > 0 && not halted do
        let (node, metadata) = stack.Pop()
        let key = keyFn node

        if visited.Add(key) then
            let (control, newAcc) = folder acc node metadata
            acc <- newAcc

            match control with
            | Halt -> halted <- true
            | Stop -> ()
            | Continue ->
                for nextNode in List.rev (successors node) do
                    let nextKey = keyFn nextNode

                    if not (visited.Contains(nextKey)) then
                        stack.Push(
                            nextNode,
                            { Depth = metadata.Depth + 1
                              Parent = Some node }
                        )

    acc

/// Like `implicitFold`, but deduplicates visited nodes by a custom key.
///
/// This is essential when your node type carries extra state beyond what
/// defines "identity". For example, in state-space search you might have
/// `(Position * Mask)` nodes, but only want to visit each `Position` once —
/// the `Mask` is just carried state, not part of the identity.
///
/// The `visitedBy` function extracts the deduplication key from each node.
/// Internally, a `HashSet<key>` tracks which keys have been visited, but the
/// full `nid` value (with all its state) is still passed to your folder.
///
/// **Time Complexity:** O(V + E) for both BFS and DFS, where V and E are
/// measured in terms of unique *keys* (not unique nodes).
///
/// ## Example
///
///     // Search a maze where nodes carry both position and step count
///     // but we only want to visit each position once (first-visit wins)
///     type State = { Pos: int * int; Steps: int }
///
///     implicitFoldBy
///       { Pos = (0, 0); Steps = 0 }
///       BreadthFirst
///       None
///       (fun state ->
///          neighbors state.Pos
///          |> List.map (fun nextPos -> { Pos = nextPos; Steps = state.Steps + 1 }))
///       (fun state -> state.Pos)  // Dedupe by position only
///       (fun acc state _meta ->
///          if state.Pos = target then
///            (Halt, Some state.Steps)
///          else
///            (Continue, acc))
///
/// ## Use Cases
///
/// - **Puzzle solving**: `(board_state, moves)` → dedupe by `board_state`
/// - **Path finding with budget**: `(pos, fuel_left)` → dedupe by `pos`
/// - **Game state search**: `(position, inventory)` → dedupe by `position`
/// - **Graph search with metadata**: `(node_id, path_history)` → dedupe by `node_id`
///
/// ## Comparison to `implicitFold`
///
/// - `implicitFold`: Deduplicates by the entire node value `nid`
/// - `implicitFoldBy`: Deduplicates by `visitedBy(nid)` but keeps full `nid`
let implicitFoldBy
    (start: 'nid)
    (order: Order)
    (initial: 'a)
    (successors: 'nid -> 'nid list)
    (visitedBy: 'nid -> 'key)
    (folder: 'a -> 'nid -> WalkMetadata<'nid> -> WalkControl * 'a)
    : 'a =
    match order with
    | BreadthFirst -> doImplicitBfsBy start successors visitedBy initial folder
    | DepthFirst -> doImplicitDfsBy start successors visitedBy initial folder

/// Traverses an *implicit* graph using BFS or DFS,
/// folding over visited nodes with metadata.
///
/// Unlike `foldWalk`, this does not require a materialised `Graph` value.
/// Instead, you supply a `successors` function that computes neighbours
/// on the fly — ideal for infinite grids, state-space search, or any
/// graph that is too large or expensive to build upfront.
///
/// ## Example
///
///     // BFS shortest path in an implicit maze
///     implicitFold
///       (1, 1)
///       BreadthFirst
///       -1
///       (fun pos -> openNeighbors pos)
///       (fun acc pos meta ->
///          if pos = target then
///            (Halt, meta.Depth)
///          else
///            (Continue, acc))
///
/// ## Use Cases
///
/// - Infinite grids (e.g., cellular automata, game maps)
/// - State-space search (puzzle solving, pathfinding)
/// - On-demand graph generation (procedural content)
let implicitFold
    (start: 'nid)
    (order: Order)
    (initial: 'a)
    (successors: 'nid -> 'nid list)
    (folder: 'a -> 'nid -> WalkMetadata<'nid> -> WalkControl * 'a)
    : 'a =
    implicitFoldBy start order initial successors id folder

/// Traverses an *implicit* weighted graph using Dijkstra's algorithm,
/// folding over visited nodes in order of increasing cost.
///
/// Like `implicitFold` but uses a priority queue so nodes are visited
/// cheapest-first. Ideal for shortest-path problems on implicit state spaces
/// where edge costs vary — e.g., state-space search with Manhattan moves, or
/// multi-robot coordination where multiple robots share a key-bitmask state.
///
/// - `successors`: Given a node, return `List<(neighbor * edge_cost)>`.
///   Include only valid transitions (filtering here avoids dead states).
/// - `folder`: Called once per node, with `(acc, node, cost_so_far)`.
///   Return `(Halt, result)` to stop immediately, `(Stop, acc)` to skip
///   expanding this node's successors, or `(Continue, acc)` to continue.
///
/// Internally maintains a `Dictionary<state, cost>` of best-known costs;
/// stale priority-queue entries are automatically skipped.
///
/// ## Example
///
///     // Shortest path in an implicit maze with uniform cost
///     implicitDijkstra
///       start
///       -1
///       (fun pos ->
///          neighbors pos
///          |> List.map (fun nb -> (nb, 1)))  // uniform cost
///       (fun acc pos cost ->
///          if pos = target then
///            (Halt, cost)
///          else
///            (Continue, acc))
///
/// ## Use Cases
///
/// - Shortest path on infinite grids
/// - Weighted state-space search
/// - Resource-constrained pathfinding
let implicitDijkstra
    (start: 'nid)
    (initial: 'a)
    (successors: 'nid -> ('nid * int) list)
    (folder: 'a -> 'nid -> int -> WalkControl * 'a)
    : 'a =
    let frontier = PriorityQueue<'nid * int, int>()
    let best = Dictionary<'nid, int>()

    frontier.Enqueue((start, 0), 0)
    best.[start] <- 0

    let mutable acc = initial
    let mutable halted = false

    while frontier.Count > 0 && not halted do
        let (node, cost) = frontier.Dequeue()

        // Skip stale entries
        let mutable currentBest = 0

        if best.TryGetValue(node, &currentBest) && cost <= currentBest then

            let (control, newAcc) = folder acc node cost
            acc <- newAcc

            match control with
            | Halt -> halted <- true
            | Stop -> ()
            | Continue ->
                for (neighbor, edgeCost) in successors node do
                    let newCost = cost + edgeCost
                    let mutable neighborBest = 0

                    if not (best.TryGetValue(neighbor, &neighborBest)) || newCost < neighborBest then
                        best.[neighbor] <- newCost
                        frontier.Enqueue((neighbor, newCost), newCost)

    acc

/// Performs a topological sort on a directed graph using Kahn's algorithm.
///
/// Returns a linear ordering of nodes such that for every directed edge (u, v),
/// node u comes before node v in the ordering.
///
/// Returns `Error()` if the graph contains a cycle.
///
/// **Time Complexity:** O(V + E) where V is vertices and E is edges
///
/// ## Example
///
///     topologicalSort graph
///     // => Ok [1; 2; 3; 4]  // Valid ordering
///     // or Error()          // Cycle detected
///
/// ## Use Cases
///
/// - Task scheduling with dependencies
/// - Compilation ordering
/// - Resolving symbol dependencies
let topologicalSort (graph: Graph<'n, 'e>) : Result<NodeId list, unit> = doTopologicalSort graph

/// Performs a topological sort that returns the lexicographically smallest sequence.
///
/// Uses a heap-based version of Kahn's algorithm to ensure that when multiple
/// nodes have in-degree 0, the smallest one (according to `compareNodes`) is chosen first.
///
/// The comparison function operates on **node data**, not node IDs, allowing intuitive
/// comparisons like `String.compare` for alphabetical ordering.
///
/// Returns `Error()` if the graph contains a cycle.
///
/// **Time Complexity:** O(V log V + E) due to heap operations
///
/// ## Example
///
///     // Get alphabetical ordering by node data
///     lexicographicalTopologicalSort (fun a b -> String.compare(a, b)) graph
///     // => Ok [0; 1; 2]  // Node IDs ordered by their string data
///
///     // Custom comparison by priority
///     lexicographicalTopologicalSort (fun a b ->
///       compare a.Priority b.Priority) graph
///
/// ## Use Cases
///
/// - Deterministic task ordering (when multiple valid orderings exist)
/// - Alphabetical/lexicographical ordering of tasks
/// - Priority-based scheduling
let lexicographicalTopologicalSort (compareNodes: 'n -> 'n -> int) (graph: Graph<'n, 'e>) : Result<NodeId list, unit> =
    let allNodesList = allNodes graph
    let totalNodes = allNodesList.Length

    let mutable inDegrees =
        allNodesList
        |> Seq.map (fun id ->
            let degree =
                graph.InEdges
                |> Map.tryFind id
                |> Option.map (fun m -> m.Count)
                |> Option.defaultValue 0

            id, degree)
        |> Map.ofSeq

    // PriorityQueue in .NET creates a min-heap by default based on the priority.
    // We create a custom comparer that looks up the node data and uses `compareNodes`.
    let nodeComparer =
        { new IComparer<NodeId> with
            member _.Compare(idA, idB) =
                match Map.tryFind idA graph.Nodes, Map.tryFind idB graph.Nodes with
                | Some dataA, Some dataB -> compareNodes dataA dataB
                | _ -> 0 } // Fallback if nodes don't exist

    let frontier = PriorityQueue<NodeId, NodeId>(nodeComparer)

    inDegrees
    |> Map.iter (fun id degree ->
        if degree = 0 then
            frontier.Enqueue(id, id))

    let mutable sorted = []

    while frontier.Count > 0 do
        let current = frontier.Dequeue()
        sorted <- current :: sorted

        let successors = successorIds current graph

        for next in successors do
            let currentDegree = inDegrees |> Map.tryFind next |> Option.defaultValue 0

            let newDegree = currentDegree - 1
            inDegrees <- inDegrees |> Map.add next newDegree

            if newDegree = 0 then
                frontier.Enqueue(next, next)

    if sorted.Length = totalNodes then
        Ok(List.rev sorted)
    else
        Error()
