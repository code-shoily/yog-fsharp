/// Eulerian path and circuit algorithms using Hierholzer's algorithm.
///
/// An Eulerian path visits every edge exactly once.
/// An Eulerian circuit visits every edge exactly once and returns to the start.
/// These problems originated from the famous Seven Bridges of Königsberg
/// solved by Leonhard Euler in 1736, founding graph theory.
///
/// ## Algorithms
///
/// | Problem | Algorithm | Function | Complexity |
/// |---------|-----------|----------|------------|
/// | Eulerian circuit check | Degree counting | hasEulerianCircuit | O(V + E) |
/// | Eulerian path check | Degree counting | hasEulerianPath | O(V + E) |
/// | Find circuit | Hierholzer's | findEulerianCircuit | O(E) |
/// | Find path | Hierholzer's | findEulerianPath | O(E) |
///
/// ## Necessary and Sufficient Conditions
///
/// **Undirected Graphs:**
/// - Circuit: All vertices have even degree, connected (ignoring isolates)
/// - Path: Exactly 0 or 2 vertices have odd degree, connected
///
/// **Directed Graphs:**
/// - Circuit: In-degree = Out-degree for all vertices, weakly connected
/// - Path: At most one vertex has (out - in) = 1 (start),
///   at most one has (in - out) = 1 (end), all others balanced
///
/// ## Use Cases
///
/// - Route planning: Garbage collection, snow plowing, mail delivery
/// - DNA sequencing: Constructing genomes from overlapping fragments
/// - Circuit board drilling: Optimizing drill paths for PCB manufacturing
module Yog.Properties.Eulerian

open System.Collections.Generic
open Yog.Model

/// Checks if all nodes in the graph are reachable from a single starting point.
/// For Eulerian purposes, we only care about nodes that actually exist in the graph.
let private isConnected (graph: Graph<'n, 'e>) : bool =
    match allNodes graph with
    | [] -> true
    | start :: _ ->
        // Start node comes FIRST, then the Order, then the graph
        let visited = Yog.Traversal.walk start Yog.Traversal.BreadthFirst graph
        visited.Length = nodeCount graph

/// Checks if the graph has an Eulerian circuit.
let hasEulerianCircuit (graph: Graph<'n, 'e>) : bool =
    if nodeCount graph = 0 then
        false
    else
        match graph.Kind with
        | Undirected ->
            let allEven =
                allNodes graph |> List.forall (fun n -> (neighbors n graph).Length % 2 = 0)

            allEven && isConnected graph
        | Directed ->
            let allBalanced =
                allNodes graph
                |> List.forall (fun n -> (predecessors n graph).Length = (successors n graph).Length)

            allBalanced && isConnected graph

/// Checks if the graph has an Eulerian path.
let hasEulerianPath (graph: Graph<'n, 'e>) : bool =
    if nodeCount graph = 0 then
        false
    else
        match graph.Kind with
        | Undirected ->
            let oddCount =
                allNodes graph
                |> List.filter (fun n -> (neighbors n graph).Length % 2 = 1)
                |> List.length

            (oddCount = 0 || oddCount = 2) && isConnected graph
        | Directed ->
            let mutable starts = 0
            let mutable ends = 0
            let mutable balanced = true

            for n in allNodes graph do
                let inDeg = (predecessors n graph).Length
                let outDeg = (successors n graph).Length

                match outDeg - inDeg with
                | 1 -> starts <- starts + 1
                | -1 -> ends <- ends + 1
                | 0 -> ()
                | _ -> balanced <- false

            balanced
            && ((starts = 0 && ends = 0) || (starts = 1 && ends = 1))
            && isConnected graph

/// Hierholzer's algorithm to find the circuit or path.
let private hierholzer (startNode: NodeId) (graph: Graph<'n, 'e>) : NodeId list option =
    // Using mutable Queues inside a Dictionary for O(1) edge removal
    let adj = Dictionary<NodeId, Queue<NodeId>>()

    for u in allNodes graph do
        adj.[u] <- Queue<NodeId>(successorIds u graph)

    let res = Stack<NodeId>()
    let currPath = Stack<NodeId>()
    currPath.Push(startNode)

    while currPath.Count > 0 do
        let u = currPath.Peek()
        let mutable neighbors = null

        if adj.TryGetValue(u, &neighbors) && neighbors.Count > 0 then
            let v = neighbors.Dequeue()

            // For undirected, we must remove the reverse edge too
            if graph.Kind = Undirected then
                let mutable revNeighbors = null

                if adj.TryGetValue(v, &revNeighbors) then
                    // Removing from a Queue by value is O(N), but necessary for Undirected
                    // In a production-grade version, one might use an adjacency of LinkedList nodes
                    let temp = ResizeArray<NodeId>(revNeighbors)
                    temp.Remove(u) |> ignore
                    adj.[v] <- Queue<NodeId>(temp)

            currPath.Push(v)
        else
            res.Push(currPath.Pop())

    let path = res |> Seq.toList

    if path.Length > 0 then Some path else None

/// Finds an Eulerian circuit in the graph.
let findEulerianCircuit (graph: Graph<'n, 'e>) : NodeId list option =
    if not (hasEulerianCircuit graph) then
        None
    else
        match allNodes graph with
        | [] -> None
        | start :: _ -> hierholzer start graph

/// Finds an Eulerian path in the graph.
let findEulerianPath (graph: Graph<'n, 'e>) : NodeId list option =
    if not (hasEulerianPath graph) then
        None
    else
        let startNode =
            match graph.Kind with
            | Undirected ->
                allNodes graph
                |> List.tryFind (fun n -> (neighbors n graph).Length % 2 = 1)
                |> Option.defaultValue (List.head (allNodes graph))
            | Directed ->
                allNodes graph
                |> List.tryFind (fun n -> (successors n graph).Length > (predecessors n graph).Length)
                |> Option.defaultValue (List.head (allNodes graph))

        hierholzer startNode graph
