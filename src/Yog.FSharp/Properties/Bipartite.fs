/// Bipartite graph analysis and matching algorithms.
///
/// A graph is bipartite (2-colorable) if its vertices can be divided into two disjoint sets
/// such that every edge connects a vertex in one set to a vertex in the other set.
/// Equivalently, a bipartite graph contains no odd-length cycles.
///
/// ## Algorithms
///
/// | Problem | Algorithm | Function | Complexity |
/// |---------|-----------|----------|------------|
/// | Bipartite check | BFS 2-coloring | isBipartite, partition | O(V + E) |
/// | Maximum matching | Augmenting paths | maximumMatching | O(VE) |
/// | Stable matching | Gale-Shapley | stableMarriage | O(V²) |
///
/// ## Use Cases
///
/// - Job assignment: Workers to tasks they can perform
/// - Scheduling: Time slots to events without conflicts
/// - Recommendation systems: Users to items they might like
/// - Stable marriage: Matching medical residents to hospitals
module Yog.Properties.Bipartite

open System.Collections.Generic
open Yog.Model

/// Represents a partition of a bipartite graph into two independent sets.
type Partition =
    { Left: Set<NodeId>
      Right: Set<NodeId> }

/// Checks if a graph is bipartite (2-colorable).
let isBipartite (graph: Graph<'n, 'e>) : bool =
    let nodes = allNodes graph
    let colors = Dictionary<NodeId, bool>()

    let rec bfsColor start startColor =
        let q = Queue<NodeId * bool>()
        q.Enqueue(start, startColor)
        colors.[start] <- startColor

        let mutable possible = true

        while q.Count > 0 && possible do
            let (node, color) = q.Dequeue()

            for neighbor in successorIds node graph do
                match colors.TryGetValue(neighbor) with
                | true, existingColor ->
                    if existingColor <> not color then
                        possible <- false
                | _ ->
                    colors.[neighbor] <- not color
                    q.Enqueue(neighbor, not color)

        possible

    nodes
    |> List.forall (fun n -> if colors.ContainsKey(n) then true else bfsColor n true)

/// Returns the two partitions of a bipartite graph, or None if the graph is not bipartite.
let partition (graph: Graph<'n, 'e>) : Partition option =
    let nodes = allNodes graph
    let colors = Dictionary<NodeId, bool>()

    // Internal BFS coloring logic
    let bfsColor start startColor =
        let q = Queue<NodeId * bool>()
        q.Enqueue(start, startColor)
        colors.[start] <- startColor
        let mutable possible = true

        while q.Count > 0 && possible do
            let (node, color) = q.Dequeue()
            // Using a custom neighbors helper to handle directed/undirected consistently
            let neighbors =
                if graph.Kind = Undirected then
                    successorIds node graph
                else
                    (successorIds node graph @ predecessorIds node graph) |> List.distinct

            for neighbor in neighbors do
                match colors.TryGetValue(neighbor) with
                | true, existingColor ->
                    if existingColor = color then
                        possible <- false
                | _ ->
                    colors.[neighbor] <- not color
                    q.Enqueue(neighbor, not color)

        possible

    let isPossible =
        nodes
        |> List.forall (fun n -> if colors.ContainsKey(n) then true else bfsColor n true)

    if not isPossible then
        None
    else
        let left = HashSet<NodeId>()
        let right = HashSet<NodeId>()

        for kvp in colors do
            if kvp.Value then
                left.Add(kvp.Key) |> ignore
            else
                right.Add(kvp.Key) |> ignore

        Some
            { Left = Set.ofSeq left
              Right = Set.ofSeq right }

/// Finds a maximum matching in a bipartite graph.
let maximumMatching (partition: Partition) (graph: Graph<'n, 'e>) : (NodeId * NodeId) list =
    let matchR = Dictionary<NodeId, NodeId>()

    let rec canMatch u (visited: HashSet<NodeId>) =
        let mutable foundMatch = false
        // Use a for...in loop and successorId (singular)
        let neighbors = successorIds u graph
        let mutable i = 0

        while i < neighbors.Length && not foundMatch do
            let v = neighbors.[i]

            if partition.Right.Contains(v) && visited.Add(v) then
                match matchR.TryGetValue(v) with
                | false, _ ->
                    matchR.[v] <- u
                    foundMatch <- true
                | true, prevL ->
                    // Here is the recursive call
                    if canMatch prevL visited then
                        matchR.[v] <- u
                        foundMatch <- true

            i <- i + 1

        foundMatch

    for u in partition.Left do
        // This is the call that was throwing FS0020
        canMatch u (HashSet<NodeId>()) |> ignore

    matchR |> Seq.map (fun kvp -> kvp.Value, kvp.Key) |> Seq.toList

// ============= Stable Marriage Problem =============



type StableMarriage = { Matches: Map<NodeId, NodeId> }

/// Finds a stable matching using the Gale-Shapley algorithm.
let stableMarriage (leftPrefs: Map<NodeId, NodeId list>) (rightPrefs: Map<NodeId, NodeId list>) : StableMarriage =
    // Pre-calculate rankings for O(1) preference comparison
    let rightRankings =
        rightPrefs
        |> Map.map (fun _ prefList -> prefList |> List.mapi (fun i person -> person, i) |> Map.ofList)

    let freeLeft = Queue<NodeId>(leftPrefs.Keys)
    let leftMatches = Dictionary<NodeId, NodeId>()
    let rightMatches = Dictionary<NodeId, NodeId>()
    let nextProposalIndex = Dictionary<NodeId, int>()

    for k in leftPrefs.Keys do
        nextProposalIndex.[k] <- 0

    while freeLeft.Count > 0 do
        let proposer = freeLeft.Dequeue()
        let prefs = leftPrefs.[proposer]
        let idx = nextProposalIndex.[proposer]

        if idx < prefs.Length then
            let receiver = prefs.[idx]
            nextProposalIndex.[proposer] <- idx + 1

            match rightMatches.TryGetValue(receiver) with
            | false, _ ->
                rightMatches.[receiver] <- proposer
                leftMatches.[proposer] <- receiver
            | true, currentPartner ->
                let rankings = rightRankings.[receiver]

                if rankings.[proposer] < rankings.[currentPartner] then
                    // Receiver prefers new proposer
                    rightMatches.[receiver] <- proposer
                    leftMatches.[proposer] <- receiver
                    leftMatches.Remove(currentPartner) |> ignore
                    freeLeft.Enqueue(currentPartner)
                else
                    // Reject, stay free
                    freeLeft.Enqueue(proposer)

    { Matches = leftMatches |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq }

let getPartner (person: NodeId) (marriage: StableMarriage) : NodeId option = marriage.Matches |> Map.tryFind person
