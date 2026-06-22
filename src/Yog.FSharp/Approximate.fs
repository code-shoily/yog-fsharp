/// Fast approximation algorithms for expensive graph properties.
module Yog.Approximate

open System
open System.Collections.Generic
open Yog.Model

/// Shuffle list using optional seed.
let private shuffle (seed: int option) (list: 't list) : 't list =
    let rng =
        match seed with
        | Some s -> Random(s)
        | None -> Random()

    let arr = list |> List.toArray

    for i = arr.Length - 1 downto 1 do
        let j = rng.Next(i + 1)
        let temp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- temp

    arr |> Array.toList

// =============================================================================
// Diameter Approximation
// =============================================================================

/// Approximates the diameter using multi-sweep BFS/Dijkstra.
let diameter
    (samples: int)
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (graph: Graph<'n, 'e>)
    : 'e option =
    let nodes = allNodes graph

    if nodes.IsEmpty then
        None
    else
        let rng = Random()
        let mutable best = zero
        let mutable candidate = nodes.[rng.Next(nodes.Length)]

        for _ in 1..samples do
            let distances =
                Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare candidate graph

            if not distances.IsEmpty then
                let farthest, maxDist =
                    distances
                    |> Map.fold
                        (fun (f, maxD) n dist -> if compare dist maxD > 0 then (n, dist) else (f, maxD))
                        (candidate, zero)

                if compare maxDist best > 0 then
                    best <- maxDist

                candidate <- farthest

        Some best

/// Approximates the diameter of an unweighted graph.
let diameterUnweighted (samples: int) (graph: Graph<'n, float>) : float option = diameter samples 0.0 (+) compare graph

// =============================================================================
// Betweenness Centrality Approximation
// =============================================================================

/// Approximates betweenness centrality using sampled Brandes sources.
let betweenness
    (samples: int option)
    (seed: int option)
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (graph: Graph<'n, 'e>)
    : Map<NodeId, float> =
    let nodes = allNodes graph
    let n = nodes.Length

    if n = 0 then
        Map.empty
    else
        let sampleCount =
            match samples with
            | Some s -> min s n
            | None -> min (int (Math.Sqrt(float n))) n

        let shuffled = shuffle seed nodes
        let sampled = shuffled |> List.take sampleCount

        let mutable accum = nodes |> List.map (fun id -> id, 0.0) |> Map.ofList

        for s in sampled do
            let stack = Stack<NodeId>()
            let preds = Dictionary<NodeId, NodeId list>()
            let sigma = Dictionary<NodeId, float>()
            let dists = Dictionary<NodeId, 'e>()

            for node in nodes do
                sigma.[node] <- 0.0
                preds.[node] <- []

            sigma.[s] <- 1.0
            dists.[s] <- zero

            let comparer =
                { new IComparer<'e> with
                    member _.Compare(a, b) = compare a b }

            let pq = PriorityQueue<NodeId, 'e>(comparer)
            pq.Enqueue(s, zero)

            while pq.Count > 0 do
                let v = pq.Dequeue()
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
                            dists.[w] <- newDist
                            pq.Enqueue(w, newDist)
                            sigma.[w] <- sigma.[v]
                            preds.[w] <- [ v ]
                        elif cmp = 0 then
                            sigma.[w] <- sigma.[w] + sigma.[v]
                            preds.[w] <- v :: preds.[w]

            let delta = Dictionary<NodeId, float>()

            for node in nodes do
                delta.[node] <- 0.0

            while stack.Count > 0 do
                let w = stack.Pop()

                for v in preds.[w] do
                    delta.[v] <- delta.[v] + (sigma.[v] / sigma.[w]) * (1.0 + delta.[w])

                if w <> s then
                    accum <- accum |> Map.add w (accum.[w] + delta.[w])

        let scale = float n / float (max sampleCount 1)
        let scaled = accum |> Map.map (fun _ score -> score * scale)

        if graph.Kind = Undirected then
            scaled |> Map.map (fun _ score -> score * 0.5)
        else
            scaled

// =============================================================================
// Closeness Centrality Approximation
// =============================================================================

/// Approximates closeness centrality for all nodes using Eppstein-Wang pivot sampling.
let closeness
    (samples: int option)
    (seed: int option)
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (toFloat: 'e -> float)
    (graph: Graph<'n, 'e>)
    : Map<NodeId, float> =
    let nodes = allNodes graph
    let n = nodes.Length

    if n <= 1 then
        nodes |> List.map (fun id -> id, 0.0) |> Map.ofList
    else
        let sampleCount =
            match samples with
            | Some s -> min s n
            | None -> min (int (Math.Sqrt(float n))) n

        let shuffled = shuffle seed nodes
        let sampled = shuffled |> List.take sampleCount

        let transposed = Yog.Transform.transpose graph

        let pivotDistances =
            sampled
            |> List.map (fun pivot -> Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare pivot transposed)

        nodes
        |> List.map (fun v ->
            let mutable sum = 0.0
            let mutable disconnected = false

            for distMap in pivotDistances do
                if not disconnected then
                    match distMap |> Map.tryFind v with
                    | Some d -> sum <- sum + toFloat d
                    | None -> disconnected <- true

            let score =
                if disconnected then
                    0.0
                elif sum > 0.0 then
                    float sampleCount * float (n - 1) / (float n * sum)
                else
                    0.0

            (v, score))
        |> Map.ofList

// =============================================================================
// Harmonic Centrality Approximation
// =============================================================================

/// Approximates harmonic centrality for all nodes using pivot sampling.
let harmonic
    (samples: int option)
    (seed: int option)
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (toFloat: 'e -> float)
    (graph: Graph<'n, 'e>)
    : Map<NodeId, float> =
    let nodes = allNodes graph
    let n = nodes.Length

    if n <= 1 then
        nodes |> List.map (fun id -> id, 0.0) |> Map.ofList
    else
        let sampleCount =
            match samples with
            | Some s -> min s n
            | None -> min (int (Math.Sqrt(float n))) n

        let shuffled = shuffle seed nodes
        let sampled = shuffled |> List.take sampleCount
        let transposed = Yog.Transform.transpose graph

        let pivotDistances =
            sampled
            |> List.map (fun pivot -> Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare pivot transposed)

        let denominator = float (n - 1)

        nodes
        |> List.map (fun v ->
            let mutable sum = 0.0

            for distMap in pivotDistances do
                match distMap |> Map.tryFind v with
                | Some d ->
                    let f = toFloat d

                    if f > 0.0 then
                        sum <- sum + 1.0 / f
                | None -> ()

            let score = sum * float n / (float sampleCount * denominator)
            (v, score))
        |> Map.ofList

// =============================================================================
// Average Shortest Path Length Approximation
// =============================================================================

/// Approximates the average shortest path length using pivot sampling.
let averagePathLength
    (samples: int option)
    (seed: int option)
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (toFloat: 'e -> float)
    (graph: Graph<'n, 'e>)
    : float option =
    let nodes = allNodes graph
    let n = nodes.Length

    if n <= 1 then
        Some 0.0
    else
        let sampleCount =
            match samples with
            | Some s -> min s n
            | None -> min (int (Math.Sqrt(float n))) n

        let shuffled = shuffle seed nodes
        let sampled = shuffled |> List.take sampleCount

        let mutable totalSum = 0.0
        let mutable reachablePairs = 0

        for source in sampled do
            let dists =
                Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare source graph

            for KeyValue(node, dist) in dists do
                if node <> source then
                    totalSum <- totalSum + toFloat dist
                    reachablePairs <- reachablePairs + 1

        if reachablePairs = 0 then
            None
        else
            Some(totalSum / float reachablePairs)

// =============================================================================
// Global Efficiency Approximation
// =============================================================================

/// Approximates global efficiency using pivot sampling.
let globalEfficiency
    (samples: int option)
    (seed: int option)
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
        let sampleCount =
            match samples with
            | Some s -> min s n
            | None -> min (int (Math.Sqrt(float n))) n

        let shuffled = shuffle seed nodes
        let sampled = shuffled |> List.take sampleCount

        let mutable total = 0.0

        for source in sampled do
            let dists =
                Yog.Pathfinding.Dijkstra.singleSourceDistances zero add compare source graph

            for KeyValue(node, dist) in dists do
                if node <> source then
                    let f = toFloat dist

                    if f > 0.0 then
                        total <- total + 1.0 / f

        total * (float n / float (max sampled.Length 1)) / (float n * float (n - 1))

// =============================================================================
// Transitivity Approximation
// =============================================================================

/// Approximates transitivity (global clustering coefficient) via wedge sampling.
let transitivity (samples: int option) (seed: int option) (graph: Graph<'n, 'e>) : float =
    let nodes = allNodes graph
    let n = nodes.Length

    if n < 3 then
        0.0
    else
        let adj =
            nodes
            |> List.map (fun u -> (u, neighborIds u graph |> Set.ofList))
            |> Map.ofList

        let totalWedges =
            adj
            |> Map.fold
                (fun acc _ neighbors ->
                    let deg = neighbors.Count
                    acc + deg * (deg - 1) / 2)
                0

        if totalWedges = 0 then
            0.0
        else
            let sampleCount =
                match samples with
                | Some s -> min s (n * n)
                | None -> min 10000 (n * n)

            let rng =
                match seed with
                | Some s -> Random(s)
                | None -> Random()

            let mutable closed = 0

            for _ in 1..sampleCount do
                let u = nodes.[rng.Next(nodes.Length)]
                let uNeighbors = adj.[u] |> Set.toList

                if uNeighbors.Length >= 2 then
                    let v = uNeighbors.[rng.Next(uNeighbors.Length)]
                    let wCandidates = uNeighbors |> List.filter (fun x -> x <> v)

                    if not wCandidates.IsEmpty then
                        let w = wCandidates.[rng.Next(wCandidates.Length)]

                        if adj.[v].Contains(w) then
                            closed <- closed + 1

            float closed / float sampleCount

// =============================================================================
// Vertex Cover Approximation
// =============================================================================

/// Returns a 2-approximation of the minimum vertex cover.
let vertexCover (graph: Graph<'n, 'e>) : Set<NodeId> =
    let edges = allEdges graph
    let mutable cover = Set.empty

    for (u, v, _) in edges do
        if not (cover.Contains(u) || cover.Contains(v)) then
            cover <- cover |> Set.add u |> Set.add v

    cover

// =============================================================================
// Max Clique Approximation
// =============================================================================

/// Returns a large clique using a greedy heuristic.
let maxClique (graph: Graph<'n, 'e>) : Set<NodeId> =
    let nodes = allNodes graph

    if nodes.IsEmpty then
        Set.empty
    else
        let adj =
            nodes
            |> List.map (fun u -> (u, neighborIds u graph |> Set.ofList))
            |> Map.ofList

        let sorted = nodes |> List.sortByDescending (fun u -> adj.[u].Count)

        let rec greedyClique (candidates: NodeId list) (clique: Set<NodeId>) =
            match candidates with
            | [] -> clique
            | candidate :: rest ->
                if clique.Contains(candidate) then
                    greedyClique rest clique
                else
                    let neighbors = adj.[candidate]

                    if clique |> Set.forall (fun v -> neighbors.Contains(v)) then
                        greedyClique rest (clique |> Set.add candidate)
                    else
                        greedyClique rest clique

        sorted
        |> List.fold
            (fun best startNode ->
                let clique = greedyClique (startNode :: sorted) (Set.singleton startNode)
                if clique.Count > best.Count then clique else best)
            Set.empty

// =============================================================================
// Treewidth Approximation
// =============================================================================

type TreewidthHeuristic =
    | MinDegree
    | MinFill

let private dfs (start: NodeId) (adj: Map<NodeId, Set<NodeId>>) (visited: Set<NodeId>) =
    let visitedSet = HashSet<NodeId>(visited)
    let queue = Queue<NodeId>()
    let comp = List<NodeId>()

    queue.Enqueue(start)
    visitedSet.Add(start) |> ignore
    comp.Add(start)

    while queue.Count > 0 do
        let node = queue.Dequeue()

        for nbr in adj.[node] do
            if visitedSet.Add(nbr) then
                queue.Enqueue(nbr)
                comp.Add(nbr)

    (comp |> Seq.toList, visitedSet |> Set.ofSeq)

let private weakComponents (graph: Graph<'n, 'e>) (nodes: NodeId list) =
    let adj =
        nodes
        |> List.map (fun u -> (u, neighborIds u graph |> Set.ofList))
        |> Map.ofList

    let mutable visited = Set.empty
    let mutable components = []

    for node in nodes do
        if not (visited.Contains(node)) then
            let compNodes, newVisited = dfs node adj visited
            components <- compNodes :: components
            visited <- newVisited

    (adj, List.rev components)

let private countMissingEdges (adj: Map<NodeId, Set<NodeId>>) (nodes: Set<NodeId>) =
    let list = nodes |> Set.toList
    let mutable count = 0

    for i in 0 .. list.Length - 1 do
        for j in i + 1 .. list.Length - 1 do
            let u = list.[i]
            let v = list.[j]

            if not (adj.[u].Contains(v)) then
                count <- count + 1

    count

let private selectVertex (remaining: Set<NodeId>) (adj: Map<NodeId, Set<NodeId>>) (heuristic: TreewidthHeuristic) =
    match heuristic with
    | MinDegree -> remaining |> Set.toList |> List.minBy (fun v -> adj.[v].Count)
    | MinFill -> remaining |> Set.toList |> List.minBy (fun v -> countMissingEdges adj adj.[v])

let private makeClique (adj: Map<NodeId, Set<NodeId>>) (nodes: Set<NodeId>) =
    let list = nodes |> Set.toList
    let mutable acc = adj

    for u in list do
        let current = acc.[u]
        let toAdd = nodes |> Set.remove u
        acc <- acc |> Map.add u (Set.union current toAdd)

    acc

let private removeFromAll (adj: Map<NodeId, Set<NodeId>>) (v: NodeId) =
    adj |> Map.map (fun _ ns -> ns |> Set.remove v)

let rec private doElimination
    (remaining: Set<NodeId>)
    (adj: Map<NodeId, Set<NodeId>>)
    (heuristic: TreewidthHeuristic)
    (bags: (NodeId * Set<NodeId>) list)
    (width: int)
    =
    if remaining.IsEmpty then
        (width, List.rev bags)
    else
        let v = selectVertex remaining adj heuristic
        let neighbors = adj.[v]
        let bag = neighbors |> Set.add v
        let newWidth = max width (bag.Count - 1)

        let newAdj =
            adj
            |> Map.remove v
            |> (fun a -> makeClique a neighbors)
            |> (fun a -> removeFromAll a v)

        doElimination (remaining |> Set.remove v) newAdj heuristic ((v, bag) :: bags) newWidth

let private eliminateComponents
    (components: NodeId list list)
    (fullAdj: Map<NodeId, Set<NodeId>>)
    (heuristic: TreewidthHeuristic)
    =
    let results =
        components
        |> List.map (fun compNodes ->
            let compSet = Set.ofList compNodes

            let adj =
                compNodes
                |> List.map (fun u -> (u, Set.intersect fullAdj.[u] compSet))
                |> Map.ofList

            doElimination compSet adj heuristic [] 0)

    let totalWidth =
        match results |> List.map fst with
        | [] -> 0
        | ws -> List.max ws

    let allBagInfo = results |> List.collect snd
    (totalWidth, allBagInfo)

let private sharesVertex (a: Set<NodeId>) (b: Set<NodeId>) = not (Set.intersect a b).IsEmpty

let private buildTreeEdges (bags: (NodeId * Set<NodeId>) array) (v2i: Map<NodeId, int>) =
    let mutable acc = []
    let mutable prevRoot = None

    for i in 0 .. bags.Length - 1 do
        let v_i, bag = bags.[i]
        let otherVertices = bag |> Set.remove v_i

        let standardParent =
            if otherVertices.IsEmpty then
                None
            else
                let parents =
                    otherVertices
                    |> Set.toList
                    |> List.map (fun v -> v2i.[v])
                    |> List.filter (fun p -> p > i)

                if parents.IsEmpty then None else Some(List.min parents)

        match standardParent with
        | Some p -> acc <- (i, p) :: acc
        | None ->
            let fallbackParent =
                let mutable found = None
                let mutable j = i - 1

                while j >= 0 && found.IsNone do
                    let _, bj = bags.[j]

                    if sharesVertex bag bj then
                        found <- Some j

                    j <- j - 1

                found

            match fallbackParent with
            | Some p -> acc <- (i, p) :: acc
            | None ->
                match prevRoot with
                | Some r when r <> i -> acc <- (i, r) :: acc
                | _ -> ()

                prevRoot <- Some i

    List.rev acc

let private buildBagTree (bagInfo: (NodeId * Set<NodeId>) list) =
    let n = bagInfo.Length
    let mutable tree = empty Undirected

    for i in 0 .. n - 1 do
        tree <- addNode i () tree

    let bagArray = bagInfo |> List.toArray
    let v2i = bagInfo |> List.mapi (fun idx (v, _) -> (v, idx)) |> Map.ofList

    let edges = buildTreeEdges bagArray v2i

    let uniqueEdges =
        edges
        |> List.map (fun (a, b) -> if a <= b then (a, b) else (b, a))
        |> List.distinct

    uniqueEdges |> List.fold (fun g (fromId, toId) -> addEdge fromId toId () g) tree

/// Returns an upper bound on the treewidth using heuristic elimination ordering.
let treewidthUpperBound (heuristic: TreewidthHeuristic) (graph: Graph<'n, 'e>) : int =
    let nodes = allNodes graph

    if nodes.IsEmpty then
        0
    else
        let fullAdj, components = weakComponents graph nodes
        let tw, _ = eliminateComponents components fullAdj heuristic
        tw

/// Returns a tree decomposition of the graph using heuristic elimination ordering.
let treeDecomposition
    (heuristic: TreewidthHeuristic)
    (graph: Graph<'n, 'e>)
    : Yog.Properties.TreeDecomposition.TreeDecomposition<unit, unit> option =
    let nodes = allNodes graph

    if nodes.IsEmpty then
        let tree = empty Undirected

        Some
            { Bags = Map.empty
              Tree = tree
              Width = 0 }
    else
        let fullAdj, components = weakComponents graph nodes
        let tw, bagInfo = eliminateComponents components fullAdj heuristic
        let tree = buildBagTree bagInfo
        let bagMap = bagInfo |> List.mapi (fun idx (_, bag) -> (idx, bag)) |> Map.ofList

        Some
            { Bags = bagMap
              Tree = tree
              Width = tw }
