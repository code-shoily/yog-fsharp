/// Graph matching algorithms.
///
/// A matching is a set of edges without common vertices.
///
/// ## Algorithms
///
/// | Problem | Algorithm | Function | Complexity |
/// |---------|-----------|----------|------------|
/// | Maximum bipartite matching | Hopcroft-Karp | hopcroftKarp | O(E√V) |
/// | Weighted bipartite matching | Hungarian (Kuhn-Munkres) | hungarian | O(V³) |
/// | Maximum general matching | Edmonds' Blossom | blossomMaximumMatching | O(V²E) |
module Yog.Properties.Matching

open System
open System.Collections.Generic
open Yog.Model
open Yog.Properties.Bipartite

// =============================================================================
// Hopcroft-Karp Algorithm
// =============================================================================

/// Finds a maximum cardinality matching in a bipartite graph using the Hopcroft-Karp algorithm.
///
/// Returns a bidirectional map where each matched pair appears twice (u => v and v => u).
/// Throws ArgumentException if the graph is not bipartite.
let hopcroftKarp (graph: Graph<'n, 'e>) : Map<NodeId, NodeId> =
    match partition graph with
    | None -> invalidArg "graph" "hopcroftKarp requires a bipartite graph"
    | Some part ->
        let uNodes = part.Left |> Set.toList
        let vNodes = part.Right |> Set.toList
        
        // Build adjacency map from Left to Right
        let adj = Dictionary<NodeId, NodeId list>()
        for u in uNodes do
            let neighbors = neighborIds u graph
            let valid = neighbors |> List.filter (fun v -> part.Right.Contains(v))
            adj.[u] <- valid
            
        let pairU = Dictionary<NodeId, NodeId option>()
        for u in uNodes do
            pairU.[u] <- None
            
        let pairV = Dictionary<NodeId, NodeId option>()
        for v in vNodes do
            pairV.[v] <- None
            
        let dist = Dictionary<NodeId, int>()
        let mutable distNone = Int32.MaxValue
        
        let bfs () =
            let q = Queue<NodeId>()
            for u in uNodes do
                if pairU.[u] = None then
                    dist.[u] <- 0
                    q.Enqueue(u)
                else
                    dist.[u] <- Int32.MaxValue
            distNone <- Int32.MaxValue
            
            while q.Count > 0 do
                let u = q.Dequeue()
                if dist.[u] < distNone then
                    let uNeighbors = if adj.ContainsKey(u) then adj.[u] else []
                    for v in uNeighbors do
                        match pairV.[v] with
                        | Some u2 ->
                            if dist.[u2] = Int32.MaxValue then
                                dist.[u2] <- dist.[u] + 1
                                q.Enqueue(u2)
                        | None ->
                            if distNone = Int32.MaxValue then
                                distNone <- dist.[u] + 1
                            
            distNone <> Int32.MaxValue
            
        let rec dfs u =
            let uNeighbors = if adj.ContainsKey(u) then adj.[u] else []
            let mutable found = false
            let mutable i = 0
            while i < uNeighbors.Length && not found do
                let v = uNeighbors.[i]
                match pairV.[v] with
                | Some u2 ->
                    if dist.[u2] = dist.[u] + 1 && dfs u2 then
                        pairV.[v] <- Some u
                        pairU.[u] <- Some v
                        found <- true
                | None ->
                    if distNone = dist.[u] + 1 then
                        pairV.[v] <- Some u
                        pairU.[u] <- Some v
                        found <- true
                i <- i + 1
            if not found then
                dist.[u] <- Int32.MaxValue
            found
            
        let rec loop () =
            if bfs() then
                for u in uNodes do
                    if pairU.[u] = None then
                        dfs u |> ignore
                loop()
            else
                ()
                
        loop()
        
        let mutable matching = Map.empty
        for u in uNodes do
            match pairU.[u] with
            | Some v ->
                matching <- matching |> Map.add u v |> Map.add v u
            | None -> ()
        matching

// =============================================================================
// Hungarian Algorithm (Kuhn-Munkres)
// =============================================================================

/// Optimization objective for the Hungarian algorithm.
type HungarianOptimization =
    | Min
    | Max

/// Finds a minimum or maximum weight perfect matching in a bipartite graph using the Hungarian algorithm.
///
/// The graph must be bipartite and complete between the two partitions.
/// Returns (totalCost, matching) where matching is a bidirectional map.
let hungarian (optimization: HungarianOptimization) (graph: Graph<'n, float>) : float * Map<NodeId, NodeId> =
    match partition graph with
    | None -> invalidArg "graph" "hungarian requires a bipartite graph"
    | Some part ->
        let leftNodes = part.Left |> Set.toList |> List.sort
        let rightNodes = part.Right |> Set.toList |> List.sort
        let n = leftNodes.Length
        let m = rightNodes.Length
        let k = Math.Max(n, m)
        
        if k = 0 then
            (0.0, Map.empty)
        else
            let maxId = 
                if graph.Nodes.IsEmpty then 0 
                else graph.Nodes |> Map.keys |> Seq.max
                
            let dummyLeft = [ for i in 1 .. (k - n) -> maxId + i ]
            let dummyRight = [ for i in 1 .. (k - m) -> maxId + (k - n) + i ]
            
            let paddedLeft = leftNodes @ dummyLeft
            let paddedRight = rightNodes @ dummyRight
            
            let dummyLeftSet = Set.ofList dummyLeft
            let dummyRightSet = Set.ofList dummyRight
            
            let matrix = Array2D.zeroCreate<float> (k + 1) (k + 1)
            for i in 1 .. k do
                let u = paddedLeft.[i - 1]
                let isDummyU = dummyLeftSet.Contains(u)
                for j in 1 .. k do
                    let v = paddedRight.[j - 1]
                    let isDummyV = dummyRightSet.Contains(v)
                    
                    let weight =
                        if isDummyU || isDummyV then
                            0.0
                        else
                            match edgeData u v graph with
                            | Some w -> w
                            | None ->
                                match edgeData v u graph with
                                | Some w -> w
                                | None ->
                                    invalidArg "graph" "hungarian requires a complete bipartite graph (edges between all L and R nodes)"
                    let cost = if optimization = Max then -weight else weight
                    matrix.[i, j] <- cost
                    
            // u, v, p, way are arrays of size k + 1
            let uPotentials = Array.zeroCreate<float> (k + 1)
            let vPotentials = Array.zeroCreate<float> (k + 1)
            let p = Array.zeroCreate<int> (k + 1)
            let way = Array.zeroCreate<int> (k + 1)
            
            for i in 1 .. k do
                p.[0] <- i
                let minv = Array.create (k + 1) Double.PositiveInfinity
                let used = Array.create (k + 1) false
                
                let mutable j0 = 0
                let mutable j1 = 0
                let mutable breakLoop = false
                
                while not breakLoop do
                    used.[j0] <- true
                    let i0 = p.[j0]
                    let mutable delta = Double.PositiveInfinity
                    
                    // Find best next column
                    for j in 1 .. k do
                        if not used.[j] then
                            let cur = matrix.[i0, j] - uPotentials.[i0] - vPotentials.[j]
                            if cur < minv.[j] then
                                minv.[j] <- cur
                                way.[j] <- j0
                            if minv.[j] < delta then
                                delta <- minv.[j]
                                j1 <- j
                    
                    // Update potentials
                    for j in 0 .. k do
                        if used.[j] then
                            uPotentials.[p.[j]] <- uPotentials.[p.[j]] + delta
                            vPotentials.[j] <- vPotentials.[j] - delta
                        else
                            minv.[j] <- minv.[j] - delta
                    
                    j0 <- j1
                    if p.[j0] = 0 then
                        breakLoop <- true
                
                // backtrack matching
                let mutable currJ = j0
                let mutable breakBacktrack = false
                while not breakBacktrack do
                    let prevJ = way.[currJ]
                    p.[currJ] <- p.[prevJ]
                    currJ <- prevJ
                    if currJ = 0 then
                        breakBacktrack <- true
                        
            let matchingIndices = Dictionary<int, int>()
            for j in 1 .. k do
                let i = p.[j]
                if i <> 0 then
                    matchingIndices.[j] <- i
                    
            let totalCost = -vPotentials.[0]
            
            let realLeft = leftNodes |> Set.ofList
            let realRight = rightNodes |> Set.ofList
            
            let mutable matching = Map.empty
            for kvp in matchingIndices do
                let j = kvp.Key
                let i = kvp.Value
                let u = paddedLeft.[i - 1]
                let v = paddedRight.[j - 1]
                
                if realLeft.Contains(u) && realRight.Contains(v) then
                    matching <- matching |> Map.add u v |> Map.add v u
                    
            let cost = if optimization = Max then -totalCost else totalCost
            (cost, matching)

// =============================================================================
// Edmonds' Blossom Algorithm
// =============================================================================

/// Finds a maximum cardinality matching in a general (possibly non-bipartite) graph
/// using Edmonds' Blossom algorithm.
///
/// Returns a bidirectional map where each matched pair appears twice (u => v and v => u).
let blossomMaximumMatching (graph: Graph<'n, 'e>) : Map<NodeId, NodeId> =
    let nodes = allNodes graph
    if nodes.IsEmpty then
        Map.empty
    else
        // Build adjacency list
        let adj = Dictionary<NodeId, NodeId list>()
        for u in nodes do
            adj.[u] <- neighborIds u graph
            
        let matchMap = Dictionary<NodeId, NodeId option>()
        for u in nodes do
            matchMap.[u] <- None
            
        let baseMap = Dictionary<NodeId, NodeId>()
        
        let rec findBase (i: NodeId) : NodeId =
            let b = baseMap.[i]
            if b = i then i
            else
                let rootB = findBase b
                baseMap.[i] <- rootB
                rootB
                
        let lca (u: NodeId) (v: NodeId) (parent: Dictionary<NodeId, NodeId option>) =
            let visited = HashSet<NodeId>()
            let mutable currU = u
            let mutable currV = v
            let mutable lcaVal = -1
            let mutable finished = false
            while not finished do
                if currU <> -1 then
                    let uBase = findBase currU
                    if visited.Contains(uBase) then
                        lcaVal <- uBase
                        finished <- true
                    else
                        visited.Add(uBase) |> ignore
                        match matchMap.[uBase] with
                        | None -> currU <- -1
                        | Some m ->
                            match parent.[m] with
                            | None -> currU <- -1
                            | Some p -> currU <- p
                
                if not finished && currV <> -1 then
                    let vBase = findBase currV
                    if visited.Contains(vBase) then
                        lcaVal <- vBase
                        finished <- true
                    else
                        visited.Add(vBase) |> ignore
                        match matchMap.[vBase] with
                        | None -> currV <- -1
                        | Some m ->
                            match parent.[m] with
                            | None -> currV <- -1
                            | Some p -> currV <- p
                            
                if currU = -1 && currV = -1 then
                    finished <- true
            lcaVal

        let contract (u: NodeId) (v: NodeId) (b: NodeId) (parent: Dictionary<NodeId, NodeId option>) (queue: Queue<NodeId>) (inQueue: HashSet<NodeId>) =
            let rec contractSide (curr: NodeId) (child: NodeId) =
                let currBase = findBase curr
                if currBase <> b then
                    baseMap.[currBase] <- b
                    let mate = matchMap.[currBase]
                    match mate with
                    | Some m ->
                        baseMap.[findBase m] <- b
                        parent.[curr] <- Some child
                        if inQueue.Add(m) then
                            queue.Enqueue(m)
                        match parent.[m] with
                        | Some nextVal -> contractSide nextVal m
                        | None -> ()
                    | None ->
                        parent.[curr] <- Some child
            contractSide u v

        let rec augment (v: NodeId) (parent: Dictionary<NodeId, NodeId option>) =
            match parent.[v] with
            | None -> ()
            | Some pv ->
                let ppv = matchMap.[pv]
                matchMap.[v] <- Some pv
                matchMap.[pv] <- Some v
                match ppv with
                | Some nextV -> augment nextV parent
                | None -> ()

        let tryAugment (root: NodeId) : bool =
            for u in nodes do
                baseMap.[u] <- u
                
            let parent = Dictionary<NodeId, NodeId option>()
            let color = Dictionary<NodeId, int>()
            for u in nodes do
                color.[u] <- 0
                
            let queue = Queue<NodeId>()
            let inQueue = HashSet<NodeId>()
            
            color.[root] <- 1
            queue.Enqueue(root)
            inQueue.Add(root) |> ignore
            
            let mutable foundPath = false
            
            while queue.Count > 0 && not foundPath do
                let v = queue.Dequeue()
                let vNeighbors = if adj.ContainsKey(v) then adj.[v] else []
                let mutable i = 0
                while i < vNeighbors.Length && not foundPath do
                    let toNode = vNeighbors.[i]
                    let vBase = findBase v
                    let toBase = findBase toNode
                    
                    if vBase <> toBase && matchMap.[v] <> Some toNode then
                        if toNode = root || (color.[toNode] = 1) then
                            let curBase = lca v toNode parent
                            contract v toNode curBase parent queue inQueue
                            contract toNode v curBase parent queue inQueue
                        elif color.[toNode] = 0 then
                            match matchMap.[toNode] with
                            | None ->
                                parent.[toNode] <- Some v
                                augment toNode parent
                                foundPath <- true
                            | Some mate ->
                                parent.[toNode] <- Some v
                                parent.[mate] <- Some toNode
                                color.[toNode] <- 2
                                color.[mate] <- 1
                                if inQueue.Add(mate) then
                                    queue.Enqueue(mate)
                    i <- i + 1
            foundPath

        for u in nodes do
            if matchMap.[u] = None then
                tryAugment u |> ignore
                
        let mutable matching = Map.empty
        for u in nodes do
            match matchMap.[u] with
            | Some v ->
                matching <- matching |> Map.add u v |> Map.add v u
            | None -> ()
        matching
