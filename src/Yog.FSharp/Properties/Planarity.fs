/// Planarity testing and planar embedding for undirected graphs.
///
/// This module provides exact planarity checks using the Left-Right (LR)
/// Planarity Test (de Fraysseix–Rosenstiehl), combinatorial embedding
/// extraction, and Kuratowski witness identification.
module Yog.Properties.Planarity

open System
open System.Collections.Generic
open Yog.Model
open Yog.Properties.Bipartite
open Yog.Transform

type KuratowskiType =
    | K5
    | K33
    | Unknown

type KuratowskiWitness<'n, 'e> =
    { Type: KuratowskiType
      Nodes: NodeId list
      Edges: (NodeId * NodeId) list
      Subgraph: Graph<'n, 'e> }

type EdgeType =
    | Tree
    | Back

type OrientedEdge = EdgeType * NodeId * NodeId

type ComponentMetadata<'n, 'e> =
    { Graph: Graph<'n, 'e>
      Partitions: Map<NodeId * NodeId, int>
      TreeInfo: OrientedEdge list * Dictionary<NodeId, int> * Dictionary<NodeId, int> * Dictionary<NodeId, int> * Dictionary<NodeId, NodeId> }

type NodeIncident =
    { Parent: NodeId option
      Children: NodeId list
      BackOut: NodeId list
      BackIn: NodeId list }

let private dfsOrient (graph: Graph<'n, 'e>) (root: NodeId) =
    let entryTimes = Dictionary<NodeId, int>()
    let lowpoints = Dictionary<NodeId, int>()
    let finishTimes = Dictionary<NodeId, int>()
    let parents = Dictionary<NodeId, NodeId>()
    let edges = List<OrientedEdge>()
    let visited = HashSet<NodeId>()

    let rec dfs u p time =
        entryTimes.[u] <- time
        lowpoints.[u] <- time
        visited.Add(u) |> ignore
        if p <> u then parents.[u] <- p

        let neighbors = 
            neighborIds u graph 
            |> List.filter (fun v -> v <> p)

        let mutable currTime = time
        for v in neighbors do
            if visited.Contains(v) then
                if entryTimes.[v] < entryTimes.[u] then
                    edges.Add(Back, u, v)
                lowpoints.[u] <- min lowpoints.[u] entryTimes.[v]
            else
                edges.Add(Tree, u, v)
                let nextTime = dfs v u (currTime + 1)
                lowpoints.[u] <- min lowpoints.[u] lowpoints.[v]
                currTime <- nextTime

        let finishTime = currTime + 1
        finishTimes.[u] <- finishTime
        finishTime

    dfs root root 0 |> ignore
    (Seq.toList edges, lowpoints, entryTimes, finishTimes, parents)

let private ancestor (a: NodeId) (d: NodeId) (times: Dictionary<NodeId, int>) (finish: Dictionary<NodeId, int>) =
    times.[a] <= times.[d] && finish.[d] <= finish.[a]

let private interlaced tu tv tx ty u v x y (times: Dictionary<NodeId, int>) (finish: Dictionary<NodeId, int>) =
    if tv < ty && ty < tu && tu < tx then
        ancestor v y times finish && ancestor y u times finish && ancestor u x times finish
    elif ty < tv && tv < tx && tx < tu then
        ancestor y v times finish && ancestor v x times finish && ancestor x u times finish
    else
        false

let private buildConflictGraph backEdges (entryTimes: Dictionary<NodeId, int>) (finishTimes: Dictionary<NodeId, int>) =
    let adj = Dictionary<NodeId * NodeId, List<NodeId * NodeId>>()
    for (_, u, v) in backEdges do
        adj.[(u, v)] <- List<NodeId * NodeId>()
    
    let nodes = backEdges |> List.map (fun (_, u, v) -> (u, v))
    let n = nodes.Length
    for i in 0 .. n - 2 do
        for j in i + 1 .. n - 1 do
            let (u, v) = nodes.[i]
            let (x, y) = nodes.[j]
            let tu = entryTimes.[u]
            let tv = entryTimes.[v]
            let tx = entryTimes.[x]
            let ty = entryTimes.[y]
            if interlaced tu tv tx ty u v x y entryTimes finishTimes then
                adj.[(u, v)].Add((x, y))
                adj.[(x, y)].Add((u, v))
    adj

let private checkBipartite (adj: Dictionary<NodeId * NodeId, List<NodeId * NodeId>>) =
    let colors = Dictionary<NodeId * NodeId, int>()
    let mutable possible = true

    for kvp in adj do
        let startNode = kvp.Key
        if not (colors.ContainsKey(startNode)) && possible then
            let queue = Queue<(NodeId * NodeId) * int>()
            queue.Enqueue((startNode, 0))
            colors.[startNode] <- 0

            while queue.Count > 0 && possible do
                let (u, c) = queue.Dequeue()
                let nextColor = 1 - c
                let neighbors = adj.[u]
                for v in neighbors do
                    match colors.TryGetValue(v) with
                    | true, existingColor ->
                        if existingColor <> nextColor then
                            possible <- false
                    | _ ->
                        colors.[v] <- nextColor
                        queue.Enqueue((v, nextColor))

    if possible then
        let result = colors |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
        Some result
    else
        None

let private lrTest (graph: Graph<'n, 'e>) =
    let nodes = allNodes graph
    match nodes with
    | [] -> Some (Map.empty, ([], Dictionary(), Dictionary(), Dictionary(), Dictionary()))
    | root :: _ ->
        let (orderedEdges, lowpoints, entryTimes, finishTimes, parents) = dfsOrient graph root
        let backEdges = orderedEdges |> List.filter (fun (t, _, _) -> t = Back)
        let conflictGraph = buildConflictGraph backEdges entryTimes finishTimes
        match checkBipartite conflictGraph with
        | Some colors -> Some (colors, (orderedEdges, lowpoints, entryTimes, finishTimes, parents))
        | None -> None

let private planarHeuristic (graph: Graph<'n, 'e>) : bool =
    let n = nodeCount graph
    let e = edgeCount graph
    if n <= 4 then
        true
    elif e > 3 * n - 6 then
        false
    elif n = 5 && e <= 9 then
        true
    elif isBipartite graph then
        e <= 2 * n - 4
    else
        true

let private doExactPlanar (graph: Graph<'n, 'e>) : ComponentMetadata<'n, 'e> option =
    if nodeCount graph <= 1 then
        Some { Graph = graph; Partitions = Map.empty; TreeInfo = ([], Dictionary(), Dictionary(), Dictionary(), Dictionary()) }
    else
        match lrTest graph with
        | Some (partitions, treeInfo) ->
            Some { Graph = graph; Partitions = partitions; TreeInfo = treeInfo }
        | None -> None

let private runExactPlanarTest (graph: Graph<'n, 'e>) : ComponentMetadata<'n, 'e> list option =
    let components = Yog.Connectivity.connectedComponents graph
    let mutable possible = true
    let acc = List<ComponentMetadata<'n, 'e>>()
    let mutable i = 0
    while i < components.Length && possible do
        let nodes = components.[i]
        let sub = subgraph nodes graph
        match doExactPlanar sub with
        | Some meta -> acc.Add(meta)
        | None -> possible <- false
        i <- i + 1
    if possible then Some (Seq.toList acc) else None

/// Checks if the graph is planar (exactly).
let isPlanar (graph: Graph<'n, 'e>) : bool =
    match graph.Kind with
    | Directed -> false
    | Undirected ->
        if planarHeuristic graph then
            (runExactPlanarTest graph).IsSome
        else
            false

let private subtreeBackEdgeSide (v: NodeId) (edges: OrientedEdge list) (partitions: Map<NodeId * NodeId, int>) (times: Dictionary<NodeId, int>) (ancestorLimit: NodeId) : int =
    let limitTime = times.[ancestorLimit]
    let found = 
        edges 
        |> List.tryFind (fun (t, u, target) -> 
            t = Back && times.[u] >= times.[v] && times.[target] < limitTime
        )
    match found with
    | Some (_, u, target) -> Map.tryFind (u, target) partitions |> Option.defaultValue 0
    | None -> 0

let private buildComponentEmbedding (meta: ComponentMetadata<'n, 'e>) : Map<NodeId, NodeId list> =
    let graph = meta.Graph
    let partitions = meta.Partitions
    let (edges, _, times, _, parents) = meta.TreeInfo

    let nodesIncident = Dictionary<NodeId, NodeIncident>()
    for u in allNodes graph do
        let parent = 
            match parents.TryGetValue(u) with
            | true, p -> Some p
            | false, _ -> None
        nodesIncident.[u] <- { Parent = parent; Children = []; BackOut = []; BackIn = [] }

    for (t, u, v) in edges do
        if t = Tree then
            let info = nodesIncident.[u]
            nodesIncident.[u] <- { info with Children = v :: info.Children }
        else
            let infoU = nodesIncident.[u]
            nodesIncident.[u] <- { infoU with BackOut = v :: infoU.BackOut }
            let infoV = nodesIncident.[v]
            nodesIncident.[v] <- { infoV with BackIn = u :: infoV.BackIn }

    let acc = Dictionary<NodeId, NodeId list>()
    for kvp in nodesIncident do
        let u = kvp.Key
        let info = kvp.Value

        let leftBo = List<NodeId>()
        let rightBo = List<NodeId>()
        for v in info.BackOut do
            if Map.tryFind (u, v) partitions = Some 0 then
                leftBo.Add(v)
            else
                rightBo.Add(v)

        let leftBi = List<NodeId>()
        let rightBi = List<NodeId>()
        for v in info.BackIn do
            if Map.tryFind (v, u) partitions = Some 0 then
                leftBi.Add(v)
            else
                rightBi.Add(v)

        let leftC = List<NodeId>()
        let rightC = List<NodeId>()
        for v in info.Children do
            if subtreeBackEdgeSide v edges partitions times u = 0 then
                leftC.Add(v)
            else
                rightC.Add(v)

        let sortedLeftBo = leftBo |> Seq.sortBy (fun v -> times.[v]) |> Seq.toList
        let sortedRightBo = rightBo |> Seq.sortByDescending (fun v -> times.[v]) |> Seq.toList

        let combined = List<NodeId>()
        match info.Parent with
        | Some p -> combined.Add(p)
        | None -> ()

        combined.AddRange(leftBi)
        combined.AddRange(leftC)
        combined.AddRange(sortedLeftBo)
        combined.AddRange(sortedRightBo)
        combined.AddRange(rightC)
        combined.AddRange(rightBi)

        let uniqueOrder = combined |> Seq.distinct |> Seq.toList
        acc.[u] <- uniqueOrder

    acc |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq

let private doReduceToMinimal (graph: Graph<'n, 'e>) : Graph<'n, 'e> =
    let edges = 
        allNodes graph 
        |> List.collect (fun u -> successorIds u graph |> List.filter (fun v -> u <= v) |> List.map (fun v -> (u, v)))
    
    let mutable currentGraph = graph
    for (u, v) in edges do
        let reduced = removeEdge u v currentGraph
        if not (isPlanar reduced) then
            currentGraph <- reduced

    let mutable finalGraph = currentGraph
    for u in allNodes currentGraph do
        if (neighbors u currentGraph).Length = 0 then
            finalGraph <- removeNode u finalGraph
    finalGraph

let private allDegrees (graph: Graph<'n, 'e>) (d: int) : bool =
    allNodes graph |> List.forall (fun u -> (neighbors u graph).Length = d)

let rec private smoothPaths (graph: Graph<'n, 'e>) (defaultWeight: 'e) : Graph<'n, 'e> =
    let deg2Node = allNodes graph |> List.tryFind (fun u -> (neighbors u graph).Length = 2)
    match deg2Node with
    | None -> graph
    | Some u ->
        match neighborIds u graph with
        | [v; w] ->
            let weight = 
                match edgeData v u graph with
                | Some wData -> wData
                | None -> 
                    match edgeData u w graph with
                    | Some wData -> wData
                    | None -> failwith "Incident edge data not found during path smoothing"
            let simplified = 
                graph 
                |> removeNode u
                |> addEdge v w weight
            smoothPaths simplified weight
        | _ -> graph

let private identifyKuratowskiType (graph: Graph<'n, 'e>) : KuratowskiType =
    let firstEdgeWeightOpt = allEdges graph |> List.tryHead |> Option.map (fun (_, _, w) -> w)
    match firstEdgeWeightOpt with
    | None -> Unknown
    | Some defaultWeight ->
        let coreGraph = smoothPaths graph defaultWeight
        let nodes = allNodes coreGraph
        let count = nodes.Length
        if count = 5 && allDegrees coreGraph 4 then
            K5
        elif count = 6 && allDegrees coreGraph 3 && isBipartite coreGraph then
            K33
        else
            let degSeq = 
                nodes 
                |> List.map (fun u -> (neighbors u coreGraph).Length) 
                |> List.sortDescending
            match degSeq with
            | [4; 4; 4; 4; 4] -> K5
            | [3; 3; 3; 3; 3; 3] -> K33
            | _ -> Unknown

/// Identifies a Kuratowski witness (a subdivision of K5 or K3,3) that proves
/// the graph is non-planar.
let kuratowskiWitness (graph: Graph<'n, 'e>) : KuratowskiWitness<'n, 'e> option =
    match graph.Kind with
    | Directed -> None
    | Undirected ->
        if isPlanar graph then
            None
        else
            let minimal = doReduceToMinimal graph
            let mutable kType = identifyKuratowskiType minimal
            if kType = Unknown then
                kType <- identifyKuratowskiType graph

            let edges = 
                allEdges minimal 
                |> List.map (fun (u, v, _) -> (u, v))

            Some { Type = kType
                   Nodes = allNodes minimal
                   Edges = edges
                   Subgraph = minimal }

/// Returns a combinatorial embedding if the graph is planar.
let planarEmbedding (graph: Graph<'n, 'e>) : Result<Map<NodeId, NodeId list>, KuratowskiWitness<'n, 'e> option> =
    match graph.Kind with
    | Directed -> Result.Error None
    | Undirected ->
        if planarHeuristic graph then
            match runExactPlanarTest graph with
            | Some componentMetaList ->
                let embedding = 
                    componentMetaList
                    |> List.fold (fun acc meta ->
                        let compEmb = buildComponentEmbedding meta
                        Map.fold (fun a k v -> Map.add k v a) acc compEmb
                    ) Map.empty
                Result.Ok embedding
            | None ->
                Result.Error (kuratowskiWitness graph)
        else
            Result.Error (kuratowskiWitness graph)
