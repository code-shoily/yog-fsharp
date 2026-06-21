namespace Yog

open System.Collections.Generic
open Yog.Model

/// Algorithm used to compute the spanning tree result.
type Algorithm =
    | Kruskal
    | Prim
    | Boruvka
    | ChuLiuEdmonds
    | Wilson

/// Represents an edge in the spanning tree.
type Edge<'e> =
    { From: NodeId; To: NodeId; Weight: 'e }

/// Result of a Spanning Tree computation.
type MstResult<'e> =
    { Edges: Edge<'e> list
      TotalWeight: 'e
      NodeCount: int
      EdgeCount: int
      Algorithm: Algorithm
      Root: NodeId option }

module Mst =

    /// Helper to build the final MstResult
    let private makeResult (edges: Edge<'e> list) (algorithm: Algorithm) (nodeCount: int) (root: NodeId option) (add: 'e -> 'e -> 'e) (zero: 'e) : MstResult<'e> =
        let totalWeight = (zero, edges) ||> List.fold (fun acc edge -> add acc edge.Weight)
        { Edges = edges
          TotalWeight = totalWeight
          NodeCount = nodeCount
          EdgeCount = List.length edges
          Algorithm = algorithm
          Root = root }

    /// Finds the Minimum Spanning Tree (MST) using Kruskal's algorithm.
    let kruskal (compare: 'e -> 'e -> int) (add: 'e -> 'e -> 'e) (zero: 'e) (graph: Graph<'n, 'e>) : Result<MstResult<'e>, string> =
        match graph.Kind with
        | Directed -> Error "kruskal requires an undirected graph"
        | Undirected ->
            let edges =
                graph.OutEdges
                |> Map.fold (fun acc src targets ->
                    targets |> Map.fold (fun innerAcc dst weight ->
                        if src <= dst then { From = src; To = dst; Weight = weight } :: innerAcc
                        else innerAcc
                    ) acc
                ) []
                |> List.sortWith (fun a b -> compare a.Weight b.Weight)

            let rec doKruskal edgesList dsu acc =
                match edgesList with
                | [] -> List.rev acc
                | edge :: rest ->
                    let dsu1, rootFrom = DisjointSet.find edge.From dsu
                    let dsu2, rootTo = DisjointSet.find edge.To dsu1

                    if rootFrom = rootTo then
                        doKruskal rest dsu2 acc
                    else
                        let nextDsu = DisjointSet.union edge.From edge.To dsu2
                        doKruskal rest nextDsu (edge :: acc)

            let mstEdges = doKruskal edges DisjointSet.empty []
            Ok (makeResult mstEdges Kruskal (order graph) None add zero)

    /// Finds the Minimum Spanning Tree (MST) using Prim's algorithm.
    let prim (compare: 'e -> 'e -> int) (add: 'e -> 'e -> 'e) (zero: 'e) (graph: Graph<'n, 'e>) : Result<MstResult<'e>, string> =
        match graph.Kind with
        | Directed -> Error "prim requires an undirected graph"
        | Undirected ->
            match allNodes graph with
            | [] -> Ok (makeResult [] Prim 0 None add zero)
            | start :: _ ->
                let comparer =
                    { new IComparer<'e> with
                        member _.Compare(a, b) = compare a b }

                let pq = PriorityQueue<Edge<'e>, 'e>(comparer)
                let visited = HashSet<NodeId>()

                visited.Add(start) |> ignore

                for (dst, weight) in successors start graph do
                    pq.Enqueue({ From = start; To = dst; Weight = weight }, weight)

                let mutable mstEdges = []

                while pq.Count > 0 do
                    let edge = pq.Dequeue()

                    if visited.Add(edge.To) then
                        mstEdges <- edge :: mstEdges

                        for (dst, weight) in successors edge.To graph do
                            if not (visited.Contains(dst)) then
                                pq.Enqueue({ From = edge.To; To = dst; Weight = weight }, weight)

                Ok (makeResult (List.rev mstEdges) Prim (order graph) None add zero)

    /// Finds the Minimum Spanning Tree (MST) using Borůvka's algorithm.
    let boruvka (compare: 'e -> 'e -> int) (add: 'e -> 'e -> 'e) (zero: 'e) (graph: Graph<'n, 'e>) : Result<MstResult<'e>, string> =
        match graph.Kind with
        | Directed -> Error "boruvka requires an undirected graph"
        | Undirected ->
            let nodes = allNodes graph
            let dsu = nodes |> List.fold (fun acc node -> DisjointSet.add node acc) DisjointSet.empty
            let allEdgesList =
                graph.OutEdges
                |> Map.fold (fun acc src targets ->
                    targets |> Map.fold (fun innerAcc dst weight ->
                        if src <= dst then { From = src; To = dst; Weight = weight } :: innerAcc
                        else innerAcc
                    ) acc
                ) []

            let rec loop currentDsu currentMst =
                if DisjointSet.countSets currentDsu <= 1 then
                    List.rev currentMst
                else
                    let cheapest =
                        (Map.empty, allEdgesList)
                        ||> List.fold (fun accMap edge ->
                            let dsu1, rootU = DisjointSet.find edge.From currentDsu
                            let _, rootV = DisjointSet.find edge.To dsu1
                            if rootU = rootV then accMap
                            else
                                let updateBest root map =
                                    match Map.tryFind root map with
                                    | None -> Map.add root edge map
                                    | Some existing ->
                                        if compare edge.Weight existing.Weight < 0 then Map.add root edge map
                                        else map
                                accMap |> updateBest rootU |> updateBest rootV
                        )

                    if cheapest.IsEmpty then
                        List.rev currentMst
                    else
                        let edgesToAdd =
                            cheapest
                            |> Map.toList
                            |> List.map snd
                            |> List.fold (fun (resList, visitedSet) edge ->
                                let key = if edge.From > edge.To then (edge.To, edge.From) else (edge.From, edge.To)
                                if Set.contains key visitedSet then (resList, visitedSet)
                                else (edge :: resList, Set.add key visitedSet)
                            ) ([], Set.empty)
                            |> fst

                        let nextDsu, nextMst =
                            ((currentDsu, currentMst), edgesToAdd)
                            ||> List.fold (fun (dsuAcc, mstAcc) edge ->
                                (DisjointSet.union edge.From edge.To dsuAcc, edge :: mstAcc)
                            )

                        let oldCount = DisjointSet.countSets currentDsu
                        let newCount = DisjointSet.countSets nextDsu
                        if newCount = oldCount then List.rev currentMst
                        else loop nextDsu nextMst

            let mstEdges = loop dsu []
            Ok (makeResult mstEdges Boruvka (List.length nodes) None add zero)

    type EdmondsGraph<'e> =
        { Nodes: NodeId list
          Edges: Edge<'e> list }

    type CycleInfo<'e> =
        { SuperNode: NodeId
          Cycle: NodeId list
          Mapping: Map<NodeId * NodeId, Edge<'e>> }

    let rec private doEdmonds (graph: EdmondsGraph<'e>) (root: NodeId) (compare: 'e -> 'e -> int) (subtract: 'e -> 'e -> 'e) (superCounter: int) : Result<Edge<'e> list, string> =
        let nodes = graph.Nodes
        let bestIn =
            (Map.empty, nodes)
            ||> List.fold (fun acc nodeId ->
                if nodeId = root then acc
                else
                    let incoming = graph.Edges |> List.filter (fun e -> e.To = nodeId)
                    match incoming with
                    | [] -> acc
                    | first :: rest ->
                        let best =
                            (first, rest)
                            ||> List.fold (fun bestOpt e ->
                                if compare e.Weight bestOpt.Weight < 0 then e else bestOpt
                            )
                        Map.add nodeId best acc
            )

        let unreachable = nodes |> List.exists (fun v -> v <> root && not (Map.containsKey v bestIn))
        if unreachable then
            Error "No arborescence exists"
        else
            let rec findCycleDfs (node: NodeId) (visited: Map<NodeId, string>) (path: NodeId list) : NodeId list option =
                match Map.tryFind node visited with
                | Some "visiting" ->
                    let cycle = node :: (path |> List.takeWhile (fun x -> x <> node))
                    Some (List.rev cycle)
                | Some _ -> None
                | None ->
                    match Map.tryFind node bestIn with
                    | None -> None
                    | Some edge ->
                        findCycleDfs edge.From (Map.add node "visiting" visited) (node :: path)

            let cycleOpt =
                nodes
                |> List.tryPick (fun startNode -> findCycleDfs startNode Map.empty [])

            match cycleOpt with
            | None -> Ok (bestIn |> Map.toList |> List.map snd)
            | Some cycleNodes ->
                let superNode = superCounter
                let nextCounter = superCounter - 1
                
                let cycleSet = Set.ofList cycleNodes
                let newNodes = nodes |> List.filter (fun n -> not (Set.contains n cycleSet))
                let newNodesWithSuper = superNode :: newNodes

                let candidates =
                    graph.Edges
                    |> List.choose (fun edge ->
                        let uIn = Set.contains edge.From cycleSet
                        let vIn = Set.contains edge.To cycleSet
                        match uIn, vIn with
                        | true, true -> None
                        | true, false -> Some ({ From = superNode; To = edge.To; Weight = edge.Weight }, edge)
                        | false, true ->
                            let bestInV = Map.find edge.To bestIn
                            let newWeight = subtract edge.Weight bestInV.Weight
                            Some ({ From = edge.From; To = superNode; Weight = newWeight }, edge)
                        | false, false -> Some (edge, edge)
                    )

                let deduped =
                    (Map.empty, candidates)
                    ||> List.fold (fun acc (cEdge, origEdge) ->
                        let key = (cEdge.From, cEdge.To)
                        match Map.tryFind key acc with
                        | Some (existing, _) ->
                            if compare cEdge.Weight existing.Weight < 0 then Map.add key (cEdge, origEdge) acc
                            else acc
                        | None -> Map.add key (cEdge, origEdge) acc
                    )

                let newEdges = deduped |> Map.toList |> List.map (fun (_, (c, _)) -> c)
                let mapping = deduped |> Map.map (fun _ (_, o) -> o)

                let contractedGraph = { Nodes = newNodesWithSuper; Edges = newEdges }
                let cycleInfo = { SuperNode = superNode; Cycle = cycleNodes; Mapping = mapping }

                match doEdmonds contractedGraph root compare subtract nextCounter with
                | Error msg -> Error msg
                | Ok contractedEdges ->
                    let entryEdgeContracted = contractedEdges |> List.tryFind (fun e -> e.To = superNode)
                    let entryOrig =
                        match entryEdgeContracted with
                        | Some e -> Map.tryFind (e.From, superNode) mapping
                        | None -> None

                    let nodeToBypass =
                        match entryOrig with
                        | Some orig -> orig.To
                        | None -> -1

                    let finalEdges =
                        contractedEdges
                        |> List.collect (fun e ->
                            if e.To = superNode then
                                match entryOrig with
                                | Some orig -> [ orig ]
                                | None -> []
                            elif e.From = superNode then
                                match Map.tryFind (superNode, e.To) mapping with
                                | Some orig -> [ orig ]
                                | None -> []
                            else [ e ]
                        )

                    let cycleEdges =
                        cycleNodes
                        |> List.choose (fun node ->
                            match Map.tryFind node bestIn with
                            | Some edge when edge.To <> nodeToBypass -> Some edge
                            | _ -> None
                        )

                    Ok (finalEdges @ cycleEdges)

    /// Finds the Minimum Spanning Arborescence (MSA) of a directed graph.
    let edmonds (compare: 'e -> 'e -> int) (add: 'e -> 'e -> 'e) (subtract: 'e -> 'e -> 'e) (zero: 'e) (root: NodeId) (graph: Graph<'n, 'e>) : Result<MstResult<'e>, string> =
        match graph.Kind with
        | Undirected -> Error "Edmonds' algorithm requires a directed graph"
        | Directed ->
            let nodes = allNodes graph
            let edges =
                graph.OutEdges
                |> Map.fold (fun acc src targets ->
                    targets |> Map.fold (fun innerAcc dst weight ->
                        { From = src; To = dst; Weight = weight } :: innerAcc
                    ) acc
                ) []
            let minId = nodes |> List.fold min 0
            match doEdmonds { Nodes = nodes; Edges = edges } root compare subtract (minId - 1) with
            | Ok mstEdges -> Ok (makeResult mstEdges ChuLiuEdmonds (List.length nodes) (Some root) add zero)
            | Error msg -> Error msg

    let rec private doWilsonLoop (graph: Graph<'n, 'e>) (unvisited: Set<NodeId>) (tree: Set<NodeId>) (accEdges: Edge<'e> list) (rng: System.Random) (add: 'e -> 'e -> 'e) (zero: 'e) : Result<Edge<'e> list, string> =
        if unvisited.IsEmpty then
            Ok (List.rev accEdges)
        else
            let unvisitedList = Set.toList unvisited
            let startNode = unvisitedList.[rng.Next(unvisitedList.Length)]

            let rec performLerw current pathMap =
                if Set.contains current tree then
                    Ok pathMap
                else
                    let successorsList = successorIds current graph
                    match successorsList with
                    | [] -> Error "wilson requires a connected graph"
                    | neighbors ->
                        let nextNode = neighbors.[rng.Next(neighbors.Length)]
                        performLerw nextNode (Map.add current nextNode pathMap)

            match performLerw startNode Map.empty with
            | Error msg -> Error msg
            | Ok pathMap ->
                let rec addPathToTree current tAcc unvAcc pEdges =
                    if Set.contains current tAcc then
                        (tAcc, unvAcc, pEdges)
                    else
                        let nextNode = Map.find current pathMap
                        let weight = edgeData current nextNode graph |> Option.get
                        let edge = { From = current; To = nextNode; Weight = weight }
                        let newT = Set.add current tAcc
                        let newUnv = Set.remove current unvAcc
                        addPathToTree nextNode newT newUnv (edge :: pEdges)

                let newTree, newUnvisited, pathEdges = addPathToTree startNode tree unvisited []
                doWilsonLoop graph newUnvisited newTree (accEdges @ pathEdges) rng add zero

    /// Generates a Uniform Spanning Tree (UST) using Wilson's algorithm.
    let wilson (add: 'e -> 'e -> 'e) (zero: 'e) (seed: int option) (graph: Graph<'n, 'e>) : Result<MstResult<'e>, string> =
        match graph.Kind with
        | Directed -> Error "wilson requires an undirected graph"
        | Undirected ->
            let nodes = allNodes graph
            match nodes with
            | [] -> Ok (makeResult [] Wilson 0 None add zero)
            | first :: _ ->
                let tree = Set.singleton first
                let unvisited = Set.difference (Set.ofList nodes) tree
                let rng =
                    match seed with
                    | Some s -> System.Random(s)
                    | None -> System.Random()
                match doWilsonLoop graph unvisited tree [] rng add zero with
                | Error msg -> Error msg
                | Ok edges -> Ok (makeResult edges Wilson (List.length nodes) None add zero)

    // Int convenience wrappers
    let kruskalInt (graph: Graph<'n, int>) = kruskal compare (+) 0 graph
    let primInt (graph: Graph<'n, int>) = prim compare (+) 0 graph
    let boruvkaInt (graph: Graph<'n, int>) = boruvka compare (+) 0 graph
    let edmondsInt (root: NodeId) (graph: Graph<'n, int>) = edmonds compare (+) (-) 0 root graph
    let wilsonInt (seed: int option) (graph: Graph<'n, int>) = wilson (+) 0 seed graph

    // Float convenience wrappers
    let kruskalFloat (graph: Graph<'n, float>) = kruskal compare (+) 0.0 graph
    let primFloat (graph: Graph<'n, float>) = prim compare (+) 0.0 graph
    let boruvkaFloat (graph: Graph<'n, float>) = boruvka compare (+) 0.0 graph
    let edmondsFloat (root: NodeId) (graph: Graph<'n, float>) = edmonds compare (+) (-) 0.0 root graph
    let wilsonFloat (seed: int option) (graph: Graph<'n, float>) = wilson (+) 0.0 seed graph
