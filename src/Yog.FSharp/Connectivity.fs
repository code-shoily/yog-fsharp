/// Graph connectivity analysis - finding bridges, articulation points, and strongly connected components.
namespace Yog

open System.Collections.Generic
open Yog.Model

/// Represents a bridge (critical edge) in an undirected graph.
/// Bridges are stored as ordered pairs where the first node ID is smaller.
type Bridge = NodeId * NodeId

/// Results from connectivity analysis containing bridges and articulation points.
type ConnectivityResults =
    { Bridges: Bridge list
      ArticulationPoints: NodeId list }

module Connectivity =

    /// Analyzes an undirected graph to find all bridges and articulation points
    /// using Tarjan's algorithm in a single DFS pass.
    let analyze (graph: Graph<'n, 'e>) : Result<ConnectivityResults, string> =
        match graph.Kind with
        | Directed -> Error "analyze requires an undirected graph; use stronglyConnectedComponents for directed graphs"
        | Undirected ->
            let tin = Dictionary<NodeId, int>()
            let low = Dictionary<NodeId, int>()
            let visited = HashSet<NodeId>()
            let points = HashSet<NodeId>()

            let mutable bridges = []
            let mutable timer = 0

            let rec dfs (v: NodeId) (parentOpt: NodeId option) =
                visited.Add(v) |> ignore
                tin.[v] <- timer
                low.[v] <- timer
                timer <- timer + 1

                let mutable children = 0

                for toId in successorIds v graph do
                    if Some toId = parentOpt then
                        ()
                    elif visited.Contains(toId) then
                        low.[v] <- min low.[v] tin.[toId]
                    else
                        dfs toId (Some v)
                        low.[v] <- min low.[v] low.[toId]

                        if low.[toId] > tin.[v] then
                            let bridge = if v < toId then (v, toId) else (toId, v)
                            bridges <- bridge :: bridges

                        if parentOpt.IsSome && low.[toId] >= tin.[v] then
                            points.Add(v) |> ignore

                        children <- children + 1

                if parentOpt.IsNone && children > 1 then
                    points.Add(v) |> ignore

            for node in allNodes graph do
                if not (visited.Contains(node)) then
                    dfs node None

            Ok { Bridges = List.rev bridges
                 ArticulationPoints = points |> Seq.toList }

    /// Finds Strongly Connected Components (SCC) using Tarjan's Algorithm.
    let stronglyConnectedComponents (graph: Graph<'n, 'e>) : NodeId list list =
        let indices = Dictionary<NodeId, int>()
        let lowLinks = Dictionary<NodeId, int>()
        let onStack = HashSet<NodeId>()
        let stack = Stack<NodeId>()

        let mutable index = 0
        let mutable components = []

        let rec strongConnect (v: NodeId) =
            indices.[v] <- index
            lowLinks.[v] <- index
            index <- index + 1

            stack.Push(v)
            onStack.Add(v) |> ignore

            for w in successorIds v graph do
                if not (indices.ContainsKey(w)) then
                    strongConnect w
                    lowLinks.[v] <- min lowLinks.[v] lowLinks.[w]
                elif onStack.Contains(w) then
                    lowLinks.[v] <- min lowLinks.[v] indices.[w]

            if lowLinks.[v] = indices.[v] then
                let mutable comp = []
                let mutable finished = false

                while not finished do
                    let w = stack.Pop()
                    onStack.Remove(w) |> ignore
                    comp <- w :: comp

                    if w = v then
                        finished <- true

                components <- comp :: components

        for node in allNodes graph do
            if not (indices.ContainsKey(node)) then
                strongConnect node

        List.rev components

    /// Finds Strongly Connected Components (SCC) using Kosaraju's Algorithm.
    let kosaraju (graph: Graph<'n, 'e>) : NodeId list list =
        let visited = HashSet<NodeId>()
        let finishStack = Stack<NodeId>()

        let rec dfs1 (v: NodeId) =
            if visited.Add(v) then
                for w in successorIds v graph do
                    dfs1 w
                finishStack.Push(v)

        for node in allNodes graph do
            dfs1 node

        let transposed = Yog.Transform.transpose graph
        visited.Clear()

        let mutable components = []

        let rec dfs2 (v: NodeId) (comp: NodeId list) =
            if visited.Add(v) then
                let mutable currentComp = v :: comp

                for w in successorIds v transposed do
                    currentComp <- dfs2 w currentComp

                currentComp
            else
                comp

        while finishStack.Count > 0 do
            let v = finishStack.Pop()

            if not (visited.Contains(v)) then
                let comp = dfs2 v []
                components <- comp :: components

        List.rev components

    /// Finds Connected Components in an undirected graph.
    let connectedComponents (graph: Graph<'n, 'e>) : NodeId list list =
        let visited = HashSet<NodeId>()
        let rec dfsCollect node comp =
            if visited.Add(node) then
                let mutable currentComp = node :: comp
                for neighbor in successorIds node graph do
                    currentComp <- dfsCollect neighbor currentComp
                currentComp
            else
                comp
        
        let mutable components = []
        for node in allNodes graph do
            if not (visited.Contains(node)) then
                let comp = dfsCollect node []
                components <- comp :: components
        List.rev components

    /// Finds Weakly Connected Components in a directed graph.
    let weaklyConnectedComponents (graph: Graph<'n, 'e>) : NodeId list list =
        let visited = HashSet<NodeId>()
        let rec dfsCollect node comp =
            if visited.Add(node) then
                let mutable currentComp = node :: comp
                for neighbor in neighborIds node graph do
                    currentComp <- dfsCollect neighbor currentComp
                currentComp
            else
                comp
        
        let mutable components = []
        for node in allNodes graph do
            if not (visited.Contains(node)) then
                let comp = dfsCollect node []
                components <- comp :: components
        List.rev components

    /// Returns the core number of every node in an undirected graph.
    let coreNumbers (graph: Graph<'n, 'e>) : Result<Map<NodeId, int>, string> =
        match graph.Kind with
        | Directed -> Error "coreNumbers requires an undirected graph"
        | Undirected ->
            let nodes = allNodes graph
            let mutable degrees = nodes |> List.map (fun u -> u, (neighbors u graph).Length) |> Map.ofList
            let maxDeg = if Map.isEmpty degrees then 0 else degrees |> Map.values |> Seq.max
            
            let mutable buckets = Map.empty
            for u in nodes do
                let deg = Map.find u degrees
                let existing = Map.tryFind deg buckets |> Option.defaultValue []
                buckets <- Map.add deg (u :: existing) buckets
                
            let mutable processed = Set.empty
            let mutable cores = Map.empty
            
            for i in 0 .. maxDeg do
                let mutable finished = false
                while not finished do
                    match Map.tryFind i buckets with
                    | None | Some [] -> finished <- true
                    | Some (u :: rest) ->
                        buckets <- Map.add i rest buckets
                        if not (Set.contains u processed) then
                            cores <- Map.add u i cores
                            processed <- Set.add u processed
                            
                            let neighborsList =
                                match Map.tryFind u graph.OutEdges with
                                | Some nbrs -> nbrs |> Map.keys |> Seq.toList
                                | None -> []
                                
                            for v in neighborsList do
                                if not (Set.contains v processed) then
                                    let oldVDeg = Map.find v degrees
                                    let newVDeg = oldVDeg - 1
                                    degrees <- Map.add v newVDeg degrees
                                    
                                    let targetBucket = max newVDeg i
                                    let existing = Map.tryFind targetBucket buckets |> Option.defaultValue []
                                    buckets <- Map.add targetBucket (v :: existing) buckets
            Ok cores

    /// Returns the maximal subgraph where every node has at least degree k.
    let kCore (graph: Graph<'n, 'e>) (k: int) : Result<Graph<'n, 'e>, string> =
        match graph.Kind with
        | Directed -> Error "kCore requires an undirected graph"
        | Undirected ->
            let nodes = allNodes graph
            let mutable degrees = nodes |> List.map (fun u -> u, (neighbors u graph).Length) |> Map.ofList
            let mutable pruneQueue = nodes |> List.filter (fun u -> Map.find u degrees < k)
            let mutable queueSet = Set.ofList pruneQueue
            let mutable pruned = Set.empty
            
            while not (List.isEmpty pruneQueue) do
                let u = List.head pruneQueue
                pruneQueue <- List.tail pruneQueue
                queueSet <- Set.remove u queueSet
                
                if not (Set.contains u pruned) then
                    pruned <- Set.add u pruned
                    let neighborsList =
                        match Map.tryFind u graph.OutEdges with
                        | Some nbrs -> nbrs |> Map.keys |> Seq.toList
                        | None -> []
                        
                    for v in neighborsList do
                        if not (Set.contains v pruned) then
                            let oldDeg = Map.find v degrees
                            let newDeg = oldDeg - 1
                            degrees <- Map.add v newDeg degrees
                            if newDeg < k && not (Set.contains v queueSet) then
                                pruneQueue <- v :: pruneQueue
                                queueSet <- Set.add v queueSet
                                
            let remaining = nodes |> List.filter (fun u -> not (Set.contains u pruned))
            Ok (Yog.Transform.subgraph remaining graph)

    /// Returns the degeneracy of the graph.
    let degeneracy (graph: Graph<'n, 'e>) : Result<int, string> =
        match coreNumbers graph with
        | Error msg -> Error msg
        | Ok cores ->
            if Map.isEmpty cores then Ok 0
            else Ok (cores |> Map.values |> Seq.max)

    /// Groups nodes by their core number (k-shell decomposition).
    let shellDecomposition (graph: Graph<'n, 'e>) : Result<Map<int, NodeId list>, string> =
        match coreNumbers graph with
        | Error msg -> Error msg
        | Ok cores ->
            let mutable shells = Map.empty
            for kvp in cores do
                let node = kvp.Key
                let core = kvp.Value
                let existing = Map.tryFind core shells |> Option.defaultValue []
                shells <- Map.add core (node :: existing) shells
            Ok shells

    module Reachability =
        type ReachabilityDirection =
            | Ancestors
            | Descendants

        type private CondensationNodeData = { Size: int }

        let private hllPrecision = 10
        let private hllNumRegisters = 1024
        let private hllAlpha = 0.7213 / (1.0 + 1.079 / 1024.0)

        let private countLeadingZeros (value: uint32) (bits: int) =
            let lz = System.Numerics.BitOperations.LeadingZeroCount(value)
            lz - (32 - bits)

        let private hashNodeId (id: NodeId) : uint32 =
            let mutable hash = 2166136261u
            let b0 = byte (id &&& 0xFF)
            let b1 = byte ((id >>> 8) &&& 0xFF)
            let b2 = byte ((id >>> 16) &&& 0xFF)
            let b3 = byte ((id >>> 24) &&& 0xFF)
            hash <- (hash ^^^ uint32 b0) * 16777619u
            hash <- (hash ^^^ uint32 b1) * 16777619u
            hash <- (hash ^^^ uint32 b2) * 16777619u
            hash <- (hash ^^^ uint32 b3) * 16777619u
            hash

        type private Hll = byte[]

        let private initHll () : Hll = Array.zeroCreate hllNumRegisters

        let private hllAdd (hll: Hll) (value: NodeId) : Hll =
            let hash = hashNodeId value
            let index = int (hash &&& uint32 (hllNumRegisters - 1))
            let remaining = hash >>> hllPrecision
            let zeros = countLeadingZeros remaining (32 - hllPrecision)
            let val' = byte (zeros + 1)
            let current = hll.[index]
            if val' > current then
                let nextHll = Array.copy hll
                nextHll.[index] <- val'
                nextHll
            else
                hll

        let private hllUnion (hll1: Hll) (hll2: Hll) : Hll =
            let result = Array.zeroCreate hllNumRegisters
            for i in 0 .. hllNumRegisters - 1 do
                result.[i] <- max hll1.[i] hll2.[i]
            result

        let private hllCount (hll: Hll) : int =
            let mutable sumInverse = 0.0
            for i in 0 .. hllNumRegisters - 1 do
                let maxZeros = float hll.[i]
                sumInverse <- sumInverse + (2.0 ** -maxZeros)
            
            let rawEstimate = hllAlpha * float hllNumRegisters * float hllNumRegisters / sumInverse
            
            let estimate =
                if rawEstimate <= 2.5 * float hllNumRegisters then
                    let mutable emptyRegisters = 0
                    for i in 0 .. hllNumRegisters - 1 do
                        if hll.[i] = 0uy then
                            emptyRegisters <- emptyRegisters + 1
                    if emptyRegisters <> 0 then
                        float hllNumRegisters * log (float hllNumRegisters / float emptyRegisters)
                    else
                        rawEstimate
                else
                    rawEstimate
            max 0 (int (round estimate))

        let private solveAcyclicReachabilitySets (graph: Graph<'n, 'e>) (sorted: NodeId list) (direction: ReachabilityDirection) =
            let nodesToProcess =
                match direction with
                | Descendants -> List.rev sorted
                | Ancestors -> sorted

            let getRelated =
                match direction with
                | Descendants -> fun node -> successorIds node graph
                | Ancestors -> fun node -> predecessorIds node graph

            let mutable reachabilitySets = Map.empty

            for node in nodesToProcess do
                let related = getRelated node
                let mutable allReachable = Set.ofList related

                for neighbor in related do
                    match Map.tryFind neighbor reachabilitySets with
                    | Some neighborSet ->
                        allReachable <- Set.union allReachable neighborSet
                    | None -> ()

                reachabilitySets <- Map.add node allReachable reachabilitySets

            reachabilitySets

        let private solveAcyclicCounts (graph: Graph<'n, 'e>) (sorted: NodeId list) (direction: ReachabilityDirection) =
            let reachabilitySets = solveAcyclicReachabilitySets graph sorted direction
            reachabilitySets |> Map.map (fun _ set -> Set.count set)

        let private buildCondensationGraph (graph: Graph<'n, 'e>) (sccs: NodeId list list) (nodeToScc: Map<NodeId, int>) =
            let init = Yog.Model.empty Directed
            let mutable condensation = init
            for i in 0 .. sccs.Length - 1 do
                condensation <- Yog.Model.addNode i { Size = List.length sccs.[i] } condensation

            for src in graph.Nodes.Keys do
                let srcScc = Map.find src nodeToScc
                let successors = successorIds src graph
                for dst in successors do
                    let dstScc = Map.find dst nodeToScc
                    if srcScc <> dstScc then
                        condensation <- Yog.Model.addEdge srcScc dstScc 1 condensation
            condensation

        let private solveCyclicCounts (graph: Graph<'n, 'e>) (direction: ReachabilityDirection) =
            let sccs = stronglyConnectedComponents graph
            let mutable nodeToScc = Map.empty
            for i in 0 .. sccs.Length - 1 do
                for node in sccs.[i] do
                    nodeToScc <- Map.add node i nodeToScc

            let condensation = buildCondensationGraph graph sccs nodeToScc
            
            let sortedSccs =
                match Yog.Traversal.topologicalSort condensation with
                | Ok s -> s
                | Error _ -> failwith "condensation graph is not acyclic"

            let sccReachabilitySets = solveAcyclicReachabilitySets condensation sortedSccs direction

            let mutable result = Map.empty
            for node in graph.Nodes.Keys do
                let sccId = Map.find node nodeToScc
                let reachableSccIds = Map.tryFind sccId sccReachabilitySets |> Option.defaultValue Set.empty
                
                let mutable nodeCount = 0
                for id in reachableSccIds do
                    let sccData = Map.find id condensation.Nodes
                    nodeCount <- nodeCount + sccData.Size

                let mySccData = Map.find sccId condensation.Nodes
                let mySccSize = mySccData.Size
                result <- Map.add node (nodeCount + (mySccSize - 1)) result
            result

        let private solveAcyclicHll (graph: Graph<'n, 'e>) (sorted: NodeId list) (direction: ReachabilityDirection) =
            let nodesToProcess =
                match direction with
                | Descendants -> List.rev sorted
                | Ancestors -> sorted

            let getRelated =
                match direction with
                | Descendants -> fun node -> successorIds node graph
                | Ancestors -> fun node -> predecessorIds node graph

            let mutable hllRegisters = Map.empty

            for node in nodesToProcess do
                let related = getRelated node
                let mutable baseHll = initHll ()
                for r in related do
                    baseHll <- hllAdd baseHll r

                let mutable merged = baseHll
                for neighbor in related do
                    let neighborHll = Map.tryFind neighbor hllRegisters |> Option.defaultValue (initHll ())
                    merged <- hllUnion merged neighborHll

                hllRegisters <- Map.add node merged hllRegisters

            hllRegisters |> Map.map (fun _ hll -> hllCount hll)

        let private solveCyclicHll (graph: Graph<'n, 'e>) (direction: ReachabilityDirection) =
            let sccs = stronglyConnectedComponents graph
            let mutable nodeToScc = Map.empty
            for i in 0 .. sccs.Length - 1 do
                for node in sccs.[i] do
                    nodeToScc <- Map.add node i nodeToScc

            let condensation = buildCondensationGraph graph sccs nodeToScc
            
            let sortedSccs =
                match Yog.Traversal.topologicalSort condensation with
                | Ok s -> s
                | Error _ -> failwith "condensation graph is not acyclic"

            let mutable sccBaseHlls = Map.empty
            for i in 0 .. sccs.Length - 1 do
                let mutable hll = initHll ()
                for node in sccs.[i] do
                    hll <- hllAdd hll node
                sccBaseHlls <- Map.add i hll sccBaseHlls

            let sccsToProcess =
                match direction with
                | Descendants -> List.rev sortedSccs
                | Ancestors -> sortedSccs

            let getSccRelated =
                match direction with
                | Descendants -> fun node -> successorIds node condensation
                | Ancestors -> fun node -> predecessorIds node condensation

            let mutable sccFinalHlls = Map.empty
            for sccId in sccsToProcess do
                let myBase = Map.find sccId sccBaseHlls
                let children = getSccRelated sccId

                let mutable mergedChildren = myBase
                for childId in children do
                    let childHll = Map.tryFind childId sccFinalHlls |> Option.defaultValue (initHll ())
                    mergedChildren <- hllUnion mergedChildren childHll

                sccFinalHlls <- Map.add sccId mergedChildren sccFinalHlls

            let mutable result = Map.empty
            for nodeId in graph.Nodes.Keys do
                let sccId = Map.find nodeId nodeToScc
                let sccHll = Map.tryFind sccId sccFinalHlls |> Option.defaultValue (initHll ())
                let totalCount = hllCount sccHll
                result <- Map.add nodeId (max 0 (totalCount - 1)) result
            result

        /// Counts the number of ancestors or descendants for every node in the graph.
        let counts (graph: Graph<'n, 'e>) (direction: ReachabilityDirection) : Map<NodeId, int> =
            match Yog.Traversal.topologicalSort graph with
            | Ok sorted -> solveAcyclicCounts graph sorted direction
            | Error _ -> solveCyclicCounts graph direction

        /// Estimates the number of ancestors or descendants using HyperLogLog.
        let countsEstimate (graph: Graph<'n, 'e>) (direction: ReachabilityDirection) : Map<NodeId, int> =
            match Yog.Traversal.topologicalSort graph with
            | Ok sorted -> solveAcyclicHll graph sorted direction
            | Error _ -> solveCyclicHll graph direction
