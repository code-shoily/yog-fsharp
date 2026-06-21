namespace Yog.Multi

open Yog.Model

/// Unique identifier for edges in a multigraph.
type EdgeId = int

/// A multigraph that can hold multiple (parallel) edges between the same pair of nodes.
type MultiGraph<'n, 'e when 'n: equality and 'e: equality> =
    { Kind: GraphType
      Nodes: Map<NodeId, 'n>
      Edges: Map<EdgeId, NodeId * NodeId * 'e>
      OutEdgeIds: Map<NodeId, Set<EdgeId>>
      InEdgeIds: Map<NodeId, Set<EdgeId>>
      NextEdgeId: EdgeId }

/// Metadata for fold-style walks.
type WalkMetadata =
    { Depth: int
      Parent: (NodeId * EdgeId) option }

/// Control flow for walk operations.
type WalkControl =
    | Continue
    | Stop
    | Halt

module Model =

    /// Creates an empty multigraph of the given type.
    let empty kind : MultiGraph<'n, 'e> =
        { Kind = kind
          Nodes = Map.empty
          Edges = Map.empty
          OutEdgeIds = Map.empty
          InEdgeIds = Map.empty
          NextEdgeId = 0 }

    /// Creates a new empty directed multigraph.
    let directed<'n, 'e when 'n: equality and 'e: equality> () : MultiGraph<'n, 'e> = empty Directed

    /// Creates a new empty undirected multigraph.
    let undirected<'n, 'e when 'n: equality and 'e: equality> () : MultiGraph<'n, 'e> = empty Undirected

    /// Adds a node to the multigraph.
    let addNode id data (graph: MultiGraph<'n, 'e>) =
        { graph with Nodes = Map.add id data graph.Nodes }

    /// Returns all node IDs in the multigraph.
    let allNodes (graph: MultiGraph<'n, 'e>) = graph.Nodes |> Map.keys |> Seq.toList

    /// Returns the number of nodes.
    let order (graph: MultiGraph<'n, 'e>) = graph.Nodes.Count

    /// Adds an edge from src to dst.
    let addEdge src dst data (graph: MultiGraph<'n, 'e>) =
        let eid = graph.NextEdgeId
        let newEdges = Map.add eid (src, dst, data) graph.Edges

        let updateMap id mapping =
            let existing = Map.tryFind id mapping |> Option.defaultValue Set.empty
            Map.add id (Set.add eid existing) mapping

        let out1 = updateMap src graph.OutEdgeIds
        let in1 = updateMap dst graph.InEdgeIds

        let out2, in2 =
            match graph.Kind with
            | Directed -> out1, in1
            | Undirected -> updateMap dst out1, updateMap src in1

        { graph with
            Edges = newEdges
            OutEdgeIds = out2
            InEdgeIds = in2
            NextEdgeId = eid + 1 },
        eid

    /// Removes a single edge by its EdgeId.
    let removeEdge edgeId (graph: MultiGraph<'n, 'e>) =
        match Map.tryFind edgeId graph.Edges with
        | None -> graph
        | Some (src, dst, _) ->
            let newEdges = Map.remove edgeId graph.Edges

            let removeId mapping id =
                match Map.tryFind id mapping with
                | None -> mapping
                | Some s -> Map.add id (Set.remove edgeId s) mapping

            let out1 = removeId graph.OutEdgeIds src
            let in1 = removeId graph.InEdgeIds dst

            let out2, in2 =
                match graph.Kind with
                | Directed -> out1, in1
                | Undirected -> removeId out1 dst, removeId in1 src

            { graph with
                Edges = newEdges
                OutEdgeIds = out2
                InEdgeIds = in2 }

    /// Removes a node and all edges connected to it.
    let removeNode id (graph: MultiGraph<'n, 'e>) =
        let outIds = Map.tryFind id graph.OutEdgeIds |> Option.defaultValue Set.empty
        let inIds = Map.tryFind id graph.InEdgeIds |> Option.defaultValue Set.empty
        let idsToRemove = Set.union outIds inIds
        
        let mutable g = graph
        for eid in idsToRemove do
            g <- removeEdge eid g

        { g with
            Nodes = Map.remove id g.Nodes
            OutEdgeIds = Map.remove id g.OutEdgeIds
            InEdgeIds = Map.remove id g.InEdgeIds }

    /// Returns true if an edge with this ID exists in the graph.
    let hasEdge (graph: MultiGraph<'n, 'e>) edgeId = Map.containsKey edgeId graph.Edges

    /// Returns all edge IDs in the graph.
    let allEdgeIds (graph: MultiGraph<'n, 'e>) = graph.Edges |> Map.keys |> Seq.toList

    /// Returns the total number of edges.
    let size (graph: MultiGraph<'n, 'e>) = graph.Edges.Count

    /// Returns all parallel edges between from and to.
    let edgesBetween (graph: MultiGraph<'n, 'e>) from toNode =
        let edgeIds = Map.tryFind from graph.OutEdgeIds |> Option.defaultValue Set.empty
        let mutable acc = []
        for eid in edgeIds do
            match Map.tryFind eid graph.Edges with
            | Some (src, dst, data) when src = from && dst = toNode ->
                acc <- (eid, data) :: acc
            | Some (src, dst, data) when src = toNode && dst = from && graph.Kind = Undirected ->
                acc <- (eid, data) :: acc
            | _ -> ()
        List.rev acc

    /// Returns the number of parallel edges between two nodes.
    let edgeCount graph from toNode = (edgesBetween graph from toNode).Length

    /// Gets successors with their EdgeIds.
    let successors id (graph: MultiGraph<'n, 'e>) =
        let edgeIds = Map.tryFind id graph.OutEdgeIds |> Option.defaultValue Set.empty
        let mutable acc = []
        for eid in edgeIds do
            match Map.tryFind eid graph.Edges with
            | Some (src, dst, data) when src = id ->
                acc <- (dst, eid, data) :: acc
            | Some (src, dst, data) when dst = id && graph.Kind = Undirected ->
                acc <- (src, eid, data) :: acc
            | _ -> ()
        List.rev acc

    /// Gets predecessors with their EdgeIds.
    let predecessors id (graph: MultiGraph<'n, 'e>) =
        let edgeIds = Map.tryFind id graph.InEdgeIds |> Option.defaultValue Set.empty
        let mutable acc = []
        for eid in edgeIds do
            match Map.tryFind eid graph.Edges with
            | Some (src, dst, data) when dst = id ->
                acc <- (src, eid, data) :: acc
            | Some (src, dst, data) when src = id && graph.Kind = Undirected ->
                acc <- (dst, eid, data) :: acc
            | _ -> ()
        List.rev acc

    /// Internal: Count self loops for a node.
    let private countSelfLoops (graph: MultiGraph<'n, 'e>) id =
        let edgeIds = Map.tryFind id graph.OutEdgeIds |> Option.defaultValue Set.empty
        let mutable count = 0
        for eid in edgeIds do
            match Map.tryFind eid graph.Edges with
            | Some (src, dst, _) when src = id && dst = id ->
                count <- count + 1
            | _ -> ()
        count

    /// Returns the out-degree of a node.
    let outDegree id (graph: MultiGraph<'n, 'e>) =
        let baseCount = Map.tryFind id graph.OutEdgeIds |> Option.map Set.count |> Option.defaultValue 0
        match graph.Kind with
        | Undirected -> baseCount + countSelfLoops graph id
        | Directed -> baseCount

    /// Returns the in-degree of a node.
    let inDegree id (graph: MultiGraph<'n, 'e>) =
        let baseCount = Map.tryFind id graph.InEdgeIds |> Option.map Set.count |> Option.defaultValue 0
        match graph.Kind with
        | Undirected -> baseCount + countSelfLoops graph id
        | Directed -> baseCount

    /// Returns the total degree of a node.
    let degree id (graph: MultiGraph<'n, 'e>) =
        match graph.Kind with
        | Undirected -> outDegree id graph
        | Directed -> inDegree id graph + outDegree id graph

    /// Collapses the multigraph into a simple Graph by combining parallel edges.
    let toSimpleGraph (combineFn: 'e -> 'e -> 'e) (graph: MultiGraph<'n, 'e>) : Graph<'n, 'e> =
        let mutable (simple: Graph<'n, 'e>) = Yog.Model.empty graph.Kind

        for kvp in graph.Nodes do
            simple <- Yog.Model.addNode kvp.Key kvp.Value simple

        for kvp in graph.Edges do
            let (src, dst, data) = kvp.Value
            simple <- Yog.Model.addEdgeWithCombine src dst data combineFn simple

        simple

    /// Converts the multigraph to a simple graph. Keep first/lowest EdgeId edge.
    let toSimpleGraphDefault (graph: MultiGraph<'n, 'e>) : Graph<'n, 'e> =
        let mutable (simple: Graph<'n, 'e>) = Yog.Model.empty graph.Kind
        for kvp in graph.Nodes do
            simple <- Yog.Model.addNode kvp.Key kvp.Value simple

        let mutable seen = Set.empty
        for kvp in graph.Edges do
            let (src, dst, data) = kvp.Value
            let key = if graph.Kind = Undirected && src > dst then (dst, src) else (src, dst)
            if not (Set.contains key seen) then
                simple <- Yog.Model.addEdge src dst data simple
                seen <- Set.add key seen
        simple

    /// Collapses the multigraph by keeping the minimum weight.
    let toSimpleGraphMinEdges (compareFn: 'e -> 'e -> int) (graph: MultiGraph<'n, 'e>) : Graph<'n, 'e> =
        let minFn (a: 'e) (b: 'e) = if compareFn a b > 0 then b else a
        toSimpleGraph minFn graph

    /// Collapses the multigraph by keeping the maximum weight.
    let toSimpleGraphMaxEdges (compareFn: 'e -> 'e -> int) (graph: MultiGraph<'n, 'e>) : Graph<'n, 'e> =
        let maxFn (a: 'e) (b: 'e) = if compareFn a b < 0 then b else a
        toSimpleGraph maxFn graph

    /// Collapses the multigraph by summing weights.
    let toSimpleGraphSumEdges (addFn: 'e -> 'e -> 'e) (graph: MultiGraph<'n, 'e>) : Graph<'n, 'e> =
        toSimpleGraph addFn graph

/// Traversal operations for multigraphs.
module Traversal =
    open Model

    /// Breadth-first search traversal.
    let bfs (source: NodeId) (graph: MultiGraph<'n, 'e>) =
        let queue = System.Collections.Generic.Queue<NodeId>()
        queue.Enqueue(source)
        let mutable visitedNodes = Set.singleton source
        let mutable visitedEdges = Set.empty
        let mutable result = []
        
        while queue.Count > 0 do
            let current = queue.Dequeue()
            result <- current :: result
            let succs = Model.successors current graph
            for (neighbor, edgeId, _) in succs do
                if not (Set.contains edgeId visitedEdges) then
                    visitedEdges <- Set.add edgeId visitedEdges
                    if not (Set.contains neighbor visitedNodes) then
                        visitedNodes <- Set.add neighbor visitedNodes
                        queue.Enqueue(neighbor)
        List.rev result

    /// Depth-first search traversal.
    let dfs (source: NodeId) (graph: MultiGraph<'n, 'e>) : NodeId list =
        let rec doDfsWithNodes current visitedNodes visitedEdges result =
            if Set.contains current visitedNodes then
                visitedNodes, visitedEdges, result
            else
                let newVn = Set.add current visitedNodes
                let newResult = current :: result
                let succs = Model.successors current graph
                doDfsSuccessors succs newVn visitedEdges newResult

        and doDfsSuccessors list visitedNodes visitedEdges result =
            match list with
            | [] -> visitedNodes, visitedEdges, result
            | (neighbor, edgeId, _) :: rest ->
                if Set.contains edgeId visitedEdges then
                    doDfsSuccessors rest visitedNodes visitedEdges result
                else
                    let newVe = Set.add edgeId visitedEdges
                    let vn2, ve2, r2 = doDfsWithNodes neighbor visitedNodes newVe result
                    doDfsSuccessors rest vn2 ve2 r2

        let _, _, result = doDfsWithNodes source Set.empty Set.empty []
        List.rev result

    /// Folds over nodes during multigraph traversal, accumulating state with metadata.
    let foldWalk (source: NodeId) (initial: 'acc) (folder: 'acc -> NodeId -> WalkMetadata -> WalkControl * 'acc) (graph: MultiGraph<'n, 'e>) : 'acc =
        let queue = System.Collections.Generic.Queue<NodeId * NodeId option * EdgeId option>()
        queue.Enqueue((source, None, None))
        
        let mutable depths = Map.empty |> Map.add source 0
        let mutable visitedEdges = Set.empty
        let mutable acc = initial
        let mutable halted = false
        
        while queue.Count > 0 && not halted do
            let (current, parent, edgeId) = queue.Dequeue()
            let depth = Map.find current depths
            let meta = { Depth = depth; Parent = Option.map2 (fun p e -> (p, e)) parent edgeId }
            
            let control, newAcc = folder acc current meta
            acc <- newAcc
            match control with
            | Halt -> halted <- true
            | Stop -> ()
            | Continue ->
                let succs = Model.successors current graph
                for (neighbor, succEdgeId, _) in succs do
                    if not (Set.contains succEdgeId visitedEdges) then
                        visitedEdges <- Set.add succEdgeId visitedEdges
                        if not (Map.containsKey neighbor depths) then
                            depths <- Map.add neighbor (depth + 1) depths
                        queue.Enqueue((neighbor, Some current, Some succEdgeId))
        acc

/// Eulerian circuit algorithms for multigraphs.
module Eulerian =
    open Model

    let private bfsVisited (graph: MultiGraph<'n, 'e>) (source: NodeId) =
        let rec loop queue visited =
            match queue with
            | [] -> visited
            | current :: rest ->
                let succs = Model.successors current graph |> List.map (fun (n, _, _) -> n)
                let preds = Model.predecessors current graph |> List.map (fun (n, _, _) -> n)
                let neighbors = succs @ preds |> List.distinct
                let newNeighbors = neighbors |> List.filter (fun n -> not (Set.contains n visited))
                let newVisited = List.fold (fun acc n -> Set.add n acc) visited newNeighbors
                loop (rest @ newNeighbors) newVisited
        loop [source] (Set.singleton source)

    let private isConnected (graph: MultiGraph<'n, 'e>) =
        let nonIsolated =
            graph.Nodes.Keys
            |> Seq.filter (fun n -> Model.degree n graph > 0)
            |> Seq.toList
        match nonIsolated with
        | [] -> true
        | source :: _ ->
            let visited = bfsVisited graph source
            nonIsolated |> List.forall (fun n -> Set.contains n visited)

    /// Returns true if the multigraph has an Eulerian circuit.
    let hasEulerianCircuit (graph: MultiGraph<'n, 'e>) =
        if graph.Nodes.Count = 0 || Model.size graph = 0 then
            false
        else
            match graph.Kind with
            | Undirected ->
                let allEven = graph.Nodes.Keys |> Seq.forall (fun n -> Model.degree n graph % 2 = 0)
                allEven && isConnected graph
            | Directed ->
                let allBalanced = graph.Nodes.Keys |> Seq.forall (fun n -> Model.inDegree n graph = Model.outDegree n graph)
                allBalanced && isConnected graph

    let private countOddDegreeNodes (graph: MultiGraph<'n, 'e>) =
        graph.Nodes.Keys
        |> Seq.filter (fun n -> (Model.outDegree n graph) % 2 = 1)
        |> Seq.length

    /// Returns true if the multigraph has an Eulerian path.
    let hasEulerianPath (graph: MultiGraph<'n, 'e>) =
        if graph.Nodes.Count = 0 || Model.size graph = 0 then
            false
        else
            match graph.Kind with
            | Undirected ->
                let oddCount = countOddDegreeNodes graph
                (oddCount = 0 || oddCount = 2) && isConnected graph
            | Directed ->
                let mutable starts = 0
                let mutable ends = 0
                let mutable balanced = true
                for n in graph.Nodes.Keys do
                    let diff = Model.outDegree n graph - Model.inDegree n graph
                    if diff = 1 then starts <- starts + 1
                    elif diff = -1 then ends <- ends + 1
                    elif diff = 0 then ()
                    else balanced <- false
                balanced && ((starts = 0 && ends = 0) || (starts = 1 && ends = 1)) && isConnected graph

    let private pickEdge (graph: MultiGraph<'n, 'e>) (current: NodeId) (available: Set<EdgeId>) =
        Model.successors current graph
        |> List.tryFind (fun (_, eid, _) -> Set.contains eid available)
        |> Option.map (fun (next, eid, _) -> next, eid)

    let private firstNonIsolated (graph: MultiGraph<'n, 'e>) =
        graph.Nodes.Keys
        |> Seq.tryFind (fun n -> Model.degree n graph > 0)

    let private findPathStart (graph: MultiGraph<'n, 'e>) =
        match graph.Kind with
        | Undirected ->
            graph.Nodes.Keys
            |> Seq.tryFind (fun n -> Model.outDegree n graph % 2 = 1)
            |> Option.orElseWith (fun () -> firstNonIsolated graph)
        | Directed ->
            graph.Nodes.Keys
            |> Seq.tryFind (fun n -> Model.outDegree n graph = Model.inDegree n graph + 1)
            |> Option.orElseWith (fun () -> firstNonIsolated graph)

    let private runHierholzer (graph: MultiGraph<'n, 'e>) (start: NodeId) =
        let allIds = Model.allEdgeIds graph |> Set.ofList
        let rec doHierholzer current available path =
            match pickEdge graph current available with
            | None -> available, path
            | Some (nextNode, eid) ->
                let av2, p2 = doHierholzer nextNode (Set.remove eid available) path
                doHierholzer current av2 (eid :: p2)
        
        let _, path = doHierholzer start allIds []
        match path with
        | [] -> None
        | _ -> Some (List.rev path)

    /// Finds an Eulerian circuit if one exists.
    let findEulerianCircuit (graph: MultiGraph<'n, 'e>) : EdgeId list option =
        if hasEulerianCircuit graph then
            match firstNonIsolated graph with
            | None -> None
            | Some start -> runHierholzer graph start
        else
            None

    /// Finds an Eulerian path if one exists.
    let findEulerianPath (graph: MultiGraph<'n, 'e>) : EdgeId list option =
        if hasEulerianPath graph then
            match findPathStart graph with
            | None -> None
            | Some start -> runHierholzer graph start
        else
            None

