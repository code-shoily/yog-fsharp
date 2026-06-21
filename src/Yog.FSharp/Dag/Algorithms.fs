namespace Yog.Dag

open System.Collections.Generic
open Yog.Model
open Yog.Dag.Model

/// Direction for reachability analysis.
type Direction =
    /// Count nodes that can reach the given node (ancestors).
    | Ancestors
    /// Count nodes reachable from the given node (descendants).
    | Descendants

/// DAG-specific algorithms that leverage acyclicity guarantees.
///
/// These algorithms are optimized for DAGs and would be incorrect or inefficient
/// on general graphs with cycles. The DAG property enables linear-time solutions
/// for problems that are NP-hard on general graphs.
module Algorithms =

    /// Topological sort guaranteed to succeed for the Dag type.
    ///
    /// ## Complexity
    /// - **Time**: O(V + E)
    /// - **Space**: O(V)
    ///
    /// ## Returns
    /// List of node IDs in topological order (sources before targets).
    let topologicalSort (dag: Dag<'n, 'e>) =
        match Yog.Traversal.topologicalSort (toGraph dag) with
        | Ok sorted -> sorted
        | Error _ -> failwith "Logic error: Dag contained a cycle"

    /// Finds the Longest Path (Critical Path) in O(V+E).
    ///
    /// In a DAG, the longest path problem is well-defined and solvable in linear time.
    /// This is useful for critical path analysis in project scheduling.
    ///
    /// ## Type Constraint
    /// Edge weights must be integers.
    ///
    /// ## Complexity
    /// - **Time**: O(V + E)
    /// - **Space**: O(V)
    ///
    /// ## Returns
    /// List of node IDs forming the longest path from any source to any sink.
    let longestPath (dag: Dag<'n, int>) =
        let graph = toGraph dag
        let sorted = topologicalSort dag

        let mutable distances = Map.empty<NodeId, int>
        let mutable predecessors = Map.empty<NodeId, NodeId>

        for u in sorted do
            let distU = Map.tryFind u distances |> Option.defaultValue 0

            for (v, weight) in Yog.Model.successors u graph do
                let newDist = distU + weight

                let currentV = Map.tryFind v distances |> Option.defaultValue System.Int32.MinValue

                if newDist > currentV then
                    distances <- Map.add v newDist distances
                    predecessors <- Map.add v u predecessors

        distances
        |> Map.toSeq
        |> Seq.sortByDescending snd
        |> Seq.tryHead
        |> function
            | None -> []
            | Some(endNode, _) ->
                let rec reconstruct curr acc =
                    match Map.tryFind curr predecessors with
                    | Some prev -> reconstruct prev (curr :: acc)
                    | None -> curr :: acc

                reconstruct endNode []

    /// Transitive Closure: Reachability map with merged weights.
    ///
    /// Computes a new DAG where there's a direct edge from u to v if v is reachable
    /// from u in the original DAG. Edge weights are merged using the provided function.
    ///
    /// ## Parameters
    /// - `mergeFn`: Function to combine weights when multiple paths exist
    /// - `dag`: The input DAG
    ///
    /// ## Complexity
    /// - **Time**: O(V² + VE) - processes each edge and potential path
    let transitiveClosure (mergeFn: 'e -> 'e -> 'e) (dag: Dag<'n, 'e>) =
        let graph = toGraph dag
        let sorted = topologicalSort dag |> List.rev
        let mutable reachability = Map.empty<NodeId, Map<NodeId, 'e>>

        for u in sorted do
            let mutable uReach = Map.empty

            for (v, weight) in Yog.Model.successors u graph do
                uReach <- Map.add v weight uReach

                match Map.tryFind v reachability with
                | Some vReach ->
                    for KeyValue(target, vWeight) in vReach do
                        let combined = mergeFn weight vWeight

                        let final =
                            match Map.tryFind target uReach with
                            | Some e -> mergeFn e combined
                            | None -> combined

                        uReach <- Map.add target final uReach
                | None -> ()

            reachability <- Map.add u uReach reachability

        let mutable newGraph = graph

        // Add reachability edges to graph
        for KeyValue(u, targets) in reachability do
            for KeyValue(v, weight) in targets do
                newGraph <- Yog.Model.addEdgeEnsured u v weight Unchecked.defaultof<'n> Unchecked.defaultof<'n> newGraph

        match Model.fromGraph newGraph with
        | Ok tcDag -> tcDag
        | Error _ -> failwith "Transitive closure of DAG should be a DAG"

    /// Counts how many nodes are reachable from or can reach each node in the DAG.
    ///
    /// ## Parameters
    /// - `direction`: Ancestors (prerequisites) or Descendants (dependents)
    /// - `dag`: The DAG to analyze
    ///
    /// ## Complexity
    /// - **Time**: O(V + E) - dynamic programming on topological sort
    /// - **Space**: O(V²) - set storage per node in the worst case
    ///
    /// ## Returns
    /// Map from each node to the count of its ancestors or descendants (excluding itself).
    let countReachability (direction: Direction) (dag: Dag<'n, 'e>) =
        let graph = toGraph dag

        let nodes =
            match direction with
            | Descendants -> topologicalSort dag |> List.rev
            | Ancestors -> topologicalSort dag

        let mutable reachSets = Map.empty<NodeId, Set<NodeId>>

        for u in nodes do
            let related =
                match direction with
                | Descendants -> Yog.Model.successors u graph |> List.map fst
                | Ancestors -> Yog.Model.predecessors u graph |> List.map fst

            let mutable currentSet = Set.ofList related

            for r in related do
                match Map.tryFind r reachSets with
                | Some s -> currentSet <- Set.union currentSet s
                | None -> ()

            reachSets <- Map.add u currentSet reachSets

        reachSets |> Map.map (fun _ s -> s.Count)

    /// Transitive Reduction: Smallest DAG preserving the same reachability.
    ///
    /// Removes redundant edges that are implied by longer paths.
    /// For example, if A->B, B->C, and A->C exist, the edge A->C is redundant
    /// and will be removed.
    ///
    /// ## Parameters
    /// - `mergeFn`: Function to combine weights
    /// - `dag`: The input DAG
    ///
    /// ## Complexity
    /// - **Time**: O(V × E) with topological sorting checks
    /// - **Space**: O(V + E)
    ///
    /// ## Returns
    /// Reduced DAG with all redundant edges removed.
    let transitiveReduction (mergeFn: 'e -> 'e -> 'e) (dag: Dag<'n, 'e>) =
        let graph = toGraph dag
        let reachDag = transitiveClosure mergeFn dag
        let reachGraph = toGraph reachDag

        let mutable reducedGraph = graph

        for KeyValue(u, targets) in graph.OutEdges do
            for KeyValue(v, _) in targets do
                // Is there an indirect path from u to v?
                // Check if u has an edge to some w, and w reaches v in the closure
                let isRedundant =
                    targets
                    |> Map.exists (fun w _ ->
                        w <> v
                        && match Map.tryFind w reachGraph.OutEdges with
                           | Some wTargets -> Map.containsKey v wTargets
                           | None -> false)

                if isRedundant then
                    reducedGraph <- Yog.Model.removeEdge u v reducedGraph

        match Model.fromGraph reducedGraph with
        | Ok newDag -> newDag
        | Error _ -> failwith "Reduction should preserve acyclicity"

    /// Finds the shortest path between two specific nodes in a weighted DAG.
    ///
    /// Uses dynamic programming on the topologically sorted DAG to find the minimum
    /// weight path from `src` to `dst`. Unlike Dijkstra's algorithm which works on
    /// general graphs in O((V+E) log V), this leverages the DAG property for linear
    /// time complexity.
    ///
    /// ## Type Constraint
    /// Edge weights must be integers.
    ///
    /// ## Complexity
    /// - **Time**: O(V + E)
    /// - **Space**: O(V)
    ///
    /// ## Returns
    /// - `Some path`: List of node IDs from src to dst with minimum total weight
    /// - `None`: If no path exists from src to dst
    let shortestPath (src: NodeId) (dst: NodeId) (dag: Dag<'n, int>) =
        let graph = toGraph dag
        let sorted = topologicalSort dag

        let mutable distances = Map.add src 0 Map.empty<NodeId, int>
        let mutable predecessors = Map.empty<NodeId, NodeId>

        for u in sorted do
            match Map.tryFind u distances with
            | Some distU ->
                for (v, weight) in Yog.Model.successors u graph do
                    let newDist = distU + weight

                    let currentV = Map.tryFind v distances |> Option.defaultValue System.Int32.MaxValue

                    if newDist < currentV then
                        distances <- Map.add v newDist distances
                        predecessors <- Map.add v u predecessors
            | None -> ()

        match Map.tryFind dst distances with
        | None -> None
        | Some _ ->
            let rec reconstruct curr acc =
                match Map.tryFind curr predecessors with
                | Some prev -> reconstruct prev (curr :: acc)
                | None -> curr :: acc

            Some(reconstruct dst [])

    /// Checks if a path exists from start to target in the DAG.
    ///
    /// Performs a simple DFS traversal. Since the graph is a DAG, no cycle
    /// detection is needed.
    ///
    /// **Time Complexity:** O(V + E) in the worst case
    let private hasPath (dag: Dag<'n, 'e>) (start: NodeId) (target: NodeId) : bool =
        let graph = toGraph dag

        let rec dfs current visited =
            if current = target then true
            elif Set.contains current visited then false
            else
                let neighbors = Yog.Model.successors current graph |> List.map fst
                let newVisited = Set.add current visited
                neighbors |> List.exists (fun n -> dfs n newVisited)

        dfs start Set.empty

    /// Finds the Lowest Common Ancestors (LCAs) of two nodes.
    ///
    /// An LCA of nodes A and B is a common ancestor X such that no descendant
    /// of X is also a common ancestor of A and B. In a DAG, there can be multiple
    /// lowest common ancestors.
    ///
    /// ## Complexity
    /// - **Time**: O(V × (V + E)) - checks paths from all nodes
    /// - **Space**: O(V)
    ///
    /// ## Returns
    /// List of node IDs that are lowest common ancestors of A and B.
    ///
    /// ## Example
    ///
    ///     // Given: X->A, X->B, Y->A, Z->B
    ///     // LCAs of A and B are [X] - the most specific shared ancestor
    ///     let lcas = Algorithms.lowestCommonAncestors nodeA nodeB dag
    ///
    /// ## Use Cases
    /// - Finding merge bases in version control
    /// - Identifying shared dependencies
    /// - Computing dominators in control flow graphs
    let lowestCommonAncestors (nodeA: NodeId) (nodeB: NodeId) (dag: Dag<'n, 'e>) =
        let graph = toGraph dag

        let getAncestorsSet node =
            allNodes graph |> List.filter (fun n -> hasPath dag n node)

        let ancestorsA = getAncestorsSet nodeA
        let ancestorsB = getAncestorsSet nodeB

        let commonAncestors =
            ancestorsA |> List.filter (fun a -> List.contains a ancestorsB)

        // Filter to find only lowest (not ancestors of other common ancestors)
        commonAncestors
        |> List.filter (fun candidate ->
            not (List.exists (fun other -> other <> candidate && hasPath dag candidate other) commonAncestors))

    /// Returns the topological generations of a DAG.
    let topologicalGenerations (dag: Dag<'n, 'e>) : NodeId list list =
        let graph = toGraph dag
        let nodes = allNodes graph
        
        let mutable inDegrees = Map.empty
        let mutable currentGen = []
        
        for node in nodes do
            let preds = Yog.Model.predecessorIds node graph
            let deg = preds.Length
            inDegrees <- Map.add node deg inDegrees
            if deg = 0 then
                currentGen <- node :: currentGen
                
        let mutable result = []
        
        while not (List.isEmpty currentGen) do
            result <- (currentGen |> List.sort) :: result
            let mutable nextGen = []
            
            for node in currentGen do
                let succs = Yog.Model.successorIds node graph
                for succ in succs do
                    let currentDeg = Map.find succ inDegrees
                    let newDeg = currentDeg - 1
                    inDegrees <- Map.add succ newDeg inDegrees
                    if newDeg = 0 then
                        nextGen <- succ :: nextGen
                        
            currentGen <- nextGen
            
        List.rev result

    /// Returns all source nodes (in-degree 0).
    let sources (dag: Dag<'n, 'e>) : NodeId list =
        let graph = toGraph dag
        allNodes graph
        |> List.filter (fun node -> (Yog.Model.predecessors node graph).Length = 0)
        |> List.sort

    /// Returns all sink nodes (out-degree 0).
    let sinks (dag: Dag<'n, 'e>) : NodeId list =
        let graph = toGraph dag
        allNodes graph
        |> List.filter (fun node -> (Yog.Model.successors node graph).Length = 0)
        |> List.sort

    /// Returns all ancestors of a node (nodes that have a path to the given node, including itself).
    let ancestors (node: NodeId) (dag: Dag<'n, 'e>) : NodeId list =
        let graph = toGraph dag
        let visited = HashSet<NodeId>()
        let q = Queue<NodeId>()
        q.Enqueue(node)
        visited.Add(node) |> ignore
        
        while q.Count > 0 do
            let curr = q.Dequeue()
            for pred in Yog.Model.predecessorIds curr graph do
                if visited.Add(pred) then
                    q.Enqueue(pred)
                    
        visited |> Seq.toList |> List.sort

    /// Returns all descendants of a node (nodes reachable from the given node, including itself).
    let descendants (node: NodeId) (dag: Dag<'n, 'e>) : NodeId list =
        let graph = toGraph dag
        let visited = HashSet<NodeId>()
        let q = Queue<NodeId>()
        q.Enqueue(node)
        visited.Add(node) |> ignore
        
        while q.Count > 0 do
            let curr = q.Dequeue()
            for succ in Yog.Model.successorIds curr graph do
                if visited.Add(succ) then
                    q.Enqueue(succ)
                    
        visited |> Seq.toList |> List.sort

    /// Computes single-source shortest distances to all reachable nodes.
    let singleSourceDistances (from: NodeId) (dag: Dag<'n, int>) : Map<NodeId, int> =
        let graph = toGraph dag
        let sorted = topologicalSort dag
        let relevant = sorted |> List.skipWhile (fun n -> n <> from)
        
        if List.isEmpty relevant then
            Map.empty
        else
            let mutable distances = Map.empty |> Map.add from 0
            for u in relevant do
                match Map.tryFind u distances with
                | Some distU ->
                    for (v, weight) in Yog.Model.successors u graph do
                        let newDist = distU + weight
                        let currentV = Map.tryFind v distances |> Option.defaultValue System.Int32.MaxValue
                        if newDist < currentV then
                            distances <- Map.add v newDist distances
                | None -> ()
            distances

    /// Finds the longest path between two specific nodes in a weighted DAG.
    let longestPathBetween (src: NodeId) (dst: NodeId) (dag: Dag<'n, int>) : NodeId list option =
        let graph = toGraph dag
        let sorted = topologicalSort dag
        let relevant = sorted |> List.skipWhile (fun n -> n <> src)

        if List.isEmpty relevant then
            None
        else
            let mutable distances = Map.empty |> Map.add src 0
            let mutable predecessors = Map.empty

            for u in relevant do
                match Map.tryFind u distances with
                | Some distU ->
                    for (v, weight) in Yog.Model.successors u graph do
                        let newDist = distU + weight
                        let currentV = Map.tryFind v distances |> Option.defaultValue System.Int32.MinValue
                        if newDist > currentV then
                            distances <- Map.add v newDist distances
                            predecessors <- Map.add v u predecessors
                | None -> ()

            match Map.tryFind dst distances with
            | None -> None
            | Some _ ->
                let rec reconstruct curr acc =
                    match Map.tryFind curr predecessors with
                    | Some prev -> reconstruct prev (curr :: acc)
                    | None -> curr :: acc
                Some(reconstruct dst [])

    /// Counts the number of distinct paths between two nodes in a DAG.
    let pathCount (src: NodeId) (dst: NodeId) (dag: Dag<'n, 'e>) : int =
        let graph = toGraph dag
        let sorted = topologicalSort dag
        let relevant = sorted |> List.skipWhile (fun n -> n <> src)
        
        if List.isEmpty relevant then
            0
        else
            let mutable counts = Map.empty |> Map.add src 1
            for u in relevant do
                match Map.tryFind u counts with
                | Some count ->
                    let succs = Yog.Model.successorIds u graph
                    for succ in succs do
                        let currentCount = Map.tryFind succ counts |> Option.defaultValue 0
                        counts <- Map.add succ (currentCount + count) counts
                | None -> ()
            Map.tryFind dst counts |> Option.defaultValue 0
