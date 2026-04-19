namespace Yog.Dag

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
/// 
/// ## Algorithms Provided
/// 
/// | Algorithm | Complexity | Use Case |
/// |-----------|------------|----------|
/// | `topologicalSort` | O(V+E) | Task scheduling, dependency ordering |
/// | `longestPath` | O(V+E) | Critical path analysis (PERT/CPM) |
/// | `shortestPath` | O(V+E) | Minimum cost path between nodes |
/// | `transitiveClosure` | O(V²) | Reachability queries, indirect dependencies |
/// | `transitiveReduction` | O(V×E) | Simplify graphs, remove implied edges |
/// | `countReachability` | O(V+E) | Impact analysis, prerequisite counting |
/// | `lowestCommonAncestors` | O(V×(V+E)) | Merge bases, shared dependencies |
/// 
/// ## Why DAGs Are Special
/// 
/// DAGs enable efficient algorithms for problems that are intractable on general graphs:
/// - **Longest Path**: O(V+E) on DAGs vs NP-hard on general graphs
/// - **Topological Sort**: Always succeeds on DAGs vs may fail with cycles
/// - **Shortest Path**: O(V+E) on DAGs vs O((V+E) log V) with Dijkstra
/// 
/// ## References
/// 
/// - [Wikipedia: Directed Acyclic Graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph)
/// - [Longest Path Problem](https://en.wikipedia.org/wiki/Longest_path_problem)
/// - [Transitive Closure](https://en.wikipedia.org/wiki/Transitive_closure)
/// - [Transitive Reduction](https://en.wikipedia.org/wiki/Transitive_reduction)
module Algorithms =

    /// Topological sort guaranteed to succeed for the Dag type.
    /// 
    /// ## Complexity
    /// - **Time**: O(V + E)
    /// - **Space**: O(V)
    /// 
    /// ## Returns
    /// List of node IDs in topological order (sources before targets).
    /// 
    /// ## Note
    /// This function cannot fail because the Dag type guarantees acyclicity.
    /// If it somehow encounters a cycle, it raises an exception (indicating a bug).
    /// 
    /// ## Example
    /// 
    ///     let order = Algorithms.topologicalSort dag
    ///     // Process nodes in dependency order
    ///     for node in order do
    ///         executeTask node
    /// 
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
    /// 
    /// ## Example
    /// 
    ///     // Task durations as edge weights
    ///     let criticalPath = Algorithms.longestPath dag
    ///     printfn "Project duration: %d"
    ///         (criticalPath.Length - 1)
    /// 
    /// ## Use Cases
    /// - Project management (PERT/CPM)
    /// - Scheduling with dependencies
    /// - Finding slowest execution path
    let longestPath (dag: Dag<'n, int>) =
        let graph = toGraph dag
        let sorted = topologicalSort dag

        let mutable distances = Map.empty<NodeId, int>
        let mutable predecessors = Map.empty<NodeId, NodeId>

        for u in sorted do
            let distU = Map.tryFind u distances |> Option.defaultValue 0

            for (v, weight) in successors u graph do
                let newDist = distU + weight

                let currentV =
                    Map.tryFind v distances
                    |> Option.defaultValue System.Int32.MinValue

                if newDist > currentV then
                    distances <- Map.add v newDist distances
                    predecessors <- Map.add v u predecessors

        distances
        |> Map.toSeq
        |> Seq.sortByDescending snd
        |> Seq.tryHead
        |> function
            | None -> []
            | Some (endNode, _) ->
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
    /// - **Space**: O(V²) - stores reachability for all pairs
    /// 
    /// ## Returns
    /// A new DAG representing the transitive closure.
    /// 
    /// ## Example
    /// 
    ///     // Merge by taking minimum weight along any path
    ///     let closure = Algorithms.transitiveClosure min dag
    /// 
    /// ## Use Cases
    /// - Dependency analysis (what indirectly depends on what)
    /// - Reachability queries
    /// - Partial order completion
    let transitiveClosure (mergeFn: 'e -> 'e -> 'e) (dag: Dag<'n, 'e>) =
        let graph = toGraph dag
        let sorted = topologicalSort dag |> List.rev
        let mutable reachability = Map.empty<NodeId, Map<NodeId, 'e>>

        for u in sorted do
            let mutable uReach = Map.empty

            for (v, weight) in successors u graph do
                uReach <- Map.add v weight uReach

                match Map.tryFind v reachability with
                | Some vReach ->
                    for KeyValue (target, vWeight) in vReach do
                        let combined = mergeFn weight vWeight

                        let final =
                            match Map.tryFind target uReach with
                            | Some e -> mergeFn e combined
                            | None -> combined

                        uReach <- Map.add target final uReach
                | None -> ()

            reachability <- Map.add u uReach reachability

        let mutable newGraph = graph

        for KeyValue (u, targets) in reachability do
            for KeyValue (v, weight) in targets do
                newGraph <- Yog.Model.addEdge u v weight newGraph

        match Model.fromGraph newGraph with
        | Ok d -> d
        | Error _ -> failwith "Closure error"

    /// Count total descendants or ancestors using set-based DP.
    /// 
    /// ## Parameters
    /// - `direction`: Count Ancestors (can reach node) or Descendants (reachable from node)
    /// - `dag`: The input DAG
    /// 
    /// ## Complexity
    /// - **Time**: O(V + E)
    /// - **Space**: O(V²) in worst case (storing all reachability sets)
    /// 
    /// ## Returns
    /// Map from node ID to count of reachable nodes in the specified direction.
    /// 
    /// ## Example
    /// 
    ///     // Count how many tasks depend on each task (descendants)
    ///     let impact = Algorithms.countReachability Descendants dag
    ///     
    ///     // Count prerequisites for each task (ancestors)
    ///     let prerequisites = Algorithms.countReachability Ancestors dag
    /// 
    /// ## Use Cases
    /// - Impact analysis (how many things break if X fails)
    /// - Work prioritization (tasks with many prerequisites first)
    /// - Dependency metrics
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
                | Descendants -> successors u graph |> List.map fst
                | Ancestors -> predecessors u graph |> List.map fst

            let mutable currentSet = Set.ofList related

            for r in related do
                match Map.tryFind r reachSets with
                | Some s -> currentSet <- Set.union currentSet s
                | None -> ()

            reachSets <- Map.add u currentSet reachSets

        reachSets |> Map.map (fun _ s -> Set.count s)

    /// Computes the transitive reduction of a DAG.
    /// 
    /// The transitive reduction removes all edges that are redundant - i.e., edges
    /// u -> v where there exists an indirect path from u to v through other nodes.
    /// The result has the minimum number of edges while preserving all reachability
    /// relationships.
    /// 
    /// This is the inverse of transitive closure.
    /// 
    /// ## Parameters
    /// - `mergeFn`: Function to combine weights when multiple paths exist
    /// - `dag`: The input DAG
    /// 
    /// ## Complexity
    /// - **Time**: O(V×E)
    /// - **Space**: O(V²)
    /// 
    /// ## Returns
    /// A new DAG with redundant edges removed.
    /// 
    /// ## Example
    /// 
    ///     // Original: A->B, B->C, A->C (A->C is implied by A->B->C)
    ///     // Reduction removes: A->C
    ///     // Result: A->B, B->C
    ///     let minimal = Algorithms.transitiveReduction min dag
    /// 
    /// ## Use Cases
    /// - Simplifying dependency graphs
    /// - Removing implied dependencies
    /// - Creating minimal representations
    let transitiveReduction (mergeFn: 'e -> 'e -> 'e) (dag: Dag<'n, 'e>) =
        let graph = toGraph dag
        let reachDag = transitiveClosure mergeFn dag
        let reachGraph = toGraph reachDag

        let mutable reducedGraph = graph

        for KeyValue (u, targets) in graph.OutEdges do
            for KeyValue (v, _) in targets do
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
    /// 
    /// ## Example
    /// 
    ///     match Algorithms.shortestPath dag 0 5 with
    ///     | Some path ->
    ///         printfn "Shortest path: %A" path
    ///     | None ->
    ///         printfn "No path exists"
    /// 
    /// ## Use Cases
    /// - Finding fastest route in task dependencies
    /// - Minimum cost path in project networks
    /// - Critical path alternatives
    let shortestPath (src: NodeId) (dst: NodeId) (dag: Dag<'n, int>) =
        let graph = toGraph dag
        let sorted = topologicalSort dag

        let mutable distances = Map.add src 0 Map.empty<NodeId, int>
        let mutable predecessors = Map.empty<NodeId, NodeId>

        for u in sorted do
            match Map.tryFind u distances with
            | Some distU ->
                for (v, weight) in successors u graph do
                    let newDist = distU + weight

                    let currentV =
                        Map.tryFind v distances
                        |> Option.defaultValue System.Int32.MaxValue

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
            if current = target then
                true
            elif Set.contains current visited then
                false
            else
                let newVisited = Set.add current visited

                successors current graph
                |> List.map fst
                |> List.exists (fun child -> dfs child newVisited)

        dfs start Set.empty

    /// Finds the lowest common ancestors (LCAs) of two nodes.
    /// 
    /// A common ancestor of nodes A and B is any node that has paths to both A and B.
    /// The "lowest" common ancestors are those that are not ancestors of any other
    /// common ancestor - they are the "closest" shared dependencies.
    /// 
    /// ## Parameters
    /// - `nodeA`: First node
    /// - `nodeB`: Second node
    /// - `dag`: The input DAG
    /// 
    /// ## Complexity
    /// - **Time**: O(V×(V+E))
    /// - **Space**: O(V)
    /// 
    /// ## Returns
    /// List of node IDs representing the lowest common ancestors.
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
            allNodes graph
            |> List.filter (fun n -> hasPath dag n node)

        let ancestorsA = getAncestorsSet nodeA
        let ancestorsB = getAncestorsSet nodeB

        let commonAncestors =
            ancestorsA
            |> List.filter (fun a -> List.contains a ancestorsB)

        // Filter to find only lowest (not ancestors of other common ancestors)
        commonAncestors
        |> List.filter (fun candidate ->
            not (List.exists (fun other -> other <> candidate && hasPath dag candidate other) commonAncestors))
