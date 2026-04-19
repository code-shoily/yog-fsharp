/// Graph connectivity analysis - finding bridges, articulation points, and strongly connected components.
/// 
/// This module provides algorithms for analyzing the connectivity structure of graphs,
/// identifying critical components whose removal would disconnect the graph.
/// 
/// ## Algorithms
/// 
/// | Algorithm | Function | Use Case |
/// |-----------|----------|----------|
/// | [Tarjan's Bridge-Finding](https://en.wikipedia.org/wiki/Bridge_(graph_theory)) | `analyze` | Find bridges and articulation points |
/// | [Tarjan's SCC](https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm) | `stronglyConnectedComponents` | Find SCCs in one pass |
/// | [Kosaraju's Algorithm](https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm) | `kosaraju` | Find SCCs using two DFS passes |
/// 
/// ## Bridges vs Articulation Points
/// 
/// - **Bridge** (cut edge): An edge whose removal increases the number of connected components.
///   In a network, this represents a single point of failure.
/// - **Articulation Point** (cut vertex): A node whose removal increases the number of connected
///   components. These are critical nodes in the network.
/// 
/// ## Strongly Connected Components
/// 
/// A **strongly connected component** (SCC) is a maximal subgraph where every node is reachable
/// from every other node. SCCs form a DAG when collapsed, useful for:
/// - Identifying cycles in dependency graphs
/// - Finding groups of mutually reachable web pages
/// - Analyzing feedback loops in systems
/// 
/// All algorithms run in **O(V + E)** linear time.
/// 
/// ## References
/// 
/// - [Wikipedia: Strongly Connected Components](https://en.wikipedia.org/wiki/Strongly_connected_component)
/// - [Wikipedia: Biconnected Component](https://en.wikipedia.org/wiki/Biconnected_component)
/// - [CP-Algorithms: Finding Bridges](https://cp-algorithms.com/graph/bridge-searching.html)
module Yog.Connectivity

open System.Collections.Generic
open Yog.Model

/// Represents a bridge (critical edge) in an undirected graph.
/// Bridges are stored as ordered pairs where the first node ID is smaller.
type Bridge = NodeId * NodeId

/// Results from connectivity analysis containing bridges and articulation points.
type ConnectivityResults =
    { Bridges: Bridge list
      ArticulationPoints: NodeId list }

/// Analyzes an undirected graph to find all bridges and articulation points
/// using Tarjan's algorithm in a single DFS pass.
/// 
/// **Important:** This algorithm is designed for undirected graphs. For directed
/// graphs, use strongly connected components analysis instead.
/// 
/// **Bridges** are edges whose removal increases the number of connected components.
/// **Articulation points** (cut vertices) are nodes whose removal increases the number
/// of connected components.
/// 
/// **Bridge ordering:** Bridges are returned as `(lower_id, higher_id)` for consistency.
/// 
/// **Time Complexity:** O(V + E)
/// 
/// ## Example
/// 
///     open Yog.Model
///     
///     let graph =
///       empty Undirected
///       |> addNode 1 ""
///       |> addNode 2 ""
///       |> addNode 3 ""
///       |> addEdge 1 2 0
///       |> addEdge 2 3 0
///     
///     let results = analyze graph
///     // results.Bridges = [(1, 2); (2, 3)]
///     // results.ArticulationPoints = [2]
/// 
/// ## Use Cases
/// 
/// - Network reliability analysis (finding single points of failure)
/// - Road network planning (identifying critical roads)
/// - Social network analysis (finding key connectors)
/// - Biconnected component decomposition
let analyze (graph: Graph<'n, 'e>) : ConnectivityResults =
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
                // Don't go back to parent
                ()
            elif visited.Contains(toId) then
                // Back-edge
                low.[v] <- min low.[v] tin.[toId]
            else
                // Tree-edge
                dfs toId (Some v)
                low.[v] <- min low.[v] low.[toId]

                // Bridge detection
                if low.[toId] > tin.[v] then
                    let bridge =
                        if v < toId then
                            (v, toId)
                        else
                            (toId, v)

                    bridges <- bridge :: bridges

                // Articulation point detection (for non-root nodes)
                if parentOpt.IsSome && low.[toId] >= tin.[v] then
                    points.Add(v) |> ignore

                children <- children + 1

        // Articulation point detection (special case for root node)
        if parentOpt.IsNone && children > 1 then
            points.Add(v) |> ignore

    // Run DFS for all disconnected components
    for node in allNodes graph do
        if not (visited.Contains(node)) then
            dfs node None

    { Bridges = List.rev bridges
      ArticulationPoints = points |> Seq.toList }

/// Finds Strongly Connected Components (SCC) using Tarjan's Algorithm.
/// Returns a list of components, where each component is a list of NodeIds.
/// 
/// A strongly connected component is a maximal subgraph where every node is
/// reachable from every other node. In other words, there's a path between
/// any two nodes in the component.
/// 
/// **Time Complexity:** O(V + E)
/// 
/// ## Algorithm
/// 
/// Tarjan's algorithm uses a single DFS pass with a stack to track the current
/// path. It assigns each node an index (discovery order) and a low-link value
/// (the lowest index reachable from that node). When a node's low-link equals
/// its index, it's the root of an SCC.
/// 
/// ## Example
/// 
///     let graph =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addNode 3 "C"
///       |> addEdge 1 2 1
///       |> addEdge 2 3 1
///       |> addEdge 3 1 1  // Creates a cycle: 1->2->3->1
///     
///     let sccs = stronglyConnectedComponents graph
///     // => [[1; 2; 3]]  // All nodes form one SCC (cycle)
/// 
/// ## Use Cases
/// 
/// - Finding cycles in directed graphs
/// - Condensation graph construction (SCC DAG)
/// - 2-SAT problem solving
/// - Identifying "tightly coupled" modules in dependency graphs
/// 
/// ## Comparison with Kosaraju's Algorithm
/// 
/// - **Tarjan's:** Single DFS pass, no transposition needed, uses low-link values
/// - **Kosaraju's:** Two DFS passes, requires graph transposition, conceptually simpler
/// 
/// Both have the same O(V + E) time complexity. Tarjan's uses slightly less memory
/// since it doesn't need the transposed graph.
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

        // If v is a root node, pop the stack and generate an SCC
        if lowLinks.[v] = indices.[v] then
            let mutable comp = []
            let mutable finished = false

            while not finished do
                let w = stack.Pop()
                onStack.Remove(w) |> ignore
                comp <- w :: comp
                if w = v then finished <- true

            components <- comp :: components

    // Run for all nodes to catch disconnected components
    for node in allNodes graph do
        if not (indices.ContainsKey(node)) then
            strongConnect node

    // Return components (reversing isn't strictly necessary but matches standard fold order)
    List.rev components

/// Finds Strongly Connected Components (SCC) using Kosaraju's Algorithm.
/// 
/// Returns a list of components, where each component is a list of NodeIds.
/// Kosaraju's algorithm uses two DFS passes and graph transposition:
/// 
/// 1. First DFS: Compute finishing times (nodes added to stack when DFS completes)
/// 2. Transpose the graph (reverse all edges) - O(1) operation!
/// 3. Second DFS: Process nodes in reverse finishing time order on transposed graph
/// 
/// **Time Complexity:** O(V + E) where V is vertices and E is edges
/// **Space Complexity:** O(V)
/// 
/// ## Example
/// 
///     let graph =
///       empty Directed
///       |> addNode 1 "A"
///       |> addNode 2 "B"
///       |> addNode 3 "C"
///       |> addEdge 1 2 1
///       |> addEdge 2 3 1
///       |> addEdge 3 1 1  // Creates a cycle
///     
///     let sccs = kosaraju graph
///     // => [[1; 2; 3]]  // All nodes form one SCC
/// 
/// ## Comparison with Tarjan's Algorithm
/// 
/// - **Kosaraju:** Two DFS passes, requires graph transposition, simpler to understand
/// - **Tarjan:** Single DFS pass, no transposition needed, uses low-link values
/// 
/// Both have the same O(V + E) time complexity, but Kosaraju may be preferred when:
/// - The graph is already stored in a format supporting fast transposition
/// - Simplicity and clarity are prioritized over single-pass execution
/// 
/// ## Use Cases
/// 
/// - Same as Tarjan's: cycle detection, condensation graphs, 2-SAT
/// - When you want a conceptually simpler algorithm
/// - When graph transposition is already available (O(1) in Yog!)
let kosaraju (graph: Graph<'n, 'e>) : NodeId list list =
    let visited = HashSet<NodeId>()
    let finishStack = Stack<NodeId>()

    // Pass 1: DFS to compute finishing times
    let rec dfs1 (v: NodeId) =
        if visited.Add(v) then
            for w in successorIds v graph do
                dfs1 w
            // Add to stack *after* visiting all descendants
            finishStack.Push(v)

    for node in allNodes graph do
        dfs1 node

    // Pass 2: DFS on the transposed graph in reverse finishing time order
    let transposed = Yog.Transform.transpose graph
    visited.Clear() // Reuse the hashset for the second pass

    let mutable components = []

    let rec dfs2 (v: NodeId) (comp: NodeId list) =
        if visited.Add(v) then
            let mutable currentComp = v :: comp

            for w in successorIds v transposed do
                currentComp <- dfs2 w currentComp

            currentComp
        else
            comp

    // Process nodes in the order they were finished
    while finishStack.Count > 0 do
        let v = finishStack.Pop()

        if not (visited.Contains(v)) then
            let comp = dfs2 v []
            components <- comp :: components

    // Reversing the final list of components matches topological ordering of the SCCs
    List.rev components
