/// Stoer-Wagner global minimum cut algorithm.
///
/// Finds the minimum weight cut that partitions the graph into two non-empty sets,
/// without specifying which nodes must be on which side (unlike s-t min cut).
///
/// ## When to Use
/// - Graph partitioning and clustering
/// - Finding the weakest link in a network
/// - Image segmentation
/// - VLSI design (circuit partitioning)
/// - Community detection in networks
///
/// ## Key Concepts
///
/// ### Global Minimum Cut
/// A partition of nodes into two non-empty sets A and B that minimizes
/// the sum of weights of edges crossing from A to B.
///
/// ### Stoer-Wagner Algorithm
/// Uses Maximum Adjacency Search (MAS) to find the "most tightly connected" node
/// pair, then contracts them and repeats.
///
/// ## Complexity
/// - **Time**: O(V³) - cubic in number of vertices
/// - **Space**: O(V + E)
///
/// ## Comparison with Max-Flow Min-Cut
/// | Aspect | Global Min Cut (Stoer-Wagner) | s-t Min Cut (Max Flow) |
/// |--------|-------------------------------|------------------------|
/// | Source/Sink | Not specified | Must be specified |
/// | Partitions | Any two non-empty sets | Separates specific nodes |
/// | Algorithm | Contraction-based | Augmenting paths |
/// | Complexity | O(V³) | O(VE²) |
///
/// ## Maximum Adjacency Search
/// The key subroutine orders vertices by their connectivity strength to the
/// growing set, identifying the most tightly connected pair.
module Yog.Flow.MinCut

open System.Collections.Generic
open Yog.Model
open Yog.Transform

/// Represents the sizes and weight of a graph partition.
///
/// Returned by `globalMinCut`, describes the minimum cut found.
type MinCut =
    {
        /// Total weight of edges crossing the cut.
        Weight: int
        /// Number of nodes in the first partition.
        GroupASize: int
        /// Number of nodes in the second partition.
        GroupBSize: int
    }

// Internal custom comparer to mimic Gleam's `compare_max`:
// Sorts by weight descending, then by node ID ascending for deterministic tie-breaking.
let private maxAdjacencyComparer =
    { new IComparer<int * NodeId> with
        member _.Compare((w1, n1), (w2, n2)) =
            let weightCompare = w2.CompareTo(w1) // Descending

            if weightCompare = 0 then
                n1.CompareTo(n2) // Ascending ID tie-breaker
            else
                weightCompare }

/// Maximum Adjacency Search (MAS): finds the two most tightly connected nodes.
///
/// ## Parameters
/// - `graph`: Input graph with integer weights
///
/// ## Returns
/// Tuple of (s, t, cut_weight) where s and t are the most tightly connected nodes
/// and cut_weight is the weight of edges from t to the rest of the graph.
///
/// ## Algorithm
/// 1. Start from arbitrary node
/// 2. Repeatedly add the node with highest adjacency weight to the current set
/// 3. The last two nodes added are the most tightly connected pair
let private maximumAdjacencySearch (graph: Graph<int, int>) : NodeId * NodeId * int =
    let nodesList = allNodes graph
    let start = List.head nodesList

    let remaining = HashSet<NodeId>(nodesList)
    remaining.Remove(start) |> ignore

    let weights = Dictionary<NodeId, int>()
    let pq = PriorityQueue<NodeId, int * NodeId>(maxAdjacencyComparer)

    // Initialize with neighbors of the start node
    for (neighbor, weight) in neighbors start graph do
        if remaining.Contains(neighbor) then
            weights.[neighbor] <- weight
            pq.Enqueue(neighbor, (weight, neighbor))

    let mutable orderList = [ start ]

    while remaining.Count > 0 do
        let mutable found = false
        let mutable currNode = 0

        // Pop until we find a valid, non-stale entry
        while pq.Count > 0 && not found do
            let mutable dequeuedNode = 0
            let mutable dequeuedPrio = (0, 0)

            // We use the out-parameter for both.
            // Crucial: The variables must be defined immediately before use.
            while pq.Count > 0 && not found do
                let node = pq.Dequeue()

                if remaining.Contains(node) then
                    // We found the node with the highest current adjacency weight
                    currNode <- node
                    found <- true

        // If the queue was empty but nodes remain, we have a disconnected component
        if not found then
            // Seq.head works on HashSets and is much cleaner
            currNode <- remaining |> Seq.head

        remaining.Remove(currNode) |> ignore
        orderList <- currNode :: orderList

        // Update neighbors of the newly added node
        for (neighbor, edgeWeight) in neighbors currNode graph do
            if remaining.Contains(neighbor) then
                let mutable existingW = 0

                weights.TryGetValue(neighbor, &existingW) |> ignore

                let newW = existingW + edgeWeight
                weights.[neighbor] <- newW
                pq.Enqueue(neighbor, (newW, neighbor))

    // The list is built with newest at head
    let t = orderList.[0]
    let s = orderList.[1]

    let mutable cutWeight = 0
    weights.TryGetValue(t, &cutWeight) |> ignore

    (s, t, cutWeight)

// Internal recursive function to find the global min cut
let rec private doMinCut (graph: Graph<int, int>) (best: MinCut) : MinCut =
    if nodeCount graph <= 1 then
        best
    else
        let (s, t, cutWeight) = maximumAdjacencySearch graph

        let tSize = graph.Nodes.[t]
        let sSize = graph.Nodes.[s]

        // Sum all current node values to get total original nodes
        let totalNodes = graph.Nodes.Values |> Seq.sum

        let currentCut =
            { Weight = cutWeight
              GroupASize = tSize
              GroupBSize = totalNodes - tSize }

        let newBest = if currentCut.Weight < best.Weight then currentCut else best

        // Contract t into s
        let nextGraph = contract s t (+) graph

        // Update the merged node 's' to hold the combined size of the original nodes
        let nextGraphMerged =
            { nextGraph with
                Nodes = nextGraph.Nodes |> Map.add s (sSize + tSize) }

        doMinCut nextGraphMerged newBest

/// Finds the global minimum cut of an undirected weighted graph using the
/// Stoer-Wagner algorithm.
///
/// ## Parameters
/// - `graph`: Input graph with integer edge weights
///
/// ## Returns
/// `MinCut` containing the minimum cut weight and partition sizes.
///
/// ## Algorithm
/// 1. Run Maximum Adjacency Search to find most tightly connected pair
/// 2. The last node added has a cut separating it from the rest
/// 3. Contract the pair and repeat
/// 4. Return the minimum cut found across all iterations
///
/// ## Complexity
/// - **Time**: O(V³)
/// - **Space**: O(V + E)
///
/// ## Example
///
///     // Create a weighted undirected graph
///     let graph =
///         empty Undirected
///         |> addNode 0 () |> addNode 1 () |> addNode 2 () |> addNode 3 ()
///         |> addEdge 0 1 3 |> addEdge 0 2 2 |> addEdge 1 2 4 |> addEdge 2 3 1
///
///     let result = globalMinCut graph
///     // result.Weight = 1 (cut between node 2 and node 3)
///     // result.GroupASize = 3, result.GroupBSize = 1
///
/// ## Use Cases
/// - **Network reliability**: Find minimum edges to remove to disconnect graph
/// - **Clustering**: Natural graph partitioning point
/// - **Image segmentation**: Separate foreground from background
let globalMinCut (graph: Graph<'n, int>) : MinCut =
    // Start every node with a weight of 1 (representing itself)
    // This tracks how many original nodes have been merged together
    let initializedGraph = mapNodes (fun _ -> 1) graph

    let initialBest =
        { Weight = System.Int32.MaxValue
          GroupASize = 0
          GroupBSize = 0 }

    doMinCut initializedGraph initialBest
