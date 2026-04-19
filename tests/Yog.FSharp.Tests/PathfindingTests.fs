/// Tests for Pathfinding algorithms.
///
/// Covers:
/// - Dijkstra's algorithm (shortest paths from source)
/// - A* search (heuristic-guided shortest paths)
/// - Bellman-Ford (single-source with negative weights)
/// - Floyd-Warshall (all-pairs shortest paths)
/// - DistanceMatrix utilities
module Yog.FSharp.Tests.PathfindingTests

open Xunit
open Yog.Model
open Yog.Pathfinding.Dijkstra
open Yog.Pathfinding.AStar
open Yog.Pathfinding.BellmanFord
open Yog.Pathfinding.FloydWarshall
open Yog.Pathfinding.DistanceMatrix

// =============================================================================
// DIJKSTRA TESTS
// =============================================================================

module DijkstraTests =
    open Yog.Pathfinding.Utils

    /// Helper to create a weighted directed graph.
    let makeWeightedGraph (edges: (NodeId * NodeId * int) list) : Graph<unit, int> =
        let allNodes = edges |> List.collect (fun (u, v, _) -> [ u; v ]) |> List.distinct

        let g = empty Directed

        let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g

        edges |> List.fold (fun acc (u, v, w) -> addEdge u v w acc) gWithNodes

    [<Fact>]
    let ``Dijkstra shortestPath - simple line`` () =
        // 0 --10--> 1 --5--> 2
        let graph = makeWeightedGraph [ (0, 1, 10); (1, 2, 5) ]

        let result = shortestPathInt 0 2 graph

        Assert.True(result.IsSome)
        Assert.Equal<int list>([ 0; 1; 2 ], result.Value.Nodes)
        Assert.Equal(15, result.Value.TotalWeight)

    [<Fact>]
    let ``Dijkstra shortestPath - two paths shorter via detour`` () =
        // 0 --100--> 2
        // 0 --1--> 1 --1--> 2
        let graph = makeWeightedGraph [ (0, 2, 100); (0, 1, 1); (1, 2, 1) ]

        let result = shortestPathInt 0 2 graph

        Assert.True(result.IsSome)
        Assert.Equal(2, result.Value.TotalWeight) // Via 1, not direct
        Assert.Equal<int list>([ 0; 1; 2 ], result.Value.Nodes)

    [<Fact>]
    let ``Dijkstra shortestPath - unreachable node`` () =
        // 0 --> 1, but 2 is isolated
        let graph = makeWeightedGraph [ (0, 1, 5) ] |> addNode 2 ()
        let result = shortestPathInt 0 2 graph

        Assert.True(result.IsNone) // Unreachable

    [<Fact>]
    let ``Dijkstra shortestPath - source equals goal`` () =
        let graph = makeWeightedGraph [ (0, 1, 5) ]
        let result = shortestPathInt 0 0 graph

        Assert.True(result.IsSome)
        Assert.Equal(0, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0 ], result.Value.Nodes)

    [<Fact>]
    let ``Dijkstra shortestPath - diamond network`` () =
        //     1
        //   1/ \2
        //   0   3
        //   2\ /3
        //     2
        let graph = makeWeightedGraph [ (0, 1, 1); (0, 2, 2); (1, 3, 2); (2, 3, 3) ]

        let result = shortestPathInt 0 3 graph

        Assert.True(result.IsSome)
        Assert.Equal(3, result.Value.TotalWeight) // Via 1: 1+2=3

    [<Fact>]
    let ``Dijkstra singleSourceDistances - computes all distances`` () =
        let graph = makeWeightedGraph [ (0, 1, 5); (0, 2, 10); (1, 2, 1) ]

        let result = singleSourceDistancesInt 0 graph

        Assert.Equal(3, result.Count) // 0, 1, 2
        Assert.Equal(0, result.[0])
        Assert.Equal(5, result.[1])
        Assert.Equal(6, result.[2]) // Via 1: 5+1=6

    [<Fact>]
    let ``Dijkstra singleSourceDistances - unreachable nodes not in result`` () =
        let graph = makeWeightedGraph [ (0, 1, 5) ] |> addNode 2 ()
        let result = singleSourceDistancesInt 0 graph

        Assert.Equal(2, result.Count) // Only 0 and 1
        Assert.True(result.ContainsKey 0)
        Assert.True(result.ContainsKey 1)
        Assert.False(result.ContainsKey 2)

    // ========== Additional Edge Cases ==========

    [<Fact>]
    let ``Dijkstra shortestPath - no path invalid start`` () =
        let graph = makeWeightedGraph [ (0, 1, 5) ]
        let result = shortestPathInt 99 1 graph
        Assert.True(result.IsNone)

    [<Fact>]
    let ``Dijkstra shortestPath - no path invalid goal`` () =
        let graph = makeWeightedGraph [ (0, 1, 5) ]
        let result = shortestPathInt 0 99 graph
        Assert.True(result.IsNone)

    [<Fact>]
    let ``Dijkstra shortestPath - empty graph`` () =
        let graph = empty Directed
        let result = shortestPathInt 0 1 graph
        Assert.True(result.IsNone)

    [<Fact>]
    let ``Dijkstra shortestPath - single node graph`` () =
        let graph = empty Directed |> addNode 0 ()
        let result = shortestPathInt 0 0 graph
        Assert.True(result.IsSome)
        Assert.Equal(0, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0 ], result.Value.Nodes)

    [<Fact>]
    let ``Dijkstra shortestPath - zero weight edges`` () =
        let graph = makeWeightedGraph [ (0, 1, 0); (1, 2, 0); (2, 3, 0) ]

        let result = shortestPathInt 0 3 graph
        Assert.True(result.IsSome)
        Assert.Equal(0, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0; 1; 2; 3 ], result.Value.Nodes)

    [<Fact>]
    let ``Dijkstra shortestPath - self loop ignored`` () =
        let graph = makeWeightedGraph [ (0, 0, 5); (0, 1, 10) ]

        let result = shortestPathInt 0 1 graph
        Assert.True(result.IsSome)
        Assert.Equal(10, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0; 1 ], result.Value.Nodes)

    [<Fact>]
    let ``Dijkstra shortestPath - with cycle`` () =
        // Cycle: 0 -> 1 -> 2 -> 0
        let graph = makeWeightedGraph [ (0, 1, 1); (1, 2, 1); (2, 0, 1); (1, 3, 5) ]

        let result = shortestPathInt 0 3 graph
        Assert.True(result.IsSome)
        Assert.Equal(6, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0; 1; 3 ], result.Value.Nodes)

    [<Fact>]
    let ``Dijkstra shortestPath - disconnected components`` () =
        // Two components: {0, 1} and {2, 3}
        let graph = makeWeightedGraph [ (0, 1, 1); (2, 3, 1) ]

        let result = shortestPathInt 0 3 graph
        Assert.True(result.IsNone)

    [<Fact>]
    let ``Dijkstra shortestPath - long chain`` () =
        let edges = [ (0, 1, 1); (1, 2, 1); (2, 3, 1); (3, 4, 1); (4, 5, 1); (5, 6, 1) ]

        let graph = makeWeightedGraph edges
        let result = shortestPathInt 0 6 graph
        Assert.True(result.IsSome)
        Assert.Equal(6, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0; 1; 2; 3; 4; 5; 6 ], result.Value.Nodes)

    // ========== Complex Graph Tests ==========

    [<Fact>]
    let ``Dijkstra shortestPath - grid network`` () =
        // 2x3 grid:
        // 0 -1-> 1 -1-> 2
        // |      |      |
        // 10     1      10
        // |      |      |
        // v      v      v
        // 3 -1-> 4 -1-> 5
        let edges =
            [ (0, 1, 1)
              (1, 2, 1) // Row 1
              (3, 4, 1)
              (4, 5, 1) // Row 2
              (0, 3, 10)
              (1, 4, 1)
              (2, 5, 10) ] // Columns

        let graph = makeWeightedGraph edges
        let result = shortestPathInt 0 5 graph
        Assert.True(result.IsSome)
        Assert.Equal(3, result.Value.TotalWeight) // 0->1->4->5
        Assert.Equal<int list>([ 0; 1; 4; 5 ], result.Value.Nodes)

    [<Fact>]
    let ``Dijkstra shortestPath - classic greedy fails`` () =
        // Classic example where greedy fails but Dijkstra succeeds
        //      0
        //     /|\
        //   (1)(2)(4)
        //   /  |  \
        //  1   2   3
        //  |   |
        // (9) (2)
        //  |   |
        //  4   4
        let edges = [ (0, 1, 1); (0, 2, 2); (0, 3, 4); (1, 4, 9); (2, 4, 2) ]

        let graph = makeWeightedGraph edges
        let result = shortestPathInt 0 4 graph
        Assert.True(result.IsSome)
        Assert.Equal(4, result.Value.TotalWeight) // Via node 2: 2+2=4, not via 1: 1+9=10
        Assert.Equal<int list>([ 0; 2; 4 ], result.Value.Nodes)

    // ========== Undirected Graph Tests ==========

    [<Fact>]
    let ``Dijkstra shortestPath - undirected can traverse both ways`` () =
        let graph =
            empty Undirected
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 5
            |> addEdge 1 2 3

        let result = shortestPathInt 2 0 graph
        Assert.True(result.IsSome)
        Assert.Equal(8, result.Value.TotalWeight)
        Assert.Equal<int list>([ 2; 1; 0 ], result.Value.Nodes)

    // ========== Float Weight Tests ==========

    [<Fact>]
    let ``Dijkstra shortestPath - float weights`` () =
        let graph =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 1.5
            |> addEdge 1 2 2.5
            |> addEdge 0 2 5.0

        let result = shortestPathFloat 0 2 graph
        Assert.True(result.IsSome)
        Assert.Equal(4.0, result.Value.TotalWeight, 3)
        Assert.Equal<int list>([ 0; 1; 2 ], result.Value.Nodes)

    // ========== More Single Source Distance Tests ==========

    [<Fact>]
    let ``Dijkstra singleSourceDistances - complete graph`` () =
        let edges = [ (0, 1, 1); (0, 2, 4); (1, 2, 2) ]
        let graph = makeWeightedGraph edges
        let result = singleSourceDistancesInt 0 graph
        Assert.Equal(3, result.Count)
        Assert.Equal(0, result.[0])
        Assert.Equal(1, result.[1])
        Assert.Equal(3, result.[2]) // Via 1: 1+2=3, not direct: 4

    [<Fact>]
    let ``Dijkstra singleSourceDistances - isolated source`` () =
        let edges = [ (1, 2, 5) ]
        let graph = makeWeightedGraph edges |> addNode 0 ()
        let result = singleSourceDistancesInt 0 graph
        Assert.Equal(1, result.Count) // Only distance to self
        Assert.Equal(0, result.[0])

    [<Fact>]
    let ``Dijkstra singleSourceDistances - with cycles`` () =
        let edges = [ (0, 1, 1); (1, 2, 1); (2, 0, 1) ]
        let graph = makeWeightedGraph edges
        let result = singleSourceDistancesInt 0 graph
        Assert.Equal(3, result.Count)
        Assert.Equal(0, result.[0])
        Assert.Equal(1, result.[1])
        Assert.Equal(2, result.[2])

    [<Fact>]
    let ``Dijkstra singleSourceDistances - undirected`` () =
        let graph =
            empty Undirected
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 5
            |> addEdge 1 2 3

        let result = singleSourceDistancesInt 0 graph
        Assert.Equal(3, result.Count)
        Assert.Equal(0, result.[0])
        Assert.Equal(5, result.[1])
        Assert.Equal(8, result.[2])

    [<Fact>]
    let ``Dijkstra singleSourceDistances - float weights`` () =
        let graph =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 1.5
            |> addEdge 1 2 2.5

        let result = singleSourceDistancesFloat 0 graph
        Assert.Equal(3, result.Count)
        Assert.Equal(0.0, result.[0], 3)
        Assert.Equal(1.5, result.[1], 3)
        Assert.Equal(4.0, result.[2], 3)

    [<Fact>]
    let ``Dijkstra singleSourceDistances - star graph`` () =
        // Center node 0 connected to 1, 2, 3, 4 with weights equal to node ID
        let edges = [ (0, 1, 1); (0, 2, 2); (0, 3, 3); (0, 4, 4) ]

        let graph = makeWeightedGraph edges
        let result = singleSourceDistancesInt 0 graph
        Assert.Equal(5, result.Count)
        Assert.Equal(0, result.[0])
        Assert.Equal(1, result.[1])
        Assert.Equal(2, result.[2])
        Assert.Equal(3, result.[3])
        Assert.Equal(4, result.[4])

    [<Fact>]
    let ``Dijkstra singleSourceDistances - empty graph`` () =
        let graph = empty Directed
        let result = singleSourceDistancesInt 0 graph
        // Source doesn't exist, but distance to self is 0
        Assert.Equal(1, result.Count)
        Assert.Equal(0, result.[0])

    // ========== Implicit Dijkstra Tests ==========

    [<Fact>]
    let ``Dijkstra implicitDijkstra - linear state space`` () =
        let successors (n: int) = if n < 5 then [ (n + 1, 10) ] else []
        let result = implicitDijkstra 0 (+) compare successors (fun n -> n = 5) 1

        match result with
        | Some cost -> Assert.Equal(40, cost) // 4 steps * 10
        | None -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``Dijkstra implicitDijkstra - multiple paths finds shortest`` () =
        let successors (pos: int) =
            match pos with
            | 1 -> [ (2, 100); (3, 10) ] // Expensive direct vs cheap via 3
            | 2 -> [ (4, 1) ]
            | 3 -> [ (2, 5) ]
            | _ -> []

        let result = implicitDijkstra 0 (+) compare successors (fun n -> n = 2) 1

        match result with
        | Some cost -> Assert.Equal(15, cost) // 1->3->2: 10+5=15, not 1->2: 100
        | None -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``Dijkstra implicitDijkstra - grid Manhattan distance`` () =
        let successors ((x, y): int * int) =
            [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
            |> List.filter (fun (nx, ny) -> nx >= 0 && ny >= 0 && nx <= 3 && ny <= 3)
            |> List.map (fun pos -> (pos, 1))

        let result =
            implicitDijkstra 0 (+) compare successors (fun (x, y) -> x = 3 && y = 3) (0, 0)

        match result with
        | Some cost -> Assert.Equal(6, cost) // Manhattan distance (0,0) to (3,3)
        | None -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``Dijkstra implicitDijkstra - no path to goal`` () =
        let successors (n: int) = if n < 3 then [ (n + 1, 1) ] else []
        let result = implicitDijkstra 0 (+) compare successors (fun n -> n = 10) 1
        Assert.True(result.IsNone)

    [<Fact>]
    let ``Dijkstra implicitDijkstra - start equals goal`` () =
        let successors (n: int) = [ (n + 1, 10) ]
        let result = implicitDijkstra 0 (+) compare successors (fun n -> n = 42) 42

        match result with
        | Some cost -> Assert.Equal(0, cost)
        | None -> Assert.True(false, "Should find path to self")

    [<Fact>]
    let ``Dijkstra implicitDijkstra - with cycle`` () =
        let successors (n: int) =
            match n with
            | 1 -> [ (2, 10) ]
            | 2 -> [ (3, 5); (1, 1) ] // Cycle back to 1
            | 3 -> [ (4, 1) ]
            | _ -> []

        let result = implicitDijkstra 0 (+) compare successors (fun n -> n = 4) 1

        match result with
        | Some cost -> Assert.Equal(16, cost)
        | None -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``Dijkstra implicitDijkstraBy - dedupe by position`` () =
        // State is (position, metadata) but dedupe by position only
        let successors ((pos, mask): int * int) =
            match pos with
            | 1 -> [ ((2, mask + 1), 10); ((3, mask + 100), 5) ]
            | 2 -> [ ((4, mask + 1), 1) ]
            | 3 -> [ ((4, mask + 100), 2) ]
            | _ -> []

        let result =
            implicitDijkstraBy 0 (+) compare successors (fun (p, _) -> p) (fun (p, _) -> p = 4) (1, 0)

        match result with
        | Some cost -> Assert.Equal(7, cost) // 1->3->4: 5+2=7
        | None -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``Dijkstra implicitDijkstraBy - identity key equals regular`` () =
        let successors (n: int) = if n < 5 then [ (n + 1, 3) ] else []

        let resultBy =
            implicitDijkstraBy 0 (+) compare successors (fun n -> n) (fun n -> n = 5) 1

        let resultRegular = implicitDijkstra 0 (+) compare successors (fun n -> n = 5) 1
        Assert.Equal(resultRegular, resultBy)

        match resultBy with
        | Some cost -> Assert.Equal(12, cost)
        | None -> Assert.True(false, "Should find path")

// =============================================================================
// A* TESTS
// =============================================================================

module AStarTests =
    open Yog.Pathfinding.Utils

    /// Manhattan distance heuristic for grid graphs.
    /// Takes both current node and goal, returns estimated cost.
    let manhattanHeuristic (node: NodeId) (goal: NodeId) : int =
        // Assume node IDs encode (x, y) as x + y*100
        let gx, gy = goal % 100, goal / 100
        let nx, ny = node % 100, node / 100
        abs (nx - gx) + abs (ny - gy)

    [<Fact>]
    let ``A* - simple path`` () =
        // 0 --1--> 1 --1--> 2
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 2, 1) ]

        let result = aStarInt manhattanHeuristic 0 2 graph

        Assert.True(result.IsSome)
        Assert.Equal(2, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0; 1; 2 ], result.Value.Nodes)

    [<Fact>]
    let ``A* - same source and goal`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1) ]
        let result = aStarInt manhattanHeuristic 0 0 graph

        Assert.True(result.IsSome)
        Assert.Equal(0, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0 ], result.Value.Nodes)

    [<Fact>]
    let ``A* - unreachable goal`` () =
        let graph = empty Directed |> addNode 0 () |> addNode 1 ()
        // No edges between them
        let result = aStarInt manhattanHeuristic 0 1 graph

        Assert.True(result.IsNone)

    [<Fact>]
    let ``A* - finds optimal path in grid`` () =
        // Create a 3x3 grid with node IDs as x + y*10
        // (0,0)=0, (1,0)=1, (2,0)=2
        // (0,1)=10, (1,1)=11, (2,1)=12
        let graph =
            let edges =
                [
                  // Horizontal edges
                  (0, 1, 1)
                  (1, 2, 1)
                  (10, 11, 1)
                  (11, 12, 1)
                  (20, 21, 1)
                  (21, 22, 1)
                  // Vertical edges
                  (0, 10, 1)
                  (10, 20, 1)
                  (1, 11, 1)
                  (11, 21, 1)
                  (2, 12, 1)
                  (12, 22, 1) ]

            DijkstraTests.makeWeightedGraph edges

        let result = aStarInt manhattanHeuristic 0 22 graph

        Assert.True(result.IsSome)
        Assert.Equal(4, result.Value.TotalWeight) // 4 steps in Manhattan distance

    [<Fact>]
    let ``A* - zero heuristic equals Dijkstra`` () =
        // Zero heuristic should give same result as Dijkstra
        let graph =
            DijkstraTests.makeWeightedGraph [ (0, 1, 5); (0, 2, 2); (2, 3, 2); (1, 3, 1) ]

        let aStarResult = aStarInt (fun _ _ -> 0) 0 3 graph
        let dijkstraResult = shortestPathInt 0 3 graph

        Assert.True(aStarResult.IsSome)
        Assert.True(dijkstraResult.IsSome)
        Assert.Equal(dijkstraResult.Value.TotalWeight, aStarResult.Value.TotalWeight)

    // ========== Additional A* Tests ==========

    [<Fact>]
    let ``A* - diamond with heuristic`` () =
        let graph =
            DijkstraTests.makeWeightedGraph [ (0, 1, 1); (0, 2, 10); (1, 3, 1); (2, 3, 1) ]

        let result = aStarInt manhattanHeuristic 0 3 graph
        Assert.True(result.IsSome)
        Assert.Equal(2, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0; 1; 3 ], result.Value.Nodes)

    [<Fact>]
    let ``A* - empty graph`` () =
        let graph = empty Directed
        let result = aStarInt manhattanHeuristic 0 1 graph
        Assert.True(result.IsNone)

    [<Fact>]
    let ``A* - single node`` () =
        let graph = empty Directed |> addNode 0 ()
        let result = aStarInt manhattanHeuristic 0 0 graph
        Assert.True(result.IsSome)
        Assert.Equal(0, result.Value.TotalWeight)

    [<Fact>]
    let ``A* - float weights with heuristic`` () =
        let graph =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 1.5
            |> addEdge 1 2 2.5
            |> addEdge 0 2 5.0

        let heuristic _ _ = 0.0
        let result = aStarFloat heuristic 0 2 graph
        Assert.True(result.IsSome)
        Assert.Equal(4.0, result.Value.TotalWeight, 3)

    [<Fact>]
    let ``A* - undirected graph`` () =
        let graph =
            empty Undirected
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 5
            |> addEdge 1 2 3

        let result = aStarInt manhattanHeuristic 2 0 graph
        Assert.True(result.IsSome)
        Assert.Equal(8, result.Value.TotalWeight)

    [<Fact>]
    let ``A* - with cycle`` () =
        let edges = [ (0, 1, 1); (1, 2, 1); (2, 0, 1); (1, 3, 5) ]

        let graph = DijkstraTests.makeWeightedGraph edges
        let result = aStarInt manhattanHeuristic 0 3 graph
        Assert.True(result.IsSome)
        Assert.Equal(6, result.Value.TotalWeight)

    [<Fact>]
    let ``A* - longer chain`` () =
        let edges = [ (0, 1, 1); (1, 2, 1); (2, 3, 1); (3, 4, 1) ]

        let graph = DijkstraTests.makeWeightedGraph edges
        let result = aStarInt manhattanHeuristic 0 4 graph
        Assert.True(result.IsSome)
        Assert.Equal(4, result.Value.TotalWeight)
        Assert.Equal<int list>([ 0; 1; 2; 3; 4 ], result.Value.Nodes)

    [<Fact>]
    let ``A* implicitAStar - simple linear`` () =
        let successors (n: int) = if n < 5 then [ (n + 1, 1) ] else []
        let heuristic (n: int) = 5 - n // Distance to goal
        let result = implicitAStar 0 (+) compare heuristic successors (fun n -> n = 5) 0

        match result with
        | Some cost -> Assert.Equal(5, cost)
        | None -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``A* implicitAStar - grid with Manhattan heuristic`` () =
        let successors ((x, y): int * int) =
            [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
            |> List.filter (fun (nx, ny) -> nx >= 0 && ny >= 0 && nx <= 3 && ny <= 3)
            |> List.map (fun pos -> (pos, 1))

        let heuristic ((x, y): int * int) = (3 - x) + (3 - y) // Manhattan to (3,3)

        let result =
            implicitAStar 0 (+) compare heuristic successors (fun (x, y) -> x = 3 && y = 3) (0, 0)

        match result with
        | Some cost -> Assert.Equal(6, cost)
        | None -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``A* implicitAStar - no path`` () =
        let successors (n: int) = if n < 3 then [ (n + 1, 1) ] else []
        let heuristic (n: int) = abs (10 - n)
        let result = implicitAStar 0 (+) compare heuristic successors (fun n -> n = 10) 1
        Assert.True(result.IsNone)

    [<Fact>]
    let ``A* implicitAStarBy - dedupe by position`` () =
        let successors ((pos, _): int * string) =
            match pos with
            | 1 -> [ ((2, "path1"), 5); ((3, "path2"), 10) ]
            | 2 -> [ ((4, "end1"), 1) ]
            | 3 -> [ ((4, "end2"), 1) ]
            | _ -> []

        let heuristic ((pos, _): int * string) = 4 - pos

        let result =
            implicitAStarBy 0 (+) compare heuristic successors (fun (p, _) -> p) (fun (p, _) -> p = 4) (1, "start")

        match result with
        | Some cost -> Assert.Equal(6, cost) // 1->2->4: 5+1=6
        | None -> Assert.True(false, "Should find path")

// =============================================================================
// BELLMAN-FORD TESTS
// =============================================================================

module BellmanFordTests =
    [<Fact>]
    let ``BellmanFord - simple path`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 5); (1, 2, 3) ]

        let result = bellmanFordInt 0 2 graph

        match result with
        | ShortestPath path ->
            Assert.Equal(8, path.TotalWeight)
            Assert.Equal<int list>([ 0; 1; 2 ], path.Nodes)
        | _ -> Assert.True(false, "Should find shortest path")

    [<Fact>]
    let ``BellmanFord - negative weights without cycle`` () =
        // 0 --5--> 1 --(-10)--> 2 --3--> 3
        // Shortest: 0 -> 1 -> 2 -> 3 = -2
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 5); (1, 2, -10); (2, 3, 3) ]

        let result = bellmanFordInt 0 3 graph

        match result with
        | ShortestPath path -> Assert.Equal(-2, path.TotalWeight)
        | _ -> Assert.True(false, "Should find shortest path")

    [<Fact>]
    let ``BellmanFord - detects negative cycle`` () =
        // 0 -> 1 (1), 1 -> 2 (-1), 2 -> 0 (-1)
        // Cycle sum: 1 + (-1) + (-1) = -1 < 0
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 2, -1); (2, 0, -1) ]

        let result = bellmanFordInt 0 2 graph

        match result with
        | NegativeCycle -> Assert.True(true)
        | _ -> Assert.True(false, "Should detect negative cycle")

    [<Fact>]
    let ``BellmanFord - self-loop negative cycle`` () =
        // Node 0 has a negative self-loop
        let graph = DijkstraTests.makeWeightedGraph [ (0, 0, -1) ]
        let result = bellmanFordInt 0 0 graph

        match result with
        | NegativeCycle -> Assert.True(true)
        | _ -> Assert.True(false, "Should detect negative self-loop")

    [<Fact>]
    let ``BellmanFord - no path to goal`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 5) ] |> addNode 2 ()

        let result = bellmanFordInt 0 2 graph

        match result with
        | NoPath -> Assert.True(true)
        | _ -> Assert.True(false, "Should report no path")

    [<Fact>]
    let ``BellmanFord - same source and goal`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 5) ]
        let result = bellmanFordInt 0 0 graph

        match result with
        | ShortestPath path ->
            Assert.Equal(0, path.TotalWeight)
            Assert.Equal<int list>([ 0 ], path.Nodes)
        | _ -> Assert.True(false, "Should find path to self")

    // ========== Additional Bellman-Ford Tests ==========

    [<Fact>]
    let ``BellmanFord - complex negative weights`` () =
        // 0 -> 1 (4), 1 -> 2 (-5), 0 -> 2 (2), 2 -> 3 (1)
        // Best: 0 -> 1 -> 2 -> 3 = 4 + (-5) + 1 = 0
        let graph =
            DijkstraTests.makeWeightedGraph [ (0, 1, 4); (1, 2, -5); (0, 2, 2); (2, 3, 1) ]

        let result = bellmanFordInt 0 3 graph

        match result with
        | ShortestPath path -> Assert.Equal(0, path.TotalWeight)
        | _ -> Assert.True(false, "Should find shortest path")

    [<Fact>]
    let ``BellmanFord - empty graph`` () =
        let graph = empty Directed
        let result = bellmanFordInt 0 1 graph

        match result with
        | NoPath -> Assert.True(true)
        | _ -> Assert.True(false, "Should report no path")

    [<Fact>]
    let ``BellmanFord - undirected with negative weights`` () =
        // Undirected edge with negative weight creates negative cycle
        let graph = empty Undirected |> addNode 0 () |> addNode 1 () |> addEdge 0 1 -1

        let result = bellmanFordInt 0 1 graph

        match result with
        | NegativeCycle -> Assert.True(true)
        | _ -> Assert.True(false, "Negative edge in undirected graph creates cycle")

    [<Fact>]
    let ``BellmanFord - negative cycle not on path`` () =
        // 0 -> 1 (1) -> 2 (1)
        // 3 -> 4 (-1) -> 3 (negative cycle, but not on path from 0 to 2)
        let graph =
            DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 2, 1); (3, 4, -1); (4, 3, -1) ]

        let result = bellmanFordInt 0 2 graph

        match result with
        | ShortestPath path -> Assert.Equal(2, path.TotalWeight)
        | _ -> Assert.True(false, "Should find path even if negative cycle exists elsewhere")

    [<Fact>]
    let ``BellmanFord - longer path with negative edges`` () =
        let edges = [ (0, 1, 5); (1, 2, -3); (2, 3, 2); (3, 4, -1); (4, 5, 3) ]

        let graph = DijkstraTests.makeWeightedGraph edges
        let result = bellmanFordInt 0 5 graph

        match result with
        | ShortestPath path -> Assert.Equal(6, path.TotalWeight) // 5-3+2-1+3
        | _ -> Assert.True(false, "Should find shortest path")

    [<Fact>]
    let ``BellmanFord implicitBellmanFord - simple path`` () =
        let successors (n: int) = if n < 3 then [ (n + 1, 2) ] else []
        let result = implicitBellmanFord 0 (+) compare successors (fun n -> n = 3) 0

        match result with
        | FoundGoal cost -> Assert.Equal(6, cost)
        | _ -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``BellmanFord implicitBellmanFord - with negative weights`` () =
        let successors (n: int) =
            match n with
            | 0 -> [ (1, 5); (2, 10) ]
            | 1 -> [ (2, -3) ] // Negative edge
            | _ -> []

        let result = implicitBellmanFord 0 (+) compare successors (fun n -> n = 2) 0

        match result with
        | FoundGoal cost -> Assert.Equal(2, cost) // 0 -> 1 -> 2 = 5 + (-3) = 2
        | _ -> Assert.True(false, "Should find path")

    [<Fact>]
    let ``BellmanFord implicitBellmanFord - no path`` () =
        let successors (n: int) = if n < 3 then [ (n + 1, 1) ] else []
        let result = implicitBellmanFord 0 (+) compare successors (fun n -> n = 10) 0

        match result with
        | NoGoal -> Assert.True(true)
        | _ -> Assert.True(false, "Should report no goal found")

// =============================================================================
// FLOYD-WARSHALL TESTS
// =============================================================================

module FloydWarshallTests =
    [<Fact>]
    let ``FloydWarshall - all pairs in triangle`` () =
        // 0 --1--> 1 --1--> 2
        // 0 --3--> 2 (direct)
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 2, 1); (0, 2, 3) ]

        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            Assert.Equal(0, dists.[(0, 0)])
            Assert.Equal(1, dists.[(0, 1)])
            Assert.Equal(2, dists.[(0, 2)]) // Via 1 is shorter (1+1=2)
            Assert.True(dists.Count >= 6) // At least 3*2 directed pairs
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

    [<Fact>]
    let ``FloydWarshall - detects negative cycle`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 2, -1); (2, 0, -1) ]

        let result = floydWarshallInt graph

        match result with
        | Error() -> Assert.True(true)
        | Ok _ -> Assert.True(false, "Should detect negative cycle")

    [<Fact>]
    let ``FloydWarshall - complete graph distances`` () =
        let graph =
            DijkstraTests.makeWeightedGraph [ (0, 1, 4); (0, 2, 1); (1, 2, 2); (1, 3, 5); (2, 3, 1); (3, 0, 7) ]

        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            Assert.Equal(0, dists.[(0, 0)])
            Assert.Equal(4, dists.[(0, 1)]) // Direct 0->1 = 4, no shorter path via 2 in directed graph
            Assert.Equal(1, dists.[(0, 2)]) // Direct 0->2 = 1
            Assert.True(dists.Count >= 6) // At least 3*2 directed pairs
            // 0 -> 3: min(0->2->3=2, 0->1->2->3=4+2+1=7, 0->1->3=4+5=9) = 2
            Assert.Equal(2, dists.[(0, 3)])
            // 3 -> 0: direct edge has weight 7
            Assert.Equal(7, dists.[(3, 0)])
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

// =============================================================================
// DISTANCE MATRIX AND FLOAT WRAPPER TESTS
// =============================================================================

module FloatWrapperTests =
    [<Fact>]
    let ``FloydWarshall - negative cycle detection`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 2, -1); (2, 0, -1) ]

        let result = floydWarshallInt graph

        match result with
        | Error() -> Assert.True(true)
        | Ok _ -> Assert.True(false, "Should detect negative cycle")

    [<Fact>]
    let ``distanceMatrixFloat - basic two-node graph`` () =
        let g0: Graph<unit, float> = Yog.Model.empty Directed
        let g1: Graph<unit, float> = Yog.Model.addNode 0 () g0
        let g2: Graph<unit, float> = Yog.Model.addNode 1 () g1
        let g3: Graph<unit, float> = Yog.Model.addEdge 0 1 1.5 g2
        let dists = distanceMatrixFloat [ 0; 1 ] g3

        match dists with
        | Ok d -> Assert.Equal(1.5, d.[(0, 1)], 5)
        | Error _ -> Assert.True(false)

    [<Fact>]
    let ``bellmanFordFloat - basic path`` () =
        let g0: Graph<unit, float> = Yog.Model.empty Directed
        let g1: Graph<unit, float> = Yog.Model.addNode 0 () g0
        let g2: Graph<unit, float> = Yog.Model.addNode 1 () g1
        let g3: Graph<unit, float> = Yog.Model.addNode 2 () g2
        let g4: Graph<unit, float> = Yog.Model.addEdge 0 1 1.5 g3
        let g5: Graph<unit, float> = Yog.Model.addEdge 1 2 2.5 g4
        let result = bellmanFordFloat 0 2 g5

        match result with
        | ShortestPath p -> Assert.Equal(4.0, p.TotalWeight, 5)
        | _ -> Assert.True(false, "Should find float shortest path")

    [<Fact>]
    let ``FloydWarshall - empty graph`` () =
        let graph = empty Directed
        let result = floydWarshallInt graph

        match result with
        | Ok dists -> Assert.Equal(0, dists.Count)
        | Error _ -> Assert.True(false, "Empty graph has no cycles")

    [<Fact>]
    let ``FloydWarshall - single node`` () =
        let graph = empty Directed |> addNode 0 ()
        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            Assert.Equal(1, dists.Count)
            Assert.Equal(0, dists.[(0, 0)])
        | Error _ -> Assert.True(false, "Single node has no cycles")

    [<Fact>]
    let ``FloydWarshall - symmetric distances in undirected graph`` () =
        let graph =
            let g = empty Undirected |> addNode 0 () |> addNode 1 () |> addNode 2 ()

            g |> addEdge 0 1 5 |> addEdge 1 2 3

        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            // In undirected graph, dist(u,v) = dist(v,u)
            Assert.Equal(dists.[(0, 1)], dists.[(1, 0)])
            Assert.Equal(dists.[(1, 2)], dists.[(2, 1)])
            Assert.Equal(dists.[(0, 2)], dists.[(2, 0)])
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

    [<Fact>]
    let ``FloydWarshall - transitive closure`` () =
        // 0 -> 1 -> 2 -> 3
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 2, 1); (2, 3, 1) ]

        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            Assert.Equal(1, dists.[(0, 1)])
            Assert.Equal(2, dists.[(0, 2)])
            Assert.Equal(3, dists.[(0, 3)])
            // Reverse should not exist (unreachable in directed graph)
            Assert.False(dists.ContainsKey((3, 0)))
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

    // ========== Additional Floyd-Warshall Tests ==========

    [<Fact>]
    let ``FloydWarshall - disconnected components`` () =
        // Two components: {0, 1} and {2, 3}
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1); (2, 3, 1) ]

        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            // Distances within components exist
            Assert.Equal(1, dists.[(0, 1)])
            Assert.Equal(1, dists.[(2, 3)])
            // Cross-component distances don't exist
            Assert.False(dists.ContainsKey((0, 2)))
            Assert.False(dists.ContainsKey((1, 3)))
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

    [<Fact>]
    let ``FloydWarshall - with cycles`` () =
        let graph =
            DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 2, 1); (2, 0, 1); (1, 3, 5) ]

        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            Assert.Equal(0, dists.[(0, 0)])
            Assert.Equal(1, dists.[(0, 1)])
            Assert.Equal(2, dists.[(0, 2)])
            Assert.Equal(6, dists.[(0, 3)]) // 0 -> 1 -> 3
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

    [<Fact>]
    let ``FloydWarshall - two nodes`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 5) ]
        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            Assert.Equal(3, dists.Count) // (0,0), (0,1), and (1,1)
            Assert.Equal(0, dists.[(0, 0)])
            Assert.Equal(5, dists.[(0, 1)])
            Assert.Equal(0, dists.[(1, 1)])
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

    [<Fact>]
    let ``FloydWarshall - complex with shorter indirect paths`` () =
        // Direct paths are longer than indirect
        let graph =
            DijkstraTests.makeWeightedGraph
                [ (0, 1, 1)
                  (1, 2, 1)
                  (2, 3, 1) // Chain
                  (0, 3, 100) ] // Expensive direct path

        let result = floydWarshallInt graph

        match result with
        | Ok dists -> Assert.Equal(3, dists.[(0, 3)]) // Via 1, 2, not direct
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

    [<Fact>]
    let ``FloydWarshall - float weights`` () =
        let graph =
            empty Directed
            |> addNode 0 ()
            |> addNode 1 ()
            |> addNode 2 ()
            |> addEdge 0 1 1.5
            |> addEdge 1 2 2.5

        let result = floydWarshallFloat graph

        match result with
        | Ok dists ->
            Assert.Equal(0.0, dists.[(0, 0)], 3)
            Assert.Equal(1.5, dists.[(0, 1)], 3)
            Assert.Equal(4.0, dists.[(0, 2)], 3)
        | Error _ -> Assert.True(false, "Should not detect negative cycle")

// =============================================================================
// DISTANCE MATRIX TESTS
// =============================================================================

module DistanceMatrixTests =
    [<Fact>]
    let ``DistanceMatrix - computes POI distances`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 5); (0, 2, 10); (1, 2, 1) ]

        let result = distanceMatrixInt [ 0; 2 ] graph // Only care about 0 and 2

        match result with
        | Ok dists ->
            // Should have distances between POIs (only 0->2 and 2->0 if reachable)
            Assert.True(dists.ContainsKey((0, 2)))
            Assert.Equal(6, dists.[(0, 2)]) // Via 1: 5+1
            // Only 2 POIs in directed graph, so just 0->2 and 2->0 (if 2 can reach 0)
            Assert.True(dists.Count >= 1)
        | Error _ -> Assert.True(false, "Should compute distances")

    [<Fact>]
    let ``DistanceMatrix - single POI`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 5) ]
        let result = distanceMatrixInt [ 0 ] graph

        match result with
        | Ok dists ->
            // Only distance from 0 to 0
            Assert.Equal(1, dists.Count)
            Assert.Equal(0, dists.[(0, 0)])
        | Error _ -> Assert.True(false, "Should compute distances")

    [<Fact>]
    let ``DistanceMatrix - detects negative cycle`` () =
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, 1); (1, 0, -2) ]

        let result = distanceMatrixInt [ 0; 1 ] graph

        match result with
        | Error() -> Assert.True(true)
        | Ok _ -> Assert.True(false, "Should detect negative cycle")

    [<Fact>]
    let ``DistanceMatrix - dense POIs uses FloydWarshall`` () =
        // 4 nodes, 4 POIs (100% density) -> should use Floyd-Warshall
        // Note: directed graph so not all pairs may be reachable
        let graph =
            DijkstraTests.makeWeightedGraph [ (0, 1, 1); (0, 2, 2); (0, 3, 3); (1, 2, 1); (1, 3, 2); (2, 3, 1) ]

        let result = distanceMatrixInt [ 0; 1; 2; 3 ] graph

        match result with
        | Ok dists ->
            // In directed graph, only pairs with a path are included
            // 0 can reach everyone, 1 can reach 2,3, 2 can reach 3
            Assert.True(dists.Count >= 6) // At least the forward paths
        | Error _ -> Assert.True(false, "Should compute distances")

// =============================================================================
// PATHFINDING PROPERTY TESTS
// =============================================================================

module PathfindingPropertyTests =
    open FsCheck
    open FsCheck.Xunit
    open Yog.Pathfinding.Utils

    [<Property>]
    let ``Dijkstra distance is non-negative for non-negative weights`` (PositiveInt w1) (PositiveInt w2) =
        let weights = [ w1 % 100 + 1; w2 % 100 + 1 ]
        let edges = weights |> List.mapi (fun i w -> (i, i + 1, w))
        let graph = DijkstraTests.makeWeightedGraph edges
        let result = shortestPathInt 0 (weights.Length) graph

        match result with
        | Some path -> path.TotalWeight >= 0
        | None -> true // No path case

    [<Property>]
    let ``A* with zero heuristic finds shortest path`` (PositiveInt w1) (PositiveInt w2) =
        let w1', w2' = w1 % 100 + 1, w2 % 100 + 1
        // Two paths: direct with w1+w2+1 (longer), or via node 1
        let graph =
            DijkstraTests.makeWeightedGraph [ (0, 1, w1'); (1, 2, w2'); (0, 2, w1' + w2' + 1) ]

        let aStarResult = aStarInt (fun _ _ -> 0) 0 2 graph
        let dijkstraResult = shortestPathInt 0 2 graph

        match aStarResult, dijkstraResult with
        | Some a, Some d -> a.TotalWeight = d.TotalWeight
        | None, None -> true
        | _ -> false

    [<Property>]
    let ``BellmanFord agrees with Dijkstra on non-negative weights`` (PositiveInt w1) (PositiveInt w2) =
        let w1', w2' = w1 % 50 + 1, w2 % 50 + 1
        let edges = [ (0, 1, w1'); (1, 2, w2') ]
        let graph = DijkstraTests.makeWeightedGraph edges
        let bfResult = bellmanFordInt 0 2 graph
        let dResult = shortestPathInt 0 2 graph

        match bfResult, dResult with
        | ShortestPath bf, Some d -> bf.TotalWeight = d.TotalWeight
        | NoPath, None -> true
        | _ -> false

    [<Property>]
    let ``FloydWarshall satisfies triangle inequality`` (PositiveInt a) (PositiveInt b) (PositiveInt c) =
        let a', b', c' = a % 50 + 1, b % 50 + 1, c % 50 + 1
        // Create triangle with edge weights a, b, c
        let graph = DijkstraTests.makeWeightedGraph [ (0, 1, a'); (1, 2, b'); (0, 2, c') ]

        let result = floydWarshallInt graph

        match result with
        | Ok dists ->
            let d01 = dists |> Map.tryFind (0, 1) |> Option.defaultValue 10000

            let d12 = dists |> Map.tryFind (1, 2) |> Option.defaultValue 10000

            let d02 = dists |> Map.tryFind (0, 2) |> Option.defaultValue 10000
            // d(0,2) <= d(0,1) + d(1,2)
            d02 <= d01 + d12
        | Error _ -> true // Negative cycle case
