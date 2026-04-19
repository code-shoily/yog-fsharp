/// Comprehensive tests for graph builders.
///
/// Covers:
/// - GridBuilder (2D grid graphs)
module Yog.FSharp.Tests.BuilderTests

open Xunit
open Yog.Model
open Yog.Builder

// =============================================================================
// GRID BUILDER TESTS
// =============================================================================

module GridBuilderTests =
    [<Fact>]
    let ``Grid.coordToId - row major ordering`` () =
        // 3x3 grid: (row, col) -> id
        Assert.Equal(0, Grid.coordToId 0 0 3)
        Assert.Equal(1, Grid.coordToId 0 1 3)
        Assert.Equal(3, Grid.coordToId 1 0 3)
        Assert.Equal(8, Grid.coordToId 2 2 3)

    [<Fact>]
    let ``Grid.idToCoord - inverse of coordToId`` () =
        for r in 0..2 do
            for c in 0..2 do
                let id = Grid.coordToId r c 3
                let (r2, c2) = Grid.idToCoord id 3
                Assert.Equal(r, r2)
                Assert.Equal(c, c2)

    [<Fact>]
    let ``Grid.fromArray2D - creates grid graph`` () =
        let data = array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] // 2x3 grid

        let grid = Grid.fromArray2D data Directed Grid.rook (fun _ _ -> true)

        Assert.Equal(2, grid.Rows)
        Assert.Equal(3, grid.Cols)
        Assert.Equal(6, nodeCount grid.Graph)

    [<Fact>]
    let ``Grid.fromArray2D - rook topology`` () =
        let data = array2D [ [ 1; 2 ]; [ 3; 4 ] ] // 2x2 grid

        let grid = Grid.fromArray2D data Directed Grid.rook (fun _ _ -> true)

        // Each cell should have outgoing edges to valid neighbors
        let centerSuccs = successors 3 grid.Graph
        Assert.True(centerSuccs.Length >= 2)

    [<Fact>]
    let ``Grid.fromArray2D - respects canMove predicate`` () =
        let data = array2D [ [ 1; 2 ]; [ 3; 4 ] ]

        // Only allow movement to cells with value > 2
        let canMove fromVal toVal = toVal > 2
        let grid = Grid.fromArray2D data Directed Grid.rook canMove

        // From cell 0 (value 1), can only move to cells with value > 2
        // Those are: cell 2 (value 3) and cell 3 (value 4)
        let succs = successors 0 grid.Graph
        // Should only have valid moves
        for (id, _) in succs do
            let cellValue = if id < 4 then data.[id / 2, id % 2] else 0

            Assert.True(cellValue > 2 || cellValue = 0)

    [<Fact>]
    let ``Grid.manhattanDistance - correct calculation`` () =
        let dist = Grid.manhattanDistance 0 8 3 // (0,0) to (2,2) in 3x3 grid
        Assert.Equal(4, dist) // |0-2| + |0-2| = 4

    [<Fact>]
    let ``Grid.manhattanDistance - same point`` () =
        let dist = Grid.manhattanDistance 4 4 3
        Assert.Equal(0, dist)

    [<Fact>]
    let ``Grid.avoiding - predicate helper`` () =
        let pred = Grid.avoiding 0 // Avoid value 0

        Assert.True(pred 1 1) // 1 -> 1 is fine
        Assert.True(pred 1 2) // 1 -> 2 is fine
        Assert.False(pred 1 0) // 1 -> 0 (wall) not allowed
        Assert.False(pred 0 1) // 0 (wall) -> 1 not allowed

    [<Fact>]
    let ``Grid.walkable - predicate helper`` () =
        let pred = Grid.walkable 1 // Only walk on value 1

        Assert.True(pred 1 1) // Both are 1
        Assert.False(pred 1 2) // 2 is not 1
        Assert.False(pred 2 1) // 2 is not 1

    [<Fact>]
    let ``Grid.always - allows all moves`` () =
        Assert.True(Grid.always 0 0)
        Assert.True(Grid.always 1 2)
        Assert.True(Grid.always -1 100)

    [<Fact>]
    let ``Grid.getCell - retrieves cell data`` () =
        let data = array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
        let grid = Grid.fromArray2D data Directed Grid.rook Grid.always

        let cell = Grid.getCell 1 1 grid
        Assert.True(cell.IsSome)
        Assert.Equal(5, cell.Value)

    [<Fact>]
    let ``Grid.getCell - out of bounds returns None`` () =
        let data = array2D [ [ 1; 2 ]; [ 3; 4 ] ]
        let grid = Grid.fromArray2D data Directed Grid.rook Grid.always

        Assert.True((Grid.getCell 5 5 grid).IsNone)
        Assert.True((Grid.getCell -1 0 grid).IsNone)

    [<Fact>]
    let ``Grid.toGraph - extracts graph`` () =
        let data = array2D [ [ 1; 2 ]; [ 3; 4 ] ]
        let grid = Grid.fromArray2D data Directed Grid.rook Grid.always
        let graph = Grid.toGraph grid

        Assert.Equal(4, nodeCount graph)

    [<Fact>]
    let ``Grid topologies - rook has 4 directions`` () = Assert.Equal(4, Grid.rook.Length)

    [<Fact>]
    let ``Grid topologies - bishop has 4 directions`` () = Assert.Equal(4, Grid.bishop.Length)

    [<Fact>]
    let ``Grid topologies - queen has 8 directions`` () = Assert.Equal(8, Grid.queen.Length)

    [<Fact>]
    let ``Grid topologies - knight has 8 directions`` () = Assert.Equal(8, Grid.knight.Length)

// =============================================================================
// LABELED BUILDER BASIC TESTS (simplified due to type inference)
// =============================================================================

module LabeledBuilderBasicTests =
    [<Fact>]
    let ``Labeled.directed - creates directed builder`` () =
        let builder = Labeled.directed<string, int> ()
        Assert.Equal(Directed, builder.Graph.Kind)

    [<Fact>]
    let ``Labeled.undirected - creates undirected builder`` () =
        let builder = Labeled.undirected<string, int> ()
        Assert.Equal(Undirected, builder.Graph.Kind)

    [<Fact>]
    let ``Labeled.toGraph - produces graph with correct node count`` () =
        // Create builder using directed/undirected functions
        let builder = Labeled.directed<string, int> ()

        // Use fromList to add edges
        let edges = [ ("A", "B", 1); ("B", "C", 2) ]
        let builder2 = Labeled.fromList Directed edges

        let graph = Labeled.toGraph builder2
        Assert.True(nodeCount graph >= 2)

    [<Fact>]
    let ``Labeled.addNode - adds a node explicitly`` () =
        let builder = Labeled.directed<string, int> () |> Labeled.addNode "A"

        let id = Labeled.getId "A" builder
        Assert.True(id.IsSome)

    [<Fact>]
    let ``Labeled.addSimpleEdge - adds edge with weight 1`` () =
        let builder = Labeled.directed<string, int> () |> Labeled.addEdge "A" "B" 1

        let succs = Labeled.successors "A" builder
        Assert.True(succs.IsSome)

    [<Fact>]
    let ``Labeled.allLabels - returns all labels`` () =
        let b0: LabeledBuilder<string, int> = Labeled.directed<string, int> ()
        let b1: LabeledBuilder<string, int> = Labeled.addEdge "A" "B" 1 b0
        let b2: LabeledBuilder<string, int> = Labeled.addNode "C" b1
        let labels: string list = Labeled.allLabels b2
        Assert.Equal(3, labels.Length)
        Assert.True(List.contains "A" labels)
        Assert.True(List.contains "B" labels)
        Assert.True(List.contains "C" labels)

    [<Fact>]
    let ``Labeled.predecessors - gets predecessors`` () =
        let b0: LabeledBuilder<string, int> = Labeled.directed<string, int> ()
        let builder: LabeledBuilder<string, int> = Labeled.addEdge "A" "B" 1 b0
        let preds = Labeled.predecessors "B" builder
        Assert.True(preds.IsSome)

// =============================================================================
// LIVE BUILDER BASIC TESTS (simplified)
// =============================================================================

module LiveBuilderBasicTests =
    [<Fact>]
    let ``Live.create - creates empty builder`` () =
        let builder = Live.create<string, int> ()
        Assert.Equal(0, builder.Registry.Count)

    [<Fact>]
    let ``Live.sync - applies pending changes`` () =
        let builder = Live.create<string, int> () |> Live.addEdge "A" "B" 10

        let builder2, graph = Live.sync builder (empty Directed)

        Assert.Equal(0, Live.pendingCount builder2)
        Assert.True(nodeCount graph >= 0)

    [<Fact>]
    let ``Live.addSimpleEdge, removeEdge, removeNode - handles mutations`` () =
        let builder =
            Live.create<string, int> ()
            |> Live.addEdge "A" "B" 1 // queues: AddNode A, AddNode B, AddEdge
            |> Live.addEdge "B" "C" 1 // queues: AddNode C, AddEdge (B already known)
            |> Live.removeEdge "A" "B" // queues: RemoveEdge
            |> Live.removeNode "C" // queues: RemoveNode
        // 3 (from first addEdge) + 2 (from second: A+B already registered, only C+edge) + 1 + 1 = 7
        Assert.Equal(7, Live.pendingCount builder)

    [<Fact>]
    let ``Live.purgePending - clears queue`` () =
        let builder = Live.create<string, int> () |> Live.addEdge "A" "B" 1

        Assert.Equal(3, Live.pendingCount builder) // AddNode A, AddNode B, AddEdge
        let purged = Live.purgePending builder
        Assert.Equal(0, Live.pendingCount purged)

    [<Fact>]
    let ``Live.getId, allLabels - works with pending but un-synced state since registry updates sync-free`` () =
        let builder = Live.create<string, int> () |> Live.addEdge "A" "B" 1

        Assert.True((Live.getId "A" builder).IsSome)
        let labels = Live.allLabels builder
        Assert.Contains("A", labels)
        Assert.Contains("B", labels)

    [<Fact>]
    let ``Live.fromLabeled - migrates from labeled builder`` () =
        let b0: LabeledBuilder<string, int> = Labeled.directed<string, int> ()
        let labeled: LabeledBuilder<string, int> = Labeled.addEdge "A" "B" 1 b0
        let live: LiveBuilder<string, int> = Live.fromLabeled labeled
        Assert.Equal(0, Live.pendingCount live)
        Assert.True((Live.getId "A" live).IsSome)
