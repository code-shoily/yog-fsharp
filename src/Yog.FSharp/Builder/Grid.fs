namespace Yog.Builder

open Yog.Model

/// A grid builder that wraps a graph and maintains grid dimensions.
///
/// The grid uses row-major ordering: node_id = row × cols + col
///
/// ## Type Parameters
/// - 'CellData: The data stored in each grid cell (becomes node data)
/// - 'EdgeData: The data stored on edges (typically int for weights)
type Grid<'CellData, 'EdgeData> =
    {
        /// The underlying graph structure
        Graph: Graph<'CellData, 'EdgeData>
        /// Number of rows in the grid
        Rows: int
        /// Number of columns in the grid
        Cols: int
    }

/// A builder for creating graphs from 2D grids.
///
/// This module provides convenient ways to convert 2D grids (like heightmaps,
/// mazes, or game boards) into graphs for pathfinding and traversal algorithms.
///
/// ## Key Concepts
///
/// - **Row-Major Ordering**: Node ID = row × cols + col
/// - **Topology**: Movement patterns (rook/bishop/queen/knight)
/// - **Predicate**: Function that determines if movement is allowed
///
/// ## Movement Topologies
///
/// The module provides 4 chess-inspired movement patterns:
///
/// **Rook (4-way cardinal):**
///
/// . ↑ .
/// ← · →
/// . ↓ .
///
/// **Bishop (4-way diagonal):**
///
/// ↖ . ↗
/// . · .
/// ↙ . ↘
///
/// **Queen (8-way):**
///
/// ↖ ↑ ↗
/// ← · →
/// ↙ ↓ ↘
///
/// **Knight (L-shaped):**
///
/// . ♞ . ♞ .
/// ♞ . . . ♞
/// . . · . .
/// ♞ . . . ♞
/// . ♞ . ♞ .
///
/// ## Example Usage
///
///     open Yog.Builder
///
///     // A simple heightmap where you can only climb up by 1
///     let heightmap = array2D [[1; 2; 3]
///                              [4; 5; 6]
///                              [7; 8; 9]]
///
///     // Build a graph with rook movement, can move if height diff <= 1
///     let grid = Grid.fromArray2D heightmap Directed Grid.rook
///         (fun fromHeight toHeight -> toHeight - fromHeight <= 1)
///
///     // Convert to graph and use with algorithms
///     let graph = Grid.toGraph grid
///     let start = Grid.coordToId 0 0 grid.Cols  // Top-left
///     let goal = Grid.coordToId 2 2 grid.Cols   // Bottom-right
///
///     // Use with pathfinding
///     Yog.Pathfinding.AStar.aStarInt
///         (fun from to' -> Grid.manhattanDistance from to' grid.Cols)
///         start goal graph
///
/// ## Use Cases
///
/// - **Game pathfinding**: Character movement on tile-based maps
/// - **Maze solving**: Finding paths through grid-based mazes
/// - **Heightmap navigation**: Terrain traversal with elevation constraints
/// - **Puzzle solving**: Grid-based puzzles like Sokoban, sliding puzzles
module Grid =

    /// Cardinal (4-way) movement: up, down, left, right.
    ///
    /// Named after the rook in chess, which moves along ranks and files.
    ///
    /// . ↑ .
    /// ← · →
    /// . ↓ .
    ///
    let rook = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

    /// Diagonal (4-way) movement: the four diagonal directions.
    ///
    /// Named after the bishop in chess, which moves along diagonals.
    ///
    /// ↖ . ↗
    /// . · .
    /// ↙ . ↘
    ///
    let bishop = [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]

    /// All 8 surrounding directions: cardinal + diagonal.
    ///
    /// Named after the queen in chess, which combines rook and bishop movement.
    ///
    /// ↖ ↑ ↗
    /// ← · →
    /// ↙ ↓ ↘
    ///
    let queen = [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

    /// L-shaped jumps in all 8 orientations.
    ///
    /// Named after the knight in chess, which jumps in an L-shape
    /// (2 squares in one direction, 1 square perpendicular).
    ///
    /// . ♞ . ♞ .
    /// ♞ . . . ♞
    /// . . · . .
    /// ♞ . . . ♞
    /// . ♞ . ♞ .
    ///
    let knight =
        [ (-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1) ]

    /// Converts grid coordinates (row, col) to a node ID.
    ///
    /// Uses row-major ordering: id = row × cols + col
    ///
    /// ## Example
    ///
    ///     coordToId 0 0 3  // => 0
    ///     coordToId 1 2 3  // => 5
    ///     coordToId 2 1 3  // => 7
    ///
    let coordToId row col cols = row * cols + col

    /// Converts a node ID back to grid coordinates (row, col).
    ///
    /// ## Example
    ///
    ///     idToCoord 0 3  // => (0, 0)
    ///     idToCoord 5 3  // => (1, 2)
    ///     idToCoord 7 3  // => (2, 1)
    ///
    let idToCoord id cols = (id / cols, id % cols)

    /// Builds a grid-graph from a 2D array and a custom movement topology.
    ///
    /// Each cell becomes a node, and edges are added between cells based on the
    /// topology and the canMove predicate.
    ///
    /// ## Parameters
    /// - gridData: 2D array of cell data
    /// - kind: Directed or Undirected
    /// - topology: List of (row_delta, col_delta) offsets (e.g., rook, queen)
    /// - canMove: Predicate function (fromData -> toData -> bool)
    ///
    /// ## Returns
    /// Grid with the underlying graph and dimensions
    ///
    /// ## Time Complexity
    /// O(rows × cols × |topology|)
    ///
    /// ## Example
    ///
    ///     // 8-way movement on a maze
    ///     let maze = array2D [[".";"#";"."]
    ///                         [".";".";"."]
    ///                         ["#";".";"."] ]
    ///
    ///     let grid = fromArray2D maze Undirected queen (avoiding "#")
    ///
    let fromArray2D (gridData: 'n[,]) kind topology canMove =
        let rows = Array2D.length1 gridData
        let cols = Array2D.length2 gridData
        let mutable g = empty kind

        // 1. Add all nodes
        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                let id = coordToId r c cols
                g <- addNode id gridData.[r, c] g

        // 2. Add edges based on topology
        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                let fromId = coordToId r c cols
                let fromData = gridData.[r, c]

                for (dr, dc) in topology do
                    let nr, nc = r + dr, c + dc

                    if nr >= 0 && nr < rows && nc >= 0 && nc < cols then
                        let toId = coordToId nr nc cols
                        let toData = gridData.[nr, nc]

                        if canMove fromData toData then
                            g <- addEdge fromId toId 1 g

        { Graph = g; Rows = rows; Cols = cols }

    /// Calculates the Manhattan distance between two node IDs.
    ///
    /// This is useful as a heuristic for A* pathfinding on grids.
    /// Manhattan distance is the sum of absolute differences in coordinates:
    /// |x1 - x2| + |y1 - y2|
    ///
    /// ## Example
    ///
    ///     let start = coordToId 0 0 10
    ///     let goal = coordToId 3 4 10
    ///     let distance = manhattanDistance start goal 10
    ///     // => 7 (3 + 4)
    ///
    let manhattanDistance fromId toId cols =
        let (r1, c1) = idToCoord fromId cols
        let (r2, c2) = idToCoord toId cols
        abs (r1 - r2) + abs (c1 - c2)

    /// Calculates the Chebyshev distance between two node IDs.
    let chebyshevDistance fromId toId cols =
        let (r1, c1) = idToCoord fromId cols
        let (r2, c2) = idToCoord toId cols
        max (abs (r1 - r2)) (abs (c1 - c2))

    /// Calculates the Octile distance between two node IDs.
    let octileDistance fromId toId cols =
        let (r1, c1) = idToCoord fromId cols
        let (r2, c2) = idToCoord toId cols
        let dx = abs (r1 - r2)
        let dy = abs (c1 - c2)
        let minD = min dx dy
        let maxD = max dx dy
        float minD * 1.414213562373095 + float (maxD - minD)

    /// Allows movement between any cells except the specified wall value.
    let avoiding wallValue =
        fun from to' -> from <> wallValue && to' <> wallValue

    /// Allows movement only between cells matching the specified value.
    let walkable validValue =
        fun from to' -> from = validValue && to' = validValue

    /// Always allows movement between adjacent cells.
    let always = fun _ _ -> true

    /// Allows movement only between cells matching any of the specified values.
    let including (validValues: 'CellData list) =
        let validSet = Set.ofList validValues
        fun from to' -> Set.contains from validSet && Set.contains to' validSet

    /// Helper to convert F# list of lists to Array2D.
    let private array2DFromList (lst: list<list<'n>>) =
        let rows = lst.Length
        let cols = if rows = 0 then 0 else lst.[0].Length
        if cols = 0 then Array2D.zeroCreate 0 0
        else
            let arr = Array2D.zeroCreate rows cols
            lst |> List.iteri (fun r row ->
                row |> List.iteri (fun c item ->
                    arr.[r, c] <- item
                )
            )
            arr

    /// Creates a grid-graph from a 2D list using 4-directional (rook) movement.
    let from2DList (gridData: list<list<'n>>) kind canMove =
        let data = array2DFromList gridData
        fromArray2D data kind rook canMove

    /// Creates a grid-graph from a 2D list using custom movement topology.
    let from2DListWithTopology (gridData: list<list<'n>>) kind topology canMove =
        let data = array2DFromList gridData
        fromArray2D data kind topology canMove

    /// Finds a node in the grid where the cell data matches a predicate.
    let findNode predicate grid =
        let maxId = grid.Rows * grid.Cols - 1
        seq { 0 .. maxId }
        |> Seq.tryPick (fun id ->
            match Map.tryFind id grid.Graph.Nodes with
            | Some data when predicate data -> Some id
            | _ -> None
        )

    /// Converts the grid to a standard Graph.
    let toGraph grid = grid.Graph

    /// Gets the cell data at the specified grid coordinate.
    let getCell row col grid =
        if row >= 0 && row < grid.Rows && col >= 0 && col < grid.Cols then
            Some(Map.find (coordToId row col grid.Cols) grid.Graph.Nodes)
        else
            None
