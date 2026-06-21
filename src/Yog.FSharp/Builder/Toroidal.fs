namespace Yog.Builder

open Yog.Model

/// A toroidal grid where edges wrap around to opposite sides.
type ToroidalGrid<'CellData, 'EdgeData> =
    {
        /// The underlying graph structure.
        Graph: Graph<'CellData, 'EdgeData>
        /// Number of rows in the grid.
        Rows: int
        /// Number of columns in the grid.
        Cols: int
    }

/// Toroidal grid builder for creating grids with wrapping boundaries.
module Toroidal =

    // Topology presets
    let rook = Grid.rook
    let bishop = Grid.bishop
    let queen = Grid.queen
    let knight = Grid.knight

    // Coordinate conversion
    let coordToId = Grid.coordToId
    let idToCoord = Grid.idToCoord

    // Movement predicates
    let avoiding = Grid.avoiding
    let walkable = Grid.walkable
    let always = Grid.always
    let including = Grid.including

    /// Wraps a coordinate to stay within bounds [0, size).
    let private wrapCoordinate coord size =
        let n = coord % size
        if n < 0 then n + size else n

    /// Creates a toroidal grid-graph from a 2D list using cardinal (rook) movement.
    let from2DList (gridData: list<list<'CellData>>) (graphType: GraphType) canMove =
        let rows = gridData.Length
        let cols = if rows = 0 then 0 else gridData.[0].Length
        
        let cells =
            gridData
            |> List.mapi (fun r row ->
                row |> List.mapi (fun c cell -> (r, c, cell))
            )
            |> List.concat

        let graphWithNodes =
            (empty graphType, cells)
            ||> List.fold (fun g (row, col, data) ->
                let id = coordToId row col cols
                addNode id data g
            )

        let graphWithEdges =
            (graphWithNodes, cells)
            ||> List.fold (fun g (row, col, fromData) ->
                let fromId = coordToId row col cols

                (g, rook)
                ||> List.fold (fun accG (dr, dc) ->
                    let nRow = wrapCoordinate (row + dr) rows
                    let nCol = wrapCoordinate (col + dc) cols
                    let toId = coordToId nRow nCol cols
                    let toData = Map.find toId graphWithNodes.Nodes

                    if canMove fromData toData then
                        addEdge fromId toId 1 accG
                    else
                        accG
                )
            )

        { Graph = graphWithEdges; Rows = rows; Cols = cols }

    /// Creates a toroidal grid-graph from a 2D list using custom movement topology.
    let from2DListWithTopology (gridData: list<list<'CellData>>) (graphType: GraphType) (topology: list<int * int>) canMove =
        let rows = gridData.Length
        let cols = if rows = 0 then 0 else gridData.[0].Length
        
        let cells =
            gridData
            |> List.mapi (fun r row ->
                row |> List.mapi (fun c cell -> (r, c, cell))
            )
            |> List.concat

        let graphWithNodes =
            (empty graphType, cells)
            ||> List.fold (fun g (row, col, data) ->
                let id = coordToId row col cols
                addNode id data g
            )

        let graphWithEdges =
            (graphWithNodes, cells)
            ||> List.fold (fun g (row, col, fromData) ->
                let fromId = coordToId row col cols

                (g, topology)
                ||> List.fold (fun accG (dr, dc) ->
                    let nRow = wrapCoordinate (row + dr) rows
                    let nCol = wrapCoordinate (col + dc) cols
                    let toId = coordToId nRow nCol cols
                    let toData = Map.find toId graphWithNodes.Nodes

                    if canMove fromData toData then
                        addEdge fromId toId 1 accG
                    else
                        accG
                )
            )

        { Graph = graphWithEdges; Rows = rows; Cols = cols }

    /// Creates a toroidal grid-graph from a 2D Array using custom movement topology.
    let fromArray2D (gridData: 'CellData[,]) (graphType: GraphType) (topology: list<int * int>) canMove =
        let rows = Array2D.length1 gridData
        let cols = Array2D.length2 gridData
        
        let mutable g = empty graphType
        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                let id = coordToId r c cols
                g <- addNode id gridData.[r, c] g

        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                let fromId = coordToId r c cols
                let fromData = gridData.[r, c]

                for (dr, dc) in topology do
                    let nRow = wrapCoordinate (r + dr) rows
                    let nCol = wrapCoordinate (c + dc) cols
                    let toId = coordToId nRow nCol cols
                    let toData = gridData.[nRow, nCol]

                    if canMove fromData toData then
                        g <- addEdge fromId toId 1 g

        { Graph = g; Rows = rows; Cols = cols }

    // Distance functions
    
    /// Calculates the Manhattan distance on a toroidal grid.
    let toroidalManhattanDistance fromId toId cols rows =
        let (r1, c1) = idToCoord fromId cols
        let (r2, c2) = idToCoord toId cols
        let rowDiff = abs (r1 - r2)
        let colDiff = abs (c1 - c2)
        let minRowDist = min rowDiff (rows - rowDiff)
        let minColDist = min colDiff (cols - colDiff)
        minRowDist + minColDist

    /// Calculates the Chebyshev distance on a toroidal grid.
    let toroidalChebyshevDistance fromId toId cols rows =
        let (r1, c1) = idToCoord fromId cols
        let (r2, c2) = idToCoord toId cols
        let rowDiff = abs (r1 - r2)
        let colDiff = abs (c1 - c2)
        let minRowDist = min rowDiff (rows - rowDiff)
        let minColDist = min colDiff (cols - colDiff)
        max minRowDist minColDist

    /// Calculates the Octile distance on a toroidal grid.
    let toroidalOctileDistance fromId toId cols rows =
        let (r1, c1) = idToCoord fromId cols
        let (r2, c2) = idToCoord toId cols
        let rowDiff = abs (r1 - r2)
        let colDiff = abs (c1 - c2)
        let minRowDist = min rowDiff (rows - rowDiff)
        let minColDist = min colDiff (cols - colDiff)
        let minD = min minRowDist minColDist
        let maxD = max minRowDist minColDist
        float minD * 1.414213562373095 + float (maxD - minD)

    // Conversions
    
    /// Converts the toroidal grid to a standard Graph.
    let toGraph grid = grid.Graph

    /// Converts the toroidal grid to a standard Grid.
    let toGrid grid : Grid<'CellData, 'EdgeData> =
        { Graph = grid.Graph; Rows = grid.Rows; Cols = grid.Cols }

    /// Gets the number of rows in the toroidal grid.
    let rows grid = grid.Rows

    /// Gets the number of columns in the toroidal grid.
    let cols grid = grid.Cols

    /// Gets the cell data at the specified grid coordinate.
    let getCell row col grid =
        if row >= 0 && row < grid.Rows && col >= 0 && col < grid.Cols then
            Some(Map.find (coordToId row col grid.Cols) grid.Graph.Nodes)
        else
            None

    /// Finds a node in the grid where the cell data matches a predicate.
    let findNode predicate grid =
        let maxId = grid.Rows * grid.Cols - 1
        seq { 0 .. maxId }
        |> Seq.tryPick (fun id ->
            match Map.tryFind id grid.Graph.Nodes with
            | Some data when predicate data -> Some id
            | _ -> None
        )
