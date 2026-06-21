namespace Yog.Render

open System.Text
open Yog.Model
open Yog.Builder

module Ascii =

    let private coordToId row col cols = row * cols + col

    let private hasEdge (graph: Graph<'CellData, 'EdgeData>) from toNode =
        match Map.tryFind from graph.OutEdges with
        | None -> false
        | Some targets -> Map.containsKey toNode targets

    let private hasPassage (graph: Graph<'CellData, 'EdgeData>) from toNode =
        hasEdge graph from toNode || hasEdge graph toNode from

    let private verticalWall (graph: Graph<'CellData, 'EdgeData>) rows cols r c =
        if c = 0 || c = cols then true
        else not (hasPassage graph (coordToId r (c - 1) cols) (coordToId r c cols))

    let private horizontalWall (graph: Graph<'CellData, 'EdgeData>) rows cols r c =
        if r = 0 || r = rows then true
        else not (hasPassage graph (coordToId (r - 1) c cols) (coordToId r c cols))

    let private drawTopBorder cols =
        let sb = StringBuilder()
        sb.Append("+") |> ignore
        for _ in 0 .. cols - 1 do
            sb.Append("---+") |> ignore
        sb.ToString()

    let private drawCellRow (graph: Graph<'CellData, 'EdgeData>) rows cols row (occupants: Map<NodeId, string>) =
        let sb = StringBuilder()
        sb.Append("|") |> ignore
        for col in 0 .. cols - 1 do
            let cellId = coordToId row col cols
            let rightId = coordToId row (col + 1) cols
            let content = Map.tryFind cellId occupants |> Option.defaultValue " "
            let cellText = $" {content} "
            if col < cols - 1 && hasPassage graph cellId rightId then
                sb.Append(cellText).Append(" ") |> ignore
            else
                sb.Append(cellText).Append("|") |> ignore
        sb.ToString()

    let private drawHorizontalWalls (graph: Graph<'CellData, 'EdgeData>) rows cols row =
        let sb = StringBuilder()
        sb.Append("+") |> ignore
        for col in 0 .. cols - 1 do
            let cellId = coordToId row col cols
            let belowId = coordToId (row + 1) col cols
            if row < rows - 1 && hasPassage graph cellId belowId then
                sb.Append("   +") |> ignore
            else
                sb.Append("---+") |> ignore
        sb.ToString()

    /// Converts a grid to ASCII art using simple characters (+, -, |).
    let gridToString (grid: Grid<'CellData, 'EdgeData>) (occupants: Map<NodeId, string>) : string =
        if grid.Rows = 0 || grid.Cols = 0 then ""
        else
            let sb = StringBuilder()
            sb.AppendLine(drawTopBorder grid.Cols) |> ignore
            for row in 0 .. grid.Rows - 1 do
                sb.AppendLine(drawCellRow grid.Graph grid.Rows grid.Cols row occupants) |> ignore
                sb.AppendLine(drawHorizontalWalls grid.Graph grid.Rows grid.Cols row) |> ignore
            sb.ToString().TrimEnd('\r', '\n')

    let private getUnicodeIntersection (graph: Graph<'CellData, 'EdgeData>) rows cols iR iC =
        let up = iR > 0 && verticalWall graph rows cols (iR - 1) iC
        let down = iR < rows && verticalWall graph rows cols iR iC
        let left = iC > 0 && horizontalWall graph rows cols iR (iC - 1)
        let right = iC < cols && horizontalWall graph rows cols iR iC
        match up, down, left, right with
        | false, false, false, false -> " "
        | false, false, true, true -> "─"
        | false, false, true, false -> "─"
        | false, false, false, true -> "─"
        | true, true, false, false -> "│"
        | true, false, false, false -> "│"
        | false, true, false, false -> "│"
        | false, true, false, true -> "┌"
        | false, true, true, false -> "┐"
        | true, false, false, true -> "└"
        | true, false, true, false -> "┘"
        | false, true, true, true -> "┬"
        | true, false, true, true -> "┴"
        | true, true, false, true -> "├"
        | true, true, true, false -> "┤"
        | true, true, true, true -> "┼"

    let private drawUnicodeIntersectionRow (graph: Graph<'CellData, 'EdgeData>) rows cols iR =
        let sb = StringBuilder()
        for iC in 0 .. cols do
            let intersection = getUnicodeIntersection graph rows cols iR iC
            sb.Append(intersection) |> ignore
            if iC < cols then
                if horizontalWall graph rows cols iR iC then
                    sb.Append("───") |> ignore
                else
                    sb.Append("   ") |> ignore
        sb.ToString()

    let private drawUnicodeCellRow (graph: Graph<'CellData, 'EdgeData>) rows cols r (occupants: Map<NodeId, string>) =
        let sb = StringBuilder()
        for c in 0 .. cols do
            let wall = if verticalWall graph rows cols r c then "│" else " "
            sb.Append(wall) |> ignore
            if c < cols then
                let cellId = coordToId r c cols
                let content = Map.tryFind cellId occupants |> Option.defaultValue " "
                sb.Append($" {content} ") |> ignore
        sb.ToString()

    /// Converts a grid to ASCII art using Unicode box-drawing characters.
    let gridToStringUnicode (grid: Grid<'CellData, 'EdgeData>) (occupants: Map<NodeId, string>) : string =
        if grid.Rows = 0 || grid.Cols = 0 then ""
        else
            let sb = StringBuilder()
            for r in 0 .. grid.Rows do
                sb.AppendLine(drawUnicodeIntersectionRow grid.Graph grid.Rows grid.Cols r) |> ignore
                if r < grid.Rows then
                    sb.AppendLine(drawUnicodeCellRow grid.Graph grid.Rows grid.Cols r occupants) |> ignore
            sb.ToString().TrimEnd('\r', '\n')

    let private addToroidalHints (ascii: string) rows cols =
        let lines = ascii.Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
        let topArrows = "  " + String.concat "   " (Seq.init cols (fun _ -> "v"))
        let bodyWithSides =
            lines
            |> Array.mapi (fun idx line ->
                if idx % 2 = 1 then $"> {line} <"
                else $"  {line}"
            )
            |> String.concat "\n"
        let bottomArrows = "  " + String.concat "   " (Seq.init cols (fun _ -> "^"))
        $"{topArrows}\n{bodyWithSides}\n{bottomArrows}"

    let private addToroidalHintsUnicode (unicode: string) rows cols =
        let lines = unicode.Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
        let topArrows = "  " + String.concat "   " (Seq.init cols (fun _ -> "v"))
        let bodyWithSides =
            lines
            |> Array.mapi (fun idx line ->
                if idx % 2 = 1 then $"> {line} <"
                else $"  {line}"
            )
            |> String.concat "\n"
        let bottomArrows = "  " + String.concat "   " (Seq.init cols (fun _ -> "ʌ"))
        $"{topArrows}\n{bodyWithSides}\n{bottomArrows}"

    /// Converts a toroidal grid to ASCII art.
    let toroidalToString (grid: ToroidalGrid<'CellData, 'EdgeData>) (occupants: Map<NodeId, string>) : string =
        let baseGrid = Toroidal.toGrid grid
        let baseAscii = gridToString baseGrid occupants
        addToroidalHints baseAscii grid.Rows grid.Cols

    /// Converts a toroidal grid to ASCII art using Unicode box-drawing characters.
    let toroidalToStringUnicode (grid: ToroidalGrid<'CellData, 'EdgeData>) (occupants: Map<NodeId, string>) : string =
        let baseGrid = Toroidal.toGrid grid
        let baseUnicode = gridToStringUnicode baseGrid occupants
        addToroidalHintsUnicode baseUnicode grid.Rows grid.Cols
