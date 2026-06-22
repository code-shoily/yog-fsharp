/// Adjacency list serialization support.
module Yog.IO.List

open System
open Yog.Model

/// Options for Adjacency List serialization.
type ListOptions<'e> =
    { Weighted: bool
      Delimiter: string
      NodeFormatter: NodeId -> string
      WeightFormatter: 'e -> string }

/// Default options: unweighted, colon delimiter, standard string formatters.
let defaultOptions: ListOptions<'e> =
    { Weighted = false
      Delimiter = ":"
      NodeFormatter = (fun id -> id.ToString())
      WeightFormatter = (fun w -> w.ToString()) }

/// Creates a graph from an adjacency list structure.
let fromList (graphType: GraphType) (entries: seq<NodeId * #seq<NodeId * 'e>>) : Graph<unit, 'e> =
    let mutable graph = empty graphType

    let ensureNode id g =
        if g.Nodes |> Map.containsKey id then g else addNode id () g

    // First pass: add all nodes declared as keys
    for (nodeId, _) in entries do
        graph <- ensureNode nodeId graph

    // Second pass: add all edges
    for (nodeId, neighbors) in entries do
        for (neighborId, weight) in neighbors do
            graph <- ensureNode neighborId graph
            graph <- addEdge nodeId neighborId weight graph

    graph

/// Exports a graph to an adjacency list structure.
let toList (graph: Graph<'n, 'e>) : (NodeId * (NodeId * 'e) list) list =
    graph.Nodes
    |> Map.toList
    |> List.map (fun (nodeId, _) ->
        let neighbors = successors nodeId graph |> List.sortBy fst
        (nodeId, neighbors))
    |> List.sortBy fst

/// Parses a string representation of an adjacency list.
let fromString
    (graphType: GraphType)
    (weighted: bool)
    (delimiter: string)
    (input: string)
    : Result<Graph<unit, float>, string> =

    let lines = input.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    let mutable entries = []
    let mutable possible = true
    let mutable errorMsg = ""

    let parseId (str: string) =
        match Int32.TryParse(str.Trim()) with
        | true, id -> Ok id
        | false, _ -> Error(sprintf "Invalid node ID: %s" str)

    let parseNeighbors (str: string) =
        let parts = str.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
        let mutable nbrs = []
        let mutable err = None

        for part in parts do
            if err.IsNone then
                if weighted then
                    let edgeParts = part.Split([| ',' |], 2, StringSplitOptions.None)

                    match edgeParts with
                    | [| destStr; wStr |] ->
                        match parseId destStr, Double.TryParse(wStr.Trim()) with
                        | Ok dest, (true, w) -> nbrs <- (dest, w) :: nbrs
                        | Error e, _ -> err <- Some e
                        | _, (false, _) -> err <- Some(sprintf "Invalid weight: %s" wStr)
                    | [| destStr |] ->
                        match parseId destStr with
                        | Ok dest -> nbrs <- (dest, 1.0) :: nbrs
                        | Error e -> err <- Some e
                    | _ -> err <- Some(sprintf "Invalid neighbor format: %s" part)
                else
                    match parseId part with
                    | Ok dest -> nbrs <- (dest, 1.0) :: nbrs
                    | Error e -> err <- Some e

        match err with
        | Some e -> Error e
        | None -> Ok(List.rev nbrs)

    for line in lines do
        let trimmed = line.Trim()

        if trimmed <> "" && not (trimmed.StartsWith("#")) && possible then
            let parts = trimmed.Split([| delimiter |], 2, StringSplitOptions.None)

            match parts with
            | [| nodeStr |] ->
                match parseId nodeStr with
                | Ok id -> entries <- (id, []) :: entries
                | Error e ->
                    possible <- false
                    errorMsg <- e
            | [| nodeStr; neighborsStr |] ->
                match parseId nodeStr, parseNeighbors neighborsStr with
                | Ok id, Ok nbrs -> entries <- (id, nbrs) :: entries
                | Error e, _ ->
                    possible <- false
                    errorMsg <- e
                | _, Error e ->
                    possible <- false
                    errorMsg <- e
            | _ -> ()

    if possible then
        let mapped =
            entries |> List.rev |> List.map (fun (id, nbrs) -> (id, nbrs |> List.toSeq))

        Ok(fromList graphType mapped)
    else
        Error errorMsg

/// Legacy alias for fromString.
let parse graphType weighted delimiter input =
    fromString graphType weighted delimiter input

/// Exports a graph to a string representation of an adjacency list.
let toString (options: ListOptions<'e>) (graph: Graph<'n, 'e>) : string =
    let sb = System.Text.StringBuilder()
    let entries = toList graph

    for (nodeId, neighbors) in entries do
        let nodeStr = options.NodeFormatter nodeId

        if neighbors.IsEmpty then
            sb.AppendLine(sprintf "%s%s" nodeStr options.Delimiter) |> ignore
        else
            let nbrStrs =
                neighbors
                |> List.map (fun (dest, w) ->
                    let destStr = options.NodeFormatter dest

                    if options.Weighted then
                        let weightStr = options.WeightFormatter w
                        sprintf "%s,%s" destStr weightStr
                    else
                        destStr)

            sb.AppendLine(sprintf "%s%s %s" nodeStr options.Delimiter (String.concat " " nbrStrs))
            |> ignore

    sb.ToString()

/// Legacy alias for toString.
let serialize (weighted: bool) (delimiter: string) (graph: Graph<'n, float>) : string =
    let options: ListOptions<float> =
        { Weighted = weighted
          Delimiter = delimiter
          NodeFormatter = (fun id -> id.ToString())
          WeightFormatter = (fun (w: float) -> w.ToString("F6", System.Globalization.CultureInfo.InvariantCulture)) }

    toString options graph
