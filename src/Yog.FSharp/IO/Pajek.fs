/// Pajek (.net) format serialization support.
module Yog.IO.Pajek

open System
open System.Text.RegularExpressions
open Yog.Model
open Yog.Transform

type PajekWarning =
    | InvalidNodeLine of string
    | MalformedEdge of string
    | EdgeAddFailed of string * string
    | NonexistentNodes of NodeId * NodeId

type PajekResult<'n, 'e> =
    { Graph: Graph<'n, 'e>
      Warnings: PajekWarning list }

type PajekError =
    | EmptyInput
    | InvalidVertexCount of string
    | InvalidVerticesLine of string
    | UnexpectedEndOfNodes
    | InvalidEdgeHeader of string

type PajekOptions<'n, 'e> =
    { NodeLabel: 'n -> string
      EdgeWeight: 'e -> string option
      NodeFormatter: NodeId -> string
      EdgeFormatter: string -> string }

let defaultOptions: PajekOptions<'n, 'e> =
    { NodeLabel = (fun data -> data.ToString())
      EdgeWeight = (fun _ -> None)
      NodeFormatter = (fun id -> id.ToString())
      EdgeFormatter = id }

let optionsWith (nodeLabel: 'n -> string) (edgeWeight: 'e -> string option) =
    { NodeLabel = nodeLabel
      EdgeWeight = edgeWeight
      NodeFormatter = (fun id -> id.ToString())
      EdgeFormatter = id }

let serializeWith (options: PajekOptions<'n, 'e>) (graph: Graph<'n, 'e>) : string =
    let typeKind = graph.Kind
    let nodesMap = graph.Nodes
    let nodeCount = order graph
    let verticesHeader = sprintf "*Vertices %d\n" nodeCount

    let nodeLines =
        nodesMap
        |> Map.toList
        |> List.sortBy fst
        |> List.map (fun (id, data) ->
            let label = options.NodeLabel data
            sprintf "%s \"%s\"" (options.NodeFormatter id) label)
        |> String.concat "\n"

    let edges = allEdges graph
    let edgeHeader = if typeKind = Directed then "*Arcs\n" else "*Edges\n"

    let edgeLines =
        edges
        |> List.map (fun (fromId, toId, weight) ->
            match options.EdgeWeight weight with
            | None -> sprintf "%s %s" (options.NodeFormatter fromId) (options.NodeFormatter toId)
            | Some w ->
                sprintf
                    "%s %s %s"
                    (options.NodeFormatter fromId)
                    (options.NodeFormatter toId)
                    (options.EdgeFormatter w))
        |> String.concat "\n"

    verticesHeader + nodeLines + "\n" + edgeHeader + edgeLines + "\n"

let serialize (graph: Graph<'n, 'e>) : string = serializeWith defaultOptions graph

let parseWith
    (input: string)
    (nodeParser: string -> 'n)
    (edgeParser: float option -> 'e)
    : Result<PajekResult<'n, 'e>, PajekError> =

    let trimmedInput = input.Trim()

    if trimmedInput = "" then
        Error EmptyInput
    else
        let rawLines =
            input.Split([| '\r'; '\n' |], StringSplitOptions.None) |> Array.toList

        let lines = rawLines |> List.filter (fun l -> not (l.Trim().StartsWith("%")))

        let rec parseVerticesHeader (linesList: string list) : Result<int * string list, PajekError> =
            match linesList with
            | [] -> Error EmptyInput
            | line :: rest ->
                let trimmed = line.Trim()

                if trimmed = "" then
                    parseVerticesHeader rest
                else
                    let m = Regex.Match(trimmed, @"^\*vertices\s+(\d+)", RegexOptions.IgnoreCase)

                    if m.Success then
                        match Int32.TryParse(m.Groups.[1].Value) with
                        | true, count -> Ok(count, rest)
                        | false, _ -> Error(InvalidVertexCount trimmed)
                    else
                        Error(InvalidVerticesLine trimmed)

        let rec parseNodes
            (linesList: string list)
            (nodeCount: int)
            (currentId: int)
            (g: Graph<'n, 'e>)
            (warnings: PajekWarning list)
            : Result<Graph<'n, 'e> * string list * PajekWarning list, PajekError> =
            if currentId > nodeCount then
                Ok(g, linesList, warnings)
            else
                match linesList with
                | [] -> Error UnexpectedEndOfNodes
                | line :: rest ->
                    let trimmed = line.Trim()

                    if trimmed = "" then
                        parseNodes rest nodeCount currentId g warnings
                    else
                        // Parse node line: id "label"
                        let parseNodeLine (lineStr: string) =
                            if lineStr.Contains("\"") then
                                let m = Regex.Match(lineStr, @"^([^\s]+)\s+""([^""]*)""")

                                if m.Success then
                                    match Int32.TryParse(m.Groups.[1].Value) with
                                    | true, id -> Ok(id, m.Groups.[2].Value)
                                    | false, _ -> Error(InvalidNodeLine lineStr)
                                else
                                    Error(InvalidNodeLine lineStr)
                            else
                                let parts = lineStr.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

                                match parts with
                                | [| idStr |] ->
                                    match Int32.TryParse(idStr) with
                                    | true, id -> Ok(id, id.ToString())
                                    | _ -> Error(InvalidNodeLine lineStr)
                                | _ ->
                                    if parts.Length >= 2 then
                                        match Int32.TryParse(parts.[0]) with
                                        | true, id -> Ok(id, parts.[1])
                                        | _ -> Error(InvalidNodeLine lineStr)
                                    else
                                        Error(InvalidNodeLine lineStr)

                        match parseNodeLine trimmed with
                        | Ok(id, label) ->
                            let updated = addNode id (nodeParser label) g
                            parseNodes rest nodeCount (currentId + 1) updated warnings
                        | Error _ -> parseNodes rest nodeCount (currentId + 1) g (InvalidNodeLine trimmed :: warnings)

        let rec parseEdgeHeader (linesList: string list) : Result<GraphType * string list, PajekError> =
            match linesList with
            | [] -> Ok(Directed, [])
            | line :: rest ->
                let trimmed = line.Trim()

                if trimmed = "" then
                    parseEdgeHeader rest
                else if Regex.IsMatch(trimmed, @"^\*arcs", RegexOptions.IgnoreCase) then
                    Ok(Directed, rest)
                elif Regex.IsMatch(trimmed, @"^\*edges", RegexOptions.IgnoreCase) then
                    Ok(Undirected, rest)
                else
                    // No edge header found, assume directed and process line as edge/content
                    Ok(Directed, linesList)

        match parseVerticesHeader lines with
        | Error e -> Error e
        | Ok(nodeCount, restNodeLines) ->
            let initialGraph = empty Directed

            match parseNodes restNodeLines nodeCount 1 initialGraph [] with
            | Error e -> Error e
            | Ok(graphWithNodes, edgeLines, nodeWarnings) ->
                match parseEdgeHeader edgeLines with
                | Error e -> Error e
                | Ok(graphType, finalEdgeLines) ->
                    let mutable graph =
                        if graphType = Undirected then
                            toUndirected (fun a _ -> a) graphWithNodes
                        else
                            graphWithNodes

                    let mutable warnings = nodeWarnings

                    for line in finalEdgeLines do
                        let trimmed = line.Trim()

                        if trimmed <> "" && not (trimmed.StartsWith("*")) then
                            let parts = trimmed.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

                            match parts with
                            | [| fromStr; toStr |] ->
                                match Int32.TryParse(fromStr), Int32.TryParse(toStr) with
                                | (true, fromId), (true, toId) ->
                                    if
                                        (graph.Nodes |> Map.containsKey fromId) && (graph.Nodes |> Map.containsKey toId)
                                    then
                                        let weight = edgeParser None

                                        try
                                            graph <- addEdge fromId toId weight graph
                                        with ex ->
                                            warnings <- EdgeAddFailed(ex.Message, trimmed) :: warnings
                                    else
                                        warnings <- NonexistentNodes(fromId, toId) :: warnings
                                | _ -> warnings <- MalformedEdge trimmed :: warnings
                            | _ ->
                                if parts.Length >= 3 then
                                    match Int32.TryParse(parts.[0]), Int32.TryParse(parts.[1]) with
                                    | (true, fromId), (true, toId) ->
                                        if
                                            (graph.Nodes |> Map.containsKey fromId)
                                            && (graph.Nodes |> Map.containsKey toId)
                                        then
                                            let weightStr = parts.[2]

                                            let weightVal =
                                                match Double.TryParse(weightStr) with
                                                | true, f -> Some f
                                                | false, _ -> None

                                            let weight = edgeParser weightVal

                                            try
                                                graph <- addEdge fromId toId weight graph
                                            with ex ->
                                                warnings <- EdgeAddFailed(ex.Message, trimmed) :: warnings
                                        else
                                            warnings <- NonexistentNodes(fromId, toId) :: warnings
                                    | _ -> warnings <- MalformedEdge trimmed :: warnings
                                else
                                    warnings <- MalformedEdge trimmed :: warnings

                    Ok
                        { Graph = graph
                          Warnings = List.rev warnings }

let parse (input: string) : Result<PajekResult<string, float>, PajekError> =
    parseWith input id (fun opt -> opt |> Option.defaultValue 1.0)
