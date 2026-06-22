/// Trivial Graph Format (TGF) serialization support.
module Yog.IO.Tgf

open System
open Yog.Model

/// Warnings collected during TGF parsing.
type TgfWarning =
    | MalformedEdge of lineNum: int * line: string
    | InvalidEdge of lineNum: int * reason: string

/// Structured errors from TGF parsing.
type TgfError =
    | MissingSeparator of string
    | DuplicateNode of lineNum: int * id: NodeId
    | InvalidNodeId of lineNum: int * idStr: string

/// Result of a successful TGF parse.
type TgfResult<'n, 'e> =
    { Graph: Graph<'n, 'e>
      Warnings: TgfWarning list }

/// Options for TGF serialization.
type TgfOptions<'n, 'e> =
    { NodeLabel: 'n -> string
      EdgeLabel: 'e -> string option
      NodeFormatter: string -> string
      EdgeFormatter: string -> string }

/// Default options: uses toString for nodes, no labels for edges, identity formatting.
let defaultOptions: TgfOptions<'n, 'e> =
    { NodeLabel = (fun data -> data.ToString())
      EdgeLabel = (fun _ -> None)
      NodeFormatter = id
      EdgeFormatter = id }

/// Creates TGF options with custom functions.
let optionsWith (nodeLabel: 'n -> string) (edgeLabel: 'e -> string option) =
    { NodeLabel = nodeLabel
      EdgeLabel = edgeLabel
      NodeFormatter = id
      EdgeFormatter = id }

/// Creates TGF options with custom formatters.
let optionsWithFormatters (nodeLabel: 'n -> string) (edgeLabel: 'e -> string option) nodeFmt edgeFmt =
    { NodeLabel = nodeLabel
      EdgeLabel = edgeLabel
      NodeFormatter = nodeFmt
      EdgeFormatter = edgeFmt }

/// Serializes a graph to TGF format with custom options.
let serializeWith (options: TgfOptions<'n, 'e>) (graph: Graph<'n, 'e>) : string =
    let sb = System.Text.StringBuilder()

    // Nodes
    for (id, value) in graph.Nodes |> Map.toList |> List.sortBy fst do
        let label = options.NodeLabel value

        sb.AppendLine(sprintf "%s %s" (options.NodeFormatter(id.ToString())) (options.NodeFormatter label))
        |> ignore

    sb.AppendLine("#") |> ignore

    // Edges
    for (src, dst, weight) in allEdges graph |> List.sortBy (fun (s, d, _) -> (s, d)) do
        let srcStr = options.NodeFormatter(src.ToString())
        let dstStr = options.NodeFormatter(dst.ToString())

        match options.EdgeLabel weight with
        | Some label ->
            sb.AppendLine(sprintf "%s %s %s" srcStr dstStr (options.EdgeFormatter label))
            |> ignore
        | None -> sb.AppendLine(sprintf "%s %s" srcStr dstStr) |> ignore

    sb.ToString()

/// Serializes a graph to TGF format.
let serialize (options: TgfOptions<'n, 'e>) (graph: Graph<'n, 'e>) : string = serializeWith options graph

/// Parses a TGF string into a graph with detailed warnings and errors.
let parseWith
    (graphType: GraphType)
    (nodeParser: NodeId -> string -> 'n)
    (edgeParser: string option -> 'e)
    (input: string)
    : Result<TgfResult<'n, 'e>, TgfError> =

    let lines = input.Split([| '\r'; '\n' |], StringSplitOptions.None)
    let separatorIndex = lines |> Array.tryFindIndex (fun l -> l.Trim() = "#")

    match separatorIndex with
    | None -> Error(MissingSeparator "Input must contain '#' separator")
    | Some sepIdx ->
        let nodeLines = lines.[0 .. sepIdx - 1]
        let edgeLines = lines.[sepIdx + 1 ..]

        let mutable graph = empty graphType
        let mutable warnings = []
        let mutable err = None

        let ensureNode id label (g: Graph<'n, 'e>) =
            if g.Nodes |> Map.containsKey id then
                g
            else
                addNode id (nodeParser id label) g

        // Parse nodes
        for idx in 0 .. nodeLines.Length - 1 do
            let lineNum = idx + 1
            let trimmed = nodeLines.[idx].Trim()

            if trimmed <> "" && trimmed <> "#" && err.IsNone then
                let parts = trimmed.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

                match parts with
                | [| idStr |] ->
                    match Int32.TryParse(idStr) with
                    | true, id ->
                        if graph.Nodes |> Map.containsKey id then
                            err <- Some(DuplicateNode(lineNum, id))
                        else
                            graph <- addNode id (nodeParser id (id.ToString())) graph
                    | false, _ -> err <- Some(InvalidNodeId(lineNum, idStr))
                | [| idStr; label |] ->
                    match Int32.TryParse(idStr) with
                    | true, id ->
                        if graph.Nodes |> Map.containsKey id then
                            err <- Some(DuplicateNode(lineNum, id))
                        else
                            let normalizedLabel =
                                label.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
                                |> String.concat " "

                            graph <- addNode id (nodeParser id normalizedLabel) graph
                    | false, _ -> err <- Some(InvalidNodeId(lineNum, idStr))
                | _ -> ()

        match err with
        | Some e -> Error e
        | None ->
            // Parse edges
            for idx in 0 .. edgeLines.Length - 1 do
                let lineNum = sepIdx + 2 + idx
                let trimmed = edgeLines.[idx].Trim()

                if trimmed <> "" && trimmed <> "#" then
                    let parts = trimmed.Split([| ' '; '\t' |], 3, StringSplitOptions.RemoveEmptyEntries)

                    match parts with
                    | [| srcStr; dstStr |] ->
                        match Int32.TryParse(srcStr), Int32.TryParse(dstStr) with
                        | (true, src), (true, dst) ->
                            graph <- ensureNode src (src.ToString()) graph
                            graph <- ensureNode dst (dst.ToString()) graph
                            graph <- addEdge src dst (edgeParser None) graph
                        | _ -> warnings <- MalformedEdge(lineNum, trimmed) :: warnings
                    | [| srcStr; dstStr; label |] ->
                        match Int32.TryParse(srcStr), Int32.TryParse(dstStr) with
                        | (true, src), (true, dst) ->
                            graph <- ensureNode src (src.ToString()) graph
                            graph <- ensureNode dst (dst.ToString()) graph
                            graph <- addEdge src dst (edgeParser (Some label)) graph
                        | _ -> warnings <- MalformedEdge(lineNum, trimmed) :: warnings
                    | _ -> warnings <- MalformedEdge(lineNum, trimmed) :: warnings

            Ok
                { Graph = graph
                  Warnings = List.rev warnings }

/// Legacy parse function for backward compatibility.
let parse
    (graphType: GraphType)
    (nodeParser: string -> 'n)
    (edgeParser: string option -> 'e)
    (input: string)
    : Result<Graph<'n, 'e>, string> =
    match parseWith graphType (fun _ label -> nodeParser label) edgeParser input with
    | Ok res -> Ok res.Graph
    | Error(MissingSeparator msg) -> Error msg
    | Error(DuplicateNode(lineNum, id)) -> Error(sprintf "Duplicate node at line %d: %d" lineNum id)
    | Error(InvalidNodeId(lineNum, idStr)) -> Error(sprintf "Invalid node ID at line %d: %s" lineNum idStr)
