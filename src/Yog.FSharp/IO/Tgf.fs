/// Trivial Graph Format (TGF) serialization support.
module Yog.IO.Tgf

open System
open System.Collections.Generic
open Yog.Model

/// Options for TGF serialization.
type TgfOptions<'n, 'e> =
    { NodeLabel: 'n -> string
      EdgeLabel: 'e -> string option }

/// Default options: uses toString for nodes, no labels for edges.
let defaultOptions: TgfOptions<'n, 'e> =
    { NodeLabel = (fun data -> data.ToString())
      EdgeLabel = (fun _ -> None) }

/// Serializes a graph to TGF format.
let serialize (options: TgfOptions<'n, 'e>) (graph: Graph<'n, 'e>) : string =
    let sb = System.Text.StringBuilder()

    // Nodes
    for (id, value) in graph.Nodes |> Map.toList |> List.sortBy fst do
        let label = options.NodeLabel value
        sb.AppendLine(sprintf "%d %s" id label) |> ignore

    sb.AppendLine("#") |> ignore

    // Edges
    for (src, dst, weight) in allEdges graph do
        match options.EdgeLabel weight with
        | Some label -> sb.AppendLine(sprintf "%d %d %s" src dst label) |> ignore
        | None -> sb.AppendLine(sprintf "%d %d" src dst) |> ignore

    sb.ToString()

/// Parses a TGF string into a graph.
let parse
    (graphType: GraphType)
    (nodeParser: string -> 'n)
    (edgeParser: string option -> 'e)
    (input: string)
    : Result<Graph<'n, 'e>, string> =
    let lines = input.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    let separatorIndex = lines |> Array.tryFindIndex (fun l -> l.Trim() = "#")

    match separatorIndex with
    | None -> Error "Missing '#' separator in TGF input"
    | Some sepIdx ->
        let nodeLines = lines.[0 .. sepIdx - 1]
        let edgeLines = lines.[sepIdx + 1 ..]

        let mutable graph = empty graphType
        let mutable possible = true
        let mutable errorMsg = ""

        // Add declared nodes
        for line in nodeLines do
            let trimmed = line.Trim()

            if trimmed <> "" && not (trimmed.StartsWith("#")) then
                let parts = trimmed.Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)

                match parts with
                | [| idStr |] ->
                    match Int32.TryParse(idStr) with
                    | true, id -> graph <- addNode id (nodeParser (id.ToString())) graph
                    | false, _ ->
                        possible <- false
                        errorMsg <- sprintf "Invalid node ID: %s" idStr
                | [| idStr; label |] ->
                    match Int32.TryParse(idStr) with
                    | true, id -> graph <- addNode id (nodeParser label) graph
                    | false, _ ->
                        possible <- false
                        errorMsg <- sprintf "Invalid node ID: %s" idStr
                | _ -> ()

        // Add edges and auto-create nodes if missing
        if possible then
            let ensureNode id data (g: Graph<'n, 'e>) =
                if g.Nodes |> Map.containsKey id then
                    g
                else
                    addNode id data g

            for line in edgeLines do
                let trimmed = line.Trim()

                if trimmed <> "" && not (trimmed.StartsWith("#")) then
                    let parts = trimmed.Split([| ' '; '\t' |], 3, StringSplitOptions.RemoveEmptyEntries)

                    match parts with
                    | [| srcStr; dstStr |] ->
                        match Int32.TryParse(srcStr), Int32.TryParse(dstStr) with
                        | (true, src), (true, dst) ->
                            graph <- ensureNode src (nodeParser (src.ToString())) graph
                            graph <- ensureNode dst (nodeParser (dst.ToString())) graph
                            graph <- addEdge src dst (edgeParser None) graph
                        | _ ->
                            possible <- false
                            errorMsg <- sprintf "Invalid edge endpoints: %s -> %s" srcStr dstStr
                    | [| srcStr; dstStr; label |] ->
                        match Int32.TryParse(srcStr), Int32.TryParse(dstStr) with
                        | (true, src), (true, dst) ->
                            graph <- ensureNode src (nodeParser (src.ToString())) graph
                            graph <- ensureNode dst (nodeParser (dst.ToString())) graph
                            graph <- addEdge src dst (edgeParser (Some label)) graph
                        | _ ->
                            possible <- false
                            errorMsg <- sprintf "Invalid edge endpoints: %s -> %s" srcStr dstStr
                    | _ -> ()

        if possible then Ok graph else Error errorMsg
