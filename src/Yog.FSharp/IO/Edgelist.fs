/// Edgelist serialization support.
module Yog.IO.Edgelist

open System
open Yog.Model

/// Creates a graph from a sequence of edges.
let fromEdges (graphType: GraphType) (edges: seq<NodeId * NodeId * 'e>) : Graph<unit, 'e> =
    let mutable graph = empty graphType

    let ensureNode id g =
        if g.Nodes |> Map.containsKey id then g else addNode id () g

    for (src, dst, weight) in edges do
        graph <- ensureNode src graph
        graph <- ensureNode dst graph
        graph <- addEdge src dst weight graph

    graph

/// Exports a graph to a list of edges.
let toEdges (graph: Graph<'n, 'e>) : (NodeId * NodeId * 'e) list =
    allEdges graph |> List.sortBy (fun (src, dst, _) -> (src, dst))

/// Parses a string representation of an edgelist.
let parse (graphType: GraphType) (weighted: bool) (input: string) : Result<Graph<unit, float>, string> =
    let lines = input.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    let mutable edges = []
    let mutable possible = true
    let mutable errorMsg = ""

    for line in lines do
        let trimmed = line.Trim()

        if trimmed <> "" && not (trimmed.StartsWith("#")) && possible then
            let parts =
                trimmed.Split([| ' '; '\t'; ',' |], StringSplitOptions.RemoveEmptyEntries)

            match parts with
            | [| srcStr; dstStr |] ->
                match Int32.TryParse(srcStr), Int32.TryParse(dstStr) with
                | (true, src), (true, dst) -> edges <- (src, dst, 1.0) :: edges
                | _ ->
                    possible <- false
                    errorMsg <- sprintf "Invalid edge endpoints: %s -> %s" srcStr dstStr
            | [| srcStr; dstStr; wStr |] ->
                match Int32.TryParse(srcStr), Int32.TryParse(dstStr), Double.TryParse(wStr) with
                | (true, src), (true, dst), (true, w) -> edges <- (src, dst, w) :: edges
                | _ ->
                    if weighted then
                        possible <- false
                        errorMsg <- sprintf "Invalid edge format or weight: %s" line
                    else
                        // If not weighted, ignore third column or treat as unweighted
                        match Int32.TryParse(srcStr), Int32.TryParse(dstStr) with
                        | (true, src), (true, dst) -> edges <- (src, dst, 1.0) :: edges
                        | _ ->
                            possible <- false
                            errorMsg <- sprintf "Invalid edge endpoints: %s -> %s" srcStr dstStr
            | _ ->
                possible <- false
                errorMsg <- sprintf "Invalid line format: %s" line

    if possible then
        Ok(fromEdges graphType (List.rev edges))
    else
        Error errorMsg

/// Serializes a graph to an edgelist string.
let serialize (weighted: bool) (graph: Graph<'n, float>) : string =
    let sb = System.Text.StringBuilder()

    for (src, dst, w) in toEdges graph do
        if weighted then
            sb.AppendLine(sprintf "%d %d %f" src dst w) |> ignore
        else
            sb.AppendLine(sprintf "%d %d" src dst) |> ignore

    sb.ToString()
