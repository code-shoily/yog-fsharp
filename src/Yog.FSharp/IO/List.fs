/// Adjacency list serialization support.
module Yog.IO.List

open System
open System.Collections.Generic
open Yog.Model

/// Creates a graph from an adjacency list representation string.
let parse (graphType: GraphType) (weighted: bool) (delimiter: string) (input: string) : Result<Graph<unit, float>, string> =
    let lines = input.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    let mutable graph = empty graphType
    let mutable possible = true
    let mutable errorMsg = ""
    
    let ensureNode id data (g: Graph<'n, 'e>) =
        if g.Nodes |> Map.containsKey id then g else addNode id data g

    for line in lines do
        let trimmed = line.Trim()
        if trimmed <> "" && not (trimmed.StartsWith("#")) && possible then
            let parts = trimmed.Split([| delimiter |], 2, StringSplitOptions.None)
            match parts with
            | [| nodeStr |] ->
                match Int32.TryParse(nodeStr.Trim()) with
                | true, node ->
                    graph <- ensureNode node () graph
                | false, _ ->
                    possible <- false
                    errorMsg <- sprintf "Invalid node ID: %s" nodeStr
            | [| nodeStr; neighborsStr |] ->
                match Int32.TryParse(nodeStr.Trim()) with
                | true, node ->
                    graph <- ensureNode node () graph
                    let nbrs = neighborsStr.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
                    for nbr in nbrs do
                        if weighted then
                            let edgeParts = nbr.Split([| ',' |], 2, StringSplitOptions.None)
                            match edgeParts with
                            | [| destStr; wStr |] ->
                                match Int32.TryParse(destStr.Trim()), Double.TryParse(wStr.Trim()) with
                                | (true, dest), (true, weight) ->
                                    graph <- ensureNode dest () graph
                                    graph <- addEdge node dest weight graph
                                | _ ->
                                    possible <- false
                                    errorMsg <- sprintf "Invalid neighbor or weight: %s" nbr
                            | [| destStr |] ->
                                match Int32.TryParse(destStr.Trim()) with
                                | true, dest ->
                                    graph <- ensureNode dest () graph
                                    graph <- addEdge node dest 1.0 graph
                                | _ ->
                                    possible <- false
                                    errorMsg <- sprintf "Invalid neighbor ID: %s" destStr
                            | _ -> ()
                        else
                            match Int32.TryParse(nbr.Trim()) with
                            | true, dest ->
                                graph <- ensureNode dest () graph
                                graph <- addEdge node dest 1.0 graph
                            | false, _ ->
                                possible <- false
                                errorMsg <- sprintf "Invalid neighbor ID: %s" nbr
                | false, _ ->
                    possible <- false
                    errorMsg <- sprintf "Invalid node ID: %s" nodeStr
            | _ -> ()
            
    if possible then Ok graph else Error errorMsg

/// Serializes a graph to a string representation of an adjacency list.
let serialize (weighted: bool) (delimiter: string) (graph: Graph<'n, float>) : string =
    let sb = System.Text.StringBuilder()
    for (node, _) in graph.Nodes |> Map.toList |> List.sortBy fst do
        let neighbors = successors node graph |> List.sortBy fst
        if neighbors.IsEmpty then
            sb.AppendLine(sprintf "%d%s" node delimiter) |> ignore
        else
            let nbrStrs =
                neighbors
                |> List.map (fun (dest, w) ->
                    if weighted then sprintf "%d,%f" dest w else sprintf "%d" dest
                )
            sb.AppendLine(sprintf "%d%s %s" node delimiter (String.concat " " nbrStrs)) |> ignore
    sb.ToString()
