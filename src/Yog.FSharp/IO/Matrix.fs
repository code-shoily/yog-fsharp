/// Adjacency matrix serialization support.
module Yog.IO.Matrix

open System
open System.Collections.Generic
open Yog.Model

/// Creates a graph from a square adjacency matrix representation.
let fromMatrix (graphType: GraphType) (matrix: float list list) : Result<Graph<unit, float>, string> =
    let n = matrix.Length

    if n = 0 then
        Ok(empty graphType)
    else
        let mutable isSquare = true

        for row in matrix do
            if row.Length <> n then
                isSquare <- false

        if not isSquare then
            Error "Adjacency matrix must be square (n x n)"
        else
            let mutable graph = empty graphType

            for i in 0 .. n - 1 do
                graph <- addNode i () graph

            let mutable possible = true
            let mutable errorMsg = ""

            match graphType with
            | Undirected ->
                for i in 0 .. n - 1 do
                    for j in i + 1 .. n - 1 do
                        let w = matrix.[i].[j]

                        if w <> 0.0 then
                            graph <- addEdge i j w graph
            | Directed ->
                for i in 0 .. n - 1 do
                    for j in 0 .. n - 1 do
                        if i <> j then
                            let w = matrix.[i].[j]

                            if w <> 0.0 then
                                graph <- addEdge i j w graph

            Ok graph

/// Exports a graph to a list of node IDs and a square adjacency matrix.
let toMatrix (graph: Graph<'n, float>) : NodeId list * float list list =
    let nodes = allNodes graph |> List.sort

    let matrix =
        nodes
        |> List.map (fun i ->
            nodes
            |> List.map (fun j ->
                if i = j then
                    0.0
                else
                    match edgeData i j graph with
                    | Some w -> w
                    | None -> 0.0))

    (nodes, matrix)

/// Serializes a graph to a string representation of an adjacency matrix.
let serialize (delimiter: string) (graph: Graph<'n, float>) : string =
    let (_, matrix) = toMatrix graph

    let lines =
        matrix
        |> List.map (fun row -> row |> List.map (fun w -> w.ToString()) |> String.concat delimiter)

    String.concat "\n" lines
