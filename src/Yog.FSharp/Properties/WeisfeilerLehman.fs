/// Weisfeiler-Lehman (WL) graph hashing algorithm.
module Yog.Properties.WeisfeilerLehman

open System
open System.Text
open System.Security.Cryptography
open Yog.Model

let private md5Hash (input: string) : string =
    use md5 = MD5.Create()
    let bytes = Encoding.UTF8.GetBytes(input)
    let hashBytes = md5.ComputeHash(bytes)
    hashBytes |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""

/// Calculates the WL structural hash for a given graph.
let graphHash (iterations: int) (nodeLabelFn: Graph<'n, 'e> -> NodeId -> string) (graph: Graph<'n, 'e>) : string =
    let initialLabels =
        allNodes graph
        |> List.fold (fun acc node -> Map.add node (nodeLabelFn graph node) acc) Map.empty

    let finalLabels =
        if iterations > 0 then
            let rec loop iter labels =
                if iter = 0 then
                    labels
                else
                    let nextLabels =
                        allNodes graph
                        |> List.fold
                            (fun acc node ->
                                let nbrLabels =
                                    neighborIds node graph |> List.map (fun nbr -> Map.find nbr labels) |> List.sort

                                let currentLabel = Map.find node labels
                                let combined = currentLabel :: nbrLabels |> String.concat ""
                                let newLabel = md5Hash combined
                                Map.add node newLabel acc)
                            Map.empty

                    loop (iter - 1) nextLabels

            loop iterations initialLabels
        else
            initialLabels

    let finalCombined = finalLabels |> Map.values |> Seq.sort |> String.concat ""

    md5Hash finalCombined

/// Default graphHash with 3 iterations and degree-based labels
let defaultGraphHash (graph: Graph<'n, 'e>) : string =
    let degreeLabelFn g node = (neighbors node g).Length.ToString()
    graphHash 3 degreeLabelFn graph
