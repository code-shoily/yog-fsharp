module Yog.FSharp.Tests.ColoringTests

open Xunit
open Yog
open Yog.Model
open Yog.Properties
open Yog.Properties.Coloring

// Helper to make a complete graph
let makeCompleteGraph (n: int) : Graph<unit, int> =
    let g = empty Undirected
    let gWithNodes = [ 0 .. n - 1 ] |> List.fold (fun acc i -> addNode i () acc) g
    let mutable finalG = gWithNodes

    for i in 0 .. n - 1 do
        for j in i + 1 .. n - 1 do
            finalG <- addEdge i j 1 finalG

    finalG

[<Fact>]
let ``Greedy coloring - complete graph K3`` () =
    let graph = makeCompleteGraph 3
    let (upper, colors) = coloringGreedy graph
    Assert.Equal(3, upper)
    Assert.NotEqual(Map.find 0 colors, Map.find 1 colors)
    Assert.NotEqual(Map.find 1 colors, Map.find 2 colors)
    Assert.NotEqual(Map.find 0 colors, Map.find 2 colors)

[<Fact>]
let ``DSatur coloring - complete graph K4`` () =
    let graph = makeCompleteGraph 4
    let (upper, colors) = coloringDsatur graph
    Assert.Equal(4, upper)

    for i in 0..3 do
        for j in i + 1 .. 3 do
            Assert.NotEqual(Map.find i colors, Map.find j colors)

[<Fact>]
let ``Exact coloring - complete graph K4`` () =
    let graph = makeCompleteGraph 4

    match coloringExact graph None with
    | Ok(chi, colors) ->
        Assert.Equal(4, chi)

        for i in 0..3 do
            for j in i + 1 .. 3 do
                Assert.NotEqual(Map.find i colors, Map.find j colors)
    | Timeout _ -> failwith "exact coloring timed out"
