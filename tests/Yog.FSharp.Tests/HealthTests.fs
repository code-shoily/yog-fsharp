module Yog.FSharp.Tests.HealthTests

open Xunit
open Yog
open Yog.Model
open Yog.Health

let makeUndirectedGraph (edges: (NodeId * NodeId) list) : Graph<unit, int> =
    let allNodes = edges |> List.collect (fun (u, v) -> [ u; v ]) |> List.distinct
    let g = empty Undirected
    let gWithNodes = allNodes |> List.fold (fun acc n -> addNode n () acc) g
    edges |> List.fold (fun acc (u, v) -> addEdge u v 1 acc) gWithNodes

[<Fact>]
let ``Diameter and Radius of a path graph`` () =
    // Path graph 1-2-3-4
    let graph = makeUndirectedGraph [ (1, 2); (2, 3); (3, 4) ]
    
    let ecc1 = eccentricity 0 (+) compare 1 graph
    let ecc2 = eccentricity 0 (+) compare 2 graph
    Assert.Equal(Some 3, ecc1)
    Assert.Equal(Some 2, ecc2)
    
    let diam = diameter 0 (+) compare graph
    let rad = radius 0 (+) compare graph
    Assert.Equal(Some 3, diam)
    Assert.Equal(Some 2, rad)

[<Fact>]
let ``Assortativity of a star graph`` () =
    // Center 1 connected to leaves 2, 3, 4
    let graph = makeUndirectedGraph [ (1, 2); (1, 3); (1, 4) ]
    let assort = assortativity graph
    Assert.True(assort < 0.0)

[<Fact>]
let ``Average Path Length of a triangle`` () =
    let graph = makeUndirectedGraph [ (1, 2); (2, 3); (3, 1) ]
    let avg = averagePathLength 0 (+) compare float graph
    Assert.True(avg.IsSome)
    Assert.True(abs (avg.Value - 1.0) < 0.001)

[<Fact>]
let ``Efficiency and Global/Local Efficiency`` () =
    let graph = makeUndirectedGraph [ (1, 2); (2, 3); (3, 1) ]
    let eff = efficiency 0 (+) compare float 1 2 graph
    let globEff = globalEfficiency 0 (+) compare float graph
    let locEff = localEfficiency 0 (+) compare float 1 graph
    
    Assert.Equal(1.0, eff)
    Assert.Equal(1.0, globEff)
    Assert.Equal(1.0, locEff)
