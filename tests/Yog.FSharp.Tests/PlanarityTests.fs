module Yog.FSharp.Tests.PlanarityTests

open Xunit
open Yog.Model
open Yog.Properties.Planarity
open Yog.Properties.TreeDecomposition
open Yog.Properties.WeisfeilerLehman

[<Fact>]
let ``K5 is non-planar`` () =
    let k5 = Yog.Generators.Classic.complete 5 Undirected
    Assert.False(isPlanar k5)

[<Fact>]
let ``K3,3 is non-planar`` () =
    let k33 = Yog.Generators.Classic.completeBipartite 3 3 Undirected
    Assert.False(isPlanar k33)

[<Fact>]
let ``Grid graph 3x3 is planar`` () =
    let grid = Yog.Generators.Classic.grid2D 3 3 Undirected
    Assert.True(isPlanar grid)

[<Fact>]
let ``K5 Kuratowski witness is K5`` () =
    let k5 = Yog.Generators.Classic.complete 5 Undirected
    match kuratowskiWitness k5 with
    | Some witness -> 
        Assert.Equal(K5, witness.Type)
    | None -> 
        Assert.Fail("K5 should have a Kuratowski witness")

[<Fact>]
let ``K3,3 Kuratowski witness is K33`` () =
    let k33 = Yog.Generators.Classic.completeBipartite 3 3 Undirected
    match kuratowskiWitness k33 with
    | Some witness -> 
        Assert.Equal(K33, witness.Type)
    | None -> 
        Assert.Fail("K3,3 should have a Kuratowski witness")

[<Fact>]
let ``WL Hashing detects isomorphism`` () =
    // Create two isomorphic graphs
    let g1 = empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addEdge 1 2 1 |> addEdge 2 3 1
    let g2 = empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addEdge 2 3 1 |> addEdge 3 1 1
    let h1 = defaultGraphHash g1
    let h2 = defaultGraphHash g2
    Assert.Equal(h1, h2)

[<Fact>]
let ``WL Hashing distinguishes non-isomorphic graphs`` () =
    let g1 = empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addEdge 1 2 1 |> addEdge 2 3 1
    let g2 = empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addEdge 1 2 1 |> addEdge 2 3 1 |> addEdge 3 1 1
    let h1 = defaultGraphHash g1
    let h2 = defaultGraphHash g2
    Assert.NotEqual<string>(h1, h2)

[<Fact>]
let ``Tree decomposition validity check`` () =
    // Path 1 - 2 - 3
    let graph = empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addEdge 1 2 1 |> addEdge 2 3 1
    
    // Bags: 0 -> {1, 2}, 1 -> {2, 3}
    let bags = 
        Map.ofList [
            0, Set.ofList [1; 2]
            1, Set.ofList [2; 3]
        ]
    let tree = empty Undirected |> addNode 0 () |> addNode 1 () |> addEdge 0 1 1
    
    let td = { Bags = bags; Tree = tree; Width = 1 }
    Assert.True(isValid td graph)
