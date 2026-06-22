module Yog.FSharp.Tests.ApproximateTests

open Xunit
open Yog.Model
open Yog.Approximate

[<Fact>]
let ``diameter approximation on path graph`` () =
    let g =
        empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addNode 4 ()

    let g = addEdge 1 2 1.0 g |> addEdge 2 3 1.0 |> addEdge 3 4 1.0
    let dOpt = diameterUnweighted 4 g
    Assert.True(dOpt.IsSome)
    Assert.Equal(3.0, dOpt.Value)

[<Fact>]
let ``betweenness approximation on simple path`` () =
    // Path: 1-2-3-4
    let g =
        empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addNode 4 ()

    let g = addEdge 1 2 1.0 g |> addEdge 2 3 1.0 |> addEdge 3 4 1.0
    // Approximate with 4 samples (exact brandes discovery)
    let scores = betweenness (Some 4) None 0.0 (+) compare g
    Assert.Equal(4, scores.Count)
    // Betweenness for endpoints should be 0.0
    Assert.Equal(0.0, scores.[1])
    Assert.Equal(0.0, scores.[4])

[<Fact>]
let ``closeness and harmonic centrality approximation`` () =
    let g = empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 ()
    let g = addEdge 1 2 1.0 g |> addEdge 2 3 1.0 |> addEdge 3 1 1.0
    let scoresClose = closeness (Some 3) None 0.0 (+) compare id g
    let scoresHarmonic = harmonic (Some 3) None 0.0 (+) compare id g
    Assert.Equal(3, scoresClose.Count)
    Assert.Equal(3, scoresHarmonic.Count)

[<Fact>]
let ``average path length and efficiency approximation`` () =
    let g = empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 ()
    let g = addEdge 1 2 1.0 g |> addEdge 2 3 1.0
    let aplOpt = averagePathLength (Some 3) None 0.0 (+) compare id g
    let eff = globalEfficiency (Some 3) None 0.0 (+) compare id g
    Assert.True(aplOpt.IsSome)
    Assert.True(eff > 0.0)

[<Fact>]
let ``transitivity approximation via wedge sampling`` () =
    let g = empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 ()
    let g = addEdge 1 2 1.0 g |> addEdge 2 3 1.0 |> addEdge 3 1 1.0
    let t = transitivity (Some 100) None g
    Assert.Equal(1.0, t)

[<Fact>]
let ``vertex cover 2-approximation`` () =
    let g =
        empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addNode 4 ()

    let g = addEdge 1 2 1.0 g |> addEdge 2 3 1.0 |> addEdge 3 4 1.0 |> addEdge 4 1 1.0
    let cover = vertexCover g
    // Cover size should be at most 2 * optimal (2) = 4, and at least 2
    Assert.True(cover.Count >= 2 && cover.Count <= 4)
    // Assert every edge is covered
    for (u, v, _) in allEdges g do
        Assert.True(cover.Contains(u) || cover.Contains(v))

[<Fact>]
let ``max clique greedy approximation`` () =
    let g =
        empty Undirected |> addNode 1 () |> addNode 2 () |> addNode 3 () |> addNode 4 ()

    let g =
        addEdge 1 2 1.0 g
        |> addEdge 1 3 1.0
        |> addEdge 1 4 1.0
        |> addEdge 2 3 1.0
        |> addEdge 2 4 1.0
        |> addEdge 3 4 1.0

    let clique = maxClique g
    Assert.Equal(4, clique.Count)

[<Fact>]
let ``treewidth upper bound and decomposition cycle graph`` () =
    let g =
        empty Undirected
        |> addNode 1 ()
        |> addNode 2 ()
        |> addNode 3 ()
        |> addNode 4 ()
        |> addNode 5 ()

    let g =
        addEdge 1 2 1.0 g
        |> addEdge 2 3 1.0
        |> addEdge 3 4 1.0
        |> addEdge 4 5 1.0
        |> addEdge 5 1 1.0

    let tw = treewidthUpperBound MinDegree g
    Assert.True(tw <= 2)
    let tdOpt = treeDecomposition MinDegree g
    Assert.True(tdOpt.IsSome)
    let td = tdOpt.Value
    Assert.True(td.Width <= 2)
