module Yog.FSharp.Tests.RenderTests

open Xunit
open Yog.Model
open Yog.Multi
open Yog.Builder
open Yog.Render

// ============================================================================
// MULTIGRAPH DOT RENDERING
// ============================================================================

[<Fact>]
let ``renderMulti generates digraph for directed multigraph`` () =
    let graph =
        Model.directed<string, int> ()
        |> Model.addNode 1 "A"
        |> Model.addNode 2 "B"
        |> fun g -> fst (Model.addEdge 1 2 10 g)

    let dot = Dot.renderMulti Dot.defaultMultiOptions graph
    Assert.Contains("digraph G {", dot)
    Assert.Contains("->", dot)
    Assert.Contains("1 -> 2", dot)
    Assert.Contains("[label=\"10\"]", dot)

[<Fact>]
let ``renderMulti highlights specific edge ID`` () =
    let graph =
        Model.directed<string, int> ()
        |> Model.addNode 1 "A"
        |> Model.addNode 2 "B"
        |> fun g -> fst (Model.addEdge 1 2 10 g) // Eid = 0

    let options =
        { Dot.defaultMultiOptions with
            HighlightedEdges = Set.singleton 0
            HighlightColor = "blue" }

    let dot = Dot.renderMulti options graph
    Assert.Contains("color=\"blue\"", dot)
    Assert.Contains("penwidth=2", dot)

// ============================================================================
// MULTIGRAPH MERMAID RENDERING
// ============================================================================

[<Fact>]
let ``renderMulti generates mermaid for directed multigraph`` () =
    let graph =
        Model.directed<string, int> ()
        |> Model.addNode 1 "A"
        |> Model.addNode 2 "B"
        |> fun g -> fst (Model.addEdge 1 2 10 g)

    let mermaid = Mermaid.renderMulti Mermaid.defaultMultiOptions graph
    Assert.Contains("graph TD", mermaid)
    Assert.Contains("1 -->|\"10\"| 2", mermaid)

[<Fact>]
let ``renderMulti highlights specific parallel edge`` () =
    let graph =
        Model.directed<string, int> ()
        |> Model.addNode 1 "A"
        |> Model.addNode 2 "B"
        |> fun g -> fst (Model.addEdge 1 2 10 g) // Eid = 0

    let options =
        { Mermaid.defaultMultiOptions with
            HighlightedEdges = Set.singleton 0 }

    let mermaid = Mermaid.renderMulti options graph
    Assert.Contains("classDef highlightEdge stroke:#f57c00,stroke-width:3px", mermaid)
    Assert.Contains("1 -->|\"10\"| 2:::highlightEdge", mermaid)

// ============================================================================
// ASCII RENDERING
// ============================================================================

[<Fact>]
let ``ascii gridToString renders simple maze`` () =
    let grid = Grid.from2DList [ [ "."; "." ] ] Undirected (fun _ _ -> false)
    let ascii = Ascii.gridToString grid Map.empty
    Assert.Contains("+---+---+", ascii)
    Assert.Contains("|   |   |", ascii)

[<Fact>]
let ``ascii gridToStringUnicode renders clean box boundaries`` () =
    let grid = Grid.from2DList [ [ "."; "." ] ] Undirected (fun _ _ -> false)
    let unicode = Ascii.gridToStringUnicode grid Map.empty
    Assert.Contains("┌───┬───┐", unicode)
    Assert.Contains("│   │   │", unicode)
    Assert.Contains("└───┴───┘", unicode)

[<Fact>]
let ``ascii gridToString with occupants`` () =
    let grid = Grid.from2DList [ [ "."; "." ] ] Undirected (fun _ _ -> false)
    let occupants = Map.empty |> Map.add 0 "X" |> Map.add 1 "Y"
    let ascii = Ascii.gridToString grid occupants
    Assert.Contains(" X ", ascii)
    Assert.Contains(" Y ", ascii)

[<Fact>]
let ``ascii toroidalToString renders arrows`` () =
    let toroidal = Toroidal.from2DList [ [ "."; "." ] ] Undirected (fun _ _ -> false)
    let ascii = Ascii.toroidalToString toroidal Map.empty
    Assert.Contains("v   v", ascii)
    Assert.Contains("> |   |   | <", ascii)
    Assert.Contains("^   ^", ascii)

[<Fact>]
let ``ascii toroidalToStringUnicode renders wrapping boundaries`` () =
    let toroidal = Toroidal.from2DList [ [ "."; "." ] ] Undirected (fun _ _ -> false)
    let unicode = Ascii.toroidalToStringUnicode toroidal Map.empty
    Assert.Contains("v   v", unicode)
    Assert.Contains("> │   │   │ <", unicode)
    Assert.Contains("ʌ   ʌ", unicode)
