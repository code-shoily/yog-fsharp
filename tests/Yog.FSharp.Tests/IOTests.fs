module Yog.FSharp.Tests.IOTests


open Xunit
open System.Text.Json
open Yog.Model
open Yog.IO

// ============================================================================
// DOT RENDERING
// ============================================================================

[<Fact>]
let ``toDot generates digraph for directed`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 5

    let dot = Dot.render Dot.defaultOptions graph

    Assert.Contains("digraph G {", dot)
    Assert.Contains("->", dot)

[<Fact>]
let ``toDot generates graph for undirected`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 5

    let dot = Dot.render Dot.defaultOptions graph

    Assert.Contains("graph G {", dot)
    Assert.Contains("--", dot)

[<Fact>]
let ``toDot includes all nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "Start"
        |> addNode 2 "Process"
        |> addNode 3 "End"

    let dot = Dot.render Dot.defaultOptions graph

    Assert.Contains("1 [label=\"1\"]", dot)
    Assert.Contains("2 [label=\"2\"]", dot)
    Assert.Contains("3 [label=\"3\"]", dot)

[<Fact>]
let ``toDot includes all edges`` () =
    let graph =
        empty Directed
        |> addNode 1 ""
        |> addNode 2 ""
        |> addNode 3 ""
        |> addEdge 1 2 10
        |> addEdge 2 3 20

    let dot = Dot.render Dot.defaultOptions graph

    Assert.Contains("1 -> 2", dot)
    Assert.Contains("2 -> 3", dot)

[<Fact>]
let ``toDot uses custom node labels`` () =
    let graph =
        empty Directed
        |> addNode 1 "Alice"
        |> addNode 2 "Bob"
        |> addEdge 1 2 1

    let options =
        { Dot.defaultOptions with NodeLabel = fun id data -> $"{id}:{data}" }

    let dot = Dot.render options graph

    Assert.Contains("1 [label=\"1:Alice\"]", dot)
    Assert.Contains("2 [label=\"2:Bob\"]", dot)

[<Fact>]
let ``toDot uses custom edge labels`` () =
    let graph =
        empty Directed
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 1 2 42

    let options = { Dot.defaultOptions with EdgeLabel = fun w -> $"weight={w}" }
    let dot = Dot.render options graph

    Assert.Contains("[label=\"weight=42\"]", dot)

[<Fact>]
let ``toDot highlights specified nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 1

    let options =
        { Dot.defaultOptions with
            HighlightedNodes = Set.ofList [ 1 ]
            HighlightColor = "blue" }

    let dot = Dot.render options graph

    Assert.Contains("fillcolor=\"blue\"", dot)
    Assert.Contains("style=filled", dot)

[<Fact>]
let ``toDot highlights specified edges`` () =
    let graph =
        empty Directed
        |> addNode 1 ""
        |> addNode 2 ""
        |> addNode 3 ""
        |> addEdge 1 2 1
        |> addEdge 2 3 1

    let options =
        { Dot.defaultOptions with
            HighlightedEdges = Set.ofList [ (1, 2) ]
            HighlightColor = "green" }

    let dot = Dot.render options graph

    Assert.Contains("color=\"green\"", dot)
    Assert.Contains("penwidth=2", dot)

[<Fact>]
let ``toDot uses custom node shape`` () =
    let graph = empty Directed |> addNode 1 "A"

    let options = { Dot.defaultOptions with NodeShape = "box" }
    let dot = Dot.render options graph

    Assert.Contains("node [shape=box]", dot)

[<Fact>]
let ``toDot empty graph`` () =
    let graph = empty Directed
    let dot = Dot.render Dot.defaultOptions graph

    Assert.Contains("digraph G {", dot)
    Assert.Contains("}", dot)

// ============================================================================
// MERMAID RENDERING
// ============================================================================

[<Fact>]
let ``toMermaid uses TD for directed`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 5

    let mermaid = Mermaid.render Mermaid.defaultOptions graph

    Assert.Contains("graph TD", mermaid)
    Assert.Contains("-->", mermaid)

[<Fact>]
let ``toMermaid uses LR for undirected`` () =
    let graph =
        empty Undirected
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 5

    let mermaid = Mermaid.render Mermaid.defaultOptions graph

    Assert.Contains("graph LR", mermaid)
    Assert.Contains("---", mermaid)

[<Fact>]
let ``toMermaid includes all nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "Start"
        |> addNode 2 "End"

    let mermaid = Mermaid.render Mermaid.defaultOptions graph

    Assert.Contains("1[\"1\"]", mermaid)
    Assert.Contains("2[\"2\"]", mermaid)

[<Fact>]
let ``toMermaid includes all edges with weights`` () =
    let graph =
        empty Directed
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 1 2 10

    let mermaid = Mermaid.render Mermaid.defaultOptions graph

    Assert.Contains("1 -->", mermaid)
    Assert.Contains("|\"10\"|", mermaid)
    Assert.Contains("2", mermaid)

[<Fact>]
let ``toMermaid uses custom node labels`` () =
    let graph =
        empty Directed
        |> addNode 1 "Alice"
        |> addNode 2 "Bob"
        |> addEdge 1 2 1

    let options = { Mermaid.defaultOptions with NodeLabel = fun id data -> data }
    let mermaid = Mermaid.render options graph

    Assert.Contains("1[\"Alice\"]", mermaid)
    Assert.Contains("2[\"Bob\"]", mermaid)

[<Fact>]
let ``toMermaid highlights nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 1

    let options =
        { Mermaid.defaultOptions with HighlightedNodes = Set.ofList [ 1 ] }

    let mermaid = Mermaid.render options graph

    Assert.Contains("classDef highlight", mermaid)
    Assert.Contains(":::highlight", mermaid)

[<Fact>]
let ``toMermaid highlights edges`` () =
    let graph =
        empty Directed
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 1 2 1

    let options =
        { Mermaid.defaultOptions with HighlightedEdges = Set.ofList [ (1, 2) ] }

    let mermaid = Mermaid.render options graph

    Assert.Contains(":::highlightEdge", mermaid)

[<Fact>]
let ``toMermaid empty graph`` () =
    let graph = empty Directed
    let mermaid = Mermaid.render Mermaid.defaultOptions graph

    Assert.Contains("graph TD", mermaid)

// ============================================================================
// JSON RENDERING
// ============================================================================

[<Fact>]
let ``toJson includes graph kind`` () =
    let directed = empty Directed
    let undirected = empty Undirected

    let jsonDir = Json.render directed
    let jsonUndir = Json.render undirected

    Assert.Contains("\"kind\": \"Directed\"", jsonDir)
    Assert.Contains("\"kind\": \"Undirected\"", jsonUndir)

[<Fact>]
let ``toJson includes all nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "Alice"
        |> addNode 2 "Bob"

    let json = Json.render graph

    Assert.Contains("\"id\": 1", json)
    Assert.Contains("\"id\": 2", json)
    Assert.Contains("\"data\": \"Alice\"", json)
    Assert.Contains("\"data\": \"Bob\"", json)

[<Fact>]
let ``toJson includes all edges`` () =
    let graph =
        empty Directed
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 1 2 42

    let json = Json.render graph

    Assert.Contains("\"source\": 1", json)
    Assert.Contains("\"target\": 2", json)
    Assert.Contains("\"weight\": \"42\"", json)

[<Fact>]
let ``toJson produces valid JSON`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addEdge 1 2 10

    let json = Json.render graph

    // Should be parseable
    let doc = JsonDocument.Parse(json)
    Assert.NotNull(doc)

[<Fact>]
let ``toJson empty graph`` () =
    let graph = empty Directed
    let json = Json.render graph

    Assert.Contains("\"kind\": \"Directed\"", json)
    Assert.Contains("\"nodes\": []", json)
    Assert.Contains("\"edges\": []", json)

[<Fact>]
let ``toJson undirected only includes one direction`` () =
    let graph =
        empty Undirected
        |> addNode 1 ""
        |> addNode 2 ""
        |> addEdge 1 2 5

    let json = Json.render graph

    // For undirected, should only have one edge entry
    // (not both 1->2 and 2->1)
    let edgeCount = json.Split("source").Length - 1
    Assert.Equal(1, edgeCount)

[<Fact>]
let ``toJson handles complex data types`` () =
    let graph =
        empty Directed
        |> addNode 1 (1, 2)
        |> addNode 2 (3, 4)
        |> addEdge 1 2 (5, 6)

    let json = Json.render graph

    // Data should be converted to strings
    Assert.Contains("(1, 2)", json)
    Assert.Contains("(3, 4)", json)

// ============================================================================
// DEFAULT OPTIONS
// ============================================================================

[<Fact>]
let ``defaultDotOptions has correct values`` () =
    let opts = Dot.defaultOptions<int, int>

    Assert.Equal("ellipse", opts.NodeShape)
    Assert.Equal("red", opts.HighlightColor)
    Assert.Empty(opts.HighlightedNodes)
    Assert.Empty(opts.HighlightedEdges)

[<Fact>]
let ``defaultMermaidOptions has correct values`` () =
    let opts = Mermaid.defaultOptions<int, int>

    Assert.Empty(opts.HighlightedNodes)
    Assert.Empty(opts.HighlightedEdges)

[<Fact>]
let ``defaultDotOptions uses ID as label`` () =
    let opts = Dot.defaultOptions<string, int>
    let label = opts.NodeLabel 42 "data"
    Assert.Equal("42", label)

[<Fact>]
let ``defaultMermaidOptions uses ID as label`` () =
    let opts = Mermaid.defaultOptions<string, int>
    let label = opts.NodeLabel 42 "data"
    Assert.Equal("42", label)
