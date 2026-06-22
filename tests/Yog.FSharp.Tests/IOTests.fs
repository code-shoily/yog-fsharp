module Yog.FSharp.Tests.IOTests


open Xunit
open System.Text.Json
open Yog.Model
open Yog.IO
open Yog.Render
open Yog
open Yog.Multi
open Yog.Pathfinding.Utils
open Yog.Flow.MaxFlow

// ============================================================================
// DOT RENDERING
// ============================================================================

[<Fact>]
let ``toDot generates digraph for directed`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 5

    let dot = Dot.render Dot.defaultOptions graph

    Assert.Contains("digraph G {", dot)
    Assert.Contains("->", dot)

[<Fact>]
let ``toDot generates graph for undirected`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 5

    let dot = Dot.render Dot.defaultOptions graph

    Assert.Contains("graph G {", dot)
    Assert.Contains("--", dot)

[<Fact>]
let ``toDot includes all nodes`` () =
    let graph =
        empty Directed |> addNode 1 "Start" |> addNode 2 "Process" |> addNode 3 "End"

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
    let graph = empty Directed |> addNode 1 "Alice" |> addNode 2 "Bob" |> addEdge 1 2 1

    let options =
        { Dot.defaultOptions with
            NodeLabel = fun id data -> $"{id}:{data}" }

    let dot = Dot.render options graph

    Assert.Contains("1 [label=\"1:Alice\"]", dot)
    Assert.Contains("2 [label=\"2:Bob\"]", dot)

[<Fact>]
let ``toDot uses custom edge labels`` () =
    let graph = empty Directed |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 42

    let options =
        { Dot.defaultOptions with
            EdgeLabel = fun w -> $"weight={w}" }

    let dot = Dot.render options graph

    Assert.Contains("[label=\"weight=42\"]", dot)

[<Fact>]
let ``toDot highlights specified nodes`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 1

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

    let options =
        { Dot.defaultOptions with
            NodeShape = "box" }

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
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 5

    let mermaid = Mermaid.render Mermaid.defaultOptions graph

    Assert.Contains("graph TD", mermaid)
    Assert.Contains("-->", mermaid)

[<Fact>]
let ``toMermaid uses LR for undirected`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 5

    let mermaid =
        Mermaid.render
            { Mermaid.defaultOptions with
                Direction = "LR" }
            graph

    Assert.Contains("graph LR", mermaid)
    Assert.Contains("---", mermaid)

[<Fact>]
let ``toMermaid includes all nodes`` () =
    let graph = empty Directed |> addNode 1 "Start" |> addNode 2 "End"

    let mermaid = Mermaid.render Mermaid.defaultOptions graph

    Assert.Contains("1[\"1\"]", mermaid)
    Assert.Contains("2[\"2\"]", mermaid)

[<Fact>]
let ``toMermaid includes all edges with weights`` () =
    let graph = empty Directed |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 10

    let mermaid = Mermaid.render Mermaid.defaultOptions graph

    Assert.Contains("1 -->", mermaid)
    Assert.Contains("|\"10\"|", mermaid)
    Assert.Contains("2", mermaid)

[<Fact>]
let ``toMermaid uses custom node labels`` () =
    let graph = empty Directed |> addNode 1 "Alice" |> addNode 2 "Bob" |> addEdge 1 2 1

    let options =
        { Mermaid.defaultOptions with
            NodeLabel = fun id data -> data }

    let mermaid = Mermaid.render options graph

    Assert.Contains("1[\"Alice\"]", mermaid)
    Assert.Contains("2[\"Bob\"]", mermaid)

[<Fact>]
let ``toMermaid highlights nodes`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 1

    let options =
        { Mermaid.defaultOptions with
            HighlightedNodes = Set.ofList [ 1 ] }

    let mermaid = Mermaid.render options graph

    Assert.Contains("classDef highlight", mermaid)
    Assert.Contains(":::highlight", mermaid)

[<Fact>]
let ``toMermaid highlights edges`` () =
    let graph = empty Directed |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1

    let options =
        { Mermaid.defaultOptions with
            HighlightedEdges = Set.ofList [ (1, 2) ] }

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
    let graph = empty Directed |> addNode 1 "Alice" |> addNode 2 "Bob"

    let json = Json.render graph

    Assert.Contains("\"id\": 1", json)
    Assert.Contains("\"id\": 2", json)
    Assert.Contains("\"data\": \"Alice\"", json)
    Assert.Contains("\"data\": \"Bob\"", json)

[<Fact>]
let ``toJson includes all edges`` () =
    let graph = empty Directed |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 42

    let json = Json.render graph

    Assert.Contains("\"source\": 1", json)
    Assert.Contains("\"target\": 2", json)
    Assert.Contains("\"weight\": \"42\"", json)

[<Fact>]
let ``toJson produces valid JSON`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 10

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
    let graph = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 5

    let json = Json.render graph

    // For undirected, should only have one edge entry
    // (not both 1->2 and 2->1)
    let edgeCount = json.Split("source").Length - 1
    Assert.Equal(1, edgeCount)

[<Fact>]
let ``toJson handles complex data types`` () =
    let graph =
        empty Directed |> addNode 1 (1, 2) |> addNode 2 (3, 4) |> addEdge 1 2 (5, 6)

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

// ============================================================================
// TGF SERIALIZATION
// ============================================================================

[<Fact>]
let ``Tgf serialize and parse undirected`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "edge"

    let options =
        { Tgf.defaultOptions with
            NodeLabel = id
            EdgeLabel = Some }

    let tgfStr = Tgf.serialize options graph

    let parsedResult =
        Tgf.parse Undirected id (fun opt -> opt |> Option.defaultValue "") tgfStr

    match parsedResult with
    | Ok parsedGraph ->
        Assert.Equal(2, order parsedGraph)
        Assert.True(hasEdge 1 2 parsedGraph)
        Assert.Equal("edge", edgeData 1 2 parsedGraph |> Option.get)
    | Error msg -> Assert.Fail(msg)

// ============================================================================
// LIST SERIALIZATION
// ============================================================================

[<Fact>]
let ``AdjacencyList parse and serialize`` () =
    let text = "1: 2,5.0 3,10.0\n2: 3,2.0\n3:\n"

    match List.parse Directed true ":" text with
    | Ok graph ->
        Assert.Equal(3, order graph)
        Assert.Equal(5.0, edgeData 1 2 graph |> Option.get)
        Assert.Equal(10.0, edgeData 1 3 graph |> Option.get)
        Assert.Equal(2.0, edgeData 2 3 graph |> Option.get)
        let serialized = List.serialize true ":" graph
        Assert.Contains("1: 2,5.000000 3,10.000000", serialized)
    | Error msg -> Assert.Fail(msg)

// ============================================================================
// MATRIX SERIALIZATION
// ============================================================================

[<Fact>]
let ``Matrix parse and serialize`` () =
    let matrix = [ [ 0.0; 5.0; 0.0 ]; [ 5.0; 0.0; 7.0 ]; [ 0.0; 7.0; 0.0 ] ]

    match Matrix.fromMatrix Undirected matrix with
    | Ok graph ->
        Assert.Equal(3, order graph)
        Assert.Equal(5.0, edgeData 0 1 graph |> Option.get)
        Assert.Equal(7.0, edgeData 1 2 graph |> Option.get)
        let serialized = Matrix.serialize " " graph
        Assert.Contains("0 5 0", serialized)
    | Error msg -> Assert.Fail(msg)

// ============================================================================
// HIGHLIGHT RENDERING TESTS
// ============================================================================

[<Fact>]
let ``pathToOptions highlights nodes and edges correctly`` () =
    let graph =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 1
        |> addEdge 2 3 1

    let path = { Nodes = [ 1; 2; 3 ]; TotalWeight = 2 }

    // Mermaid
    let mermaidOpts = Mermaid.pathToOptions path Mermaid.defaultOptions
    let mermaidStr = Mermaid.render mermaidOpts graph
    Assert.Contains("classDef highlight", mermaidStr)
    Assert.Contains("classDef highlightEdge", mermaidStr)
    Assert.Contains("1[\"1\"]:::highlight", mermaidStr)
    Assert.Contains("1 -->|\"1\"| 2:::highlightEdge", mermaidStr)

    // DOT
    let dotOpts = Dot.pathToOptions path Dot.defaultOptions
    let dotStr = Dot.render dotOpts graph
    Assert.Contains("1 [label=\"1\" fillcolor=\"red\", style=filled]", dotStr)
    Assert.Contains("1 -> 2 [label=\"1\" color=\"red\", penwidth=2]", dotStr)

[<Fact>]
let ``pathToMultiOptions highlights nodes and edge endpoints correctly`` () =
    let graph =
        Yog.Multi.Model.empty Directed
        |> Yog.Multi.Model.addNode 1 "A"
        |> Yog.Multi.Model.addNode 2 "B"

    let graph = Yog.Multi.Model.addEdge 1 2 5 graph |> fst
    let path = { Nodes = [ 1; 2 ]; TotalWeight = 5 }

    // Mermaid
    let mermaidOpts = Mermaid.pathToMultiOptions path Mermaid.defaultMultiOptions
    let mermaidStr = Mermaid.renderMulti mermaidOpts graph
    Assert.Contains("1[\"1\"]:::highlight", mermaidStr)
    Assert.Contains("2[\"2\"]:::highlight", mermaidStr)
    Assert.Contains("1 -->|\"5\"| 2:::highlightEdge", mermaidStr)

    // DOT
    let dotOpts = Dot.pathToMultiOptions path Dot.defaultMultiOptions
    let dotStr = Dot.renderMulti dotOpts graph
    Assert.Contains("1 [label=\"1\" fillcolor=\"red\", style=filled]", dotStr)
    Assert.Contains("1 -> 2 [label=\"5\" color=\"red\", penwidth=2]", dotStr)

[<Fact>]
let ``mstToOptions highlights spanning tree correctly`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 5

    let mstResult: MstResult<int> =
        { Edges = [ { From = 1; To = 2; Weight = 5 } ]
          TotalWeight = 5
          NodeCount = 2
          EdgeCount = 1
          Algorithm = Kruskal
          Root = None }

    // Mermaid
    let mermaidOpts = Mermaid.mstToOptions mstResult Mermaid.defaultOptions
    let mermaidStr = Mermaid.render mermaidOpts graph
    Assert.Contains("1[\"1\"]:::highlight", mermaidStr)
    Assert.Contains("1 ---|\"5\"| 2:::highlightEdge", mermaidStr)

    // DOT
    let dotOpts = Dot.mstToOptions mstResult Dot.defaultOptions
    let dotStr = Dot.render dotOpts graph
    Assert.Contains("1 [label=\"1\" fillcolor=\"red\", style=filled]", dotStr)
    Assert.Contains("1 -- 2 [label=\"5\" color=\"red\", penwidth=2]", dotStr)

[<Fact>]
let ``cutToOptions highlights source and sink partition correctly`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 5

    let minCutResult =
        { SourceSide = Set.ofList [ 1 ]
          SinkSide = Set.ofList [ 2 ] }

    // Mermaid
    let mermaidOpts = Mermaid.cutToOptions minCutResult Mermaid.defaultOptions
    let mermaidStr = Mermaid.render mermaidOpts graph
    Assert.Contains("classDef highlightSource fill:#a8d8ea,stroke:#0288d1,stroke-width:3px", mermaidStr)
    Assert.Contains("classDef highlightSink fill:#f08080,stroke:#c62828,stroke-width:3px", mermaidStr)
    Assert.Contains("1[\"1\"]:::highlightSource", mermaidStr)
    Assert.Contains("2[\"2\"]:::highlightSink", mermaidStr)

    // DOT
    let dotOpts = Dot.cutToOptions minCutResult Dot.defaultOptions
    let dotStr = Dot.render dotOpts graph
    Assert.Contains("1 [label=\"1\" fillcolor=\"#a8d8ea\", style=filled, color=\"#0288d1\"]", dotStr)
    Assert.Contains("2 [label=\"2\" fillcolor=\"#f08080\", style=filled, color=\"#c62828\"]", dotStr)

[<Fact>]
let ``matchingToOptions highlights matching edges correctly`` () =
    let graph = empty Undirected |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 1
    let matching = Map.ofList [ (1, 2); (2, 1) ]

    // Mermaid
    let mermaidOpts = Mermaid.matchingToOptions matching Mermaid.defaultOptions
    let mermaidStr = Mermaid.render mermaidOpts graph
    Assert.Contains("1[\"1\"]:::highlight", mermaidStr)
    Assert.Contains("2[\"2\"]:::highlight", mermaidStr)
    Assert.Contains("1 ---|\"1\"| 2:::highlightEdge", mermaidStr)

    // DOT
    let dotOpts = Dot.matchingToOptions matching Dot.defaultOptions
    let dotStr = Dot.render dotOpts graph
    Assert.Contains("1 [label=\"1\" fillcolor=\"red\", style=filled]", dotStr)
    Assert.Contains("1 -- 2 [label=\"1\" color=\"red\", penwidth=2]", dotStr)

// ============================================================================
// ALIGNED TGF TESTS
// ============================================================================

[<Fact>]
let ``Tgf parseWith parses nodes and edges with warning collection`` () =
    let input = "1 Node One\n2 Node Two\n#\n1 2\n1 3\nmalformed_line\n"

    let parseRes =
        Tgf.parseWith Directed (fun id lbl -> lbl) (fun opt -> opt |> Option.defaultValue "default") input

    match parseRes |> Result.map (fun r -> (r.Graph, r.Warnings)) with
    | Ok(graph, warnings) ->
        Assert.Equal(3, order graph) // Node 3 is auto-created because of 1 3
        Assert.True(hasEdge 1 2 graph)
        Assert.True(hasEdge 1 3 graph)
        Assert.Single(warnings) |> ignore

        match warnings.Head with
        | Tgf.MalformedEdge(lineNum, text) ->
            Assert.Equal(6, lineNum)
            Assert.Equal("malformed_line", text)
        | _ -> Assert.Fail("Expected MalformedEdge warning")
    | Error err -> Assert.Fail(sprintf "Expected successful parse, got %A" err)

[<Fact>]
let ``Tgf parseWith returns duplicate node error`` () =
    let input = "1 Node A\n1 Node B\n#\n1 1\n"

    let parseRes =
        Tgf.parseWith Directed (fun _ lbl -> lbl) (fun opt -> opt |> Option.defaultValue "") input

    match parseRes with
    | Error(Tgf.DuplicateNode(lineNum, id)) ->
        Assert.Equal(2, lineNum)
        Assert.Equal(1, id)
    | _ -> Assert.Fail("Expected DuplicateNode error")

[<Fact>]
let ``Tgf serializeWith formats graph correctly`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "e1"

    let options: Tgf.TgfOptions<string, string> =
        { Tgf.defaultOptions with
            NodeLabel = fun n -> n.ToUpper()
            EdgeLabel = Some
            NodeFormatter = fun s -> "n_" + s
            EdgeFormatter = fun s -> "label_" + s }

    let tgfStr = Tgf.serializeWith options graph
    Assert.Contains("n_1 n_A", tgfStr)
    Assert.Contains("n_2 n_B", tgfStr)
    Assert.Contains("n_1 n_2 label_e1", tgfStr)

// ============================================================================
// ALIGNED LIST TESTS
// ============================================================================

[<Fact>]
let ``List fromList and toList parity`` () =
    let entries = [ (1, [ (2, 5.0); (3, 10.0) ]); (2, [ (3, 2.0) ]); (3, []) ]

    let graph =
        List.fromList Directed (entries |> Seq.map (fun (id, nbrs) -> (id, nbrs |> Seq.ofList)))

    Assert.Equal(3, order graph)
    Assert.Equal(5.0, edgeData 1 2 graph |> Option.get)
    let exported = List.toList graph
    Assert.Equal(3, exported.Length)
    Assert.Equal(1, fst exported.[0])
    Assert.Equal(2, (snd exported.[0]).Length)

[<Fact>]
let ``List toString with custom options`` () =
    let graph = empty Directed |> addNode 1 () |> addNode 2 () |> addEdge 1 2 4.5

    let options =
        { List.defaultOptions with
            Weighted = true
            Delimiter = "->"
            NodeFormatter = fun id -> "node" + id.ToString()
            WeightFormatter = fun w -> sprintf "w%.1f" w }

    let serialized = List.toString options graph
    Assert.Contains("node1-> node2,w4.5", serialized)

// ============================================================================
// EDGELIST TESTS
// ============================================================================

[<Fact>]
let ``Edgelist parse and serialize unweighted`` () =
    let input = "1 2\n2 3\n# comment line\n3 1"

    match Edgelist.parse Directed false input with
    | Ok graph ->
        Assert.Equal(3, order graph)
        Assert.True(hasEdge 1 2 graph)
        Assert.True(hasEdge 2 3 graph)
        Assert.True(hasEdge 3 1 graph)
        let serialized = Edgelist.serialize false graph
        Assert.Contains("1 2", serialized)
        Assert.Contains("2 3", serialized)
        Assert.Contains("3 1", serialized)
    | Error msg -> Assert.Fail(msg)

[<Fact>]
let ``Edgelist parse and serialize weighted`` () =
    let input = "1 2 5.5\n2 3 10\n"

    match Edgelist.parse Directed true input with
    | Ok graph ->
        Assert.Equal(3, order graph)
        Assert.Equal(5.5, edgeData 1 2 graph |> Option.get)
        Assert.Equal(10.0, edgeData 2 3 graph |> Option.get)
        let serialized = Edgelist.serialize true graph
        Assert.Contains("1 2 5.5", serialized)
        Assert.Contains("2 3 10", serialized)
    | Error msg -> Assert.Fail(msg)
