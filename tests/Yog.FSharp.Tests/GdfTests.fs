module Yog.FSharp.Tests.GdfTests


open System.IO
open Xunit
open Yog.Model
open Yog.IO.Gdf

// ============================================================================
// SERIALIZATION
// ============================================================================

[<Fact>]
let ``serialize creates nodedef and edgedef sections`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "5"

    let gdf = serialize graph

    Assert.Contains("nodedef>name", gdf)
    Assert.Contains("edgedef>node1", gdf)

[<Fact>]
let ``serialize includes all nodes`` () =
    let graph = empty Directed |> addNode 1 "Alice" |> addNode 2 "Bob"

    let gdf = serialize graph

    Assert.Contains("1,Alice", gdf)
    Assert.Contains("2,Bob", gdf)

[<Fact>]
let ``serialize includes all edges`` () =
    let graph =
        empty Directed
        |> addNode 1 ""
        |> addNode 2 ""
        |> addNode 3 ""
        |> addEdge 1 2 "10"
        |> addEdge 2 3 "20"

    let gdf = serialize graph

    Assert.Contains("1,2,true,10", gdf)
    Assert.Contains("2,3,true,20", gdf)

[<Fact>]
let ``serialize always includes directed column`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "x"

    let gdf = serialize graph

    Assert.Contains("directed BOOLEAN", gdf)
    Assert.Contains(",true,", gdf)

[<Fact>]
let ``serialize handles undirected graphs with false directed`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "x"

    let gdf = serialize graph

    Assert.Contains("directed BOOLEAN", gdf)
    Assert.Contains(",false,", gdf)

[<Fact>]
let ``serializeWith collects attributes from all nodes`` () =
    let nodeAttr (v: int) =
        if v > 10 then
            [ "label", string v; "extra", "yes" ]
        else
            [ "label", string v ]

    let graph = empty Directed |> addNode 1 5 |> addNode 2 15

    let gdf = serializeWith nodeAttr (fun _ -> []) defaultOptions graph

    // "extra" column should appear even though only node 2 has it
    Assert.Contains("extra", gdf)

[<Fact>]
let ``serialize empty graph`` () =
    let graph: Graph<string, string> = empty Directed
    let gdf = serialize graph

    Assert.Contains("nodedef>name", gdf)
    Assert.Contains("edgedef>node1", gdf)

[<Fact>]
let ``serializeWeighted uses weight column`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 42

    let gdf = serializeWeighted graph

    Assert.Contains("weight", gdf)
    Assert.Contains("42", gdf)

[<Fact>]
let ``serialize escapes values with separator`` () =
    let graph =
        empty Directed |> addNode 1 "Hello,World" |> addNode 2 "B" |> addEdge 1 2 "x"

    let gdf = serialize graph

    Assert.Contains("\"Hello,World\"", gdf)

[<Fact>]
let ``serialize includes type annotations by default`` () =
    let graph = empty Directed |> addNode 1 "A" |> addEdge 1 1 "x"

    let gdf = serialize graph

    Assert.Contains("VARCHAR", gdf)
    Assert.Contains("BOOLEAN", gdf)

[<Fact>]
let ``serializeWithOptions can disable type annotations`` () =
    let graph = empty Directed |> addNode 1 "A" |> addEdge 1 1 "x"

    let gdf =
        serializeWithOptions
            { defaultOptions with
                IncludeTypes = false }
            graph

    Assert.DoesNotContain("VARCHAR", gdf)
    Assert.DoesNotContain("BOOLEAN", gdf)

// ============================================================================
// DESERIALIZATION
// ============================================================================

[<Fact>]
let ``deserialize parses simple GDF`` () =
    let gdf =
        "nodedef>name VARCHAR,label VARCHAR\n1,Alice\n2,Bob\nedgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN,label VARCHAR\n1,2,true,friend"

    let result = deserialize gdf

    match result with
    | Ok graph ->
        Assert.Equal(2, order graph)
        Assert.Equal(1, edgeCount graph)
    | Error msg -> failwith msg

[<Fact>]
let ``deserialize detects directed from column`` () =
    let gdf =
        "nodedef>name VARCHAR,label VARCHAR\n1,A\nedgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN\n1,1,true"

    match deserialize gdf with
    | Ok graph -> Assert.Equal(Directed, graph.Kind)
    | Error msg -> failwith msg

[<Fact>]
let ``deserialize detects undirected from column`` () =
    let gdf =
        "nodedef>name VARCHAR,label VARCHAR\n1,A\n2,B\nedgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN\n1,2,false"

    match deserialize gdf with
    | Ok graph -> Assert.Equal(Undirected, graph.Kind)
    | Error msg -> failwith msg

[<Fact>]
let ``deserialize handles quoted values`` () =
    let gdf =
        "nodedef>name VARCHAR,label VARCHAR\n1,\"Hello,World\"\nedgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN\n1,1,true"

    match deserialize gdf with
    | Ok graph ->
        let data = graph.Nodes.[1]
        Assert.Equal("Hello,World", Map.find "label" data)
    | Error msg -> failwith msg

[<Fact>]
let ``deserialize auto-creates nodes for edges`` () =
    let gdf =
        "nodedef>name VARCHAR,label VARCHAR\n1,A\nedgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN\n1,2,true"

    match deserialize gdf with
    | Ok graph ->
        Assert.Equal(2, order graph)
        Assert.True(Map.containsKey 2 graph.Nodes)
    | Error msg -> failwith msg

[<Fact>]
let ``deserializeWith uses custom mappers`` () =
    let gdf =
        "nodedef>name VARCHAR,label VARCHAR,age VARCHAR\n1,Alice,30\n2,Bob,25\nedgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN,weight VARCHAR\n1,2,true,5"

    let nodeFolder (attrs: Map<string, string>) =
        Map.tryFind "label" attrs |> Option.defaultValue ""

    let edgeFolder (attrs: Map<string, string>) =
        Map.tryFind "weight" attrs |> Option.map int |> Option.defaultValue 0

    match deserializeWith nodeFolder edgeFolder gdf with
    | Ok graph ->
        Assert.Equal("Alice", graph.Nodes.[1])
        Assert.Equal("Bob", graph.Nodes.[2])
        Assert.Equal(5, graph.OutEdges.[1].[2])
    | Error msg -> failwith msg

[<Fact>]
let ``deserialize returns Error for missing nodedef`` () =
    let gdf = "edgedef>node1 VARCHAR,node2 VARCHAR\n1,2"

    match deserialize gdf with
    | Ok _ -> failwith "Expected Error"
    | Error msg -> Assert.Contains("nodedef", msg)

[<Fact>]
let ``deserialize handles no edge section`` () =
    let gdf = "nodedef>name VARCHAR,label VARCHAR\n1,Alice\n2,Bob"

    match deserialize gdf with
    | Ok graph ->
        Assert.Equal(2, order graph)
        Assert.Equal(0, edgeCount graph)
    | Error msg -> failwith msg

// ============================================================================
// ROUND-TRIP
// ============================================================================

[<Fact>]
let ``round-trip preserves directed graph structure`` () =
    let original =
        empty Directed
        |> addNode 1 "A"
        |> addNode 2 "B"
        |> addNode 3 "C"
        |> addEdge 1 2 "10"
        |> addEdge 2 3 "20"

    let gdf = serialize original

    match deserialize gdf with
    | Ok restored ->
        Assert.Equal(original.Kind, restored.Kind)
        Assert.Equal(order original, order restored)
        Assert.Equal(edgeCount original, edgeCount restored)
    | Error msg -> failwith msg

[<Fact>]
let ``round-trip preserves undirected graph structure`` () =
    let original = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "5"

    let gdf = serialize original

    match deserialize gdf with
    | Ok restored ->
        Assert.Equal(original.Kind, restored.Kind)
        Assert.Equal(order original, order restored)
    | Error msg -> failwith msg

// ============================================================================
// FILE I/O
// ============================================================================

[<Fact>]
let ``writeFile and readFile round-trip`` () =
    let path = Path.GetTempFileName()

    try
        let original =
            empty Directed |> addNode 1 "Alice" |> addNode 2 "Bob" |> addEdge 1 2 "friend"

        writeFile path original

        match readFile path with
        | Ok restored ->
            Assert.Equal(order original, order restored)
            Assert.Equal(edgeCount original, edgeCount restored)
            Assert.Equal(original.Kind, restored.Kind)
        | Error msg -> failwith msg
    finally
        if File.Exists(path) then
            File.Delete(path)

[<Fact>]
let ``readFile returns Error for non-existent file`` () =
    let path =
        Path.Combine(Path.GetTempPath(), $"nonexistent-{System.Guid.NewGuid()}.gdf")

    match readFile path with
    | Ok _ -> failwith "Expected Error"
    | Error _ -> ()
