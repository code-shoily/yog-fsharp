module Yog.FSharp.Tests.GraphMLTests


open System.IO
open Xunit
open Yog.Model
open Yog.IO.GraphML

// Type definitions for tests
type Person = { Name: string; Age: int }
type Connection = { Weight: int; Type: string }

// ============================================================================
// SERIALIZATION
// ============================================================================

[<Fact>]
let ``serialize creates directed graph element`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "5"

    let xml = serialize graph

    Assert.Contains("edgedefault=\"directed\"", xml)

[<Fact>]
let ``serialize creates undirected graph element`` () =
    let graph = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "5"

    let xml = serialize graph

    Assert.Contains("edgedefault=\"undirected\"", xml)

[<Fact>]
let ``serialize includes all nodes`` () =
    let graph = empty Directed |> addNode 1 "Start" |> addNode 2 "End"

    let xml = serialize graph

    Assert.Contains("<node id=\"1\">", xml)
    Assert.Contains("<node id=\"2\">", xml)

[<Fact>]
let ``serialize includes node data`` () =
    let graph = empty Directed |> addNode 1 "Alice" |> addNode 2 "Bob"

    let xml = serialize graph

    Assert.Contains(">Alice</data>", xml)
    Assert.Contains(">Bob</data>", xml)

[<Fact>]
let ``serialize includes all edges`` () =
    let graph =
        empty Directed
        |> addNode 1 ""
        |> addNode 2 ""
        |> addNode 3 ""
        |> addEdge 1 2 "10"
        |> addEdge 2 3 "20"

    let xml = serialize graph

    Assert.Contains("source=\"1\"", xml)
    Assert.Contains("target=\"2\"", xml)
    Assert.Contains("source=\"2\"", xml)
    Assert.Contains("target=\"3\"", xml)

[<Fact>]
let ``serialize includes edge data`` () =
    let graph = empty Directed |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 "42"

    let xml = serialize graph

    Assert.Contains(">42</data>", xml)

[<Fact>]
let ``serialize empty graph`` () =
    let graph = empty Directed
    let xml = serialize graph

    Assert.Contains("<graphml", xml)
    Assert.Contains("<graph", xml)
    Assert.Contains("edgedefault=\"directed\"", xml)

[<Fact>]
let ``serialize produces valid XML`` () =
    let graph = empty Directed |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "5"

    let xml = serialize graph

    // Should be parseable as XML
    let doc = System.Xml.Linq.XDocument.Parse(xml)
    Assert.NotNull(doc)

// ============================================================================
// SERIALIZATION WITH CUSTOM MAPPERS
// ============================================================================

[<Fact>]
let ``serializeWith creates custom node attributes`` () =
    let graph = empty Directed |> addNode 1 "Alice" |> addNode 2 "Bob"

    let nodeAttr name = [ "name", name; "type", "person" ]
    let edgeAttr _ = []

    let xml = serializeWith nodeAttr edgeAttr graph

    Assert.Contains("attr.name=\"name\"", xml)
    Assert.Contains("attr.name=\"type\"", xml)
    Assert.Contains(">Alice</data>", xml)
    Assert.Contains(">person</data>", xml)

[<Fact>]
let ``serializeWith creates custom edge attributes`` () =
    let graph = empty Directed |> addNode 1 "" |> addNode 2 "" |> addEdge 1 2 42

    let nodeAttr _ = []

    let edgeAttr weight =
        [ "weight", string weight; "label", $"w={weight}" ]

    let xml = serializeWith nodeAttr edgeAttr graph

    Assert.Contains("attr.name=\"weight\"", xml)
    Assert.Contains(">42</data>", xml)
    Assert.Contains(">w=42</data>", xml)

[<Fact>]
let ``serializeWith handles complex data types`` () =
    let graph =
        empty Directed
        |> addNode 1 { Name = "Alice"; Age = 30 }
        |> addNode 2 { Name = "Bob"; Age = 25 }
        |> addEdge 1 2 "friend"

    let nodeAttr p = [ "name", p.Name; "age", string p.Age ]
    let edgeAttr e = [ "type", e ]

    let xml = serializeWith nodeAttr edgeAttr graph

    Assert.Contains(">Alice</data>", xml)
    Assert.Contains(">30</data>", xml)
    Assert.Contains(">Bob</data>", xml)
    Assert.Contains(">friend</data>", xml)

// ============================================================================
// DESERIALIZATION
// ============================================================================

[<Fact>]
let ``deserialize creates directed graph`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="directed">
    <node id="1"><data key="label">A</data></node>
    <node id="2"><data key="label">B</data></node>
    <edge source="1" target="2"><data key="weight">5</data></edge>
  </graph>
</graphml>"""

    let graph = deserialize xml

    Assert.Equal(Directed, graph.Kind)
    Assert.Equal(2, order graph)
    Assert.Equal(1, edgeCount graph)

[<Fact>]
let ``deserialize creates undirected graph`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="undirected">
    <node id="1"><data key="label">A</data></node>
    <node id="2"><data key="label">B</data></node>
    <edge source="1" target="2"><data key="weight">5</data></edge>
  </graph>
</graphml>"""

    let graph = deserialize xml

    Assert.Equal(Undirected, graph.Kind)

[<Fact>]
let ``deserialize restores node data`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="directed">
    <node id="1"><data key="label">Alice</data></node>
    <node id="2"><data key="label">Bob</data></node>
  </graph>
</graphml>"""

    let graph = deserialize xml

    let node1Data = graph.Nodes.[1]
    let node2Data = graph.Nodes.[2]

    Assert.Equal("Alice", Map.find "label" node1Data)
    Assert.Equal("Bob", Map.find "label" node2Data)

[<Fact>]
let ``deserialize restores edge data`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="directed">
    <node id="1"><data key="label">A</data></node>
    <node id="2"><data key="label">B</data></node>
    <edge source="1" target="2"><data key="weight">42</data></edge>
  </graph>
</graphml>"""

    let graph = deserialize xml

    let edgeData = graph.OutEdges.[1].[2]
    Assert.Equal("42", Map.find "weight" edgeData)

[<Fact>]
let ``deserialize restores multiple edges`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="directed">
    <node id="1"/><node id="2"/><node id="3"/>
    <edge source="1" target="2"/>
    <edge source="2" target="3"/>
    <edge source="1" target="3"/>
  </graph>
</graphml>"""

    let graph = deserialize xml

    Assert.Equal(3, order graph)
    Assert.Equal(3, edgeCount graph)
    Assert.True(Map.containsKey 2 graph.OutEdges.[1])
    Assert.True(Map.containsKey 3 graph.OutEdges.[2])
    Assert.True(Map.containsKey 3 graph.OutEdges.[1])

[<Fact>]
let ``deserialize handles empty graph`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="directed">
  </graph>
</graphml>"""

    let graph = deserialize xml

    Assert.Equal(0, order graph)
    Assert.Equal(0, edgeCount graph)

// ============================================================================
// DESERIALIZATION WITH CUSTOM MAPPERS
// ============================================================================

[<Fact>]
let ``deserializeWith reconstructs custom types`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="directed">
    <node id="1">
      <data key="name">Alice</data>
      <data key="age">30</data>
    </node>
    <node id="2">
      <data key="name">Bob</data>
      <data key="age">25</data>
    </node>
    <edge source="1" target="2">
      <data key="weight">5</data>
    </edge>
  </graph>
</graphml>"""

    let nodeFolder (data: Map<string, string>) =
        { Name = Map.tryFind "name" data |> Option.defaultValue ""
          Age = Map.tryFind "age" data |> Option.map int |> Option.defaultValue 0 }

    let edgeFolder (data: Map<string, string>) =
        Map.tryFind "weight" data |> Option.map int |> Option.defaultValue 0

    let graph: Graph<Person, int> = deserializeWith nodeFolder edgeFolder xml

    Assert.Equal("Alice", graph.Nodes.[1].Name)
    Assert.Equal(30, graph.Nodes.[1].Age)
    Assert.Equal("Bob", graph.Nodes.[2].Name)
    Assert.Equal(25, graph.Nodes.[2].Age)
    Assert.Equal(5, graph.OutEdges.[1].[2])

// ============================================================================
// ROUND-TRIP TESTS
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

    let xml = serialize original
    let restored = deserialize xml

    Assert.Equal(original.Kind, restored.Kind)
    Assert.Equal(order original, order restored)
    Assert.Equal(edgeCount original, edgeCount restored)
    Assert.True(Map.containsKey 1 restored.Nodes)
    Assert.True(Map.containsKey 2 restored.Nodes)
    Assert.True(Map.containsKey 3 restored.Nodes)

[<Fact>]
let ``round-trip preserves undirected graph structure`` () =
    let original = empty Undirected |> addNode 1 "A" |> addNode 2 "B" |> addEdge 1 2 "5"

    let xml = serialize original
    let restored = deserialize xml

    Assert.Equal(original.Kind, restored.Kind)
    Assert.Equal(order original, order restored)

[<Fact>]
let ``round-trip with custom mappers preserves data`` () =
    let original =
        empty Directed
        |> addNode 1 { Name = "Alice"; Age = 30 }
        |> addNode 2 { Name = "Bob"; Age = 25 }
        |> addEdge 1 2 "friend"

    let nodeAttr p = [ "name", p.Name; "age", string p.Age ]
    let edgeAttr e = [ "type", e ]

    let xml = serializeWith nodeAttr edgeAttr original

    let nodeFolder (data: Map<string, string>) =
        { Name = Map.tryFind "name" data |> Option.defaultValue ""
          Age = Map.tryFind "age" data |> Option.map int |> Option.defaultValue 0 }

    let edgeFolder (data: Map<string, string>) =
        Map.tryFind "type" data |> Option.defaultValue ""

    let restored: Graph<Person, string> = deserializeWith nodeFolder edgeFolder xml

    Assert.Equal(original.Nodes.[1], restored.Nodes.[1])
    Assert.Equal(original.Nodes.[2], restored.Nodes.[2])
    Assert.Equal(original.OutEdges.[1].[2], restored.OutEdges.[1].[2])

// ============================================================================
// FILE I/O TESTS
// ============================================================================

[<Fact>]
let ``writeFile and readFile round-trip`` () =
    let path = Path.GetTempFileName()

    try
        let original =
            empty Directed |> addNode 1 "Alice" |> addNode 2 "Bob" |> addEdge 1 2 "5"

        writeFile path original
        let restored = readFile path

        Assert.Equal(order original, order restored)
        Assert.Equal(edgeCount original, edgeCount restored)
        Assert.Equal(original.Kind, restored.Kind)
        Assert.Equal(original.Nodes.[1], Map.find "label" restored.Nodes.[1])
    finally
        if File.Exists(path) then
            File.Delete(path)

[<Fact>]
let ``writeFileWith and readFileWith round-trip`` () =
    let path = Path.GetTempFileName()

    try
        let original =
            empty Directed
            |> addNode 1 { Name = "Alice"; Age = 30 }
            |> addNode 2 { Name = "Bob"; Age = 25 }
            |> addEdge 1 2 "friend"

        let nodeAttr p = [ "name", p.Name; "age", string p.Age ]
        let edgeAttr e = [ "type", e ]

        writeFileWith nodeAttr edgeAttr path original

        let nodeFolder (data: Map<string, string>) =
            { Name = Map.tryFind "name" data |> Option.defaultValue ""
              Age = Map.tryFind "age" data |> Option.map int |> Option.defaultValue 0 }

        let edgeFolder (data: Map<string, string>) =
            Map.tryFind "type" data |> Option.defaultValue ""

        let restored: Graph<Person, string> = readFileWith nodeFolder edgeFolder path

        Assert.Equal(original.Nodes.[1], restored.Nodes.[1])
        Assert.Equal(original.Nodes.[2], restored.Nodes.[2])
    finally
        if File.Exists(path) then
            File.Delete(path)

[<Fact>]
let ``readFile handles non-existent file`` () =
    let path =
        Path.Combine(Path.GetTempPath(), $"nonexistent-{System.Guid.NewGuid()}.graphml")

    Assert.Throws<FileNotFoundException>(fun () -> readFile path |> ignore)

// ============================================================================
// EDGE CASES
// ============================================================================

[<Fact>]
let ``serialize handles isolated nodes`` () =
    let graph =
        empty Directed
        |> addNode 1 "Isolated"
        |> addNode 2 "Connected"
        |> addNode 3 "Missing"
        |> addEdge 2 3 "to nowhere"

    let xml = serialize graph
    let restored = deserialize xml

    Assert.Equal(order graph, order restored)
    // Isolated node 1 should still be present
    Assert.True(Map.containsKey 1 restored.Nodes)

[<Fact>]
let ``deserialize handles nodes without data elements`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="directed">
    <node id="1"/>
    <node id="2"/>
    <edge source="1" target="2"/>
  </graph>
</graphml>"""

    let graph = deserialize xml

    Assert.Equal(2, order graph)
    // Nodes should have empty maps
    Assert.Empty(graph.Nodes.[1])
    Assert.Empty(graph.Nodes.[2])

[<Fact>]
let ``deserialize handles edges without data elements`` () =
    let xml =
        """<?xml version="1.0" encoding="UTF-8"?>
<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
  <graph id="G" edgedefault="directed">
    <node id="1"/><node id="2"/>
    <edge source="1" target="2"/>
  </graph>
</graphml>"""

    let graph = deserialize xml

    Assert.Equal(1, edgeCount graph)
    // Edge should have empty map
    Assert.Empty(graph.OutEdges.[1].[2])

[<Fact>]
let ``serialize handles special characters in data`` () =
    let graph =
        empty Directed |> addNode 1 "A & B" |> addNode 2 "Test" |> addEdge 1 2 "<edge>"

    let xml = serialize graph

    // Should produce valid XML that can be parsed
    let restored = deserialize xml

    // Note: XML encoding may change the format, but content should be preserved
    Assert.Equal(2, order restored)
