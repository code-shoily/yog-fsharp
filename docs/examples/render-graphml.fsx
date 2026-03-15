(**
# GraphML Serialization & Deserialization

This example demonstrates exporting and importing graphs using GraphML,
the standard XML format supported by tools like Gephi, yEd, and Cytoscape.

## Serialization
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.IO.GraphML

// Build a small directed graph
let graph =
    empty Directed
    |> addNode 1 "Alice" |> addNode 2 "Bob" |> addNode 3 "Carol"
    |> addEdge 1 2 "friend"
    |> addEdge 2 3 "colleague"
    |> addEdge 1 3 "neighbor"

// Serialize to GraphML XML
let xml = serialize graph
printfn "=== GraphML Output ==="
printfn "%s" xml

(**
## Round-Trip Deserialization

We can parse GraphML back into a Yog graph:
*)

let loaded = deserialize xml

printfn "\n=== Round-Trip Verification ==="
printfn "Nodes: %d" (nodeCount loaded)
printfn "Edges:"
for src in allNodes loaded do
    for (dst, data) in successors src loaded do
        let weight = Map.tryFind "weight" data |> Option.defaultValue "?"
        printfn "  %d -> %d [%s]" src dst weight

(**
## Custom Types with serializeWith

For graphs with structured data, use `serializeWith` to control attribute mapping:
*)

type Person = { Name: string; Role: string }

let team =
    empty Directed
    |> addNode 1 { Name = "Alice"; Role = "Lead" }
    |> addNode 2 { Name = "Bob"; Role = "Dev" }
    |> addEdge 1 2 5

let customXml =
    serializeWith
        (fun p -> ["name", p.Name; "role", p.Role])
        (fun w -> ["weight", string w])
        team

printfn "\n=== Custom Attributes ==="
printfn "%s" customXml

(**
## Output

```
=== GraphML Output ===
<graphml xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://graphml.graphdrawing.org/xmlns">
  <key id="label" for="node" attr.name="label" attr.type="string" />
  <key id="weight" for="edge" attr.name="weight" attr.type="string" />
  <graph id="G" edgedefault="directed">
    <node id="1">
      <data key="label">Alice</data>
    </node>
    <node id="2">
      <data key="label">Bob</data>
    </node>
    <node id="3">
      <data key="label">Carol</data>
    </node>
    <edge source="1" target="2">
      <data key="weight">friend</data>
    </edge>
    <edge source="1" target="3">
      <data key="weight">neighbor</data>
    </edge>
    <edge source="2" target="3">
      <data key="weight">colleague</data>
    </edge>
  </graph>
</graphml>

=== Round-Trip Verification ===
Nodes: 3
Edges:
  1 -> 2 [friend]
  1 -> 3 [neighbor]
  2 -> 3 [colleague]

=== Custom Attributes ===
<graphml xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://graphml.graphdrawing.org/xmlns">
  <key id="name" for="node" attr.name="name" attr.type="string" />
  <key id="role" for="node" attr.name="role" attr.type="string" />
  <key id="weight" for="edge" attr.name="weight" attr.type="string" />
  <graph id="G" edgedefault="directed">
    <node id="1">
      <data key="name">Alice</data>
      <data key="role">Lead</data>
    </node>
    <node id="2">
      <data key="name">Bob</data>
      <data key="role">Dev</data>
    </node>
    <edge source="1" target="2">
      <data key="weight">5</data>
    </edge>
  </graph>
</graphml>
```

## When to Use GraphML

GraphML is ideal when you need to exchange graphs with external tools.
Unlike JSON, it supports typed attributes and namespaces, and is the
native format for tools like **Gephi**, **yEd**, **Cytoscape**, and **NetworkX**.
*)
