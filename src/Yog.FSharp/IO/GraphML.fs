/// GraphML (Graph Markup Language) serialization support.
///
/// Provides functions to serialize and deserialize graphs in the GraphML format,
/// an XML-based format widely supported by graph visualization and analysis tools
/// like Gephi, yEd, Cytoscape, and NetworkX.
///
/// ## Example
///
/// ```fsharp
/// open Yog.IO
/// open Yog.Model
///
/// // Create a simple graph
/// let graph =
///     empty Directed
///     |> addNode 1 "Alice"
///     |> addNode 2 "Bob"
///     |> addEdge 1 2 5
///
/// // Serialize to GraphML
/// let xml = GraphML.serialize graph
/// File.WriteAllText("graph.graphml", xml)
///
/// // Deserialize from GraphML
/// let loaded = File.ReadAllText("graph.graphml") |> GraphML.deserialize
/// ```
module Yog.IO.GraphML

open System.IO
open System.Xml.Linq
open Yog.Model

// Standard GraphML Namespace
let private ns = XNamespace.Get("http://graphml.graphdrawing.org/xmlns")

// =============================================================================
// XML Helpers
// =============================================================================

/// Helper to extract data dictionary from an element safely
let private getData (el: XElement) =
    el.Elements(ns + "data")
    |> Seq.choose (fun d ->
        let attr = d.Attribute(XName.Get "key")
        if isNull attr then None else Some (attr.Value, d.Value))
    |> Map.ofSeq

// =============================================================================
// Serialization
// =============================================================================

/// Renders a graph to a GraphML string with custom attribute mappers.
///
/// This function allows you to control how node and edge data are converted
/// to GraphML attributes. Use `serialize` for simple cases where node/edge
/// data are strings.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
/// ```fsharp
/// type Person = { Name: string; Age: int }
/// type Connection = { Weight: int; Type: string }
///
/// let graph =
///     empty Directed
///     |> addNode 1 { Name = "Alice"; Age = 30 }
///     |> addNode 2 { Name = "Bob"; Age = 25 }
///     |> addEdge 1 2 { Weight = 5; Type = "friend" }
///
/// let nodeAttrs p = ["name", p.Name; "age", string p.Age]
/// let edgeAttrs c = ["weight", string c.Weight; "type", c.Type]
///
/// let xml = GraphML.serializeWith nodeAttrs edgeAttrs graph
/// ```
///
/// ## Use Cases
///
/// - Exporting graphs with complex data types
/// - Interoperability with tools like Gephi, yEd, Cytoscape
/// - Preserving node and edge attributes in XML format
let serializeWith
    (nodeAttr: 'n -> (string * string) list)
    (edgeAttr: 'e -> (string * string) list)
    (graph: Graph<'n, 'e>) : string =

    // 1. Collect unique keys for the header schema
    let nodeKeys = graph.Nodes |> Map.toSeq |> Seq.collect (fun (_, d) -> nodeAttr d) |> Seq.map fst |> Seq.distinct
    let edgeKeys = graph.OutEdges |> Map.toSeq |> Seq.collect (fun (_, m) -> m |> Map.toSeq |> Seq.collect (fun (_, d) -> edgeAttr d)) |> Seq.map fst |> Seq.distinct

    let createKey target name =
        XElement(ns + "key",
            XAttribute(XName.Get "id", name),
            XAttribute(XName.Get "for", target),
            XAttribute(XName.Get "attr.name", name),
            XAttribute(XName.Get "attr.type", "string"))

    // 2. Build Nodes
    let nodes =
        graph.Nodes
        |> Map.toSeq
        |> Seq.map (fun (id, data) ->
            let el = XElement(ns + "node", XAttribute(XName.Get "id", string id))
            nodeAttr data |> List.iter (fun (k, v) ->
                el.Add(XElement(ns + "data", XAttribute(XName.Get "key", k), v)))
            el)

    // 3. Build Edges
    let edges =
        graph.OutEdges
        |> Map.toSeq
        |> Seq.collect (fun (src, targets) ->
            targets |> Map.toSeq |> Seq.map (fun (dst, data) ->
                let el = XElement(ns + "edge",
                            XAttribute(XName.Get "source", string src),
                            XAttribute(XName.Get "target", string dst))
                edgeAttr data |> List.iter (fun (k, v) ->
                    el.Add(XElement(ns + "data", XAttribute(XName.Get "key", k), v)))
                el))

    // 4. Final Assembly
    let root =
        XElement(ns + "graphml",
            XAttribute(XNamespace.Xmlns + "xsi", "http://www.w3.org/2001/XMLSchema-instance"),
            nodeKeys |> Seq.map (createKey "node"),
            edgeKeys |> Seq.map (createKey "edge"),
            XElement(ns + "graph",
                XAttribute(XName.Get "id", "G"),
                XAttribute(XName.Get "edgedefault", (if graph.Kind = Directed then "directed" else "undirected")),
                nodes,
                edges))

    root.ToString()

/// Serializes a graph to a GraphML string.
///
/// This is a simplified version of `serializeWith` for graphs where
/// node data and edge data are already strings.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
/// ```fsharp
/// let graph =
///     empty Directed
///     |> addNode 1 "Alice"
///     |> addNode 2 "Bob"
///     |> addEdge 1 2 "5"
///
/// let xml = GraphML.serialize graph
/// // <?xml version="1.0" encoding="utf-8"?>
/// // <graphml xmlns="http://graphml.graphdrawing.org/xmlns">
/// //   <key id="label" for="node" attr.name="label" attr.type="string" />
/// //   <key id="weight" for="edge" attr.name="weight" attr.type="string" />
/// //   <graph id="G" edgedefault="directed">
/// //     <node id="1"><data key="label">Alice</data></node>
/// //     <node id="2"><data key="label">Bob</data></node>
/// //     <edge source="1" target="2"><data key="weight">5</data></edge>
/// //   </graph>
/// // </graphml>
/// ```
let serialize (graph: Graph<string, string>) : string =
    serializeWith (fun d -> ["label", d]) (fun w -> ["weight", w]) graph

/// Writes a graph to a GraphML file.
///
/// ## Example
///
/// ```fsharp
/// let graph =
///     empty Directed
///     |> addNode 1 "Start"
///     |> addNode 2 "End"
///     |> addEdge 1 2 "connection"
///
/// GraphML.writeFile "mygraph.graphml" graph
/// ```
let writeFile (path: string) (graph: Graph<string, string>) : unit =
    File.WriteAllText(path, serialize graph)

/// Writes a graph to a GraphML file with custom attribute mappers.
///
/// ## Example
///
/// ```fsharp
/// type Person = { Name: string; Age: int }
///
/// let graph =
///     empty Directed
///     |> addNode 1 { Name = "Alice"; Age = 30 }
///     |> addEdge 1 2 "friend"
///
/// GraphML.writeFileWith
///     (fun p -> ["name", p.Name; "age", string p.Age])
///     (fun e -> ["type", e])
///     "people.graphml"
///     graph
/// ```
let writeFileWith
    (nodeAttr: 'n -> (string * string) list)
    (edgeAttr: 'e -> (string * string) list)
    (path: string)
    (graph: Graph<'n, 'e>) : unit =
    File.WriteAllText(path, serializeWith nodeAttr edgeAttr graph)

// =============================================================================
// Deserialization
// =============================================================================

/// Deserializes GraphML string back into a Yog Graph with custom data mappers.
///
/// This function allows you to control how GraphML attributes are converted
/// to your node and edge data types. Use `deserialize` for simple cases
/// where node/edge data are strings.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
/// ```fsharp
/// type Person = { Name: string; Age: int }
///
/// let nodeFolder (data: Map<string, string>) =
///     { Name = Map.tryFind "name" data |> Option.defaultValue ""
///       Age = Map.tryFind "age" data |> Option.map int |> Option.defaultValue 0 }
///
/// let edgeFolder (data: Map<string, string>) =
///     Map.tryFind "type" data |> Option.defaultValue ""
///
/// let xml = File.ReadAllText("people.graphml")
/// let graph : Graph<Person, string> = GraphML.deserializeWith nodeFolder edgeFolder xml
/// ```
let deserializeWith
    (nodeFolder: Map<string, string> -> 'n)
    (edgeFolder: Map<string, string> -> 'e)
    (xml: string) : Graph<'n, 'e> =

    let doc = XDocument.Parse(xml)
    let gEl = doc.Descendants(ns + "graph") |> Seq.head
    let isDirected = 
        match gEl.Attribute(XName.Get "edgedefault") with
        | null -> false
        | attr -> attr.Value = "directed"

    let mutable g = empty (if isDirected then Directed else Undirected)

    for nEl in gEl.Descendants(ns + "node") do
        let id = int (nEl.Attribute(XName.Get "id").Value)
        g <- addNode id (getData nEl |> nodeFolder) g

    for eEl in gEl.Descendants(ns + "edge") do
        let src = int (eEl.Attribute(XName.Get "source").Value)
        let dst = int (eEl.Attribute(XName.Get "target").Value)
        g <- addEdge src dst (getData eEl |> edgeFolder) g

    g

/// Deserializes a GraphML string to a graph.
///
/// This is a simplified version of `deserializeWith` for graphs where
/// you want node data and edge data as string maps containing all attributes.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
/// ```fsharp
/// let xml = File.ReadAllText("graph.graphml")
/// let graph = GraphML.deserialize xml
///
/// // Access node data
/// let node1Data = graph.Nodes.[1]  // Map<string, string>
/// let label = Map.tryFind "label" node1Data
/// ```
let deserialize (xml: string) : Graph<Map<string, string>, Map<string, string>> =
    deserializeWith id id xml

/// Reads a graph from a GraphML file.
///
/// ## Example
///
/// ```fsharp
/// let graph : Graph<Map<string, string>, Map<string, string>> = GraphML.readFile "graph.graphml"
///
/// // Access node data
/// for (id, data) in Map.toSeq graph.Nodes do
///     printfn "Node %d: %A" id data
/// ```
let readFile (path: string) : Graph<Map<string, string>, Map<string, string>> =
    File.ReadAllText(path) |> deserialize

/// Reads a graph from a GraphML file with custom data mappers.
///
/// ## Example
///
/// ```fsharp
/// type Person = { Name: string; Age: int }
///
/// let nodeFolder (data: Map<string, string>) =
///     { Name = Map.tryFind "name" data |> Option.defaultValue ""
///       Age = Map.tryFind "age" data |> Option.map int |> Option.defaultValue 0 }
///
/// let edgeFolder (data: Map<string, string>) =
///     Map.tryFind "weight" data |> Option.map int |> Option.defaultValue 0
///
/// let graph : Graph<Person, int> = GraphML.readFileWith nodeFolder edgeFolder "people.graphml"
/// ```
let readFileWith
    (nodeFolder: Map<string, string> -> 'n)
    (edgeFolder: Map<string, string> -> 'e)
    (path: string) : Graph<'n, 'e> =
    File.ReadAllText(path) |> deserializeWith nodeFolder edgeFolder
