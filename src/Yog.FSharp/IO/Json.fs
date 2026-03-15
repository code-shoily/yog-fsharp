/// JSON graph serialization.
///
/// Provides functions to export graphs in JSON format for data interchange
/// and web applications.
///
/// ## Example
///
/// ```fsharp
/// open Yog.IO
/// open Yog.Model
///
/// let graph =
///     empty Directed
///     |> addNode 1 "Start"
///     |> addNode 2 "End"
///     |> addEdge 1 2 5
///
/// // Export to JSON
/// let json = Json.render graph
/// File.WriteAllText("graph.json", json)
/// ```
module Yog.IO.Json

open System.IO
open System.Text
open System.Text.Json
open Yog.Model

/// Converts a graph to JSON format.
///
/// The JSON format is suitable for data interchange, web APIs, and
/// storage. Node and edge data are converted to strings using their
/// `ToString()` method.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
/// ```fsharp
/// let graph =
///     empty Directed
///     |> addNode 1 "A"
///     |> addNode 2 "B"
///     |> addEdge 1 2 10
///
/// let json = Json.render graph
/// // {
/// //   "kind": "Directed",
/// //   "nodes": [
/// //     { "id": 1, "data": "A" },
/// //     { "id": 2, "data": "B" }
/// //   ],
/// //   "edges": [
/// //     { "source": 1, "target": 2, "weight": "10" }
/// //   ]
/// // }
/// ```
///
/// ## Use Cases
///
/// - Web API responses
/// - Data persistence
/// - Interoperability with other languages/tools
/// - Graph database import/export
let render (graph: Graph<'n, 'e>) : string =
    use stream = new MemoryStream()
    use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))

    writer.WriteStartObject()
    writer.WriteString("kind", string graph.Kind)

    writer.WriteStartArray("nodes")

    for kvp in graph.Nodes do
        writer.WriteStartObject()
        writer.WriteNumber("id", kvp.Key)
        writer.WriteString("data", string kvp.Value)
        writer.WriteEndObject()

    writer.WriteEndArray()

    writer.WriteStartArray("edges")

    for KeyValue (src, targets) in graph.OutEdges do
        for KeyValue (dst, weight) in targets do
            if graph.Kind = Directed || src <= dst then
                writer.WriteStartObject()
                writer.WriteNumber("source", src)
                writer.WriteNumber("target", dst)
                writer.WriteString("weight", string weight)
                writer.WriteEndObject()

    writer.WriteEndArray()

    writer.WriteEndObject()
    writer.Flush()
    Encoding.UTF8.GetString(stream.ToArray())

/// Renders a graph to a JSON file.
///
/// ## Example
///
/// ```fsharp
/// let graph =
///     empty Directed
///     |> addNode 1 "A"
///     |> addNode 2 "B"
///     |> addEdge 1 2 5
///
/// Json.writeFile "output.json" graph
/// ```
let writeFile (path: string) (graph: Graph<'n, 'e>) : unit =
    File.WriteAllText(path, render graph)
