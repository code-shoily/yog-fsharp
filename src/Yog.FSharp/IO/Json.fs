/// JSON graph serialization and deserialization.
///
/// Provides functions to export and import graphs in various JSON formats for data interchange,
/// visualization, and web applications.
///
/// ## Supported Formats
/// - **YogGeneric**: Full metadata with type preservation
/// - **NetworkX**: NetworkX compatibility format
/// - **D3Force**: D3.js force-directed graphs
/// - **Cytoscape**: Cytoscape.js elements list
/// - **VisJs**: vis.js network format
///
module Yog.IO.Json

open System
open System.IO
open System.Text
open System.Text.Json
open Yog.Model

/// JSON graph formats supported.
type Format =
    | YogGeneric
    | NetworkX
    | D3Force
    | Cytoscape
    | VisJs

/// Options for JSON serialization.
type ExportOptions<'n, 'e> =
    { Format: Format
      IncludeMetadata: bool
      NodeSerializer: 'n -> JsonElement -> unit
      EdgeSerializer: 'e -> JsonElement -> unit }

/// Build default serialize functions using System.Text.Json
let defaultNodeSerializer (n: 'n) (writer: Utf8JsonWriter) = JsonSerializer.Serialize(writer, n)

let defaultEdgeSerializer (e: 'e) (writer: Utf8JsonWriter) = JsonSerializer.Serialize(writer, e)

/// Safely checks if a property exists on a JsonElement.
let private hasProperty (name: string) (el: JsonElement) : bool =
    match el.ValueKind with
    | JsonValueKind.Object ->
        let mutable outVal = new JsonElement()
        el.TryGetProperty(name, &outVal)
    | _ -> false

/// Detects the format of a JSON document.
let detectFormat (doc: JsonDocument) : Format =
    let root = doc.RootElement

    if hasProperty "elements" root then
        Cytoscape
    elif hasProperty "directed" root then
        NetworkX
    elif hasProperty "graph_type" root || hasProperty "kind" root then
        YogGeneric
    elif hasProperty "nodes" root && hasProperty "links" root then
        D3Force
    elif hasProperty "nodes" root && hasProperty "edges" root then
        // If it's nodes and edges, let's look at the first edge to see if it has 'from'/'to' (VisJs) or 'source'/'target' (YogGeneric/D3)
        let edgesProp = root.GetProperty("edges")

        if edgesProp.ValueKind = JsonValueKind.Array && edgesProp.GetArrayLength() > 0 then
            let firstEdge = edgesProp.[0]
            if hasProperty "from" firstEdge then VisJs else YogGeneric
        else
            YogGeneric
    else
        YogGeneric

/// Converts a graph to JSON format according to the specified format.
let renderWithFormat (format: Format) (graph: Graph<'n, 'e>) : string =
    use stream = new MemoryStream()
    use writer = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))

    match format with
    | YogGeneric ->
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

        for KeyValue(src, targets) in graph.OutEdges do
            for KeyValue(dst, weight) in targets do
                if graph.Kind = Directed || src <= dst then
                    writer.WriteStartObject()
                    writer.WriteNumber("source", src)
                    writer.WriteNumber("target", dst)
                    writer.WriteString("weight", string weight)
                    writer.WriteEndObject()

        writer.WriteEndArray()

        writer.WriteEndObject()

    | NetworkX ->
        writer.WriteStartObject()
        writer.WriteBoolean("directed", graph.Kind = Directed)
        writer.WriteBoolean("multigraph", false)
        writer.WriteStartObject("graph")
        writer.WriteEndObject()

        writer.WriteStartArray("nodes")

        for kvp in graph.Nodes do
            writer.WriteStartObject()
            writer.WriteNumber("id", kvp.Key)
            writer.WritePropertyName("data")
            JsonSerializer.Serialize<'n>(writer, kvp.Value)
            writer.WriteEndObject()

        writer.WriteEndArray()

        writer.WriteStartArray("links")

        for KeyValue(src, targets) in graph.OutEdges do
            for KeyValue(dst, weight) in targets do
                if graph.Kind = Directed || src <= dst then
                    writer.WriteStartObject()
                    writer.WriteNumber("source", src)
                    writer.WriteNumber("target", dst)
                    writer.WritePropertyName("weight")
                    JsonSerializer.Serialize<'e>(writer, weight)
                    writer.WriteEndObject()

        writer.WriteEndArray()
        writer.WriteEndObject()

    | D3Force ->
        writer.WriteStartObject()
        writer.WriteStartArray("nodes")

        for kvp in graph.Nodes do
            writer.WriteStartObject()
            writer.WriteNumber("id", kvp.Key)
            writer.WritePropertyName("data")
            JsonSerializer.Serialize<'n>(writer, kvp.Value)
            writer.WriteEndObject()

        writer.WriteEndArray()

        writer.WriteStartArray("links")

        for KeyValue(src, targets) in graph.OutEdges do
            for KeyValue(dst, weight) in targets do
                if graph.Kind = Directed || src <= dst then
                    writer.WriteStartObject()
                    writer.WriteNumber("source", src)
                    writer.WriteNumber("target", dst)
                    writer.WritePropertyName("weight")
                    JsonSerializer.Serialize<'e>(writer, weight)
                    writer.WriteEndObject()

        writer.WriteEndArray()
        writer.WriteEndObject()

    | Cytoscape ->
        writer.WriteStartObject()
        writer.WriteStartArray("elements")

        // Nodes
        for kvp in graph.Nodes do
            writer.WriteStartObject()
            writer.WriteStartObject("data")
            writer.WriteString("id", string kvp.Key)
            writer.WritePropertyName("label")
            JsonSerializer.Serialize<'n>(writer, kvp.Value)
            writer.WriteEndObject()
            writer.WriteEndObject()

        // Edges
        for KeyValue(src, targets) in graph.OutEdges do
            for KeyValue(dst, weight) in targets do
                if graph.Kind = Directed || src <= dst then
                    writer.WriteStartObject()
                    writer.WriteStartObject("data")
                    writer.WriteString("source", string src)
                    writer.WriteString("target", string dst)
                    writer.WritePropertyName("weight")
                    JsonSerializer.Serialize<'e>(writer, weight)
                    writer.WriteEndObject()
                    writer.WriteEndObject()

        writer.WriteEndArray()
        writer.WriteEndObject()

    | VisJs ->
        writer.WriteStartObject()
        writer.WriteStartArray("nodes")

        for kvp in graph.Nodes do
            writer.WriteStartObject()
            writer.WriteNumber("id", kvp.Key)
            writer.WritePropertyName("label")
            JsonSerializer.Serialize<'n>(writer, kvp.Value)
            writer.WriteEndObject()

        writer.WriteEndArray()

        writer.WriteStartArray("edges")

        for KeyValue(src, targets) in graph.OutEdges do
            for KeyValue(dst, weight) in targets do
                if graph.Kind = Directed || src <= dst then
                    writer.WriteStartObject()
                    writer.WriteNumber("from", src)
                    writer.WriteNumber("to", dst)
                    writer.WritePropertyName("label")
                    JsonSerializer.Serialize<'e>(writer, weight)
                    writer.WriteEndObject()

        writer.WriteEndArray()
        writer.WriteEndObject()

    writer.Flush()
    Encoding.UTF8.GetString(stream.ToArray())

/// Helper to parse ID as integer
let private parseId (element: JsonElement) : int =
    match element.ValueKind with
    | JsonValueKind.Number -> element.GetInt32()
    | JsonValueKind.String ->
        match Int32.TryParse(element.GetString()) with
        | true, v -> v
        | false, _ -> element.GetString().GetHashCode()
    | _ -> failwith "Unsupported Node ID JSON type"

/// Parse a graph from a JSON string using custom deserializers.
let deserializeWith
    (nodeDeserializer: JsonElement -> 'n)
    (edgeDeserializer: JsonElement -> 'e)
    (json: string)
    : Result<Graph<'n, 'e>, string> =
    try
        use doc = JsonDocument.Parse(json)
        let root = doc.RootElement
        let format = detectFormat doc

        match format with
        | YogGeneric ->
            let graphType =
                if hasProperty "graph_type" root then
                    let gt = root.GetProperty("graph_type").GetString()
                    if gt = "directed" then Directed else Undirected
                elif hasProperty "kind" root then
                    let gt = root.GetProperty("kind").GetString()
                    if gt = "Directed" then Directed else Undirected
                else
                    Directed

            let mutable g = empty graphType

            if hasProperty "nodes" root then
                let nodesProp = root.GetProperty("nodes")

                for nodeEl in nodesProp.EnumerateArray() do
                    let id = parseId (nodeEl.GetProperty("id"))

                    let data =
                        if hasProperty "data" nodeEl then
                            nodeDeserializer (nodeEl.GetProperty("data"))
                        elif hasProperty "label" nodeEl then
                            nodeDeserializer (nodeEl.GetProperty("label"))
                        else
                            nodeDeserializer nodeEl

                    g <- addNode id data g

            if hasProperty "edges" root then
                let edgesProp = root.GetProperty("edges")

                for edgeEl in edgesProp.EnumerateArray() do
                    let src = parseId (edgeEl.GetProperty("source"))
                    let dst = parseId (edgeEl.GetProperty("target"))

                    let weight =
                        if hasProperty "weight" edgeEl then
                            edgeDeserializer (edgeEl.GetProperty("weight"))
                        elif hasProperty "label" edgeEl then
                            edgeDeserializer (edgeEl.GetProperty("label"))
                        else
                            edgeDeserializer edgeEl

                    g <- addEdgeEnsured src dst weight (nodeDeserializer edgeEl) (nodeDeserializer edgeEl) g

            Ok g

        | NetworkX ->
            let directed =
                if hasProperty "directed" root then
                    root.GetProperty("directed").GetBoolean()
                else
                    true

            let graphType = if directed then Directed else Undirected
            let mutable g = empty graphType

            if hasProperty "nodes" root then
                let nodesProp = root.GetProperty("nodes")

                for nodeEl in nodesProp.EnumerateArray() do
                    let id = parseId (nodeEl.GetProperty("id"))

                    let data =
                        if hasProperty "data" nodeEl then
                            nodeDeserializer (nodeEl.GetProperty("data"))
                        else
                            nodeDeserializer nodeEl

                    g <- addNode id data g

            let linksKey = if hasProperty "links" root then "links" else "edges"

            if hasProperty linksKey root then
                let linksProp = root.GetProperty(linksKey)

                for linkEl in linksProp.EnumerateArray() do
                    let src = parseId (linkEl.GetProperty("source"))
                    let dst = parseId (linkEl.GetProperty("target"))

                    let weight =
                        if hasProperty "weight" linkEl then
                            edgeDeserializer (linkEl.GetProperty("weight"))
                        else
                            edgeDeserializer linkEl

                    g <- addEdgeEnsured src dst weight (nodeDeserializer linkEl) (nodeDeserializer linkEl) g

            Ok g

        | D3Force ->
            let mutable g = empty Undirected // default assumption for D3

            if hasProperty "nodes" root then
                let nodesProp = root.GetProperty("nodes")

                for nodeEl in nodesProp.EnumerateArray() do
                    let id = parseId (nodeEl.GetProperty("id"))

                    let data =
                        if hasProperty "data" nodeEl then
                            nodeDeserializer (nodeEl.GetProperty("data"))
                        else
                            nodeDeserializer nodeEl

                    g <- addNode id data g

            if hasProperty "links" root then
                let linksProp = root.GetProperty("links")

                for linkEl in linksProp.EnumerateArray() do
                    let src = parseId (linkEl.GetProperty("source"))
                    let dst = parseId (linkEl.GetProperty("target"))

                    let weight =
                        if hasProperty "weight" linkEl then
                            edgeDeserializer (linkEl.GetProperty("weight"))
                        elif hasProperty "value" linkEl then
                            edgeDeserializer (linkEl.GetProperty("value"))
                        else
                            edgeDeserializer linkEl

                    g <- addEdgeEnsured src dst weight (nodeDeserializer linkEl) (nodeDeserializer linkEl) g

            Ok g

        | Cytoscape ->
            let mutable g = empty Undirected

            if hasProperty "elements" root then
                let elementsProp = root.GetProperty("elements")

                for el in elementsProp.EnumerateArray() do
                    let data = el.GetProperty("data")

                    if hasProperty "id" data && not (hasProperty "source" data) then
                        let id = parseId (data.GetProperty("id"))

                        let nodeData =
                            if hasProperty "label" data then
                                nodeDeserializer (data.GetProperty("label"))
                            elif hasProperty "name" data then
                                nodeDeserializer (data.GetProperty("name"))
                            else
                                nodeDeserializer data

                        g <- addNode id nodeData g

                for el in elementsProp.EnumerateArray() do
                    let data = el.GetProperty("data")

                    if hasProperty "source" data && hasProperty "target" data then
                        let src = parseId (data.GetProperty("source"))
                        let dst = parseId (data.GetProperty("target"))

                        let weight =
                            if hasProperty "weight" data then
                                edgeDeserializer (data.GetProperty("weight"))
                            elif hasProperty "label" data then
                                edgeDeserializer (data.GetProperty("label"))
                            else
                                edgeDeserializer data

                        g <- addEdgeEnsured src dst weight (nodeDeserializer data) (nodeDeserializer data) g

            Ok g

        | VisJs ->
            let mutable g = empty Undirected

            if hasProperty "nodes" root then
                let nodesProp = root.GetProperty("nodes")

                for nodeEl in nodesProp.EnumerateArray() do
                    let id = parseId (nodeEl.GetProperty("id"))

                    let data =
                        if hasProperty "label" nodeEl then
                            nodeDeserializer (nodeEl.GetProperty("label"))
                        elif hasProperty "name" nodeEl then
                            nodeDeserializer (nodeEl.GetProperty("name"))
                        else
                            nodeDeserializer nodeEl

                    g <- addNode id data g

            if hasProperty "edges" root then
                let edgesProp = root.GetProperty("edges")

                for edgeEl in edgesProp.EnumerateArray() do
                    let src = parseId (edgeEl.GetProperty("from"))
                    let dst = parseId (edgeEl.GetProperty("to"))

                    let weight =
                        if hasProperty "label" edgeEl then
                            edgeDeserializer (edgeEl.GetProperty("label"))
                        elif hasProperty "weight" edgeEl then
                            edgeDeserializer (edgeEl.GetProperty("weight"))
                        else
                            edgeDeserializer edgeEl

                    g <- addEdgeEnsured src dst weight (nodeDeserializer edgeEl) (nodeDeserializer edgeEl) g

            Ok g

    with ex ->
        Error ex.Message

/// Converts a graph to JSON format (backward compatibility with original `render`).
let render (graph: Graph<'n, 'e>) : string = renderWithFormat YogGeneric graph

/// Renders a graph to a JSON file.
let writeFile (path: string) (graph: Graph<'n, 'e>) : unit = File.WriteAllText(path, render graph)

/// Deserializes a JSON string into a graph where node and edge data are simply strings.
let deserialize (json: string) : Result<Graph<string, string>, string> =
    let deserializeStr (el: JsonElement) =
        match el.ValueKind with
        | JsonValueKind.String -> el.GetString()
        | JsonValueKind.Number -> string (el.GetRawText())
        | JsonValueKind.True -> "true"
        | JsonValueKind.False -> "false"
        | JsonValueKind.Null -> ""
        | _ -> el.GetRawText()

    deserializeWith deserializeStr deserializeStr json

/// Reads a graph from a JSON file.
let readFile (path: string) : Result<Graph<string, string>, string> =
    try
        File.ReadAllText(path) |> deserialize
    with ex ->
        Error ex.Message
