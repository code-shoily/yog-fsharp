/// GDF (GUESS Graph Format) serialization support.
/// 
/// Provides functions to serialize graphs in GDF format, a simple text-based format
/// used by Gephi and other graph visualization tools. GDF uses a column-based format
/// similar to CSV with separate sections for nodes and edges.
/// 
/// ## Format Overview
/// 
/// GDF files consist of two sections:
/// - **nodedef>** - Defines node columns and data
/// - **edgedef>** - Defines edge columns and data
/// 
/// ## Example
/// 
///     open Yog.IO
///     open Yog.Model
///     
///     // Create a simple graph
///     let graph =
///         empty Directed
///         |> addNode 1 "Alice"
///         |> addNode 2 "Bob"
///         |> addEdge 1 2 5
///     
///     // Serialize to GDF
///     let gdf = Gdf.serialize graph
///     File.WriteAllText("graph.gdf", gdf)
/// 
/// ## Output Format
/// 
/// nodedef>name VARCHAR,label VARCHAR
/// 1,Alice
/// 2,Bob
/// edgedef>node1 VARCHAR,node2 VARCHAR,weight VARCHAR
/// 1,2,5
/// 
module Yog.IO.Gdf

open System.Text
open Yog.Model

/// Options for GDF serialization.
type Options =
    { /// Column separator (default: comma)
      Separator: string
      /// Include type annotations (default: true)
      IncludeTypes: bool }

/// Default GDF serialization options.
let defaultOptions =
    { Separator = ","
      IncludeTypes = true }

// =============================================================================
// Private Helpers
// =============================================================================

/// Escape a value for GDF format (wrap in quotes if contains separator or newline)
let private escapeValue (separator: string) (value: string) =
    if value.Contains(separator) || value.Contains("\n") || value.Contains("\r") || value.Contains("\"") then
        "\"" + value.Replace("\"", "\"\"") + "\""
    else
        value

/// Build the node definition header
let private buildNodeHeader (options: Options) (attributes: string list) =
    let sb = StringBuilder("nodedef>name")

    if options.IncludeTypes then
        sb.Append(" VARCHAR") |> ignore

    for attr in attributes do
        sb.Append(options.Separator).Append(attr) |> ignore
        if options.IncludeTypes then
            sb.Append(" VARCHAR") |> ignore

    sb.ToString()

/// Build the edge definition header
let private buildEdgeHeader (options: Options) (attributes: string list) (directed: bool) =
    let sb = StringBuilder("edgedef>node1")

    if options.IncludeTypes then
        sb.Append(" VARCHAR") |> ignore

    sb.Append(options.Separator).Append("node2") |> ignore

    if options.IncludeTypes then
        sb.Append(" VARCHAR") |> ignore

    if directed then
        sb.Append(options.Separator).Append("directed") |> ignore
        if options.IncludeTypes then
            sb.Append(" BOOLEAN") |> ignore

    for attr in attributes do
        sb.Append(options.Separator).Append(attr) |> ignore
        if options.IncludeTypes then
            sb.Append(" VARCHAR") |> ignore

    sb.ToString()

// =============================================================================
// Serialization
// =============================================================================

/// Serializes a graph to GDF format with custom attribute mappers and options.
/// 
/// This function allows you to control how node and edge data are converted
/// to GDF attributes, and customize the output format.
/// 
/// **Time Complexity:** O(V + E)
/// 
/// ## Example
/// 
///     type Person = { Name: string; Age: int }
///     type Connection = { Weight: int; Type: string }
///     
///     let graph =
///         empty Directed
///         |> addNode 1 { Name = "Alice"; Age = 30 }
///         |> addNode 2 { Name = "Bob"; Age = 25 }
///         |> addEdge 1 2 { Weight = 5; Type = "friend" }
///     
///     let nodeAttrs p = ["label", p.Name; "age", string p.Age]
///     let edgeAttrs c = ["weight", string c.Weight; "type", c.Type]
///     
///     let gdf = Gdf.serializeWith nodeAttrs edgeAttrs defaultOptions graph
/// 
/// ## Use Cases
/// 
/// - Exporting graphs for Gephi visualization
/// - Simple text-based graph interchange format
/// - Easy to parse and generate programmatically
let serializeWith
    (nodeAttr: 'n -> (string * string) list)
    (edgeAttr: 'e -> (string * string) list)
    (options: Options)
    (graph: Graph<'n, 'e>) : string =

    let sb = StringBuilder()

    // 1. Determine node and edge attribute columns
    let nodeAttrs =
        if Map.isEmpty graph.Nodes then []
        else
            graph.Nodes
            |> Map.toSeq
            |> Seq.head
            |> snd
            |> nodeAttr
            |> List.map fst

    let edgeAttrs =
        graph.OutEdges
        |> Map.toSeq
        |> Seq.tryPick (fun (_, targets) ->
            targets |> Map.toSeq |> Seq.tryHead)
        |> Option.map (snd >> edgeAttr >> List.map fst)
        |> Option.defaultValue []

    // 2. Write node section
    sb.AppendLine(buildNodeHeader options nodeAttrs) |> ignore

    for id, data in graph.Nodes |> Map.toSeq do
        let attrs = nodeAttr data |> Map.ofList
        sb.Append(escapeValue options.Separator (string id)) |> ignore

        for attrName in nodeAttrs do
            sb.Append(options.Separator) |> ignore
            match Map.tryFind attrName attrs with
            | Some value -> sb.Append(escapeValue options.Separator value) |> ignore
            | None -> () // Empty value

        sb.AppendLine() |> ignore

    // 3. Write edge section
    let directed = graph.Kind = Directed
    sb.AppendLine(buildEdgeHeader options edgeAttrs directed) |> ignore

    for (src, targets) in graph.OutEdges |> Map.toSeq do
        for (dst, data) in targets |> Map.toSeq do
            let attrs = edgeAttr data |> Map.ofList
            sb.Append(escapeValue options.Separator (string src)) |> ignore
            sb.Append(options.Separator).Append(escapeValue options.Separator (string dst)) |> ignore

            if directed then
                sb.Append(options.Separator).Append("true") |> ignore

            for attrName in edgeAttrs do
                sb.Append(options.Separator) |> ignore
                match Map.tryFind attrName attrs with
                | Some value -> sb.Append(escapeValue options.Separator value) |> ignore
                | None -> () // Empty value

            sb.AppendLine() |> ignore

    sb.ToString()

/// Serializes a graph to GDF format where node and edge data are strings.
/// 
/// This is a simplified version of `serializeWith` for graphs where
/// node data and edge data are already strings. The string data is used
/// as the "label" attribute for both nodes and edges.
/// 
/// **Time Complexity:** O(V + E)
/// 
/// ## Example
/// 
///     let graph =
///         empty Directed
///         |> addNode 1 "Alice"
///         |> addNode 2 "Bob"
///         |> addEdge 1 2 "friend"
///     
///     let gdf = Gdf.serialize graph
///     File.WriteAllText("graph.gdf", gdf)
/// 
let serialize (graph: Graph<string, string>) : string =
    serializeWith
        (fun label -> ["label", label])
        (fun label -> ["label", label])
        defaultOptions
        graph

/// Serializes a graph to GDF format with custom options.
/// 
/// Uses default attribute mapping (single "label" column) but allows
/// customization of separator and type annotations.
/// 
/// **Time Complexity:** O(V + E)
/// 
/// ## Example
/// 
///     let options = { defaultOptions with Separator = "\t"; IncludeTypes = false }
///     let gdf = Gdf.serializeWithOptions options graph
/// 
let serializeWithOptions (options: Options) (graph: Graph<string, string>) : string =
    serializeWith
        (fun label -> ["label", label])
        (fun label -> ["label", label])
        options
        graph

/// Serializes a graph to GDF format with integer edge weights.
/// 
/// This is a convenience function for the common case of graphs with
/// integer weights. Node data is used as labels, and edge weights are
/// serialized to the "weight" column.
/// 
/// **Time Complexity:** O(V + E)
/// 
/// ## Example
/// 
///     let graph =
///         empty Directed
///         |> addNode 1 "Alice"
///         |> addNode 2 "Bob"
///         |> addEdge 1 2 5
///     
///     let gdf = Gdf.serializeWeighted graph
/// 
let serializeWeighted (graph: Graph<string, int>) : string =
    serializeWith
        (fun label -> ["label", label])
        (fun weight -> ["weight", string weight])
        defaultOptions
        graph
