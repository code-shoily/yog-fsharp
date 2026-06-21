/// GDF (GUESS Graph Format) serialization support.
///
/// Provides functions to serialize and deserialize graphs in GDF format, a simple text-based format
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
///     // Deserialize from GDF
///     let graph = Gdf.deserialize gdfString
///
/// ## Output Format
///
/// nodedef>name VARCHAR,label VARCHAR
/// 1,Alice
/// 2,Bob
/// edgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN,weight VARCHAR
/// 1,2,true,5
///
module Yog.IO.Gdf

open System.IO
open System.Text
open Yog.Model

/// Options for GDF serialization.
type Options =
    {
        /// Column separator (default: comma)
        Separator: string
        /// Include type annotations (default: true)
        IncludeTypes: bool
    }

/// Default GDF serialization options.
let defaultOptions = { Separator = ","; IncludeTypes = true }

// =============================================================================
// Private Helpers
// =============================================================================

/// Escape a value for GDF format (wrap in quotes if contains separator or newline)
let private escapeValue (separator: string) (value: string) =
    if
        value.Contains(separator)
        || value.Contains("\n")
        || value.Contains("\r")
        || value.Contains("\"")
    then
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

/// Build the edge definition header (always includes directed column)
let private buildEdgeHeader (options: Options) (attributes: string list) =
    let sb = StringBuilder("edgedef>node1")

    if options.IncludeTypes then
        sb.Append(" VARCHAR") |> ignore

    sb.Append(options.Separator).Append("node2") |> ignore

    if options.IncludeTypes then
        sb.Append(" VARCHAR") |> ignore

    sb.Append(options.Separator).Append("directed") |> ignore

    if options.IncludeTypes then
        sb.Append(" BOOLEAN") |> ignore

    for attr in attributes do
        sb.Append(options.Separator).Append(attr) |> ignore

        if options.IncludeTypes then
            sb.Append(" VARCHAR") |> ignore

    sb.ToString()

// =============================================================================
// CSV Parsing
// =============================================================================

/// Parse a single CSV line respecting quoted values.
let private parseCsvValues (separator: string) (line: string) =
    let result = ResizeArray<string>()
    let mutable i = 0
    let sb = StringBuilder()

    while i < line.Length do
        if line.[i] = '"' then
            // Quoted field
            i <- i + 1
            sb.Clear() |> ignore

            let mutable inQuote = true

            while inQuote && i < line.Length do
                if line.[i] = '"' then
                    if i + 1 < line.Length && line.[i + 1] = '"' then
                        sb.Append('"') |> ignore
                        i <- i + 2
                    else
                        inQuote <- false
                        i <- i + 1
                else
                    sb.Append(line.[i]) |> ignore
                    i <- i + 1

            result.Add(sb.ToString())

            // Skip separator after quoted field
            if i < line.Length && line.Substring(i).StartsWith(separator) then
                i <- i + separator.Length
        else
            // Unquoted field
            let sepIdx = line.IndexOf(separator, i)

            if sepIdx < 0 then
                result.Add(line.Substring(i).Trim())
                i <- line.Length
            else
                result.Add(line.Substring(i, sepIdx - i).Trim())
                i <- sepIdx + separator.Length

    result |> Seq.toList

/// Parse column names from a header string, stripping type annotations.
let private parseColumnNames (separator: string) (headerPart: string) =
    headerPart.Split(separator)
    |> Array.map (fun col ->
        let trimmed = col.Trim()
        let parts = trimmed.Split(' ')
        parts.[0])
    |> Array.toList

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
    (graph: Graph<'n, 'e>)
    : string =

    let sb = StringBuilder()

    // 1. Determine node and edge attribute columns from ALL nodes/edges
    let nodeAttrs =
        graph.Nodes
        |> Map.toSeq
        |> Seq.collect (fun (_, d) -> nodeAttr d |> List.map fst)
        |> Seq.distinct
        |> Seq.toList

    let edgeAttrs =
        graph.OutEdges
        |> Map.toSeq
        |> Seq.collect (fun (_, targets) ->
            targets |> Map.toSeq |> Seq.collect (fun (_, d) -> edgeAttr d |> List.map fst))
        |> Seq.distinct
        |> Seq.toList

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

    // 3. Write edge section (always includes directed column)
    let directedValue = if graph.Kind = Directed then "true" else "false"
    sb.AppendLine(buildEdgeHeader options edgeAttrs) |> ignore

    for (src, targets) in graph.OutEdges |> Map.toSeq do
        for (dst, data) in targets |> Map.toSeq do
            let attrs = edgeAttr data |> Map.ofList
            sb.Append(escapeValue options.Separator (string src)) |> ignore

            sb.Append(options.Separator).Append(escapeValue options.Separator (string dst))
            |> ignore

            sb.Append(options.Separator).Append(directedValue) |> ignore

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
    serializeWith (fun label -> [ "label", label ]) (fun label -> [ "label", label ]) defaultOptions graph

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
    serializeWith (fun label -> [ "label", label ]) (fun label -> [ "label", label ]) options graph

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
    serializeWith (fun label -> [ "label", label ]) (fun weight -> [ "weight", string weight ]) defaultOptions graph

// =============================================================================
// File I/O (Serialization)
// =============================================================================

/// Writes a graph to a GDF file.
///
/// ## Example
///
///     let graph =
///         empty Directed
///         |> addNode 1 "Alice"
///         |> addNode 2 "Bob"
///         |> addEdge 1 2 "friend"
///
///     Gdf.writeFile "graph.gdf" graph
///
let writeFile (path: string) (graph: Graph<string, string>) : unit =
    File.WriteAllText(path, serialize graph)

/// Writes a graph to a GDF file with custom attribute mappers and options.
///
/// ## Example
///
///     type Person = { Name: string; Age: int }
///
///     let nodeAttrs p = ["name", p.Name; "age", string p.Age]
///     let edgeAttrs e = ["type", e]
///
///     Gdf.writeFileWith nodeAttrs edgeAttrs defaultOptions "people.gdf" graph
///
let writeFileWith
    (nodeAttr: 'n -> (string * string) list)
    (edgeAttr: 'e -> (string * string) list)
    (options: Options)
    (path: string)
    (graph: Graph<'n, 'e>)
    : unit =
    File.WriteAllText(path, serializeWith nodeAttr edgeAttr options graph)

// =============================================================================
// Deserialization
// =============================================================================

/// Deserializes a GDF string into a graph with custom data mappers.
///
/// Parses the nodedef and edgedef sections, handles quoted CSV values,
/// auto-detects directed/undirected from the `directed` column, and
/// auto-creates nodes referenced by edges that weren't in the node section.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
///     let gdf = """
///     nodedef>name VARCHAR,label VARCHAR,age VARCHAR
///     1,Alice,30
///     2,Bob,25
///     edgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN,weight VARCHAR
///     1,2,true,5
///     """
///
///     let nodeFolder (attrs: Map<string, string>) =
///         Map.tryFind "label" attrs |> Option.defaultValue ""
///
///     let edgeFolder (attrs: Map<string, string>) =
///         Map.tryFind "weight" attrs |> Option.defaultValue ""
///
///     match Gdf.deserializeWith nodeFolder edgeFolder gdf with
///     | Ok graph -> printfn "Loaded %d nodes" (order graph)
///     | Error msg -> printfn "Error: %s" msg
///
let deserializeWith
    (nodeFolder: Map<string, string> -> 'n)
    (edgeFolder: Map<string, string> -> 'e)
    (gdf: string)
    : Result<Graph<'n, 'e>, string> =

    let separator = ","
    let lines =
        gdf.Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun l -> l.Trim())
        |> Array.filter (fun l -> l.Length > 0)
        |> Array.toList

    // Find nodedef> and edgedef> section boundaries
    let nodeDefIdx = lines |> List.tryFindIndex (fun l -> l.StartsWith("nodedef>"))
    let edgeDefIdx = lines |> List.tryFindIndex (fun l -> l.StartsWith("edgedef>"))

    match nodeDefIdx with
    | None -> Error "Missing nodedef> section"
    | Some ndi ->
        // Parse node columns
        let nodeHeaderPart = lines.[ndi].Substring("nodedef>".Length)
        let nodeColumns = parseColumnNames separator nodeHeaderPart

        // Node data lines: everything between nodedef header and edgedef (or end)
        let nodeEndIdx = edgeDefIdx |> Option.defaultValue lines.Length
        let nodeDataLines = lines |> List.skip (ndi + 1) |> List.take (nodeEndIdx - ndi - 1)

        // Parse edge columns and data
        let edgeColumns, edgeDataLines =
            match edgeDefIdx with
            | None -> [], []
            | Some edi ->
                let edgeHeaderPart = lines.[edi].Substring("edgedef>".Length)
                let cols = parseColumnNames separator edgeHeaderPart
                let dataLines = lines |> List.skip (edi + 1)
                cols, dataLines

        // Build nodes
        let mutable g = empty Directed // Will adjust based on directed column

        let mutable isDirectedKnown = false

        for line in nodeDataLines do
            let values = parseCsvValues separator line

            if values.Length = nodeColumns.Length then
                let attrs = List.zip nodeColumns values |> Map.ofList
                let idStr = values.[0]

                let id =
                    match System.Int32.TryParse(idStr) with
                    | true, v -> v
                    | false, _ -> idStr.GetHashCode()

                g <- addNode id (nodeFolder attrs) g

        // Detect directed/undirected from first edge line
        let directedColIdx =
            edgeColumns |> List.tryFindIndex (fun c -> c = "directed")

        for line in edgeDataLines do
            let values = parseCsvValues separator line

            if values.Length = edgeColumns.Length then
                // Detect graph type from first edge
                if not isDirectedKnown then
                    match directedColIdx with
                    | Some idx ->
                        let dirVal = values.[idx].Trim().ToLowerInvariant()

                        if dirVal <> "true" then
                            // Convert to undirected
                            g <- { g with Kind = Undirected }

                        isDirectedKnown <- true
                    | None ->
                        isDirectedKnown <- true

                let attrs = List.zip edgeColumns values |> Map.ofList
                let node1Str = Map.find "node1" attrs
                let node2Str = Map.find "node2" attrs

                let node1 =
                    match System.Int32.TryParse(node1Str) with
                    | true, v -> v
                    | false, _ -> node1Str.GetHashCode()

                let node2 =
                    match System.Int32.TryParse(node2Str) with
                    | true, v -> v
                    | false, _ -> node2Str.GetHashCode()

                // Auto-create nodes if they don't exist
                if not (Map.containsKey node1 g.Nodes) then
                    g <- addNode node1 (nodeFolder Map.empty) g

                if not (Map.containsKey node2 g.Nodes) then
                    g <- addNode node2 (nodeFolder Map.empty) g

                g <- addEdge node1 node2 (edgeFolder attrs) g

        Ok g

/// Deserializes a GDF string to a graph.
///
/// Node and edge attributes are stored as-is in string maps.
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
///     let gdf = """
///     nodedef>name VARCHAR,label VARCHAR
///     1,Alice
///     edgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN
///     1,2,true
///     """
///
///     match Gdf.deserialize gdf with
///     | Ok graph -> printfn "Loaded %d nodes" (order graph)
///     | Error msg -> printfn "Error: %s" msg
///
let deserialize (gdf: string) : Result<Graph<Map<string, string>, Map<string, string>>, string> =
    deserializeWith id id gdf

// =============================================================================
// File I/O (Deserialization)
// =============================================================================

/// Reads a graph from a GDF file.
///
/// ## Example
///
///     match Gdf.readFile "graph.gdf" with
///     | Ok graph -> printfn "Loaded %d nodes" (order graph)
///     | Error msg -> printfn "Error: %s" msg
///
let readFile (path: string) : Result<Graph<Map<string, string>, Map<string, string>>, string> =
    try
        File.ReadAllText(path) |> deserialize
    with ex ->
        Error ex.Message

/// Reads a graph from a GDF file with custom data mappers.
///
/// ## Example
///
///     let nodeFolder (attrs: Map<string, string>) =
///         Map.tryFind "label" attrs |> Option.defaultValue ""
///
///     let edgeFolder (attrs: Map<string, string>) =
///         Map.tryFind "weight" attrs |> Option.defaultValue ""
///
///     match Gdf.readFileWith nodeFolder edgeFolder "graph.gdf" with
///     | Ok graph -> printfn "Loaded %d nodes" (order graph)
///     | Error msg -> printfn "Error: %s" msg
///
let readFileWith
    (nodeFolder: Map<string, string> -> 'n)
    (edgeFolder: Map<string, string> -> 'e)
    (path: string)
    : Result<Graph<'n, 'e>, string> =
    try
        File.ReadAllText(path) |> deserializeWith nodeFolder edgeFolder
    with ex ->
        Error ex.Message
