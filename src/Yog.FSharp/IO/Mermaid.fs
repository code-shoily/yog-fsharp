/// Mermaid diagram rendering.
///
/// Provides functions to export graphs in Mermaid syntax for embedding
/// in markdown documents.
///
/// ## Example
///
///     open Yog.IO
///     open Yog.Model
///
///     let graph =
///         empty Directed
///         |> addNode 1 "Start"
///         |> addNode 2 "End"
///         |> addEdge 1 2 5
///
///     // Export to Mermaid for markdown
///     let mermaid = Mermaid.render Mermaid.defaultOptions graph
///
module Yog.IO.Mermaid

open System.Text
open Yog.Model

/// Options for customizing Mermaid diagram rendering.
type Options<'n, 'e> =
    {
        /// Function to convert node ID and data to a display label
        NodeLabel: NodeId -> 'n -> string
        /// Function to convert edge weight to a display label
        EdgeLabel: 'e -> string
        /// Set of node IDs to highlight (e.g., a path)
        HighlightedNodes: Set<NodeId>
        /// Set of edges to highlight as (from, to) pairs
        HighlightedEdges: Set<NodeId * NodeId>
    }

/// Default configuration for Mermaid output.
///
/// Uses node ID as a label and edge weight as a string.
let defaultOptions<'n, 'e> : Options<'n, 'e> =
    { NodeLabel = fun id _ -> string id
      EdgeLabel = fun weight -> string weight
      HighlightedNodes = Set.empty
      HighlightedEdges = Set.empty }

/// Converts a graph to Mermaid diagram syntax.
///
/// Mermaid diagrams can be embedded directly in markdown and rendered
/// by many documentation tools (GitHub, GitLab, Notion, etc.).
///
/// **Time Complexity:** O(V + E)
///
/// ## Example
///
///     let graph =
///         empty Directed
///         |> addNode 1 "Start"
///         |> addNode 2 "Process"
///         |> addNode 3 "End"
///         |> addEdge 1 2 5
///         |> addEdge 2 3 3
///
///     // Highlight a path
///     let options = { Mermaid.defaultOptions with
///         HighlightedNodes = Set.ofList [1; 2; 3]
///         HighlightedEdges = Set.ofList [(1, 2); (2, 3)]
///     }
///
///     let diagram = Mermaid.render options graph
///     // graph TD
///     //   classDef highlight fill:#ffeb3b,stroke:#f57c00,stroke-width:3px
///     //   1["Start"]:::highlight
///     //   2["Process"]:::highlight
///     //   3["End"]:::highlight
///     //   1 -->|"5"| 2:::highlightEdge
///     //   2 -->|"3"| 3:::highlightEdge
///
/// ## Use Cases
///
/// - Documentation and README files
/// - Wiki pages
/// - Presentation slides
/// - Quick prototyping and sharing
let render (options: Options<'n, 'e>) (graph: Graph<'n, 'e>) : string =
    let sb = StringBuilder()

    let arrow = if graph.Kind = Directed then "-->" else "---"

    let header = if graph.Kind = Directed then "graph TD" else "graph LR"

    sb.AppendLine(header) |> ignore

    if not (Set.isEmpty options.HighlightedNodes && Set.isEmpty options.HighlightedEdges) then
        sb.AppendLine("  classDef highlight fill:#ffeb3b,stroke:#f57c00,stroke-width:3px")
        |> ignore

        sb.AppendLine("  classDef highlightEdge stroke:#f57c00,stroke-width:3px")
        |> ignore

    for kvp in graph.Nodes do
        let id = kvp.Key
        let label = options.NodeLabel id kvp.Value

        let style =
            if options.HighlightedNodes.Contains(id) then
                ":::highlight"
            else
                ""

        sb.AppendLine($"  {id}[\"{label}\"]{style}") |> ignore

    for KeyValue(src, targets) in graph.OutEdges do
        for KeyValue(dst, weight) in targets do
            if graph.Kind = Directed || src <= dst then
                let isHighlighted =
                    options.HighlightedEdges.Contains((src, dst))
                    || (graph.Kind = Undirected && options.HighlightedEdges.Contains((dst, src)))

                let style = if isHighlighted then ":::highlightEdge" else ""

                sb.AppendLine($"  {src} {arrow}|\"{options.EdgeLabel weight}\"| {dst}{style}")
                |> ignore

    sb.ToString()

/// Renders a graph to a Mermaid file.
///
/// ## Example
///
///     let graph =
///         empty Directed
///         |> addNode 1 "A"
///         |> addNode 2 "B"
///         |> addEdge 1 2 5
///
///     Mermaid.writeFile "output.mmd" Mermaid.defaultOptions graph
///
let writeFile (path: string) (options: Options<'n, 'e>) (graph: Graph<'n, 'e>) : unit =
    System.IO.File.WriteAllText(path, render options graph)
