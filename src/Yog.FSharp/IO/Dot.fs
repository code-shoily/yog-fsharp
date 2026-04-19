/// DOT (Graphviz) graph rendering.
/// 
/// Provides functions to export graphs in DOT format for visualization
/// with Graphviz tools.
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
///     // Export to DOT for Graphviz
///     let dot = Dot.render Dot.defaultOptions graph
///     File.WriteAllText("graph.dot", dot)
/// 
module Yog.IO.Dot

open System.Text
open Yog.Model

/// Options for customizing DOT (Graphviz) diagram rendering.
type Options<'n, 'e> =
    { /// Function to convert node ID and data to a display label
      NodeLabel: NodeId -> 'n -> string
      /// Function to convert edge weight to a display label
      EdgeLabel: 'e -> string
      /// Set of node IDs to highlight
      HighlightedNodes: Set<NodeId>
      /// Set of edges to highlight as (from, to) pairs
      HighlightedEdges: Set<NodeId * NodeId>
      /// Node shape (e.g., "circle", "box", "ellipse")
      NodeShape: string
      /// Highlight color for nodes/edges
      HighlightColor: string }

/// Default configuration for DOT output.
/// 
/// Uses node ID as a label and edge weight as a string.
let defaultOptions<'n, 'e> : Options<'n, 'e> =
    { NodeLabel = fun id _ -> string id
      EdgeLabel = string
      HighlightedNodes = Set.empty
      HighlightedEdges = Set.empty
      NodeShape = "ellipse"
      HighlightColor = "red" }

/// Converts a graph to DOT (Graphviz) syntax.
/// 
/// The DOT format can be processed by Graphviz tools:
/// `dot -Tpng -o graph.png graph.dot`
/// 
/// **Time Complexity:** O(V + E)
/// 
/// ## Example
/// 
///     let graph =
///         empty Directed
///         |> addNode 1 "Start"
///         |> addNode 2 "Process"
///         |> addEdge 1 2 "5"
///     
///     let options = { Dot.defaultOptions with
///         NodeLabel = fun id data -> $"{id}:{data}"
///         HighlightedNodes = Set.ofList [1]
///     }
///     
///     let diagram = Dot.render options graph
///     // digraph G {
///     //   node [shape=ellipse];
///     //   1 [label="1:Start", fillcolor="red", style=filled];
///     //   2 [label="2:Process"];
///     //   1 -> 2 [label="5"];
///     // }
/// 
/// ## Use Cases
/// 
/// - Professional graph visualization
/// - Publication-quality diagrams
/// - Complex graph layouts (hierarchical, circular, etc.)
let render (options: Options<'n, 'e>) (graph: Graph<'n, 'e>) : string =
    let sb = StringBuilder()

    let connector =
        if graph.Kind = Directed then "->" else "--"

    let header =
        if graph.Kind = Directed then "digraph G {"
        else "graph G {"

    sb.AppendLine(header) |> ignore

    sb.AppendLine($"  node [shape={options.NodeShape}];")
    |> ignore

    sb.AppendLine("  edge [fontname=\"Helvetica\", fontsize=10];")
    |> ignore

    for kvp in graph.Nodes do
        let id = kvp.Key
        let label = options.NodeLabel id kvp.Value

        let highlight =
            if options.HighlightedNodes.Contains(id) then
                $" fillcolor=\"{options.HighlightColor}\", style=filled"
            else
                ""

        sb.AppendLine($"  {id} [label=\"{label}\"{highlight}];")
        |> ignore

    for KeyValue (src, targets) in graph.OutEdges do
        for KeyValue (dst, weight) in targets do
            if graph.Kind = Directed || src <= dst then
                let isHighlighted =
                    options.HighlightedEdges.Contains((src, dst))
                    || (graph.Kind = Undirected
                        && options.HighlightedEdges.Contains((dst, src)))

                let highlight =
                    if isHighlighted then
                        $" color=\"{options.HighlightColor}\", penwidth=2"
                    else
                        ""

                sb.AppendLine($"  {src} {connector} {dst} [label=\"{options.EdgeLabel weight}\"{highlight}];")
                |> ignore

    sb.AppendLine("}") |> ignore
    sb.ToString()

/// Renders a graph to a DOT file.
/// 
/// ## Example
/// 
///     let graph =
///         empty Directed
///         |> addNode 1 "A"
///         |> addNode 2 "B"
///         |> addEdge 1 2 5
///     
///     Dot.writeFile "output.dot" Dot.defaultOptions graph
/// 
let writeFile (path: string) (options: Options<'n, 'e>) (graph: Graph<'n, 'e>) : unit =
    System.IO.File.WriteAllText(path, render options graph)
