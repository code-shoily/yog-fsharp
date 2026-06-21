namespace Yog.Render

open System.IO
open System.Text
open Yog.Model
open Yog.Multi

module Dot =

    /// Options for customizing DOT (Graphviz) diagram rendering.
    type Options<'n, 'e> =
        {
            /// Function to convert node ID and data to a display label
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
            HighlightColor: string
        }

    /// Options for customizing MultiGraph DOT diagram rendering.
    type MultiOptions<'n, 'e> =
        {
            /// Function to convert node ID and data to a display label
            NodeLabel: NodeId -> 'n -> string
            /// Function to convert edge weight to a display label
            EdgeLabel: EdgeId -> 'e -> string
            /// Set of node IDs to highlight
            HighlightedNodes: Set<NodeId>
            /// Set of edge IDs to highlight
            HighlightedEdges: Set<EdgeId>
            /// Set of edge endpoints to highlight
            HighlightedNodePairs: Set<NodeId * NodeId>
            /// Node shape (e.g., "circle", "box", "ellipse")
            NodeShape: string
            /// Highlight color for nodes/edges
            HighlightColor: string
        }

    /// Default configuration for DOT output.
    let defaultOptions<'n, 'e> : Options<'n, 'e> =
        { NodeLabel = fun id _ -> string id
          EdgeLabel = fun weight -> string weight
          HighlightedNodes = Set.empty
          HighlightedEdges = Set.empty
          NodeShape = "ellipse"
          HighlightColor = "red" }

    /// Default configuration for MultiGraph DOT output.
    let defaultMultiOptions<'n, 'e> : MultiOptions<'n, 'e> =
        { NodeLabel = fun id _ -> string id
          EdgeLabel = fun _ weight -> string weight
          HighlightedNodes = Set.empty
          HighlightedEdges = Set.empty
          HighlightedNodePairs = Set.empty
          NodeShape = "ellipse"
          HighlightColor = "red" }

    /// Converts a graph to DOT (Graphviz) syntax.
    let render (options: Options<'n, 'e>) (graph: Graph<'n, 'e>) : string =
        let sb = StringBuilder()
        let connector = if graph.Kind = Directed then "->" else "--"
        let header = if graph.Kind = Directed then "digraph G {" else "graph G {"
        sb.AppendLine(header) |> ignore
        sb.AppendLine($"  node [shape={options.NodeShape}];") |> ignore
        sb.AppendLine("  edge [fontname=\"Helvetica\", fontsize=10];") |> ignore
        
        for kvp in graph.Nodes do
            let id = kvp.Key
            let label = options.NodeLabel id kvp.Value
            let highlight =
                if Set.contains id options.HighlightedNodes then
                    $" fillcolor=\"{options.HighlightColor}\", style=filled"
                else ""
            sb.AppendLine($"  {id} [label=\"{label}\"{highlight}];") |> ignore

        for KeyValue(src, targets) in graph.OutEdges do
            for KeyValue(dst, weight) in targets do
                if graph.Kind = Directed || src <= dst then
                    let isHighlighted =
                        Set.contains (src, dst) options.HighlightedEdges
                        || (graph.Kind = Undirected && Set.contains (dst, src) options.HighlightedEdges)
                    let highlight =
                        if isHighlighted then
                            $" color=\"{options.HighlightColor}\", penwidth=2"
                        else ""
                    sb.AppendLine($"  {src} {connector} {dst} [label=\"{options.EdgeLabel weight}\"{highlight}];") |> ignore
        sb.AppendLine("}") |> ignore
        sb.ToString()

    /// Converts a multigraph to DOT syntax.
    let renderMulti (options: MultiOptions<'n, 'e>) (graph: MultiGraph<'n, 'e>) : string =
        let sb = StringBuilder()
        let connector = if graph.Kind = Directed then "->" else "--"
        let header = if graph.Kind = Directed then "digraph G {" else "graph G {"
        sb.AppendLine(header) |> ignore
        sb.AppendLine($"  node [shape={options.NodeShape}];") |> ignore
        sb.AppendLine("  edge [fontname=\"Helvetica\", fontsize=10];") |> ignore
        
        for kvp in graph.Nodes do
            let id = kvp.Key
            let label = options.NodeLabel id kvp.Value
            let highlight =
                if Set.contains id options.HighlightedNodes then
                    $" fillcolor=\"{options.HighlightColor}\", style=filled"
                else ""
            sb.AppendLine($"  {id} [label=\"{label}\"{highlight}];") |> ignore

        for kvp in graph.Edges do
            let edgeId = kvp.Key
            let (src, dst, weight) = kvp.Value
            if graph.Kind = Directed || src <= dst then
                let isHighlighted =
                    Set.contains edgeId options.HighlightedEdges
                    || Set.contains (src, dst) options.HighlightedNodePairs
                    || (graph.Kind = Undirected && Set.contains (dst, src) options.HighlightedNodePairs)
                let highlight =
                    if isHighlighted then
                        $" color=\"{options.HighlightColor}\", penwidth=2"
                    else ""
                sb.AppendLine($"  {src} {connector} {dst} [label=\"{options.EdgeLabel edgeId weight}\"{highlight}];") |> ignore
        sb.AppendLine("}") |> ignore
        sb.ToString()

    /// Renders a graph to a DOT file.
    let writeFile (path: string) (options: Options<'n, 'e>) (graph: Graph<'n, 'e>) : unit =
        File.WriteAllText(path, render options graph)

    /// Renders a multigraph to a DOT file.
    let writeFileMulti (path: string) (options: MultiOptions<'n, 'e>) (graph: MultiGraph<'n, 'e>) : unit =
        File.WriteAllText(path, renderMulti options graph)
