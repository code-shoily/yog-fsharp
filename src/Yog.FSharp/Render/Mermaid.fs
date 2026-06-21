namespace Yog.Render

open System.IO
open System.Text
open Yog
open Yog.Model
open Yog.Multi
open Yog.Pathfinding.Utils
open Yog.Flow.MaxFlow

module Mermaid =

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
            /// Set of source side nodes to highlight (e.g. min-cut)
            HighlightedSourceNodes: Set<NodeId>
            /// Set of sink side nodes to highlight (e.g. min-cut)
            HighlightedSinkNodes: Set<NodeId>
            /// Direction of layout (e.g., "TD", "LR")
            Direction: string
        }

    /// Options for customizing MultiGraph Mermaid rendering.
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
            /// Set of source side nodes to highlight (e.g. min-cut)
            HighlightedSourceNodes: Set<NodeId>
            /// Set of sink side nodes to highlight (e.g. min-cut)
            HighlightedSinkNodes: Set<NodeId>
            /// Direction of layout (e.g., "TD", "LR")
            Direction: string
        }

    /// Default configuration for Mermaid output.
    let defaultOptions<'n, 'e> : Options<'n, 'e> =
        { NodeLabel = fun id _ -> string id
          EdgeLabel = fun weight -> string weight
          HighlightedNodes = Set.empty
          HighlightedEdges = Set.empty
          HighlightedSourceNodes = Set.empty
          HighlightedSinkNodes = Set.empty
          Direction = "TD" }

    /// Default configuration for MultiGraph Mermaid output.
    let defaultMultiOptions<'n, 'e> : MultiOptions<'n, 'e> =
        { NodeLabel = fun id _ -> string id
          EdgeLabel = fun _ weight -> string weight
          HighlightedNodes = Set.empty
          HighlightedEdges = Set.empty
          HighlightedNodePairs = Set.empty
          HighlightedSourceNodes = Set.empty
          HighlightedSinkNodes = Set.empty
          Direction = "TD" }

    /// Converts a graph to Mermaid diagram syntax.
    let render (options: Options<'n, 'e>) (graph: Graph<'n, 'e>) : string =
        let sb = StringBuilder()
        let arrow = if graph.Kind = Directed then "-->" else "---"
        let header = $"graph {options.Direction}"
        sb.AppendLine(header) |> ignore
        
        let hasHighlights =
            not (Set.isEmpty options.HighlightedNodes && 
                 Set.isEmpty options.HighlightedEdges &&
                 Set.isEmpty options.HighlightedSourceNodes &&
                 Set.isEmpty options.HighlightedSinkNodes)
        if hasHighlights then
            sb.AppendLine("  classDef highlight fill:#ffeb3b,stroke:#f57c00,stroke-width:3px") |> ignore
            sb.AppendLine("  classDef highlightEdge stroke:#f57c00,stroke-width:3px") |> ignore
            sb.AppendLine("  classDef highlightSource fill:#a8d8ea,stroke:#0288d1,stroke-width:3px") |> ignore
            sb.AppendLine("  classDef highlightSink fill:#f08080,stroke:#c62828,stroke-width:3px") |> ignore

        for kvp in graph.Nodes do
            let id = kvp.Key
            let label = options.NodeLabel id kvp.Value
            let style = 
                if Set.contains id options.HighlightedNodes then ":::highlight" 
                elif Set.contains id options.HighlightedSourceNodes then ":::highlightSource"
                elif Set.contains id options.HighlightedSinkNodes then ":::highlightSink"
                else ""
            sb.AppendLine($"  {id}[\"{label}\"]{style}") |> ignore

        for KeyValue(src, targets) in graph.OutEdges do
            for KeyValue(dst, weight) in targets do
                if graph.Kind = Directed || src <= dst then
                    let isHighlighted =
                        Set.contains (src, dst) options.HighlightedEdges
                        || (graph.Kind = Undirected && Set.contains (dst, src) options.HighlightedEdges)
                    let style = if isHighlighted then ":::highlightEdge" else ""
                    sb.AppendLine($"  {src} {arrow}|\"{options.EdgeLabel weight}\"| {dst}{style}") |> ignore
        sb.ToString()

    /// Converts a multigraph to Mermaid diagram syntax.
    let renderMulti (options: MultiOptions<'n, 'e>) (graph: MultiGraph<'n, 'e>) : string =
        let sb = StringBuilder()
        let arrow = if graph.Kind = Directed then "-->" else "---"
        let header = $"graph {options.Direction}"
        sb.AppendLine(header) |> ignore
        
        let hasHighlights =
            not (Set.isEmpty options.HighlightedNodes && 
                 (Set.isEmpty options.HighlightedEdges && Set.isEmpty options.HighlightedNodePairs) &&
                 Set.isEmpty options.HighlightedSourceNodes &&
                 Set.isEmpty options.HighlightedSinkNodes)
        if hasHighlights then
            sb.AppendLine("  classDef highlight fill:#ffeb3b,stroke:#f57c00,stroke-width:3px") |> ignore
            sb.AppendLine("  classDef highlightEdge stroke:#f57c00,stroke-width:3px") |> ignore
            sb.AppendLine("  classDef highlightSource fill:#a8d8ea,stroke:#0288d1,stroke-width:3px") |> ignore
            sb.AppendLine("  classDef highlightSink fill:#f08080,stroke:#c62828,stroke-width:3px") |> ignore

        for kvp in graph.Nodes do
            let id = kvp.Key
            let label = options.NodeLabel id kvp.Value
            let style = 
                if Set.contains id options.HighlightedNodes then ":::highlight" 
                elif Set.contains id options.HighlightedSourceNodes then ":::highlightSource"
                elif Set.contains id options.HighlightedSinkNodes then ":::highlightSink"
                else ""
            sb.AppendLine($"  {id}[\"{label}\"]{style}") |> ignore

        for kvp in graph.Edges do
            let edgeId = kvp.Key
            let (src, dst, weight) = kvp.Value
            if graph.Kind = Directed || src <= dst then
                let isHighlighted =
                    Set.contains edgeId options.HighlightedEdges
                    || Set.contains (src, dst) options.HighlightedNodePairs
                    || (graph.Kind = Undirected && Set.contains (dst, src) options.HighlightedNodePairs)
                let style = if isHighlighted then ":::highlightEdge" else ""
                sb.AppendLine($"  {src} {arrow}|\"{options.EdgeLabel edgeId weight}\"| {dst}{style}") |> ignore
        sb.ToString()

    /// Renders a graph to a Mermaid file.
    let writeFile (path: string) (options: Options<'n, 'e>) (graph: Graph<'n, 'e>) : unit =
        File.WriteAllText(path, render options graph)

    /// Renders a multigraph to a Mermaid file.
    let writeFileMulti (path: string) (options: MultiOptions<'n, 'e>) (graph: MultiGraph<'n, 'e>) : unit =
        File.WriteAllText(path, renderMulti options graph)

    /// Creates Mermaid options that highlight a path.
    let pathToOptions (path: Path<'e>) (baseOptions: Options<'n, 'e>) : Options<'n, 'e> =
        let nodes = Set.ofList path.Nodes
        let edges = List.pairwise path.Nodes |> Set.ofList
        { baseOptions with
            HighlightedNodes = nodes
            HighlightedEdges = edges }

    /// Creates Mermaid options that highlight a path in a multigraph.
    let pathToMultiOptions (path: Path<'e>) (baseOptions: MultiOptions<'n, 'e>) : MultiOptions<'n, 'e> =
        let nodes = Set.ofList path.Nodes
        let edges = List.pairwise path.Nodes |> Set.ofList
        { baseOptions with
            HighlightedNodes = nodes
            HighlightedNodePairs = edges }

    /// Creates Mermaid options that highlight an MST result.
    let mstToOptions (result: MstResult<'e>) (baseOptions: Options<'n, 'e>) : Options<'n, 'e> =
        let edges = result.Edges |> List.map (fun e -> (e.From, e.To)) |> Set.ofList
        let nodes = result.Edges |> List.collect (fun e -> [e.From; e.To]) |> Set.ofList
        { baseOptions with
            HighlightedNodes = nodes
            HighlightedEdges = edges }

    /// Creates Mermaid options that highlight an MST result in a multigraph.
    let mstToMultiOptions (result: MstResult<'e>) (baseOptions: MultiOptions<'n, 'e>) : MultiOptions<'n, 'e> =
        let edges = result.Edges |> List.map (fun e -> (e.From, e.To)) |> Set.ofList
        let nodes = result.Edges |> List.collect (fun e -> [e.From; e.To]) |> Set.ofList
        { baseOptions with
            HighlightedNodes = nodes
            HighlightedNodePairs = edges }

    /// Creates Mermaid options that color the source and sink sides of a min-cut.
    let cutToOptions (result: MinCut) (baseOptions: Options<'n, 'e>) : Options<'n, 'e> =
        { baseOptions with
            HighlightedSourceNodes = result.SourceSide
            HighlightedSinkNodes = result.SinkSide }

    /// Creates Mermaid options that color the source and sink sides of a min-cut in a multigraph.
    let cutToMultiOptions (result: MinCut) (baseOptions: MultiOptions<'n, 'e>) : MultiOptions<'n, 'e> =
        { baseOptions with
            HighlightedSourceNodes = result.SourceSide
            HighlightedSinkNodes = result.SinkSide }

    /// Creates Mermaid options that highlight matched edges from a matching result.
    let matchingToOptions (matching: Map<NodeId, NodeId>) (baseOptions: Options<'n, 'e>) : Options<'n, 'e> =
        let edges = 
            matching 
            |> Map.toList 
            |> List.map (fun (u, v) -> if u <= v then (u, v) else (v, u)) 
            |> Set.ofList
        let nodes = matching |> Map.keys |> Set.ofSeq
        { baseOptions with
            HighlightedNodes = nodes
            HighlightedEdges = edges }

    /// Creates Mermaid options that highlight matched edges in a multigraph.
    let matchingToMultiOptions (matching: Map<NodeId, NodeId>) (baseOptions: MultiOptions<'n, 'e>) : MultiOptions<'n, 'e> =
        let edges = 
            matching 
            |> Map.toList 
            |> List.map (fun (u, v) -> if u <= v then (u, v) else (v, u)) 
            |> Set.ofList
        let nodes = matching |> Map.keys |> Set.ofSeq
        { baseOptions with
            HighlightedNodes = nodes
            HighlightedNodePairs = edges }
