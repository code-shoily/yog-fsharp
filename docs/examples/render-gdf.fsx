(**
---
category: Visualization
categoryindex: 8
index: 4
---
*)

(**
# GDF Format Export

This example demonstrates how to export graphs to GDF (GUESS Graph Format),
a simple text-based format used by Gephi and other visualization tools.

## What is GDF?

GDF is a column-based format similar to CSV with two sections:
- `nodedef>` - Defines node columns and data
- `edgedef>` - Defines edge columns and data

It's simpler than GraphML and easy to parse, making it ideal for:
- Gephi visualization
- Quick data exchange
- Programmatic graph generation
- Testing and debugging
*)

#r "nuget: Yog.FSharp"

open Yog.Model
open Yog.IO.Gdf

(**
## Example 1: Simple Social Network

Let's create a basic social network and export it to GDF format.
*)

let socialGraph =
    empty Undirected
    |> addNode 1 "Alice"
    |> addNode 2 "Bob"
    |> addNode 3 "Charlie"
    |> addNode 4 "Diana"
    |> addEdge 1 2 "friend"
    |> addEdge 2 3 "colleague"
    |> addEdge 3 4 "friend"
    |> addEdge 1 4 "family"

let socialGdf = serialize socialGraph

printfn "=== Social Network GDF ==="
printfn "%s\n" socialGdf

(**
Output:
```
nodedef>name VARCHAR,label VARCHAR
1,Alice
2,Bob
3,Charlie
4,Diana
edgedef>node1 VARCHAR,node2 VARCHAR,label VARCHAR
1,2,friend
1,4,family
2,3,colleague
3,4,friend
```
*)

(**
## Example 2: Weighted Graph

Export a graph with numeric edge weights.
*)

let weightedGraph =
    empty Directed
    |> addNode 1 "New York"
    |> addNode 2 "Boston"
    |> addNode 3 "Philadelphia"
    |> addNode 4 "Washington DC"
    |> addEdge 1 2 215  // miles
    |> addEdge 1 3 95
    |> addEdge 2 3 310
    |> addEdge 3 4 140
    |> addEdge 1 4 225

let weightedGdf = serializeWeighted weightedGraph

printfn "=== Weighted Graph GDF ==="
printfn "%s\n" weightedGdf

(**
Output:
```
nodedef>name VARCHAR,label VARCHAR
1,New York
2,Boston
3,Philadelphia
4,Washington DC
edgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN,weight VARCHAR
1,2,true,215
1,3,true,95
1,4,true,225
2,3,true,310
3,4,true,140
```
*)

(**
## Example 3: Custom Attributes

Export graphs with custom node and edge attributes.
*)

type Person =
    { Name: string
      Age: int
      City: string }

type Relationship =
    { Type: string
      Duration: int
      Strength: float }

let customGraph =
    empty Undirected
    |> addNode 1 { Name = "Alice"; Age = 30; City = "NYC" }
    |> addNode 2 { Name = "Bob"; Age = 25; City = "Boston" }
    |> addNode 3 { Name = "Charlie"; Age = 35; City = "Philly" }
    |> addEdge 1 2 { Type = "friend"; Duration = 5; Strength = 0.8 }
    |> addEdge 2 3 { Type = "colleague"; Duration = 2; Strength = 0.6 }
    |> addEdge 1 3 { Type = "family"; Duration = 30; Strength = 0.95 }

let nodeAttrs (p: Person) =
    [ "label", p.Name
      "age", string p.Age
      "city", p.City ]

let edgeAttrs (r: Relationship) =
    [ "type", r.Type
      "duration", string r.Duration
      "strength", string r.Strength ]

let customGdf = serializeWith nodeAttrs edgeAttrs defaultOptions customGraph

printfn "=== Custom Attributes GDF ==="
printfn "%s\n" customGdf

(**
Output:
```
nodedef>name VARCHAR,label VARCHAR,age VARCHAR,city VARCHAR
1,Alice,30,NYC
2,Bob,25,Boston
3,Charlie,35,Philly
edgedef>node1 VARCHAR,node2 VARCHAR,type VARCHAR,duration VARCHAR,strength VARCHAR
1,2,friend,5,0.8
1,3,family,30,0.95
2,3,colleague,2,0.6
```
*)

(**
## Example 4: Custom Options

Customize the GDF output format with different separators and type annotations.
*)

let tabOptions =
    { Separator = "\t"
      IncludeTypes = false }

let tabGdf = serializeWithOptions tabOptions socialGraph

printfn "=== Tab-Separated GDF (No Types) ==="
printfn "%s\n" tabGdf

(**
Output (with tabs instead of commas):
```
nodedef>name	label
1	Alice
2	Bob
3	Charlie
4	Diana
edgedef>node1	node2	label
1	2	friend
1	4	family
2	3	colleague
3	4	friend
```
*)

(**
## Example 5: Escaping Special Characters

GDF automatically escapes values containing separators or quotes.
*)

let specialGraph =
    empty Directed
    |> addNode 1 "Node with, comma"
    |> addNode 2 "Node with \"quotes\""
    |> addNode 3 "Normal node"
    |> addEdge 1 2 "edge, with, commas"
    |> addEdge 2 3 "normal edge"

let specialGdf = serialize specialGraph

printfn "=== GDF with Special Characters ==="
printfn "%s\n" specialGdf

(**
Output (note the quoted values):
```
nodedef>name VARCHAR,label VARCHAR
1,"Node with, comma"
2,"Node with ""quotes"""
3,Normal node
edgedef>node1 VARCHAR,node2 VARCHAR,directed BOOLEAN,label VARCHAR
1,2,true,"edge, with, commas"
2,3,true,normal edge
```
*)

(**
## Saving to File

To save GDF output to a file for use in Gephi:
*)

open System.IO

let saveGdf filename content =
    File.WriteAllText(filename, content)
    printfn "Saved to %s" filename

// Uncomment to save
// saveGdf "social_network.gdf" socialGdf
// saveGdf "weighted_graph.gdf" weightedGdf
// saveGdf "custom_graph.gdf" customGdf

(**
## Use Cases

GDF format is particularly useful for:

1. **Gephi Import**: Direct import into Gephi for visualization
2. **Quick Prototyping**: Simple format for testing graph layouts
3. **Data Exchange**: Easy to parse and generate
4. **Human Readable**: Can be viewed and edited in text editors
5. **Lightweight**: Smaller file size compared to XML formats like GraphML

## Comparison with Other Formats

| Format   | Complexity | File Size | Features | Tools |
|----------|-----------|-----------|----------|-------|
| **GDF**  | Low       | Small     | Basic    | Gephi |
| GraphML  | High      | Large     | Rich     | Many  |
| DOT      | Medium    | Medium    | Layout   | Graphviz |
| JSON     | Medium    | Medium    | Web      | Custom |
| Mermaid  | Low       | Small     | Docs     | Markdown |

## Tips

- Use `serializeWeighted` for graphs with numeric edge weights
- Use `serializeWith` for custom types with multiple attributes
- Set `IncludeTypes = false` for cleaner output when types aren't needed
- Use tab separator for easier viewing in spreadsheet applications
- GDF is great for Gephi, but use GraphML for more complex metadata

*)
