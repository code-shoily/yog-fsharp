namespace Yog.Dag

open Yog.Model
open Yog.Properties.Cyclicity

/// Error indicating a cycle was detected during DAG construction.
type DagError = CycleDetected

/// The 'internal' constructor allows the Model and Algorithms modules
/// to create Dags, but prevents external users from bypassing the guard.
/// 
/// ## Type Parameters
/// - `'n`: Node data type (must support equality)
/// - `'e`: Edge weight type (must support equality)
type Dag<'n, 'e when 'n: equality and 'e: equality> internal (graph: Graph<'n, 'e>) =
    member internal _.InternalGraph = graph

    override this.Equals(other) =
        match other with
        | :? Dag<'n, 'e> as otherDag -> this.InternalGraph = otherDag.InternalGraph
        | _ -> false

    override this.GetHashCode() = hash this.InternalGraph

/// Directed Acyclic Graph (DAG) type and operations.
/// 
/// A DAG is a directed graph with no cycles. This module provides a type-safe
/// wrapper around Graph that guarantees acyclicity through construction,
/// enabling total functions for operations that would be partial on general graphs.
/// 
/// ## Core Concept
/// 
/// The DAG type uses the "smart constructor" pattern:
/// - **fromGraph** validates acyclicity and wraps the graph
/// - **addEdge** returns Result because it could create a cycle
/// - **addNode/removeNode/removeEdge** are safe (cannot create cycles)
/// 
/// Once constructed, the DAG type guarantees no cycles exist, allowing
/// algorithms like topological sort to be total functions.
/// 
/// ## When to Use
/// 
/// | Use Case | Example |
/// |----------|---------|
/// | **Task Scheduling** | Build systems (Make, Gradle), CI/CD pipelines |
/// | **Dependencies** | Package managers (npm, cargo), module imports |
/// | **Partial Orders** | Preference rankings, event causality |
/// | **Control Flow** | Compiler IRs, dataflow graphs |
/// | **Version Control** | Git commit history, merge bases |
/// | **Bayesian Networks** | Probabilistic graphical models |
/// 
/// ## Key Properties
/// 
/// - **Guaranteed acyclic**: Type system prevents cycles at construction
/// - **Topological sort always succeeds**: No need to handle cycle errors
/// - **Longest path is well-defined**: Can find critical paths in O(V+E)
/// - **Shortest path is efficient**: O(V+E) vs O((V+E) log V) for general graphs
/// 
/// ## Comparison with Graph
/// 
/// | Aspect | Dag | Graph |
/// |--------|-----|-------|
/// | Cycles allowed | ❌ No | ✅ Yes |
/// | addEdge result | `Result<Dag, error>` | `Graph` |
/// | topologicalSort | Always succeeds | May fail |
/// | longestPath | O(V+E) | NP-hard |
/// | shortestPath | O(V+E) | O((V+E) log V) |
/// 
/// ## Example
/// 
///     open Yog.Dag
///     
///     // Build a task dependency DAG
///     let dagResult =
///         Yog.Model.empty Directed
///         |> Yog.Model.addNode 0 "Compile"
///         |> Yog.Model.addNode 1 "Link"
///         |> Yog.Model.addNode 2 "Test"
///         |> Yog.Model.addEdge 0 1 ()
///         |> Yog.Model.addEdge 1 2 ()
///         |> Model.fromGraph
///     
///     match dagResult with
///     | Ok dag ->
///         let order = Algorithms.topologicalSort dag
///         printfn "Execution order: %A" order
///     | Error CycleDetected ->
///         printfn "Circular dependency detected!"
/// 
/// ## References
/// 
/// - [Wikipedia: Directed Acyclic Graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph)
/// - [Topological Sorting](https://en.wikipedia.org/wiki/Topological_sorting)
/// - [Smart Constructors](https://wiki.haskell.org/Smart_constructors)
module Model =
    /// The Guard: Uses isAcyclic to validate the graph.
    /// 
    /// ## Parameters
    /// - `graph`: The graph to validate
    /// 
    /// ## Returns
    /// - `Ok dag`: If the graph is acyclic
    /// - `Error CycleDetected`: If the graph contains a cycle
    /// 
    /// ## Example
    /// 
    ///     let result = Model.fromGraph myGraph
    ///     match result with
    ///     | Ok dag -> printfn "Valid DAG"
    ///     | Error CycleDetected -> printfn "Graph has a cycle"
    /// 
    let fromGraph (graph: Graph<'n, 'e>) =
        if isAcyclic graph then
            Ok(Dag<'n, 'e>(graph))
        else
            Error CycleDetected

    /// The Exit: Unwraps back into a standard Graph.
    /// 
    /// ## Example
    /// 
    ///     let graph = Model.toGraph myDag
    /// 
    let toGraph (dag: Dag<'n, 'e>) = dag.InternalGraph

    /// Adds a node. Safe for DAGs (cannot create cycles).
    /// 
    /// ## Example
    /// 
    ///     let dag2 = Model.addNode 3 "New Task" dag
    /// 
    let addNode id data (dag: Dag<'n, 'e>) =
        Dag<'n, 'e>(Yog.Model.addNode id data dag.InternalGraph)

    /// Removes a node. Safe for DAGs.
    /// 
    /// Removing nodes cannot create cycles.
    let removeNode id (dag: Dag<'n, 'e>) =
        Dag<'n, 'e>(Yog.Model.removeNode id dag.InternalGraph)

    /// Removes an edge. Safe for DAGs.
    /// 
    /// Removing edges cannot create cycles.
    let removeEdge src dst (dag: Dag<'n, 'e>) =
        Dag<'n, 'e>(Yog.Model.removeEdge src dst dag.InternalGraph)

    /// Adds an edge. Returns Result because it could create a cycle.
    /// 
    /// ## Parameters
    /// - `src`: Source node ID
    /// - `dst`: Target node ID
    /// - `weight`: Edge weight
    /// - `dag`: The DAG to modify
    /// 
    /// ## Returns
    /// - `Ok updatedDag`: If adding the edge doesn't create a cycle
    /// - `Error CycleDetected`: If the edge would create a cycle
    /// 
    /// ## Example
    /// 
    ///     match Model.addEdge 0 1 weight dag with
    ///     | Ok dag' -> printfn "Edge added"
    ///     | Error CycleDetected -> printfn "Would create cycle"
    /// 
    let addEdge src dst weight (dag: Dag<'n, 'e>) =
        Yog.Model.addEdge src dst weight dag.InternalGraph
        |> fromGraph
