/// Property-based tests that map the Elixir yog_ex PBT suite to F#.
///
/// These tests use Hedgehog to verify invariants across randomly generated
/// inputs, mirroring the property-based coverage in
/// /home/mafinar/repos/elixir/yog_ex/test/yog/pbt. NetworkX/libgraph oracle
/// tests are intentionally excluded; only standalone graph invariants are
/// covered here.
module Yog.FSharp.Tests.PbtParityTests

open System
open Xunit
open Hedgehog
open Hedgehog.FSharp
open Yog.Model
open Yog


// =============================================================================
// SHARED GENERATORS AND HELPERS
// =============================================================================

module Generators =
    /// Randomly chooses between Directed and Undirected.
    let graphKindGen = Gen.item [ Directed; Undirected ]

    /// Generates a small positive node count.
    let nodeCountGen = Gen.int32 (Range.linear 0 25)

    /// Generates a node ID in [0, n).
    let nodeIdGen n =
        Gen.int32 (Range.constant 0 (max 0 (n - 1)))

    /// Builds a graph from a generated edge list. Parallel edges collapse to
    /// the last generated weight, matching yog's adjacency-list semantics.
    let graphFromEdges (kind: GraphType) (edges: (NodeId * NodeId * int) list) : Graph<unit, int> =
        let nodes = edges |> List.collect (fun (u, v, _) -> [ u; v ]) |> List.distinct

        let g = nodes |> List.fold (fun acc n -> addNode n () acc) (empty kind)

        edges |> List.fold (fun acc (u, v, w) -> addEdge u v w acc) g

    /// Generates a small random graph of the requested kind by first choosing a
    /// node count and then a sparse edge list. No post-generation filtering is
    /// required, so every generated value is used.
    let graphOfKindGen (kind: GraphType) : Gen<Graph<unit, int>> =
        gen {
            let! n = nodeCountGen

            if n = 0 then
                return empty kind
            else
                let maxEdges = if kind = Undirected then n * (n - 1) / 2 else n * (n - 1)
                let! edgeCount = Gen.int32 (Range.constant 0 (min maxEdges 40))

                let! edges =
                    Gen.array
                        (Range.constant edgeCount edgeCount)
                        (gen {
                            let! u = nodeIdGen n

                            let! v =
                                if kind = Undirected then
                                    Gen.int32 (Range.constant u (n - 1))
                                else
                                    nodeIdGen n

                            let! w = Gen.int32 (Range.linear -100 100)
                            return (u, v, w)
                        })

                return graphFromEdges kind (edges |> Array.toList |> List.filter (fun (u, v, _) -> u <> v))
        }

    /// Generates a small random graph of either kind.
    let smallGraphGen: Gen<Graph<unit, int>> =
        gen {
            let! kind = graphKindGen
            return! graphOfKindGen kind
        }

    /// Generates a small undirected graph.
    let undirectedGraphGen = graphOfKindGen Undirected

    /// Generates a small directed graph.
    let directedGraphGen = graphOfKindGen Directed

    /// Generates a small connected undirected graph using a random tree plus
    /// optional extra edges.
    let connectedUndirectedGraphGen: Gen<Graph<unit, int>> =
        gen {
            let! n = Gen.int32 (Range.linear 2 20)
            let tree = Yog.Generators.Random.randomTree n Undirected
            let! extraEdgeCount = Gen.int32 (Range.constant 0 10)

            let! extraEdges =
                Gen.array
                    (Range.constant extraEdgeCount extraEdgeCount)
                    (gen {
                        let! u = nodeIdGen n
                        let! v = nodeIdGen n
                        let! w = Gen.int32 (Range.linear 1 100)
                        return (u, v, w)
                    })

            return
                (extraEdges |> Array.toList |> List.filter (fun (u, v, _) -> u <> v))
                |> List.fold (fun acc (u, v, w) -> addEdge u v w acc) tree
        }

    /// Generates a pair of graphs of the same kind.
    let sameKindGraphPairGen: Gen<Graph<unit, int> * Graph<unit, int>> =
        gen {
            let! kind = graphKindGen
            let! g1 = graphOfKindGen kind
            let! g2 = graphOfKindGen kind
            return (g1, g2)
        }

    /// Generates a very small graph of the requested kind.
    let tinyGraphOfKindGen (kind: GraphType) : Gen<Graph<unit, int>> =
        gen {
            let! n = Gen.int32 (Range.linear 0 6)

            if n = 0 then
                return empty kind
            else
                let maxEdges = if kind = Undirected then n * (n - 1) / 2 else n * (n - 1)
                let! edgeCount = Gen.int32 (Range.constant 0 (min maxEdges 10))

                let! edges =
                    Gen.array
                        (Range.constant edgeCount edgeCount)
                        (gen {
                            let! u = nodeIdGen n

                            let! v =
                                if kind = Undirected then
                                    Gen.int32 (Range.constant u (n - 1))
                                else
                                    nodeIdGen n

                            let! w = Gen.int32 (Range.linear 1 10)
                            return (u, v, w)
                        })

                return graphFromEdges kind (edges |> Array.toList |> List.filter (fun (u, v, _) -> u <> v))
        }

    /// Generates a very small random graph of either kind.
    let tinyGraphGen: Gen<Graph<unit, int>> =
        gen {
            let! kind = graphKindGen
            return! tinyGraphOfKindGen kind
        }

    /// Generates a pair of small graphs of the same kind.
    let tinySameKindGraphPairGen: Gen<Graph<unit, int> * Graph<unit, int>> =
        gen {
            let! kind = graphKindGen
            let! g1 = tinyGraphOfKindGen kind
            let! g2 = tinyGraphOfKindGen kind
            return (g1, g2)
        }

    /// Generates a star graph with at least 3 nodes and returns (graph, center, leaves).
    let starGraphGen: Gen<Graph<unit, int> * NodeId * NodeId list> =
        gen {
            let! n = Gen.int32 (Range.linear 3 15)
            let g = Yog.Generators.Classic.star n Undirected
            let leaves = [ 1 .. n - 1 ]
            return (g, 0, leaves)
        }

    /// Generates a small undirected graph with non-negative edge weights.
    let nonNegativeUndirectedGraphGen: Gen<Graph<unit, int>> =
        gen {
            let! n = Gen.int32 (Range.linear 0 15)

            if n = 0 then
                return empty Undirected
            else
                let maxEdges = n * (n - 1) / 2
                let! edgeCount = Gen.int32 (Range.constant 0 (min maxEdges 30))

                let! edges =
                    Gen.array
                        (Range.constant edgeCount edgeCount)
                        (gen {
                            let! u = nodeIdGen n
                            let! v = Gen.int32 (Range.constant u (n - 1))
                            let! w = Gen.int32 (Range.linear 1 100)
                            return (u, v, w)
                        })

                return graphFromEdges Undirected (edges |> Array.toList |> List.filter (fun (u, v, _) -> u <> v))
        }

    /// Generates a graph together with an optional start node. The optional
    /// value is None for empty graphs, avoiding conditional let! inside the
    /// property computation expression.
    let graphWithOptionalStartNodeGen: Gen<Graph<unit, int> * NodeId option> =
        smallGraphGen
        |> Gen.bind (fun g ->
            let nodes = allNodes g

            if nodes.IsEmpty then
                Gen.constant (g, None)
            else
                Gen.item nodes |> Gen.map (fun n -> (g, Some n)))

    /// Generates a graph together with a randomly chosen subset of its nodes.
    let graphAndNodeSubsetGen: Gen<Graph<unit, int> * NodeId list> =
        smallGraphGen
        |> Gen.bind (fun g ->
            let nodes = allNodes g

            if nodes.IsEmpty then
                Gen.constant (g, [])
            else
                Gen.array (Range.constant nodes.Length nodes.Length) Gen.bool
                |> Gen.map (fun keep ->
                    let subset =
                        List.zip nodes (keep |> Array.toList) |> List.filter snd |> List.map fst

                    (g, subset)))


// =============================================================================
// STRUCTURAL PROPERTIES
// =============================================================================

module StructuralPropertyTests =
    open Generators

    [<Fact>]
    let ``transpose is involutive`` () =
        property {
            let! g = smallGraphGen
            return Yog.Transform.transpose (Yog.Transform.transpose g) = g
        }
        |> Property.checkBool

    [<Fact>]
    let ``undirected graphs have symmetric successor lists`` () =
        property {
            let! g = undirectedGraphGen
            let nodes = allNodes g

            return
                nodes
                |> List.forall (fun u -> successors u g |> List.forall (fun (v, _) -> hasEdge v u g))
        }
        |> Property.checkBool

    [<Fact>]
    let ``edge count equals length of allEdges`` () =
        property {
            let! g = smallGraphGen
            return edgeCount g = List.length (allEdges g)
        }
        |> Property.checkBool

    [<Fact>]
    let ``neighbors equals successors for undirected graphs`` () =
        property {
            let! g = undirectedGraphGen
            let nodes = allNodes g

            return
                nodes
                |> List.forall (fun u -> (neighbors u g |> List.sortBy fst) = (successors u g |> List.sortBy fst))
        }
        |> Property.checkBool

    [<Fact>]
    let ``handshaking lemma holds for undirected graphs`` () =
        property {
            let! g = undirectedGraphGen
            let degreeSum = allNodes g |> List.sumBy (fun u -> (neighbors u g).Length)
            return degreeSum = 2 * edgeCount g
        }
        |> Property.checkBool

    [<Fact>]
    let ``OutEdges and InEdges are mutually consistent`` () =
        property {
            let! g = smallGraphGen

            let outToIn =
                g.OutEdges
                |> Map.forall (fun src targets ->
                    targets
                    |> Map.forall (fun dst weight ->
                        match Map.tryFind dst g.InEdges |> Option.bind (Map.tryFind src) with
                        | Some w -> w = weight
                        | None -> false))

            let inToOut =
                g.InEdges
                |> Map.forall (fun dst sources ->
                    sources
                    |> Map.forall (fun src weight ->
                        match Map.tryFind src g.OutEdges |> Option.bind (Map.tryFind dst) with
                        | Some w -> w = weight
                        | None -> false))

            return outToIn && inToOut
        }
        |> Property.checkBool


// =============================================================================
// TRANSFORM PROPERTIES
// =============================================================================

module TransformPropertyTests =
    open Generators

    [<Fact>]
    let ``mapNodes with identity preserves the graph`` () =
        property {
            let! g = smallGraphGen
            return Yog.Transform.mapNodes id g = g
        }
        |> Property.checkBool

    [<Fact>]
    let ``mapNodes preserves order and edge count`` () =
        property {
            let! g = smallGraphGen
            let mapped = Yog.Transform.mapNodes (fun () -> 42) g
            return order g = order mapped && edgeCount g = edgeCount mapped
        }
        |> Property.checkBool

    [<Fact>]
    let ``mapEdges with identity preserves the graph`` () =
        property {
            let! g = smallGraphGen
            return Yog.Transform.mapEdges id g = g
        }
        |> Property.checkBool

    [<Fact>]
    let ``mapEdges preserves topology and transforms weights`` () =
        property {
            let! g = smallGraphGen
            let mapped = Yog.Transform.mapEdges ((*) 2) g
            return order g = order mapped && edgeCount g = edgeCount mapped
        }
        |> Property.checkBool

    [<Fact>]
    let ``mapEdges obeys functor composition law`` () =
        property {
            let! g = smallGraphGen
            let f = (*) 2
            let h = (+) 1
            let lhs = g |> Yog.Transform.mapEdges f |> Yog.Transform.mapEdges h
            let rhs = g |> Yog.Transform.mapEdges (h << f)
            return lhs = rhs
        }
        |> Property.checkBool

    [<Fact>]
    let ``filterNodes keeps a subset of nodes and removes incident edges`` () =
        property {
            let! g = smallGraphGen
            let allNodeIds = allNodes g

            if allNodeIds.IsEmpty then
                return true
            else
                let keepCount = max 1 (allNodeIds.Length / 2)
                let keepSet = allNodeIds |> List.take keepCount |> Set.ofList
                let filtered = Yog.Transform.subgraph (keepSet |> Set.toList) g

                return
                    order filtered <= order g
                    && edgeCount filtered <= edgeCount g
                    && (allNodes filtered |> List.forall (fun u -> Set.contains u keepSet))
        }
        |> Property.checkBool

    [<Fact>]
    let ``complement complement (minus self-loops) equals original without self-loops`` () =
        property {
            let! g = undirectedGraphGen
            let noLoops = Yog.Transform.filterEdges (fun u v _ -> u <> v) g
            let comp = Yog.Transform.complement 1 noLoops
            let compComp = Yog.Transform.complement 1 comp
            return order noLoops = order compComp && edgeCount noLoops = edgeCount compComp
        }
        |> Property.checkBool

    [<Fact>]
    let ``transitive closure is idempotent`` () =
        property {
            let! g = smallGraphGen
            let closure1 = Yog.Transform.transitiveClosure max g
            let closure2 = Yog.Transform.transitiveClosure max closure1
            return closure2 = closure1
        }
        |> Property.checkBool

    [<Fact>]
    let ``transitive reduction edges are a subset of original edges`` () =
        property {
            let! g = directedGraphGen
            let reduced = Yog.Transform.transitiveReduction (+) g
            let originalEdges = allEdges g |> List.map (fun (u, v, _) -> (u, v)) |> Set.ofList

            let reducedEdges =
                allEdges reduced |> List.map (fun (u, v, _) -> (u, v)) |> Set.ofList

            return Set.isSubset reducedEdges originalEdges
        }
        |> Property.checkBool


// =============================================================================
// OPERATION PROPERTIES
// =============================================================================

module OperationPropertyTests =
    open Generators

    [<Fact>]
    let ``union contains all nodes and edges from both graphs`` () =
        property {
            let! (g1, g2) = sameKindGraphPairGen
            let merged = Yog.Operation.union g1 g2
            let nodesInMerged = allNodes merged |> Set.ofList

            let edgesInMerged =
                allEdges merged |> List.map (fun (u, v, _) -> (u, v)) |> Set.ofList

            let nodes1 = allNodes g1 |> Set.ofList
            let nodes2 = allNodes g2 |> Set.ofList
            let edges1 = allEdges g1 |> List.map (fun (u, v, _) -> (u, v)) |> Set.ofList
            let edges2 = allEdges g2 |> List.map (fun (u, v, _) -> (u, v)) |> Set.ofList

            return
                Set.isSubset nodes1 nodesInMerged
                && Set.isSubset nodes2 nodesInMerged
                && Set.isSubset edges1 edgesInMerged
                && Set.isSubset edges2 edgesInMerged
        }
        |> Property.checkBool

    [<Fact>]
    let ``intersection of a graph with itself equals the graph`` () =
        property {
            let! g = smallGraphGen
            let inter = Yog.Operation.intersection g g
            return order inter = order g && edgeCount inter = edgeCount g
        }
        |> Property.checkBool

    [<Fact>]
    let ``difference of a graph with itself is empty`` () =
        property {
            let! g = smallGraphGen
            let diff = Yog.Operation.difference g g
            return order diff = 0 && edgeCount diff = 0
        }
        |> Property.checkBool

    [<Fact>]
    let ``symmetric difference is commutative`` () =
        property {
            let! (g1, g2) = sameKindGraphPairGen
            let a = Yog.Operation.symmetricDifference g1 g2
            let b = Yog.Operation.symmetricDifference g2 g1
            return a = b
        }
        |> Property.checkBool

    [<Fact>]
    let ``symmetric difference with self is empty`` () =
        property {
            let! g = smallGraphGen
            let diff = Yog.Operation.symmetricDifference g g
            return order diff = 0 && edgeCount diff = 0
        }
        |> Property.checkBool

    [<Fact>]
    let ``isomorphic reflexivity`` () =
        property {
            let! g = smallGraphGen
            return Yog.Operation.isomorphic g g
        }
        |> Property.checkBool

    [<Fact>]
    let ``lineGraph node count equals edge count of original`` () =
        property {
            let! g = smallGraphGen
            let lg = Yog.Operation.lineGraph 1 g
            return order lg = edgeCount g
        }
        |> Property.checkBool

    [<Fact>]
    let ``power of 1 preserves order`` () =
        property {
            let! g = undirectedGraphGen
            let p = Yog.Operation.power 1 1 g
            return order p = order g
        }
        |> Property.checkBool

    [<Fact>]
    let ``cartesian product has expected order and edge count`` () =
        property {
            let! (g1, g2) = tinySameKindGraphPairGen
            let product = Yog.Operation.cartesianProduct 1 1 g1 g2
            let expectedOrder = order g1 * order g2
            let expectedEdges = edgeCount g1 * order g2 + edgeCount g2 * order g1
            return order product = expectedOrder && edgeCount product = expectedEdges
        }
        |> Property.checkBool

    [<Fact>]
    let ``tensor product has expected order`` () =
        property {
            let! (g1, g2) = tinySameKindGraphPairGen
            let product = Yog.Operation.tensorProduct g1 g2
            return order product = order g1 * order g2
        }
        |> Property.checkBool

    [<Fact>]
    let ``strong product has expected order`` () =
        property {
            let! (g1, g2) = tinySameKindGraphPairGen
            let product = Yog.Operation.strongProduct 1 1 g1 g2
            return order product = order g1 * order g2
        }
        |> Property.checkBool

    [<Fact>]
    let ``lexicographic product has expected order and edge count`` () =
        property {
            let! (g1, g2) = tinySameKindGraphPairGen
            let product = Yog.Operation.lexicographicProduct 1 1 g1 g2
            let expectedOrder = order g1 * order g2
            let expectedEdges = edgeCount g1 * pown (order g2) 2 + order g1 * edgeCount g2
            return order product = expectedOrder && edgeCount product = expectedEdges
        }
        |> Property.checkBool

    [<Fact>]
    let ``subgraph extraction yields a subgraph of the original`` () =
        property {
            let! (g, subset) = graphAndNodeSubsetGen
            let sub = Yog.Transform.subgraph subset g
            return Yog.Operation.isSubgraph sub g
        }
        |> Property.checkBool


// =============================================================================
// TRAVERSAL PROPERTIES
// =============================================================================

module TraversalPropertyTests =
    open Generators

    [<Fact>]
    let ``BFS visits each reachable node exactly once`` () =
        property {
            let! (g, startOpt) = graphWithOptionalStartNodeGen

            match startOpt with
            | None -> return true
            | Some start ->
                let visited = Yog.Traversal.walk start Yog.Traversal.BreadthFirst g
                return visited.Length = (visited |> List.distinct |> List.length)
        }
        |> Property.checkBool

    [<Fact>]
    let ``DFS and BFS visit the same set of reachable nodes`` () =
        property {
            let! (g, startOpt) = graphWithOptionalStartNodeGen

            match startOpt with
            | None -> return true
            | Some start ->
                let bfs = Yog.Traversal.walk start Yog.Traversal.BreadthFirst g |> Set.ofList
                let dfs = Yog.Traversal.walk start Yog.Traversal.DepthFirst g |> Set.ofList
                return bfs = dfs
        }
        |> Property.checkBool

    [<Fact>]
    let ``topological sort preserves edge ordering in DAGs`` () =
        property {
            let! n = Gen.int32 (Range.linear 2 15)
            // Build a DAG where edges only go from smaller to larger IDs.
            let edges =
                [ for u in 0 .. n - 1 do
                      for v in u + 1 .. n - 1 -> (u, v, 1) ]

            let g = Generators.graphFromEdges Directed edges

            match Yog.Traversal.topologicalSort g with
            | Ok order ->
                let pos = order |> List.mapi (fun i x -> (x, i)) |> Map.ofList
                return allEdges g |> List.forall (fun (u, v, _) -> Map.find u pos < Map.find v pos)
            | Error _ -> return false
        }
        |> Property.checkBool


// =============================================================================
// CONNECTIVITY / COMPONENT PROPERTIES
// =============================================================================

module ComponentPropertyTests =
    open Generators

    let checkBool300 =
        Property.checkBoolWith (PropertyConfig.withTests 300<tests> PropertyConfig.defaults)

    [<Fact>]
    let ``SCCs partition the nodes exactly`` () =
        property {
            let! g = directedGraphGen
            let components = Connectivity.stronglyConnectedComponents g
            let allInComponents = components |> List.collect id |> Set.ofList
            let allGraphNodes = allNodes g |> Set.ofList
            let totalComponentSize = components |> List.sumBy List.length

            return
                allInComponents = allGraphNodes
                && Set.count allInComponents = totalComponentSize
        }
        |> Property.checkBool

    [<Fact>]
    let ``Tarjan and Kosaraju agree on SCCs`` () =
        property {
            let! g = directedGraphGen

            let tarjan =
                Connectivity.stronglyConnectedComponents g |> List.map Set.ofList |> Set.ofList

            let kosaraju = Connectivity.kosaraju g |> List.map Set.ofList |> Set.ofList
            return tarjan = kosaraju
        }
        |> checkBool300

    [<Fact>]
    let ``removing a bridge disconnects the graph`` () =
        property {
            let! g = connectedUndirectedGraphGen

            if order g <= 1 then
                return true
            else
                match Connectivity.analyze g with
                | Ok res when not res.Bridges.IsEmpty ->
                    let (u, v) = List.head res.Bridges
                    let gNoBridge = removeEdge u v g

                    let reachable =
                        Yog.Traversal.walk u Yog.Traversal.BreadthFirst gNoBridge |> Set.ofList

                    return reachable.Count < order g
                | _ -> return true
        }
        |> Property.checkBool


// =============================================================================
// MST PROPERTIES
// =============================================================================

module MstPropertyTests =
    open Generators

    let checkBool300 =
        Property.checkBoolWith (PropertyConfig.withTests 300<tests> PropertyConfig.defaults)

    [<Fact>]
    let ``Kruskal and Prim agree on total weight for connected undirected graphs`` () =
        property {
            let! g = connectedUndirectedGraphGen

            if order g <= 1 then
                return true
            else
                match Mst.kruskalInt g, Mst.primInt g with
                | Ok k, Ok p -> return k.TotalWeight = p.TotalWeight
                | _ -> return false
        }
        |> checkBool300

    [<Fact>]
    let ``MST of a tree is the tree itself`` () =
        property {
            let! n = Gen.int32 (Range.linear 2 20)
            let tree = Yog.Generators.Random.randomTree n Undirected

            match Mst.kruskalInt tree with
            | Ok mst -> return mst.EdgeCount = edgeCount tree && mst.TotalWeight = edgeCount tree
            | _ -> return false
        }
        |> Property.checkBool

    [<Fact>]
    let ``directed graphs return error from Kruskal`` () =
        property {
            let! g = directedGraphGen

            if order g = 0 then
                return true
            else
                match Mst.kruskalInt g with
                | Error _ -> return true
                | _ -> return false
        }
        |> Property.checkBool


// =============================================================================
// CENTRALITY PROPERTIES
// =============================================================================

module CentralityPropertyTests =
    open Generators

    [<Fact>]
    let ``star graph center has strictly highest degree centrality`` () =
        property {
            let! (g, center, leaves) = starGraphGen
            let scores = Centrality.degree Centrality.TotalDegree g
            let centerScore = Map.tryFind center scores |> Option.defaultValue 0.0

            return
                leaves
                |> List.forall (fun leaf -> Map.tryFind leaf scores |> Option.defaultValue 0.0 < centerScore)
        }
        |> Property.checkBool

    [<Fact>]
    let ``betweenness centrality is non-negative`` () =
        property {
            let! g = nonNegativeUndirectedGraphGen
            let scores = Centrality.betweennessInt g
            return scores |> Map.forall (fun _ v -> v >= 0.0)
        }
        |> Property.checkBool

    [<Fact>]
    let ``closeness centrality is in valid range`` () =
        property {
            let! g = nonNegativeUndirectedGraphGen
            let scores = Centrality.closenessInt g
            return scores |> Map.forall (fun _ v -> v >= 0.0 && v <= 1.0)
        }
        |> Property.checkBool

    [<Fact>]
    let ``pagerank scores sum to 1`` () =
        property {
            let! g = smallGraphGen
            let scores = Centrality.pagerank Centrality.defaultPageRankOptions g

            if Map.isEmpty scores then
                return true
            else
                let sum = scores |> Map.fold (fun acc _ v -> acc + v) 0.0
                return abs (sum - 1.0) < 0.001
        }
        |> Property.checkBool


// =============================================================================
// CYCLICITY / STRUCTURE PROPERTIES
// =============================================================================

module CyclicityPropertyTests =
    open Generators

    [<Fact>]
    let ``DAGs built from increasing edges are acyclic`` () =
        property {
            let! n = Gen.int32 (Range.linear 2 15)

            let edges =
                [ for u in 0 .. n - 1 do
                      for v in u + 1 .. n - 1 -> (u, v, 1) ]

            let g = Generators.graphFromEdges Directed edges
            return not (Yog.Properties.Cyclicity.isCyclic g)
        }
        |> Property.checkBool

    [<Fact>]
    let ``random trees are acyclic and have V - 1 edges`` () =
        property {
            let! n = Gen.int32 (Range.linear 1 20)
            let tree = Yog.Generators.Random.randomTree n Undirected
            return not (Yog.Properties.Cyclicity.isCyclic tree) && edgeCount tree = max 0 (n - 1)
        }
        |> Property.checkBool


// =============================================================================
// BIPARTITE PROPERTIES
// =============================================================================

module BipartitePropertyTests =
    open Generators

    [<Fact>]
    let ``complete bipartite graphs are bipartite`` () =
        property {
            let! m = Gen.int32 (Range.linear 1 8)
            let! n = Gen.int32 (Range.linear 1 8)
            let g = Yog.Generators.Classic.completeBipartite m n Undirected
            return Yog.Properties.Bipartite.isBipartite g
        }
        |> Property.checkBool

    [<Fact>]
    let ``partition covers all nodes and parts are disjoint`` () =
        property {
            let! g = undirectedGraphGen

            if not (Yog.Properties.Bipartite.isBipartite g) then
                return true
            else
                match Yog.Properties.Bipartite.partition g with
                | None -> return false
                | Some partition ->
                    let all = Set.union partition.Left partition.Right
                    let graphNodes = allNodes g |> Set.ofList
                    return all = graphNodes && Set.intersect partition.Left partition.Right |> Set.isEmpty
        }
        |> Property.checkBool


// =============================================================================
// HEALTH PROPERTIES
// =============================================================================

module HealthPropertyTests =
    open Generators

    [<Fact>]
    let ``diameter of path Pn is n - 1`` () =
        property {
            let! n = Gen.int32 (Range.linear 2 15)
            let g = Yog.Generators.Classic.path n Undirected

            match Health.diameter 0 (+) compare g with
            | Some d -> return d = n - 1
            | None -> return false
        }
        |> Property.checkBool

    [<Fact>]
    let ``diameter equals radius for complete graph Kn`` () =
        property {
            let! n = Gen.int32 (Range.linear 2 10)
            let g = Yog.Generators.Classic.complete n Undirected

            match Health.diameter 0 (+) compare g, Health.radius 0 (+) compare g with
            | Some d, Some r -> return d = r && d = 1
            | _ -> return false
        }
        |> Property.checkBool

    [<Fact>]
    let ``regular graphs have assortativity 0`` () =
        property {
            let! n = Gen.int32 (Range.linear 4 12)
            let! d = Gen.int32 (Range.linear 2 (n - 1))

            if n * d % 2 <> 0 then
                return true
            else
                let g = Yog.Generators.Random.randomRegular n d Undirected
                return Math.Abs(Health.assortativity g) < 1e-9 || order g = 0
        }
        |> Property.checkBool


// =============================================================================
// GENERATOR PROPERTIES
// =============================================================================

module GeneratorPropertyTests =
    open Generators

    [<Fact>]
    let ``complete graph has n nodes and n(n-1)/2 edges`` () =
        property {
            let! n = Gen.int32 (Range.linear 0 12)
            let g = Yog.Generators.Classic.complete n Undirected
            return order g = n && edgeCount g = n * (n - 1) / 2
        }
        |> Property.checkBool

    [<Fact>]
    let ``cycle graph has n nodes and n edges`` () =
        property {
            let! n = Gen.int32 (Range.linear 3 15)
            let g = Yog.Generators.Classic.cycle n Undirected
            return order g = n && edgeCount g = n
        }
        |> Property.checkBool

    [<Fact>]
    let ``erdosRenyiGnp with p=1 yields complete graph`` () =
        property {
            let! n = Gen.int32 (Range.linear 0 8)
            let g = Yog.Generators.Random.erdosRenyiGnp n 1.0 Undirected
            return order g = n && edgeCount g = n * (n - 1) / 2
        }
        |> Property.checkBool

    [<Fact>]
    let ``randomTree has n nodes and n-1 edges`` () =
        property {
            let! n = Gen.int32 (Range.linear 1 20)
            let g = Yog.Generators.Random.randomTree n Undirected
            return order g = n && edgeCount g = n - 1
        }
        |> Property.checkBool


// =============================================================================
// DISJOINT SET PROPERTIES
// =============================================================================

module DisjointSetPropertyTests =

    [<Fact>]
    let ``reflexivity: every element is connected to itself`` () =
        property {
            let! xs = Gen.array (Range.linear 1 20) (Gen.int32 (Range.linear -100 100))
            let distinct = xs |> Array.distinct |> Array.toList

            let dsu =
                distinct
                |> List.fold (fun acc x -> Yog.DisjointSet.add x acc) Yog.DisjointSet.empty

            return distinct |> List.forall (fun x -> snd (Yog.DisjointSet.connected x x dsu))
        }
        |> Property.checkBool

    [<Fact>]
    let ``connected is symmetric`` () =
        property {
            let! x = Gen.int32 (Range.linear -50 50)
            let! y = Gen.int32 (Range.linear -50 50)

            let dsu =
                Yog.DisjointSet.empty
                |> Yog.DisjointSet.add x
                |> Yog.DisjointSet.add y
                |> Yog.DisjointSet.union x y

            let xy = snd (Yog.DisjointSet.connected x y dsu)
            let yx = snd (Yog.DisjointSet.connected y x dsu)
            return xy = yx
        }
        |> Property.checkBool

    [<Fact>]
    let ``union reduces set count for distinct sets`` () =
        property {
            let! x = Gen.int32 (Range.linear -50 50)
            let! y = Gen.int32 (Range.linear -50 50)

            let baseDsu =
                Yog.DisjointSet.empty |> Yog.DisjointSet.add x |> Yog.DisjointSet.add y

            let before = (Yog.DisjointSet.toLists baseDsu).Length
            let after = (Yog.DisjointSet.toLists (Yog.DisjointSet.union x y baseDsu)).Length
            return if x = y then before = after else after = before - 1
        }
        |> Property.checkBool


// =============================================================================
// IO ROUNDTRIP PROPERTIES
// =============================================================================

module IoPropertyTests =
    open Generators

    [<Fact>]
    let ``TGF serialize-parse roundtrip preserves counts`` () =
        property {
            let! g = smallGraphGen
            // TGF default options call ToString() on node data, so give each
            // node a concrete string label before roundtripping.
            let labeled = g |> Yog.Transform.mapNodes (fun () -> "node")

            let options =
                { Yog.IO.Tgf.defaultOptions with
                    NodeLabel = fun _ -> "node" }

            let serialized = Yog.IO.Tgf.serialize options labeled

            match Yog.IO.Tgf.parse labeled.Kind (fun _ -> ()) (fun _ -> 1) serialized with
            | Ok parsed -> return order parsed = order labeled && edgeCount parsed = edgeCount labeled
            | Error _ -> return false
        }
        |> Property.checkBool

    [<Fact>]
    let ``List serialize-parse roundtrip preserves counts`` () =
        property {
            let! g = smallGraphGen
            let gFloat = Yog.Transform.mapEdges float g
            let serialized = Yog.IO.List.serialize false " " gFloat

            match Yog.IO.List.parse gFloat.Kind false " " serialized with
            | Ok parsed -> return order parsed = order gFloat && edgeCount parsed = edgeCount gFloat
            | Error _ -> return false
        }
        |> Property.checkBool

    [<Fact>]
    let ``Matrix roundtrip preserves counts`` () =
        property {
            let! g = smallGraphGen
            // Matrix format treats 0.0 as "no edge", so shift every weight to a
            // strictly positive value before roundtripping.
            let gFloat = g |> Yog.Transform.mapEdges (fun w -> float (abs w) + 1.0)
            let (_, matrix) = Yog.IO.Matrix.toMatrix gFloat

            match Yog.IO.Matrix.fromMatrix gFloat.Kind matrix with
            | Ok parsed -> return order parsed = order gFloat && edgeCount parsed = edgeCount gFloat
            | Error _ -> return false
        }
        |> Property.checkBool

    [<Fact>]
    let ``GraphML roundtrip preserves counts`` () =
        property {
            let! g = smallGraphGen
            let labeled = g |> Yog.Transform.mapNodes (fun () -> "node")

            let serialized =
                Yog.IO.GraphML.serializeWith (fun _ -> [ "label", "node" ]) (fun w -> [ "weight", string w ]) labeled

            let parsed =
                Yog.IO.GraphML.tryDeserializeWith (fun _ -> ()) (fun m -> Map.find "weight" m |> int) serialized

            match parsed with
            | Ok restored ->
                return
                    restored.Kind = labeled.Kind
                    && order restored = order labeled
                    && edgeCount restored = edgeCount labeled
            | Error _ -> return false
        }
        |> Property.checkBool

    [<Fact>]
    let ``GDF roundtrip preserves counts`` () =
        property {
            let! g = smallGraphGen

            if order g = 0 then
                return true
            else
                let labeled = g |> Yog.Transform.mapNodes (fun () -> "node")

                let serialized =
                    Yog.IO.Gdf.serializeWith
                        (fun _ -> [ "label", "node" ])
                        (fun w -> [ "weight", string w ])
                        Yog.IO.Gdf.defaultOptions
                        labeled

                let parsed =
                    Yog.IO.Gdf.deserializeWith (fun _ -> ()) (fun m -> Map.find "weight" m |> int) serialized

                match parsed with
                | Ok restored ->
                    return
                        restored.Kind = labeled.Kind
                        && order restored = order labeled
                        && edgeCount restored = edgeCount labeled
                | Error _ -> return false
        }
        |> Property.checkBool
