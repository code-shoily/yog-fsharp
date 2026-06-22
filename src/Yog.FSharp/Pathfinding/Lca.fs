namespace Yog.Pathfinding

open System
open System.Collections.Generic
open Yog.Model

type LcaError =
    | RootNotFound
    | NotATree

type LcaState<'n, 'e> =
    { Root: NodeId
      MaxLog: int
      Depth: Map<NodeId, int>
      Up: Map<int, Map<NodeId, NodeId option>>
      Graph: Graph<'n, 'e> }

module Lca =

    /// Preprocesses the tree for LCA queries.
    let preprocess (root: NodeId) (graph: Graph<'n, 'e>) : Result<LcaState<'n, 'e>, LcaError> =
        if not (graph.Nodes.ContainsKey root) then
            Error RootNotFound
        else
            let q = Queue<NodeId * int * NodeId option>()
            q.Enqueue((root, 0, None))
            let visited = HashSet<NodeId>()
            visited.Add(root) |> ignore

            let depth = Dictionary<NodeId, int>()
            let parent = Dictionary<NodeId, NodeId option>()
            let mutable isTree = true

            while q.Count > 0 && isTree do
                let (node, d, par) = q.Dequeue()
                depth.[node] <- d
                parent.[node] <- par

                let neighbors = successorIds node graph

                for nb in neighbors do
                    if Some nb <> par then
                        if visited.Contains(nb) then
                            isTree <- false
                        else
                            visited.Add(nb) |> ignore
                            q.Enqueue((nb, d + 1, Some node))

            if not isTree || visited.Count <> graph.Nodes.Count then
                Error NotATree
            else
                let n = graph.Nodes.Count
                let maxLog = if n <= 1 then 1 else int (Math.Log2(float n)) + 1

                let nodesList = allNodes graph
                let mutable up = Map.empty

                // k = 0
                let up0 = nodesList |> List.map (fun v -> v, parent.[v]) |> Map.ofList
                up <- Map.add 0 up0 up

                for k in 1 .. maxLog - 1 do
                    let prev = Map.find (k - 1) up

                    let next =
                        nodesList
                        |> List.map (fun v ->
                            let p = Map.find v prev

                            let ancestor =
                                match p with
                                | Some parentNode -> Map.find parentNode prev
                                | None -> None

                            v, ancestor)
                        |> Map.ofList

                    up <- Map.add k next up

                Ok
                    { Root = root
                      MaxLog = maxLog
                      Depth = depth |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
                      Up = up
                      Graph = graph }

    let private ancestorAt (state: LcaState<'n, 'e>) (k: int) (v: NodeId option) : NodeId option =
        match v with
        | Some node -> Map.find k state.Up |> Map.find node
        | None -> None

    /// Returns the lowest common ancestor of two nodes.
    let lca (state: LcaState<'n, 'e>) (a: NodeId) (b: NodeId) : NodeId option =
        if not (state.Depth.ContainsKey a) || not (state.Depth.ContainsKey b) then
            None
        else
            let mutable u = a
            let mutable v = b

            if state.Depth.[u] < state.Depth.[v] then
                let temp = u
                u <- v
                v <- temp

            let diff = state.Depth.[u] - state.Depth.[v]

            // Lift u to same depth as v
            for k in state.MaxLog - 1 .. -1 .. 0 do
                if (diff &&& (1 <<< k)) <> 0 then
                    match ancestorAt state k (Some u) with
                    | Some nextU -> u <- nextU
                    | None -> ()

            if u = v then
                Some u
            else
                for k in state.MaxLog - 1 .. -1 .. 0 do
                    let pu = ancestorAt state k (Some u)
                    let pv = ancestorAt state k (Some v)

                    if pu <> pv && pu.IsSome && pv.IsSome then
                        u <- pu.Value
                        v <- pv.Value

                ancestorAt state 0 (Some u)

    /// Calculates the tree distance (number of edges) between two nodes.
    let treeDistance (state: LcaState<'n, 'e>) (a: NodeId) (b: NodeId) : int option =
        match lca state a b with
        | Some ancestor ->
            let depthA = state.Depth.[a]
            let depthB = state.Depth.[b]
            let depthLca = state.Depth.[ancestor]
            Some(depthA + depthB - 2 * depthLca)
        | None -> None
