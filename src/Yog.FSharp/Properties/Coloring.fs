namespace Yog.Properties

open System.Collections.Generic
open Yog.Model

type ColoringResult = int * Map<NodeId, int>

type ExactColoringResult =
    | Ok of ColoringResult
    | Timeout of ColoringResult

module Coloring =

    /// Greedy graph coloring using Welsh-Powell ordering.
    let coloringGreedy (graph: Graph<'n, 'e>) : ColoringResult =
        let nodes = allNodes graph

        if List.isEmpty nodes then
            (0, Map.empty)
        else
            let degree node = (neighbors node graph).Length
            let sorted = nodes |> List.sortBy (fun n -> -degree n)

            let mutable coloring = Map.empty
            let mutable maxColor = 0

            for node in sorted do
                let nbrIds = neighborIds node graph
                let mutable neighborColors = Set.empty

                for nbr in nbrIds do
                    match Map.tryFind nbr coloring with
                    | Some c -> neighborColors <- Set.add c neighborColors
                    | None -> ()

                let rec smallestAvailableColor candidate =
                    if Set.contains candidate neighborColors then
                        smallestAvailableColor (candidate + 1)
                    else
                        candidate

                let color = smallestAvailableColor 1
                coloring <- Map.add node color coloring
                maxColor <- max maxColor color

            (maxColor, coloring)

    /// DSatur heuristic for graph coloring.
    let coloringDsatur (graph: Graph<'n, 'e>) : ColoringResult =
        let nodes = allNodes graph

        if List.isEmpty nodes then
            (0, Map.empty)
        else
            let adj =
                nodes
                |> List.map (fun node -> node, Set.ofList (neighborIds node graph))
                |> Map.ofList

            let degrees =
                nodes
                |> List.map (fun node -> node, (neighbors node graph).Length)
                |> Map.ofList

            let uncolored = HashSet<NodeId>(nodes)

            let forbiddenColors = Dictionary<NodeId, HashSet<int>>()
            let saturations = Dictionary<NodeId, int>()

            for node in nodes do
                forbiddenColors.[node] <- HashSet<int>()
                saturations.[node] <- 0

            let coloring = Dictionary<NodeId, int>()
            let mutable maxColor = 0

            let rec smallestAvailableColor (forbidden: HashSet<int>) candidate =
                if forbidden.Contains(candidate) then
                    smallestAvailableColor forbidden (candidate + 1)
                else
                    candidate

            while uncolored.Count > 0 do
                let node = uncolored |> Seq.maxBy (fun n -> saturations.[n], degrees.[n])

                uncolored.Remove(node) |> ignore

                let color = smallestAvailableColor forbiddenColors.[node] 1
                coloring.[node] <- color
                maxColor <- max maxColor color

                for neighbor in adj.[node] do
                    if uncolored.Contains(neighbor) then
                        let fColors = forbiddenColors.[neighbor]

                        if fColors.Add(color) then
                            saturations.[neighbor] <- saturations.[neighbor] + 1

            let finalColoring = coloring |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
            (maxColor, finalColoring)

    type private ExactColoringState =
        { Adj: Map<NodeId, NodeId list>
          OrderedNodes: NodeId list
          mutable BestChromatic: int
          mutable BestColoring: Map<NodeId, int>
          Deadline: int64
          mutable TimedOut: bool }

    let rec private exactBacktrack
        (nodes: NodeId list)
        (coloring: Map<NodeId, int>)
        (maxUsed: int)
        (state: ExactColoringState)
        : unit =
        if state.TimedOut then
            ()
        elif System.Environment.TickCount64 > state.Deadline then
            state.TimedOut <- true
        else
            match nodes with
            | [] ->
                if maxUsed < state.BestChromatic then
                    state.BestChromatic <- maxUsed
                    state.BestColoring <- coloring
            | node :: rest ->
                let neighbors = state.Adj.[node]
                let mutable neighborColors = Set.empty

                for nbr in neighbors do
                    match Map.tryFind nbr coloring with
                    | Some c -> neighborColors <- Set.add c neighborColors
                    | None -> ()

                let maxExisting = if maxUsed = 0 then 0 else maxUsed

                let mutable current = 1

                while current <= maxExisting && not state.TimedOut do
                    if not (Set.contains current neighborColors) then
                        let newMax = max maxUsed current

                        if newMax < state.BestChromatic then
                            let newColoring = Map.add node current coloring
                            exactBacktrack rest newColoring newMax state

                    current <- current + 1

                if not state.TimedOut then
                    let newColor = maxUsed + 1

                    if newColor < state.BestChromatic && not (Set.contains newColor neighborColors) then
                        let newColoring = Map.add node newColor coloring
                        exactBacktrack rest newColoring newColor state

    /// Exact graph coloring using backtracking with pruning and an optional timeout.
    let coloringExact (graph: Graph<'n, 'e>) (timeoutMs: int option) : ExactColoringResult =
        let timeout = defaultArg timeoutMs 5000
        let nodes = allNodes graph

        if List.isEmpty nodes then
            Ok(0, Map.empty)
        else
            let adj = nodes |> List.map (fun node -> node, neighborIds node graph) |> Map.ofList
            let (upperBound, initialColoring) = coloringDsatur graph
            let orderedNodes = nodes |> List.sortBy (fun n -> -adj.[n].Length)
            let deadline = System.Environment.TickCount64 + int64 timeout

            let state =
                { Adj = adj
                  OrderedNodes = orderedNodes
                  BestChromatic = upperBound
                  BestColoring = initialColoring
                  Deadline = deadline
                  TimedOut = false }

            exactBacktrack orderedNodes Map.empty 0 state

            let result = (state.BestChromatic, state.BestColoring)
            if state.TimedOut then Timeout result else Ok result
