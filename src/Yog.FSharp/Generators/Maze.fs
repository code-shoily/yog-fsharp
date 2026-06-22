namespace Yog.Generators

open System
open System.Collections.Generic
open Yog
open Yog.Model
open Yog.Builder

/// Perfect maze generators on 2D grids (spanning trees).
module Maze =
    let private rng = Random()

    /// Helper to create a grid with rows x cols nodes and no edges.
    let private createEmptyGrid rows cols : Grid<unit, int> =
        let mutable graph = empty Undirected

        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                let id = r * cols + c
                graph <- addNode id () graph

        { Graph = graph
          Rows = rows
          Cols = cols }

    /// Helper to add a passage (undirected edge) between adjacent cells.
    let private addPassage (grid: Grid<unit, int>) r1 c1 r2 c2 : Grid<unit, int> =
        let id1 = r1 * grid.Cols + c1
        let id2 = r2 * grid.Cols + c2

        { grid with
            Graph = addEdge id1 id2 1 grid.Graph }

    /// Helper to remove a passage (undirected edge) between adjacent cells.
    let private removePassage (grid: Grid<unit, int>) r1 c1 r2 c2 : Grid<unit, int> =
        let id1 = r1 * grid.Cols + c1
        let id2 = r2 * grid.Cols + c2

        { grid with
            Graph = removeEdge id1 id2 grid.Graph }

    /// Helper to get all neighbors of a cell on a 2D grid.
    let private getNeighbors (r, c) rows cols =
        let mutable neighbors = []

        if r > 0 then
            neighbors <- (r - 1, c) :: neighbors

        if r < rows - 1 then
            neighbors <- (r + 1, c) :: neighbors

        if c > 0 then
            neighbors <- (r, c - 1) :: neighbors

        if c < cols - 1 then
            neighbors <- (r, c + 1) :: neighbors

        neighbors

    /// Helper to pick a random neighbor of a cell.
    let private pickAnyNeighbor cell rows cols =
        let list = getNeighbors cell rows cols
        list.[rng.Next(list.Length)]

    /// Helper to get unvisited neighbors of a cell.
    let private unvisitedNeighbors cell (visited: HashSet<int * int>) rows cols =
        getNeighbors cell rows cols |> List.filter (fun n -> not (visited.Contains(n)))

    /// Helper to get visited neighbors of a cell.
    let private visitedNeighbors cell (visited: HashSet<int * int>) rows cols =
        getNeighbors cell rows cols |> List.filter (fun n -> visited.Contains(n))

    /// Helper to find a visited neighbor of a cell.
    let private findVisitedNeighbor cell visited rows cols =
        let list = visitedNeighbors cell visited rows cols
        list.[rng.Next(list.Length)]

    /// Generates a perfect maze using the Binary Tree algorithm.
    let binaryTree rows cols bias : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols

            for r in 0 .. rows - 1 do
                for c in 0 .. cols - 1 do
                    let mutable neighbors = []

                    match bias with
                    | "ne" ->
                        if r > 0 then
                            neighbors <- (r - 1, c) :: neighbors

                        if c < cols - 1 then
                            neighbors <- (r, c + 1) :: neighbors
                    | "nw" ->
                        if r > 0 then
                            neighbors <- (r - 1, c) :: neighbors

                        if c > 0 then
                            neighbors <- (r, c - 1) :: neighbors
                    | "se" ->
                        if r < rows - 1 then
                            neighbors <- (r + 1, c) :: neighbors

                        if c < cols - 1 then
                            neighbors <- (r, c + 1) :: neighbors
                    | "sw" ->
                        if r < rows - 1 then
                            neighbors <- (r + 1, c) :: neighbors

                        if c > 0 then
                            neighbors <- (r, c - 1) :: neighbors
                    | _ -> // default to ne
                        if r > 0 then
                            neighbors <- (r - 1, c) :: neighbors

                        if c < cols - 1 then
                            neighbors <- (r, c + 1) :: neighbors

                    if not neighbors.IsEmpty then
                        let (nr, nc) = neighbors.[rng.Next(neighbors.Length)]
                        grid <- addPassage grid r c nr nc

            grid

    /// Generates a perfect maze using the Sidewinder algorithm.
    let sidewinder rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols

            for r in 0 .. rows - 1 do
                let mutable runStartCol = 0

                for c in 0 .. cols - 1 do
                    let atEastEnd = (c = cols - 1)
                    let atNorthEdge = (r = 0)

                    let shouldCloseRun =
                        if atNorthEdge then
                            false
                        else
                            atEastEnd || rng.NextDouble() > 0.5

                    if r = rows - 1 && not atEastEnd then
                        grid <- addPassage grid r c r (c + 1)
                    elif r = rows - 1 && atEastEnd then
                        let runCol =
                            if runStartCol = c then
                                c
                            else
                                runStartCol + rng.Next(c - runStartCol + 1)

                        grid <- addPassage grid r runCol (r - 1) runCol
                    elif atNorthEdge && not atEastEnd then
                        grid <- addPassage grid r c r (c + 1)
                    elif atNorthEdge && atEastEnd then
                        ()
                    elif shouldCloseRun then
                        let runCol =
                            if runStartCol = c then
                                c
                            else
                                runStartCol + rng.Next(c - runStartCol + 1)

                        grid <- addPassage grid r runCol (r - 1) runCol
                        runStartCol <- c + 1
                    else
                        grid <- addPassage grid r c r (c + 1)

            grid

    /// Generates a perfect maze using the Recursive Backtracker (DFS) algorithm.
    let recursiveBacktracker rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols
            let startRow = rng.Next(rows)
            let startCol = rng.Next(cols)

            let stack = Stack<int * int>()
            let visited = HashSet<int * int>()

            stack.Push((startRow, startCol))
            visited.Add((startRow, startCol)) |> ignore

            while stack.Count > 0 do
                let (r, c) = stack.Peek()
                let neighbors = unvisitedNeighbors (r, c) visited rows cols

                if neighbors.IsEmpty then
                    stack.Pop() |> ignore
                else
                    let (nr, nc) = neighbors.[rng.Next(neighbors.Length)]
                    grid <- addPassage grid r c nr nc
                    visited.Add((nr, nc)) |> ignore
                    stack.Push((nr, nc))

            grid

    /// Generates a perfect maze using the Hunt-and-Kill algorithm.
    let huntAndKill rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols
            let startRow = rng.Next(rows)
            let startCol = rng.Next(cols)

            let visited = HashSet<int * int>()
            visited.Add((startRow, startCol)) |> ignore

            let mutable current = (startRow, startCol)
            let mutable finished = false
            let total = rows * cols

            while visited.Count < total && not finished do
                let (r, c) = current
                let unvisitedNeighborsList = unvisitedNeighbors (r, c) visited rows cols

                if not unvisitedNeighborsList.IsEmpty then
                    let nextCell = unvisitedNeighborsList.[rng.Next(unvisitedNeighborsList.Length)]
                    let (nr, nc) = nextCell
                    grid <- addPassage grid r c nr nc
                    visited.Add(nextCell) |> ignore
                    current <- nextCell
                else
                    // Hunt phase
                    let mutable found = false
                    let mutable rIdx = 0

                    while rIdx < rows && not found do
                        let mutable cIdx = 0

                        while cIdx < cols && not found do
                            let cell = (rIdx, cIdx)

                            if not (visited.Contains(cell)) then
                                let visitedNeighborsList = visitedNeighbors cell visited rows cols

                                if not visitedNeighborsList.IsEmpty then
                                    let vNeighbor = visitedNeighborsList.[rng.Next(visitedNeighborsList.Length)]
                                    grid <- addPassage grid rIdx cIdx (fst vNeighbor) (snd vNeighbor)
                                    visited.Add(cell) |> ignore
                                    current <- cell
                                    found <- true

                            cIdx <- cIdx + 1

                        rIdx <- rIdx + 1

                    if not found then
                        finished <- true

            grid

    /// Generates a perfect maze using the Aldous-Broder algorithm.
    let aldousBroder rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols
            let totalCells = rows * cols
            let visited = HashSet<int * int>()
            let mutable current = (rng.Next(rows), rng.Next(cols))
            visited.Add(current) |> ignore
            let mutable count = 1

            while count < totalCells do
                let nextCell = pickAnyNeighbor current rows cols

                if not (visited.Contains(nextCell)) then
                    grid <- addPassage grid (fst current) (snd current) (fst nextCell) (snd nextCell)
                    visited.Add(nextCell) |> ignore
                    count <- count + 1

                current <- nextCell

            grid

    /// Generates a perfect maze using Wilson's algorithm.
    let wilson rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols

            let allCells =
                [ for r in 0 .. rows - 1 do
                      for c in 0 .. cols - 1 -> (r, c) ]

            let shuffled = allCells |> List.sortBy (fun _ -> rng.Next())
            let visited = HashSet<int * int>()
            let unvisited = HashSet<int * int>(shuffled)

            let first = shuffled.[0]
            visited.Add(first) |> ignore
            unvisited.Remove(first) |> ignore

            while unvisited.Count > 0 do
                let unvisitedList = [ for c in unvisited -> c ]
                let startCell = unvisitedList.[rng.Next(unvisitedList.Length)]

                let arrows = Dictionary<int * int, int * int>()
                let mutable current = startCell
                let mutable walking = true

                while walking do
                    let nextCell = pickAnyNeighbor current rows cols
                    arrows.[current] <- nextCell

                    if visited.Contains(nextCell) then
                        walking <- false
                    else
                        current <- nextCell

                let mutable pathCell = startCell
                let mutable carving = true

                while carving do
                    let nextCell = arrows.[pathCell]
                    grid <- addPassage grid (fst pathCell) (snd pathCell) (fst nextCell) (snd nextCell)
                    visited.Add(pathCell) |> ignore
                    unvisited.Remove(pathCell) |> ignore

                    if visited.Contains(nextCell) then
                        carving <- false
                    else
                        pathCell <- nextCell

            grid

    /// Generates a perfect maze using Kruskal's algorithm.
    let kruskal rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols
            let mutable dsu = DisjointSet.empty

            for r in 0 .. rows - 1 do
                for c in 0 .. cols - 1 do
                    dsu <- DisjointSet.add (r, c) dsu

            let horiz =
                [ for r in 0 .. rows - 1 do
                      for c in 0 .. cols - 2 -> ((r, c), (r, c + 1)) ]

            let vert =
                [ for r in 0 .. rows - 2 do
                      for c in 0 .. cols - 1 -> ((r, c), (r + 1, c)) ]

            let edges = horiz @ vert |> List.sortBy (fun _ -> rng.Next())

            for (u, v) in edges do
                let (dsuTemp, isConnected) = DisjointSet.connected u v dsu
                dsu <- dsuTemp

                if not isConnected then
                    grid <- addPassage grid (fst u) (snd u) (fst v) (snd v)
                    dsu <- DisjointSet.union u v dsu

            grid

    /// Generates a perfect maze using the Simplified Prim's algorithm.
    let primSimplified rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols
            let startCell = (rng.Next(rows), rng.Next(cols))
            let visited = HashSet<int * int>([ startCell ])
            let frontier = List<int * int>(unvisitedNeighbors startCell visited rows cols)

            while frontier.Count > 0 do
                let idx = rng.Next(frontier.Count)
                let cell = frontier.[idx]
                frontier.RemoveAt(idx)

                let (nr, nc) = findVisitedNeighbor cell visited rows cols
                grid <- addPassage grid (fst cell) (snd cell) nr nc
                visited.Add(cell) |> ignore

                for n in unvisitedNeighbors cell visited rows cols do
                    if not (frontier.Contains(n)) then
                        frontier.Add(n)

            grid

    /// Generates a perfect maze using the True Prim's algorithm.
    let primTrue rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols
            let weights = Dictionary<int * int, int>()

            for r in 0 .. rows - 1 do
                for c in 0 .. cols - 1 do
                    weights.[(r, c)] <- rng.Next(1000000)

            let startCell = (rng.Next(rows), rng.Next(cols))
            let visited = HashSet<int * int>([ startCell ])
            let frontier = List<int * int>(unvisitedNeighbors startCell visited rows cols)

            while frontier.Count > 0 do
                let mutable minIdx = 0
                let mutable minWeight = weights.[frontier.[0]]

                for i in 1 .. frontier.Count - 1 do
                    let w = weights.[frontier.[i]]

                    if w < minWeight then
                        minWeight <- w
                        minIdx <- i

                let cell = frontier.[minIdx]
                frontier.RemoveAt(minIdx)

                let (nr, nc) = findVisitedNeighbor cell visited rows cols
                grid <- addPassage grid (fst cell) (snd cell) nr nc
                visited.Add(cell) |> ignore

                for n in unvisitedNeighbors cell visited rows cols do
                    if not (frontier.Contains(n)) then
                        frontier.Add(n)

            grid

    /// Generates a perfect maze using Eller's algorithm.
    let ellers rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols
            let rowState = Dictionary<int, int>()
            let mutable nextSetId = 0

            let assignSets () =
                for c in 0 .. cols - 1 do
                    if not (rowState.ContainsKey(c)) then
                        rowState.[c] <- nextSetId
                        nextSetId <- nextSetId + 1

            let mergeSets oldSet newSet =
                let keys = [ for kv in rowState -> kv.Key ]

                for c in keys do
                    if rowState.[c] = oldSet then
                        rowState.[c] <- newSet

            for r in 0 .. rows - 1 do
                assignSets ()

                if r = rows - 1 then
                    for c in 0 .. cols - 2 do
                        let set1 = rowState.[c]
                        let set2 = rowState.[c + 1]

                        if set1 <> set2 then
                            grid <- addPassage grid r c r (c + 1)
                            mergeSets set2 set1
                else
                    for c in 0 .. cols - 2 do
                        let set1 = rowState.[c]
                        let set2 = rowState.[c + 1]

                        if set1 <> set2 && rng.NextDouble() > 0.5 then
                            grid <- addPassage grid r c r (c + 1)
                            mergeSets set2 set1

                    let setCells = Dictionary<int, List<int>>()

                    for c in 0 .. cols - 1 do
                        let s = rowState.[c]

                        if not (setCells.ContainsKey(s)) then
                            setCells.[s] <- List<int>()

                        setCells.[s].Add(c)

                    let nextRowState = Dictionary<int, int>()

                    for kv in setCells do
                        let colsInSet = kv.Value
                        let count = rng.Next(1, colsInSet.Count + 1)
                        let shuffledCols = [ for c in colsInSet -> c ] |> List.sortBy (fun _ -> rng.Next())
                        let toCarve = shuffledCols |> List.take count

                        for c in toCarve do
                            grid <- addPassage grid r c (r + 1) c
                            nextRowState.[c] <- kv.Key

                    rowState.Clear()

                    for kv in nextRowState do
                        rowState.[kv.Key] <- kv.Value

            grid

    type GrowingTreeStrategy =
        | Last
        | First
        | RandomStrategy
        | Middle
        | Mix of strategy: GrowingTreeStrategy * probability: float

    /// Generates a perfect maze using the Growing Tree algorithm.
    let growingTree rows cols strategy : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createEmptyGrid rows cols
            let startCell = (rng.Next(rows), rng.Next(cols))
            let activeCells = List<int * int>([ startCell ])
            let visited = HashSet<int * int>([ startCell ])

            let rec selectIndex (list: List<int * int>) strat =
                let len = list.Count

                match strat with
                | Last -> 0
                | First -> len - 1
                | RandomStrategy -> rng.Next(len)
                | Middle -> len / 2
                | Mix(s, p) ->
                    if rng.NextDouble() < p then
                        selectIndex list s
                    else
                        rng.Next(len)

            while activeCells.Count > 0 do
                let index = selectIndex activeCells strategy
                let cell = activeCells.[index]
                let neighbors = unvisitedNeighbors cell visited rows cols

                if neighbors.IsEmpty then
                    activeCells.RemoveAt(index)
                else
                    let neighbor = neighbors.[rng.Next(neighbors.Length)]
                    grid <- addPassage grid (fst cell) (snd cell) (fst neighbor) (snd neighbor)
                    visited.Add(neighbor) |> ignore
                    activeCells.Insert(0, neighbor)

            grid

    /// Helper to create a fully connected grid.
    let private createFullGrid rows cols : Grid<unit, int> =
        let mutable grid = createEmptyGrid rows cols

        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                if c < cols - 1 then
                    grid <- addPassage grid r c r (c + 1)

                if r < rows - 1 then
                    grid <- addPassage grid r c (r + 1) c

        grid

    /// Generates a perfect maze using the Recursive Division algorithm.
    let recursiveDivision rows cols : Grid<unit, int> =
        if rows <= 0 || cols <= 0 then
            createEmptyGrid 0 0
        else
            let mutable grid = createFullGrid rows cols

            let rec divide r c h w =
                if h < 2 || w < 2 then
                    ()
                else
                    let horizontal =
                        if h > w then true
                        elif w > h then false
                        else rng.NextDouble() > 0.5

                    if horizontal then
                        let wallRow = r + rng.Next(h - 1)
                        let passageCol = c + rng.Next(w)

                        for colVal in c .. c + w - 1 do
                            if colVal <> passageCol then
                                grid <- removePassage grid wallRow colVal (wallRow + 1) colVal

                        divide r c (wallRow - r + 1) w
                        divide (wallRow + 1) c (r + h - wallRow - 1) w
                    else
                        let wallCol = c + rng.Next(w - 1)
                        let passageRow = r + rng.Next(h)

                        for rowVal in r .. r + h - 1 do
                            if rowVal <> passageRow then
                                grid <- removePassage grid rowVal wallCol rowVal (wallCol + 1)

                        divide r c h (wallCol - c + 1)
                        divide r (wallCol + 1) h (c + w - wallCol - 1)

            divide 0 0 rows cols
            grid
