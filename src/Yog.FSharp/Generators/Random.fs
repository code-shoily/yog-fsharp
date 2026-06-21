namespace Yog.Generators

open System
open System.Collections.Generic
open Yog.Model

/// Random graph generators for stochastic network models.
module Random =
    open Utils
    let private rng = Random()

    /// Erdős-Rényi G(n, p) model: each edge exists with probability p.
    let erdosRenyiGnp n (p: float) kind =
        let mutable g = createNodes n (empty kind)

        for i in 0 .. n - 1 do
            let startJ = if kind = Undirected then i + 1 else 0

            for j in startJ .. n - 1 do
                if i <> j && rng.NextDouble() < p then
                    g <- addEdge i j 1 g

        g

    /// Erdős-Rényi G(n, m) model: exactly m edges are added uniformly at random.
    let erdosRenyiGnm n m kind =
        let maxEdges = if kind = Undirected then n * (n - 1) / 2 else n * (n - 1)

        let actualM = min m maxEdges
        let mutable g = createNodes n (empty kind)
        let mutable addedEdges = HashSet<NodeId * NodeId>()

        while addedEdges.Count < actualM do
            let i = rng.Next(n)
            let j = rng.Next(n)

            if i <> j then
                let edge = if kind = Undirected && i > j then (j, i) else (i, j)

                if addedEdges.Add(edge) then
                    g <- addEdge (fst edge) (snd edge) 1 g

        g

    /// Barabási-Albert model: creates scale-free networks via preferential attachment.
    let barabasiAlbert n m kind =
        if n < m || m < 1 then
            empty kind
        else
            let m0 = max m 2
            let mutable g = Classic.complete m0 kind

            for newNode in m0 .. n - 1 do
                g <- addNode newNode () g
                let mutable pool = []

                for node in allNodes g do
                    if node <> newNode then
                        let deg =
                            if kind = Undirected then
                                (neighbors node g).Length
                            else
                                (successors node g).Length

                        for _ in 1 .. max deg 1 do
                            pool <- node :: pool

                let targets =
                    pool |> List.sortBy (fun _ -> rng.Next()) |> List.distinct |> List.truncate m

                for t in targets do
                    g <- addEdge newNode t 1 g

            g

    /// Watts-Strogatz model: small-world network (ring lattice + rewiring).
    let wattsStrogatz n k (p: float) kind =
        if n < 3 || k < 2 || k >= n then
            empty kind
        else
            let mutable g = createNodes n (empty kind)
            let halfK = k / 2

            for i in 0 .. n - 1 do
                for offset in 1..halfK do
                    if rng.NextDouble() >= p then
                        g <- addEdge i ((i + offset) % n) 1 g
                    else
                        let mutable target = rng.Next(n)

                        while target = i || (successors i g |> List.exists (fun (id, _) -> id = target)) do
                            target <- rng.Next(n)

                        g <- addEdge i target 1 g

            g

    /// Generates a uniformly random tree on n nodes.
    let randomTree n kind =
        if n < 2 then
            createNodes n (empty kind)
        else
            let mutable g = createNodes n (empty kind)
            let inTree = HashSet<NodeId>([ 0 ])

            for nextNode in 1 .. n - 1 do
                let treeList = inTree |> Seq.toArray
                let parent = treeList.[rng.Next(treeList.Length)]
                g <- addEdge parent nextNode 1 g
                inTree.Add(nextNode) |> ignore

            g
