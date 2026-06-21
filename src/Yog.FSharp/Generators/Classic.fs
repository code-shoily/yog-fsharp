namespace Yog.Generators

open Yog.Model

module internal Utils =
    let createNodes n (graph: Graph<unit, int>) =
        let mutable g = graph

        for i in 0 .. n - 1 do
            g <- addNode i () g

        g

/// Deterministic graph generators for classic graph structures.
module Classic =
    open Utils

    /// Generates a complete graph K_n where every node connects to every other.
    let complete n kind =
        let mutable g = createNodes n (empty kind)

        for i in 0 .. n - 1 do
            let startJ = if kind = Undirected then i + 1 else 0

            for j in startJ .. n - 1 do
                if i <> j then
                    g <- addEdge i j 1 g

        g

    /// Generates a cycle graph C_n where nodes form a ring.
    let cycle n kind =
        if n < 3 then
            empty kind
        else
            let mutable g = createNodes n (empty kind)

            for i in 0 .. n - 1 do
                g <- addEdge i ((i + 1) % n) 1 g

            g

    /// Generates a path graph P_n (linear chain).
    let path n kind =
        let mutable g = createNodes n (empty kind)

        for i in 0 .. n - 2 do
            g <- addEdge i (i + 1) 1 g

        g

    /// Generates a star graph S_n where node 0 is the hub.
    let star n kind =
        let mutable g = createNodes n (empty kind)

        for i in 1 .. n - 1 do
            g <- addEdge 0 i 1 g

        g

    /// Generates a wheel graph W_n (cycle with a central hub).
    let wheel n kind =
        if n < 4 then
            empty kind
        else
            let mutable g = star n kind

            for i in 1 .. n - 1 do
                let next = if i = n - 1 then 1 else i + 1
                g <- addEdge i next 1 g

            g

    /// Generates a 2D grid graph (lattice) of rows x cols.
    let grid2D rows cols kind =
        let mutable g = createNodes (rows * cols) (empty kind)

        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                let node = r * cols + c

                if c < cols - 1 then
                    g <- addEdge node (node + 1) 1 g

                if r < rows - 1 then
                    g <- addEdge node (node + cols) 1 g

        g

    /// Generates a complete bipartite graph K_{m,n}.
    let completeBipartite m n kind =
        let total = m + n
        let mutable g = createNodes total (empty kind)

        for left in 0 .. m - 1 do
            for right in m .. total - 1 do
                g <- addEdge left right 1 g

        g

    /// Generates an empty graph with n nodes and no edges.
    let emptyGraph n kind = createNodes n (empty kind)

    /// Generates a complete binary tree of given depth.
    let binaryTree depth kind =
        if depth < 0 then
            empty kind
        else
            let rec pow b e = if e = 0 then 1 else b * pow b (e - 1)
            let n = pow 2 (depth + 1) - 1
            let mutable g = createNodes n (empty kind)

            for i in 0 .. n - 1 do
                let leftChild = 2 * i + 1
                let rightChild = 2 * i + 2

                if leftChild < n then
                    g <- addEdge i leftChild 1 g

                if rightChild < n then
                    g <- addEdge i rightChild 1 g

            g

    /// Generates the Petersen graph.
    let petersenGraph kind =
        let mutable g = createNodes 10 (empty kind)
        // Outer pentagon: 0-1-2-3-4-0
        g <- addEdge 0 1 1 g
        g <- addEdge 1 2 1 g
        g <- addEdge 2 3 1 g
        g <- addEdge 3 4 1 g
        g <- addEdge 4 0 1 g
        // Inner pentagram: 5-7-9-6-8-5
        g <- addEdge 5 7 1 g
        g <- addEdge 7 9 1 g
        g <- addEdge 9 6 1 g
        g <- addEdge 6 8 1 g
        g <- addEdge 8 5 1 g
        // Connect outer to inner (spokes)
        g <- addEdge 0 5 1 g
        g <- addEdge 1 6 1 g
        g <- addEdge 2 7 1 g
        g <- addEdge 3 8 1 g
        g <- addEdge 4 9 1 g
        g
