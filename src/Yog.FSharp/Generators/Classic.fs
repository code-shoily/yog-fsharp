namespace Yog.Generators

open Yog.Model
open System

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

    /// Generates a hypercube graph Q_n.
    let hypercube n kind =
        if n < 0 then
            empty kind
        elif n = 0 then
            createNodes 1 (empty kind)
        else
            let numNodes = int (Math.Pow(2.0, float n))
            let mutable g = createNodes numNodes (empty kind)
            for i in 0 .. numNodes - 1 do
                for bit in 0 .. n - 1 do
                    let j = i ^^^ (1 <<< bit)
                    if kind = Directed || i < j then
                        g <- addEdge i j 1 g
            g

    /// Generates a ladder graph with n rungs.
    let ladder n kind =
        if n <= 0 then
            empty kind
        else
            let mutable g = createNodes (2 * n) (empty kind)
            for i in 0 .. n - 2 do
                g <- addEdge i (i + 1) 1 g
                g <- addEdge (i + n) (i + 1 + n) 1 g
            for i in 0 .. n - 1 do
                g <- addEdge i (i + n) 1 g
            g

    /// Generates a circular ladder (prism) graph with n rungs.
    let circularLadder n kind =
        if n < 3 then
            empty kind
        else
            let mutable g = createNodes (2 * n) (empty kind)
            for i in 0 .. n - 1 do
                g <- addEdge i ((i + 1) % n) 1 g
                g <- addEdge (i + n) (((i + 1) % n) + n) 1 g
                g <- addEdge i (i + n) 1 g
            g

    /// Generates a Möbius ladder graph with n rungs.
    let mobiusLadder n kind =
        if n < 2 then
            empty kind
        else
            let mutable g = createNodes (2 * n) (empty kind)
            for i in 0 .. 2 * n - 1 do
                g <- addEdge i ((i + 1) % (2 * n)) 1 g
            for i in 0 .. n - 1 do
                g <- addEdge i ((i + n) % (2 * n)) 1 g
            g

    /// Generates the friendship graph F_n (n triangles sharing center 0).
    let friendship n kind =
        if n < 1 then
            empty kind
        else
            let total = 2 * n + 1
            let mutable g = createNodes total (empty kind)
            for i in 1 .. n do
                let outer1 = 2 * i - 1
                let outer2 = 2 * i
                g <- addEdge 0 outer1 1 g
                g <- addEdge 0 outer2 1 g
                g <- addEdge outer1 outer2 1 g
            g

    /// Generates the windmill graph W_n^(k).
    let windmill n k kind =
        if n < 1 || k < 2 then
            empty kind
        else
            let total = 1 + n * (k - 1)
            let mutable g = createNodes total (empty kind)
            for i in 0 .. n - 1 do
                let cliqueStart = 1 + i * (k - 1)
                let cliqueEnd = cliqueStart + k - 2
                let clique = 0 :: [ cliqueStart .. cliqueEnd ]
                for idx1 in 0 .. clique.Length - 1 do
                    let startIdx = if kind = Undirected then idx1 + 1 else 0
                    for idx2 in startIdx .. clique.Length - 1 do
                        if idx1 <> idx2 then
                            g <- addEdge clique.[idx1] clique.[idx2] 1 g
            g

    /// Generates the book graph B_n (n triangles sharing spine 0-1).
    let book n kind =
        if n < 1 then
            empty kind
        else
            let total = n + 2
            let mutable g = createNodes total (empty kind)
            g <- addEdge 0 1 1 g
            for i in 2 .. n + 1 do
                g <- addEdge 0 i 1 g
                g <- addEdge 1 i 1 g
            g

    /// Generates the crown graph S_n^0 (K_{n,n} minus a perfect matching).
    let crown n kind =
        if n < 2 then
            empty kind
        else
            let mutable g = createNodes (2 * n) (empty kind)
            for i in 0 .. n - 1 do
                for j in 0 .. n - 1 do
                    if i <> j then
                        g <- addEdge i (n + j) 1 g
            g

    /// Generates the lollipop graph L(m, n).
    let lollipop m n kind =
        if m < 1 then
            empty kind
        else
            let total = m + n
            let mutable g = createNodes total (empty kind)
            for i in 0 .. m - 1 do
                let startJ = if kind = Undirected then i + 1 else 0
                for j in startJ .. m - 1 do
                    if i <> j then
                        g <- addEdge i j 1 g
            for i in 0 .. n - 2 do
                g <- addEdge (m + i) (m + i + 1) 1 g
            if n > 0 then
                g <- addEdge (m - 1) m 1 g
            g

    /// Generates the barbell graph B(m1, m2).
    let barbell m1 m2 kind =
        if m1 < 1 then
            empty kind
        else
            let total = 2 * m1 + m2
            let mutable g = createNodes total (empty kind)
            for i in 0 .. m1 - 1 do
                let startJ = if kind = Undirected then i + 1 else 0
                for j in startJ .. m1 - 1 do
                    if i <> j then
                        g <- addEdge i j 1 g
            let start2 = m1 + m2
            for i in 0 .. m1 - 1 do
                let startJ = if kind = Undirected then i + 1 else 0
                for j in startJ .. m1 - 1 do
                    if i <> j then
                        g <- addEdge (start2 + i) (start2 + j) 1 g
            for i in 0 .. m2 - 2 do
                g <- addEdge (m1 + i) (m1 + i + 1) 1 g
            if m2 > 0 then
                g <- addEdge (m1 - 1) m1 1 g
                g <- addEdge (m1 + m2 - 1) (m1 + m2) 1 g
            else
                g <- addEdge (m1 - 1) m1 1 g
            g

    /// Generates the Turán graph T(n, r).
    let turan n r kind =
        if n <= 0 || r <= 0 then
            empty kind
        else
            let mutable g = createNodes n (empty kind)
            let partitionOf node =
                if r >= n then
                    node
                else
                    let baseSize = n / r
                    let remainder = n % r
                    if node < remainder * (baseSize + 1) then
                        node / (baseSize + 1)
                    else
                        remainder + (node - remainder * (baseSize + 1)) / baseSize
            for i in 0 .. n - 1 do
                for j in i + 1 .. n - 1 do
                    if partitionOf i <> partitionOf j then
                        g <- addEdge i j 1 g
            g

    /// Generates a k-ary tree of given depth and arity.
    let karyTree depth arity kind =
        if depth < 0 || arity < 1 then
            empty kind
        elif depth = 0 then
            createNodes 1 (empty kind)
        elif arity = 1 then
            path (depth + 1) kind
        else
            let totalNodes = int ((Math.Pow(float arity, float (depth + 1)) - 1.0) / float (arity - 1))
            let mutable g = createNodes totalNodes (empty kind)
            let nonLeafCount = int ((Math.Pow(float arity, float depth) - 1.0) / float (arity - 1))
            for i in 0 .. nonLeafCount - 1 do
                for offset in 1 .. arity do
                    let child = arity * i + offset
                    if child < totalNodes then
                        g <- addEdge i child 1 g
            g

    /// Generates a complete k-ary tree with exactly n nodes.
    let completeKary n arity kind =
        if n <= 0 then
            empty kind
        elif n = 1 then
            createNodes 1 (empty kind)
        else
            let mutable g = createNodes n (empty kind)
            for i in 0 .. n - 2 do
                let childStart = arity * i + 1
                let childEnd = min (arity * i + arity) (n - 1)
                for child in childStart .. childEnd do
                    g <- addEdge i child 1 g
            g

    /// Generates a caterpillar tree with exactly n nodes and a central spine.
    let caterpillar n spineLength kind =
        if n <= 0 then
            empty kind
        elif n = 1 then
            createNodes 1 (empty kind)
        else
            let spine = min spineLength n
            let leafCount = n - spine
            let mutable g = createNodes n (empty kind)
            for i in 0 .. spine - 2 do
                g <- addEdge i (i + 1) 1 g
            if leafCount > 0 then
                let leavesPerSpine = leafCount / spine
                let extraLeaves = leafCount % spine
                let mutable nextLeaf = spine
                for spineIdx in 0 .. spine - 1 do
                    let numLeaves = leavesPerSpine + (if spineIdx < extraLeaves then 1 else 0)
                    for i in 0 .. numLeaves - 1 do
                        g <- addEdge spineIdx (nextLeaf + i) 1 g
                    nextLeaf <- nextLeaf + numLeaves
            g

    // =============================================================================
    // Platonic Solids
    // =============================================================================

    /// Generates a tetrahedron graph K_4.
    let tetrahedron kind = complete 4 kind

    /// Generates a cube graph (3D hypercube).
    let cube kind = hypercube 3 kind

    /// Generates an octahedron graph.
    let octahedron kind =
        let mutable g = createNodes 6 (empty kind)
        let edges = [
            (0, 1); (0, 2); (0, 4); (0, 5)
            (1, 2); (1, 3); (1, 5)
            (2, 3); (2, 4)
            (3, 4); (3, 5)
            (4, 5)
        ]
        for (u, v) in edges do
            g <- addEdge u v 1 g
        g

    /// Generates a dodecahedron graph.
    let dodecahedron kind =
        let mutable g = createNodes 20 (empty kind)
        let edges = [
            (0, 1); (1, 2); (2, 3); (3, 4); (4, 0)
            (15, 16); (16, 17); (17, 18); (18, 19); (19, 15)
            (5, 6); (6, 7); (7, 8); (8, 9); (9, 10); (10, 11); (11, 12); (12, 13); (13, 14); (14, 5)
            (0, 5); (1, 6); (2, 7); (3, 8); (4, 9)
            (10, 15); (11, 16); (12, 17); (13, 18); (14, 19)
        ]
        for (u, v) in edges do
            g <- addEdge u v 1 g
        g

    /// Generates an icosahedron graph.
    let icosahedron kind =
        let mutable g = createNodes 12 (empty kind)
        let edges = [
            (0, 1); (0, 2); (0, 3); (0, 4); (0, 5)
            (11, 6); (11, 7); (11, 8); (11, 9); (11, 10)
            (1, 2); (2, 3); (3, 4); (4, 5); (5, 1)
            (6, 7); (7, 8); (8, 9); (9, 10); (10, 6)
            (1, 6); (1, 10); (2, 6); (2, 7); (3, 7); (3, 8); (4, 8); (4, 9); (5, 9); (5, 10)
        ]
        for (u, v) in edges do
            g <- addEdge u v 1 g
        g

    // =============================================================================
    // Special/Famous Graphs
    // =============================================================================

    /// Generates the Sedgewick maze graph.
    let sedgewickMaze kind =
        let mutable g = createNodes 8 (empty kind)
        let edges = [
            (0, 2); (0, 7); (0, 5)
            (1, 7)
            (2, 6)
            (3, 4); (3, 5)
            (4, 5); (4, 7); (4, 6)
        ]
        for (u, v) in edges do
            g <- addEdge u v 1 g
        g

    /// Generates the Tutte graph.
    let tutte kind =
        let mutable g = createNodes 46 (empty kind)
        let adjacency = [
            [1; 2; 3]
            [4; 26]
            [10; 11]
            [18; 19]
            [5; 33]
            [6; 29]
            [7; 27]
            [8; 14]
            [9; 38]
            [10; 37]
            [39]
            [12; 39]
            [13; 35]
            [14; 15]
            [34]
            [16; 22]
            [17; 44]
            [18; 43]
            [45]
            [20; 45]
            [21; 41]
            [22; 23]
            [40]
            [24; 27]
            [25; 32]
            [26; 31]
            [33]
            [28]
            [29; 32]
            [30]
            [31; 33]
            [32]
            []
            []
            [35; 38]
            [36]
            [37; 39]
            [38]
            []
            []
            [41; 44]
            [42]
            [43; 45]
            [44]
            []
            []
        ]
        for u in 0 .. adjacency.Length - 1 do
            for v in adjacency.[u] do
                g <- addEdge u v 1 g
        g
