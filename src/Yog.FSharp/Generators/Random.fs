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

    /// Generates a random d-regular graph.
    let randomRegular n d kind =
        if n <= 0 || d < 0 || d >= n || (n * d % 2 <> 0) then
            empty kind
        elif d = 0 then
            createNodes n (empty kind)
        else
            let rec attempt retries =
                if retries <= 0 then
                    empty kind
                else
                    // Create stubs: each node appears d times
                    let stubs = [| for i in 0 .. n - 1 do for _ in 1 .. d -> i |]
                    // Fisher-Yates shuffle
                    for idx = stubs.Length - 1 downto 1 do
                        let swapIdx = rng.Next(idx + 1)
                        let temp = stubs.[idx]
                        stubs.[idx] <- stubs.[swapIdx]
                        stubs.[swapIdx] <- temp
                    let pairs = [ for i in 0 .. 2 .. stubs.Length - 2 -> (stubs.[i], stubs.[i+1]) ]
                    let mutable hasSelfLoop = false
                    for (a, b) in pairs do
                        if a = b then hasSelfLoop <- true
                    if hasSelfLoop then
                        attempt (retries - 1)
                    else
                        let edges = 
                            pairs 
                            |> List.map (fun (a, b) -> if kind = Undirected then (min a b, max a b) else (a, b))
                            |> List.distinct
                        if edges.Length = pairs.Length then
                            let mutable g = createNodes n (empty kind)
                            for (u, v) in edges do
                                g <- addEdge u v 1 g
                            g
                        else
                            attempt (retries - 1)
            attempt 100

    /// Stochastic Block Model (SBM).
    let sbm n k (pIn: float) (pOut: float) kind =
        let baseSize = n / k
        let remainder = n % k
        let sizes = [ for i in 0 .. k - 1 -> baseSize + (if i < remainder then 1 else 0) ]
        let communities = Dictionary<NodeId, int>()
        let mutable nodeIdx = 0
        for commIdx in 0 .. k - 1 do
            let size = sizes.[commIdx]
            for _ in 1 .. size do
                communities.[nodeIdx] <- commIdx
                nodeIdx <- nodeIdx + 1
        let mutable g = createNodes n (empty kind)
        for i in 0 .. n - 1 do
            let startJ = if kind = Undirected then i + 1 else 0
            for j in startJ .. n - 1 do
                if i <> j then
                    let p = if communities.[i] = communities.[j] then pIn else pOut
                    if rng.NextDouble() <= p then
                        g <- addEdge i j 1 g
        g

    /// Generates a random graph with the given degree sequence.
    let configurationModel (degrees: int list) allowSelfloops allowMultiedges maxRetries =
        let sum = List.sum degrees
        if degrees.IsEmpty || degrees |> List.exists (fun d -> d < 0) || sum % 2 <> 0 then
            Error "Invalid degree sequence: degrees must be non-negative and sum to an even number."
        else
            let rec attempt retries =
                if retries <= 0 then
                    Error "Max retries exceeded while trying to pair stubs."
                else
                    let stubsList = List.init degrees.Length (fun idx -> List.replicate degrees.[idx] idx) |> List.concat |> List.toArray
                    for idx = stubsList.Length - 1 downto 1 do
                        let swapIdx = rng.Next(idx + 1)
                        let temp = stubsList.[idx]
                        stubsList.[idx] <- stubsList.[swapIdx]
                        stubsList.[swapIdx] <- temp
                    let pairs = [ for i in 0 .. 2 .. stubsList.Length - 2 -> (stubsList.[i], stubsList.[i+1]) ]
                    let hasSelfloop = pairs |> List.exists (fun (a, b) -> a = b)
                    let edges = 
                        pairs 
                        |> List.map (fun (a, b) -> (min a b, max a b))
                        |> List.filter (fun (a, b) -> a <> b || allowSelfloops)
                    let edgeSet = Set.ofList edges
                    let hasMultiedges = edgeSet.Count < edges.Length
                    let valid = (not hasSelfloop || allowSelfloops) && (not hasMultiedges || allowMultiedges)
                    if valid then
                        let n = degrees.Length
                        let mutable g = createNodes n (empty Undirected)
                        for (u, v) in edgeSet do
                            g <- addEdge u v 1 g
                        Ok g
                    else
                        attempt (retries - 1)
            attempt maxRetries

    /// R-MAT random graph generator.
    let rmat nNodes nEdges a b c d kind =
        if nNodes <= 0 || nEdges < 0 then
            empty kind
        else
            let total = a + b + c + d
            let normA = a / total
            let normB = b / total
            let normC = c / total
            let normD = d / total
            let rec chooseEdgeRecursive uLo uHi vLo vHi =
                if uLo = uHi && vLo = vHi then
                    (uLo, vLo)
                else
                    let uMid = (uLo + uHi) / 2
                    let vMid = (vLo + vHi) / 2
                    let r = rng.NextDouble()
                    if r < normA then
                        chooseEdgeRecursive uLo uMid vLo vMid
                    elif r < normA + normB then
                        chooseEdgeRecursive uLo uMid (vMid + 1) vHi
                    elif r < normA + normB + normC then
                        chooseEdgeRecursive (uMid + 1) uHi vLo vMid
                    else
                        chooseEdgeRecursive (uMid + 1) uHi (vMid + 1) vHi
            let edges = [ for _ in 1 .. nEdges -> chooseEdgeRecursive 0 (nNodes - 1) 0 (nNodes - 1) ]
            let edgeSet = 
                if kind = Directed then
                    Set.ofList edges
                else
                    edges |> List.map (fun (u, v) -> (min u v, max u v)) |> Set.ofList
            let mutable g = createNodes nNodes (empty kind)
            for (u, v) in edgeSet do
                if u <> v then
                    g <- addEdge u v 1 g
            g

    /// Random geometric graph.
    let geometric n (radius: float) metric periodic =
        let mutable g = createNodes n (empty Undirected)
        let positions = Dictionary<NodeId, float * float>()
        for i in 0 .. n - 1 do
            positions.[i] <- (rng.NextDouble(), rng.NextDouble())
        let periodicDiff c1 c2 =
            let d = abs (c1 - c2)
            min d (1.0 - d)
        let isWithin (x1, y1) (x2, y2) =
            let dx, dy =
                if periodic then
                    (periodicDiff x1 x2, periodicDiff y1 y2)
                else
                    (x1 - x2, y1 - y2)
            match metric with
            | "euclidean" ->
                let d = sqrt (dx * dx + dy * dy)
                d <= radius
            | "manhattan" ->
                let d = dx + dy
                d <= radius
            | _ ->
                let d = sqrt (dx * dx + dy * dy)
                d <= radius
        for i in 0 .. n - 1 do
            for j in i + 1 .. n - 1 do
                if isWithin positions.[i] positions.[j] then
                    g <- addEdge i j 1 g
        g

    /// Random geometric graph in d dimensions.
    let geometricNd n dimensions (radius: float) =
        let mutable g = createNodes n (empty Undirected)
        let positions = Dictionary<NodeId, float list>()
        for i in 0 .. n - 1 do
            positions.[i] <- [ for _ in 1 .. dimensions -> rng.NextDouble() ]
        let radiusSq = radius * radius
        let distSq (p1: float list) (p2: float list) =
            List.fold2 (fun acc c1 c2 -> acc + (c1 - c2) * (c1 - c2)) 0.0 p1 p2
        for i in 0 .. n - 1 do
            for j in i + 1 .. n - 1 do
                if distSq positions.[i] positions.[j] <= radiusSq then
                    g <- addEdge i j 1 g
        g

    /// Waxman random graph model.
    let waxman n alpha beta =
        let mutable g = createNodes n (empty Undirected)
        let positions = Dictionary<NodeId, float * float>()
        for i in 0 .. n - 1 do
            positions.[i] <- (rng.NextDouble(), rng.NextDouble())
        let maxDist = sqrt 2.0
        let dist (x1, y1) (x2, y2) =
            let dx = x1 - x2
            let dy = y1 - y2
            sqrt (dx * dx + dy * dy)
        for i in 0 .. n - 1 do
            for j in i + 1 .. n - 1 do
                let d = dist positions.[i] positions.[j]
                let prob = beta * exp (-d / (alpha * maxDist))
                if rng.NextDouble() <= prob then
                    g <- addEdge i j 1 g
        g

    /// Returns the SBM graph along with community assignments.
    let sbmWithLabels n k (pIn: float) (pOut: float) kind communitySizesOpt =
        let communitySizes = 
            match communitySizesOpt with
            | Some sizes -> sizes
            | None -> 
                let baseSize = n / k
                let remainder = n % k
                [ for i in 0 .. k - 1 -> baseSize + (if i < remainder then 1 else 0) ]
        
        let communities = Dictionary<NodeId, int>()
        let mutable nodeIdx = 0
        for commIdx in 0 .. k - 1 do
            let size = communitySizes.[commIdx]
            for _ in 1 .. size do
                communities.[nodeIdx] <- commIdx
                nodeIdx <- nodeIdx + 1
        
        let mutable g = createNodes n (empty kind)
        for i in 0 .. n - 1 do
            let startJ = if kind = Undirected then i + 1 else 0
            for j in startJ .. n - 1 do
                if i <> j then
                    let p = if communities.[i] = communities.[j] then pIn else pOut
                    if rng.NextDouble() <= p then
                        g <- addEdge i j 1 g
        (g, Map [ for kv in communities -> kv.Key, kv.Value ])

    /// Degree-corrected stochastic block model degree distribution options.
    type DegreeDistribution =
        | PowerLaw of gamma: float
        | Poisson
        | Custom of thetas: float[]

    /// Degree-Corrected Stochastic Block Model (DCSBM).
    let dcsbm n k (pIn: float) (pOut: float) degreeDist communitySizesOpt =
        let communitySizes =
            match communitySizesOpt with
            | Some sizes -> sizes
            | None ->
                let baseSize = n / k
                let remainder = n % k
                [ for i in 0 .. k - 1 -> baseSize + (if i < remainder then 1 else 0) ]
        
        let communities = Dictionary<NodeId, int>()
        let mutable nodeIdx = 0
        for commIdx in 0 .. k - 1 do
            let size = communitySizes.[commIdx]
            for _ in 1 .. size do
                communities.[nodeIdx] <- commIdx
                nodeIdx <- nodeIdx + 1
        
        let rawThetas =
            match degreeDist with
            | PowerLaw gamma ->
                [| for i in 1 .. n -> Math.Pow(float i, -gamma) |]
            | Poisson ->
                [| for _ in 1 .. n -> 0.5 + rng.NextDouble() |]
            | Custom thetas ->
                if thetas.Length = n then thetas else [| for _ in 1 .. n -> 1.0 |]

        let thetas = Array.copy rawThetas
        for idx = thetas.Length - 1 downto 1 do
            let swapIdx = rng.Next(idx + 1)
            let temp = thetas.[idx]
            thetas.[idx] <- thetas.[swapIdx]
            thetas.[swapIdx] <- temp

        let sumThetas = Array.sum thetas
        let mean = sumThetas / float n
        let normalizedThetas =
            if mean > 0.0 then thetas |> Array.map (fun t -> t / mean)
            else thetas

        let mutable g = createNodes n (empty Undirected)
        for u in 0 .. n - 1 do
            for v in u + 1 .. n - 1 do
                let pBase = if communities.[u] = communities.[v] then pIn else pOut
                let p = min 1.0 (normalizedThetas.[u] * normalizedThetas.[v] * pBase)
                if rng.NextDouble() <= p then
                    g <- addEdge u v 1 g
        g

    /// Hierarchical Stochastic Block Model (HSBM) with nested communities.
    let hsbm n levels branching pIn pOut pMidOpt probsOpt =
        if n <= 0 || levels < 1 || branching < 2 then
            empty Undirected
        else
            let leafBlocks = int (Math.Pow(float branching, float levels))
            let baseLeafSize = n / leafBlocks
            if baseLeafSize >= 1 then
                let probs =
                    match probsOpt with
                    | Some explicitProbs -> explicitProbs
                    | None ->
                        match pMidOpt with
                        | Some pMid when levels = 2 ->
                            [| pIn; pMid; pOut |]
                        | _ ->
                            [| for l in 0 .. levels ->
                                pIn + (pOut - pIn) * float l / float levels |]
                
                let powers = [| for l in 0 .. levels -> int (Math.Pow(float branching, float l)) |]
                let bu u = u / baseLeafSize
                let findLcaLevel u v =
                    let b_u = bu u
                    let b_v = bu v
                    if b_u = b_v then 0
                    else
                        let mutable foundLevel = levels
                        let mutable level = 1
                        let mutable found = false
                        while level < powers.Length && not found do
                            if b_u / powers.[level] = b_v / powers.[level] then
                                foundLevel <- level
                                found <- true
                            level <- level + 1
                        foundLevel

                let mutable g = createNodes n (empty Undirected)
                for u in 0 .. n - 1 do
                    for v in u + 1 .. n - 1 do
                        let lcaLevel = findLcaLevel u v
                        let p = if lcaLevel < probs.Length then probs.[lcaLevel] else 0.0
                        if rng.NextDouble() <= p then
                            g <- addEdge u v 1 g
                g
            else
                empty Undirected

    /// Generates a random graph matching the degree sequence of a given graph.
    let randomizeDegreeSequence (graph: Graph<'NodeData, 'EdgeData>) allowSelfloops allowMultiedges maxRetries =
        let nodes = allNodes graph
        let n = nodes.Length
        if n = 0 then
            Ok (empty Undirected)
        else
            let degrees = nodes |> List.map (fun node -> (neighbors node graph).Length)
            match configurationModel degrees allowSelfloops allowMultiedges maxRetries with
            | Ok intGraph ->
                let idMapping = nodes |> List.mapi (fun idx orig -> (idx, orig)) |> Map.ofList
                let mutable g = empty Undirected
                for origNode in nodes do
                    let data = 
                        match Map.tryFind origNode graph.Nodes with
                        | Some d -> d
                        | None -> failwith "Node not found"
                    g <- addNode origNode data g
                
                for (u, v, _) in allEdges intGraph do
                    let origU = idMapping.[u]
                    let origV = idMapping.[v]
                    g <- addEdge origU origV 1 g
                Ok g
            | Error err -> Error err

    /// Generates a random graph with power-law degree distribution using the configuration model.
    let powerLawGraph n gamma kMin kMaxOpt allowSelfloops allowMultiedges maxRetries =
        if n <= 0 then
            Error "Number of nodes must be positive."
        elif gamma <= 2.0 then
            Error "Gamma must be greater than 2.0."
        else
            let kMax = 
                match kMaxOpt with
                | Some km -> min km (n - 1)
                | None -> n - 1
            if kMin < 0 || kMax < kMin then
                Error "Invalid degree bounds."
            else
                let zeta = Math.Pow(float kMin, 1.0 - gamma) - Math.Pow(float (kMax + 1), 1.0 - gamma)
                let degrees = [|
                    for _ in 1 .. n do
                        let u = rng.NextDouble()
                        let k = Math.Pow(Math.Pow(float kMin, 1.0 - gamma) - u * zeta, 1.0 / (1.0 - gamma))
                        let roundK = int (Math.Round(k))
                        yield max kMin (min kMax roundK)
                |]
                
                let sum = Array.sum degrees
                if sum % 2 <> 0 then
                    let idx = rng.Next(n)
                    let current = degrees.[idx]
                    let newValue =
                        if current < kMax then current + 1
                        elif current > kMin then current - 1
                        else current
                    degrees.[idx] <- newValue
                
                configurationModel (Array.toList degrees) allowSelfloops allowMultiedges maxRetries

    /// Generates a Kronecker graph using recursive expansion via R-MAT.
    let kronecker k (initiator: float[,]) nEdgesOpt kind =
        let n = int (Math.Pow(2.0, float k))
        let a = initiator.[0, 0]
        let b = initiator.[0, 1]
        let c = initiator.[1, 0]
        let d = initiator.[1, 1]
        let totalProb = a + b + c + d
        
        let nEdges =
            match nEdgesOpt with
            | Some m -> m
            | None ->
                let factor = if kind = Directed then 1.0 else 0.5
                int (Math.Round(float (n * n) * totalProb / 4.0 * factor))
        
        let (na, nb, nc, nd) =
            if kind = Directed then
                (a, b, c, d)
            else
                let avgBC = (b + c) / 2.0
                (a, avgBC, avgBC, d)
        
        rmat n nEdges na nb nc nd kind

    /// Generates a Kronecker graph with a general n x n initiator matrix.
    let kroneckerGeneral k (initiator: float[,]) nEdgesOpt kind =
        if k < 0 then
            empty kind
        elif k = 0 then
            createNodes 1 (empty kind)
        else
            let n = Array2D.length1 initiator
            let numNodes = int (Math.Pow(float n, float k))
            
            let flatInit = [|
                for r in 0 .. n - 1 do
                    for c in 0 .. n - 1 do
                        yield initiator.[r, c]
            |]
            let total = Array.sum flatInit
            let probs = flatInit |> Array.map (fun x -> x / total)
            
            let cumProbs = Array.zeroCreate probs.Length
            let mutable acc = 0.0
            for i in 0 .. probs.Length - 1 do
                acc <- acc + probs.[i]
                cumProbs.[i] <- acc
            
            let nEdges =
                match nEdgesOpt with
                | Some m -> m
                | None -> max 1 (numNodes * 2)

            let findQuadrant r =
                let idx = Array.tryFindIndex (fun cp -> cp >= r) cumProbs
                match idx with
                | Some i -> i
                | None -> cumProbs.Length - 1

            let rec chooseEdge level uAcc vAcc =
                if level = 0 then
                    (uAcc, vAcc)
                else
                    let r = rng.NextDouble()
                    let entry = findQuadrant r
                    let uDigit = entry / n
                    let vDigit = entry % n
                    chooseEdge (level - 1) (uAcc * n + uDigit) (vAcc * n + vDigit)

            let edges = [ for _ in 1 .. nEdges -> chooseEdge k 0 0 ]
            let edgeSet =
                if kind = Directed then
                    Set.ofList edges
                else
                    edges |> List.map (fun (u, v) -> (min u v, max u v)) |> Set.ofList
            
            let mutable g = createNodes numNodes (empty kind)
            for (u, v) in edgeSet do
                if u <> v && u >= 0 && u < numNodes && v >= 0 && v < numNodes then
                    g <- addEdge u v 1 g
            g
