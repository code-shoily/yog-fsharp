/// Disjoint Set Union (Union-Find) data structure for efficient set operations.
///
/// The disjoint-set data structure maintains a partition of elements into disjoint (non-overlapping)
/// sets. It provides near-constant time operations to add elements, find which set an element
/// belongs to, and merge two sets together.
///
/// ## Key Operations
///
/// | Operation | Function | Complexity |
/// |-----------|----------|------------|
/// | Make Set | `add/2` | O(1) |
/// | Find | `find/2` | O(α(n)) amortized |
/// | Union | `union/3` | O(α(n)) amortized |
///
/// Where α(n) is the [inverse Ackermann function](https://en.wikipedia.org/wiki/Ackermann_function#Inverse),
/// which grows so slowly that it is effectively a small constant (≤ 4) for all practical inputs.
///
/// ## Optimizations
///
/// This implementation uses two key optimizations:
/// - **Path Compression**: Flattens the tree structure during find operations, making future queries faster
/// - **Union by Rank**: Attaches the shorter tree under the taller tree to minimize tree height
///
/// ## Use Cases
///
/// - [Kruskal's MST algorithm](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm) - detecting cycles
/// - Connected components in dynamic graphs
/// - Equivalence relations and partitioning
/// - Percolation theory and network reliability
///
/// ## References
///
/// - [Wikipedia: Disjoint-set data structure](https://en.wikipedia.org/wiki/Disjoint-set_data_structure)
/// - [CP-Algorithms: Disjoint Set Union](https://cp-algorithms.com/data_structures/disjoint_set_union.html)
module Yog.DisjointSet

/// Disjoint Set Union (Union-Find) data structure.
///
/// Efficiently tracks a partition of elements into disjoint sets.
/// Uses path compression and union by rank for near-constant time operations.
///
/// **Time Complexity:** O(α(n)) amortized per operation, where α is the inverse Ackermann function
/// (effectively constant for all practical purposes, less than 5 for 2^65536 elements).
type DisjointSet<'a when 'a: comparison> =
    { Parents: Map<'a, 'a>
      Ranks: Map<'a, int> }

/// Creates a new empty disjoint set structure.
let empty: DisjointSet<'a> =
    { Parents = Map.empty
      Ranks = Map.empty }

/// Adds a new element to the disjoint set.
///
/// The element starts in its own singleton set.
/// If the element already exists, the structure is returned unchanged.
///
/// **Time Complexity:** O(log n) without path compression (amortized O(α(n)) with find)
///
/// ## Example
///
/// ```fsharp
/// let dsu = empty |> add 1 |> add 2 |> add 3
/// // Three separate sets: {1}, {2}, {3}
/// ```
let add (element: 'a) (dsu: DisjointSet<'a>) : DisjointSet<'a> =
    if dsu.Parents |> Map.containsKey element then
        dsu
    else
        { Parents = dsu.Parents |> Map.add element element
          Ranks = dsu.Ranks |> Map.add element 0 }

/// Finds the representative (root) of the set containing the element.
///
/// Uses path compression to flatten the tree structure for future queries.
/// If the element doesn't exist, it's automatically added first.
///
/// Returns a tuple of `(updated_disjoint_set, root)`.
///
/// **Time Complexity:** O(α(n)) amortized (inverse Ackermann)
///
/// ## Example
///
/// ```fsharp
/// let dsu = empty |> add 1 |> add 2 |> union 1 2
/// let (dsu2, root) = find 1 dsu
/// // root is the representative of the set containing 1 and 2
/// ```
let rec find (element: 'a) (dsu: DisjointSet<'a>) : DisjointSet<'a> * 'a =
    match dsu.Parents |> Map.tryFind element with
    // If not found, add it and return as its own root
    | None -> (add element dsu, element)
    | Some parent when parent = element -> (dsu, element)
    | Some parent ->
        let (updatedDsu, root) = find parent dsu
        let newParents = updatedDsu.Parents |> Map.add element root
        ({ updatedDsu with Parents = newParents }, root)

/// Merges the sets containing the two elements.
///
/// Uses union by rank to keep the tree balanced.
/// If the elements are already in the same set, returns unchanged.
///
/// **Time Complexity:** O(α(n)) amortized
///
/// ## Example
///
/// ```fsharp
/// let dsu = empty |> add 1 |> add 2 |> add 3
/// let dsu2 = union 1 2 dsu // {1,2}, {3}
/// let dsu3 = union 2 3 dsu2 // {1,2,3}
/// ```
let union (x: 'a) (y: 'a) (dsu: DisjointSet<'a>) : DisjointSet<'a> =
    let (dsu1, rootX) = find x dsu
    let (dsu2, rootY) = find y dsu1

    if rootX = rootY then
        dsu2
    else
        let rankX =
            dsu2.Ranks
            |> Map.tryFind rootX
            |> Option.defaultValue 0

        let rankY =
            dsu2.Ranks
            |> Map.tryFind rootY
            |> Option.defaultValue 0

        if rankX < rankY then
            { dsu2 with Parents = dsu2.Parents |> Map.add rootX rootY }
        else
            let dsu3 = { dsu2 with Parents = dsu2.Parents |> Map.add rootY rootX }

            if rankX = rankY then
                { dsu3 with Ranks = dsu3.Ranks |> Map.add rootX (rankX + 1) }
            else
                dsu3

/// Creates a disjoint set from a sequence of pairs to union.
///
/// This is a convenience function for building a disjoint set from edge lists
/// or connection pairs. Perfect for graph problems, AoC, and competitive programming.
///
/// **Time Complexity:** O(k × α(n)) where k is the number of pairs
///
/// ## Example
///
/// ```fsharp
/// let dsu = fromPairs [(1, 2); (3, 4); (2, 3)]
/// // Results in: {1,2,3,4} as one set
/// ```
///
/// ## Use Cases
///
/// - Building DSU from edge lists in graph algorithms
/// - Quick setup for connected component problems
/// - Union-Find based cycle detection
let fromPairs (pairs: seq<'a * 'a>) : DisjointSet<'a> =
    pairs
    |> Seq.fold (fun dsu (x, y) -> union x y dsu) empty

/// Checks if two elements are in the same set (connected).
///
/// Returns the updated disjoint set (due to path compression) and a boolean result.
///
/// **Time Complexity:** O(α(n)) amortized
///
/// ## Example
///
/// ```fsharp
/// let dsu = fromPairs [(1, 2); (3, 4)]
/// let (dsu2, result1) = connected 1 2 dsu   // => true
/// let (dsu3, result2) = connected 1 3 dsu2  // => false
/// ```
///
/// ## Use Cases
///
/// - Cycle detection in undirected graphs
/// - Dynamic connectivity checking
/// - Network connectivity verification
let connected (x: 'a) (y: 'a) (dsu: DisjointSet<'a>) : DisjointSet<'a> * bool =
    let dsu1, rootX = find x dsu
    let dsu2, rootY = find y dsu1
    (dsu2, rootX = rootY)

/// Returns the total number of elements in the structure.
///
/// **Time Complexity:** O(1)
///
/// ## Example
///
/// ```fsharp
/// let dsu = empty |> add 1 |> add 2 |> add 3
/// let n = size dsu // => 3
/// ```
let size (dsu: DisjointSet<'a>) : int = dsu.Parents.Count

// Private helper that finds root without path compression (read-only operation)
let rec private findRootReadonly (element: 'a) (dsu: DisjointSet<'a>) : 'a =
    match dsu.Parents |> Map.tryFind element with
    | None -> element
    | Some parent when parent = element -> element
    | Some parent -> findRootReadonly parent dsu

/// Returns the number of disjoint sets.
///
/// Counts the distinct sets by finding the unique roots.
///
/// **Time Complexity:** O(n × α(n)) where n is the number of elements
///
/// ## Example
///
/// ```fsharp
/// let dsu = fromPairs [(1, 2); (3, 4)]
/// let count = countSets dsu  // => 2 (sets: {1,2} and {3,4})
/// ```
///
/// ## Use Cases
///
/// - Counting connected components in a graph
/// - Verifying if all elements are connected (countSets = 1)
let countSets (dsu: DisjointSet<'a>) : int =
    dsu.Parents.Keys
    |> Seq.map (fun element -> findRootReadonly element dsu)
    |> Set.ofSeq
    |> Set.count

/// Returns all disjoint sets as a list of lists.
///
/// Each inner list contains all members of one set. The order of sets and
/// elements within sets is unspecified.
///
/// Note: This operation doesn't perform path compression, so the structure
/// is not modified.
///
/// **Time Complexity:** O(n × α(n))
///
/// ## Example
///
/// ```fsharp
/// let dsu = fromPairs [(1, 2); (3, 4); (5, 6)]
/// let sets = toLists dsu  // => [[1; 2]; [3; 4]; [5; 6]] (order may vary)
/// ```
///
/// ## Use Cases
///
/// - Extracting connected components for further processing
/// - Grouping elements by their connectivity
/// - Visualizing the partition structure
let toLists (dsu: DisjointSet<'a>) : 'a list list =
    dsu.Parents.Keys
    |> Seq.fold
        (fun acc element ->
            let root = findRootReadonly element dsu
            let members = acc |> Map.tryFind root |> Option.defaultValue []
            acc |> Map.add root (element :: members))
        Map.empty
    |> Map.values
    |> List.ofSeq
