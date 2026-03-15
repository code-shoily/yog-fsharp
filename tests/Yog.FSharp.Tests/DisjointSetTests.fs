module Yog.FSharp.Tests.DisjointSetTests


open Xunit
open Yog.DisjointSet

// ============================================================================
// BASIC OPERATIONS
// ============================================================================

[<Fact>]
let ``empty creates empty disjoint set`` () =
    let dsu = empty
    Assert.Equal(0, size dsu)
    Assert.Equal(0, countSets dsu)

[<Fact>]
let ``add creates singleton set`` () =
    let dsu = empty |> add 1
    Assert.Equal(1, size dsu)
    Assert.Equal(1, countSets dsu)

[<Fact>]
let ``add multiple elements`` () =
    let dsu = empty |> add 1 |> add 2 |> add 3

    Assert.Equal(3, size dsu)
    Assert.Equal(3, countSets dsu) // Each in its own set

[<Fact>]
let ``add ignores duplicate`` () =
    let dsu = empty |> add 1 |> add 1 // Duplicate

    Assert.Equal(1, size dsu)

// ============================================================================
// FIND OPERATION
// ============================================================================

[<Fact>]
let ``find returns element as its own root`` () =
    let dsu = empty |> add 1
    let (dsu2, root) = find 1 dsu

    Assert.Equal(1, root)

[<Fact>]
let ``find auto-adds missing element`` () =
    let dsu = empty
    let (dsu2, root) = find 1 dsu

    Assert.Equal(1, root)
    Assert.Equal(1, size dsu2)

[<Fact>]
let ``find is idempotent`` () =
    let dsu = empty |> add 1
    let (dsu2, root1) = find 1 dsu
    let (dsu3, root2) = find 1 dsu2

    Assert.Equal(root1, root2)

// ============================================================================
// UNION OPERATION
// ============================================================================

[<Fact>]
let ``union merges two singleton sets`` () =
    let dsu = empty |> add 1 |> add 2 |> union 1 2

    Assert.Equal(2, size dsu)
    Assert.Equal(1, countSets dsu) // Both in same set

[<Fact>]
let ``union is symmetric`` () =
    let dsu1 = empty |> add 1 |> add 2 |> union 1 2

    let dsu2 = empty |> add 1 |> add 2 |> union 2 1

    let (_, root1) = connected 1 2 dsu1
    let (_, root2) = connected 1 2 dsu2

    Assert.True(root1)
    Assert.True(root2)

[<Fact>]
let ``union is idempotent`` () =
    let dsu = empty |> add 1 |> add 2 |> union 1 2 |> union 1 2 // Already unioned

    Assert.Equal(1, countSets dsu)

[<Fact>]
let ``union multiple elements into one set`` () =
    let dsu =
        empty
        |> add 1
        |> add 2
        |> add 3
        |> add 4
        |> union 1 2
        |> union 2 3
        |> union 3 4

    Assert.Equal(4, size dsu)
    Assert.Equal(1, countSets dsu)

[<Fact>]
let ``union creates multiple sets`` () =
    let dsu =
        empty
        |> add 1
        |> add 2
        |> add 3
        |> add 4
        |> union 1 2 // Set 1: {1, 2}
        |> union 3 4 // Set 2: {3, 4}

    Assert.Equal(4, size dsu)
    Assert.Equal(2, countSets dsu)

[<Fact>]
let ``union by rank keeps tree balanced`` () =
    // Create a set with 4 elements in a balanced way
    let dsu =
        empty
        |> add 1
        |> add 2
        |> add 3
        |> add 4
        |> union 1 2 // rank(1) = 1
        |> union 3 4 // rank(3) = 1
        |> union 1 3 // Should attach rank(1) under rank(3) or vice versa

    // Should still be one set
    Assert.Equal(1, countSets dsu)

    // All should be connected
    let (_, c12) = connected 1 2 dsu
    let (_, c34) = connected 3 4 dsu
    let (_, c14) = connected 1 4 dsu

    Assert.True(c12)
    Assert.True(c34)
    Assert.True(c14)

// ============================================================================
// CONNECTED OPERATION
// ============================================================================

[<Fact>]
let ``connected returns true for same element`` () =
    let dsu = empty |> add 1
    let (dsu2, result) = connected 1 1 dsu

    Assert.True(result)

[<Fact>]
let ``connected returns false for disconnected elements`` () =
    let dsu = empty |> add 1 |> add 2

    let (dsu2, result) = connected 1 2 dsu

    Assert.False(result)

[<Fact>]
let ``connected returns true for connected elements`` () =
    let dsu = empty |> add 1 |> add 2 |> union 1 2

    let (dsu2, result) = connected 1 2 dsu

    Assert.True(result)

[<Fact>]
let ``connected is symmetric`` () =
    let dsu = empty |> add 1 |> add 2 |> union 1 2

    let (_, c12) = connected 1 2 dsu
    let (_, c21) = connected 2 1 dsu

    Assert.Equal(c12, c21)

[<Fact>]
let ``connected is transitive`` () =
    let dsu =
        empty
        |> add 1
        |> add 2
        |> add 3
        |> union 1 2
        |> union 2 3

    let (_, c13) = connected 1 3 dsu

    Assert.True(c13)

// ============================================================================
// SIZE AND COUNT SETS
// ============================================================================

[<Fact>]
let ``size counts total elements`` () =
    let dsu = empty |> add 1 |> add 2 |> add 3

    Assert.Equal(3, size dsu)

[<Fact>]
let ``countSets counts disjoint sets`` () =
    let dsu =
        empty
        |> add 1
        |> add 2
        |> add 3
        |> add 4
        |> union 1 2

    // Sets: {1, 2}, {3}, {4}
    Assert.Equal(3, countSets dsu)

[<Fact>]
let ``countSets returns zero for empty`` () = Assert.Equal(0, countSets empty)

// ============================================================================
// FROM PAIRS
// ============================================================================

[<Fact>]
let ``fromPairs empty`` () =
    let dsu = fromPairs []
    Assert.Equal(0, size dsu)

[<Fact>]
let ``fromPairs single pair`` () =
    let dsu = fromPairs [ (1, 2) ]

    Assert.Equal(2, size dsu)
    Assert.Equal(1, countSets dsu)

    let (_, connected) = connected 1 2 dsu
    Assert.True(connected)

[<Fact>]
let ``fromPairs multiple pairs`` () =
    let dsu = fromPairs [ (1, 2); (3, 4); (2, 3) ]

    // Results in: {1, 2, 3, 4} as one set
    Assert.Equal(4, size dsu)
    Assert.Equal(1, countSets dsu)

[<Fact>]
let ``fromPairs chain`` () =
    let dsu =
        fromPairs [ (1, 2)
                    (2, 3)
                    (3, 4)
                    (4, 5) ]

    Assert.Equal(1, countSets dsu)

    let (_, c15) = connected 1 5 dsu
    Assert.True(c15)

[<Fact>]
let ``fromPairs separate chains`` () =
    let dsu =
        fromPairs [ (1, 2)
                    (2, 3)
                    (4, 5)
                    (5, 6) ]

    // Two sets: {1, 2, 3} and {4, 5, 6}
    Assert.Equal(2, countSets dsu)

    let (_, c13) = connected 1 3 dsu
    let (_, c46) = connected 4 6 dsu
    let (_, c14) = connected 1 4 dsu

    Assert.True(c13)
    Assert.True(c46)
    Assert.False(c14)

// ============================================================================
// TO LISTS
// ============================================================================

[<Fact>]
let ``toLists empty`` () =
    let lists = toLists empty
    Assert.Empty(lists)

[<Fact>]
let ``toLists singleton sets`` () =
    let dsu = empty |> add 1 |> add 2 |> add 3

    let lists = toLists dsu

    Assert.Equal(3, lists |> List.length)
    // Each list should have one element
    Assert.True(lists |> List.forall (fun l -> List.length l = 1))

[<Fact>]
let ``toLists merged sets`` () =
    let dsu =
        empty
        |> add 1
        |> add 2
        |> add 3
        |> add 4
        |> union 1 2
        |> union 3 4

    let lists = toLists dsu

    Assert.Equal(2, lists |> List.length)
    // Each list should have 2 elements
    Assert.True(lists |> List.forall (fun l -> List.length l = 2))

[<Fact>]
let ``toLists preserves all elements`` () =
    let dsu = empty |> add 1 |> add 2 |> add 3 |> union 1 2

    let lists = toLists dsu
    let allElements = lists |> List.concat |> Set.ofList

    Assert.Equal<Set<int>>(Set.ofList [ 1; 2; 3 ], allElements)

// ============================================================================
// PATH COMPRESSION
// ============================================================================

[<Fact>]
let ``find performs path compression`` () =
    // Create a long chain: 1-2-3-4-5
    let dsu =
        empty
        |> add 1
        |> add 2
        |> add 3
        |> add 4
        |> add 5
        |> union 1 2
        |> union 2 3
        |> union 3 4
        |> union 4 5

    // Find should compress the path
    let (dsu2, root) = find 1 dsu

    // Root should be same for all
    let (_, root2) = find 2 dsu2
    let (_, root3) = find 3 dsu2
    let (_, root4) = find 4 dsu2
    let (_, root5) = find 5 dsu2

    Assert.Equal(root, root2)
    Assert.Equal(root, root3)
    Assert.Equal(root, root4)
    Assert.Equal(root, root5)

// ============================================================================
// EDGE CASES
// ============================================================================

[<Fact>]
let ``union on same element is no-op`` () =
    let dsu = empty |> add 1 |> union 1 1

    Assert.Equal(1, size dsu)
    Assert.Equal(1, countSets dsu)

[<Fact>]
let ``operations on empty set`` () =
    // Should not throw
    let dsu = empty
    let (dsu2, _) = find 1 dsu // Auto-adds
    let (dsu3, _) = connected 1 2 dsu2
    let dsu4 = union 1 2 dsu3

    Assert.Equal(1, countSets dsu4)

[<Fact>]
let ``string elements`` () =
    let dsu =
        empty
        |> add "alice"
        |> add "bob"
        |> add "charlie"
        |> union "alice" "bob"

    Assert.Equal(2, countSets dsu)

    let (_, connected) = connected "alice" "bob" dsu
    Assert.True(connected)

[<Fact>]
let ``tuple elements`` () =
    let dsu =
        empty
        |> add (0, 0)
        |> add (0, 1)
        |> add (1, 0)
        |> union (0, 0) (0, 1)

    Assert.Equal(2, countSets dsu)

    let (_, connected) = connected (0, 0) (0, 1) dsu
    Assert.True(connected)
