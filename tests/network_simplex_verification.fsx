// Network Simplex Verification Tests
// Ported from Gleam version to ensure correctness

#I "../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Model
open Yog.Flow.NetworkSimplex

printfn "=== Network Simplex Verification Tests ==="
printfn ""

// =============================================================================
// TEST 1: Simple Network Simplex (from Gleam)
// =============================================================================

printfn "Test 1: Simple 3-node network"
printfn "-------------------------------"

let test1Graph =
    empty Directed
    |> addNode 1 10 // Supply (positive)
    |> addNode 2 0
    |> addNode 3 (-10) // Demand (negative)
    |> addEdge 1 2 (5, 2) // (capacity, cost)
    |> addEdge 1 3 (10, 5)
    |> addEdge 2 3 (5, 1)

let getDemand (d: int) = d
let getCapacity (e: int * int) = fst e
let getCost (e: int * int) = snd e

let result1 = minCostFlow test1Graph getDemand getCapacity getCost

match result1 with
| Ok res ->
    printfn "✓ Solution found!"
    printfn "  Total Cost: %d (expected: 40)" res.Cost
    printfn "  Flow edges:"

    for edge in res.Flow do
        printfn "    %d -> %d: flow %d" edge.Source edge.Target edge.Flow

    // Verify expected cost
    if res.Cost = 40 then
        printfn "✓ TEST 1 PASSED: Cost matches expected value (40)"
    else
        printfn "✗ TEST 1 FAILED: Expected cost 40, got %d" res.Cost

| Error err -> printfn "✗ TEST 1 FAILED: Error - %A" err

printfn ""

// =============================================================================
// TEST 2: Transportation Problem (from Gleam)
// =============================================================================

printfn "Test 2: Transportation problem (2 factories, 3 stores)"
printfn "-------------------------------------------------------"

type NodeData = { Demand: int }
type EdgeData = { Capacity: int; Cost: int }

let test2Graph =
    empty Directed
    // Factories (supply)
    |> addNode 1 { Demand = 50 } // Factory 1
    |> addNode 2 { Demand = 50 } // Factory 2
    // Stores (demand)
    |> addNode 3 { Demand = -30 } // Store 1
    |> addNode 4 { Demand = -40 } // Store 2
    |> addNode 5 { Demand = -30 } // Store 3
    // Factory 1 routes
    |> addEdge 1 3 { Capacity = 100; Cost = 10 } // Cheap to S1
    |> addEdge 1 4 { Capacity = 100; Cost = 20 } // Okay to S2
    |> addEdge 1 5 { Capacity = 100; Cost = 50 } // Expensive to S3
    // Factory 2 routes
    |> addEdge 2 3 { Capacity = 100; Cost = 60 } // Expensive to S1
    |> addEdge 2 4 { Capacity = 100; Cost = 15 } // Cheap to S2
    |> addEdge 2 5 { Capacity = 100; Cost = 10 } // Cheap to S3

let demandOf (n: NodeData) = n.Demand
let capacityOf (e: EdgeData) = e.Capacity
let costOf (e: EdgeData) = e.Cost

let result2 = minCostFlow test2Graph demandOf capacityOf costOf

match result2 with
| Ok res ->
    printfn "✓ Solution found!"
    printfn "  Total Cost: %d (expected: 1300)" res.Cost

    // Build flow lookup
    let flowMap =
        res.Flow |> List.map (fun e -> (e.Source, e.Target), e.Flow) |> Map.ofList

    let getFlow src tgt =
        Map.tryFind (src, tgt) flowMap |> Option.defaultValue 0

    printfn "  Flow routes:"
    printfn "    Factory 1 -> Store 1: %d (expected: 30)" (getFlow 1 3)
    printfn "    Factory 1 -> Store 2: %d (expected: 20)" (getFlow 1 4)
    printfn "    Factory 1 -> Store 3: %d (expected: 0)" (getFlow 1 5)
    printfn "    Factory 2 -> Store 1: %d (expected: 0)" (getFlow 2 3)
    printfn "    Factory 2 -> Store 2: %d (expected: 20)" (getFlow 2 4)
    printfn "    Factory 2 -> Store 3: %d (expected: 30)" (getFlow 2 5)

    // Verify expected cost
    let costCorrect = res.Cost = 1300

    // Verify expected flows
    let flowsCorrect =
        (getFlow 1 3) = 30
        && (getFlow 1 4) = 20
        && (getFlow 1 5) = 0
        && (getFlow 2 3) = 0
        && (getFlow 2 4) = 20
        && (getFlow 2 5) = 30

    if costCorrect && flowsCorrect then
        printfn "✓ TEST 2 PASSED: Cost and all flows match expected values"
    elif costCorrect then
        printfn "⚠ TEST 2 PARTIAL: Cost correct but some flows differ"
    else
        printfn "✗ TEST 2 FAILED: Expected cost 1300, got %d" res.Cost

| Error err -> printfn "✗ TEST 2 FAILED: Error - %A" err

printfn ""

// =============================================================================
// TEST 3: Unbalanced Demands (should error)
// =============================================================================

printfn "Test 3: Unbalanced demands detection"
printfn "--------------------------------------"

let test3Graph =
    empty Directed
    |> addNode 1 10 // Supply
    |> addNode 2 (-5) // Demand doesn't match!
    |> addEdge 1 2 (20, 1)

let result3 = minCostFlow test3Graph getDemand getCapacity getCost

match result3 with
| Error UnbalancedDemands -> printfn "✓ TEST 3 PASSED: Correctly detected unbalanced demands"
| Ok _ -> printfn "✗ TEST 3 FAILED: Should have detected unbalanced demands"
| Error err -> printfn "✗ TEST 3 FAILED: Wrong error type - %A" err

printfn ""

// =============================================================================
// TEST 4: Minimal Example
// =============================================================================

printfn "Test 4: Minimal 2-node example"
printfn "--------------------------------"

let test4Graph =
    empty Directed
    |> addNode 0 (-10) // Demand
    |> addNode 1 10 // Supply
    |> addEdge 1 0 (20, 5)

let result4 = minCostFlow test4Graph getDemand getCapacity getCost

match result4 with
| Ok res ->
    printfn "✓ Solution found!"
    printfn "  Total Cost: %d (expected: 50)" res.Cost

    if res.Cost = 50 then
        printfn "✓ TEST 4 PASSED: Cost matches expected value (50)"
    else
        printfn "✗ TEST 4 FAILED: Expected cost 50, got %d" res.Cost

| Error err -> printfn "✗ TEST 4 FAILED: Error - %A" err

printfn ""
printfn "=== All Tests Complete ==="
