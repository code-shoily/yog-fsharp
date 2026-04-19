/// Optimized distance matrix computation for subsets of nodes.
///
/// This module provides an auto-selecting algorithm for computing shortest path
/// distances between specified "points of interest" (POIs) in a graph. It chooses
/// between Floyd-Warshall and multiple Dijkstra runs based on POI density.
///
/// ## Algorithm Selection
///
/// | Algorithm | When Selected | Complexity |
/// |-----------|---------------|------------|
/// | Floyd-Warshall | Many POIs (> V/3) | O(V³) then filter |
/// | Dijkstra × P | Few POIs (≤ V/3) | O(P × (V + E) log V) |
///
/// ## Heuristic
///
/// The crossover point is P > V/3 where P is the number of points of interest:
/// - **Dense POIs**: Floyd-Warshall computes all-pairs once, then filter
/// - **Sparse POIs**: Run Dijkstra from each POI individually
///
/// This heuristic balances the O(V³) Floyd-Warshall against the O(P(V+E) log V)
/// cost of multiple Dijkstra runs.
///
/// ## Use Cases
///
/// - Game AI: Pathfinding between key locations (not all nodes)
/// - Logistics: Distance matrix for delivery stops
/// - Facility location: Distances between candidate sites
/// - Network analysis: Selected node pairwise distances
module Yog.Pathfinding.DistanceMatrix

open Yog.Model
open Yog.Pathfinding

/// Computes shortest distances between all pairs of points of interest.
///
/// Automatically chooses between Floyd-Warshall and multiple Dijkstra runs
/// based on the density of POIs relative to graph size.
///
/// Time Complexity: O(V³) or O(P * (V + E) log V)
let distanceMatrix
    (zero: 'e)
    (add: 'e -> 'e -> 'e)
    (compare: 'e -> 'e -> int)
    (pois: NodeId list)
    (graph: Graph<'n, 'e>)
    : Result<Map<NodeId * NodeId, 'e>, unit> =
    let numNodes = nodeCount graph
    let numPois = pois.Length
    let poiSet = Set.ofList pois

    // Choose algorithm based on POI density
    // Floyd-Warshall: O(V³)
    // Multiple Dijkstra: O(P * (V + E) log V) where P = numPois
    // Crossover heuristic: P > V/3
    if numPois * 3 > numNodes then
        match FloydWarshall.floydWarshall zero add compare graph with
        | Error() -> Error()
        | Ok allDistances ->
            // Filter to only POI-to-POI distances
            let poiDistances =
                allDistances
                |> Map.filter (fun (src, dst) _ -> Set.contains src poiSet && Set.contains dst poiSet)

            Ok poiDistances
    else
        // Sparse POIs: Run singleSourceDistances from each POI
        let result =
            pois
            |> List.fold
                (fun acc source ->
                    let distances = Dijkstra.singleSourceDistances zero add compare source graph

                    // Add only POI-to-POI distances
                    pois
                    |> List.fold
                        (fun innerAcc target ->
                            match distances |> Map.tryFind target with
                            | Some dist -> innerAcc |> Map.add (source, target) dist
                            | None -> innerAcc)
                        acc)
                Map.empty

        Ok result

// -----------------------------------------------------------------------------
// CONVENIENCE WRAPPERS FOR COMMON TYPES
// -----------------------------------------------------------------------------

/// Computes POI distance matrix using integer weights.
let distanceMatrixInt (pois: NodeId list) (graph: Graph<'n, int>) : Result<Map<NodeId * NodeId, int>, unit> =
    distanceMatrix 0 (+) compare pois graph

/// Computes POI distance matrix using float weights.
let distanceMatrixFloat (pois: NodeId list) (graph: Graph<'n, float>) : Result<Map<NodeId * NodeId, float>, unit> =
    distanceMatrix 0.0 (+) compare pois graph
