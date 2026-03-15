(**
# Medical Residency Matching (Gale-Shapley)
This example demonstrates the Gale-Shapley algorithm for finding a stable matching between two groups.
This algorithm is used in the National Resident Matching Program (NRMP) to match medical students to residency programs.

## Problem
Match 5 medical residents to 5 hospitals based on their mutual preferences such that the matching is stable (no resident and hospital would both prefer to be matched with each other over their current matches).
*)

#I "../../src/Yog.FSharp/bin/Debug/net10.0"
#r "Yog.FSharp.dll"

open Yog.Properties.Bipartite

// 1. Resident preferences (most preferred first)
let residents =
    [ 1, [101; 102; 103; 104; 105]  // Dr. Anderson
      2, [102; 105; 101; 103; 104]  // Dr. Brown
      3, [103; 101; 104; 102; 105]  // Dr. Chen
      4, [104; 103; 105; 102; 101]  // Dr. Davis
      5, [105; 104; 103; 102; 101] ] // Dr. Evans
    |> Map.ofList

// 2. Hospital preferences (most preferred first)
let hospitals =
    [ 101, [3; 1; 2; 4; 5]          // City General
      102, [1; 2; 5; 3; 4]          // Metro Hospital
      103, [3; 4; 1; 2; 5]          // University Med
      104, [4; 5; 3; 2; 1]          // Regional Care
      105, [5; 2; 4; 3; 1] ]        // Coastal Medical
    |> Map.ofList

printfn "=== Medical Residency Matching ==="

// 3. Run Gale-Shapley algorithm
let marriage = stableMarriage residents hospitals

// 4. Display Results
let residentNames =
    [ 1, "Anderson"; 2, "Brown"; 3, "Chen"; 4, "Davis"; 5, "Evans" ] |> Map.ofList
let hospitalNames =
    [ 101, "City General"; 102, "Metro Hospital"; 103, "University Med"
      104, "Regional Care"; 105, "Coastal Medical" ] |> Map.ofList

residents.Keys
|> Seq.iter (fun rid ->
    match getPartner rid marriage with
    | Some hid ->
        printfn "Dr. %s (#%d) matched to %s (#%d)"
            residentNames.[rid] rid hospitalNames.[hid] hid
    | None ->
        printfn "Dr. %s (#%d) was not matched" residentNames.[rid] rid)

(**
## Output

```
=== Medical Residency Matching ===
Dr. Anderson (#1) matched to City General (#101)
Dr. Brown (#2) matched to Metro Hospital (#102)
Dr. Chen (#3) matched to University Med (#103)
Dr. Davis (#4) matched to Regional Care (#104)
Dr. Evans (#5) matched to Coastal Medical (#105)
```
*)
