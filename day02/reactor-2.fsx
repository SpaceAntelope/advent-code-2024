open System.IO
open System

#load "../global.fsx"
#load "./common.fsx"

let isReportSafe(report: int[]) =
    let (slopes,diffs) = 
        report
        |> Array.pairwise
        |> Array.map(fun (x1,x2)-> 
            let diff= Math.Abs(x2-x1)
            x1 > x2, diff >= 1 && diff <= 3)
        |> Array.unzip
    
    let steadySlope = slopes |> Array.pairwise |> Array.forall (fun (x,y) -> x=y)
    let diffsInRange = diffs |> Array.forall ( fun x -> x = true)
    // printfn "%A slope %A  diffs %A" slopes  steadySlope diffsInRange
    steadySlope && diffsInRange
let rec bruteForceReportSafety (skippedIndex : int option) (report: int[])  =
    // printf "--- %A %A" skippedIndex report
    match skippedIndex with
    | Some idx when idx = report.Length -> false
    | _ ->
        let dampenedReport = 
            match skippedIndex with
            | None -> report
            | Some idx -> report |> Array.removeAt idx

        // printfn " --- %A" dampenedReport

        match (isReportSafe dampenedReport), skippedIndex with
        | true, _ -> true
        | false, None -> bruteForceReportSafety (Some 0) report
        | false, Some idx-> bruteForceReportSafety (Some (idx + 1)) report


let isSafe (report: int []) =
    let isUnsafe (lastValidIndex :int ) (index:int) (nextIndex: int) =
        let x1 = report.[index]
        let x2 = report.[nextIndex]
        let signedDiff = x2 - x1
        let diff = Math.Abs(signedDiff)
        let currentSlope = signedDiff > 0
        let previousSlope = 
            if lastValidIndex >= 0 
            then report.[index] > report.[lastValidIndex] 
            else currentSlope

        // printfn "%A: %A %A: %A" index x1 nextIndex x2 

        currentSlope <> previousSlope
        || diff < 1
        || diff > 3

    let mutable currentIndex = 0
    let unsafeIndices = ResizeArray<int>()
    
    while currentIndex < report.Length-1 && unsafeIndices.Count <= 1 do
        // printf "current: %d (%d) " currentIndex report.[currentIndex]
        
        let nextIndex = 
            [currentIndex+1..report.Length-1] 
            |> List.find (fun i -> (not<<unsafeIndices.Contains) i)
        
        // printf "next: %d (%d) " nextIndex report.[nextIndex]

        let previousValidIndex =
            [0..currentIndex-1]
            |> List.tryFindBack (fun i -> (not<<unsafeIndices.Contains) i)
            |> function
            | Some x -> x
            | None -> -1
        
        // printfn "last: %d (%d)" previousValidIndex (if previousValidIndex >= 0 then report.[previousValidIndex] else -1)

        if isUnsafe previousValidIndex currentIndex nextIndex
        then
            if currentIndex = report.Length - 2 && unsafeIndices.Count = 0
            then 
                currentIndex <- currentIndex + 1
            else 
                unsafeIndices.Add currentIndex
                currentIndex <- 
                    if currentIndex = 0 
                    then currentIndex + 1 
                    else currentIndex - 1            
        else 
            currentIndex <- nextIndex
    // printfn "unsafe found: %A %d" (unsafeIndices |> Seq.map (fun x-> sprintf "%d: %A" x report.[x])) unsafeIndices.Count
    unsafeIndices.Count <= 1

"input.example"
|> Common.parseInput
|> Array.map (bruteForceReportSafety None)
// |> Global.shouldBe [| false;true |]
|> Global.shouldBe [| true;false;false;true;true;true;true;false;true |]

let actualData =
    "input.actual"
    |> Common.parseInput

let bruteForceSafe = actualData |> Array.filter ((bruteForceReportSafety None))
let softForceSafe = actualData |> Array.filter (isSafe) 

bruteForceSafe 
|> Array.except softForceSafe 
|> Array.iter (printfn "%A")

actualData    
|> Array.filter (bruteForceReportSafety None)
|> Array.iter (printfn "Safe reports after unpruned search: %A")



// "input.example"
// |> Common.parseInput
// |> Array.map isSafe
// |> Global.shouldBe [| true;false;false;true;true;true;true;false;true |]

// "input.actual"
// |> Common.parseInput
// |> Array.countBy (isSafe>>not)
// // |> Array.randomChoices 15
// |> Array.iter (printfn "%A")

// "input.actual"
// |> Common.parseInput
// |> Array.filter isSafe
// |> Array.length
// |> printfn "Found %d safe reports when Problem Dampener is implemented."
