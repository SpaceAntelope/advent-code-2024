open System.IO
open System

#load "../global.fsx"
#load "./common.fsx"

let isSafe (report: int[]) =
    let rising = (report[0] - report[1]) < 0
    let initialErrorCount = 0
    report 
    |> Array.pairwise
    |> Array.fold (fun errorCount (x1,x2) ->
        if errorCount > 0
        then errorCount + 1
        else
            let diff = Math.Abs(x1-x2)
            let isUnsafe = 
                ((rising && (x2 - x1) < 0) || (not rising && (x2-x1) > 0 )) 
                || diff < 1 || diff > 3
            
            if isUnsafe 
            then errorCount + 1
            else errorCount
            ) initialErrorCount
    |> fun x -> x <= 1

"input.example"
|> Common.parseInput
|> Array.filter isSafe
|> Array.length
|> Global.shouldBe 4

"input.actual"
|> Common.parseInput
|> Array.filter isSafe
|> Array.length
|> printfn "Found %d safe reports when Problem Dampener is implemented."