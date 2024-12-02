open System.IO
open System

#load "../global.fsx"
#load "./common.fsx"

let isSafe (report: int[]) =
    let rising = (report[0] - report[1]) < 0
    report 
    |> Array.pairwise
    |> Array.tryFind (fun (x1,x2) ->
        let diff = Math.Abs(x1-x2)
        ((rising && (x2 - x1) < 0) || (not rising && (x2-x1) > 0 )) 
        || diff < 1 || diff > 3)
    |> Option.isNone

"input.example"
|> Common.parseInput
|> Array.filter isSafe
|> Array.length
|> Global.shouldBe 2

"input.actual"
|> Common.parseInput
|> Array.filter isSafe
|> Array.length
|> printfn "Found %d safe reports."