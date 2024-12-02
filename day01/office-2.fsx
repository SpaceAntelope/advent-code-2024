#load "./common.fsx"
open System.IO

let similarityScore (left : int array) (right : int array) =
    let rightHist = 
        right |> Array.countBy id |> readOnlyDict
    
    left
    |> Array.fold (fun state current -> 
        match rightHist.TryGetValue(current) with
        | true, v -> v
        | _ -> 0
        |> fun freq -> state + (current * freq)
    ) 0

"./input.example"
|> Common.parseInput
||> similarityScore
|> Common.shouldBe 31

"./input.actual"
|> Common.parseInput
||> similarityScore
|> printfn "%d"