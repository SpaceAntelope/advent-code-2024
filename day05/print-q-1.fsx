open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

#load "../global.fsx"
#load "./common.fsx"


let sumMiddleElementsOfValidUpdates (updates : int[][])  (ruleset: (int*int) list) =

    let validateUpdate (update: int array) = 
        update |> Array.pairwise |> Array.forall (fun pair -> ruleset |> List.contains pair)

    updates
    |> Array.filter validateUpdate
    |> Array.map Common.middleElement
    |> Array.sum

"./input.example"
|> Common.parseInput
|> fun (_, rules, updates) ->
    rules
    |> List.ofArray
    |> sumMiddleElementsOfValidUpdates updates
|> Global.shouldBe 143


"./input.actual"
|> Common.parseInput
|> fun (_, rules, updates) ->
    rules
    |> List.ofArray
    |> sumMiddleElementsOfValidUpdates updates
|> printfn "The sum of the middle element of each valid update is %d"
