open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

#load "../global.fsx"
#load "./common.fsx"


let comparerFactory (ruleset: (int*int) list) :int -> int -> int = 
    let leftIndex = 
        ruleset 
        |> List.groupBy fst 
        |> List.map (fun (key,grp)-> key, grp |> List.map snd)
        |> Map.ofList

    fun page1 page2 -> 
        match (leftIndex  |> Map.tryFind page1) with
        | Some afterSet when afterSet |> List.contains page2 -> -1
        | _ -> 1
        

let sumMiddleElementOfReorderedUpdates (inputPath :string) =
    let (_, rules, updates) = 
        Common.parseInput inputPath
    
    let validateUpdate (update: int array) = 
        update 
        |> Array.pairwise 
        |> Array.forall (fun pair -> rules |> Array.contains pair)
    
    let comparer = comparerFactory (List.ofArray rules) //expandedRuleset
    
    updates
    |> Array.filter (validateUpdate>>not)
    |> Array.map (Array.sortWith comparer)
    |> Array.map (Common.middleElement)
    |> Array.sum


"./input.example"
|> sumMiddleElementOfReorderedUpdates
|> Global.shouldBe 123


"./input.actual"
|> sumMiddleElementOfReorderedUpdates
|> printfn "The sum of the middle element of each valid update is %d"
