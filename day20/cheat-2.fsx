#load "../global.fsx"
#load "./common.fsx"

open System
open System.Collections.Generic
open Common

type AdvancedCheat = {
    Start: Point
    End: Point
    Picos: int }

let point2index (path: Point list) =
    path 
    |> List.indexed 
    |> List.map (fun pt -> snd pt, fst pt) 
    |> readOnlyDict

let availableCheats (fullPath: Point list) : AdvancedCheat list = 
    let pt2index = point2index fullPath 
        
    fullPath 
    |> List.allPairs fullPath
    |> List.map (fun (pt1,pt2) -> pt1,pt2,Global.manhattan pt1 pt2)
    |> List.filter (fun (pt1,pt2,dist) -> dist <= 20 && pt2index.[pt1] < pt2index[pt2])
    |> List.map (fun (pt1,pt2,dist) -> { Start = pt1; End = pt2; Picos = dist })

let applyCheats (fullPath: Point list) (cheats: AdvancedCheat list) : CheatResult list = 
    let pt2index = point2index fullPath 

    printfn "Cheat count: %d" cheats.Length

    cheats
    |> List.countBy (fun cheat ->
        let shortenedPathLength = pt2index.[cheat.Start] + cheat.Picos + ((fullPath.Length - 1) - pt2index.[cheat.End])
        fullPath.Length - 1 - shortenedPathLength )
    |> List.sortBy fst
    |> List.map(fun (saved,freq) -> { Saved = saved; CheatCount = freq }) 

"./input.example"
|> parse
|> fun matrix -> 
    matrix
    |> findFullPath
    |> fun path -> 
        availableCheats path
        |> applyCheats path
        |> List.filter (fun x -> x.Saved >= 50)
    |> fun c -> c |> List.iter (printfn "%O"); c
    |> Global.seqShouldBe [
            { Saved = 50; CheatCount = 32 }
            { Saved = 52; CheatCount = 31 }
            { Saved = 54; CheatCount = 29 }
            { Saved = 56; CheatCount = 39 }
            { Saved = 58; CheatCount = 25 }
            { Saved = 60; CheatCount = 23 }
            { Saved = 62; CheatCount = 20 }
            { Saved = 64; CheatCount = 19 }
            { Saved = 66; CheatCount = 12 }
            { Saved = 68; CheatCount = 14 }
            { Saved = 70; CheatCount = 12 }
            { Saved = 72; CheatCount = 22 }
            { Saved = 74; CheatCount = 4 }
            { Saved = 76; CheatCount = 3 }  ]

"./input.actual"
|> parse
|> fun matrix -> 
    matrix
    |> findFullPath
    |> fun path -> 
        availableCheats path
        |> applyCheats path
        |> List.filter (fun cheat -> cheat.Saved >= 100)
        |> List.sumBy _.CheatCount
|> printfn "%d cheats would save you at least 100 picoseconds."