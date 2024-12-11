open System
open System.IO

#load "../global.fsx"
#load "./common.fsx"

    
[<TailCall>]
let rec blink (idx:int) (maxIdx:int) (pebbles : (int64*int64) list) =
    if idx = maxIdx
    then pebbles |> List.sumBy snd
    else
        pebbles
        // Expand array
        |> List.collect (fun (pebbleId, pebbleCount) -> 
            let fpb = float pebbleId
            let digitCount = Math.Ceiling(Math.Log(fpb + 1.0,10))      
            match pebbleId with
            | 0L -> [ 1L, pebbleCount ]
            | x when digitCount % 2.0 = 0.0 -> 
                let factor = Math.Pow(10,digitCount/2.0)
                let right = fpb % factor
                let left = (fpb - right) / factor
                [int64 left, pebbleCount; int64 right,pebbleCount]   
            | x -> [ x * 2024L, pebbleCount ])
        // Compress array
        |> List.groupBy fst
        |> List.map (fun (pebbleId, pebbleGroup) -> pebbleId, pebbleGroup |> List.sumBy snd)
        |> blink (idx+1) maxIdx


"./input.example"
|> Common.parse
|> List.map (fun pebble -> pebble,1L)
|> blink 0 25 
|> Global.shouldBe 55312L

"./input.actual"
|> Common.parse
|> List.map (fun pebble -> pebble,1L)
|> blink 0 75 
|> printfn "Pebble count after 75 blinks is %d" 