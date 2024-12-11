open System.IO

#load "../global.fsx"
#load "./common.fsx"

let rec expand (idx:int) (maxIdx:int) (pebbles : int64 seq) =
    if idx = maxIdx
    then pebbles
    else
        pebbles
        |> Seq.collect (fun pb -> 
            match pb with
            | 0L -> seq { 1L }
            | x when x.ToString().Length%2 = 0 -> 
                let str = x.ToString()
                let ln = str.Length
                seq { 
                    int64 <| str.Substring(0,ln/2)
                    int64 <| str.Substring(ln/2) }            
            | x -> seq { x * 2024L })
        |> expand (idx+1) maxIdx


"./input.example"
|> Common.parse
|> expand 0 25
|> Seq.length
|> Global.shouldBe 55312

"./input.actual"
|> Common.parse
|> expand 0 25
|> Seq.length
|> printfn "Pebble count after 25 blinks is %d"