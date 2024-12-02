open System
open System.IO
open System.Text.RegularExpressions

#load "./common.fsx"
#load "../global.fsx"

let distanceSum (left : int array) (right: int array) =
    (left |> Array.sort, right |> Array.sort)
    ||> Array.zip
    |> Array.map (fun (l,r) -> Math.Abs(l-r))
    |> Array.reduce (+)

"./input.example"
|> Common.parseInput
||> distanceSum 
|> Global.shouldBe 11


"./input.actual"
|> Common.parseInput
||> distanceSum 
|> printfn "%A" 