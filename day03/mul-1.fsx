open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"

let sumMults (source: string) = 
    Regex.Matches(source, @"mul\((\d+),(\d+)\)")
    |> Seq.map(fun m -> int m.Groups.[1].Value, int m.Groups.[2].Value)
    |> Seq.fold(fun state (x,y)-> state + x*y) 0

"./input-1.example"
|> File.ReadAllText
|> sumMults
|> Global.shouldBe 161    

"./input.actual"
|> File.ReadAllText
|> sumMults
|> printfn "The sum of all valid multiplications is %A"