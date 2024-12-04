open System.IO
open System.Text.RegularExpressions

#load "../global.fsx"

let sumMults (source: string) =
    Regex.Matches(source, @"don't\(\)|do\(\)|mul\((\d+),(\d+)\)")
    |> Seq.fold
        (fun (acc, enabled) m ->
            match m.Value with
            | "don't()" -> acc, false
            | "do()" -> acc, true
            | mul when mul.StartsWith("mul") && enabled ->
                let (x, y) =
                    int m.Groups.[1].Value, int m.Groups.[2].Value

                acc + x * y, enabled
            | x -> acc, enabled)
        (0, true)
    |> fst

"./input-2.example"
|> File.ReadAllText
|> sumMults
|> Global.shouldBe 48

"./input.actual"
|> File.ReadAllText
|> sumMults
|> printfn "The sum of all valid multiplications with respect to do() and don't() is %A"
