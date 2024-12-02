open System.Text.RegularExpressions
open System.IO
let shouldBe expected actual= 
    if expected <> actual 
    then 
        failwith $"Comparison between expected {expected} and {actual} failed."

let parseInput (path: string) =
    path
    |> File.ReadAllLines
    |> Array.filter (not<<System.String.IsNullOrEmpty)
    |> Array.map (fun x -> 
        Regex.Matches(x, "\\d+") 
        |> Seq.map _.Value 
        |> Seq.map int
        |> fun s -> Seq.head s, Seq.last s)
    |> Array.unzip

