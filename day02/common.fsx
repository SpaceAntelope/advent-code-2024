open System.IO

let parseInput path =
    path
    |> File.ReadAllLines
    |> Array.map _.Split(' ')
    |> Array.map (Array.map int)
