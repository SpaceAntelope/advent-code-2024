open System.IO

let parse path = 
    path
    |> File.ReadAllLines
    |> Array.map _.ToCharArray()
    |> Array.map (Array.map (fun c -> int (c - '0')))
    |> array2D
