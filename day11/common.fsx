open System.IO

let parse path = 
    path 
    |> File.ReadAllText 
    |> _.Trim() 
    |> _.Split(' ') 
    |> Array.map int64 
    |> List.ofArray