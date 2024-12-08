open System.IO
let parse (path: string) =
    path
    |> File.ReadAllLines
    |> Array.map (fun line -> 
        let parts = line.Split(':')
        let nums = 
            parts.[1].Trim().Split(' ') 
            |> Array.map int64
        int64 parts[0],nums)