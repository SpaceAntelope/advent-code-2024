#load "../global.fsx"

open System
open System.IO

type Node =
    | Root of string
    | Branch of Node*string

let parse path = 
    path 
    |> File.ReadAllLines 
    |> Array.fold (fun map current -> 
        let pair = current.Split('-')
        map |> Map.add pair.[0] pair.[1]) Map.empty<string,string>

let parse' path = 
    path 
    |> File.ReadAllLines 
    |> Array.map _.Split('-')


let pairs = 
    "./input.example"
    |> parse'
    |> Array.map (fun arr -> arr[0],arr[1])
    
pairs 
|> Array.groupBy fst 
|> Array.append (pairs |> Array.groupBy snd) 
|> Array.groupBy fst 
|> Array.map (fun (key, grp) -> key, grp |> Array.map snd |> Array.collect id)
|> Array.filter (fun (key,grp) -> key.StartsWith('t')) 
|> Array.iter(printfn "%A")
